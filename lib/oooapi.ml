open Ppxlib
open AstUtil
module H = Http_spec

(* re-exported just for testing *)
module Internal__ = struct
  module DAG = DAG
end

(* Exceptions ans helpers to raise them *)
module Exn = struct
  exception Invalid_spec of string
  let invalid msg = raise (Invalid_spec msg)

  exception Unsupported of string
  let unsupported msg = raise (Unsupported msg)

  exception Invalid_data of string
  let invalid_data msg = raise (Invalid_data msg)
end

let get_or
  : 'a option -> f:(unit -> 'a) -> 'a
  = fun o ~f ->
    match o with
    | None   -> f ()
    | Some v -> v

(** Code to construct the [Data] module, which includes type declarations to
    represent all the request and response data, along with serialization
    functions and helpers to construct instances of the data.

    The [Data] module is a module of modules, and looks roughly like:

    {|
    module Data = struct
      module SomeData = struct
        type some_type = ...
          [@@deriving make, yojson]
        ...
        type t = {foo: some_type; ... }
          [@@deriving make, yojson]
      end

      ...
    end
    |} *)
module DataModule = struct

  (** Generate a form part for a record field *)
  let form_part_of_label
    : label_declaration -> expression
    = fun {pld_name; pld_type; _} ->
      let field_name: expression = Ast.estring pld_name.txt
      in
      (* Access the record field data: [t.field_name] *)
      let field_value: expression = AstExt.Exp.field_access [%expr t] pld_name.txt
      in
      (* Access the record field data: [t.field_name] *)
      let (field_principle_type, is_optional) : core_type * bool =
        match pld_type with
        | [%type: [%t? typ] option] -> typ, true
        | typ                       -> typ, false
      in
      (* A conversion function of type [field_principle_type -> Multipart.part_data] *)
      let field_value_conv: expression =
        match field_principle_type with
        | [%type: bool]          -> [%expr fun b -> `String (string_of_bool b)]
        | [%type: int]           -> [%expr fun i -> `String (string_of_int i)]
        | [%type: float]         -> [%expr fun f -> `String (string_of_float f)]
        | [%type: string]        -> [%expr fun s -> `String s]
        | [%type: Yojson.Safe.t] -> [%expr fun j -> `String (Yojson.Safe.to_string j)]

        (* We cast from the narrower [[`String]] or [[`File]] into the wider [Multipart.part_data] *)
        | [%type: [`String of string]] -> [%expr fun s -> (s :> Multipart.part_data)]
        | [%type: [`File of string ]]  -> [%expr fun f -> (f :> Multipart.part_data)]

        (* TODO: Add support for, e.g., "deepObjects": https://github.com/OAI/OpenAPI-Specification/issues/1706*)
        | unsupported_type ->
          let msg =
            Format.asprintf "unsupported type for field '%s' of multipart form: %a"
              pld_name.txt
              Astlib.Pprintast.core_type unsupported_type
          in
          Exn.unsupported msg
      in
      if is_optional then
        (* E.g. [Option.map (fun v -> ("foo", field_value_conv v)) t.field_name] *)
        [%expr Option.map (fun v -> ([%e field_name], [%e field_value_conv] v)) [%e field_value]]
      else
        (* E.g. [Some ("foo", field_value_conv t.field_name)] *)
        [%expr Some ([%e field_name], [%e field_value_conv] [%e field_value])]

  (** Construct a function to convert a value of the declared type into the
      parts for a multipart form *)
  let to_multipart_fun_of_decl
    : type_declaration -> structure_item
    = fun decl -> match decl.ptype_kind with
      | Ptype_record labels ->
        let body : expression =
          [%expr List.filter_map (fun x -> x)
              [%e labels |> List.map form_part_of_label |> Ast.elist]]
        in
        let fun_def: expression = AstExt.Exp.f [%pat? t] body in
        [%stri
          let to_multipart
            : t -> (string * [> `String of string | `File of string]) list
            = [%e fun_def]
        ]
      | _ ->
        (* Why does OpenAPI allow this? *)
        Exn.invalid "multipart media type must be described by a record"

  (** Construct the contents of the [Data] module that contains all the data type modules *)
  let data_module_of_schema_entry
    : string -> Http_spec.schema -> structure_item
    = fun name schema ->
      let name_str = Camelsnakekebab.upper_camel_case name in
      let name = n (Some name_str) in
      let main_decl, declarations = Ocaml_of_json_schema.type_declarations schema.schema.schema in
      let type_declarations =
        let dependency_ordered_declarations = declarations @ [main_decl] in
        List.map (fun decl -> Ast.pstr_type Recursive [decl]) dependency_ordered_declarations
      in
      let to_multipart : structure_item list = match schema.kind with
        | `Url_encoded_form (* The to_multipart function also works for URL encoded data to hand to oooapi_lib *)
        | `Multipart_form ->
          let f_exp =
            try to_multipart_fun_of_decl main_decl
            with Exn.Unsupported err ->
              Exn.unsupported (err ^ " while converting" ^ name_str)
          in
          [f_exp]
        | _ -> [] (* We don't need this conversion function for other media *)
      in
      let expr = Ast.pmod_structure (type_declarations @ to_multipart) in
      Ast.module_binding ~name ~expr |> Ast.pstr_module

  (* We only need the last part of the path, which is the key in the OpenAPI schema object *)
  let json_query_path_terminal
    : Json_query.path -> string
    = fun path ->
      path
      |> ListLabels.fold_left
        ~init:None
        ~f:(fun _ x -> match x with `Field f -> Some f | _ -> None)
      |> function
      | Some f -> f
      | None  -> Exn.invalid "Invalid reference path: it does not end with field."

  (* The names of the dependencies of a JSON Schema element *)
  let rec element_deps
    : Json_schema.element -> string list
    = fun el ->
      match el.kind with
      | Def_ref path ->
        [ json_query_path_terminal path ]
      | Monomorphic_array (e, _) ->
        element_deps e
      | Object o ->
        o.properties |> List.fold_left (fun acc (_, e, _, _) -> acc @ element_deps e) []
      | Array (es, _)
      | Combine (_, es) ->
        List.fold_left (fun acc e -> acc @ element_deps e) [] es
      (* TODO Should these refs be accounted for at some point? *)
      | Id_ref _ | Ext_ref _
      (*  *)
      | String _ | Integer _ | Number _ | Boolean | Null | Any | Dummy ->
        []

  (* TODO: Find non-terminatoin here on the Stripe spec *)
  let of_schemata (schemata : Http_spec.schemata) =
    let schemata = Http_spec.SM.bindings schemata in
    (* The DAG is used for dependency analysis *)
    let module Graph = DAG.Make (String) in
    let add_dependency graph (src, (s : Http_spec.schema)) =
      (* Add each schema and its deps to the graph *)
      let deps = s.schema.schema |> Json_schema.root |> element_deps  in
      Graph.add_arcs ~src deps graph
    in
    let generate_data_module schema_label =
      schemata
      |> List.assoc_opt schema_label
      |> get_or ~f:(fun () -> Exn.invalid ("No schema found matching reference " ^ schema_label))
      |> data_module_of_schema_entry schema_label
    in
    let expr =
      try
        schemata
        |> ListLabels.fold_left ~init:Graph.empty ~f:add_dependency (* Build the dependency graph *)
        |> Graph.topological_sort
        |> List.rev (* We need to generate the modules with the least dependent first *)
        |> List.map generate_data_module
        |> Ast.pmod_structure
      with
      | Graph.Cycle_found cycle ->
        let msg = Format.asprintf
            "Cycle found in data dependencies found between schema components: %a"
            (Graph.pp_trail Format.pp_print_string)
            cycle
        in
        Exn.invalid_data msg
    in
    let name = Ast.Located.mk (Some "Data") in
    Ast.module_binding ~name ~expr |> Ast.pstr_module
end

(** Code to construct the [Make] functor, which takes an HTTP client module,
    and a configuration module and constructs a module with functions for
    each operation specified by the OpenAPI spec.

    It looks roughly like this:

    {|
    module Make (Client: Oooapy_lib.Client) (Config: Oooapy_lib.Config) = struct
      module Client = Client (Config)
      open Client

      let base_url = "https://foo/bar/baz"

      let operation_foo = ...
      let operation_bar = ...
      ...
    end
    |} *)
module ApiMakeFunctor = struct

  (* OpenAPI "params", which can be in lots of different places

     I do not know why these are all treated the same way in the OpenAPI schema. *)
  module Params = struct
    type param = {
      name : string; (** The name of the params key *)
      var : expression; (** The variable used to refer to the parameter in expressions *)
      pat : pattern; (** The pattern for binding the parameter's value in function heads *)
      to_string : expression; (** A function to convert the parameter data to a string for serialization *)
      optional : bool; (** True iff the parameter is not required *)
      default : expression option; (** [Some v] if the parameter has a default value [v] *)
      format : string option (* E.g., "binary" *);
    }

    type t = {
      query : param list;
      path : param list;
      header : param list;
      cookie : param list;
    }

    let empty = { query = []; path = []; header = []; cookie = [] }
    let to_list { query; path; header; cookie } = query @ path @ header @ cookie

    (* TODO: We need to gather objects, records decls, from parameters *)
    let of_http_spec_param : string * H.Message.Params.param -> param =
     fun (name, p) ->
      let var = Ast.evar name in
      let pat = Ast.pvar name in
      let optional = not p.required in
      let default, to_string =
        let schema = p.schema.schema.schema in
        let elem = Json_schema.root schema in
        let to_string, typ =
          let t = Ocaml_of_json_schema.type_of_element ~qualifier:"not_applicable____" elem |> fst in
          match t with
          (* TODO: Add support for all types *)
          | [%type: string]      -> ([%expr fun x -> x], t)
          | [%type: string list] -> ([%expr fun x -> String.concat "," x], t)
          | [%type: bool]        -> ([%expr string_of_bool], t)
          | [%type: int]         -> ([%expr string_of_int], t)
          | [%type: float]       -> ([%expr string_of_float], t)
          | unsupported_typ      ->
            (Printf.eprintf
               "ERROR: parameters of type %s not supported for parameter %s, using JSON as fallback. This is probably invalid\n"
               (string_of_core_type unsupported_typ)
               name);
            ([%expr Yojson.Safe.to_string], [%type: Yojson.Safe.t])
        in
        let default =
          (* TODO: Default serialization should be based on the param's type *)
          elem.default
          |> Option.map (Json_repr.any_to_repr (module Json_repr.Yojson))
          |> Option.map (function
               | `String s -> Ast.estring s
               | `Bool b -> Ast.ebool b
               | `Int i -> (
                   (* JS doesn't distinguish between `Number` "types", so we have be sure we
                      decode the default correctly in case it is given in an integral form when
                      it should be a float. *)
                   match typ with
                   | [%type: float] ->
                       Ast.efloat (string_of_float (float_of_int i))
                   | _ -> Ast.eint i)
               | `Float f -> Ast.efloat (string_of_float f)
               | unsupported_default ->
                   Exn.unsupported
                     (Printf.sprintf
                        "default parmamters value %s not supported for param %s"
                        (Yojson.Safe.to_string unsupported_default)
                        name))
        in
        (default, to_string)
      in
      let default = if not optional then None else default in
      { name; optional; default; to_string; var; pat; format = None }

    let of_openapi_parameters
      : Http_spec.Message.Params.t -> t
      = fun params ->
        let module P = H.Message.Params in
        {
          query = P.query params |> List.map of_http_spec_param;
          path = P.path params |> List.map of_http_spec_param;
          header = P.header params |> List.map of_http_spec_param;
          cookie = P.cookie params |> List.map of_http_spec_param;
        }
  end

  type operation_function =
    Openapi_spec.path -> Openapi_spec.operation -> structure_item

  let operation_function_name operation_id =
    operation_id |> AstExt.to_identifier |> AstExt.Pat.var

  let data_module_fun_name name f = Printf.sprintf "Data.%s.%s" name f

  let to_json_fun mod_name : expression =
    let mod_name = Camelsnakekebab.upper_camel_case mod_name in
    Ast.evar (data_module_fun_name mod_name "to_yojson")

  let to_multipart_fun mod_name : expression =
    let mod_name = Camelsnakekebab.upper_camel_case mod_name in
    Ast.evar (data_module_fun_name mod_name "to_multipart")

  (* TODO Generalize for any supported responses, including default *)
  let response_decoder (responses : H.Message.Responses.t) : expression =
    match responses |> List.assoc_opt `OK with
    | None ->
      (* TODO 200 is required only when it is the ONLY response. So need to account for others *)
      (* https://spec.openapis.org/oas/v3.1.0#responses-object *)
      Exn.invalid_data "Invalid schema: no 200 response"
    | Some schema ->
    match schema.kind with
    | `Html | `Binary | `Pdf | `Url_encoded_form | `Multipart_form ->
      [%expr fun x -> Ok x]
    | `Json ->
      let mod_name = Camelsnakekebab.upper_camel_case schema.name in
      let conv_fun =
        Ast.evar (data_module_fun_name mod_name "of_yojson")
      in
      [%expr of_json_string [%e conv_fun]]

  let path_parts path : expression =
    path
    |> List.map (function `C c -> Ast.estring c | `P p -> Ast.evar p)
    |> Ast.elist

  let query_param
    : Params.param -> expression
    = fun { name; to_string; var; optional; default; _ } ->
      match optional, default with
      | true, None ->
        (* The parameter is optional, defaulting to None *)
        [%expr Option.map (fun p -> [%e Ast.estring name], ([%e to_string] p)) [%e var]]
      | _ ->
        (* We are assured to have a value, either via a default or because it is required *)
        [%expr Some ([%e Ast.estring name], [%e to_string] [%e var])]

  let query_params
    : Params.param list -> expression
    = fun params ->
      let params_option_list = params |> List.map query_param |> Ast.elist
      (* We filter out just the param entries that are actually supplied *)
      in [%expr List.filter_map Fun.id [%e params_option_list]]

  let extra_headers
    : Params.param list -> expression
    = fun params ->
      params
      |> List.map (fun (p : Params.param) ->
          [%expr [%e Ast.estring p.name], [%e p.to_string] [%e p.var]])
      |> Ast.elist

  let fun_with_params ~data (params : Params.t) body =
    let module Exp = AstExt.Exp in
    params
    |> Params.to_list
    |> ListLabels.fold_right (* "right" to preserving argument order *)
      ~init:(Exp.f data body) (* the last, innermost function *)
      ~f:(fun (param : Params.param) body' ->
          (* A unary function for each parameter *)
          Exp.f
            ~label:param.name
            ~optional:param.optional
            ?default:param.default
            param.pat
            body')

  (* Extract the Ref path from a schema, if it has one *)
  let ref_of_schema
    : Json_schema.schema -> string option
    = fun s ->
      match (Json_schema.root s).kind with
      | Def_ref path -> Some (DataModule.json_query_path_terminal path)
      | _            -> None

  (* Serialization and content tag based on the media type *)
  let data_converter
    : H.schema -> expression
    = fun schema ->
      match schema.kind with
      | `Html             -> [%expr Some (`Html data)]
      | `Binary           -> [%expr Some (`Binary data)]
      | `Pdf              -> [%expr Some (`Pdf data)]
      | `Json             -> [%expr Some (`Json ([%e to_json_fun schema.name] data))]
      | `Url_encoded_form -> [%expr Some (`Url_encoded_form ([%e to_multipart_fun schema.name] data))]
      | `Multipart_form   -> [%expr Some (`Multipart_form ([%e to_multipart_fun schema.name] data))]

  let operation_fun_expr
    : Http_spec.Message.t -> structure_item
    = fun message ->
      let meth = Ast.pexp_variant (Http.Method.to_string message.req.meth) None in
      let params = Params.of_openapi_parameters message.req.params in
      let data_conv, data_pat =
        match message.req.content with
        | None         -> ([%expr None], [%pat? ()]) (* The data is a unit for requests with no payload (E.g.,  GET) *)
        | Some content -> (data_converter content.schema, [%pat? data])
      in
      let body =
        [%expr
          let path    = [%e path_parts message.req.path] in
          let params  = [%e query_params params.query] in
          let headers = [%e extra_headers params.header] in
          let decode  = [%e response_decoder message.resp] in
          let data    = [%e data_conv] in
          make_request [%e meth] ~base_url ~path ~params ~headers ~decode ?data
        ]
      in
      let name = operation_function_name message.req.id in
      [%stri let [%p name] = [%e fun_with_params ~data:data_pat params body]]

  (** Construct the [Make] functor *)
  let of_messages
    : Http_spec.Message.t list -> structure_item
    = fun messages->
      let endpoint_functor =
        let client_param =
          AstUtil.Mod.(named_functor_param "Client" (module_type_const "Oooapi_lib.Client"))
        in
        let config_param =
          AstUtil.Mod.(named_functor_param "Config" (module_type_const "Oooapi_lib.Config"))
        in
        let mod_impl =
          (* Instantiate the client and also open if for direct reference *)
          let module_declarations =
            [ [%stri module Client = Client (Config)]
            ; [%stri open Client]
            ]
          in
          let function_definitions = messages |> List.map operation_fun_expr in
          Ast.pmod_structure (module_declarations @ function_definitions)
        in
        Ast.pmod_functor client_param
        @@ Ast.pmod_functor config_param
        @@ mod_impl
      in
      AstUtil.Mod.binding "Make" endpoint_functor
end

let write_ast_f fmt str =
  Format.fprintf fmt "%a\n" Pprintast.structure str;
  Format.print_flush ()

let write_ast str = write_ast_f Format.std_formatter str

let notice () =
  let Unix.{ tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; _ } =
    Unix.time () |> Unix.gmtime
  in
  Printf.sprintf "Generated by oooapi at %i-%i-%iT%i:%i:%iZ" (tm_year + 1900)
    (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let module_of_spec
  : Openapi_spec.t -> Ppxlib.Ast.structure
  = fun spec ->
    let spec = Http_spec.of_openapi_spec spec in
    let data_module = DataModule.of_schemata spec.schemata in
    let api_functor = ApiMakeFunctor.of_messages spec.messages in
    [ [%stri let __NOTE__ = [%e Ast.estring (notice ())] ]
    ; [%stri let __TITLE__ = [%e Ast.estring spec.title] ]
    ; [%stri let __API_VERSION__ = [%e Ast.estring spec.version] ]
    ; [%stri open Oooapi_lib [@@warning "-33"] ]
    ; [%stri let base_url = [%e Ast.estring spec.base_url] ]
    ; data_module
    ; api_functor
    ]
