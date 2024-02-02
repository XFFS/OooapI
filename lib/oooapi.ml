open Ppxlib
open AstUtil

(* re-exported just for testing *)
module DAG = DAG
module H = Http_spec

module DataModule = struct

  exception Unsupported of string
  exception Invalid_spec of string
  let raise_invalid msg = raise (Invalid_spec msg)

  let form_part_of_label
    : label_declaration -> expression =
    fun {pld_name; pld_type; _} ->
    let field_name = Ast.estring pld_name.txt in
    (* Access the record field data: [t.field_name] *)
    let field_access = AstExt.Exp.field_access [%expr t] pld_name.txt in
    let field_principle_type : core_type = match pld_type with
      | [%type: [%t? typ] option] -> typ
      | typ -> typ
    in
    (* TODO: More general and nicer way to derive this  *)
    (* The [v] is bound in the `Some` match case bellow *)
    let field_value_conv = match field_principle_type with
      | [%type: int] -> [%expr fun i -> `String (string_of_int i)]
      | [%type: float] -> [%expr fun f -> `String (string_of_float f)]
      | [%type: bool] -> [%expr fun b -> `String (string_of_bool b)]
      | [%type: string] -> [%expr fun s -> `String s]
      (* Fields that accept a [`String s] must be cast into the wider type *)
      | [%type: [`String of string]] -> [%expr fun s -> (s :> Multipart.part_data)]
      (* Fields that accept a [`File f] must be cast into the wider type *)
      | [%type: [`File of string ]] -> [%expr fun f -> (f :> Multipart.part_data)]
      | [%type: Yojson.Safe.t] -> [%expr fun j -> `String (Yojson.Safe.to_string j)]

      | unsupported_type ->
        let msg =
          Format.asprintf "unsupported type for field '%s' of multipart form: %a"
            pld_name.txt
            Astlib.Pprintast.core_type unsupported_type
        in
        raise (Unsupported (msg))
    in
    (* Make the values uniformly optional *)
    match pld_type with
    | [%type: [%t? _] option]
      (* E.g. [Option.map (fun v -> ("foo", field_value_conv v)) t.field_name] *)
      -> [%expr Option.map (fun v -> ([%e field_name], [%e field_value_conv] v)) [%e field_access]]
    | _
      (* E.g. [Some ("foo", field_value_conv t.field_name)] *)
      -> [%expr Some ([%e field_name], [%e field_value_conv] [%e field_access])]

  (** Construct a function to convert a value of the type declaration to multipart form parts *)
  let to_multipart_fun_of_decl
    : type_declaration -> structure_item =
    fun decl -> match decl.ptype_kind with
      | Ptype_record labels ->
        let body : expression =
          [%expr List.filter_map (fun x -> Fun.id x)
              [%e labels |> List.map form_part_of_label |> Ast.elist]]
        in
        let fun_def: expression = AstExt.Exp.f [%pat? t] body in
        [%stri
          let to_multipart
            : t -> (string * [> `String of string | `File of string]) list
            = [%e fun_def]
        ]
      | _ ->
        (** Why does OpenAPI allow this? *)
        raise_invalid "multipart media type must be described by a record"

  let data_module_of_schema_entry : string * Http_spec.schema -> structure_item
      =
   fun (name, schema) ->
    let name_str = Camelsnakekebab.upper_camel_case name in
    let name = n (Some name_str) in
    let main_decl, declarations = Ocaml_of_json_schema.type_declarations schema.schema.schema in
    let type_declarations =
      let dependency_ordered_declarations = declarations @ [main_decl] in
      List.map (fun decl -> Ast.pstr_type Recursive [decl]) dependency_ordered_declarations
    in
    let to_multipart = match schema.kind with
      | `Url_encoded_form (* The to_multipart function also works for URL encoded data to hand to oooapi_lib *)
      | `Multipart_form ->
        let f_exp =
          try to_multipart_fun_of_decl main_decl
          with Unsupported err ->
            raise (Unsupported (name_str ^ ": " ^ err))
        in
        [f_exp]
      | _ -> []
    in
    let expr = Ast.pmod_structure (type_declarations @ to_multipart) in
    Ast.module_binding ~name ~expr |> Ast.pstr_module

  (* We only need the last part of the path, which is the key in the OpenAPI schema object *)
  let rec json_query_path_terminal : Json_query.path -> string = function
    | [] -> failwith "Invalid ref path: does not end with field"
    | [ `Field f ] -> f
    | _ :: rest -> json_query_path_terminal rest

  let schema_deps : Json_schema.schema -> string list =
   fun schema ->
    let rec gather_deps (el : Json_schema.element) =
      match el.kind with
      | Def_ref path -> [ json_query_path_terminal path ]
      | Monomorphic_array (e, _) -> gather_deps e
      | Object o ->
          o.properties
          |> List.fold_left (fun acc (_, e, _, _) -> acc @ gather_deps e) []
      | Array (es, _) | Combine (_, es) ->
          List.fold_left (fun acc e -> acc @ gather_deps e) [] es
      (* TODO These should be accounted for at some point *)
      | Id_ref _ | Ext_ref _ | String _ | Integer _ | Number _ | Boolean | Null
      | Any | Dummy ->
          []
    in
    Json_schema.root schema |> gather_deps

  (* The DAG is used for dependency analysis *)
  module Graph = DAG.Make (String)

  let of_schemata (schemata : Http_spec.schemata) =
    let name = Ast.Located.mk (Some "Data") in
    let expr =
      let schemata = Http_spec.SM.bindings schemata in
      let structure_items =
        let dep_ordering =
          let dep_graph =
            (* Build a dependency graph of each component *)
            schemata
            |> ListLabels.fold_left ~init:Graph.empty
                 ~f:(fun graph (src, (s : Http_spec.schema)) ->
                   (* Add each schema and its deps to the graph *)
                   let deps = schema_deps s.schema.schema in
                   Graph.add_arcs ~src deps graph)
          in
          let sorted_rev = Graph.topological_sort dep_graph in
          List.rev sorted_rev
        in
        dep_ordering
        |> List.map (fun label ->
               match List.assoc_opt label schemata with
               | Some s -> data_module_of_schema_entry (label, s)
               | None ->
                   failwith
                     ("TODO: No schema provided matching reference " ^ label))
      in
      let file_type_decls =
        [ [%stri let string_of_file (`File n) = n]
        ; [%stri let string_of_str (`String n) = n]
        ]
      in
      Ast.pmod_structure (file_type_decls @ structure_items)
    in
    Ast.module_binding ~name ~expr |> Ast.pstr_module
end

module EndpointsModule = struct
  exception Invalid_data of string

  module Params = struct
    type param = {
      name : string;
      var : expression;
      pat : pattern;
      to_string : expression;
      optional : bool;
      default : expression option;
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
    (* TODO: Needs cleanup *)
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
          | [%type: string] -> ([%expr fun x -> x], t)
          | [%type: string list] -> ([%expr fun x -> String.concat "," x], t)
          | [%type: bool] -> ([%expr string_of_bool], t)
          | [%type: int] -> ([%expr string_of_int], t)
          | [%type: float] -> ([%expr string_of_float], t)
          | unsupported_typ ->
            (Printf.eprintf
               "ERROR: parameters of type %s not supported for parameter %s, using JSON as fallback. This is probably invalid\n"
               (string_of_core_type unsupported_typ)
               name);
            ([%expr Yojson.Safe.to_string], [%type: Yojson.Safe.t])
        in
        let default =
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
                   failwith
                     (Printf.sprintf
                        "default parmamters value %s not supported for param %s"
                        (Yojson.Safe.to_string unsupported_default)
                        name))
        in
        (default, to_string)
      in
      let default = if not optional then None else default in
      { name; optional; default; to_string; var; pat; format = None }

    let of_openapi_parameters : Http_spec.Message.Params.t -> t =
     fun params ->
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
        raise (Invalid_data "Invalid schema: no 200 response")
    | Some schema -> (
        match schema.kind with
        | `Json ->
            let mod_name = Camelsnakekebab.upper_camel_case schema.name in
            let conv_fun =
              Ast.evar (data_module_fun_name mod_name "of_yojson")
            in
            [%expr of_json_string [%e conv_fun]]
        | `Html -> [%expr fun x -> Ok x]
        | `Binary -> [%expr fun x -> Ok x]
        | `Pdf -> [%expr fun x -> Ok x]
        (* TODO Add support for decoding form responses *)
        | `Url_encoded_form -> [%expr fun x -> Ok x]
        | `Multipart_form -> [%expr fun x -> Ok x])

  let path_parts path : expression =
    path
    |> List.map (function `C c -> Ast.estring c | `P p -> Ast.evar p)
    |> Ast.elist

  let query_params
    : Params.param list -> expression
    =
    let param_entry_expression
      : Params.param -> expression
      = fun { name; to_string; var; optional; default; _ } ->
        match optional, default with
        | true, None ->
          (* The parameter is optional, defaulting to None *)
          [%expr Option.map (fun p -> [%e Ast.estring name], ([%e to_string] p)) [%e var]]
        | _ ->
          (* We are assured to have a value. via default or required *)
          [%expr Some ([%e Ast.estring name], [%e to_string] [%e var])]
    in
    fun params ->
      let params_option_list = params |> List.map param_entry_expression |> Ast.elist
      (* We filter out just the param entries that are actually supplied *)
      in [%expr List.filter_map Fun.id [%e params_option_list]]


  let extra_headers (params : Params.param list) : expression =
    params
    |> List.map (fun (p : Params.param) ->
           [%expr [%e Ast.estring p.name], [%e p.to_string] [%e p.var]])
    |> Ast.elist

  let fun_with_params ~data (params : Params.t) body =
    let open AstExt in
    params |> Params.to_list
    |> ListLabels.fold_right
         ~init:(Exp.f data body) (* the last, innermost function *)
         ~f:(fun (param : Params.param) body' ->
           Exp.f ~label:param.name ~optional:param.optional
             ?default:param.default param.pat body')

  (* Extract the Ref path from a schema, if it has one *)
  let ref_of_schema : Json_schema.schema -> string option =
   fun s ->
    match (Json_schema.root s).kind with
    | Def_ref path -> Some (DataModule.json_query_path_terminal path)
    | _ -> None

  let data_conv_and_pat : H.Message.Request.t -> expression * pattern = function
    | { content = None; _ } -> ([%expr None], [%pat? ()])
    | { content = Some { schema; _ }; _ } ->
        let data_exp =
          match schema.kind with
          | `Json -> [%expr Some (`Json ([%e to_json_fun schema.name] data))]
          | `Html -> [%expr Some (`Html data)]
          | `Binary -> [%expr Some (`Binary data)]
          | `Pdf -> [%expr Some (`Pdf data)]
          | `Url_encoded_form ->
            (* TODO Use Cohttp.Body.of_form *)
            [%expr Some (`Url_encoded_form ([%e to_multipart_fun schema.name] data))]
          | `Multipart_form ->
              [%expr
                Some (`Multipart_form ([%e to_multipart_fun schema.name] data))]
        in
        (data_exp, [%pat? data])

  let operation_fun_expr : Http_spec.Message.t -> structure_item =
   fun message ->
    let name = operation_function_name message.req.id in
    let params = Params.of_openapi_parameters message.req.params in
    let data_conv, data_pat = data_conv_and_pat message.req in
    let meth = Ast.pexp_variant (Http.Method.to_string message.req.meth) None in
    let body =
      [%expr
        let path = [%e path_parts message.req.path] in
        let params = [%e query_params params.query] in
        let headers = [%e extra_headers params.header] in
        let decode = [%e response_decoder message.resp] in
        let data = [%e data_conv] in
        make_request [%e meth] ~base_url ~path ~params ~headers ~decode ?data
      ]
    in
    [%stri let [%p name] = [%e fun_with_params ~data:data_pat params body]]

  (** Construct the AST node of the module with all endpoint functions *)
  let of_messages : Http_spec.Message.t list -> string -> structure_item list =
   fun messages base_url ->
    let endpoint_functor =
      let endpoint_functor_param =
        Named
          ( n (Some "Client"),
            Ast.pmty_ident (n (Astlib.Longident.parse "Oooapi_lib.Client")) )
      in
      let config_functor_param =
        Named
          ( n (Some "Config"),
            Ast.pmty_ident (n (Astlib.Longident.parse "Oooapi_lib.Config")) )
      in
      let mod_impl =
        (* Instantiate the client and also provide if for direct reference *)
        let decls =
          [ [%stri module Client = Cohttp_client (Config)]
          ; [%stri open Client]
          ]
        in
        let endpoint_functions = messages |> List.map operation_fun_expr in
        Ast.pmod_structure (decls @ endpoint_functions)
      in
      Ast.pmod_functor endpoint_functor_param
      @@ Ast.pmod_functor config_functor_param
      @@ mod_impl
    in
    [
      [%stri let base_url = [%e Ast.estring base_url]];
      Ast.module_binding ~name:(n (Some "Make")) ~expr:endpoint_functor
      |> Ast.pstr_module;
    ]
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

let module_of_spec : Openapi_spec.t -> Ppxlib.Ast.structure =
 fun spec ->
  let spec = Http_spec.of_openapi_spec spec in
  [%stri let __NOTE__ = [%e Ast.estring (notice ())]]
  :: [%stri let __TITLE__ = [%e Ast.estring spec.title]]
  :: [%stri let __API_VERSION__ = [%e Ast.estring spec.version]]
  :: [%stri open Oooapi_lib]
  :: DataModule.of_schemata spec.schemata
  :: EndpointsModule.of_messages spec.messages spec.base_url
