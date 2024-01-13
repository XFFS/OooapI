open Ppxlib
open AstUtil

(* re-exported just for testing *)
module DAG = DAG
module H = Http_spec

module DataModule = struct
  (* TODO Account for constraints, like pattern, min/max format etc.?
     This could be thru abstract types or thru validators in constructors *)

  let deriving_attrs ~is_record =
    if is_record then
      [
        Ast.attribute ~name:(n "deriving")
          ~payload:(PStr [ [%stri yojson { strict = false }, make] ]);
      ]
    else
      [
        Ast.attribute ~name:(n "deriving")
          ~payload:(PStr [ [%stri yojson { strict = false }] ]);
      ]

  let rec module_name_of_def_ref : Json_query.path -> string = function
    | [] -> failwith "invalid component query path"
    | [ `Field n ] -> Camelsnakekebab.upper_camel_case n
    | _ :: rest -> module_name_of_def_ref rest

  let type_name_of_def_ref : Json_query.path -> string =
   fun p -> module_name_of_def_ref p ^ ".t"

  let rec type_of_element
    : qualifier:string -> Json_schema.element -> (core_type * type_declaration list)
    =
    fun ~qualifier element ->
    match (element.kind : Json_schema.element_kind) with
    (* Unsupported schemas *)
    | Id_ref _ -> failwith "unsupported: id_ref schema"
    | Ext_ref _ -> failwith "unsupported: ext_ref schema"
    | Dummy -> failwith "unsupported: dummy schema"
    (* Supported schemas *)
    (* TODO: If min and max items are equal, Array can be a tuple *)
    | Array (_, _) -> [%type: Yojson.Safe.t], []
    | Any -> [%type: Yojson.Safe.t], []
    | Null -> [%type: unit], []
    | Boolean ->  [%type: bool], []
    | Integer _ -> [%type: int], []
    | Number _ -> [%type: float], []
    | String _ -> [%type: string], []
    | Combine (comb, elems) -> type_of_combine ~qualifier (comb, elems)
    | Def_ref path ->
      AstExt.Type.v (AstExt.Type.constr (type_name_of_def_ref path)), []
    | Monomorphic_array (e, _) ->
      let item_type_name = qualifier ^ "_item" in
      let item_type, decls = type_of_element ~qualifier:item_type_name e in
      ([%type: [%t item_type] list], decls)
    | Object o ->
      let decls = type_decl_of_object ~name:qualifier o in
      let typ = AstExt.Type.v (AstExt.Type.constr qualifier) in
      let typ = if o.nullable then [%type: [%t typ] option] else typ in
      (typ, decls)

  and type_of_combine
    : qualifier:string -> Json_schema.combinator * Json_schema.element list
      -> (core_type * type_declaration list) =
   fun ~qualifier (comb, elems) ->
   match comb with
   | Json_schema.All_of | Json_schema.Not -> [%type: Yojson.Safe.t], []
   | Json_schema.Any_of | Json_schema.One_of ->
    match
      elems |> List.find_opt (function | Json_schema.{ kind = String _; _ } -> true | _ -> false)
    with
    (* If it can be a string, let it be a string *)
    | Some s -> type_of_element ~qualifier s
    (* Otherwise let it be the first thing it could be *)
    (* TODO What if elems is empty? *)
    | _ -> type_of_element ~qualifier (List.hd elems)

  and record_label
    : type_name:string -> string * Json_schema.element * bool * _ -> label_declaration * type_declaration list =
    fun ~type_name (field_name, element, required, _) ->
    let fname = AstExt.to_identifier field_name in
    let pld_type, declarations =
      let field_type, decls = type_of_element ~qualifier:fname element in
      let field_type = if required then field_type else [%type: [%t field_type] option] in
      field_type, decls
    in
    let pld_attributes =
      let doc_attr =
        match element.description with
        | None -> []
        | Some d -> [ AstExt.attr_str ~name:"ocaml.doc" d ]
      in
      let key_attr =
        (* ppx_json_conv *)
        [ AstExt.attr_str ~name:"key" field_name ]
      in
      let qualifier_attrs =
        if required then (* ppx_make *)
          [ AstExt.attr ~name:"required" ]
        else
          (* ppx_json_conv *)
          [ AstExt.attr_ident ~name:"default" "None" ]
      in
      doc_attr @ key_attr @ qualifier_attrs
    in
    let pld_name =
      if String.equal type_name "t" then n fname
      else
        (* Prefix each field label with the type name to avoid
           label clashes between different types *)
        n (type_name ^ "_" ^ fname)
    in
    let label_type = {pld_type; pld_attributes; pld_name; pld_mutable = Immutable; pld_loc = loc;} in
    label_type, declarations

  and type_decl_of_object
    : name:string -> Json_schema.object_specs -> type_declaration list =
    (* TODO Support for ignored aspects of spec? *)
    fun ~name { properties; _ } ->
      match properties with
      | [] ->
        let manifest = [%type: Yojson.Safe.t] in
        let attributes = deriving_attrs ~is_record:false in
        [ AstExt.Type.decl name ~manifest ~attributes ]
      | _ ->
        let labels, decls =
          properties |> ListLabels.fold_right
            ~init:([], [])
            ~f:(fun property (labels, decls) ->
                let (label, decl) = record_label ~type_name:name property in
                label :: labels, decl @ decls)
        in
        let kind = Ptype_record labels in
        let attributes = deriving_attrs ~is_record:true in
        decls @ [AstExt.Type.decl name ~kind ~attributes]

  let type_declarations_of_schema
    : Openapi_spec.schema -> type_declaration list
    = fun schema ->
      let name = "t" in
      let root =  Json_schema.root schema.schema  in
      match root.kind with
      | Object specs -> type_decl_of_object ~name specs
      | _ ->
        let attributes = deriving_attrs ~is_record:false in
        let typ, decls = type_of_element ~qualifier:name root in
        decls @ [AstExt.Type.decl name ~attributes ~manifest:typ]

  let data_module_of_schema_entry : string * Http_spec.schema -> structure_item
      =
   fun (name, schema) ->
    let name = n (Some (Camelsnakekebab.upper_camel_case name)) in
    (* TODO: NEXT add generation of "to_multipart" *)
    let type_declarations =
      [ Ast.pstr_type Recursive (type_declarations_of_schema schema.schema) ]
    in
    let to_multipart = match schema.kind with
      | `Multipart_form -> []
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
      Ast.pmod_structure structure_items
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

    let rec string_conv_and_format_of_elem_kind :
        Json_schema.element_kind -> expression * string option = function
      (* Unsupported schemas *)
      | Id_ref _ ->
          raise
            (Invalid_argument
               "string_conv_and_format_of_elem_kind given id_ref schema")
      | Ext_ref _ ->
          raise
            (Invalid_argument
               "string_conv_and_format_of_elem_kind given ext_ref schema")
      | Dummy ->
          raise
            (Invalid_argument
               "string_conv_and_format_of_elem_kind given dummy schema")
      | Array (_, _) ->
          raise
            (Invalid_argument
               "string_conv_and_format_of_elem_kind given het-array")
      | Object _ ->
          raise
            (Invalid_argument "string_conv_and_format_of_elem_kind given object")
      | Def_ref _ ->
          raise
            (Invalid_argument
               "string_conv_and_format_of_elem_kind given def-ref")
      | Monomorphic_array (_, _) ->
          (* TODO Why are we not currently supporting array? *)
          raise
            (Invalid_argument "string_conv_and_format_of_elem_kind given array")
      | Any ->
          (* TODO Why not accept JSON here? *)
          raise
            (Invalid_argument "string_conv_and_format_of_elem_kind given any")
      (* Supported schemas *)
      | Null -> ([%expr fun _ -> "null"], None)
      | Boolean -> ([%expr string_of_bool], None)
      | Integer _ -> ([%expr string_of_int], None)
      | Number _ -> ([%expr string_of_float], None)
      | String s -> ([%expr fun x -> x], s.str_format)
      | Combine (_, []) ->
          raise
            (Invalid_argument
               "param_of_json_schema_element given combine with empty types")
      | Combine (Json_schema.All_of, _) ->
          raise
            (Invalid_argument
               "param_of_json_schema_element given combine all_of")
      | Combine (Json_schema.Not, _) ->
          raise
            (Invalid_argument "param_of_json_schema_element given combine not")
      | Combine ((One_of | Any_of), elems) -> (
          (* If it can be a string, let it be a string *)
          match
            elems
            |> List.find_opt (function
                 | Json_schema.{ kind = String _; _ } -> true
                 | _ -> false)
          with
          | Some s -> string_conv_and_format_of_elem_kind s.kind
          (* Otherwise let it be the first thing it could be *)
          | _ -> string_conv_and_format_of_elem_kind (List.hd elems).kind)

    let param_of_element :
        name:string -> optional:bool -> Json_schema.element -> param =
     fun ~name ~optional elem ->
      let var = Ast.evar (AstExt.to_identifier name) in
      let pat = Ast.pvar (AstExt.to_identifier name) in
      let to_string, format = string_conv_and_format_of_elem_kind elem.kind in
      let default =
        elem.default
        |> Option.map (Json_repr.any_to_repr (module Json_repr.Yojson))
        |> Option.map (function
             | `String s -> Ast.estring s
             | `Bool b -> Ast.ebool b
             | `Int i -> (
                 (* JS doesn't distinguish between `Number` "types", so we have be sure we
                    decode the default correctly in case it is given in an integer form when
                    it should be a float. *)
                 match elem.kind with
                 | Integer _ -> Ast.eint i
                 | Number _ -> Ast.efloat (string_of_float (float_of_int i))
                 | _ ->
                     raise
                       (Invalid_data
                          "Numerical value given as default for schema \
                           specifying non-numerical type"))
             | `Float f -> Ast.efloat (string_of_float f)
             | unsupported_default ->
                 failwith
                   (Printf.sprintf
                      "default value %s not supported for parameter  %s"
                      (Yojson.Safe.to_string unsupported_default)
                      name))
      in
      { name; var; pat; optional; default; to_string; format }

    let of_http_spec_param : string * H.Message.Params.param -> param =
     fun (name, p) ->
      let var = Ast.evar name in
      let pat = Ast.pvar name in
      let optional = not p.required in
      let default, to_string =
        let s = p.schema.schema in
        let to_string, typ =
          match DataModule.type_declarations_of_schema s with
          | [] -> failwith ("no schema specified for parameter " ^ name)
          | _ :: _ :: _ ->
              failwith ("multiple schemas specified for parameter " ^ name)
          | [ typ_decl ] -> (
              match typ_decl.ptype_manifest with
              | None -> failwith ("unsupported type for parameter " ^ name)
              | Some t -> (
                  match t with
                  | [%type: string] -> ([%expr fun x -> x], t)
                  | [%type: bool] -> ([%expr string_of_bool], t)
                  | [%type: int] -> ([%expr string_of_int], t)
                  | [%type: float] -> ([%expr string_of_float], t)
                  | unsupported_typ ->
                      failwith
                        (Printf.sprintf
                           "parmamters of type %s not supported for param %s"
                           (string_of_core_type unsupported_typ)
                           name)))
        in
        let default =
          (Json_schema.root s.schema).default
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

  let response_decoder (responses : H.Message.Responses.t) : expression =
    (* TODO What to do with this unsupported thing? *)
    match responses |> List.assoc_opt `OK with
    | None ->
        (* TODO This is only required if it is the ONLY response. So need to account for others *)
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
        | `Html -> [%expr fun x -> x]
        | `Multipart_form -> raise (Failure "TODO"))

  let path_parts path : expression =
    path
    |> List.map (function `C c -> Ast.estring c | `P p -> Ast.evar p)
    |> Ast.elist

  let query_params (params : Params.param list) =
    params
    |> List.map (fun Params.{ name; to_string; var; _ } ->
           [%expr [%e Ast.estring name], [%e to_string] [%e var]])
    |> Ast.elist

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

  (* (\* Constructs a method that will construct multipart form data. *\) *)
  (* let form_part_data : H.schema -> expression = *)
  (*  fun schema -> *)
  (*  let json_conv = "Data." AstExt.to_module_name schema.name in *)
  (*   let element = Json_schema.root schema in *)
  (*   let properties = *)
  (*     match element.kind with *)
  (*     | Object specs -> specs.properties *)
  (*     | _ -> *)
  (*         raise *)
  (*           (Invalid_data *)
  (*              "Multipart form data is not specified by a schema object") *)
  (*   in *)
  (*   let property_to_part *)
  (*       ((name, elem, required, _) : string * Json_schema.element * bool * _) = *)
  (*     (\** This is not safe *\) *)
  (*     let field = Camelsnakekebab.lower_snake_case name in *)
  (*     let format v = match elem.format with *)
  (*       | Some "binary" -> Ast.pexp_variant "File" (Some v) (\* `File v *\) *)
  (*       | _ -> Ast.pexp_variant "String" (Some v) (\* `String v *\) *)
  (*     in *)
  (*     let conv = match elem.kind with *)
  (*     | Boolean -> [%expr string_of_bool] *)
  (*     | Integer _ -> [%expr string_of_int] *)
  (*     | Number _ -> [%expr string_of_float] *)
  (*     | String _ -> [%expr fun x -> x] *)
  (*     | _ -> failwith ("TODO Unsupported parameter type in multipart form spec in field" ^ field) *)
  (*     in *)
  (*     let value = match required, elem.default with *)
  (*       | true, _ -> Ast.pexp_field [%expr data] AstExt.to *)
  (*     in *)
  (*     _ *)

  (*   in *)
  (*   let optional_part_data = *)
  (*     element.kind *)
  (*     |> List.map (fun (param : Params.param) -> *)
  (*            let name = Ast.estring param.name in *)
  (*            (\* If we have an optional param with no default, we need to map it from an option type *\) *)
  (*            if param.optional && Option.is_none param.default then *)
  (*              match param.format with *)
  (*              | Some "binary" -> *)
  (*                  [%expr *)
  (*                    Option.map (fun v -> `File ([%e name], v)) [%e param.var]] *)
  (*              | _ -> *)
  (*                  [%expr *)
  (*                    Option.map *)
  (*                      (fun v -> `String ([%e name], [%e param.to_string] v)) *)
  (*                      [%e param.var]] *)
  (*            else *)
  (*              match param.format with *)
  (*              | Some "binary" -> *)
  (*                  [%expr Some (`File ([%e name], [%e param.var]))] *)
  (*              | _ -> *)
  (*                  [%expr *)
  (*                    Some *)
  (*                      (`String *)
  (*                        ([%e name], [%e param.to_string] [%e param.var]))]) *)
  (*     |> Ast.elist *)
  (*   in *)
  (*   (\* Just take the parts that where defined *\) *)
  (*   [%expr List.filter_map (fun x -> x) [%e optional_part_data]] *)

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
    let decode_resp = response_decoder message.resp in
    let meth = Ast.pexp_variant (Http.Method.to_string message.req.meth) None in
    let body =
      [%expr
        make_request ?data:[%e data_conv] ~base_url
          ~path:[%e path_parts message.req.path]
          ~params:[%e query_params params.query]
          ~headers:[%e extra_headers params.header]
          ~decode:[%e decode_resp] [%e meth]]
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
        let decls = [ [%stri open Oooapi_lib.Cohttp_client (Config)] ] in
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
  :: DataModule.of_schemata spec.schemata
  :: EndpointsModule.of_messages spec.messages spec.base_url
