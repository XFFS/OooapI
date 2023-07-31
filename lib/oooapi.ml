(* re-exported just for testing *)
module DAG = DAG
open Ppxlib
open AstUtil

(* TODO some objects are not being converted correctly, e.g., the `Engine` schema *)
module DataModule = struct
  (* TODO Account for constraints, like pattern, min/max format etc.?
     This could be thru abstract types or thru validators in constructors *)

  (* TODO: Add support for ppx_make derivation *)
  (* let deriving_attrs = *)
  (*   [ AstExt.attr_ident ~name:"deriving" "yojson {strict = false}" ] *)

  let deriving_attrs ~is_record =
    if is_record then
      [ Ast.attribute
          ~name:(n "deriving")
          ~payload:(PStr [ [%stri yojson { strict = false }, make] ])
      ]
    else
      [ Ast.attribute
          ~name:(n "deriving")
          ~payload:(PStr [ [%stri yojson { strict = false }] ])
      ]

  let rec module_name_of_def_ref : Json_query.path -> string = function
    | [] -> failwith "unsupported component query path"
    | [ `Field n ] -> Camelsnakekebab.upper_camel_case n
    | _ :: rest -> module_name_of_def_ref rest

  let type_name_of_def_ref : Json_query.path -> string =
   fun p -> module_name_of_def_ref p ^ ".t"

  (* TODO refactor this mess! *)
  let type_declarations_of_schema : Openapi_spec.schema -> type_declaration list
      =
   fun schema ->
    (* We need to form and gather all declarations implied by the schema
       and we use this ref to collect them without having to thread the data thru *)
    let declarations = ref [] in
    let root = Json_schema.root schema in
    let rec gather_declarations :
        type_name:string -> Json_schema.element -> unit =
     fun ~type_name element ->
      (* `qualifier` is used to construct names for types when new declarations are needed *)
      let rec type_of_element :
          qualifier:string -> Json_schema.element -> core_type =
       fun ~qualifier element ->
        match (element.kind : Json_schema.element_kind) with
        (* Unsupported schemas *)
        | Id_ref _ -> failwith "unsupported: id_ref schema"
        | Ext_ref _ -> failwith "unsupported: ext_ref schema"
        | Dummy -> failwith "unsupported: dummy schema"
        | Array (_, _) -> failwith "unsupported: het-array"
        (* Supported schemas *)
        | Any -> [%type: Yojson.Safe.t]
        | Null -> [%type: unit]
        | Boolean -> [%type: bool]
        | Integer _ -> [%type: int]
        | Number _ -> [%type: float]
        | String _ -> [%type: string]
        | Combine _ -> [%type: Yojson.Safe.t]
        | Def_ref path ->
            AstExt.typ (AstExt.typ_constr (type_name_of_def_ref path))
        | Monomorphic_array (e, _) ->
            let item_type_name = qualifier ^ "_item" in
            let item_type = type_of_element ~qualifier:item_type_name e in
            [%type: [%t item_type] list]
        | Object o ->
        match o.properties with
        | [] -> [%type: Yojson.Safe.t]
        | _ :: _ ->
            gather_declarations ~type_name:qualifier element;
            AstExt.typ (AstExt.typ_constr qualifier)
      in
      let type_decl_of_object :
          name:string -> Json_schema.object_specs -> type_declaration =
        let record_label :
               type_name:string
            -> string * Json_schema.element * bool * _
            -> label_declaration =
         fun ~type_name (field_name, element, required, _) ->
          let fname = AstExt.to_identifier field_name in
          let pld_type =
            let field_type = type_of_element ~qualifier:fname element in
            if required then
              field_type
            else
              [%type: [%t field_type] option]
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
              if required then
                (* ppx_make *)
                [ AstExt.attr ~name:"required" ]
              else
                (* ppx_json_conv *)
                [ AstExt.attr_ident ~name:"default" "None" ]
            in
            doc_attr @ key_attr @ qualifier_attrs
          in
          let pld_name =
            if String.equal type_name "t" then
              n fname
            else
              (* Prefix each field label with the type name to avoid
                 label clashes between different types *)
              n (type_name ^ "_" ^ fname)
          in
          { pld_type
          ; pld_attributes
          ; pld_name
          ; pld_mutable = Immutable
          ; pld_loc = loc
          }
        in
        (* TODO Support for ignored aspects of spec? *)
        fun ~name { properties; _ } ->
          match properties with
          | [] ->
              AstExt.typ_decl
                type_name
                ~manifest:[%type: Yojson.Safe.t]
                ~attributes:(deriving_attrs ~is_record:false)
          | ps ->
              AstExt.typ_decl
                type_name
                ~kind:
                  (Ptype_record (List.map (record_label ~type_name:name) ps))
                ~attributes:(deriving_attrs ~is_record:true)
      in
      let decl =
        let attributes = deriving_attrs ~is_record:false in
        match (element.kind : Json_schema.element_kind) with
        | Combine (_, _)
        | Any ->
            AstExt.typ_decl "t" ~attributes ~manifest:[%type: Yojson.Safe.t]
        | Null -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: unit]
        | Boolean -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: bool]
        | String _ -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: string]
        | Integer _ -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: int]
        | Number _ -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: float]
        | Object o -> type_decl_of_object ~name:type_name o
        | Monomorphic_array (elem, _) ->
            gather_declarations ~type_name:"item" elem;
            AstExt.typ_decl "t" ~attributes ~manifest:[%type: item list]
        (* TODO: Support for one_of *)
        (* TODO: Error on unsupported types? Or better to leave placeholder..  *)
        | Def_ref _
        | Id_ref _
        | Ext_ref _
        | Array (_, _)
        | Dummy ->
            AstExt.typ_decl
              type_name
              ~kind:
                (Ptype_variant
                   [ Ast.constructor_declaration
                       ~name:(n "Unimplemented_type")
                       ~args:(Pcstr_tuple [])
                       ~res:None
                   ])
      in
      declarations := decl :: !declarations
    in
    gather_declarations ~type_name:"t" root;
    !declarations

  let data_module_of_schema_entry :
      string * Openapi_spec.schema -> structure_item =
   fun (name, schema) ->
    let name = n (Some (Camelsnakekebab.upper_camel_case name)) in
    let type_declarations =
      [ Ast.pstr_type Recursive (type_declarations_of_schema schema) ]
    in
    let expr = Ast.pmod_structure type_declarations in
    Ast.module_binding ~name ~expr |> Ast.pstr_module

  (* Why don't we need the full path? *)
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
      | Array (es, _)
      | Combine (_, es) ->
          List.fold_left (fun acc e -> acc @ gather_deps e) [] es
      (* TODO These should be accounted for at some point *)
      | Id_ref _
      | Ext_ref _
      | String _
      | Integer _
      | Number _
      | Boolean
      | Null
      | Any
      | Dummy ->
          []
    in
    Json_schema.root schema |> gather_deps

  module Graph = DAG.Make (String)

  let of_components (components : Openapi_spec.components option) =
    let name = Ast.Located.mk (Some "Data") in
    let expr =
      let structure_items =
        let schemas =
          match components with
          | None -> []
          | Some c -> c.schemas |> Option.value ~default:[]
        in
        let dep_ordering =
          let dep_graph =
            (* Build a dependency graph of each component *)
            schemas
            |> ListLabels.fold_left
                 ~init:Graph.empty
                 ~f:(fun graph (src, schema) ->
                   (* Add each schema and its deps to the graph *)
                   let deps = schema_deps schema in
                   Graph.add_arcs ~src deps graph)
          in
          let sorted_rev = Graph.topological_sort dep_graph in
          List.rev sorted_rev
        in
        dep_ordering
        |> List.map (fun label ->
               data_module_of_schema_entry (label, List.assoc label schemas))
      in
      Ast.pmod_structure structure_items
    in

    Ast.module_binding ~name ~expr |> Ast.pstr_module
end

module EndpointsModule = struct
  module Params = struct
    type param =
      { name : string
      ; var : expression
      ; pat : pattern
      ; to_string : expression
      ; optional : bool
      ; default : expression option
      ; format : string option (* E.g., "binary" *)
      }

    type t =
      { query : param list
      ; path : param list
      ; header : param list
      ; form : param list
      }

    let empty = { query = []; path = []; header = []; form = [] }
    let to_list { query; path; header; form } = query @ path @ header @ form

    let param_of_element :
        name:string -> optional:bool -> Json_schema.element -> param =
     fun ~name ~optional elem ->
      let var = Ast.evar (AstExt.to_identifier name) in
      let pat = Ast.pvar (AstExt.to_identifier name) in
      let to_string, format =
        match (elem.kind : Json_schema.element_kind) with
        (* Unsupported schemas *)
        | Id_ref _ ->
            raise
              (Invalid_argument
                 "param_of_json_schema_element given id_ref schema")
        | Ext_ref _ ->
            raise
              (Invalid_argument
                 "param_of_json_schema_element given ext_ref schema")
        | Dummy ->
            raise
              (Invalid_argument
                 "param_of_json_schema_element given dummy schema")
        | Array (_, _) ->
            raise
              (Invalid_argument "param_of_json_schema_element given het-array")
        | Object _ ->
            raise (Invalid_argument "param_of_json_schema_element given object")
        | Def_ref _ ->
            raise
              (Invalid_argument "param_of_json_schema_element given def-ref")
        | Monomorphic_array (_, _) ->
            raise (Invalid_argument "param_of_json_schema_element given array")
        (* Supported schemas *)
        | Any ->
            raise (Invalid_argument "param_of_json_schema_element given any")
        | Combine _ ->
            raise
              (Invalid_argument "param_of_json_schema_element given combine")
        | Null -> ([%expr fun _ -> "null"], None)
        | Boolean -> ([%expr string_of_bool], None)
        | Integer _ -> ([%expr string_of_int], None)
        | Number _ -> ([%expr string_of_float], None)
        | String s -> ([%expr fun x -> x], s.str_format)
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
                   match elem.kind with
                   | Number _ -> Ast.efloat (string_of_float (float_of_int i))
                   | _ -> Ast.eint i)
               | `Float f -> Ast.efloat (string_of_float f)
               | unsupported_default ->
                   failwith
                     (Printf.sprintf
                        "default value %s not supported for parameter  %s"
                        (Yojson.Safe.to_string unsupported_default)
                        name))
      in
      { name; var; pat; optional; default; to_string; format }

    let of_openapi_parameter : Openapi_spec.parameter -> param =
     fun p ->
      let var = Ast.evar p.name in
      let pat = Ast.pvar p.name in
      let optional = not p.required in
      let default, to_string =
        match p.schema with
        (* TODO should the be an error? Or is defaulting to string right? *)
        | None -> (None, var)
        | Some s ->
            let to_string, typ =
              match DataModule.type_declarations_of_schema s with
              | [] -> failwith ("no schema specified for parameter " ^ p.name)
              | _ :: _ :: _ ->
                  failwith ("multiple schemas specified for parameter " ^ p.name)
              | [ typ_decl ] ->
              match typ_decl.ptype_manifest with
              | None -> failwith ("unsupported type for parameter " ^ p.name)
              | Some t ->
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
                       p.name)
            in
            let default =
              (Json_schema.root s).default
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
                              "default parmamters value %s not supported for \
                               param %s"
                              (Yojson.Safe.to_string unsupported_default)
                              p.name))
            in
            (default, to_string)
      in
      let default =
        if not optional then
          None
        else
          default
      in
      { name = p.name; optional; default; to_string; var; pat; format = None }

    let of_openapi_parameters :
        Openapi_spec.parameter Openapi_spec.or_ref list option -> t =
     fun params ->
      match params with
      | None -> empty
      | Some ps ->
          ps
          |> ListLabels.fold_right
               ~init:{ query = []; path = []; header = []; form = [] }
               ~f:(fun i acc ->
                 match i with
                 | `Ref (r : Openapi_spec.reference) ->
                     failwith
                       (Printf.sprintf
                          "references not yet supported for params, given \
                           param ref %s"
                          r.ref_)
                 | `Obj (p : Openapi_spec.parameter) ->
                 match p.in_ with
                 | "query" ->
                     { acc with query = of_openapi_parameter p :: acc.query }
                 | "path" ->
                     { acc with path = of_openapi_parameter p :: acc.path }
                 | "header" ->
                     { acc with header = of_openapi_parameter p :: acc.header }
                 | unsupported ->
                     failwith ("unsupported parameter location " ^ unsupported))
  end

  type operation_function =
    Openapi_spec.path -> Openapi_spec.operation -> structure_item

  let operation_function_name (operation : Openapi_spec.operation) path =
    operation.operationId
    |> Option.value ~default:(Openapi_spec.Openapi_path.to_string path)
       (* TODO fail in missing id? *)
    |> AstExt.to_identifier
    |> AstExt.Pat.var

  let data_module_fun_name name f = Printf.sprintf "Data.%s.%s" name f

  let to_json_fun mod_name : expression =
    let mod_name = Camelsnakekebab.upper_camel_case mod_name in
    Ast.evar (data_module_fun_name mod_name "to_yojson")

  let response_decoder (operation : Openapi_spec.operation) : expression =
    let unsupported = [%expr fun x -> Ok x] in
    match operation.responses |> List.assoc_opt "200" with
    | None ->
        (* https://spec.openapis.org/oas/v3.1.0#responses-object *)
        failwith "Invalid schema: no 200 response"
    | Some r ->
    match Option.bind r.content (List.assoc_opt "application/json") with
    | None -> unsupported
    | Some m ->
    match m.schema with
    | None -> unsupported
    | Some s ->
    match (Json_schema.root s).kind with
    | Def_ref p ->
        let mod_name = DataModule.module_name_of_def_ref p in
        let conv_fun = Ast.evar (data_module_fun_name mod_name "of_yojson") in
        [%expr of_json_string [%e conv_fun]]
    (* TODO handle other schemas gracefully *)
    | _ -> unsupported

  let path_parts path : expression =
    path
    |> List.map (function
           | `C c -> Ast.estring c
           | `P p -> Ast.evar p)
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
    params
    |> Params.to_list
    |> ListLabels.fold_right
         ~init:(Exp.f data body) (* the last, innermost function *)
         ~f:(fun (param : Params.param) body' ->
           Exp.f
             ~label:param.name
             ~optional:param.optional
             ?default:param.default
             param.pat
             body')

  let form_part_data : Params.param list -> expression =
   fun params ->
    let optional_part_data =
      params
      |> List.map (fun (param : Params.param) ->
             let name = Ast.estring param.name in
             (* If we have an optional param with no default, we need to map it from an option type *)
             if param.optional && Option.is_none param.default then
               match param.format with
               | Some "binary" ->
                   [%expr
                     Option.map (fun v -> `File ([%e name], v)) [%e param.var]]
               | _ ->
                   [%expr
                     Option.map
                       (fun v -> `String ([%e name], [%e param.to_string] v))
                       [%e param.var]]
             else
               match param.format with
               | Some "binary" ->
                   [%expr Some (`File ([%e name], [%e param.var]))]
               | _ ->
                   [%expr
                     Some
                       (`String
                         ([%e name], [%e param.to_string] [%e param.var]))])
      |> Ast.elist
    in
    (* Just take the parts that where defined *)
    [%expr List.filter_map (fun x -> x) [%e optional_part_data]]

  (* Extract the Ref path from a schema, if it has one *)
  let ref_of_schema : Json_schema.schema -> string option =
   fun s ->
    match (Json_schema.root s).kind with
    | Def_ref path -> Some (DataModule.json_query_path_terminal path)
    | _ -> None

  let data_conv_and_param ?(name = "<no operation id>") :
         Openapi_spec.components
      -> Openapi_spec.request_body Openapi_spec.or_ref option
      -> expression * pattern * Params.param list =
    let from_json = function
      | None -> failwith ("Content type is missing schema: " ^ name)
      | Some s ->
      match ref_of_schema s with
      | None -> failwith ("only Ref_def supported for Json currently :" ^ name)
      | Some schema_name ->
          ( [%expr Some (`Json ([%e to_json_fun schema_name] data))]
          , [%pat? data]
          , [] )
    and from_multipart (components : Openapi_spec.components) = function
      | None -> failwith ("Content type is missing schema: " ^ name)
      | Some s ->
      match ref_of_schema s with
      | None ->
          failwith
            ("only Ref_def supported for multipart forms currently :" ^ name)
      | Some schema_name ->
      match components.schemas with
      | None ->
          failwith
            ("Given ref to schemas but no schemas are specified for" ^ name)
      | Some schemas ->
      match List.assoc_opt schema_name schemas with
      | None ->
          failwith
            ("no schema exists for "
            ^ schema_name
            ^ " referenced in operation "
            ^ name)
      | Some form_data_schema ->
      match Json_schema.(root form_data_schema).kind with
      | Object { properties; _ } ->
          let params =
            properties
            |> List.map (fun (name, elem, required, _) ->
                   Params.param_of_element ~name ~optional:(not required) elem)
          in
          ( [%expr Some (`Multipart_form [%e form_part_data params])]
          , [%pat? ()]
          , params )
      | _ ->
          raise (Invalid_argument "non object as input for multipart form data")
    in
    fun components request_body ->
      match request_body with
      | None -> ([%expr None], [%pat? ()], [])
      | Some body ->
      match body with
      | `Ref r ->
          ([%expr Some (`Json ([%e to_json_fun r.ref_] data))], [%pat? data], [])
      | `Obj o ->
      match List.assoc_opt "application/json" o.content with
      | Some media_type -> from_json media_type.schema
      | None ->
      match List.assoc_opt "multipart/form-data" o.content with
      | Some media_type -> from_multipart components media_type.schema
      | None ->
          failwith
            ("unsupported request body (only JSON and multipart form data)"
            ^ name)

  (** A function to make a get request *)
  let get_fun : operation_function =
   fun path operation ->
    let name = operation_function_name operation path in
    let params = Params.of_openapi_parameters operation.parameters in
    let decode_resp= response_decoder operation in
    let body =
      [%expr
        make_request
          ~path:[%e path_parts path]
          ~params:[%e query_params params.query]
          ~headers:[%e extra_headers params.header]
          ~decode:[%e decode_resp]
          `GET]
    in
    [%stri let [%p name] = [%e fun_with_params ~data:Ast.punit params body]]

  (* multipart:

     - abstract function parts -> form
     - post_fun also requires components
     - check that path requires multipart
     - if so, we need to examine component:
       - all properties of component become params to function
     - each param needs must be prepared as a part option (string of file path)
     - concat all present parts into list and bass to Multipart form
      - construct header and body from form, then add all header stuff to header
  *)
  let post_fun : Openapi_spec.components -> operation_function =
   fun components path operation ->
    let name = operation_function_name operation path in
    let params = Params.of_openapi_parameters operation.parameters in
    let data_conv, data_param, form_params =
      data_conv_and_param
        ?name:operation.operationId
        components
        operation.requestBody
    in
    let params = { params with form = form_params } in
    let decode_resp = response_decoder operation in
    let body =
      [%expr
        make_request
          ?data:[%e data_conv]
          ~path:[%e path_parts path]
          ~params:[%e query_params params.query]
          ~headers:[%e extra_headers params.header]
          ~decode:[%e decode_resp]
          `POST]
    in
    [%stri let [%p name] = [%e fun_with_params ~data:data_param params body]]

  let endpoint :
         Openapi_spec.components
      -> Openapi_spec.path * Openapi_spec.path_item
      -> structure_item list =
   fun components (path, path_item) ->
    [ (get_fun path, path_item.get)
    ; (post_fun components path, path_item.post)
    ]
    |> List.filter_map (fun (f, operation) -> Option.map f operation)

  (** Construct the AST node of the module with all endpoint functions *)
  let of_paths :
         Openapi_spec.components
      -> string * Openapi_spec.paths
      -> structure_item list =
   fun components (base_uri, endpoints) ->
    let endpoint_functor =
      let functor_param =
        Named
          ( n (Some "Config")
          , Ast.pmty_ident (n (Astlib.Longident.parse "Config")) )
      in
      let mod_impl =
        let decls =
          [ [%stri
              include
                EndpointLib
                  (struct
                    let uri = [%e Ast.estring base_uri]
                  end)
                  (Config)]
          ]
        in
        let endpoint_functions =
          endpoints |> List.map (endpoint components) |> List.flatten
        in
        Ast.pmod_structure (decls @ endpoint_functions)
      in
      Ast.pmod_functor functor_param mod_impl
    in
    Oooapi_lib.endpoint
    @ [ Ast.module_binding ~name:(n (Some "Endpoint")) ~expr:endpoint_functor
        |> Ast.pstr_module
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
  Printf.sprintf
    "Generated by oooapi at %i-%i-%iT%i:%i:%iZ"
    (tm_year + 1900)
    (tm_mon + 1)
    tm_mday
    tm_hour
    tm_min
    tm_sec

let module_of_spec : Openapi_spec.t -> Ppxlib.Ast.structure =
 fun spec ->
  let base_uri =
    match spec.servers with
    | None
    | Some [] ->
        failwith "No servers specified"
    | Some (s :: _) -> s
  in
  (* TODO safe handling of optional components *)
  [%stri let __NOTE__ = [%e Ast.estring (notice ())]]
  :: DataModule.of_components spec.components
  :: EndpointsModule.of_paths
       (Option.get spec.components)
       (base_uri.url, Option.value ~default:[] spec.paths)
