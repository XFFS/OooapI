(* re-exported just for testing *)
module DAG = DAG
open Ppxlib
open AstUtil

(* TODO some objects are not being converted correctly, e.g., the `Engine` schema *)
module DataModule = struct
  (* TODO Account for constraints, like pattern, min/max format etc.?
     This could be thru abstract types or thru validators in constructors *)

  (* TODO: Add support for ppx_make derivation *)
  let deriving_attrs =
    [ AstExt.attr_ident ~name:"deriving" "yojson"  ]

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
      let type_decl_of_object : Json_schema.object_specs -> type_declaration =
        let record_label :
            string * Json_schema.element * bool * _ -> label_declaration =
         fun (field_name, element, required, _) ->
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
              if OcamlBuiltins.is_keyword field_name then
                (* ppx_json_conv *)
                [ AstExt.attr_str ~name:"key" field_name ]
              else
                []
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
          { pld_type
          ; pld_attributes
          ; pld_name = n fname
          ; pld_mutable = Immutable
          ; pld_loc = loc
          }
        in
        (* TODO Support for ignored aspects of spec? *)
        fun { properties; _ } ->
          let attributes = deriving_attrs in
          match properties with
          | [] ->
              AstExt.typ_decl
                type_name
                ~manifest:[%type: Yojson.Safe.t]
                ~attributes
          | ps ->
              AstExt.typ_decl
                type_name
                ~kind:(Ptype_record (List.map record_label ps))
                ~attributes
      in
      let decl =
        let attributes = deriving_attrs in
        match (element.kind : Json_schema.element_kind) with
        | Any ->
            AstExt.typ_decl "t" ~attributes ~manifest:[%type: Yojson.Safe.t]
        | Null -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: unit]
        | Boolean -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: bool]
        | String _ -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: string]
        | Integer _ -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: int]
        | Number _ -> AstExt.typ_decl "t" ~attributes ~manifest:[%type: float]
        | Object o -> type_decl_of_object o
        | Monomorphic_array (elem, _) ->
            gather_declarations ~type_name:"item" elem;
            AstExt.typ_decl "t" ~attributes ~manifest:[%type: item list]
        (* TODO: Error on unsupported types? Or better to leave placeholder..  *)
        | Combine (_, _)
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
            (* Build a debendency graph of each component *)
            schemas
            |> ListLabels.fold_left
                 ~init:Graph.empty
                 ~f:(* Add each schema to the graph *)
                 (fun graph (src, schema) ->
                   schema
                   |> schema_deps
                   |> ListLabels.fold_left
                        ~init:graph
                        ~f:(* Add each of the schema's dependencies as nodes *)
                        (fun graph' dst -> Graph.add_arc ~src ~dst graph'))
          in
          dep_graph |> Graph.topological_sort |> List.rev
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
  (* Construct the AST node of an endpoint function *)
  (* let endpoint (fname, uri, data) : structure_item = *)
  (*   let function_name = Ast.ppat_var (n fname) in *)
  (*   let uri = Ast.estring uri in *)
  (*   let data = Ast.estring data in *)
  (*   [%stri let [%p function_name] = Client.request [%e uri] [%e data]] *)

  type operation_function =
    Openapi_spec.path -> Openapi_spec.operation -> structure_item

  let operation_function_name (operation : Openapi_spec.operation) path =
    operation.operationId
    |> Option.value ~default:(Openapi_spec.Openapi_path.to_string path)
       (* TODO fail in missing id? *)
    |> AstExt.to_identifier
    |> AstExt.Pat.var

  let to_json mod_name : expression =
    let mod_name = Camelsnakekebab.upper_camel_case mod_name in
    Ast.(
      eapply (evar (Printf.sprintf "%s.to_yojson" mod_name)) [ [%expr data] ])

  module Params = struct
    type param =
      { name : string
      ; var : expression
      ; pat : pattern
      ; as_string : expression
      ; optional : bool
      ; default : expression option
      }

    type t =
      { query : param list
      ; path : param list
      ; header : param list
      }

    let empty = { query = []; path = []; header = [] }

    let of_openapi_parameter : Openapi_spec.parameter -> param =
     fun p ->
      let var = Ast.evar p.name in
      let pat = Ast.pvar p.name in
      let default, as_string =
        match p.schema with
        (* TODO should the be an error? Or is defaulting to string right? *)
        | None -> (None, var)
        | Some s ->
            let as_string =
              match DataModule.type_declarations_of_schema s with
              | [] -> failwith ("no schema specified for parameter " ^ p.name)
              | _ :: _ :: _ ->
                  failwith ("multiple schemas specified for parameter " ^ p.name)
              | [ typ_decl ] ->
              match typ_decl.ptype_manifest with
              | None -> failwith ("unsupported type for parameter " ^ p.name)
              | Some t ->
              match t with
              | [%type: string] -> var
              | [%type: bool] -> [%expr string_of_bool [%e var]]
              | [%type: int] -> [%expr string_of_int [%e var]]
              | [%type: float] -> [%expr string_of_float [%e var]]
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
                     | `Int i -> Ast.eint i
                     | `Float f -> Ast.efloat (string_of_float f)
                     | unsupported_default ->
                         failwith
                           (Printf.sprintf
                              "default parmamters value %s not supported for \
                               param %s"
                              (Yojson.Safe.to_string unsupported_default)
                              p.name))
            in
            (default, as_string)
      in
      let optional, default =
        match (p.required, default) with
        | true, _ -> (false, None)
        | false, None -> (true, None)
        | false, Some default_expr -> (true, Some default_expr)
      in
      { name = p.name; optional; default; as_string; var; pat }

    let of_openapi_parameters :
        Openapi_spec.parameter Openapi_spec.or_ref list -> t =
     fun params ->
      params
      |> ListLabels.fold_right
           ~init:{ query = []; path = []; header = [] }
           ~f:(fun i acc ->
             match i with
             | `Ref (r : Openapi_spec.reference) ->
                 failwith
                   (Printf.sprintf
                      "references not yet supported for params, given param \
                       ref %s"
                      r.ref_)
             | `Obj (p : Openapi_spec.parameter) ->
             match p.in_ with
             | "query" ->
                 { acc with query = of_openapi_parameter p :: acc.query }
             | "path" -> { acc with path = of_openapi_parameter p :: acc.path }
             | "header" ->
                 { acc with header = of_openapi_parameter p :: acc.header }
             | unsupported ->
                 failwith ("unsupported parameter location " ^ unsupported))
  end

  let let_uri path (params : Params.t) =
    let parts_list =
      path
      |> List.map (function
             | `C c -> Ast.estring c
             | `P p -> Ast.evar p)
      |> Ast.elist
    in
    let query_params =
      params.query
      |> List.map (fun Params.{ name; as_string; _ } ->
             [%expr [%e Ast.estring name], [%e as_string]])
      |> Ast.elist
    in
    ( [%pat? uri]
    , [%expr
        base_uri :: [%e parts_list] |> String.concat "/" |> Uri.of_string
        |> fun u -> Uri.add_query_params' u [%e query_params]] )

  let let_headers (params : Params.t) =
    let header_params =
      params.header
      |> List.map (fun (p : Params.param) ->
             [%expr [%e Ast.estring p.name], [%e p.as_string]])
      |> Ast.elist
    in
    ( [%pat? headers]
    , [%expr Cohttp.Header.add_list Config.default_headers [%e header_params]]
    )

  let fun_with_params ~data (params : Params.t) body =
    let open AstExt in
    params.path @ params.query @ params.header
    |> ListLabels.fold_right
         ~init:(Exp.f data body) (* the last, innermost function *)
         ~f:(fun (param : Params.param) body' ->
           Exp.f
             ~label:param.name
             ~optional:param.optional
             ?default:param.default
             param.pat
             body')

  let get_fun : operation_function =
   fun path operation ->
    let open AstExt in
    let name = operation_function_name operation path in
    let data, to_json =
      match operation.requestBody with
      | None -> (Ast.punit, [])
      | Some body ->
      match body with
      | `Obj _ -> failwith "TODO: non ref requestBody is not yet supported"
      | `Ref r -> ([%pat? data], [ ([%pat? "body"], to_json r.ref_) ])
    in
    let params =
      match operation.parameters with
      | None -> Params.empty
      | Some ps -> Params.of_openapi_parameters ps
    in
    (* TODO make request, decode, and return error value *)
    let of_yojson =
      match operation.responses |> List.assoc_opt "200" with
      | None ->
          (* https://spec.openapis.org/oas/v3.1.0#responses-object *)
          failwith "Invalid schema: no 200 response"
      | Some r ->
      match Option.bind r.content (List.assoc_opt "application/json") with
      | None -> [%expr fun _ -> ()]
      | Some m ->
      match m.schema with
      | None -> [%expr fun _ -> ()]
      | Some s ->
      match (Json_schema.root s).kind with
      | Def_ref p ->
          let data_mod = DataModule.module_name_of_def_ref p in
          Ast.evar ("Data." ^ data_mod ^ ".of_yojson")
      (* TODO handle other schemas gracefully *)
      | _ -> [%expr fun x -> x]
    in
    let make_request =
      [%expr
        let open Lwt.Syntax in
        let* resp, body = Cohttp_lwt_unix.Client.get ~headers uri in
        let+ bodystr = Cohttp_lwt.Body.to_string body in
        match resp.status with
        | `OK -> Ok (Yojson.Safe.from_string bodystr |> [%e of_yojson])
        | other -> Error (other, bodystr)]
    in
    let body =
      Exp.lets
        (to_json @ [ let_uri path params; let_headers params ])
        make_request
    in
    let fun_expr = fun_with_params ~data params body in
    [%stri let [%p name] = [%e fun_expr]]

  let endpoint :
      Openapi_spec.path * Openapi_spec.path_item -> structure_item list =
   fun (path, path_item) ->
    [ (get_fun path, path_item.get) ]
    |> List.filter_map (fun (f, operation) -> Option.map f operation)

  (* TODO Add module type as available for imports  *)
  let client_config_sig =
    Ast.pmty_signature
      [%sig:
        [@@@warning "-32"]

        val default_headers : Cohttp.Header.t
        (** Headers to be appended to every request

        Can be used, e.g., to supply bearer tokens. *)]

  (** Construct the AST node of the module with all endpoint functions *)
  let of_paths : string * Openapi_spec.paths -> structure_item =
   fun (base_uri, endpoints) ->
    let expr =
      let functor_param = Named (n (Some "Config"), client_config_sig) in
      let mod_impl =
        let decls = [ [%stri let base_uri = [%e Ast.estring base_uri]] ] in
        let endpoint_functions =
          endpoints |> List.map endpoint |> List.flatten
        in
        Ast.pmod_structure (decls @ endpoint_functions)
      in
      Ast.pmod_functor functor_param mod_impl
    in
    Ast.module_binding ~name:(n (Some "Endpoint")) ~expr |> Ast.pstr_module
end

(* TODO: name and endpoints to become all data from OpenAPI spec *)
let modules components server_endpoints =
              [ DataModule.of_components components
              ; EndpointsModule.of_paths server_endpoints
              ]

(* TODO: Put in comment *)
let header = "Generated by oooapi on DATE"

let write_ast_f fmt str =
  Format.fprintf fmt "%a\n" Pprintast.structure str;
  Format.print_flush ()

let write_ast str = write_ast_f Format.std_formatter str

let module_of_spec : Openapi_spec.t -> Ppxlib.Ast.structure =
 fun spec ->
  let base_uri =
    match spec.servers with
    | None
    | Some [] ->
        failwith "No servers specified"
    | Some (s :: _) -> s
  in
  modules
    spec.components
    (base_uri.url, Option.value ~default:[] spec.paths)
