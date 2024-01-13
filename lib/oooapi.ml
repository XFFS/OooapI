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
    | [] -> failwith "invalid component query path"
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
    let root = Json_schema.root schema.schema in
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
        (* Supported schemas *)
        | Array (_, _) ->
            [%type: Yojson.Safe.t]
            (* TODO: If min and max items are equal, this can be a tuple *)
        | Any -> [%type: Yojson.Safe.t]
        | Null -> [%type: unit]
        | Boolean -> [%type: bool]
        | Integer _ -> [%type: int]
        | Number _ -> [%type: float]
        | String _ -> [%type: string]
        | Combine (comb, elems) -> type_of_combine ~qualifier (comb, elems)
        | Def_ref path ->
            AstExt.Type.v (AstExt.Type.constr (type_name_of_def_ref path))
        | Monomorphic_array (e, _) ->
            let item_type_name = qualifier ^ "_item" in
            let item_type = type_of_element ~qualifier:item_type_name e in
            [%type: [%t item_type] list]
        | Object o ->
        match o.properties with
        | [] -> [%type: Yojson.Safe.t]
        | _ :: _ ->
            gather_declarations ~type_name:qualifier element;
            let typ = AstExt.Type.v (AstExt.Type.constr qualifier) in
            if o.nullable then
              [%type: [%t typ] option]
            else
              typ
      and type_of_combine :
             qualifier:string
          -> Json_schema.combinator * Json_schema.element list
          -> core_type =
       fun ~qualifier (comb, elems) ->
        match comb with
        | Json_schema.All_of
        | Json_schema.Not ->
            [%type: Yojson.Safe.t]
        | Json_schema.Any_of
        | Json_schema.One_of ->
        match
          elems
          |> List.find_opt (function
                 | Json_schema.{ kind = String _; _ } -> true
                 | _ -> false)
        with
        (* If it can be a string, let it be a string *)
        | Some s -> type_of_element ~qualifier s
        (* Otherwise let it be the first thing it could be *)
        | _ -> type_of_element ~qualifier (List.hd elems)
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
              AstExt.Type.decl
                type_name
                ~manifest:[%type: Yojson.Safe.t]
                ~attributes:(deriving_attrs ~is_record:false)
          | ps ->
              AstExt.Type.decl
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
            AstExt.Type.decl
              type_name
              ~attributes
              ~manifest:[%type: Yojson.Safe.t]
        | Null -> AstExt.Type.decl type_name ~attributes ~manifest:[%type: unit]
        | Boolean ->
            AstExt.Type.decl type_name ~attributes ~manifest:[%type: bool]
        | String _ ->
            AstExt.Type.decl type_name ~attributes ~manifest:[%type: string]
        | Integer _ ->
            AstExt.Type.decl type_name ~attributes ~manifest:[%type: int]
        | Number _ ->
            AstExt.Type.decl type_name ~attributes ~manifest:[%type: float]
        | Object o -> type_decl_of_object ~name:type_name o
        | Monomorphic_array (elem, _) ->
            let item_type_name = type_name ^ "_item" in
            let item_type = AstExt.Type.t item_type_name [] in
            gather_declarations ~type_name:item_type_name elem;
            AstExt.Type.decl
              type_name
              ~attributes
              ~manifest:[%type: [%t item_type] list]
        | Def_ref _ ->
            AstExt.Type.decl
              type_name
              ~kind:
                (Ptype_variant
                   [ Ast.constructor_declaration
                       ~name:(n "Unimplemented_def_ref")
                       ~args:(Pcstr_tuple [])
                       ~res:None
                   ])
        (* TODO: Error on unsupported types? Or better to leave placeholder..
           Why not just jason? *)
        | Id_ref _
        | Ext_ref _
        | Array (_, _)
        | Dummy ->
            AstExt.Type.decl
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

  let data_module_of_schema_entry : string * Http_spec.schema -> structure_item
      =
   fun (name, schema) ->
    let name = n (Some (Camelsnakekebab.upper_camel_case name)) in
    let type_declarations =
      [ Ast.pstr_type Recursive (type_declarations_of_schema schema.schema) ]
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
            |> ListLabels.fold_left
                 ~init:Graph.empty
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
                     ("TODO: No schema provided for reference in other schema "
                     ^ label))
      in
      Ast.pmod_structure structure_items
    in

    Ast.module_binding ~name ~expr |> Ast.pstr_module
end

module EndpointsModule = struct
  exception Invalid_data of string

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
          (* TODO Why are we not currently supportin array? *)
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
      | Combine ((One_of | Any_of), elems) ->
      (* If it can be a string, let it be a string *)
      match
        elems
        |> List.find_opt (function
               | Json_schema.{ kind = String _; _ } -> true
               | _ -> false)
      with
      | Some s -> string_conv_and_format_of_elem_kind s.kind
      (* Otherwise let it be the first thing it could be *)
      | _ -> string_conv_and_format_of_elem_kind (List.hd elems).kind

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
          | [ typ_decl ] ->
          match typ_decl.ptype_manifest with
          | None -> failwith ("unsupported type for parameter " ^ name)
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
                   name)
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
                          "default parmamters value %s not supported for param \
                           %s"
                          (Yojson.Safe.to_string unsupported_default)
                          name))
        in
        (default, to_string)
      in
      let default =
        if not optional then
          None
        else
          default
      in
      { name; optional; default; to_string; var; pat; format = None }

    let of_openapi_parameters : Http_spec.Message.Params.t -> t =
     fun params ->
      let module P = H.Message.Params in
      { query = P.query params |> List.map of_http_spec_param
      ; path = P.path params |> List.map of_http_spec_param
      ; header = P.header params |> List.map of_http_spec_param
      ; form =
          P.cookie params |> List.map of_http_spec_param
          (* TODO! Move this into data object! *)
          (* ; cookie = P.cookie params |> List.map of_http_spec_param   *)
      }

    (* match params with *)
    (* | None -> empty *)
    (* | Some ps -> *)
    (*     ps *)
    (*     |> ListLabels.fold_right *)
    (*          ~init:{ query = []; path = []; header = []; form = [] } *)
    (*          ~f:(fun i acc -> *)
    (*            match i with *)
    (*            | `Ref (r : Openapi_spec.reference) -> *)
    (*                failwith *)
    (*                  (Printf.sprintf *)
    (* "references not yet supported for params, given \ *)
       (*                      param ref %s" *)
    (*                     r.ref_) *)
    (*            | `Obj (p : Openapi_spec.parameter) -> *)
    (*            match p.in_ with *)
    (*            | `Query -> *)
    (*                { acc with query = of_openapi_parameter p :: acc.query } *)
    (*            | `Path -> *)
    (*                { acc with path = of_openapi_parameter p :: acc.path } *)
    (*            | `Header -> *)
    (*                { acc with header = of_openapi_parameter p :: acc.header } *)
    (*            | `Cookie -> *)
    (*                (\* TODO *\) *)
    (*                failwith "unsupported parameter location 'cookie") *)
  end

  type operation_function =
    Openapi_spec.path -> Openapi_spec.operation -> structure_item

  let operation_function_name operation_id =
    operation_id |> AstExt.to_identifier |> AstExt.Pat.var

  let data_module_fun_name name f = Printf.sprintf "Data.%s.%s" name f

  let to_json_fun mod_name : expression =
    let mod_name = Camelsnakekebab.upper_camel_case mod_name in
    Ast.evar (data_module_fun_name mod_name "to_yojson")

  let response_decoder (responses : H.Message.Responses.t) : expression =
    (* TODO What to do with this unsupported thing? *)
    match responses |> List.assoc_opt `OK with
    | None ->
        (* TODO This is only required if it is the ONLY response. So need to account for others *)
        (* https://spec.openapis.org/oas/v3.1.0#responses-object *)
        raise (Invalid_data "Invalid schema: no 200 response")
    | Some schema ->
    match schema.kind with
    | `Json ->
        let mod_name = Camelsnakekebab.upper_camel_case schema.name in
        let conv_fun = Ast.evar (data_module_fun_name mod_name "of_yojson") in
        [%expr of_json_string [%e conv_fun]]
    | `Html -> [%expr fun x -> x]
    | `Multipart_form -> raise (Failure "TODO")

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

  let data_conv_and_param :
      H.Message.Request.t -> expression * pattern * Params.param list =
    let from_json (schema : H.schema) =
      ( [%expr Some (`Json ([%e to_json_fun schema.name] data))]
      , [%pat? data]
      , [] )
    and from_multipart (schema : H.schema) =
      (* TODO hella awkward schema.schema.schema, lol -- need to rename *)
      match (Json_schema.root schema.schema.schema).kind with
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
    fun req ->
      match req.content with
      | None -> ([%expr None], [%pat? ()], [])
      | Some { schema; _ } ->
      match schema.kind with
      | `Json -> from_json schema
      | `Multipart_form -> from_multipart schema
      | `Html -> raise (Failure "TODO")

  (* TODO rename to operation_fun? *)
  let operation_fun_expr : Http_spec.Message.t -> structure_item =
   fun message ->
    let name = operation_function_name message.req.id in
    let params = Params.of_openapi_parameters message.req.params in
    let data_conv, data_param, form_params = data_conv_and_param message.req in
    let params = { params with form = form_params } in
    let decode_resp = response_decoder message.resp in
    let meth = Ast.pexp_variant (Http.Method.to_string message.req.meth) None in
    let body =
      [%expr
        make_request
          ?data:[%e data_conv]
          ~base_url
          ~path:[%e path_parts message.req.path]
          ~params:[%e query_params params.query]
          ~headers:[%e extra_headers params.header]
          ~decode:[%e decode_resp]
          [%e meth]]
    in
    [%stri let [%p name] = [%e fun_with_params ~data:data_param params body]]

  (** Construct the AST node of the module with all endpoint functions *)
  let of_messages : Http_spec.Message.t list -> string -> structure_item list =
   fun messages base_url ->
    let endpoint_functor =
      let endpoint_functor_param =
        Named
          ( n (Some "Client")
          , Ast.pmty_ident (n (Astlib.Longident.parse "Oooapi_lib.Client")) )
      in
      let config_functor_param =
        Named
          ( n (Some "Config")
          , Ast.pmty_ident (n (Astlib.Longident.parse "Oooapi_lib.Config")) )
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
    [ [%stri let base_url = [%e Ast.estring base_url]]
    ; Ast.module_binding ~name:(n (Some "Make")) ~expr:endpoint_functor
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
  let spec = Http_spec.of_openapi_spec spec in
  [%stri let __NOTE__ = [%e Ast.estring (notice ())]]
  :: [%stri let __TITLE__ = [%e Ast.estring spec.title]]
  :: [%stri let __API_VERSION__ = [%e Ast.estring spec.version]]
  :: DataModule.of_schemata spec.schemata
  :: EndpointsModule.of_messages spec.messages spec.base_url
