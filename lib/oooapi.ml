open Ppxlib

module DAG = DAG

let loc = Location.in_file "ooo_spec_gen.ml"

module Ast = Ast_builder.Make (struct
  let loc = loc
end)


(** An AST node for a name v *)
let n v = Ast.Located.mk v

(** Construct the AST node of an endpoint function *)
(* let endpoint (fname, uri, data) : structure_item = *)
(*   let function_name = Ast.ppat_var (n fname) in *)
(*   let uri = Ast.estring uri in *)
(*   let data = Ast.estring data in *)
(*   [%stri let [%p function_name] = Client.request [%e uri] [%e data]] *)

let path_to_fun : Openapi_spec.path * Openapi_spec.path_item -> structure_item list =
  fun (_path, _desc) ->
  []
  (* failwith "TODO convert path parts to endpoints: one path can yield multiple endpoint funs cause of multiple methods" *)

(** Construct the AST node of the module with all endpoint functions *)
let endpoint_module : Openapi_spec.paths -> structure_item =
 fun endpoints ->
  let expr = endpoints |> List.map path_to_fun |> List.flatten |> Ast.pmod_structure in
  Ast.module_binding ~name:(n (Some "Endpoint")) ~expr |> Ast.pstr_module

(* TODO Account for constraints, like pattern, min/max format etc.?
   This could be thru abstract types or thru validators in constructors *)

module OcamlBuiltins = struct
  module StrSet = Set.Make (String)

  let keywords =
    StrSet.of_list
      [ "and"
      ; "as"
      ; "assert"
      ; "asr"
      ; "begin"
      ; "class"
      ; "constraint"
      ; "do"
      ; "done"
      ; "downto"
      ; "else"
      ; "end"
      ; "exception"
      ; "external"
      ; "false"
      ; "for"
      ; "fun"
      ; "function"
      ; "functor"
      ; "if"
      ; "in"
      ; "include"
      ; "inherit"
      ; "initializer"
      ; "land"
      ; "lazy"
      ; "let"
      ; "lor"
      ; "lsl"
      ; "lsr"
      ; "lxor"
      ; "match"
      ; "method"
      ; "mod"
      ; "module"
      ; "mutable"
      ; "new"
      ; "nonrec"
      ; "object"
      ; "of"
      ; "open"
      ; "or"
      ; "private"
      ; "rec"
      ; "sig"
      ; "struct"
      ; "then"
      ; "to"
      ; "true"
      ; "try"
      ; "type"
      ; "val"
      ; "virtual"
      ; "when"
      ; "while"
      ; "with"
      ]

  let is_keyword s = StrSet.mem s keywords

  let sanitize s =
    if is_keyword s then
      s ^ "_"
    else
      s
end

let to_identifier s =
  s
  |> String.map (function
         | '/' -> '-'
         | c -> c)
  |> Camelsnakekebab.lower_snake_case
  |> OcamlBuiltins.sanitize

let typ ptyp_desc : core_type =
  { ptyp_desc; ptyp_loc = loc; ptyp_loc_stack = []; ptyp_attributes = [] }

let typ_constr ?(args = []) name =
  Ptyp_constr (n (Astlib.Longident.parse name), args)

let typ_decl
    ?kind
    ?manifest
    ?(params = [])
    ?(cstrs = [])
    ?(private_ = false)
    ?(attributes = [])
    name : type_declaration =
  let private_ =
    if private_ then
      Private
    else
      Public
  in
  let ptype_kind = Option.value kind ~default:Ptype_abstract in
  { ptype_kind
  ; ptype_name = n name
  ; ptype_params = params
  ; ptype_cstrs = cstrs
  ; ptype_private = private_
  ; ptype_manifest = manifest
  ; ptype_attributes = attributes
  ; ptype_loc = loc
  }

(* Ast.type_declaration ~name:(n name) ~private_ ~params ~cstrs ~kind ~manifest *)

let attr ~name = Ast.attribute ~name:(n name) ~payload:(PStr [])
let const_str s : expression = Ast.pexp_constant (Pconst_string (s, loc, None))

let attr_str ~name str =
  Ast.attribute
    ~name:(n name)
    ~payload:(PStr [ Ast.pstr_eval (const_str str) [] ])

let attr_strs ~name strs =
  Ast.attribute
    ~name:(n name)
    ~payload:
      (PStr [ Ast.pstr_eval (Ast.pexp_tuple (List.map const_str strs)) [] ])

let attr_ident ~name ident =
  let ident = Astlib.Longident.parse ident in
  Ast.attribute
    ~name:(n name)
    ~payload:(PStr [ Ast.pstr_eval (Ast.pexp_ident (n ident)) [] ])

let deriving_attrs = [ attr_strs ~name:"deriving" [ "make"; "yojson" ] ]

let rec type_name_of_def_ref : Json_query.path -> string = function
  | [] -> failwith "unsupported component query path"
  | [ `Field n ] -> Camelsnakekebab.upper_camel_case n ^ ".t"
  | _ :: rest -> type_name_of_def_ref rest

let type_declarations_of_schema : Openapi_spec.schema -> type_declaration list =
 fun schema ->
  (* We need to form and gather all declarations implied by the schema
     and we use this ref to collect them without having to thread the data thru *)
  let declarations = ref [] in
  let root = Json_schema.root schema in
  let rec gather_declarations : type_name:string -> Json_schema.element -> unit
      =
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
      | Def_ref path -> typ (typ_constr (type_name_of_def_ref path))
      | Monomorphic_array (e, _) ->
          let item_type_name = qualifier ^ "_item" in
          let item_type = type_of_element ~qualifier:item_type_name e in
          [%type: [%t item_type] list]
      | Object o ->
      match o.properties with
      | [] -> [%type: Yojson.Safe.t]
      | _ :: _ ->
          gather_declarations ~type_name:qualifier element;
          typ (typ_constr qualifier)
    in
    let type_decl_of_object : Json_schema.object_specs -> type_declaration =
      let record_label :
          string * Json_schema.element * bool * _ -> label_declaration =
       (* TODO Account for required vs. optional fields (optional must be in `option`) *)
       fun (field_name, element, required, _) ->
        let fname = to_identifier field_name in
        let pld_type =
          let field_type = type_of_element ~qualifier:fname element in
          if required then
            field_type
          else
            [%type: [%t field_type] option]
        in
        let pld_attributes =
          if OcamlBuiltins.is_keyword field_name then
            (* ppx_json_conv *)
            [ attr_str ~name:"key" field_name ]
          else
            []
            @
            if required then
              (* ppx_make *)
              [ attr ~name:"required" ]
            else
              (* ppx_json_conv *)
              [ attr_ident ~name:"default" "None" ]
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
        | [] -> typ_decl type_name ~manifest:[%type: Yojson.Safe.t] ~attributes
        | ps ->
            typ_decl
              type_name
              ~kind:(Ptype_record (List.map record_label ps))
              ~attributes
    in
    let decl =
      let attributes = deriving_attrs in
      match (element.kind : Json_schema.element_kind) with
      | Any -> typ_decl "t" ~attributes ~manifest:[%type: Yojson.Safe.t]
      | Null -> typ_decl "t" ~attributes ~manifest:[%type: unit]
      | Boolean -> typ_decl "t" ~attributes ~manifest:[%type: bool]
      | String _ -> typ_decl "t" ~attributes ~manifest:[%type: string]
      | Integer _ -> typ_decl "t" ~attributes ~manifest:[%type: int]
      | Number _ -> typ_decl "t" ~attributes ~manifest:[%type: float]
      | Object o -> type_decl_of_object o
      | Monomorphic_array (elem, _) ->
          gather_declarations ~type_name:"item" elem;
          typ_decl "t" ~attributes ~manifest:[%type: item list]
      (* TODO: Error on unsupported types? Or better to leave placeholder..  *)
      | Combine (_, _)
      | Def_ref _
      | Id_ref _
      | Ext_ref _
      | Array (_, _)
      | Dummy ->
          typ_decl
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

let data_module_of_schema_entry : string * Openapi_spec.schema -> structure_item
    =
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

let data_module (components : Openapi_spec.components option) =
  (* let type_name = Ast.ppat_var (var "data_type") in *)
  let name = Ast.Located.mk (Some "Data") in
  let expr =
    let structure_items =
      match components with
      | Some { schemas = Some schemas; _ } ->
          let dep_ordering =
            schemas
            |> List.fold_left
                 (fun graph (src, schema) ->
                   schema_deps schema
                   |> List.fold_left
                        (fun graph' dst -> Graph.add_arc ~src ~dst graph')
                        graph)
                 Graph.empty
            |> Graph.topological_sort
            |> List.rev
          in
          let sorted_schemas =
            List.map
              (fun label -> (label, List.assoc label schemas))
              dep_ordering
          in
          List.map data_module_of_schema_entry sorted_schemas
      | _ -> []
    in
    Ast.pmod_structure structure_items
  in
  Ast.module_binding ~name ~expr |> Ast.pstr_module

let to_module_name s =
  let camel_case = Camelsnakekebab.upper_camel_case s in
  match String.get s 0 with
  | 'A' .. 'Z' -> camel_case
  | _ -> "O" ^ s

(* TODO: name and endpoints to become all data from OpenAPI spec *)
let modules name data endpoints =
  [ Ast.pstr_module
    @@ Ast.module_binding
         ~name:(n (Some (to_module_name name)))
         ~expr:
           (Ast.pmod_structure [ data_module data; endpoint_module endpoints ])
  ]

(* TODO: Put in comment *)
let header = "Generated by oooapi on DATE"

let write_ast_f fmt str =
  Format.fprintf fmt "%a\n" Pprintast.structure str;
  Format.print_flush ()

let write_ast str = write_ast_f Format.std_formatter str

let module_of_spec : Openapi_spec.t -> Ppxlib.Ast.structure =
 fun spec ->
  modules
    (to_module_name spec.info.title)
    spec.components
    (Option.value ~default:[] spec.paths)
