open Ppxlib
open AstUtil

(* TODO Factor into separate library as part of the Json_schema package? *)
(* TODO Account for constraints, like pattern, min/max format etc.?
   This could be thru abstract types or thru validators in constructors *)


let deriving_attrs ~is_record =
  if is_record then
    [
      Ast.attribute ~name:(n "deriving")
        ~payload:(PStr [ [%stri make, yojson { strict = false }] ]);
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

let type_of_string_specs
  : Json_schema.string_specs -> core_type
  = fun specs -> match specs.str_format with
    | None -> [%type: string]
    | Some "binary" -> [%type: [`File of string ]]
    | Some "uri" -> [%type: [`String of string]] (* TODO special support for URIs? *)
    | Some fmt -> failwith ("unsupported string format " ^ fmt)

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
  | String s -> type_of_string_specs s, []
  | Combine (comb, elems) -> type_of_combine ~qualifier (comb, elems)
  | Def_ref path -> AstExt.Type.v (AstExt.Type.constr (type_name_of_def_ref path)), []
  | Monomorphic_array (e, _) ->
    let item_type_name = qualifier ^ "_item" in
    let item_type, decls = type_of_element ~qualifier:item_type_name e in
    ([%type: [%t item_type] list], decls)
  | Object o ->
    let decl, decls = type_decl_of_object ~name:qualifier o in
    let typ = AstExt.Type.v (AstExt.Type.constr qualifier) in
    let typ = if o.nullable then [%type: [%t typ] option] else typ in
    let dependency_ordered_declarations = decls @ [decl] in
    (typ, dependency_ordered_declarations)

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
        [ AstExt.attr_ident ~name:"yojson.default" "None" ]
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
  : name:string -> Json_schema.object_specs -> (type_declaration * type_declaration list) =
  (* TODO Support for ignored aspects of spec? *)
  fun ~name { properties; _ } ->
  match properties with
  | [] ->
    let manifest = [%type: Yojson.Safe.t] in
    let attributes = deriving_attrs ~is_record:false in
    let main_decl = AstExt.Type.decl name ~manifest ~attributes in
    main_decl, []
  | _ ->
    let labels, decls =
      properties |> ListLabels.fold_right
        ~init:([], [])
        ~f:(fun property (labels, decls) ->
            let (label, decls') = record_label ~type_name:name property in
            label :: labels, decls' @ decls)
    in
    let kind = Ptype_record labels in
    let attributes = deriving_attrs ~is_record:true in
    let main_decl = AstExt.Type.decl name ~kind ~attributes in
    (main_decl, decls)

let type_declarations
  : Json_schema.schema -> (type_declaration * type_declaration list)
  = fun schema ->
    let name = "t" in
    let root =  Json_schema.root schema  in
    match root.kind with
    | Object specs -> type_decl_of_object ~name specs
    | _ ->
      let attributes = deriving_attrs ~is_record:false in
      let typ, decls = type_of_element ~qualifier:name root in
      AstExt.Type.decl name ~attributes ~manifest:typ, decls
