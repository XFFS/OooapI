include Openapi_t
module Openapi_path = Openapi_path

let of_json s =
  match Openapi_j.t_of_string s with
  | spec -> Ok spec
  | exception Yojson.Json_error err -> Error (`Msg err)

let to_json t = Openapi_j.string_of_t t

let from_file file =
  let decoder =
    if file |> String.ends_with ~suffix:".json" then
      of_json
    else
      (* TODO Support YAML input (requires a YAML parser that supports anchors etc.) *)
      raise (Invalid_argument "Only JSON is currently supported")
  in
  if not (Sys.file_exists file) then
    Error (`Msg ("File " ^ file ^ " does not exist or cannot be read"))
  else
    In_channel.(input_all |> with_open_text file |> decoder)

(** TODO: Figure out conversion from/to YAML? *)

exception Unsupported_reference of string * string
exception Invalid_reference of string

let bad_ref r = Invalid_reference r

let get_exn ~exn : 'a option -> 'a = function
  | Some x -> x
  | None -> raise exn

let get_ref_id_for ~(section : string) ref' : string =
  match String.split_on_char '/' ref' with
  | [ "#"; "components"; section'; item_id ] ->
      if String.equal section' section then
        item_id
      else
        raise
          (Invalid_argument
             ("get_ref_for_section called on unexpected section " ^ section'))
  | [ _; _; _; _ ] ->
      (* All supported references to components sections should have 4 segments *)
      raise
        (Unsupported_reference
           ( ref'
           , "Only references to the component section of this document are \
              supported. I.e., refs starting with `#/components/`" ))
  | _ -> raise (Unsupported_reference (ref', "Unkown reason"))

(* We don't worry about the fact that a path_item can both be referenced
   and defined, because: "In case a Path Item Object field appears both in
   the defined object and the referenced object, the behavior is
   undefined."  See https://spec.openapis.org/oas/latest.html#fixed-fields-6*)
let dereferenced_path_item : components -> path_item -> path_item =
 fun components item ->
  match item with
  | { ref_ = None; _ } -> item
  | { ref_ = Some ref'; _ } ->
      let ref_id = get_ref_id_for ~section:"pathItems" ref' in
      components.pathItems
      |> List.assoc_opt ref_id
      |> get_exn ~exn:(bad_ref ref')

(* let dereferenced_parameter *)

(* Descend into all operations to replace paths *)
let resolved_path_item : components -> path_item -> path_item =
 fun _components item -> item (* TODO *)

let resolve_refs : t -> t =
 fun spec ->
  let default : components =
    { schemas = None
    ; responses = None
    ; parameters = None
    ; examples = None
    ; requestBodies = None
    ; headers = None
    ; securitySchemes = None
    ; links = None
    ; callbacks = None
    ; pathItems = []
    }
  in
  (* If there is no components object, we still want to check the spec *)
  (* in case there are dangling references *)
  let components = Option.value ~default spec.components in
  { spec with
    paths =
      spec.paths
      |> List.map (fun (path, item) ->
             let resolved_path_item =
               item
               |> dereferenced_path_item components
               |> resolved_path_item components
             in
             (path, resolved_path_item))
  }
