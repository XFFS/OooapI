include Openapi_v
include Openapi_t
module Openapi_path = Openapi_path

let of_json s =
  match Openapi_j.t_of_string s with
  | spec -> Ok spec
  | exception Yojson.Json_error err -> Error (`Msg err)

let to_json t = Openapi_j.string_of_t t

let from_in_channel ic =
  ic |> In_channel.input_all |> of_json

let from_file file =
  if not (Sys.file_exists file) then
    Error (`Msg ("File " ^ file ^ " does not exist or cannot be read"))
  else if not (file |> String.ends_with ~suffix:".json") then
    Error (`Msg ("Only JSON input is supported, expecting .json file but found " ^ file ))
  else
    In_channel.with_open_text file from_in_channel

exception Unsupported_reference of string * string
exception Invalid_reference of string

let bad_ref r = Invalid_reference r

let get_exn ~exn : 'a option -> 'a = function
  | Some x -> x
  | None -> raise exn

let get_name_of_ref ref = ref |> String.split_on_char '/' |> List.rev |> List.hd

let get_ref_id_for ~(section : string) ref' : string =
  match String.split_on_char '/' ref' with
  | [ "#"; "components"; section'; item_id ] ->
      if String.equal section' section then
        item_id
      else
        raise
          (Invalid_reference
             (Printf.sprintf
                "Invalid reference '%s' found in a location that should only \
                 refer to objects in the section '#/components/%s'"
                ref'
                section))
  | [ _; _; _; _ ] ->
      (* All supported references to components sections should have 4 segments *)
      raise
        (Unsupported_reference
           ( ref'
           , "Only references to the component section of this document are \
              supported. I.e., refs starting with `#/components/`" ))
  | _ -> raise (Unsupported_reference (ref', "Unkown reason"))

let with_obj_of_ref
    (ref' : string)
    (assoc : (string * 'a) list)
    ~(section : string)
    ~(f : 'a -> 'b) : 'b =
  let ref_id = get_ref_id_for ~section ref' in
  assoc |> List.assoc_opt ref_id |> get_exn ~exn:(bad_ref ref') |> f

(* We don't worry about the fact that a path_item can both be referenced
   and defined, because: "In case a Path Item Object field appears both in
   the defined object and the referenced object, the behavior is
   undefined."  See https://spec.openapis.org/oas/latest.html#fixed-fields-6*)
let dereferenced_path_item : components -> path_item -> path_item =
 fun components item ->
  match item with
  | { ref_ = None; _ } -> item
  | { ref_ = Some ref'; _ } ->
      with_obj_of_ref ref' components.pathItems ~section:"pathItems" ~f:Fun.id

let dereference_schema : components -> schema -> schema =
 fun components schema ->
  match ((Json_schema.root schema.schema).kind : Json_schema.element_kind) with
  (* TODO Do we also need to handle Id_ref? *)
  | Def_ref ref' ->
      (* TODO: Is there a better way to derive this? *)
      let ref' = "#" ^ Json_query.json_pointer_of_path ref' in
      with_obj_of_ref ref' components.schemas ~section:"schemas" ~f:(fun s ->
          let name = get_name_of_ref ref' in
          { s with
            (* The name is the schema key *)
            (* TODO Should we be adding these names during deserialization? *)
            name = Some name
          })
  | _ -> schema

let dereferenced_media_type : components -> media_type -> media_type =
 fun components media ->
  match media.schema with
  | None -> media
  | Some s -> { media with schema = Some (dereference_schema components s) }

let dereferenced_parameters =
  let dereferenced_parameter :
      components -> parameter or_ref -> parameter or_ref =
   fun components p ->
    match p with
    | `Obj _ -> p
    | `Ref r ->
        with_obj_of_ref
          r.ref_
          components.parameters
          ~section:"parameters"
          ~f:(fun p ->
            let resolved =
              match p.schema with
              | Some s ->
                  { p with schema = Some (dereference_schema components s) }
              | None ->
              match p.content with
              | None
              | Some [] ->
                  failwith "TODO Invalid spec: param without schema or content"
              | Some (_ :: _ :: _) ->
                  failwith "TODO Invalid spec: only one entry allowed"
              | Some [ (media_type, media) ] ->
                  { p with
                    content =
                      Some
                        [ (media_type, dereferenced_media_type components media)
                        ]
                  }
            in
            `Obj resolved)
  in
  fun components params ->
    Option.map (List.map @@ dereferenced_parameter components) params

let dereferenced_request_body :
    components -> request_body or_ref option -> request_body or_ref option =
 fun components orb ->
  match orb with
  | None -> orb (* Nothing to deref *)
  | Some rb ->
      let req_body : request_body =
        (* We need to obtain the actual reference, so we can resolve its children *)
        match rb with
        | `Obj r -> r
        | `Ref r ->
            with_obj_of_ref
              r.ref_
              components.requestBodies
              ~section:"requestBodies"
              ~f:Fun.id
      in
      Some
        (`Obj
          { req_body with
            content =
              List.map
                (fun (key, media) ->
                  (key, dereferenced_media_type components media))
                req_body.content
          })

let dereferenced_response : components -> response -> response =
 fun components resp ->
  { resp with
    content =
      resp.content
      |> Option.map
           (List.map (fun (key, media) ->
                (key, dereferenced_media_type components media)))
  }

let dereferenced_operation : components -> operation option -> operation option
    =
 fun components oper ->
  match oper with
  | None -> None
  | Some oper ->
      Some
        { oper with
          parameters = dereferenced_parameters components oper.parameters
        ; requestBody = dereferenced_request_body components oper.requestBody
        ; responses =
            List.map
              (fun (code, resp) ->
                (code, dereferenced_response components resp))
              oper.responses
        }

(* Descend into all operations to replace paths *)
let resolved_path_item : components -> path_item -> path_item =
 fun components item ->
  let deref_op = dereferenced_operation components in
  { item with
    parameters = dereferenced_parameters components item.parameters
  ; get = deref_op item.get
  ; put = deref_op item.put
  ; post = deref_op item.post
  ; delete = deref_op item.delete
  ; options = deref_op item.options
  ; head = deref_op item.head
  ; patch = deref_op item.patch
  ; trace = deref_op item.trace
  }

let resolve_refs : t -> t =
 fun spec ->
  (* If there is no components object, we still want to check the spec *)
  (* in case there are dangling references *)
  let components =
    Option.value ~default:(create_components ()) spec.components
  in
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
