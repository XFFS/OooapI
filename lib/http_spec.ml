(** A simplified, normalized representation of HTTP API's *)

(* A message

   > A client sends requests to a server in the form of a request message with a method (Section 9) and request target (Section 7.1). The request
   might also contain header fields (Section 6.3) for request modifiers, client information, and representation metadata, content (Section 6.4)
   intended for processing in accordance with the method, and trailer fields (Section 6.5) to communicate information collected while sending
   the content.¶
   >
   > A server responds to a client's request by sending one or more response messages, each including a status code (Section 15). The response
   might also contain header fields for server information, resource metadata, and representation metadata, content to be interpreted in
   accordance with the status code, and trailer fields to communicate information collected while sending the content.¶
*)

module Message = struct
  type to_data = Ppxlib.Ast.expression
  type of_data = Ppxlib.Ast.expression

  type param =
    { name : string
    ; required : bool
    ; conv : to_data
    }

  (* Parameters that specialize a request *)
  type params =
    { path : param list
    ; query : param list
    ; header : param list
    ; cookie : param list
    }

  type request =
    { params : params
    ; path : Openapi_spec.Openapi_path.t (* Locator *)
    ; meth : Http.Method.t
    ; content_conv : to_data option (* Representation (if applicable) *)
    }

  (* TODO: Support different status returns? E.g. via [`200 (of_data data)] *)
  type responses = (Http.Status.t * of_data) list
  type t = request * responses
end

type t =
  { base_url : string
  ; messages : Message.t list
  }

exception Unsupported_reference of string * string
exception Invalid_reference of string

let bad_ref r = Invalid_reference r

let get_exn ~exn : 'a option -> 'a = function
  | Some x -> x
  | None -> raise exn

(* let with_component (components : Openapi_spec.components) ref' = *)
(*   let get_ref_in_section section itemId = *)
(*     get_exn ~exn:(Invalid_reference ref') *)
(*     @@ *)
(*     let ( let* ) = Option.bind in *)
(*     match section with *)
(*     | "schemas" -> *)
(*         let* schemas = components.schemas in *)
(*         let* schema = List.assoc_opt itemId schemas in *)
(*         Some (`Schema schema) *)
(*     | _ -> raise (Failure "TODO") *)
(*   in *)
(*   match String.split_on_char '/' ref' with *)
(*   | [ "#"; "components"; section; itemId ] -> get_ref_in_section section itemId *)
(*   | [ _; _; _; _ ] -> *)
(*       raise *)
(*         (Unsupported_reference *)
(*            ( ref' *)
(* , "Only references to the component section of this document are \ *)
   (*               supported. I.e., refs starting with `#/components/`" )) *)
(*   | _ -> raise (Unsupported_reference (ref', "Unkown reason")) *)

let resolve_refs : Openapi_spec.t -> Openapi_spec.t =
  let open Openapi_spec in
  fun spec ->
    match spec.components with
    | None -> spec (* All refs are located in components? *)
    | Some components ->
        let paths =
          spec.paths
          |> List.map (function
                 | (_, { ref_ = None; _ }) as entry -> entry
                 (* In case a Path Item Object field appears both in the defined
                    object and the referenced object, the behavior is undefined.
                    See https://spec.openapis.org/oas/latest.html#fixed-fields-6*)
                 | path, { ref_ = Some ref'; _ } ->
                     let item' =
                       get_exn ~exn:(bad_ref ref') components.pathItems
                       |> List.assoc_opt ref'
                       |> get_exn ~exn:(bad_ref ref')
                     in
                     (path, item'))
          (* TODO Need to go into each path and replace those refs *)
        in
        { spec with paths }

let of_openapi_spec : Openapi_spec.t -> t = raise (Failure "TODO")
