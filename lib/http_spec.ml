(** A simplified, normalized representation of HTTP API's *)

module O = Openapi_spec
module Ast = Ppxlib.Ast

exception Invalid_spec of string

(** A message

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
  exception Unsupported_spec of string

  let get ~msg = function
    | Some x -> x
    | None -> raise (Unsupported_spec msg)

  module Params : sig
    type param =
      { required : bool
      ; schema : O.schema
      }

    type t

    val empty : t
    val of_openapi : t -> O.parameter list -> t
    val path : t -> (string * param) list
    val query : t -> (string * param) list
    val header : t -> (string * param) list
    val cookie : t -> (string * param) list
  end = struct
    module M = Map.Make (String)

    type param =
      { required : bool
      ; schema : O.schema
      }

    let param_of_openapi : O.parameter -> param =
     fun param ->
      { required = param.required
      ; schema = param.schema |> get ~msg:"no schema in parameter"
      }

    (* Parameters that specialize a request *)
    type t =
      { path : param M.t
      ; query : param M.t
      ; header : param M.t
      ; cookie : param M.t
      }

    let empty =
      { path = M.empty; query = M.empty; header = M.empty; cookie = M.empty }

    let of_openapi : t -> O.parameter list -> t =
     fun init params ->
      (* Later added params will override params in the same section with
         the same name, as per
         https://spec.openapis.org/oas/latest.html#fixed-fields-7*)
      params
      |> List.fold_left
           (fun t' (p : O.parameter) ->
             match p.in_ with
             | `Path ->
                 { t' with path = M.add p.name (param_of_openapi p) t'.path }
             | `Query ->
                 { t' with query = M.add p.name (param_of_openapi p) t'.query }
             | `Header ->
                 { t' with
                   header = M.add p.name (param_of_openapi p) t'.header
                 }
             | `Cookie ->
                 { t' with
                   cookie = M.add p.name (param_of_openapi p) t'.cookie
                 })
           init

    let path t = M.bindings t.path
    let query t = M.bindings t.query
    let header t = M.bindings t.header
    let cookie t = M.bindings t.cookie
  end

  let deref : 'a O.or_ref -> 'a = function
    | `Obj a -> a
    | `Ref ref' ->
        (* All refs should be resolved via `O.resolve_refs` in `of_openapi_spec` *)
        raise (failwith ("unresolved reference " ^ ref'.ref_))

  module Request = struct
    type content =
      { media_type : string
      ; required : bool
      ; schema : O.schema
      }

    type t =
      { params : Params.t
      ; path : O.Openapi_path.t (* Locator *)
      ; meth : Http.Method.t
      ; content : content option (* Representation (if applicable) *)
      }

    let of_operation :
        O.Openapi_path.t -> Http.Method.t -> Params.t -> O.operation -> t =
     fun path meth path_params oper ->
      let params =
        oper.parameters
        |> Option.value ~default:[]
        |> List.map deref
        |> Params.of_openapi path_params
      in
      let content =
        match oper.requestBody with
        | None -> None
        | Some r -> (
            let ({ content; required; _ } : O.request_body) = deref r in
            match content with
            | [] -> None
            | (media_type, c) :: _ ->
            match c.schema with
            | Some schema -> Some { media_type; schema; required }
            | None ->
                failwith
                  (Printf.sprintf
                     "Request body with media type %s has no schema. Why does \
                      OpenAPI allow this?"
                     media_type))
      in
      { params; path; meth; content }
  end

  (* TODO: Support different status returns? E.g. via [`200 (of_data data)] *)
  module Responses = struct
    type content =
      { schema : O.schema
      ; media_type : string
      }

    type t = (Http.Status.t * content) list

    let of_responses : (string * O.response) list -> t =
     fun resps ->
      resps
      |> List.map (fun (code, (resp : O.response)) ->
             let status =
               match
                 code |> int_of_string_opt |> Option.map Http.Status.of_int
               with
               | Some s -> s
               | None ->
                   raise
                     (Invalid_spec
                        (Printf.sprintf "Invalid response code %s" code))
             in
             let response =
               match resp.content with
               | None
               | Some [] ->
                   raise
                     (Invalid_spec
                        (Printf.sprintf
                           "Response for code %s has no content. Why does \
                            OpenAPI allow this?"
                           code))
               | Some ((media_type, c) :: _) ->
               match c.schema with
               | Some schema -> { media_type; schema }
               | None ->
                   failwith
                     (Printf.sprintf
                        "Request body with media type %s has no schema. Why \
                         does OpenAPI allow this?"
                        media_type)
             in
             (status, response))
  end

  type t = Request.t * Responses.t

  let of_openapi_path : O.Openapi_path.t * O.path_item -> t list =
   fun (path, item) ->
    let path_params =
      item.parameters
      |> Option.value ~default:[]
      |> List.map deref
      |> Params.(of_openapi empty)
    in
    List.filter_map (* Filter out just the operations which are [Some] *)
      Fun.id
      [ item.get
        |> Option.map (fun p ->
               ( Request.of_operation path `GET path_params p
               , Responses.of_responses p.responses ))
      ; item.put
        |> Option.map (fun p ->
               ( Request.of_operation path `PUT path_params p
               , Responses.of_responses p.responses ))
      ; item.post
        |> Option.map (fun p ->
               ( Request.of_operation path `POST path_params p
               , Responses.of_responses p.responses ))
      ; item.delete
        |> Option.map (fun p ->
               ( Request.of_operation path `DELETE path_params p
               , Responses.of_responses p.responses ))
      ; item.options
        |> Option.map (fun p ->
               ( Request.of_operation path `OPTIONS path_params p
               , Responses.of_responses p.responses ))
      ; item.head
        |> Option.map (fun p ->
               ( Request.of_operation path `HEAD path_params p
               , Responses.of_responses p.responses ))
      ; item.patch
        |> Option.map (fun p ->
               ( Request.of_operation path `PATCH path_params p
               , Responses.of_responses p.responses ))
      ; item.trace
        |> Option.map (fun p ->
               ( Request.of_operation path `TRACE path_params p
               , Responses.of_responses p.responses ))
      ]
end

type t =
  { base_url : string
  ; messages : Message.t list
  }

let of_openapi_spec : Openapi_spec.t -> t =
 fun s ->
  let s = O.resolve_refs s in
  let base_url =
    match s.servers with
    | [] ->
        (* https://spec.openapis.org/oas/v3.1.0#openapi-object
           > If the servers property is not provided, or is an empty array, the
           default value would be a Server Object with a url value of /. *)
        "/"
    | s :: _ ->
        (* NOTE: Currently we are just using the first server.
           What should we do if there are multiple servers? *)
        s.url
  in
  let messages = List.concat_map Message.of_openapi_path s.paths in
  { base_url; messages }
