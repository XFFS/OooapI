(** A simplified, normalized representation of HTTP API's *)

module O = Openapi_spec
module Ast = Ppxlib.Ast

exception Invalid_spec of string

(* A string map *)
module SM = Map.Make (String)

module Media_kind = struct
  type t =
    [ `Html
    | `Json
    | `Multipart_form
    | `Url_encoded_form
    | `Binary
    | `Pdf
    ]

  let to_variant_name = function
    | `Html -> "Html"
    | `Json -> "Json"
    | `Multipart_form -> "Multipart_form"
    | `Url_encoded_form -> "Url_encoded_form"
    | `Binary -> "Binary"
    | `Pdf -> "Pdf"

  let of_media_type = function
    | "text/html" -> `Html
    | "application/json" -> `Json
    | "multipart/form-data" -> `Multipart_form
    | "application/octet-stream" -> `Binary
    | "application/x-www-form-urlencoded" -> `Url_encoded_form
    | "application/pdf" -> `Pdf
    | media_type ->
        raise (Invalid_spec ("unsupported media type " ^ media_type))
end

type schema =
  { name : string
  ; schema : O.schema
  ; kind : Media_kind.t
  }
(** All schemas in the http_spec must have a name, this should be unique *)

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
      ; schema : schema
      }

    type t

    val empty : t
    val of_openapi : t -> O.parameter list -> t
    val path : t -> (string * param) list
    val query : t -> (string * param) list
    val header : t -> (string * param) list
    val cookie : t -> (string * param) list
    val schemas : t -> schema list
  end = struct
    module M = SM

    type param =
      { required : bool
      ; schema : schema
      }

    let param_of_openapi : section:string -> O.parameter -> param =
     fun ~section param ->
      let default_name = Printf.sprintf "%s_param_%s" section param.name in
      let schema =
        match param.schema with
        | Some schema ->
            { name = schema.name |> Option.value ~default:default_name
            ; kind = `Json
            ; schema
            }
        | None ->
            let media_type, content =
              param.content
              |> Option.value ~default:[]
              |> (fun l -> List.nth_opt l 0)
              |> get
                   ~msg:
                     (Printf.sprintf
                        "Parameter %s has neither schema nor content"
                        param.name)
            in
            let schema =
              content.schema
              |> get
                   ~msg:
                     (Printf.sprintf
                        "Parameter %s has neither schema nor content"
                        param.name)
            in
            let kind = Media_kind.of_media_type media_type in
            { name = schema.name |> Option.value ~default:default_name
            ; kind
            ; schema
            }
      in
      { required = param.required; schema }

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
                 { t' with
                   path =
                     M.add p.name (param_of_openapi ~section:"path" p) t'.path
                 }
             | `Query ->
                 { t' with
                   query =
                     M.add p.name (param_of_openapi ~section:"query" p) t'.query
                 }
             | `Header ->
                 { t' with
                   header =
                     M.add
                       p.name
                       (param_of_openapi ~section:"header" p)
                       t'.header
                 }
             | `Cookie ->
                 { t' with
                   cookie =
                     M.add
                       p.name
                       (param_of_openapi ~section:"cookie" p)
                       t'.cookie
                 })
           init

    let path t = M.bindings t.path
    let query t = M.bindings t.query
    let header t = M.bindings t.header
    let cookie t = M.bindings t.cookie

    let schemas t : schema list =
      [ path t |> List.map (fun (_, p) -> p.schema)
      ; query t |> List.map (fun (_, p) -> p.schema)
      ; header t |> List.map (fun (_, p) -> p.schema)
      ; cookie t |> List.map (fun (_, p) -> p.schema)
      ]
      |> List.flatten
  end

  let deref : 'a O.or_ref -> 'a = function
    | `Obj a -> a
    | `Ref ref' ->
        (* All refs should be resolved via `O.resolve_refs` in `of_openapi_spec` *)
        raise (failwith ("unresolved reference " ^ ref'.ref_))

  module Request = struct
    type content =
      { required : bool
      ; schema : schema
      }

    type t =
      { id : string
      ; params : Params.t
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
      let id =
        oper.operationId
        |> get ~msg:"Oooapi requires operations to have an operationId"
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
            | Some schema ->
                let kind = Media_kind.of_media_type media_type in
                let name =
                  match schema.name with
                  | Some n -> n
                  | None -> id ^ "_request"
                in
                let schema = { kind; name; schema } in
                Some { schema; required }
            | None ->
                failwith
                  (Printf.sprintf
                     "Request body with media type %s has no schema. Why does \
                      OpenAPI allow this?"
                     media_type))
      in
      { id; params; path; meth; content }
  end

  module Responses = struct
    type t = (Http.Status.t * schema) list

    let of_responses : O.operation -> t =
     fun oper ->
      oper.responses
      |> List.map (fun (code, (resp : O.response)) ->
             let status =
               match code with
               | "default" -> `Code (-1) (* Interpreted as a fallback for undeclared responses *)
               | code ->
               match code |> int_of_string_opt |> Option.map Http.Status.of_int with
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
               | None ->
                   failwith
                     (Printf.sprintf
                        "Request body with media type %s has no schema. Why \
                         does OpenAPI allow this?"
                        media_type)
               | Some schema ->
                   let kind = Media_kind.of_media_type media_type in
                   let name =
                     match schema.name with
                     | Some n -> n
                     | None ->
                         let op_id =
                           oper.operationId
                           |> get
                                ~msg:
                                  "Oooapi expects operations to have an \
                                   operationId"
                         in
                         op_id ^ "_response"
                   in
                   { kind; name; schema }
             in
             (status, response))
  end

  type t =
    { req : Request.t
    ; resp : Responses.t
    }

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
               { req = Request.of_operation path `GET path_params p
               ; resp = Responses.of_responses p
               })
      ; item.put
        |> Option.map (fun p ->
               { req = Request.of_operation path `PUT path_params p
               ; resp = Responses.of_responses p
               })
      ; item.post
        |> Option.map (fun p ->
               { req = Request.of_operation path `POST path_params p
               ; resp = Responses.of_responses p
               })
      ; item.delete
        |> Option.map (fun p ->
               { req = Request.of_operation path `DELETE path_params p
               ; resp = Responses.of_responses p
               })
      ; item.options
        |> Option.map (fun p ->
               { req = Request.of_operation path `OPTIONS path_params p
               ; resp = Responses.of_responses p
               })
      ; item.head
        |> Option.map (fun p ->
               { req = Request.of_operation path `HEAD path_params p
               ; resp = Responses.of_responses p
               })
      ; item.patch
        |> Option.map (fun p ->
               { req = Request.of_operation path `PATCH path_params p
               ; resp = Responses.of_responses p
               })
      ; item.trace
        |> Option.map (fun p ->
               { req = Request.of_operation path `TRACE path_params p
               ; resp = Responses.of_responses p
               })
      ]
end

(* Mapping schema names to schemas *)
type schemata = schema SM.t

type t =
  { title : string
  ; version : string
  ; base_url : string
  ; messages : Message.t list
  ; schemata : schemata
  }

let add_msg_schema : schemata -> Message.t -> schemata =
 fun schemata msg ->
  let schemas : schema list =
    (msg.req.content
    |> Option.fold ~none:[] ~some:(fun (c : Message.Request.content) ->
           [ c.schema ]))
    @ Message.Params.schemas msg.req.params
    @ (msg.resp |> List.map snd)
  in
  schemas
  |> ListLabels.fold_left ~init:schemata ~f:(fun ss s -> SM.add s.name s ss)

let open_api_major_version s : int =
  Option.value ~default:0
  @@
  match String.split_on_char '.' s with
  | n :: _ -> int_of_string_opt n
  | _ -> None

let of_openapi_spec : Openapi_spec.t -> t =
 fun spec ->
  let ({ version; title; _ } : O.info) = spec.info in
  if not (open_api_major_version spec.openapi >= 3) then
    failwith
      ("Required OpenAPI version 3 or later, spec is for version "
      ^ spec.openapi);
  let s = O.resolve_refs spec in
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
  let schemata, messages =
    s.paths
    |> ListLabels.fold_left
         ~init:(SM.empty, [])
         ~f:(fun (schemata, paths) path ->
           let msgs = Message.of_openapi_path path in
           let scm =
             ListLabels.fold_left ~init:schemata ~f:add_msg_schema msgs
           in
           (scm, msgs @ paths))
  in
  let schemata =
    match spec.components with
    | None -> schemata
    | Some { schemas; _ } ->
        schemas
        |> ListLabels.fold_left
             ~init:schemata
             ~f:(fun schemata (name, schema) ->
               schemata
               |> SM.update name (function
                      | None -> Some { name; schema; kind = `Json }
                      | Some s -> Some s))
  in
  { version; title; base_url; messages; schemata }
