(* Auto-generated from "openapi.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type json = Json.t

type todo = Openapi_t.todo

type webhooks = Openapi_t.webhooks

type version = Openapi_t.version

type tags = Openapi_t.tags

type server = Openapi_t.server = {
  url: string;
  description: string option;
  variables: (string * todo) list option
}

type security = Openapi_t.security

type schema = Openapi_t.schema

type media_type = Openapi_t.media_type = {
  schema: schema option
    (**
      The schema defining the content of the request, response, or parameter.
    *);
  example: todo option
    (**
      Example of the media type. The example object SHOULD be in the correct
      format as specified by the media type. The example field is mutually
      exclusive of the examples field. Furthermore, if referencing a schema
      which contains an example, the example value SHALL override the example
      provided by the schema.
    *);
  examples: todo option
    (**
      Examples of the media type. Each example object SHOULD match the media
      type and specified schema if present. The examples field is mutually
      exclusive of the example field. Furthermore, if referencing a schema
      which contains an example, the examples value SHALL override the
      example provided by the schema.
    *);
  encoding: todo option
    (**
      A map between a property name and its encoding information. The key,
      being the property name, MUST exist in the schema as a property. The
      encoding object SHALL only apply to requestBody objects when the media
      type is multipart or application/x-www-form-urlencoded.
    *)
}

type media = Openapi_t.media

type response = Openapi_t.response = {
  description: string
    (**
      REQUIRED. A description of the response. CommonMark syntax MAY be used
      for rich text representation.
    *);
  headers: todo option
    (**
      Maps a header name to its definition. \[RFC7230\] states header names
      are case insensitive. If a response header is defined with the name
      "Content-Type", it SHALL be ignored.
    *);
  content: (media * media_type) list option
    (**
      A map containing descriptions of potential response payloads. The key
      is a media type or media type range and the value describes it. For
      responses that match multiple keys, only the most specific key is
      applicable. e.g. text/plain overrides text/*
    *);
  links: todo option
    (**
      A map of operations links that can be followed from the response. The
      key of the map is a short name for the link, following the naming
      constraints of the names for Component Objects.
    *)
}

type request_body = Openapi_t.request_body = {
  description: string option
    (**
      A brief description of the request body. This could contain examples of
      use. CommonMark syntax MAY be used for rich text representation.
    *);
  content: (string * media_type) list
    (**
      REQUIRED. The content of the request body. The key is a media type or
      media type range and the value describes it. For requests that match
      multiple keys, only the most specific key is applicable. e.g.
      text/plain overrides text/*
    *);
  required: bool
    (**
      Determines if the request body is required in the request. Defaults to
      false.
    *)
}

type reference = Openapi_t.reference = {
  ref_: string
    (**
      REQUIRED. The reference identifier. This MUST be in the form of a URI.
    *);
  summary: string option
    (**
      A short summary which by default SHOULD override that of the referenced
      component. If the referenced object-type does not allow a summary
      field, then this field has no effect.
    *);
  description: string option
    (**
      A description which by default SHOULD override that of the referenced
      component. CommonMark syntax MAY be used for rich text representation.
      If the referenced object-type does not allow a description field, then
      this field has no effect.
    *)
}

type param_location = Openapi_t.param_location

type parameter = Openapi_t.parameter = {
  name: string
    (**
      REQUIRED. The name of the parameter. Parameter names are case
      sensitive. If \[in\] is "path", the name field MUST correspond to a
      template expression occurring within the path field in the Paths
      Object. See Path Templating for further information. If \[in\] is
      "header" and the name field is "Accept", "Content-Type" or
      "Authorization", the parameter definition SHALL be ignored. For all
      other cases, the name corresponds to the parameter name used by the in
      property.
    *);
  in_: param_location
    (**
      REQUIRED. The location of the parameter. Possible values are "query",
      "header", "path" or "cookie".
    *);
  description: string option
    (**
      A brief description of the parameter. This could contain examples of
      use. CommonMark syntax MAY be used for rich text representation.
    *);
  required: bool
    (**
      Determines whether this parameter is mandatory. If the parameter
      location is "path", this property is REQUIRED and its value MUST be
      true. Otherwise, the property MAY be included and its default value is
      false.
    *);
  deprecated: bool
    (**
      Specifies that a parameter is deprecated and SHOULD be transitioned out
      of usage. Default value is false.
    *);
  allowEmptyValue: bool
    (**
      Sets the ability to pass empty-valued parameters. This is valid only
      for query parameters and allows sending a parameter with an empty
      value. Default value is false. If style is used, and if behavior is n/a
      (cannot be serialized), the value of allowEmptyValue SHALL be ignored.
      Use of this property is NOT RECOMMENDED, as it is likely to be removed
      in a later revision.
    *);
  style: string option
    (**
      Describes how the parameter value will be serialized depending on the
      type of the parameter value. Default values (based on value of in): for
      query - form; for path - simple; for header - simple; for cookie -
      form.
    *);
  explode: bool option
    (**
      When this is true, parameter values of type array or object generate
      separate parameters for each value of the array or key-value pair of
      the map. For other types of parameters this property has no effect.
      When style is form, the default value is true. For all other styles,
      the default value is false.
    *);
  allowReserved: bool
    (**
      Determines whether the parameter value SHOULD allow reserved
      characters, as defined by \[RFC3986\] :/?#\[\]\@!$&'()*+,;= to be
      included without percent-encoding. This property only applies to
      parameters with an in value of query. The default value is false.
    *);
  schema: schema option
    (** The schema defining the type used for the parameter. *);
  example: json option
    (**
      Example of the parameter’s potential value. The example SHOULD match
      the specified schema and encoding properties if present. The example
      field is mutually exclusive of the examples field. Furthermore, if
      referencing a schema that contains an example, the example value SHALL
      override the example provided by the schema. To represent examples of
      media types that cannot naturally be represented in JSON or YAML, a
      string value can contain the example with escaping where necessary.
    *);
  examples: todo option
    (**
      Examples of the parameter’s potential value. Each example SHOULD
      contain a value in the correct format as specified in the parameter
      encoding. The examples field is mutually exclusive of the example
      field. Furthermore, if referencing a schema that contains an example,
      the examples value SHALL override the example provided by the schema.
    *);
  content: (string * media_type) list option
    (**
      A map containing the representations for the parameter. The key is the
      media type and the value describes it. The map MUST only contain one
      entry.
    *)
}

type 'a or_ref = 'a Openapi_t.or_ref

type operation = Openapi_t.operation = {
  tags: string list option
    (**
      A list of tags for API documentation control. Tags can be used for
      logical grouping of operations by resources or any other qualifier.
    *);
  summary: string option (** A short summary of what the operation does. *);
  description: string option
    (**
      A verbose explanation of the operation behavior. CommonMark syntax MAY
      be used for rich text representation.
    *);
  externalDocs: todo option
    (** Additional external documentation for this operation. *);
  operationId: string option
    (**
      Unique string used to identify the operation. The id MUST be unique
      among all operations described in the API. The operationId value is
      case-sensitive. Tools and libraries MAY use the operationId to uniquely
      identify an operation, therefore, it is RECOMMENDED to follow common
      programming naming conventions.
    *);
  parameters: parameter or_ref list option
    (**
      A list of parameters that are applicable for this operation. If a
      parameter is already defined at the Path Item, the new definition will
      override it but can never remove it. The list MUST NOT include
      duplicated parameters. A unique parameter is defined by a combination
      of a name and location. The list can use the Reference Object to link
      to parameters that are defined at the OpenAPI Object’s
      components/parameters.
    *);
  requestBody: request_body or_ref option
    (**
      The request body applicable for this operation. The requestBody is
      fully supported in HTTP methods where the HTTP 1.1 specification
      \[RFC7231\] has explicitly defined semantics for request bodies. In
      other cases where the HTTP spec is vague (such as GET, HEAD and
      DELETE), requestBody is permitted but does not have well-defined
      semantics and SHOULD be avoided if possible.
    *);
  responses: (string * response) list
    (**
      The list of possible responses as they are returned from executing this
      operation.
    *);
  callbacks: todo option
    (**
      A map of possible out-of band callbacks related to the parent
      operation. The key is a unique identifier for the Callback Object. Each
      value in the map is a Callback Object that describes a request that may
      be initiated by the API provider and the expected responses.
    *);
  deprecated: bool
    (**
      Declares this operation to be deprecated. Consumers SHOULD refrain from
      usage of the declared operation. Default value is false.
    *);
  security: todo option
    (**
      A declaration of which security mechanisms can be used for this
      operation. The list of values includes alternative security requirement
      objects that can be used. Only one of the security requirement objects
      need to be satisfied to authorize a request. To make security optional,
      an empty security requirement (\{\}) can be included in the array. This
      definition overrides any declared top-level security. To remove a
      top-level security declaration, an empty array can be used.
    *);
  servers: server list option
    (**
      An alternative server array to service this operation. If an
      alternative server object is specified at the Path Item Object or Root
      level, it will be overridden by this value.
    *)
}

type path_item = Openapi_t.path_item = {
  ref_: string option;
  summary: string option;
  description: string option;
  get: operation option;
  put: operation option;
  post: operation option;
  delete: operation option;
  options: operation option;
  head: operation option;
  patch: operation option;
  trace: operation option;
  servers: server list option;
  parameters: parameter or_ref list option
}

type path = Openapi_t.path

type paths = Openapi_t.paths

type info = Openapi_t.info = {
  title: string (** REQUIRED. The title of the API. *);
  version: string
    (**
      REQUIRED. The version of the OpenAPI document (which is distinct from
      the OpenAPI Specification version or the API implementation version).
    *);
  summary: string option (** A short summary of the API. *);
  description: string option
    (**
      A description of the API. CommonMark syntax MAY be used for rich text
      representation.
    *);
  termsOfService: string option
    (**
      A URL to the Terms of Service for the API. This MUST be in the form of
      a URL.
    *);
  contact: todo option (** The contact information for the exposed API. *);
  license: todo option (** The license information for the exposed API. *)
}

type externalDocs = Openapi_t.externalDocs

type components = Openapi_t.components = {
  schemas: (string * schema) list;
  parameters: (string * parameter) list;
  requestBodies: (string * request_body) list;
  pathItems: (string * path_item) list;
  responses: todo option;
  examples: todo option;
  headers: todo option;
  securitySchemes: todo option;
  links: todo option;
  callbacks: todo option
}

type t = Openapi_t.t = {
  openapi: version;
  info: info;
  servers: server list;
  paths: paths;
  components: components option;
  jsonSchemaDialect: string option;
  webhooks: webhooks option;
  security: security option;
  tags: tags option;
  externalDocs: externalDocs option
}

type password = Openapi_t.password

type int64 = Openapi_t.int64

type int32 = Openapi_t.int32

type float_ = Openapi_t.float_

type double = Openapi_t.double

val write_json :
  Buffer.t -> json -> unit
  (** Output a JSON value of type {!type:json}. *)

val string_of_json :
  ?len:int -> json -> string
  (** Serialize a value of type {!type:json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> json
  (** Input JSON data of type {!type:json}. *)

val json_of_string :
  string -> json
  (** Deserialize JSON data of type {!type:json}. *)

val write_todo :
  Buffer.t -> todo -> unit
  (** Output a JSON value of type {!type:todo}. *)

val string_of_todo :
  ?len:int -> todo -> string
  (** Serialize a value of type {!type:todo}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_todo :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> todo
  (** Input JSON data of type {!type:todo}. *)

val todo_of_string :
  string -> todo
  (** Deserialize JSON data of type {!type:todo}. *)

val write_webhooks :
  Buffer.t -> webhooks -> unit
  (** Output a JSON value of type {!type:webhooks}. *)

val string_of_webhooks :
  ?len:int -> webhooks -> string
  (** Serialize a value of type {!type:webhooks}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_webhooks :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> webhooks
  (** Input JSON data of type {!type:webhooks}. *)

val webhooks_of_string :
  string -> webhooks
  (** Deserialize JSON data of type {!type:webhooks}. *)

val write_version :
  Buffer.t -> version -> unit
  (** Output a JSON value of type {!type:version}. *)

val string_of_version :
  ?len:int -> version -> string
  (** Serialize a value of type {!type:version}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_version :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> version
  (** Input JSON data of type {!type:version}. *)

val version_of_string :
  string -> version
  (** Deserialize JSON data of type {!type:version}. *)

val write_tags :
  Buffer.t -> tags -> unit
  (** Output a JSON value of type {!type:tags}. *)

val string_of_tags :
  ?len:int -> tags -> string
  (** Serialize a value of type {!type:tags}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tags :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tags
  (** Input JSON data of type {!type:tags}. *)

val tags_of_string :
  string -> tags
  (** Deserialize JSON data of type {!type:tags}. *)

val write_server :
  Buffer.t -> server -> unit
  (** Output a JSON value of type {!type:server}. *)

val string_of_server :
  ?len:int -> server -> string
  (** Serialize a value of type {!type:server}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_server :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> server
  (** Input JSON data of type {!type:server}. *)

val server_of_string :
  string -> server
  (** Deserialize JSON data of type {!type:server}. *)

val write_security :
  Buffer.t -> security -> unit
  (** Output a JSON value of type {!type:security}. *)

val string_of_security :
  ?len:int -> security -> string
  (** Serialize a value of type {!type:security}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_security :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> security
  (** Input JSON data of type {!type:security}. *)

val security_of_string :
  string -> security
  (** Deserialize JSON data of type {!type:security}. *)

val write_schema :
  Buffer.t -> schema -> unit
  (** Output a JSON value of type {!type:schema}. *)

val string_of_schema :
  ?len:int -> schema -> string
  (** Serialize a value of type {!type:schema}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_schema :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> schema
  (** Input JSON data of type {!type:schema}. *)

val schema_of_string :
  string -> schema
  (** Deserialize JSON data of type {!type:schema}. *)

val write_media_type :
  Buffer.t -> media_type -> unit
  (** Output a JSON value of type {!type:media_type}. *)

val string_of_media_type :
  ?len:int -> media_type -> string
  (** Serialize a value of type {!type:media_type}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_media_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> media_type
  (** Input JSON data of type {!type:media_type}. *)

val media_type_of_string :
  string -> media_type
  (** Deserialize JSON data of type {!type:media_type}. *)

val write_media :
  Buffer.t -> media -> unit
  (** Output a JSON value of type {!type:media}. *)

val string_of_media :
  ?len:int -> media -> string
  (** Serialize a value of type {!type:media}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_media :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> media
  (** Input JSON data of type {!type:media}. *)

val media_of_string :
  string -> media
  (** Deserialize JSON data of type {!type:media}. *)

val write_response :
  Buffer.t -> response -> unit
  (** Output a JSON value of type {!type:response}. *)

val string_of_response :
  ?len:int -> response -> string
  (** Serialize a value of type {!type:response}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_response :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> response
  (** Input JSON data of type {!type:response}. *)

val response_of_string :
  string -> response
  (** Deserialize JSON data of type {!type:response}. *)

val write_request_body :
  Buffer.t -> request_body -> unit
  (** Output a JSON value of type {!type:request_body}. *)

val string_of_request_body :
  ?len:int -> request_body -> string
  (** Serialize a value of type {!type:request_body}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_request_body :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> request_body
  (** Input JSON data of type {!type:request_body}. *)

val request_body_of_string :
  string -> request_body
  (** Deserialize JSON data of type {!type:request_body}. *)

val write_reference :
  Buffer.t -> reference -> unit
  (** Output a JSON value of type {!type:reference}. *)

val string_of_reference :
  ?len:int -> reference -> string
  (** Serialize a value of type {!type:reference}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_reference :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> reference
  (** Input JSON data of type {!type:reference}. *)

val reference_of_string :
  string -> reference
  (** Deserialize JSON data of type {!type:reference}. *)

val write_param_location :
  Buffer.t -> param_location -> unit
  (** Output a JSON value of type {!type:param_location}. *)

val string_of_param_location :
  ?len:int -> param_location -> string
  (** Serialize a value of type {!type:param_location}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_param_location :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> param_location
  (** Input JSON data of type {!type:param_location}. *)

val param_location_of_string :
  string -> param_location
  (** Deserialize JSON data of type {!type:param_location}. *)

val write_parameter :
  Buffer.t -> parameter -> unit
  (** Output a JSON value of type {!type:parameter}. *)

val string_of_parameter :
  ?len:int -> parameter -> string
  (** Serialize a value of type {!type:parameter}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_parameter :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> parameter
  (** Input JSON data of type {!type:parameter}. *)

val parameter_of_string :
  string -> parameter
  (** Deserialize JSON data of type {!type:parameter}. *)

val write_or_ref :
  (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a or_ref -> unit
  (** Output a JSON value of type {!type:or_ref}. *)

val string_of_or_ref :
  (Buffer.t -> 'a -> unit) ->
  ?len:int -> 'a or_ref -> string
  (** Serialize a value of type {!type:or_ref}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_or_ref :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a or_ref
  (** Input JSON data of type {!type:or_ref}. *)

val or_ref_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a or_ref
  (** Deserialize JSON data of type {!type:or_ref}. *)

val write_operation :
  Buffer.t -> operation -> unit
  (** Output a JSON value of type {!type:operation}. *)

val string_of_operation :
  ?len:int -> operation -> string
  (** Serialize a value of type {!type:operation}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_operation :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> operation
  (** Input JSON data of type {!type:operation}. *)

val operation_of_string :
  string -> operation
  (** Deserialize JSON data of type {!type:operation}. *)

val write_path_item :
  Buffer.t -> path_item -> unit
  (** Output a JSON value of type {!type:path_item}. *)

val string_of_path_item :
  ?len:int -> path_item -> string
  (** Serialize a value of type {!type:path_item}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_path_item :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> path_item
  (** Input JSON data of type {!type:path_item}. *)

val path_item_of_string :
  string -> path_item
  (** Deserialize JSON data of type {!type:path_item}. *)

val write_path :
  Buffer.t -> path -> unit
  (** Output a JSON value of type {!type:path}. *)

val string_of_path :
  ?len:int -> path -> string
  (** Serialize a value of type {!type:path}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_path :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> path
  (** Input JSON data of type {!type:path}. *)

val path_of_string :
  string -> path
  (** Deserialize JSON data of type {!type:path}. *)

val write_paths :
  Buffer.t -> paths -> unit
  (** Output a JSON value of type {!type:paths}. *)

val string_of_paths :
  ?len:int -> paths -> string
  (** Serialize a value of type {!type:paths}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_paths :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> paths
  (** Input JSON data of type {!type:paths}. *)

val paths_of_string :
  string -> paths
  (** Deserialize JSON data of type {!type:paths}. *)

val write_info :
  Buffer.t -> info -> unit
  (** Output a JSON value of type {!type:info}. *)

val string_of_info :
  ?len:int -> info -> string
  (** Serialize a value of type {!type:info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> info
  (** Input JSON data of type {!type:info}. *)

val info_of_string :
  string -> info
  (** Deserialize JSON data of type {!type:info}. *)

val write_externalDocs :
  Buffer.t -> externalDocs -> unit
  (** Output a JSON value of type {!type:externalDocs}. *)

val string_of_externalDocs :
  ?len:int -> externalDocs -> string
  (** Serialize a value of type {!type:externalDocs}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_externalDocs :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> externalDocs
  (** Input JSON data of type {!type:externalDocs}. *)

val externalDocs_of_string :
  string -> externalDocs
  (** Deserialize JSON data of type {!type:externalDocs}. *)

val write_components :
  Buffer.t -> components -> unit
  (** Output a JSON value of type {!type:components}. *)

val string_of_components :
  ?len:int -> components -> string
  (** Serialize a value of type {!type:components}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_components :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> components
  (** Input JSON data of type {!type:components}. *)

val components_of_string :
  string -> components
  (** Deserialize JSON data of type {!type:components}. *)

val write_t :
  Buffer.t -> t -> unit
  (** Output a JSON value of type {!type:t}. *)

val string_of_t :
  ?len:int -> t -> string
  (** Serialize a value of type {!type:t}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_t :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
  (** Input JSON data of type {!type:t}. *)

val t_of_string :
  string -> t
  (** Deserialize JSON data of type {!type:t}. *)

val write_password :
  Buffer.t -> password -> unit
  (** Output a JSON value of type {!type:password}. *)

val string_of_password :
  ?len:int -> password -> string
  (** Serialize a value of type {!type:password}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_password :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> password
  (** Input JSON data of type {!type:password}. *)

val password_of_string :
  string -> password
  (** Deserialize JSON data of type {!type:password}. *)

val write_int64 :
  Buffer.t -> int64 -> unit
  (** Output a JSON value of type {!type:int64}. *)

val string_of_int64 :
  ?len:int -> int64 -> string
  (** Serialize a value of type {!type:int64}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_int64 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> int64
  (** Input JSON data of type {!type:int64}. *)

val int64_of_string :
  string -> int64
  (** Deserialize JSON data of type {!type:int64}. *)

val write_int32 :
  Buffer.t -> int32 -> unit
  (** Output a JSON value of type {!type:int32}. *)

val string_of_int32 :
  ?len:int -> int32 -> string
  (** Serialize a value of type {!type:int32}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_int32 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> int32
  (** Input JSON data of type {!type:int32}. *)

val int32_of_string :
  string -> int32
  (** Deserialize JSON data of type {!type:int32}. *)

val write_float_ :
  Buffer.t -> float_ -> unit
  (** Output a JSON value of type {!type:float_}. *)

val string_of_float_ :
  ?len:int -> float_ -> string
  (** Serialize a value of type {!type:float_}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_float_ :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> float_
  (** Input JSON data of type {!type:float_}. *)

val float__of_string :
  string -> float_
  (** Deserialize JSON data of type {!type:float_}. *)

val write_double :
  Buffer.t -> double -> unit
  (** Output a JSON value of type {!type:double}. *)

val string_of_double :
  ?len:int -> double -> string
  (** Serialize a value of type {!type:double}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_double :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> double
  (** Input JSON data of type {!type:double}. *)

val double_of_string :
  string -> double
  (** Deserialize JSON data of type {!type:double}. *)

