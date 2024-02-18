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

let validate_json = (
  Json.validate_t
)
let validate_todo = (
  validate_json
)
let validate_webhooks = (
  validate_todo
)
let validate_version = (
  (fun _ _ -> None)
)
let validate_tags = (
  validate_todo
)
let validate__string_todo_list = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let _, x = x in
      (
        validate_todo
      ) (`Index 1 :: path) x
      )
  )
)
let validate__x_796bf02 = (
  Atdgen_runtime.Ov_run.validate_option (
    validate__string_todo_list
  )
)
let validate__string_option = (
  fun _ _ -> None
)
let validate_server : _ -> server -> _ = (
  fun path x ->
    (
      validate__x_796bf02
    ) (`Field "variables" :: path) x.variables
)
let validate_security = (
  validate_todo
)
let validate__x_6d755e1 = (
  fun _ _ -> None
)
let validate_schema = (
  validate__x_6d755e1
)
let validate__todo_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_todo
  )
)
let validate__schema_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_schema
  )
)
let validate_media_type : _ -> media_type -> _ = (
  fun path x ->
    match
      (
        validate__schema_option
      ) (`Field "schema" :: path) x.schema
    with
      | Some _ as err -> err
      | None ->
        match
          (
            validate__todo_option
          ) (`Field "example" :: path) x.example
        with
          | Some _ as err -> err
          | None ->
            match
              (
                validate__todo_option
              ) (`Field "examples" :: path) x.examples
            with
              | Some _ as err -> err
              | None ->
                (
                  validate__todo_option
                ) (`Field "encoding" :: path) x.encoding
)
let validate_media = (
  (fun _ _ -> None)
)
let validate__x_acfd536 = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let _, x = x in
      (
        validate_media_type
      ) (`Index 1 :: path) x
      )
  )
)
let validate__x_ac7bbbd = (
  Atdgen_runtime.Ov_run.validate_option (
    validate__x_acfd536
  )
)
let validate_response : _ -> response -> _ = (
  fun path x ->
    match
      (
        validate__todo_option
      ) (`Field "headers" :: path) x.headers
    with
      | Some _ as err -> err
      | None ->
        match
          (
            validate__x_ac7bbbd
          ) (`Field "content" :: path) x.content
        with
          | Some _ as err -> err
          | None ->
            (
              validate__todo_option
            ) (`Field "links" :: path) x.links
)
let validate__x_4e54759 = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let _, x = x in
      (
        validate_media_type
      ) (`Index 1 :: path) x
      )
  )
)
let validate_request_body : _ -> request_body -> _ = (
  fun path x ->
    (
      validate__x_4e54759
    ) (`Field "content" :: path) x.content
)
let validate_reference : _ -> reference -> _ = (
  fun _ _ -> None
)
let validate_param_location = (
  fun _ _ -> None
)
let validate__x_6854380 = (
  Atdgen_runtime.Ov_run.validate_option (
    validate__x_4e54759
  )
)
let validate__json_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_json
  )
)
let validate__bool_option = (
  fun _ _ -> None
)
let validate_parameter : _ -> parameter -> _ = (
  fun path x ->
    match
      (
        validate__schema_option
      ) (`Field "schema" :: path) x.schema
    with
      | Some _ as err -> err
      | None ->
        match
          (
            validate__json_option
          ) (`Field "example" :: path) x.example
        with
          | Some _ as err -> err
          | None ->
            match
              (
                validate__todo_option
              ) (`Field "examples" :: path) x.examples
            with
              | Some _ as err -> err
              | None ->
                (
                  validate__x_6854380
                ) (`Field "content" :: path) x.content
)
let validate__x_157be2f = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let _, x = x in
      (
        validate_response
      ) (`Index 1 :: path) x
      )
  )
)
let validate__string_list = (
  fun _ _ -> None
)
let validate__string_list_option = (
  fun _ _ -> None
)
let validate__server_list = (
  Atdgen_runtime.Ov_run.validate_list (
    validate_server
  )
)
let validate__server_list_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate__server_list
  )
)
let validate__request_body_or_ref = (
  fun path x ->
    match x with
      | `Ref x ->
        (
          validate_reference
        ) path x
      | `Obj x ->
        (
          validate_request_body
        ) path x
)
let validate__request_body_or_ref_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate__request_body_or_ref
  )
)
let validate__parameter_or_ref = (
  fun path x ->
    match x with
      | `Ref x ->
        (
          validate_reference
        ) path x
      | `Obj x ->
        (
          validate_parameter
        ) path x
)
let validate__parameter_or_ref_list = (
  Atdgen_runtime.Ov_run.validate_list (
    validate__parameter_or_ref
  )
)
let validate__parameter_or_ref_list_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate__parameter_or_ref_list
  )
)
let validate_operation : _ -> operation -> _ = (
  fun path x ->
    match
      (
        validate__todo_option
      ) (`Field "externalDocs" :: path) x.externalDocs
    with
      | Some _ as err -> err
      | None ->
        match
          (
            validate__parameter_or_ref_list_option
          ) (`Field "parameters" :: path) x.parameters
        with
          | Some _ as err -> err
          | None ->
            match
              (
                validate__request_body_or_ref_option
              ) (`Field "requestBody" :: path) x.requestBody
            with
              | Some _ as err -> err
              | None ->
                match
                  (
                    validate__x_157be2f
                  ) (`Field "responses" :: path) x.responses
                with
                  | Some _ as err -> err
                  | None ->
                    match
                      (
                        validate__todo_option
                      ) (`Field "callbacks" :: path) x.callbacks
                    with
                      | Some _ as err -> err
                      | None ->
                        match
                          (
                            validate__todo_option
                          ) (`Field "security" :: path) x.security
                        with
                          | Some _ as err -> err
                          | None ->
                            (
                              validate__server_list_option
                            ) (`Field "servers" :: path) x.servers
)
let validate__operation_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_operation
  )
)
let validate_path_item : _ -> path_item -> _ = (
  fun path x ->
    match
      (
        validate__operation_option
      ) (`Field "get" :: path) x.get
    with
      | Some _ as err -> err
      | None ->
        match
          (
            validate__operation_option
          ) (`Field "put" :: path) x.put
        with
          | Some _ as err -> err
          | None ->
            match
              (
                validate__operation_option
              ) (`Field "post" :: path) x.post
            with
              | Some _ as err -> err
              | None ->
                match
                  (
                    validate__operation_option
                  ) (`Field "delete" :: path) x.delete
                with
                  | Some _ as err -> err
                  | None ->
                    match
                      (
                        validate__operation_option
                      ) (`Field "options" :: path) x.options
                    with
                      | Some _ as err -> err
                      | None ->
                        match
                          (
                            validate__operation_option
                          ) (`Field "head" :: path) x.head
                        with
                          | Some _ as err -> err
                          | None ->
                            match
                              (
                                validate__operation_option
                              ) (`Field "patch" :: path) x.patch
                            with
                              | Some _ as err -> err
                              | None ->
                                match
                                  (
                                    validate__operation_option
                                  ) (`Field "trace" :: path) x.trace
                                with
                                  | Some _ as err -> err
                                  | None ->
                                    match
                                      (
                                        validate__server_list_option
                                      ) (`Field "servers" :: path) x.servers
                                    with
                                      | Some _ as err -> err
                                      | None ->
                                        (
                                          validate__parameter_or_ref_list_option
                                        ) (`Field "parameters" :: path) x.parameters
)
let validate__x_5734e50 = (
  fun _ _ -> None
)
let validate_path = (
  validate__x_5734e50
)
let validate__x_9e0f808 = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let _, x = x in
      (
        validate_path_item
      ) (`Index 1 :: path) x
      )
  )
)
let validate_paths = (
  validate__x_9e0f808
)
let validate_info : _ -> info -> _ = (
  fun path x ->
    match
      (
        validate__todo_option
      ) (`Field "contact" :: path) x.contact
    with
      | Some _ as err -> err
      | None ->
        (
          validate__todo_option
        ) (`Field "license" :: path) x.license
)
let validate_externalDocs = (
  validate_todo
)
let validate__x_95a74d2 = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let _, x = x in
      (
        validate_schema
      ) (`Index 1 :: path) x
      )
  )
)
let validate__x_5c2577b = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let _, x = x in
      (
        validate_parameter
      ) (`Index 1 :: path) x
      )
  )
)
let validate__x_1101895 = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let _, x = x in
      (
        validate_request_body
      ) (`Index 1 :: path) x
      )
  )
)
let validate__x_0c065f0 = (
  Atdgen_runtime.Ov_run.validate_list (
    fun path x ->
      (let _, x = x in
      (
        validate_path_item
      ) (`Index 1 :: path) x
      )
  )
)
let validate_components : _ -> components -> _ = (
  fun path x ->
    match
      (
        validate__x_95a74d2
      ) (`Field "schemas" :: path) x.schemas
    with
      | Some _ as err -> err
      | None ->
        match
          (
            validate__x_5c2577b
          ) (`Field "parameters" :: path) x.parameters
        with
          | Some _ as err -> err
          | None ->
            match
              (
                validate__x_1101895
              ) (`Field "requestBodies" :: path) x.requestBodies
            with
              | Some _ as err -> err
              | None ->
                match
                  (
                    validate__x_0c065f0
                  ) (`Field "pathItems" :: path) x.pathItems
                with
                  | Some _ as err -> err
                  | None ->
                    match
                      (
                        validate__todo_option
                      ) (`Field "responses" :: path) x.responses
                    with
                      | Some _ as err -> err
                      | None ->
                        match
                          (
                            validate__todo_option
                          ) (`Field "examples" :: path) x.examples
                        with
                          | Some _ as err -> err
                          | None ->
                            match
                              (
                                validate__todo_option
                              ) (`Field "headers" :: path) x.headers
                            with
                              | Some _ as err -> err
                              | None ->
                                match
                                  (
                                    validate__todo_option
                                  ) (`Field "securitySchemes" :: path) x.securitySchemes
                                with
                                  | Some _ as err -> err
                                  | None ->
                                    match
                                      (
                                        validate__todo_option
                                      ) (`Field "links" :: path) x.links
                                    with
                                      | Some _ as err -> err
                                      | None ->
                                        (
                                          validate__todo_option
                                        ) (`Field "callbacks" :: path) x.callbacks
)
let validate__webhooks_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_webhooks
  )
)
let validate__tags_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_tags
  )
)
let validate__security_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_security
  )
)
let validate__externalDocs_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_externalDocs
  )
)
let validate__components_option = (
  Atdgen_runtime.Ov_run.validate_option (
    validate_components
  )
)
let validate_t : _ -> t -> _ = (
  fun path x ->
    match
      (
        validate_info
      ) (`Field "info" :: path) x.info
    with
      | Some _ as err -> err
      | None ->
        match
          (
            validate__server_list
          ) (`Field "servers" :: path) x.servers
        with
          | Some _ as err -> err
          | None ->
            match
              (
                validate_paths
              ) (`Field "paths" :: path) x.paths
            with
              | Some _ as err -> err
              | None ->
                match
                  (
                    validate__components_option
                  ) (`Field "components" :: path) x.components
                with
                  | Some _ as err -> err
                  | None ->
                    match
                      (
                        validate__webhooks_option
                      ) (`Field "webhooks" :: path) x.webhooks
                    with
                      | Some _ as err -> err
                      | None ->
                        match
                          (
                            validate__security_option
                          ) (`Field "security" :: path) x.security
                        with
                          | Some _ as err -> err
                          | None ->
                            match
                              (
                                validate__tags_option
                              ) (`Field "tags" :: path) x.tags
                            with
                              | Some _ as err -> err
                              | None ->
                                (
                                  validate__externalDocs_option
                                ) (`Field "externalDocs" :: path) x.externalDocs
)
let validate_password = (
  (fun _ _ -> None)
)
let validate_or_ref validate__a = (
  fun path x ->
    match x with
      | `Ref x ->
        (
          validate_reference
        ) path x
      | `Obj x ->
        (
          validate__a
        ) path x
)
let validate_int64 = (
  (fun _ _ -> None)
)
let validate_int32 = (
  (fun _ _ -> None)
)
let validate_float_ = (
  (fun _ _ -> None)
)
let validate_double = (
  (fun _ _ -> None)
)
let create_server 
  ~url
  ?description
  ?variables
  () : server =
  {
    url = url;
    description = description;
    variables = variables;
  }
let create_media_type 
  ?schema
  ?example
  ?examples
  ?encoding
  () : media_type =
  {
    schema = schema;
    example = example;
    examples = examples;
    encoding = encoding;
  }
let create_response 
  ?(description = "")
  ?headers
  ?content
  ?links
  () : response =
  {
    description = description;
    headers = headers;
    content = content;
    links = links;
  }
let create_request_body 
  ?description
  ~content
  ?(required = false)
  () : request_body =
  {
    description = description;
    content = content;
    required = required;
  }
let create_reference 
  ~ref_
  ?summary
  ?description
  () : reference =
  {
    ref_ = ref_;
    summary = summary;
    description = description;
  }
let create_parameter 
  ~name
  ~in_
  ?description
  ?(required = false)
  ?(deprecated = false)
  ?(allowEmptyValue = false)
  ?style
  ?explode
  ?(allowReserved = false)
  ?schema
  ?example
  ?examples
  ?content
  () : parameter =
  {
    name = name;
    in_ = in_;
    description = description;
    required = required;
    deprecated = deprecated;
    allowEmptyValue = allowEmptyValue;
    style = style;
    explode = explode;
    allowReserved = allowReserved;
    schema = schema;
    example = example;
    examples = examples;
    content = content;
  }
let create_operation 
  ?tags
  ?summary
  ?description
  ?externalDocs
  ?operationId
  ?parameters
  ?requestBody
  ?(responses = [])
  ?callbacks
  ?(deprecated = false)
  ?security
  ?servers
  () : operation =
  {
    tags = tags;
    summary = summary;
    description = description;
    externalDocs = externalDocs;
    operationId = operationId;
    parameters = parameters;
    requestBody = requestBody;
    responses = responses;
    callbacks = callbacks;
    deprecated = deprecated;
    security = security;
    servers = servers;
  }
let create_path_item 
  ?ref_
  ?summary
  ?description
  ?get
  ?put
  ?post
  ?delete
  ?options
  ?head
  ?patch
  ?trace
  ?servers
  ?parameters
  () : path_item =
  {
    ref_ = ref_;
    summary = summary;
    description = description;
    get = get;
    put = put;
    post = post;
    delete = delete;
    options = options;
    head = head;
    patch = patch;
    trace = trace;
    servers = servers;
    parameters = parameters;
  }
let create_info 
  ~title
  ~version
  ?summary
  ?description
  ?termsOfService
  ?contact
  ?license
  () : info =
  {
    title = title;
    version = version;
    summary = summary;
    description = description;
    termsOfService = termsOfService;
    contact = contact;
    license = license;
  }
let create_components 
  ?(schemas = [])
  ?(parameters = [])
  ?(requestBodies = [])
  ?(pathItems = [])
  ?responses
  ?examples
  ?headers
  ?securitySchemes
  ?links
  ?callbacks
  () : components =
  {
    schemas = schemas;
    parameters = parameters;
    requestBodies = requestBodies;
    pathItems = pathItems;
    responses = responses;
    examples = examples;
    headers = headers;
    securitySchemes = securitySchemes;
    links = links;
    callbacks = callbacks;
  }
let create_t 
  ~openapi
  ~info
  ?(servers = [])
  ?(paths = [])
  ?components
  ?jsonSchemaDialect
  ?webhooks
  ?security
  ?tags
  ?externalDocs
  () : t =
  {
    openapi = openapi;
    info = info;
    servers = servers;
    paths = paths;
    components = components;
    jsonSchemaDialect = jsonSchemaDialect;
    webhooks = webhooks;
    security = security;
    tags = tags;
    externalDocs = externalDocs;
  }
