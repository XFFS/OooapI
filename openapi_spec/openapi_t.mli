(* Auto-generated from "openapi.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type json = Json.t

type todo = json

type webhooks = todo

type version = string

type tags = todo

type server = {
  url: string;
  description: string option;
  variables: (string * todo) list option
}

type security = todo

type schema = Json_schema_converter.t

type media_type = {
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

type media = string

type response = {
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

type request_body = {
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

type reference = {
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

type param_location = [ `Path | `Query | `Header | `Cookie ]

type parameter = {
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

type 'a or_ref = [ `Ref of reference | `Obj of 'a ]

type operation = {
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

type path_item = {
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

type path = Openapi_path.t

type paths = (path * path_item) list

type info = {
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

type externalDocs = todo

type components = {
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

type t = {
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

type password = string

type int64 = Int64.t

type int32 = Int32.t

type float_ = float

type double = float
