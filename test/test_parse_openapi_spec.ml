open Tezt
open Tezt.Base
module O = Openapi_spec

(* TEST HELPERS *)

(* A schema that is a ref *)
let ref_schema ref' : O.schema =
  let trimmed = String.sub ref' 1 (String.length ref' - 1) in
  let path = Json_query.path_of_json_pointer trimmed in
  { name = None
  ; schema =
      Json_schema.(
        create
          { kind = Def_ref path
          ; title = Some "Ref schema"
          ; description = None
          ; default = None
          ; enum = None
          ; id = None
          ; format = None
          })
  }

let dummy_schema_type : O.schema =
  { name = None
  ; schema =
      Json_schema.(
        create
          { kind = Boolean
          ; title = Some "Test schema"
          ; description = None
          ; default = None
          ; enum = None
          ; id = None
          ; format = None
          })
  }

let test ?(tags = [ "openapi_spec" ]) title f =
  Test.register ~__FILE__ ~title ~tags f
;;

test "can parse paths" @@ fun () ->
let open Openapi_spec in
let path_params path = Openapi_path.(of_string path |> params) in
Check.(
  (path_params "foo/bar/baz" = [])
    (list string)
    ~error_msg:"expected no parameters");
Check.(
  (path_params "foo/{bar}/baz/{bing}/bong" = [ "bar"; "bing" ])
    (list string)
    ~error_msg:"expected parameters");
unit
;;

test "can parse openapi spec" @@ fun () ->
match Openapi_spec.from_file "openapi-openai.json" with
| Ok _ -> unit
| Error (`Msg err) -> Test.fail "parse failure: %s" err
| exception exn ->
    Test.fail "exception while paring: %S" (Printexc.to_string exn)
;;

test "can parse tictactoe example spec" @@ fun () ->
match Openapi_spec.from_file "tictactoe.json" with
| Error (`Msg err) -> Test.fail "parse failure: %s" err
| exception exn ->
    Test.fail "exception while paring: %S" (Printexc.to_string exn)
| Ok spec ->
match
  let path = Openapi_spec.Openapi_path.of_string "/board/{row}/{column}" in
  List.assoc_opt path spec.paths
with
| None -> Test.fail "tictactoe spec missing expected path /board/{row}/{column}"
| Some path_item ->
match path_item.parameters with
| None
| Some [] ->
    Test.fail "tictactoe spec missing expected path parameters"
| Some [ `Ref _; `Ref _ ] -> unit
| Some _ ->
    Test.fail
      "tictactoe spec does not have expected, ref parameter in \
       /board/{row}/{column} path"

(* TESTING REFERENCE RESOLUTION *)

let default_spec : Openapi_spec.t =
  { openapi = "3.0.0"
  ; info =
      { title = "test spec"
      ; version = "0.1"
      ; summary = None
      ; description = None
      ; termsOfService = None
      ; contact = None
      ; license = None
      }
  ; servers = []
  ; paths = []
  ; components = None
  ; jsonSchemaDialect = None
  ; webhooks = None
  ; security = None
  ; tags = None
  ; externalDocs = None
  }

let default_path_item : Openapi_spec.path_item =
  { ref_ = None
  ; summary = None
  ; description = None
  ; get = None
  ; put = None
  ; post = None
  ; delete = None
  ; options = None
  ; head = None
  ; patch = None
  ; trace = None
  ; servers = None
  ; parameters = None
  }

let default_components : Openapi_spec.components = O.create_components ()

let ref_ str : _ Openapi_spec.or_ref =
  `Ref { ref_ = str; description = None; summary = None }
;;

test "detects errors on dangling reference" @@ fun () ->
let spec : Openapi_spec.t =
  { default_spec with
    paths =
      [ ( [ `C "foo" ]
        , { default_path_item with
            ref_ = Some "#/components/pathItems/fooPath"
          } )
      ]
  }
in
Check.raises
  ~error_msg:"Expected Invalid_reference"
  (Openapi_spec.Invalid_reference "#/components/pathItems/fooPath")
  (fun () -> ignore (Openapi_spec.resolve_refs spec));
unit
;;

test "detects errors on invalid reference" @@ fun () ->
let spec : Openapi_spec.t =
  { default_spec with
    paths =
      [ ( [ `C "foo" ]
        , { default_path_item with
            ref_ = Some "http://unsupported/reference/uri"
          } )
      ]
  }
in
Check.raises
  ~error_msg:"Expected Unsupported_reference"
  (Openapi_spec.Unsupported_reference
     ("http://unsupported/reference/uri", "Unkown reason"))
  (fun () -> ignore (Openapi_spec.resolve_refs spec));
unit
;;

test "can resolve path references" @@ fun () ->
let path = [ `C "foo" ] in
let summary = Some "Path foo" in
let spec : Openapi_spec.t =
  { default_spec with
    paths =
      [ ( path
        , { default_path_item with ref_ = Some "#/components/pathItems/fooId" }
        )
      ]
  ; components =
      Some
        { default_components with
          pathItems = [ ("fooId", { default_path_item with summary }) ]
        }
  }
in
let resolved_spec = Openapi_spec.resolve_refs spec in
Check.(
  ((List.assoc path resolved_spec.paths).summary = summary)
    (option string)
    ~error_msg:"Expected path reference to be resolved");
unit

(* Make a spec with default args provided *)
let spec =
  O.create_t
    ~openapi:"3.0.0"
    ~info:(O.create_info ~title:"test spec" ~version:"0.1" ())
;;

test "can resolve path parameter references" @@ fun () ->
let path = [ `C "bar"; `P "foo" ] in
let name = "fooParam" in
let schema_name = "fooSchema" in
let spec : O.t =
  spec
    ~paths:
      [ ( path
        , O.create_path_item
            ~parameters:[ ref_ ("#/components/parameters/" ^ name) ]
            () )
      ]
    ~components:
      (O.create_components
         ()
         ~parameters:
           [ ( name
             , O.create_parameter
                 ~name
                 ~in_:`Path
                 ~schema:(ref_schema ("#/components/schemas/" ^ schema_name))
                 () )
           ]
         ~schemas:[ (schema_name, dummy_schema_type) ])
    ()
in
let resolved_spec = Openapi_spec.resolve_refs spec in
match (List.assoc path resolved_spec.paths).parameters with
| Some [ `Obj param ] ->
    Check.(
      (param.name = name)
        string
        ~error_msg:"Expected parameter reference to be resolved");
    Check.(
      match param.schema with
      | None -> Test.fail "Param somehow lost its schema"
      | Some s ->
          Check.(s.name = Some schema_name)
            (option string)
            ~error_msg:"Expected param schema reference to be resolved");
    unit
| _ -> Test.fail "Expected parameter was not found"
;;

test "can resolve operation parameter references" @@ fun () ->
let path = [ `C "foo" ] in
let name = "fooParam" in
let spec : O.t =
  spec
    ~paths:
      [ ( path
        , O.create_path_item
            ~get:
              (O.create_operation
                 ~parameters:[ ref_ ("#/components/parameters/" ^ name) ]
                 ())
            () )
      ]
    ~components:
      (O.create_components
         ~parameters:
           [ ( name
             , O.create_parameter
                 ()
                 ~name
                 ~in_:`Header
                 ~schema:dummy_schema_type )
           ]
         ())
    ()
in
let resolved_spec = O.resolve_refs spec in
match
  Option.bind
    (List.assoc path resolved_spec.paths).get
    (fun (x : O.operation) -> x.parameters)
with
| Some [ `Obj param ] ->
    Check.(
      (param.name = name)
        string
        ~error_msg:"Expected parameter reference to be resolved");
    unit
| _ -> Test.fail "Expected parameter was not found"
;;

test "can resolve request body references" @@ fun () ->
let path = [ `C "foo" ] in
let name = "fooReqBody" in
let spec : O.t =
  spec
    ~paths:
      [ ( path
        , O.create_path_item
            ~get:
              (O.create_operation
                 ~requestBody:(ref_ ("#/components/requestBodies/" ^ name))
                 ())
            () )
      ]
    ~components:
      (O.create_components
         ~requestBodies:
           [ ( name
             , O.create_request_body ~description:"Foo req body" ~content:[] ()
             )
           ]
         ())
    ()
in
let resolved_spec = O.resolve_refs spec in
match
  Option.bind
    (List.assoc path resolved_spec.paths).get
    (fun (x : O.operation) -> x.requestBody)
with
| Some (`Obj req_body) ->
    Check.(
      (req_body.description = Some "Foo req body")
        (option string)
        ~error_msg:"Expected reference to be resolved");
    unit
| _ -> Test.fail "Expected object was not found"
;;

test "can resolve request body content references" @@ fun () ->
(* We also test nested references here: the request body is a
   reference to a body that has a reference to a schema *)
let path = [ `C "foo" ] in
let name = "fooReqBody" in
let schema_name = "fooContentSchema" in
let spec : O.t =
  spec
    ~paths:
      [ ( path
        , O.create_path_item
            ()
            ~get:
              (O.create_operation
                 ()
                 ~requestBody:(ref_ ("#/components/requestBodies/" ^ name))) )
      ]
    ~components:
      (O.create_components
         ()
         ~schemas:[ (schema_name, dummy_schema_type) ]
         ~requestBodies:
           [ ( name
             , O.create_request_body
                 ()
                 ~description:"Foo req body"
                 ~content:
                   [ ( "application/json"
                     , O.create_media_type
                         ()
                         ~schema:
                           (ref_schema ("#/components/schemas/" ^ schema_name))
                     )
                   ] )
           ])
    ()
in
let resolved_spec = O.resolve_refs spec in
match
  Option.bind
    (List.assoc path resolved_spec.paths).get
    (fun (x : O.operation) -> x.requestBody)
with
| Some (`Obj { content = (_, { schema = Some schema; _ }) :: _; _ }) ->
    Check.(
      (schema.name = Some schema_name)
        (option string)
        ~error_msg:"Expected reference to be resolved");
    unit
| _ -> Test.fail "Expected object was not found"
;;

test "can resolve response content reference" @@ fun () ->
(* We also test nested references here: the request body is a
   reference to a body that has a reference to a schema *)
let path = [ `C "foo" ] in
let name = "fooRespContent" in
let spec : O.t =
  (* This formatting is terrible. I blame ocamlformat *)
  spec
    ()
    ~paths:
      [ ( path
        , O.create_path_item
            ()
            ~post:
              (O.create_operation
                 ()
                 ~responses:
                   [ ( "200"
                     , O.create_response
                         ()
                         ~description:
                           "Why is this the only required description field?"
                         ~content:
                           [ ( "application/json"
                             , O.create_media_type
                                 ()
                                 ~schema:
                                   (ref_schema ("#/components/schemas/" ^ name))
                             )
                           ] )
                   ]) )
      ]
    ~components:(O.create_components () ~schemas:[ (name, dummy_schema_type) ])
in
let resolved_spec = O.resolve_refs spec in
match
  Option.map
    (fun (x : O.operation) -> x.responses)
    (List.assoc path resolved_spec.paths).post
with
| Some
    [ ( "200"
      , ({ content = Some [ ("application/json", { schema = Some schema; _ }) ]
         ; _
         } :
          O.response) )
    ] ->
    Check.(
      (schema.name = Some name)
        (option string)
        ~error_msg:"Expected reference to be resolved");
    unit
| _ -> Test.fail "Expected object was not found"
