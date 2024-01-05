open Tezt
open Tezt.Base
module O = Openapi_spec

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

let default_components : Openapi_spec.components =
  { schemas = None
  ; responses = None
  ; parameters = []
  ; examples = None
  ; requestBodies = None
  ; headers = None
  ; securitySchemes = None
  ; links = None
  ; callbacks = None
  ; pathItems = []
  }

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
;;

test "can resolve parameter references" @@ fun () ->
let path = [ `C "foo" ] in
let name = "fooParam" in
let spec : O.t =
  { default_spec with
    paths =
      [ ( path
        , { default_path_item with
            parameters = Some [ ref_ "#/components/parameters/fooParam" ]
          } )
      ]
  ; components =
      Some
        { default_components with
          parameters = [ (name, O.create_parameter ~name ~in_:"path" ()) ]
        }
  }
in
let resolved_spec = Openapi_spec.resolve_refs spec in
match (List.assoc path resolved_spec.paths).parameters with
| Some [ `Obj param ] ->
    Check.(
      (param.name = name)
        string
        ~error_msg:"Expected parameter reference to be resolved");
    unit
| _ -> Test.fail "Expected parameter was not found"
