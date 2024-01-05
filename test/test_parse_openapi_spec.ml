open Tezt
open Tezt.Base

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
