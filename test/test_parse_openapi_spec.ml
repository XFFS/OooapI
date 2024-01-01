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
let json = In_channel.(with_open_text "openapi-openai.json" input_all) in
match Openapi_spec.of_json json with
| Ok _ -> unit
| Error (`Msg err) -> Test.fail "parse failure: %s" err
| exception exn ->
    Test.fail "exception while paring: %S" (Printexc.to_string exn)
;;

test "can parse tictactoe example spec" @@ fun () ->
let json = In_channel.(with_open_text "tictactoe.json" input_all) in
match Openapi_spec.of_json json with
| Ok _ -> unit
| Error (`Msg err) -> Test.fail "parse failure: %s" err
| exception exn ->
    Test.fail "exception while paring: %S" (Printexc.to_string exn)
