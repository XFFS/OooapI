open Tezt
open Tezt.Base
open Lib.Http_spec

(* TODO Write tests for the http_spec conversion *)
let test ?(tags = [ "http_spec" ]) title f =
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
