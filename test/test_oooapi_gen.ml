open Tezt
open Lib

let test ?(tags = [ "oooapi" ]) title f = Test.register ~__FILE__ ~title ~tags f
;;

test "can generate library for openai spec" @@ fun () ->
match Openapi_spec.from_file "openapi-openai.json" with
| Error (`Msg err) -> Test.fail "parse failure: %s" err
| exception exn ->
    Test.fail "exception while paring: %S" (Printexc.to_string exn)
| Ok spec ->
    Lwt.return
    @@ Out_channel.with_open_text "openai_api.ml" (fun oc ->
           let fmt = Format.formatter_of_out_channel oc in
           Oooapi.(spec |> module_of_spec |> write_ast_f fmt))
