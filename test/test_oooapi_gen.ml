open Tezt
open Tezt.Base

let test ?(tags = [ "oooapi" ]) title f = Test.register ~__FILE__ ~title ~tags f
;;

test "can generate library for openai spec" @@ fun () ->
let spec = Openapi_spec.from_file "openapi-openai.json" |> Result.get_ok in
let () =
  Out_channel.with_open_text "openai_api.ml" (fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Oooapi.(spec |> module_of_spec |> write_ast_f fmt))
in
unit