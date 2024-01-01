open Tezt
open Tezt.Base
open Lib

let test ?(tags = [ "oooapi" ]) title f = Test.register ~__FILE__ ~title ~tags f
;;

(* TODO Test this in a way we can confirm the generated ocaml with build *)
test "can generate library for openai spec" @@ fun () ->
let spec = Openapi_spec.from_file "openapi-openai.json" |> Result.get_ok in
let () =
  Out_channel.with_open_text "openai_api.ml" (fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Oooapi.(spec |> module_of_spec |> write_ast_f fmt))
in
unit
