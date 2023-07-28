let api_key = Sys.getenv "OPENAI_API_KEY"

module Config = struct
  let default_headers = Cohttp.Header.add Cohttp.Header.(init ()) "Authorization" ("Bearer " ^ api_key)
end

module Api = Openai_api.Endpoint (Config)

let main =
  let open Lwt.Syntax in
  let* res = Api.list_models () in
  let* () = Lwt_io.printl "" in
  match res with
  | Error (_, s) ->
    let* () = Lwt_io.printl "Bad!" in
    Lwt_io.printl s
  | Ok _ ->
    Lwt_io.printl "Ok!"

let () =
  Lwt_main.run main
