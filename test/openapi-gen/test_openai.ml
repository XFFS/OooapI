module Config : Openai_api.Config = struct
  let default_headers = None
  let bearer_token = Sys.getenv_opt "OPENAI_API_KEY"
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
  | Ok _ -> Lwt_io.printl "Ok!"

let () = Lwt_main.run main
