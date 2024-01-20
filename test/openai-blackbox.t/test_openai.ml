module Config : Oooapi_lib.Config = struct
  let default_headers = None
  let bearer_token = Sys.getenv_opt "OPENAI_API_KEY"
end

module Data = Openai_api.Data
module Api = Openai_api.Make (Oooapi_lib.Cohttp_client) (Config)

let main =
  let open Lwt.Syntax in
  let* () =
    let* res = Api.list_models () in
    let* () = Lwt_io.printl "" in
    match res with
    | Ok data ->
        let* () = Lwt_io.printl "list_models: OK" in
        Lwt_list.iter_s
          (fun s -> Yojson.Safe.to_string s |> Lwt_io.printl)
          data.data
    | Error (`Request (_, s)) ->
        let* () = Lwt_io.printl "list_models: request failed" in
        Lwt_io.printl s
    | Error (`Deseriaization (resp, err)) ->
        let* () = Lwt_io.printl "list_models: deser failed" in
        let* () = Lwt_io.printl err in
        Lwt_io.printl resp
  in
  let* () =
    let* res =
      let data =
        Data.CreateCompletionRequest.(
          make ~model:"davinci-002" ~prompt:"You complete me" ())
      in
      Api.create_completion data
    in
    match res with
    | Ok data ->
        let* () = Lwt_io.printl "create_completion: OK" in
        data.choices
        |> Lwt_list.iter_s (fun c ->
               c.Data.CreateCompletionResponse.choices_item_text
               |> Lwt_io.printl)
    | Error (`Request (_, s)) ->
        let* () = Lwt_io.printl "create_completion: request failed" in
        Lwt_io.printl s
    | Error (`Deseriaization (resp, err)) ->
        let* () = Lwt_io.printl "create_completion: deser failed" in
        let* () = Lwt_io.printl err in
        Lwt_io.printl resp
  in
  Lwt.return_unit

let () = Lwt_main.run main
