module Config : Openai_api.Config = struct
  let default_headers = None
  let bearer_token = Sys.getenv_opt "OPENAI_API_KEY"
end

module Data = Openai_api.Data
module Api = Openai_api.Endpoint (Config)

let main =
  let open Lwt.Syntax in
  let* () =
    let* res = Api.list_models () in
    let* () = Lwt_io.printl "" in
    match res with
    | Ok data ->
        let* () = Lwt_io.printl "list_models: Ok!" in
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
          make
            ~model:"text-davinci-003"
            ~prompt:(Some (`String "You complete me"))
            ())
      in
      Api.create_completion data
    in
    match res with
    | Ok data ->
        data.choices
        |> Lwt_list.iter_s (fun c ->
               match c.Data.CreateCompletionResponse.choices_item_text with
               | None -> Lwt.return_unit
               | Some t -> Lwt_io.printl t)
    | Error (`Request (_, s)) ->
        let* () = Lwt_io.printl "list_models: request failed" in
        Lwt_io.printl s
    | Error (`Deseriaization (resp, err)) ->
        let* () = Lwt_io.printl "list_models: deser failed" in
        let* () = Lwt_io.printl err in
        Lwt_io.printl resp
  in
  Lwt.return_unit

let () = Lwt_main.run main
