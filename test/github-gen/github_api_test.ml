module Ooo = Oooapi_lib
module Config : Ooo.Config = struct
  let bearer_token = None
  let default_headers = None
end

module Api = Github_api.Make (Ooo.Cohttp_client) (Config)

let () =
  begin
    let open Lwt_result.Syntax in
    let+ resp = Api.repos_get_readme ~owner:"shonfeder" ~repo:"nomad" () in
    resp.name
  end
  |> Lwt_main.run
  |> function
  | Ok file_name -> assert (file_name = "README.org")
  | Error (`Deserialization (data, err)) ->
    Printf.eprintf "Deserialization error at %s\n\non data\n\n%s\n%!" err  data;
    exit 1
  | Error (`Request (_code, err)) ->
    Printf.eprintf "Request error %s\n%!" err;
    exit 1
