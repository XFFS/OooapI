(* TODO: Proper tests *)

let can_parse_paths () =
  let open Openapi_spec in
  let no_params = Openapi_path.of_string "foo/bar/baz" in
  assert (Openapi_path.params no_params = []);
  let with_params = Openapi_path.of_string "foo/{bar}/baz/{bing}/bong" in
  assert (Openapi_path.params with_params = ["bar"; "bing"])

let can_parse_openapi_spec () =
  let json = In_channel.(with_open_text "openapi-openai.json" input_all) in
  match Openapi_spec.of_json json with
  | Ok _ -> ()
  | Error (`Msg err) -> Printf.sprintf "parse failure: %s" err |> failwith

let () =
  can_parse_paths ();
  can_parse_openapi_spec ()
