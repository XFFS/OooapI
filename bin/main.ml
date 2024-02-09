open Lib

let ok_or_handle_error result =
  match result with
  | Ok spec ->
    spec
  | Error (`Msg m) ->
    Printf.eprintf "error: could not parse spec\n%s" m;
    exit 1
  | exception Json_schema.Cannot_parse(path, exn) ->
    let err = match exn with
      | Json_schema.Unexpected(expected, actual) ->
        Format.sprintf "expected value of type %s, but got value of type %s" expected actual
      | exn -> (Printexc.to_string exn)
    in
    Format.eprintf "Error when parsing json schema at path %a\n%s\n%!"
      (Json_query.print_path_as_json_path ~wildcards:false) path
      err;
    exit 1

let () =
  let spec_file = ref "" in
  Arg.parse [] (fun f -> spec_file := f) "oooapi [spec_file]";
  !spec_file
  |> (function
      | ""   -> Openapi_spec.from_in_channel In_channel.stdin
      | file -> Openapi_spec.from_file file)
  |> ok_or_handle_error
  |> Oooapi.module_of_spec
  |> Oooapi.write_ast
