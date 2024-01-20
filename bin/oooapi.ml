open Lib

let () =
  let spec_file = ref "" in
  Arg.parse [] (fun f -> spec_file := f) "oooapi <spec_file>";
  let spec =
    match !spec_file with
    | "" -> failwith "argument spec_file must be provided" (* TODO *)
    | f ->
    match Openapi_spec.from_file f with
    | Ok s -> s
    | Error (`Msg m) ->
        Printf.eprintf "error: could not parse spec %s\n%s" f m;
        exit 1
    | exception Json_schema.Cannot_parse(path,exn) ->
      let err = match exn with
        | Json_schema.Unexpected(expected, actual) ->
          Format.sprintf "expected value of type %s, but got value of type %s" expected actual
        | exn -> (Printexc.to_string exn)
      in
      Format.eprintf "Error when parsing json schema at path %a\n%s\n%!"
        (Json_query.print_path_as_json_path ~wildcards:false) path
        err;
      exit 1
  in
  spec |> Oooapi.module_of_spec |> Oooapi.write_ast
