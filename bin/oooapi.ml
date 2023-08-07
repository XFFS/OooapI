open Lib

let () =
  let spec_file = ref "" in
  Arg.parse [] (fun f -> spec_file := f) "oooapi <spec_file>";
  let spec =
    match !spec_file with
    | "" -> failwith "argument spec_file must be provided" (* TODO *)
    | f ->
    match Openapi_spec.from_file f with
    | Error (`Msg m) ->
        Printf.eprintf "error: could not parse spec %s\n%s" f m;
        exit 1
    | Ok s -> s
  in
  spec |> Oooapi.module_of_spec |> Oooapi.write_ast
