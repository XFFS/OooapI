include Openapi_t

let of_json s =
  match Openapi_j.t_of_string s with
  | spec -> Ok spec
  | exception Yojson.Json_error err -> Error (`Msg err)

let to_json t = Openapi_j.string_of_t t

let from_file file =
  let decoder =
    if file |> String.ends_with ~suffix:".json" then
      of_json
    else
      (* TODO Support YAML input (requires a YAML parser that supports anchors etc.) *)
      raise (Invalid_argument "Only JSON is currently supported")
  in
  if not (Sys.file_exists file) then
    Error (`Msg ("File " ^ file ^ " does not exist or cannot be read"))
  else
    In_channel.(input_all |> with_open_text file |> decoder)

(** TODO: Figure out conversion from/to YAML? *)

module Openapi_path = Openapi_path
