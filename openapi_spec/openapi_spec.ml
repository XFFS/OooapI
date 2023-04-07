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
      failwith "Only JSON supported"
  in
  In_channel.(with_open_text file input_all)
  |> decoder

(** TODO: Figure out conversion from/to YAML? *)

module Openapi_path = Openapi_path
