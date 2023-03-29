include Openapi_t


let of_json s =
  match Openapi_j.t_of_string s with
  | spec -> Ok spec
  | exception Yojson.Json_error err -> Error (`Msg err)

let to_json t = Openapi_j.string_of_t t


(* let ( let* ) = Result.bind *)

(* let of_yaml s = *)
(*   let* yaml = Yaml.yaml_of_string s in *)
(*   let* json = Yaml.to_json yaml in *)
(*   json |> Ezjsonm.wrap |> Ezjsonm.to_string |> of_json *)

(* let to_yaml t = to_json t |> Yaml.of_string_exn |> Yaml.to_string_exn *)

let from_file file =
  let decoder =
    if file |> String.ends_with ~suffix:".json" then
      of_json
    else
      failwith "Only JSON supported"
      (* of_yaml *)
  in
  In_channel.(with_open_text file input_all)
  |> decoder

(** TODO: Figure out conversion from/to YAML *)

module Openapi_path = Openapi_path
