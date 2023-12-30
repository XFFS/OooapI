(** A converter that lets ATD delegate ser/de of JSON schema stuff to the
    vendored [json-data-encoding] library. *)

type t = Json_schema.schema

module Schema = Json_schema.Make (Json_repr.Yojson)

let wrap json = Schema.of_json ~validate_refs:false json
let unwrap = Schema.to_json
