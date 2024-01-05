(** A converter that lets ATD delegate ser/de of JSON schema stuff to the
    vendored [json-data-encoding] library. *)

type t =
  { name : string option
        (** Used to support dereferencing without losing the component's ID.
            Not part of the OpenAPI spec. *)
  ; schema : Json_schema.schema
  }

module Schema = Json_schema.Make (Json_repr.Yojson)

let wrap json =
  { name = None; schema = Schema.of_json ~validate_refs:false json }

let unwrap s = Schema.to_json s.schema
