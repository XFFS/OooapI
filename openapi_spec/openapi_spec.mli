include module type of Openapi_t with type todo := Yojson.Safe.t
include module type of Openapi_v

val of_json : string -> (t, [ `Msg of string ]) Result.t
val to_json : t -> string

(* val of_yaml : string -> (t, [`Msg of string]) Result.t *)
(* val to_yaml : t -> string *)
val from_file : string -> (t, [ `Msg of string ]) Result.t

exception Unsupported_reference of string * string
exception Invalid_reference of string

val resolve_refs : t -> t
(** [resolve_refs spec] is the [spec] with all references resolved so that the
    referenced component spec replaces the reference  *)

module Openapi_path : sig
  type t = path

  val params : t -> string list
  val of_string : string -> t
  val to_string : t -> string
end
