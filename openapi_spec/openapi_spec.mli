include module type of Openapi_t with type todo := Yojson.Safe.t

val of_json : string -> (t, [`Msg of string]) Result.t
val to_json : t -> string
(* val of_yaml : string -> (t, [`Msg of string]) Result.t *)
(* val to_yaml : t -> string *)
val from_file : string -> (t, [`Msg of string]) Result.t

module Openapi_path : sig
  type t
  val params : t -> string list
  val of_string : string -> t
  val to_string : t -> string
end
