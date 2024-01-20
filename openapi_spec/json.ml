(* Wrapper for Yojson, required for atgen -v, as per https://github.com/ocaml-community/yojson/pull/136  *)

include Yojson.Safe

let validate_t _ _ = None
