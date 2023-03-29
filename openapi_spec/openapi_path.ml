type segment =
  | C of string (** Concrete segment *)
  | P of string (** Parameter segment *)

let segment_to_string = function
  | C s -> s
  | P s -> "{" ^ s ^ "}"

let segment_of_string s =
  if String.starts_with ~prefix:"{" s then
    P String.(sub s 1 (length s - 2))
  else
    C s

type t = segment list

let to_string p = p |> List.map segment_to_string |> String.concat "/"
let of_string s = s |> String.split_on_char '/' |> List.map segment_of_string

let unwrap = to_string
let wrap = of_string

let params p = List.filter_map (function | P p -> Some p | _ -> None) p
