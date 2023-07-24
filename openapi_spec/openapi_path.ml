type segment =
  [ `C of string  (** Concrete segment *)
  | `P of string  (** Parameter segment *)
  ]

let segment_to_string = function
  | `C s -> s
  | `P s -> "{" ^ s ^ "}"

let segment_of_string s =
  if String.starts_with ~prefix:"{" s then
    `P String.(sub s 1 (length s - 2))
  else
    `C s

type t = segment list

let to_string p =
  (* Component paths must always begin with a leading `/` *)
  "/" ^ (p |> List.map segment_to_string |> String.concat "/")

let of_string s =
  s
  |> String.split_on_char '/'
  |> (function
       (* Leading `/` means we get an empty string as the first element, which we don't want as  part *)
       | "" :: parts -> parts
       | parts -> parts)
  |> List.map segment_of_string

let unwrap = to_string
let wrap = of_string

let params t =
  List.filter_map
    (function
      | `P p -> Some p
      | _ -> None)
    t
