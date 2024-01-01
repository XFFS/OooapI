(** Adapt values that are either a reference or some other type *)
module Of_ref = struct
  let normalize : Yojson.Safe.t -> Yojson.Safe.t =
   fun json ->
    match json with
    | `Assoc fields
      when List.exists
             (function
               | "$ref", _ -> true
               | _ -> false)
             fields ->
        `List [ `String "Ref"; json ]
    | other -> `List [ `String "Obj"; other ]

  let restore : Yojson.Safe.t -> Yojson.Safe.t =
   fun json ->
    match json with
    | `List [ `String ("Ref" | "Obj"); restored ] -> restored
    | other -> other (* Should be impossible *)
end
