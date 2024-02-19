val type_of_element : qualifier:string -> media:Http_spec.Media_kind.t -> Json_schema.element -> (Ppxlib.core_type * Ppxlib.type_declaration list)

(** Takes a schema to the principle type declaration representing the schema and
    a list of  auxiliary declarations. *)
val type_declarations : media:Http_spec.Media_kind.t -> Json_schema.schema -> (Ppxlib.type_declaration * Ppxlib.type_declaration list)
