val type_of_element : qualifier:string -> Json_schema.element -> (Ppxlib.core_type * Ppxlib.type_declaration list)

(** Takes a schema to the principle type declaration representing the schema and
    a list of  auxiliary declarations. *)
val type_declarations : Json_schema.schema -> (Ppxlib.type_declaration * Ppxlib.type_declaration list)
