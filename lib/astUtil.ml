(** Utilities to ease the pain of ast construction a bit *)

open Ppxlib

module OcamlBuiltins : sig
  val is_keyword : string -> bool

  val sanitize : string -> string
  (** [sanitize s] is [s ^ "_"] if [is_keyword s]. Otherwise, it is [s]. *)
end = struct
  module StrSet = Set.Make (String)

  let keywords =
    StrSet.of_list
      [ "and"
      ; "as"
      ; "assert"
      ; "asr"
      ; "begin"
      ; "class"
      ; "constraint"
      ; "do"
      ; "done"
      ; "downto"
      ; "else"
      ; "end"
      ; "exception"
      ; "external"
      ; "false"
      ; "for"
      ; "fun"
      ; "function"
      ; "functor"
      ; "if"
      ; "in"
      ; "include"
      ; "inherit"
      ; "initializer"
      ; "land"
      ; "lazy"
      ; "let"
      ; "lor"
      ; "lsl"
      ; "lsr"
      ; "lxor"
      ; "match"
      ; "method"
      ; "mod"
      ; "module"
      ; "mutable"
      ; "new"
      ; "nonrec"
      ; "object"
      ; "of"
      ; "open"
      ; "or"
      ; "private"
      ; "rec"
      ; "sig"
      ; "struct"
      ; "then"
      ; "to"
      ; "true"
      ; "try"
      ; "type"
      ; "val"
      ; "virtual"
      ; "when"
      ; "while"
      ; "with"
      ]

  let is_keyword s = StrSet.mem s keywords

  let sanitize s =
    if is_keyword s then
      s ^ "_"
    else
      s
end

let loc = Location.in_file "ooo_spec_gen.ml"

module Ast = Ast_builder.Make (struct
  let loc = loc
end)

(** An AST node for a located v *)
let n v = Ast.Located.mk v

module AstExt = struct
  (* Extensions to ppixlib's `Ast` module *)

  let to_module_name s =
    let camel_case = Camelsnakekebab.upper_camel_case s in
    match String.get s 0 with
    | 'A' .. 'Z' -> camel_case
    | _ -> "O" ^ s

  let to_identifier s =
    s
    |> String.map (function
           | '/' -> '-'
           | c -> c)
    |> Camelsnakekebab.lower_snake_case
    |> OcamlBuiltins.sanitize

  module Type = struct
    (** Create a core type from a type description *)
    let v ptyp_desc : core_type =
      { ptyp_desc; ptyp_loc = loc; ptyp_loc_stack = []; ptyp_attributes = [] }

    (** [t n args] is the type derived by applying the constructor [n] to [args], e.g., [t list [int_t]] is [int list] *)
    let t name args = Ast.ptyp_constr (Ast.Located.lident name) args

    (** A type constructor

        E.g., [constr "Foo" [int, str]] makes the type constructor [(int, str) Foo] *)
    let constr ?(args = []) name =
      try
        Ptyp_constr (n (Astlib.Longident.parse name), args)
      with Syntaxerr.Error _ ->
        Format.sprintf "Attempted to build type of invalid name '%s'" name |> failwith


    (** A type declaration *)
    let decl
        ?kind
        ?manifest
        ?(params = [])
        ?(cstrs = [])
        ?(private_ = false)
        ?(attributes = [])
        name : type_declaration =
      let private_ =
        if private_ then
          Private
        else
          Public
      in
      let ptype_kind = Option.value kind ~default:Ptype_abstract in
      { ptype_kind
      ; ptype_name = n name
      ; ptype_params = params
      ; ptype_cstrs = cstrs
      ; ptype_private = private_
      ; ptype_manifest = manifest
      ; ptype_attributes = attributes
      ; ptype_loc = loc
      }
  end

  module Pat = struct
    (** Patterns *)

    (** A variable pattern *)
    let var v = Ast.ppat_var (n v)
  end

  module Exp = struct
    (** Expressions *)

    (** A string constant *)
    let const_str s : expression =
      Ast.pexp_constant (Pconst_string (s, loc, None))

    (** Access a record field *)
    let field_access : expression -> string -> expression =
      fun record field_name -> Ast.pexp_field record (n (Longident.parse field_name))

    (** A function

        - If [label] is provided, then argument to the function is labeled
        - If [optional] is [true], then the label is optional
        - If [default] is provided, it is the default value, and this requires that [optional] also be true
        - [pat] is the pattern of the function parameter
        - [exp] is the body of the function *)
    let f ?label ?(optional = false) ?default pat exp =
      let label =
        Option.(
          label
          |> map (fun l ->
                 if optional then
                   Optional l
                 else
                   Labelled l)
          |> value ~default:Nolabel)
      in
      Ast.pexp_fun label default pat exp

    (*** Let bindings *)
    let lets ?(rec_ = false) bindings expr =
      let rec_flag =
        if rec_ then
          Recursive
        else
          Nonrecursive
      in
      let bindings =
        bindings |> List.map (fun (pat, expr) -> Ast.value_binding ~pat ~expr)
      in
      match bindings with
      | [] -> expr
      | bindings -> Ast.pexp_let rec_flag bindings expr
  end

  let attr ~name = Ast.attribute ~name:(n name) ~payload:(PStr [])

  let attr_str ~name str =
    Ast.attribute
      ~name:(n name)
      ~payload:(PStr [ Ast.pstr_eval (Exp.const_str str) [] ])

  let attr_strs ~name strs =
    Ast.attribute
      ~name:(n name)
      ~payload:
        (PStr
           [ Ast.pstr_eval (Ast.pexp_tuple (List.map Exp.const_str strs)) [] ])

  let attr_ident ~name ident =
    let ident = Astlib.Longident.parse ident in
    Ast.attribute
      ~name:(n name)
      ~payload:(PStr [ Ast.pstr_eval (Ast.pexp_ident (n ident)) [] ])
end

module Mod = struct
  (* Module structures and types *)

  let module_type_const name : module_type =
    Ast.pmty_ident (n (Astlib.Longident.parse name))

  let named_functor_param name mod_type : functor_parameter =
    Named (n (Some name), mod_type)

  let binding name structure =
    Ast.pstr_module @@ Ast.module_binding ~name:(n (Some name)) ~expr:structure
end
