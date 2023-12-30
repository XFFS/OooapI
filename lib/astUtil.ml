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

(** An AST node for a name v *)
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

    (** A type constructor

        E.g., [constr "Foo" [int, str]] makes the type constructor [(int, str) Foo] *)
    let constr ?(args = []) name =
      Ptyp_constr (n (Astlib.Longident.parse name), args)

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

    (** A function *)
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
