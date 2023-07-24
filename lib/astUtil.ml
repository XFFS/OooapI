open Ppxlib
(** Utilities to ease the pain of ast construction a bit *)

module OcamlBuiltins = struct
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

  let typ ptyp_desc : core_type =
    { ptyp_desc; ptyp_loc = loc; ptyp_loc_stack = []; ptyp_attributes = [] }

  let typ_constr ?(args = []) name =
    Ptyp_constr (n (Astlib.Longident.parse name), args)

  let typ_decl
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

  let attr ~name = Ast.attribute ~name:(n name) ~payload:(PStr [])

  let const_str s : expression =
    Ast.pexp_constant (Pconst_string (s, loc, None))

  let attr_str ~name str =
    Ast.attribute
      ~name:(n name)
      ~payload:(PStr [ Ast.pstr_eval (const_str str) [] ])

  let attr_strs ~name strs =
    Ast.attribute
      ~name:(n name)
      ~payload:
        (PStr [ Ast.pstr_eval (Ast.pexp_tuple (List.map const_str strs)) [] ])

  let attr_ident ~name ident =
    let ident = Astlib.Longident.parse ident in
    Ast.attribute
      ~name:(n name)
      ~payload:(PStr [ Ast.pstr_eval (Ast.pexp_ident (n ident)) [] ])

  (* A variable pattern *)
  module Pat = struct
    let var v = Ast.ppat_var (n v)
  end

  module Exp = struct
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
end
