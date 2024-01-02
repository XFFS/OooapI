(* Cohttp's Http types *)
(* open Http *)

type todo = TODO

type 'a param =
  { name : string
  ; pp : Format.formatter -> 'a -> unit
  }

module Hlist (T : sig
  type 'a t
end) =
struct
  type _ t =
    | Nil : unit t
    | Cons : 'p T.t * 'params t -> ('p * 'params) t

  let nil = Nil

  let cons : type p params. p T.t -> params t -> (p * params) t =
   fun p ps -> Cons (p, ps)

  let ( @/ ) = cons
end

module Params = Hlist (struct
  type 'a t = 'a param
end)

module Args = Hlist (struct
  type 'a t = 'a
end)

module Resource : sig
  module Param : sig
    type 'a t = 'a param

    val pp : 'a t -> Format.formatter -> 'a -> unit
    val to_string : 'a t -> 'a -> string
    val str : string -> string t
    val int : string -> int t
    val float : string -> float t
    val bool : string -> bool t
  end

  module Locator : sig
    module Path : sig
      type _ t = private
        | Nil : unit t
        | Const : string * 'params t -> 'params t
        | Param : 'p Param.t * 'params t -> ('p * 'params) t

      val nil : unit t
      val cons : string -> 'params t -> 'params t
      val ( @/ ) : string -> 'params t -> 'params t
      val param : 'p Param.t -> 'params t -> ('p * 'params) t
      val ( @? ) : 'p Param.t -> 'params t -> ('p * 'params) t
      val pp : Format.formatter -> 'params t -> unit
      val to_string : 'params t -> string
    end

    type ('path, 'query) t =
      { base : Uri.t
      ; path : 'path Path.t
      ; query : 'query Params.t
      }

    val v :
         path:'params Path.t
      -> query:'query Params.t
      -> string
      -> ('params, 'query) t

    val to_uri :
      ?path:'path Args.t -> ?query:'query Args.t -> ('path, 'query) t -> Uri.t
  end
  (* Locator *)

  (* TODO We need params for representation *)
  (* module Representation : sig *)
  (* module Content = struct *)
  (*   type kind = `Json | Multipart_ *)
  (*   type 'a t = *)
  (*     { data : 'a option *)
  (*     ; pp : Format.formatter -> 'a -> unit *)
  (*     ; meta :  *)
  (*     } *)
  (* end *)

  type ('path, 'query, 'header) args =
    { path : 'path Args.t
    ; query : 'query Args.t
    ; header : 'header Args.t
    }

  type ('path, 'query, 'header) t =
    { meth : Http.Method.t
    ; locator : ('path, 'query) Locator.t
    ; header : todo
    ; content : todo
    }
end = struct
  module Param = struct
    type 'a t = 'a param

    let pp t fmt x = t.pp fmt x
    let to_string t x : string = Format.asprintf "%a%!" (pp t) x
    let str name = { name; pp = Format.pp_print_string }
    let int name = { name; pp = Format.pp_print_int }
    let float name = { name; pp = Format.pp_print_float }
    let bool name = { name; pp = Format.pp_print_bool }
  end

  module Locator = struct
    module Path = struct
      type _ t =
        | Nil : unit t
        | Const : string * 'params t -> 'params t
        | Param : 'p Param.t * 'params t -> ('p * 'params) t

      let nil = Nil

      let cons : type params. string -> params t -> params t =
       fun s path -> Const (s, path)

      let ( @/ ) s path = cons s path

      let param : type p params. p Param.t -> params t -> (p * params) t =
       fun p path -> Param (p, path)

      let ( @? ) p path = param p path

      let pp : type a. Format.formatter -> a t -> unit =
       fun fmt path ->
        let rec pp_path : type a. Format.formatter -> a t -> unit =
         fun fmt -> function
          | Nil -> ()
          | Const (s, Nil) -> Format.pp_print_string fmt s
          | Const (s, path) ->
              Format.fprintf fmt "%s/" s;
              pp_path fmt path
          | Param (p, Nil) -> Format.fprintf fmt "{%s}" p.name
          | Param (p, path) ->
              Format.fprintf fmt "{%s}/" p.name;
              pp_path fmt path
        in
        pp_path fmt path

      let to_string t : string = Format.asprintf "%a%!" pp t
    end

    type ('path, 'query) t =
      { base : Uri.t
      ; path : 'path Path.t
      ; query : 'query Params.t
      }

    let v :
        type query path.
        path:path Path.t -> query:query Params.t -> string -> (path, query) t =
     fun ~path ~query base -> { base = Uri.of_string base; path; query }

    let to_uri :
        type path query.
        ?path:path Args.t -> ?query:query Args.t -> (path, query) t -> Uri.t =
     fun ?path ?query t ->
      let rec pp_path :
          type path.
          Format.formatter -> path Path.t * path Args.t option -> unit =
       fun fmt -> function
        | Path.Nil, None -> ()
        | Path.Nil, Some Args.Nil -> ()
        | Path.Const (s, Nil), _ -> Format.pp_print_string fmt s
        | Path.Const (s, path), args ->
            Format.fprintf fmt "%s/" s;
            pp_path fmt (path, args)
        | Path.Param ((p : _ Param.t), Nil), Some (Args.Cons (v, Nil)) ->
            Param.pp p fmt v
        | Path.Param ((p : _ Param.t), path), Some (Args.Cons (v, args)) ->
            Format.fprintf fmt "%a/" (Param.pp p) v;
            pp_path fmt (path, Some args)
        | _, None ->
            raise
              (Invalid_argument
                 "In Locator.to_uri ~path can only be None if t.path is Nil")
      in
      let params :
          type query.
          query Params.t * query Args.t option -> (string * string) list =
        function
        | Params.Nil, None -> []
        | params, Some args ->
            let rec aux :
                type q. q Params.t * q Args.t -> (string * string) list =
              function
              | Params.Nil, Args.Nil -> []
              | Params.Cons (p, params), Args.Cons (a, args) ->
                  (p.name, Param.to_string p a) :: aux (params, args)
            in
            aux (params, args)
        | _, None ->
            raise
              (Invalid_argument
                 "In Locator.to_uri ~query can only be None if t.query is Nil")
      in
      let uri =
        Uri.of_string
        @@ Format.asprintf "%a/%a%!" Uri.pp t.base pp_path (t.path, path)
      in
      Uri.add_query_params' uri (params (t.query, query))
  end

  type ('path, 'query, 'header) args =
    { path : 'path Args.t
    ; query : 'query Args.t
    ; header : 'header Args.t
    }

  type ('path, 'query, 'header) t =
    { meth : Http.Method.t
    ; locator : ('path, 'query) Locator.t
    ; header : todo
    ; content : todo
    }
end

(* module S = struct *)
(*   module type Resource = sig *)
(*     module Locator : sig *)
(*       include module type of T.Resource.Locator *)
(*     end *)

(*     type t = { location : Uri.t } *)
(*     type representation *)
(*   end *)

(*   module Message (R : Resource) = struct *)
(*     module type Request = sig *)
(*       type t = *)
(*         { resource : R.t *)
(*         ; meth : Method.t *)
(*         } *)
(*     end *)

(*     module type Response = sig *)
(*       type t *)
(*     end *)
(*   end *)
(* end *)
