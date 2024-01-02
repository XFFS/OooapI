(* Cohttp's Http types *)
(* open Http *)

type todo = TODO

type 'a locator =
  { name : string
  ; pp : Format.formatter -> 'a -> unit
  }

module Resource : sig
  module Locator : sig
    module Param : sig
      type 'a t = 'a locator

      val pp : 'a t -> Format.formatter -> 'a -> unit
      val to_string : 'a t -> 'a -> string
      val str : string -> string t
      val int : string -> int t
      val float : string -> float t
      val bool : string -> bool t
    end

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

    module Args : sig
      type _ t = private
        | Nil : unit t
        | Cons : 'a * 'args t -> ('a * 'args) t

      val nil : unit t
      val cons : 'a -> 'args t -> ('a * 'args) t
      val ( @/ ) : 'a -> 'args t -> ('a * 'args) t
    end

    type 'params t =
      { base : Uri.t
      ; path : 'params Path.t
      }

    val v : string -> 'params Path.t -> 'params t
    val to_uri : 'params Args.t -> 'params t -> Uri.t
  end
end = struct
  module Locator = struct
    module Param = struct
      type 'a t = 'a locator

      let pp t fmt x = t.pp fmt x
      let to_string t x : string = Format.asprintf "%a%!" (pp t) x
      let str name = { name; pp = Format.pp_print_string }
      let int name = { name; pp = Format.pp_print_int }
      let float name = { name; pp = Format.pp_print_float }
      let bool name = { name; pp = Format.pp_print_bool }
    end

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

    module Args = struct
      type _ t =
        | Nil : unit t
        | Cons : 'x * 'rest t -> ('x * 'rest) t

      let nil = Nil

      let cons (type a args) (x : a) (args : args t) : (a * args) t =
        Cons (x, args)

      let ( @/ ) = cons
    end

    type 'params t =
      { base : Uri.t
      ; path : 'params Path.t
      }

    let v base path = { base = Uri.of_string base; path }

    let to_uri : type params. params Args.t -> params t -> Uri.t =
     fun args t ->
      let rec pp_path :
          type params. Format.formatter -> params Path.t * params Args.t -> unit
          =
       fun fmt -> function
        | Path.Nil, Args.Nil -> ()
        | Path.Const (s, Nil), _ -> Format.pp_print_string fmt s
        | Path.Const (s, path), args ->
            Format.fprintf fmt "%s/" s;
            pp_path fmt (path, args)
        | Path.Param ((p : _ Param.t), Nil), Args.Cons (v, Nil) ->
            Param.pp p fmt v
        | Path.Param ((p : _ Param.t), path), Args.Cons (v, args) ->
            Format.fprintf fmt "%a/" (Param.pp p) v;
            pp_path fmt (path, args)
      in
      Uri.of_string
      @@ Format.asprintf "%a/%a%!" Uri.pp t.base pp_path (t.path, args)
  end
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
