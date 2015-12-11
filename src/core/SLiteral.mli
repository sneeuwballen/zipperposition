

(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Literal}

  Used for reduction to CNF, this is a basic representation of literals *)

type form = TypedSTerm.t
type term = TypedSTerm.t

exception NotALit of form

type 't t =
  | True
  | False
  | Atom of 't * bool
  | Eq of 't * 't
  | Neq of 't * 't

type 'a lit = 'a t

val of_form : form -> term t (** @raise NotALit *)
val to_form : term t -> form

val map : f:('a -> 'b) -> 'a t -> 'b t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val is_true : _ t -> bool
val is_false : _ t -> bool

include Interfaces.PRINT1 with type 'a t := 'a t
