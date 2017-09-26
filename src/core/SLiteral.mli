

(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Literal} *)

(** Simple literals, used typically in {!CNF} before being converted to
    whatever representation the prover prefers. *)

type form = TypedSTerm.t
type term = TypedSTerm.t

exception NotALit of form

type +'t t =
  | True
  | False
  | Atom of 't * bool
  | Eq of 't * 't
  | Neq of 't * 't

type 'a lit = 'a t

val of_form : form -> term t (** @raise NotALit if the form is not a literal *)
val to_form : term t -> form

val map : f:('a -> 'b) -> 'a t -> 'b t
val fold : ('a -> 't -> 'a) -> 'a -> 't t -> 'a
val iter : f:('a -> unit) -> 'a t -> unit
val to_seq : 'a t -> 'a Sequence.t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val true_ : _ t
val false_ : _ t
val eq : 'a -> 'a -> 'a t
val neq : 'a -> 'a -> 'a t
val atom : 'a -> bool -> 'a t
val atom_true : 'a -> 'a t
val atom_false : 'a -> 'a t

val is_true : _ t -> bool
val is_false : _ t -> bool

val sign : _ t -> bool
val is_pos : _ t -> bool
val is_neg : _ t -> bool

val negate : 'a t -> 'a t
(** negation of literal *)

include Interfaces.PRINT1 with type 'a t := 'a t

module TPTP : sig
  include Interfaces.PRINT1 with type 'a t := 'a t
end

module ZF : sig
  include Interfaces.PRINT1 with type 'a t := 'a t
end

val pp_in : Output_format.t -> 'a CCFormat.printer -> 'a t CCFormat.printer
