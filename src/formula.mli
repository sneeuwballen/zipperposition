
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 First-order Formulas} *)

(** Hashconsed formulas for first-order logic. This provides many useful
    functions, and smart constructors that perform some basic
    simplifications *)

(* TODO: attributes, to speed up
    simplification/flattening/groundness/closenes checking *)

type t = private {
  form : cell;
  mutable flags : int;
  mutable id : int;
}
and cell = private
  | True
  | False
  | Atom of Term.t
  | And of t list
  | Or of t list
  | Not of t
  | Imply of t * t
  | Equiv of t * t
  | Equal of Term.t * Term.t
  | Forall of t    (** Quantified formula, with De Bruijn *)
  | Exists of t

type sourced_form = t * string * string    (* form, filename, axiom name *)
type form = t

val eq : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val hash_novar : t -> int

val mk_true : t
val mk_false : t
val mk_atom : Term.t -> t
val mk_not : t -> t
val mk_and : t list -> t
val mk_or : t list -> t
val mk_imply : t -> t -> t
val mk_equiv : t -> t -> t
val mk_xor : t -> t -> t
val mk_eq : Term.t -> Term.t -> t
val mk_neq : Term.t -> Term.t -> t
val mk_forall : t -> t
val mk_exists : t -> t

(** {2 Flags} *)

val flag_simplified : int
val flag_ground : int

val set_flag : t -> int -> unit
val has_flag : t -> int -> bool
val new_flag : unit -> int

(** {2 Combinators} *)

val map_leaf : (t -> t) -> t -> t
  (** Call the function on leaves (atom,equal,true,false) and replace the
      leaves by their image. The image formulas should {b not} contain
      free De Bruijn indexes (ie, should verify {! db_closed}). *)

val map : (Term.t -> Term.t) -> t -> t    (** Map on terms *)
val fold : ('a -> Term.t -> 'a) -> 'a -> t -> 'a  (** Fold on terms *)
val iter : (Term.t -> unit) -> t -> unit

val map_depth: ?depth:int ->
                (int -> Term.t -> Term.t) ->
                t -> t
  (** Map each term to another term. The number of binders from the root
      of the formula to the term is given to the function. *)

val map_leaf_depth : ?depth:int ->
                      (int -> t -> t) ->
                      t -> t

val fold_depth : ?depth:int ->
              ('a -> int -> Term.t -> 'a) ->
              'a -> t -> 'a
  (** Fold on terms, but with an additional argument, the number of
      De Bruijn indexes bound on through the path to the term *)

val weight : t -> int
  (** Number of 'nodes' of the formula, including the terms *)

(** The following functions gather the terms of a formula. *)

val add_terms : Term.THashSet.t -> t -> unit
val terms : t -> Term.THashSet.t

val terms_seq : t -> Term.t Sequence.t
  (** Sequence of terms. Terms may occur several times *)

val subterm : Term.t -> t -> bool
  (** [subterm t f] true iff [t] occurs in some term of [f] *)

val free_variables : t -> Term.varlist
  (** Variables not bound by any (formula) quantifier *)

val var_occurs : Term.t -> t -> bool

val is_atomic : t -> bool   (** No connectives? *)
val is_ground : t -> bool   (** No variables? *)
val is_closed : t -> bool   (** All variables bound? *)

val contains_symbol : Symbol.t -> t -> bool

(** {2 De Bruijn indexes} *)

val db_closed : t -> bool
  (** All De Bruijn indexes bound? *)

val db_contains : t -> int -> bool
  (** Does the formula contain the De Bruijn variable of index n? *)

val db_replace : t -> Term.t -> t
  (** Replace De Bruijn index 0 by the given term *)

val db_type : t -> int -> Type.t option
  (** Type of the n-th DB variable *)

val db_lift : t -> t

val db_unlift : ?depth:int -> t -> t

val db_from_term : ?ty:Type.t -> t -> Term.t -> t
  (** Replace the given term by a De Bruijn index *)

val db_from_var : t -> Term.t -> t
  (** Replace the given variable by a De Bruijn index *)

val mk_forall_list : Term.t list -> t -> t
val mk_exists_list : Term.t list -> t -> t

val close_forall : t -> t   (** Bind all free variables with forall *)
val close_exists : t -> t   (** Bind all free variables with exists *)

val open_forall : ?offset:int -> t -> t
  (** Remove outer forall binders, using fresh vars instead of DB symbols *)

val open_and : t -> t list
  (** If the formula is an outer conjunction, return the list of elements of
      the conjunction *)

val open_or : t -> t list

(** {2 Simplifications} *)

val flatten : t -> t        (** Flatten AC connectives (or/and) *)
val simplify : t -> t       (** Simplify the formula *)

val is_trivial : t -> bool  (** Trivially true formula? *)

val ac_normal_form : t -> t (** Normal form modulo AC of "or" and "and" *)
val ac_eq : t -> t -> bool  (** Equal modulo AC? *)

(** {2 Conversions} *)

val to_term : t -> Term.t   (** Conversion to term *)
val of_term : Term.t -> t

(** {2 Typing} *)

val infer_type : TypeInference.Ctx.t -> t -> unit
val signature : ?signature:Signature.t -> t -> Signature.t
val signature_seq : ?signature:Signature.t -> t Sequence.t -> Signature.t

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string

val bij : t Bij.t

(** {2 Containers} *)

module Tbl : Hashtbl.S with type key = t

(** {2 Set} *)

(** Imperative set of formulas *)

module FSet : sig
  type t

  val create : int -> t

  val copy : t -> t
    (** Copy of the set *)

  val is_empty : t -> bool

  val mem : t -> form -> bool

  val size : t -> int
    (** Number of formulas *)

  val eq : t -> t -> bool
    (** Equality of sets *)
  
  val add : t -> form -> unit

  val remove : t -> form -> unit

  val add_seq : t -> form Sequence.t -> unit

  val remove_seq : t -> form Sequence.t -> unit

  val filter : t -> (form -> bool) -> t

  val map : t -> (form -> form) -> t

  val fmap : t -> (form -> form option) -> t

  val flatMap : t -> (form -> form list) -> t

  val union : t -> t -> t
    (** Union of two sets *)

  val inter : t -> t -> t
    (** Intersection of two sets *)

  val to_seq : t -> form Sequence.t

  val iter : t -> (form -> unit) -> unit

  val of_list : form list -> t

  val to_list : t -> form list

  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
end
