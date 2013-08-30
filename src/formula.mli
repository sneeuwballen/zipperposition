
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

type t = private {
  form : cell;
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
  | Forall of Term.t * t    (** Quantified variable, plus formula *)
  | Exists of Term.t * t

type sourced_form = t * string * string    (* form, filename, axiom name *)

val eq : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

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
val mk_forall : Term.t -> t -> t
val mk_exists : Term.t -> t -> t

val mk_forall_list : Term.t list -> t -> t
val mk_exists_list : Term.t list -> t -> t

(** {2 Combinators} *)

val map_leaf : (t -> t) -> t -> t
  (** Call the function on leaves (atom,equal,true,false) and replace the
      leaves by their image *)

val map : (Term.t -> Term.t) -> t -> t    (** Map on terms *)
val fold : ('a -> Term.t -> 'a) -> 'a -> t -> 'a  (** Fold on terms *)
val iter : (Term.t -> unit) -> t -> unit

val fold_bv : ?bv:Term.varlist ->
              ('a -> Term.varlist -> Term.t -> 'a) ->
              'a -> t -> 'a
  (** Fold on terms, but with an additional argument, the list of
      variables that are bound through the path to the term *)

(** The following functions gather the terms of a formula.
    However, bound variables are not gathered. *)

val add_terms : Term.THashSet.t -> t -> unit
val terms : t -> Term.THashSet.t

val terms_seq : t -> Term.t Sequence.t
  (** Sequence of terms. Terms may occur several times *)

val subterm : Term.t -> t -> bool
  (** [subterm t f] true iff [t] occurs in some term of [f] *)

val bound_variables : t -> Term.varlist
  (** Variables bound in a quantifier *)

val free_variables : t -> Term.varlist
  (** Variables not bound by any (formula) quantifier *)

val close_forall : t -> t   (** Bind all free variables with forall *)
val close_exists : t -> t   (** Bind all free variables with exists *)

val is_atomic : t -> bool   (** No connectives? *)
val is_ground : t -> bool   (** No variables? *)
val is_closed : t -> bool   (** All variables bound? *)

val flatten : t -> t        (** Flatten AC connectives (or/and) *)
val simplify : t -> t       (** Simplify the formula *)

val ac_normal_form : t -> t (** Normal form modulo AC of "or" and "and" *)
val ac_eq : t -> t -> bool  (** Equal modulo AC? *)

val apply_subst : ?renaming:Substs.Renaming.t -> ?recursive:bool ->
                  subst:Substs.t -> 
                  t -> Substs.scope -> t
  (** Apply substitution to the formula. Quantified variables are not
      replaced. *)

(** {2 Conversions} *)

val to_term : t -> Term.t   (** Conversion to term *)
val of_term : Term.t -> t

(** {2 Typing} *)

val infer_type : TypeInference.Ctx.t -> t -> unit
val signature : ?signature:Signature.t -> t -> Signature.t

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string

val bij : t Bij.t
