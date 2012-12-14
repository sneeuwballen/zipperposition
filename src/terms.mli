(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

open Types
open Symbols

(** Functions on first-order terms *)

(** symbols that are symmetric (that is, order of arguments does not matter) *)
val is_symmetric_symbol : symbol -> bool
val is_infix_symbol : symbol -> bool
val is_binder_symbol : symbol -> bool

(* ----------------------------------------------------------------------
 * comparison, equality, containers
 * ---------------------------------------------------------------------- *)
val member_term : term -> term -> bool    (** [a] [b] checks if a subterm of b *)
val member_term_rec : term -> term -> bool(** same, but follows variable bindings *)
val eq_term : term -> term -> bool        (** standard equality on terms *)
val compare_term : term -> term -> int    (** a simple order on terms *)

module TSet : Set.S with type elt = term
module TPairSet : Set.S with type elt = term * term
module THashtbl : Hashtbl.S with type key = term

(** Simple hashset for small sets of terms *)
module THashSet :
  sig
    type t
    val create : unit -> t
    val member : t -> term -> bool
    val iter : t -> (term -> unit) -> unit
    val add : t -> term -> unit
    val merge : t -> t -> unit              (** [merge s1 s2] adds elements of s2 to s1 *)
    val to_list : t -> term list            (** build a list from the set *)
    val from_list : term list -> t          (** build a set from the list *)
  end

(* ----------------------------------------------------------------------
 * access global terms table (hashconsing)
 * ---------------------------------------------------------------------- *)

val iter_terms : (term -> unit) -> unit       (** iterate through existing terms *)
val all_terms : unit -> term list             (** all currently existing terms *)
val stats : unit -> (int*int*int*int*int*int) (** hashcons stats *)

(* ----------------------------------------------------------------------
 * boolean flags
 * ---------------------------------------------------------------------- *)

val flag_db_closed : int
val flag_simplified : int
val flag_normal_form : int

val set_flag : int -> term -> bool -> unit
  (** set or reset the given flag of the term to bool *)
val get_flag : int -> term -> bool
  (** read the flag *)

(* ----------------------------------------------------------------------
 * smart constructors, with a bit of type-checking
 * ---------------------------------------------------------------------- *)

val mk_var : int -> sort -> term
val mk_node : symbol -> sort -> term list -> term
val mk_const : symbol -> sort -> term

val true_term : term                        (** tautology symbol *)
val false_term : term                       (** antilogy symbol *)

val mk_not : term -> term
val mk_and : term -> term -> term
val mk_or : term -> term -> term
val mk_imply : term -> term -> term
val mk_equiv : term -> term -> term
val mk_eq : term -> term -> term
val mk_lambda : term -> term
val mk_forall : term -> term
val mk_exists : term -> term

val cast : term -> sort -> term             (** cast (change sort) *)

(* ----------------------------------------------------------------------
 * examine term/subterms, positions...
 * ---------------------------------------------------------------------- *)
val is_var : term -> bool
val is_const : term -> bool
val is_node : term -> bool

val at_pos : term -> position -> term 
  (** retrieve subterm at pos, or raise Invalid_argument*)

val replace_pos : term -> position -> term -> term
  (** replace t|_p by the second term *)

val at_cpos : term -> compact_position -> term
  (** retrieve subterm at the compact pos, or raise Invalid_argument*)

val max_cpos : term -> compact_position
  (** maximum position in the term *)

val pos_to_cpos : position -> compact_position
val cpos_to_pos : compact_position -> position

val var_occurs : term -> term -> bool       (** [var_occurs x t] true iff x in t *)
val is_ground_term : term -> bool           (** is the term ground? *)
val merge_varlist : varlist -> varlist -> varlist (** set union of variable list *)
val max_var : varlist -> int                (** find the maximum variable index *)
val min_var : varlist -> int

(* ----------------------------------------------------------------------
 * De Bruijn terms, and dotted formulas
 * ---------------------------------------------------------------------- *)
val atomic : term -> bool                   (** atomic proposition, or term, at root *)
val atomic_rec : term -> bool               (** does not contain connectives/quantifiers *)
val db_closed : term -> bool                (** check whether the term is closed *)
val db_var : term -> bool                   (** is the term a De Bruijn index? *)

(** Does t contains the De Bruijn variable of index n? *)
val db_contains : term -> int -> bool
(** Substitution of De Bruijn symbol by a term. [db_replace t s]
    replaces the De Bruijn symbol 0 by s in t *)
val db_replace : term -> term -> term
(** Create a De Bruijn variable of index n *)
val db_make : int -> sort -> term
(** lift the non-captured De Bruijn indexes in the term by n *)
val db_lift : int -> term -> term
(** Unlift the term (decrement indices of all De Bruijn variables inside *)
val db_unlift : term -> term
(** [db_from_var t v] replace v by a De Bruijn symbol in t *)
val db_from_var : term -> term -> term
(** index of the De Bruijn term *)
val db_depth : term -> int
(** [look_db_sort n t] find the sort of the De Bruijn index n in t *)
val look_db_sort : int -> term -> sort option

(* ----------------------------------------------------------------------
 * bindings and normal forms
 * ---------------------------------------------------------------------- *)
val set_binding : term -> term -> unit      (** [set_binding t d] set variable binding or normal form of t *)
val reset_binding : term -> unit            (** reset variable binding/normal form *)
val get_binding : term -> term              (** get the binding of variable/normal form of term *)
val expand_bindings : ?recursive:bool ->
                      term -> term          (** replace variables by their bindings *)

val reset_vars : term -> unit               (** reset bindings of variables of the term *)

(* ----------------------------------------------------------------------
 * Pretty printing
 * ---------------------------------------------------------------------- *)

(** type of a pretty printer for symbols *)
class type pprinter_symbol =
  object
    method pp : Format.formatter -> symbol -> unit  (** pretty print a symbol *)
    method infix : symbol -> bool                   (** which symbol is infix? *)
  end

val pp_symbol : pprinter_symbol ref                 (** default pp for symbols *)
val pp_symbol_unicode : pprinter_symbol             (** print with unicode special symbols*)
val pp_symbol_tstp : pprinter_symbol                (** tstp convention (raw) *)

(** type of a pretty printer for terms *)
class type pprinter_term =
  object
    method pp : Format.formatter -> term -> unit  (** pretty print a term *)
  end

val pp_term : pprinter_term ref                     (** current choice *)
val pp_term_tstp : pprinter_term                    (** print term in TSTP syntax *)
val pp_term_debug :                                 (** print term in a nice syntax *)
  <
    pp : Format.formatter  -> term -> unit;
    bindings : bool -> unit;                        (** print bindings of variables? *)
    sort : bool -> unit;                            (** print sorts of terms? *)
    skip_lambdas : bool -> unit;                    (** print lambdas after quantifiers? *)
    skip_db : bool -> unit;                         (** nice printing of De Bruijn terms *)
  >

val pp_signature : Format.formatter -> symbol list -> unit      (** print signature *)

(* ----------------------------------------------------------------------
 * conversions with simple terms/formulas
 * ---------------------------------------------------------------------- *)

val from_simple : Simple.term -> term
val from_simple_formula : Simple.formula -> term
val to_simple : term -> Simple.term option  (** fails if the term is of bool sort *)

(* ----------------------------------------------------------------------
 * skolem terms
 * ---------------------------------------------------------------------- *)

(** Skolemize the given term at root (assumes it occurs just under an
    existential quantifier, whose De Bruijn variable is replaced
    by a fresh symbol applied to free variables). This also
    caches symbols, so that the same term is always skolemized
    the same way. The sort is the sort of the free De Bruijn symbol in t.

    It also refreshes the ordering (the signature has changed) *)
val classic_skolem : ord:ordering -> term -> sort -> term

(** Skolemization with a special non-first order symbol. The purpose is
    not to introduce too many terms. A proposition p is skolemized
    into $$skolem(p), which makes naturally for inner skolemization.

    The advantage is that it does not modify the signature, and also that
    rewriting can be performed inside the skolem terms. *)
val unamed_skolem : ord:ordering -> term -> sort -> term

(** default skolemization function *)
val skolem : (ord:ordering -> term -> sort -> term) ref
