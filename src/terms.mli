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

(* ----------------------------------------------------------------------
 * comparison, equality, containers
 * ---------------------------------------------------------------------- *)

val member_term : term -> term -> bool    (** [a] [b] checks if a subterm of b *)
val eq_term : term -> term -> bool        (** standard equality on terms *)
val compare_term : term -> term -> int    (** a simple order on terms *)

module THashtbl : Hashtbl.S with type key = term

(** Simple hashset for small sets of terms *)
module THashSet :
  sig
    type t
    val create : unit -> t
    val cardinal : t -> int
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
val flag_ground : int
val flag_db_closed_computed : int

val set_flag : int -> term -> bool -> unit
  (** set or reset the given flag of the term to bool *)

val get_flag : int -> term -> bool
  (** read the flag *)

(* ----------------------------------------------------------------------
 * smart constructors, with a bit of type-checking
 * ---------------------------------------------------------------------- *)

(** In this section, term smart constructors are defined. Some of them
    accept a [?old] optional argument. This argument is an already existing
    term that the caller believes is likely to be equal to the result.
    This makes hashconsing faster if the result is equal to [old]. *)

val mk_var : ?old:term -> int -> sort -> term       (** Create a variable. The index must be >= 0 *)
val mk_bound_var : ?old:term -> int -> sort -> term (** De Bruijn index, must be >= 0 *)
val mk_bind : ?old:term -> symbol -> term -> term
val mk_node : ?old:term -> symbol -> sort -> term list -> term
val mk_const : ?old:term -> symbol -> sort -> term

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

val mk_at : ?old:term -> term -> term -> term   (** t1 t2 -> t1 @ t2 *)

val cast : term -> sort -> term             (** cast (change sort) *)

(* ----------------------------------------------------------------------
 * examine term/subterms, positions...
 * ---------------------------------------------------------------------- *)

val is_var : term -> bool
val is_bound_var : term -> bool
val is_node : term -> bool
val is_const : term -> bool
val is_bind : term -> bool

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
val max_var : varlist -> int                (** find the maximum variable index, >= 0 *)
val min_var : varlist -> int
val add_vars : THashSet.t -> term -> unit   (** add variables of the term to the set *)
val vars : term -> varlist                  (** compute variables of the term *)
val vars_list : term list -> varlist        (** variables of terms in the list *)
val depth : term -> int                     (** depth of the term *)

(* ----------------------------------------------------------------------
 * De Bruijn terms, and dotted formulas
 * ---------------------------------------------------------------------- *)

val atomic : term -> bool                   (** atomic proposition, or term, at root *)
val atomic_rec : term -> bool               (** does not contain connectives/quantifiers *)
val db_closed : term -> bool                (** check whether the term is closed *)

val db_contains : term -> int -> bool
  (** Does t contains the De Bruijn variable of index n? *)

val db_replace : term -> term -> term
  (** Substitution of De Bruijn symbol by a term. [db_replace t s]
      replaces the De Bruijn symbol 0 by s in t *)

val db_lift : int -> term -> term
  (** lift the non-captured De Bruijn indexes in the term by n *)

val db_unlift : term -> term
  (** Unlift the term (decrement indices of all De Bruijn variables inside *)

val db_from_var : term -> term -> term
  (** [db_from_var t v] replace v by a De Bruijn symbol in t *)

val look_db_sort : int -> term -> sort option
  (** [look_db_sort n t] find the sort of the De Bruijn index n in t *)

(** {2 High-level transformations} *)

val close_forall : term -> term             (** Bind all free variables by 'forall' *)
val close_exists : term -> term             (** Bind all free variables by 'exists' *)

val db_to_classic : ?varindex:int ref -> term -> term
  (** Transform binders and De Bruijn indexes into regular variables.
      [varindex] is a variable counter used to give fresh variables
      names to De Bruijn indexes. *)

val curry : term -> term                    (** Currify all subterms *)
val uncurry : term -> term                  (** Un-currify all subterms *)

val beta_reduce : term -> term              (** Beta-reduce the (curryfied) term *)

val eta_reduce : term -> term               (** Eta-reduce the (curryfied) term *)

val eta_lift : term -> term -> term
  (** [eta_lift t sub_t], applied to a currified term [t], and a
      subterm [sub_t] of [t], gives [t'] such that
      [beta_reduce (t' @ sub_t) == t] holds.
      It basically abstracts out [sub_t] with a lambda. If [sub_t] is not
      a subterm of [t], then [t' == ^[X]: t].

      For instance (@ are omitted), [eta_lift f(a,g @ b,c) g] will return
      the term [^[X]: f(a, X @ b, c)] *)

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
    sort : bool -> unit;                            (** print sorts of terms? *)
  >

val pp_precedence : Format.formatter -> symbol list -> unit      (** print symbol precedence *)

(* ----------------------------------------------------------------------
 * conversions with simple terms/formulas
 * ---------------------------------------------------------------------- *)

val from_simple : Simple.term -> term
val from_simple_formula : Simple.formula -> term
val to_simple : term -> Simple.term option  (** fails if the term is of bool sort *)

(** {2 JSON} *)

val to_json : term -> json
val of_json : json -> term

(* ----------------------------------------------------------------------
 * skolem terms
 * ---------------------------------------------------------------------- *)

(** Prefix used for skolem symbols *)
val skolem_prefix : string ref

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
