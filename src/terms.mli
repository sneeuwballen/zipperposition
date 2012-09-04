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

(** Functions on first-order terms *)

(** symbols that are symmetric (that is, order of arguments does not matter) *)
val is_symmetric_symbol : symbol -> bool

module H : Hashcons.S with type key = typed_term
val terms : H.t

val iter_terms : (foterm -> unit) -> unit     (** iterate through existing terms *)
val all_terms : unit -> foterm list           (** all currently existing terms *)

(** smart constructors, with a bit of type-checking *)
val mk_var : int -> sort -> foterm
val mk_leaf : symbol -> sort -> foterm
val mk_node : foterm list -> foterm
val mk_apply : symbol -> sort -> foterm list -> foterm

val is_var : foterm -> bool
val is_leaf : foterm -> bool
val is_node : foterm -> bool
val hd_term : foterm -> foterm option         (** the head of the term *)
val hd_symbol : foterm -> symbol option       (** the head of the term *)

val true_term : foterm                        (** tautology symbol *)
val false_term : foterm                       (** antilogy symbol *)

val mk_not : foterm -> foterm
val mk_and : foterm -> foterm -> foterm
val mk_or : foterm -> foterm -> foterm
val mk_imply : foterm -> foterm -> foterm
val mk_eq : foterm -> foterm -> foterm
val mk_lambda : foterm -> foterm
val mk_forall : foterm -> foterm
val mk_exists : foterm -> foterm

val member_term : foterm -> foterm -> bool    (** [a] [b] checks if a subterm of b *)
val eq_foterm : foterm -> foterm -> bool      (** standard equality on terms *)
val compare_foterm : foterm -> foterm -> int  (** a simple order on terms *)
val cast : foterm -> sort -> foterm           (** cast (change sort) *)

val at_pos : foterm -> position -> foterm     (** retrieve subterm at pos, or
                                                  raise Invalid_argument
                                                  TODO also return a context? *)
val replace_pos : foterm -> position          (** replace t|_p by the second term *)
               -> foterm -> foterm

val vars_of_term : foterm -> varlist          (** free variables in the term *)
val is_ground_term : foterm -> bool           (** is the term ground? *)
val merge_varlist : varlist -> varlist -> varlist (** set union of variable list *)

val max_var : varlist -> int                  (** find the maximum variable index *)
val min_var : varlist -> int

val atomic : foterm -> bool                   (** atomic proposition, or term, at root *)
val atomic_rec : foterm -> bool               (** does not contain connectives/quantifiers *)
val db_closed : foterm -> bool                (** check whether the term is closed *)

(** Does t contains the De Bruijn variable of index n? *)
val db_contains : foterm -> int -> bool
(** Substitution of De Bruijn symbol by a term. [db_replace t s]
    replaces the De Bruijn symbol 0 by s in t *)
val db_replace : foterm -> foterm -> foterm
(** Create a De Bruijn variable of index n *)
val db_make : int -> sort -> foterm
(** Unlift the term (decrement indices of all De Bruijn variables inside *)
val db_unlift : foterm -> foterm
(** [db_from_var t v] replace v by a De Bruijn symbol in t *)
val db_from_var : foterm -> foterm -> foterm
(** index of the De Bruijn term *)
val db_depth : foterm -> int

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
    method pp : Format.formatter -> foterm -> unit  (** pretty print a term *)
  end

val pp_term : pprinter_term ref                     (** current choice *)
val pp_term_tstp : pprinter_term                    (** print term in TSTP syntax *)
val pp_term_debug :                                 (** print term in a nice syntax *)
  <
    pp : Format.formatter  -> foterm -> unit;
    sort : bool -> unit;                            (** print sorts of terms? *)
    skip_lambdas : bool -> unit;                    (** print lambdas after quantifiers? *)
    skip_db : bool -> unit;                         (** nice printing of De Bruijn terms *)
  >

val pp_signature : Format.formatter -> symbol list -> unit      (** print signature *)
