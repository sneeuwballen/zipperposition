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
(** infix symbols *)
val is_infix_symbol : symbol -> bool

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

val pp_symbol : Format.formatter -> symbol -> unit
val pp_foterm: Format.formatter -> foterm -> unit
val pp_foterm_sort : Format.formatter -> ?sort:bool -> foterm -> unit
val pp_signature : Format.formatter -> symbol list -> unit
