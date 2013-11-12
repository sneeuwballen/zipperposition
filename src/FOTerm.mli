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

(** {1 First-order terms}
Terms are all annotated with their type. Hashconsing is always performed,
and takes the type into account (two terms are only equal if
they have the same type).
*)

(** term *)
type t = private {
  term : term_cell;       (** the term itself *)
  ty : Type.t;            (** type of the term *)
  mutable tsize : int;    (** size (number of subterms) *)
  mutable flags : int;    (** boolean flags about the term *)
  mutable tag : int;      (** hashconsing tag *)
}
(** content of the term *)
and term_cell = private
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Node of Symbol.t * t list   (** term application *)
and sourced_term =
  t * string * string           (** Term + file,name *)

type term = t

(** list of variables *)
type varlist = t list

(** {2 Comparison, equality, containers} *)

val subterm : sub:t -> t -> bool    (** checks whether [sub] is a (non-strict) subterm of [t] *)
val eq : t -> t -> bool             (** standard equality on terms *)
val compare : t -> t -> int         (** a simple order on terms *)
val hash : t -> int                 (** Fast hash on hashconsed terms *)
val hash_novar : t -> int           (** Hash that does not depend on variables *)

val get_type : t -> Type.t          (** Obtain the type of a term.. *)

module Tbl : sig
  include Hashtbl.S with type key = t
  val to_list : unit t -> term list
  val from_list : term list -> unit t
  val to_seq : unit t -> term Sequence.t
  val from_seq : term Sequence.t -> unit t
  val add_list : unit t -> term list -> unit
  val add_seq : unit t -> term Sequence.t -> unit
end

module Set : Sequence.Set.S with type elt = t
module Map : Sequence.Map.S with type key = t

module TCache : Cache.S with type key = t
module T2Cache : Cache.S2 with type key1 = t and type key2 = t

(** {2 Global terms table (hashconsing)} *)

module H : Hashcons.S with type elt = t

(** {2 Boolean flags} *)

val flag_ground : int       (** No variables *)
val flag_monomorphic : int  (** No type variable in term nor subterms *)

val set_flag : int -> t -> bool -> unit
  (** set or reset the given flag of the term to bool *)

val get_flag : int -> t -> bool
  (** read the flag *)

val new_flag : unit -> int
  (** New flag, different from all other term flags *)

(** {2 Smart constructors} *)

val mk_var : ty:Type.t -> int -> t  (** Create a variable. Providing a type is mandatory. *)
val mk_bound_var : ty:Type.t -> int -> t  (** Create a De Bruijn index. *)
val mk_node : ty:Type.t -> Symbol.t -> t list -> t    (** Application *)
val mk_const : ty:Type.t -> Symbol.t -> t       (** Shortcut for constants *)

val true_term : t                        (** tautology symbol *)
val false_term : t                       (** antilogy symbol *)

val cast : t -> Type.t -> t
  (** Change the type. *)

(** {2 Subterms and positions} *)

val is_var : t -> bool
val is_bound_var : t -> bool
val is_node : t -> bool
val is_const : t -> bool

val at_pos : t -> Position.t -> t
  (** retrieve subterm at pos, or raise Invalid_argument*)

val replace_pos : t -> Position.t -> t -> t
  (** replace t|_p by the second term. They should have the same type. *)

val replace : t -> old:t -> by:t -> t
  (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
      in [t] by the term [by]. *)

val at_cpos : t -> int -> t
  (** retrieve subterm at the compact pos.
      @raise Invalid_argument if the position is not valid within the term *)

val max_cpos : t -> int
  (** maximum compact position in the term *)

val var_occurs : t -> t -> bool          (** [var_occurs x t] true iff x in t *)
val is_ground : t -> bool                (** is the term ground? (no free vars) *)
val monomorphic : t -> bool              (** true if the term contains no type var *)
val max_var : varlist -> int             (** find the maximum variable index, or 0 *)
val min_var : varlist -> int             (** minimum variable, or 0 if ground *)
val add_vars : unit Tbl.t -> t -> unit   (** add variables of the term to the set *)
val vars : t -> varlist                  (** compute variables of the term *)
val vars_list : t list -> varlist        (** variables of terms in the list *)
val vars_seq : t Sequence.t -> varlist   (** variables of terms in the sequence *)
val vars_prefix_order : t -> varlist     (** variables of the term in prefix traversal order *)
val depth : t -> int                     (** depth of the term *)
val head : t -> Symbol.t                 (** head symbol (or Invalid_argument) *)
val size : t -> int

(** {2 De Bruijn Indexes manipulations} *)

val db_closed : ?depth:int -> t -> bool
  (** check whether the term is closed (all DB vars are bound within the term) *)

val db_contains : t -> int -> bool
  (** Does t contains the De Bruijn variable of index n? *)

val db_replace : ?depth:int -> into:t -> by:t -> t
  (** Substitution of De Bruijn symbol by a term. [db_replace ~into ~by]
      replaces the De Bruijn symbol 0 by [by] in [into]. *)

val db_lift : ?depth:int -> int -> t -> t
  (** lift the non-captured De Bruijn indexes in the term by n *)

val db_unlift : ?depth:int -> t -> t
  (** Unlift the term (decrement indices of all free De Bruijn variables
      inside *)

val db_from_term : ?depth:int -> t -> t -> t
  (** [db_from_term t t'] Replace [t'] by a fresh De Bruijn index in [t]. *)

val db_from_var : ?depth:int -> t -> t -> t
  (** [db_from_var t v] replace v by a De Bruijn symbol in t.
      Same as db_from_term. *)

(** {2 High-level operations} *)

val symbols : ?init:Symbol.Set.t -> t Sequence.t -> Symbol.Set.t
  (** Symbols of the terms (keys of signature) *)

val contains_symbol : Symbol.t -> t -> bool
  (** Does the term contain this given symbol? *)

(** {2 Fold} *)

(** High level fold-like combinators *)

val all_positions : ?vars:bool -> ?pos:Position.t -> t -> 'a ->
                    ('a -> t -> Position.t -> 'a) -> 'a
  (** apply f to all non-variable positions in t, accumulating the
      results along.
      [vars] specifies whether variables are folded on (default true). *)

(** {2 Some AC-utils} *)

val flatten_ac : Symbol.t -> t list -> t list
  (** [flatten_ac f l] flattens the list of terms [l] by deconstructing all its
      elements that have [f] as head symbol. For instance, if l=[1+2; 3+(4+5)]
      with f="+", this will return [1;2;3;4;5], perhaps in a different order *)

val ac_normal_form :  ?is_ac:(Symbol.t -> bool) ->
                      ?is_com:(Symbol.t -> bool) ->
                      t -> t
  (** normal form of the term modulo AC *)

val ac_eq : ?is_ac:(Symbol.t -> bool) -> ?is_com:(Symbol.t -> bool) ->
            t -> t -> bool
  (** Check whether the two terms are AC-equal. Optional arguments specify
      which symbols are AC or commutative (by default by looking at
      attr_ac and attr_commut). *)

val ac_symbols : is_ac:(Symbol.t -> bool) -> t Sequence.t -> Symbol.Set.t
  (** Set of symbols occurring in the terms, that are AC *)

(** {2 Printing/parsing} *)

type print_hook = (Buffer.t -> t -> unit) -> Buffer.t -> t -> bool
  (** User-provided hook that can be used to print terms (the {!Node} case)
      before the default printing occurs.
      A hook takes as first argument the recursive printing function
      that it can use to print subterms.
      A hook should return [true] if it fired, [false] to fall back
      on the default printing. *)

val pp_depth : ?hooks:print_hook list -> int -> Buffer.t -> t -> unit
val pp_tstp_depth : int -> Buffer.t -> t -> unit

val pp_debug : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit

val arith_hook : print_hook           (* hook to print arithmetic expressions *)
val pp_arith : Buffer.t -> t -> unit  (* use arith_hook with pp_debug *)

val pp : Buffer.t -> t -> unit
val set_default_pp : (Buffer.t -> t -> unit) -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

val print_var_types : bool ref
  (** If true, {!pp_debug} will print types of all vars, except those of type $i *)

val print_all_types : bool ref
  (** If true, {!pp_debug} will print the types of all annotated terms *)

val bij : t Bij.t

val debug : Format.formatter -> t -> unit
  (** debug printing, with sorts *)
