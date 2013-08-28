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

(** {1 First-order terms} *)

(** term *)
type t = private {
  term : term_cell;             (** the term itself *)
  type_ : Type.t option;        (** optional type *)
  mutable tsize : int;          (** size (number of subterms) *)
  mutable flags : int;          (** boolean flags about the term *)
  mutable tag : int;            (** hashconsing tag *)
}
(** content of the term *)
and term_cell = private
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Bind of Symbol.t * t        (** bind one variable (of given sort), with the symbol *)
  | Node of Symbol.t * t list   (** term application *)
  | At of t * t                 (** HO application (curried) *)
and sourced_term =
  t * string * string        (** Term + file,name *)

type term = t

(** list of variables *)
type varlist = t list            

(** {2 Comparison, equality, containers} *)

val subterm : sub:t -> t -> bool    (** checks whether [sub] is a (non-strict) subterm of [t] *)
val eq : t -> t -> bool             (** standard equality on terms *)
val compare : t -> t -> int         (** a simple order on terms *)
val hash : t -> int

val has_type : t -> bool              (** Has a known type *)
val compatible_type : t -> t -> bool  (** Unifiable types? false if type missing *)
val same_type : t -> t -> bool        (** Alpha-equiv types? false if type missing *)
val compare_type : t -> t -> int      (** Comparison of types *)

module THashtbl : Hashtbl.S with type key = t
module TSet : Sequence.Set.S with type elt = t
module TMap : Sequence.Map.S with type key = t

module TCache : Cache.S with type key = t
module T2Cache : Cache.S2 with type key1 = t and type key2 = t

(** {2 Hashset of terms} *)
module THashSet : sig
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

(** {2 Global terms table (hashconsing)} *)

module H : Hashcons.S with type elt = t

(** {2 Boolean flags} *)

val flag_db_closed : int
val flag_simplified : int
val flag_normal_form : int
val flag_ground : int
val flag_db_closed_computed : int

val set_flag : int -> t -> bool -> unit
  (** set or reset the given flag of the term to bool *)

val get_flag : int -> t -> bool
  (** read the flag *)

(** {2 Smart constructors} *)

val mk_var : ?ty:Type.t -> int -> t        (** Create a variable. The index must be >= 0 *)
val mk_bound_var : ?ty:Type.t -> int -> t  (** De Bruijn index, must be >= 0 *)

val mk_bind : Symbol.t -> t -> t
  (** [mk_bind s t] binds the De Bruijn 0 in [t]. *)

val mk_node : Symbol.t -> t list -> t
val mk_const : Symbol.t -> t

val mk_at : t -> t -> t
val mk_at_list : t -> t list -> t

val true_term : t                        (** tautology symbol *)
val false_term : t                       (** antilogy symbol *)

val mk_not : t -> t
val mk_and : t -> t -> t
val mk_or : t -> t -> t
val mk_imply : t -> t -> t
val mk_equiv : t -> t -> t
val mk_xor : t -> t -> t
val mk_eq : t -> t -> t
val mk_neq : t -> t -> t
val mk_lambda : t -> t
val mk_forall : t -> t
val mk_exists : t -> t

val mk_and_list : t list -> t
val mk_or_list : t list -> t

(** {2 Typing} *)

val is_bool : t -> bool               (** Boolean typed? *)
val cast : t -> Type.t -> t           (** Set the type *)
val arity : t -> int                  (** Arity, or 0 if it makes no sense *)

(** {2 Subterms and positions} *)

val is_var : t -> bool
val is_bound_var : t -> bool
val is_node : t -> bool
val is_const : t -> bool
val is_at : t -> bool
val is_bind : t -> bool

val at_pos : t -> Position.t -> t 
  (** retrieve subterm at pos, or raise Invalid_argument*)

val replace_pos : t -> Position.t -> t -> t
  (** replace t|_p by the second term *)

val replace : t -> old:term -> by:term -> t
  (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
      in [t] by the term [by]. *)

val at_cpos : term -> int -> term
  (** retrieve subterm at the compact pos, or raise Invalid_argument*)

val max_cpos : term -> int
  (** maximum compact position in the term *)

val var_occurs : t -> t -> bool          (** [var_occurs x t] true iff x in t *)
val is_ground : t -> bool                (** is the term ground? (no free vars) *)
val max_var : varlist -> int             (** find the maximum variable index, >= 0 *)
val min_var : varlist -> int
val add_vars : THashSet.t -> t -> unit   (** add variables of the term to the set *)
val vars : t -> varlist                  (** compute variables of the term *)
val vars_list : t list -> varlist        (** variables of terms in the list *)
val vars_seq : t Sequence.t -> varlist   (** variables of terms in the sequence *)
val vars_prefix_order : t -> varlist     (** variables of the term in prefix traversal order *)
val depth : t -> int                     (** depth of the term *)
val head : t -> Symbol.t                  (** head symbol (or Invalid_argument) *)
val size : t -> int

(** {2 De Bruijn indexes} *)

val atomic : t -> bool                   (** atomic proposition, or term, at root *)
val atomic_rec : t -> bool               (** does not contain connectives/quantifiers *)
val db_closed : t -> bool                (** check whether the term is closed *)

val db_contains : t -> int -> bool
  (** Does t contains the De Bruijn variable of index n? *)

val db_replace : t -> t -> t
  (** Substitution of De Bruijn symbol by a term. [db_replace t s]
      replaces the De Bruijn symbol 0 by s in t *)

val db_type : t -> int -> Type.t option
  (** [db_type t n] returns the type of the [n]-th De Bruijn index in [t] *)

val db_lift : int -> t -> t
  (** lift the non-captured De Bruijn indexes in the term by n *)

val db_unlift : t -> t
  (** Unlift the term (decrement indices of all De Bruijn variables inside *)

val db_from_term : t -> t -> t
  (** Replace [t'] by a fresh De Bruijn index in [t]. *)

val db_from_var : t -> t -> t
  (** [db_from_var t v] replace v by a De Bruijn symbol in t.
      Same as db_from_term. *)

(** {2 High-level operations} *)

(** constructors with free variables. The first argument is the
    list of variables that is bound, then the quantified/abstracted
    term. *)

val mk_lambda_var : t list -> t -> t   (** (lambda v1,...,vn. t). *)
val mk_forall_var : t list -> t -> t
val mk_exists_var : t list -> t -> t

val close_forall : t -> t             (** Bind all free variables by 'forall' *)
val close_exists : t -> t             (** Bind all free variables by 'exists' *)

val symbols : t Sequence.t -> Symbol.SSet.t   (** Symbols of the terms (keys of signature) *)

val db_to_classic : ?varindex:int ref -> t -> t
  (** Transform binders and De Bruijn indexes into regular variables.
      [varindex] is a variable counter used to give fresh variables
      names to De Bruijn indexes. *)

val curry : t -> t                    (** Curry all subterms *)
val uncurry : t -> t                  (** Un-curry all subterms *)
val curryfied : t -> bool             (** Is the term already curried? *)

val is_fo : t -> bool                 (** Check that the (curried) term is first-order *)

val beta_reduce : t -> t              (** Beta-reduce the (curried) term *)

val eta_reduce : t -> t               (** Eta-reduce the (curried) term *)

val lambda_abstract : t -> t -> t
  (** [lambda_abstract t sub_t], applied to a curried term [t], and a
      subterm [sub_t] of [t], gives [t'] such that
      [beta_reduce (t' @ sub_t) == t] holds.
      It basically abstracts out [sub_t] with a lambda. If [sub_t] is not
      a subterm of [t], then [t' == ^[X]: t].

      For instance (@ are omitted), [lambda_abstract f(a,g @ b,c) g] will return
      the term [^[X]: f(a, X @ b, c)] *)

val lambda_abstract_list : t -> t list -> t
val lambda_apply_list : t -> t list -> t

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

(** {2 Printing/parsing} *)

val pp : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit
val bij : t Bij.t

val debug : Format.formatter -> t -> unit
  (** debug printing, with sorts *)

val print_sort : bool ref
  (** If enabled, sorts will be printed by pp_debug (default false) *)
