
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

(** {1 Higher Order Terms}

     Both higher order formulas and terms are represented by terms. *)

(** term *)
type t = private {
  term : term_cell;             (** the term itself *)
  mutable ty : Type.t;          (** type *)
  mutable tsize : int;          (** size (number of subterms) *)
  mutable flags : int;          (** boolean flags about the term *)
  mutable tag : int;            (** hashconsing tag *)
}
(** content of the term *)
and term_cell = private
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Lambda of t                 (** lambda abstraction over one variable. *)
  | Const of Symbol.t           (** Constant *)
  | At of t * Type.t list * t list
    (** HO function application. Invariant: first term is not a {!At}. *)

and sourced_term =
  t * string * string           (** Term + file,name *)

type term = t

type varlist = t list

(** {2 Basics} *)

val subterm : sub:t -> t -> bool    (** checks whether [sub] is a (non-strict) subterm of [t] *)
val eq : t -> t -> bool             (** standard equality on terms *)
val compare : t -> t -> int         (** a simple order on terms *)
val hash : t -> int
val hash_novar : t -> int           (** Hash that does not depend on variables *)

val ty : t -> Type.t
val lambda_var_ty : t -> Type.t     (** Only on lambda terms. @raise Invalid_argument otherwise. *)

module Tbl : Hashtbl.S with type key = t
module Set : Sequence.Set.S with type elt = t
module Map : Sequence.Map.S with type key = t
module Cache : Cache.S with type key = t

(** {2 Global terms table (hashconsing)} *)

module H : Hashcons.S with type elt = t

(** {2 Boolean flags} *)

val flag_db_closed : int
val flag_normal_form : int
val flag_ground : int
val flag_db_closed_computed : int

val set_flag : int -> t -> bool -> unit
  (** set or reset the given flag of the term to bool *)

val get_flag : int -> t -> bool
  (** read the flag *)

val new_flag : unit -> int
  (** New flag, different from all other flags *)

(** {2 Smart Constructors}

The constructors take care of type-checking and hashconsing.
They may raise Type.Error in case of type error.

Use {!mk_lambda} rather than {!__mk_lambda}, and try not to create bound
variables by yourself.
*)

val mk_var : ty:Type.t -> int -> t
  (** Create a variable. The index must be >= 0 *)

val __mk_bound_var : ty:Type.t -> int -> t  (** not documented *)
val __mk_lambda : varty:Type.t -> t -> t    (** not documented *)

val mk_const : Symbol.t -> t
  (** Constant. The type of the constant is the type of the symbol. *)

val mk_at : ?tyargs:Type.t list -> t -> t list -> t
  (** Apply a term to other terms. Type can be deduced from the constant,
      the optional type arguments, and the term arguments.
      @raise Type.Error if types do not match. *)

val true_term : t   (** tautology term *)
val false_term : t  (** antilogy term *)

val not_term : t
val and_term : t
val or_term : t
val imply_term : t
val equiv_term : t

val eq_term : t
val forall_term : t
val exists_term : t

val mk_not : t -> t
val mk_and : t -> t -> t
val mk_or : t -> t -> t
val mk_imply : t -> t -> t
val mk_equiv : t -> t -> t
val mk_xor : t -> t -> t
val mk_eq : t -> t -> t
val mk_neq : t -> t -> t

val mk_and_list : t list -> t
val mk_or_list : t list -> t

(** constructors with free variables. The first argument is the
    list of variables that is bound, then the quantified/abstracted
    term. *)

val mk_lambda : t list -> t -> t   (** (lambda v1,...,vn. t). *)
val mk_forall : t list -> t -> t
val mk_exists : t list -> t -> t

val __mk_forall : varty:Type.t -> t -> t
val __mk_exists : varty:Type.t -> t -> t

val cast : t -> Type.t -> t
  (** Change the type. Only works for variables and bound variables. *)

(** {2 Properties} *)

val is_var : t -> bool
val is_bound_var : t -> bool
val is_const : t -> bool
val is_at : t -> bool
val is_lambda : t -> bool

(** {2 Positions} *)

val at_pos : t -> Position.t -> t 
  (** retrieve subterm at pos, or raise Invalid_argument*)

val replace_pos : t -> Position.t -> t -> t
  (** replace t|_p by the second term *)

val replace : t -> old:t -> by:t -> t
  (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
      in [t] by the term [by]. *)

val at_cpos : t -> int -> t
  (** retrieve subterm at the compact pos, or raise Invalid_argument*)

val max_cpos : t -> int
  (** maximum compact position in the term *)

val var_occurs : t -> t -> bool          (** [var_occurs x t] true iff x in t *)
val is_ground : t -> bool                (** is the term ground? (no free vars) *)
val monomorphic : t -> bool              (** true if no type variable occurs in term *)
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

val symbols : t Sequence.t -> Symbol.Set.t   (** Symbols of the terms (keys of signature) *)
val contains_symbol : Symbol.t -> t -> bool

val flatten_ac : Symbol.t -> t list -> t list
  (** [flatten_ac f l] flattens the list of terms [l] by deconstructing all its
      elements that have [f] as head symbol. For instance, if l=[1+2; 3+(4+5)]
      with f="+", this will return [1;2;3;4;5], perhaps in a different order *)

(** {2 Conversion with {!FOTerm}} *)

val curry : FOTerm.t -> t         (** Curry all subterms *)
val uncurry : t -> FOTerm.t       (** Un-curry all subterms *)

val is_fo : t -> bool             (** Check whether the term is convertible
                                      to a first-order term (no binders, no
                                      variable applied to something...) *)

(** {2 De Bruijn indexes} *)

val atomic : t -> bool        (** does not contain connectives/quantifiers *)

module DB : sig
  val closed : ?depth:int -> t -> bool
    (** check whether the term is closed (all DB vars are bound within
        the term) *)

  val contains : t -> int -> bool
    (** Does t contains the De Bruijn variable of index n? *)

  val shift : ?depth:int -> int -> t -> t
    (** shift the non-captured De Bruijn indexes in the term by n *)

  val unshift : ?depth:int -> int -> t -> t
    (** Unshift the term (decrement indices of all free De Bruijn variables
        inside) by [n] *)

  val replace : ?depth:int -> t -> sub:t -> t
    (** [db_from_term t ~sub] replaces [sub] by a fresh De Bruijn index in [t]. *)

  val from_var : ?depth:int -> t -> var:t -> t
    (** [db_from_var t ~var] replace [var] by a De Bruijn symbol in t.
        Same as {!replace}. *)

  val eval : ?depth:int -> t DBEnv.t -> t -> t
    (** Evaluate the term in the given De Bruijn environment, by
        replacing De Bruijn indices by their value in the environment. *)
end

(** {2 High level operations} *)

val close_forall : t -> t     (** Bind all free variables with 'forall' *)
val close_exists : t -> t     (** Bind all free variables with 'exists' *)

(** {2 IO} *)

(** First, full functions with the amount of surrounding binders; then helpers
    in the case this amount is 0 (for instance in clauses) *)

val pp_depth : int -> Buffer.t -> t -> unit
val pp_tstp_depth : int -> Buffer.t -> t -> unit

val pp_debug : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit

val print_all_types : bool ref

val pp : Buffer.t -> t -> unit
val set_default_pp : (Buffer.t -> t -> unit) -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

val bij : t Bij.t

val debug : Format.formatter -> t -> unit
  (** debug printing, with sorts *)
