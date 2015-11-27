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
Terms use LogtkScopedTerm with kind "LogtkFOTerm".
*)

type symbol = LogtkSymbol.t

(** {2 Term} *)

type t = private LogtkScopedTerm.t

type term = t

type view = private
  | Var of int                (** Term variable *)
  | BVar of int               (** Bound variable (De Bruijn index) *)
  | Const of LogtkSymbol.t         (** LogtkTyped constant *)
  | TyApp of t * LogtkType.t       (** Application to type *)
  | App of t  * t list        (** Application to a list of terms (cannot be left-nested) *)

val view : t -> view

val kind : LogtkScopedTerm.Kind.t

val open_app : t -> t * LogtkType.t list * t list
  (** Open application recursively so as to gather all type arguments *)

(** {2 Classic view} *)
module Classic : sig
  type view = private
  | Var of int
  | BVar of int
  | App of symbol * LogtkType.t list * t list  (** covers Const and App *)
  | NonFO   (* any other case *)

  val view : t -> view
end

(** {2 LogtkComparison, equality, containers} *)

val subterm : sub:t -> t -> bool
  (** checks whether [sub] is a (non-strict) subterm of [t] *)

include LogtkInterfaces.HASH with type t := t
include LogtkInterfaces.ORD with type t := t

val ty : t -> LogtkType.t                (** Obtain the type of a term.. *)

module Tbl : sig
  include Hashtbl.S with type key = t
  val to_list : 'a t -> (key * 'a) list
  val of_list : ?init:'a t -> (key * 'a) list -> 'a t
  val to_seq : 'a t -> (key * 'a) Sequence.t
  val of_seq : ?init:'a t -> (key * 'a) Sequence.t -> 'a t
end

module Set : Sequence.Set.S with type elt = t
module Map : Sequence.Map.S with type key = t

module TLogtkCache : LogtkCache.S with type key = t
module T2LogtkCache : LogtkCache.S2 with type key1 = t and type key2 = t

(** {2 Constructors} *)

val var : ty:LogtkType.t -> int -> t
  (** Create a variable. Providing a type is mandatory.
      @raise LogtkScopedTerm.IllFormedTerm if the index is < 0 *)

val bvar : ty:LogtkType.t -> int -> t
  (** Create a bound variable. Providing a type is mandatory.
      {b Warning}: be careful and try not to use this function directly.
      @raise LogtkScopedTerm.IllFormedTerm if the index is < 0 *)

val const : ty:LogtkType.t -> symbol -> t
  (** Create a typed constant *)

val tyapp : t -> LogtkType.t -> t
  (** Apply a term to a type
      @raise LogtkType.Error if types do not match. *)

val app : t -> t list -> t
  (** Apply a term to a list of terms
      @raise LogtkType.Error if types do not match. *)

val app_full : t -> LogtkType.t list -> t list -> t
  (** Apply the term to types, then to terms *)

val cast : ty:LogtkType.t -> t -> t
  (** Change the type. Only works for variables and bound variables. *)

val of_term : LogtkScopedTerm.t -> t option
val of_term_exn : LogtkScopedTerm.t -> t
val is_term : LogtkScopedTerm.t -> bool

val is_var : t -> bool
val is_bvar : t -> bool
val is_tyapp : t -> bool
val is_app : t -> bool
val is_const : t -> bool

(** {2 Sequences} *)

module Seq : sig
  val vars : t -> t Sequence.t
  val subterms : t -> t Sequence.t
  val subterms_depth : t -> (t * int) Sequence.t  (* subterms with their depth *)
  val symbols : t -> LogtkSymbol.t Sequence.t
  val max_var : t Sequence.t -> int     (** max var, or 0 *)
  val min_var : t Sequence.t -> int     (** min var, or 0 *)
  val ty_vars : t -> LogtkType.t Sequence.t
  val typed_symbols : t -> (LogtkSymbol.t * LogtkType.t) Sequence.t

  val add_set : Set.t -> t Sequence.t -> Set.t
end

val var_occurs : var:t -> t -> bool     (** [var_occurs ~var t] true iff [var] in t *)
val is_ground : t -> bool               (** is the term ground? (no free vars) *)
val monomorphic : t -> bool             (** true if the term contains no type var *)
val max_var : Set.t -> int              (** find the maximum variable index, or 0 *)
val min_var : Set.t -> int              (** minimum variable, or 0 if ground *)
val add_vars : unit Tbl.t -> t -> unit  (** add variables of the term to the set *)
val vars : t Sequence.t -> Set.t        (** compute variables of the terms *)
val vars_prefix_order : t -> t list     (** variables in prefix traversal order *)
val depth : t -> int                    (** depth of the term *)
val head : t -> LogtkSymbol.t option         (** head symbol *)
val head_exn : t -> LogtkSymbol.t            (** head symbol (or Invalid_argument) *)
val size : t -> int                     (** Size (number of nodes) *)

val weight : ?var:int -> ?sym:(LogtkSymbol.t -> int) -> t -> int
  (** Compute the weight of a term, given a weight for variables
      and one for symbols.
      @param var unique weight for every variable (default 1)
      @param sym function from symbols to their weight (default [const 1])
      @since 0.5.3 *)

val ty_vars : t -> LogtkType.Set.t
  (** Set of free type variables *)

(** {2 Subterms and LogtkPositions} *)

module Pos : sig
  val at : t -> LogtkPosition.t -> t
  (** retrieve subterm at pos
      @raise Invalid_argument if the position is invalid *)

  val replace : t -> LogtkPosition.t -> by:t -> t
  (** [replace t pos ~by] replaces the subterm at position [pos]
      in [t] by the term [by]. The two terms should have the same type.
      @raise Invalid_argument if the position is not valid *)

  val at_cpos : t -> int -> t
    (** retrieve subterm at the compact pos, or raise Invalid_argument*)

  val max_cpos : t -> int
    (** maximum compact position in the term *)
end

val replace : t -> old:t -> by:t -> t
  (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
      in [t] by the term [by]. *)

(** {2 High-level operations} *)

val symbols : ?init:LogtkSymbol.Set.t -> t -> LogtkSymbol.Set.t
  (** LogtkSymbols of the term (keys of signature) *)

val contains_symbol : LogtkSymbol.t -> t -> bool
  (** Does the term contain this given symbol? *)

(** {2 Fold} *)

(** High level fold-like combinators *)

val all_positions : ?vars:bool -> ?pos:LogtkPosition.t ->
                    t -> 'a ->
                    ('a -> t -> LogtkPosition.t -> 'a) -> 'a
  (** apply f to all non-variable positions in t, accumulating the
      results along.
      [vars] specifies whether variables are folded on (default true). *)

(** {2 Some AC-utils} *)

module type AC_SPEC = sig
  val is_ac : LogtkSymbol.t -> bool
  val is_comm : LogtkSymbol.t -> bool
end

module AC(A : AC_SPEC) : sig
  val flatten : LogtkSymbol.t -> t list -> t list
    (** [flatten_ac f l] flattens the list of terms [l] by deconstructing all its
        elements that have [f] as head symbol. For instance, if l=[1+2; 3+(4+5)]
        with f="+", this will return [1;2;3;4;5], perhaps in a different order *)

  val normal_form : t -> t
    (** normal form of the term modulo AC *)

  val equal : t -> t -> bool
    (** Check whether the two terms are AC-equal. Optional arguments specify
        which symbols are AC or commutative (by default by looking at
        attr_ac and attr_commut). *)

  val symbols : t Sequence.t -> LogtkSymbol.Set.t
    (** Set of symbols occurring in the terms, that are AC *)
end

(** {2 Conversions} *)

val to_simple_term : ?depth:int -> t -> LogtkSTerm.t

(** {2 Printing/parsing} *)

val print_all_types : bool ref
  (** If true, {!pp} will print the types of all annotated terms *)

include LogtkInterfaces.PRINT with type t := t
include LogtkInterfaces.PRINT_DE_BRUIJN with type t := t
    and type term := t

val add_hook : print_hook -> unit
  (** Hook used by default for printing *)

val default_hooks : unit -> print_hook list
  (** List of default hooks *)

(* TODO
include LogtkInterfaces.SERIALIZABLE with type t := t
*)

val debug : Format.formatter -> t -> unit
  (** debug printing, with sorts *)

(** {2 TPTP} *)

module TPTP : sig
  val true_ : t     (** tautology term *)
  val false_ : t    (** antilogy term *)

  include LogtkInterfaces.PRINT with type t := t
  include LogtkInterfaces.PRINT_DE_BRUIJN with type t := t
    and type term := t
    and type print_hook := print_hook

  module Arith : sig
    val floor : t
    val ceiling : t
    val truncate : t
    val round : t

    val prec : t
    val succ : t

    val sum : t
    val difference : t
    val uminus : t
    val product : t
    val quotient : t

    val quotient_e : t
    val quotient_t : t
    val quotient_f : t
    val remainder_e : t
    val remainder_t : t
    val remainder_f : t

    val less : t
    val lesseq : t
    val greater : t
    val greatereq : t

    val arith_hook : print_hook
      (** hook to print arithmetic expressions *)

    val pp_debug : t CCFormat.printer
      (** use arith_hook with pp_debug *)
  end
end

