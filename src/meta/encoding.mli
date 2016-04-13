
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Encoding of clauses} *)

open Libzipperposition

type 'a printer = Format.formatter -> 'a -> unit

(** {2 Base definitions} *)

type 'a lit =
  | Eq of 'a * 'a * bool
  | Prop of 'a * bool
  | Bool of bool

val fmap_lit : ('a -> 'b) -> 'a lit -> 'b lit

type 'a clause = 'a lit list

val fmap_clause : ('a -> 'b) -> 'a clause -> 'b clause

type foterm = FOTerm.t
type hoterm = TypedSTerm.t

type foclause = foterm clause
type hoclause = hoterm clause

val foclause_of_clause : FOTerm.t SLiteral.t list -> foclause
(** @raise Invalid_argument if the argument is not a proper clause *)

val clause_of_foclause : foclause -> FOTerm.t SLiteral.t list
(** Convert back to a list of formulas
    @since 0.8 *)

val pp_clause : 'a printer -> 'a clause printer
(** Printer of clauses *)

(** {6 Encoding abstraction} *)

class type ['a, 'b] t = object
  method encode : 'a -> 'b
  method decode : 'b -> 'a option
end

val id : ('a,'a) t
(** Identity encoding *)

val compose : ('a,'b) t -> ('b, 'c) t -> ('a, 'c) t
(** Compose two encodings together *)

val (>>>) : ('a,'b) t -> ('b, 'c) t -> ('a, 'c) t
(** Infix notation for composition *)

(** {6 Currying} *)

val currying : (foterm clause, hoterm clause) t

(** {6 Clause encoding}

    Encode the whole clause into a {!Reasoner.Property.t}, ie a higher-order term
    that represents a meta-level property. *)

module EncodedClause : sig
  type t = private Reasoner.term

  include Interfaces.PRINT with type t := t
  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  val __magic : hoterm -> t
  (** Don't use unless you know what you're doing. *)
end

val clause_prop : (hoclause, EncodedClause.t) t

