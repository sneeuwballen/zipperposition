
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement}

    The input problem is made of {b statements}. Each statement can declare
    a type, assert a formula, or a conjecture, define a term, etc.

    Those statements do not necessarily reflect exactly statements in the input
    language(s) (e.g., TPTP). *)

type ('f, 'ty) view =
  | TyDecl of ID.t * 'ty
  | Assert of 'f

type ('f, 'ty, 'meta) t = {
  view: ('f, 'ty) view;
  src: 'meta;
}

type ('f, 'ty) sourced_t = ('f, 'ty, StatementSrc.t) t

type clause = FOTerm.t SLiteral.t list
type clause_t = (clause, Type.t) sourced_t

val view : ('f, 'ty, _) t -> ('f, 'ty) view
val src : (_, _, 'src) t -> 'src

val ty_decl : src:'src -> ID.t -> 'ty -> (_, 'ty, 'src) t
val assert_ : src:'src -> 'f -> ('f, _, 'src) t

val signature : ?init:Signature.t -> (_, Type.t, _) t Sequence.t -> Signature.t
(** Compute signature when the types are using {!Type} *)

val map : form:('f1 -> 'f2) -> ty:('ty1 -> 'ty2) -> ('f1, 'ty1, 'src) t -> ('f2, 'ty2, 'src) t
val map_src : f:('a -> 'b) -> ('f, 'ty, 'a) t -> ('f, 'ty, 'b) t

(** {2 Iterators} *)

module Seq : sig
  val ty_decls : (_, 'ty, _) t -> (ID.t * 'ty) Sequence.t
  val forms : ('f, _, _) t -> 'f Sequence.t
  val lits : (clause, _, _) t -> FOTerm.t SLiteral.t Sequence.t
  val terms : (clause, _, _) t -> FOTerm.t Sequence.t
end

(** {2 IO} *)

val pp : 'a CCFormat.printer -> 'b CCFormat.printer -> ('a,'b,_) t CCFormat.printer
val to_string : 'a CCFormat.printer -> 'b CCFormat.printer -> ('a,'b,_) t -> string

val pp_clause : clause_t CCFormat.printer

module TPTP : sig
  include Interfaces.PRINT2 with type ('a, 'b) t := ('a, 'b) sourced_t
end


