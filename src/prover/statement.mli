
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement}

    The input problem is made of {b statements}. Each statement can declare
    a type, assert a formula, or a conjecture, define a term, etc.

    Those statements do not necessarily reflect exactly statements in the input
    language(s) (e.g., TPTP). *)

open Libzipperposition
open Libzipperposition_parsers

type clause = FOTerm.t SLiteral.t list

type view =
  | TyDecl of ID.t * Type.t
  | Assert of clause

type t = {
  view: view;
  src: StatementSrc.t;
}

val view : t -> view
val src : t -> StatementSrc.t

val ty_decl : src:StatementSrc.t -> ID.t -> Type.t -> t
val assert_ : src:StatementSrc.t -> clause -> t

val of_cnf_tptp : file:string -> (Ast_tptp.role * string) Cnf.statement -> t
(** Conversion from a CNF statement *)

val signature : ?init:Signature.t -> t Sequence.t -> Signature.t
(** Compute signature *)

(** {2 Iterators} *)

module Seq : sig
  val ty_decls : t -> (ID.t * Type.t) Sequence.t
  val clauses : t -> clause Sequence.t
  val lits : t -> FOTerm.t SLiteral.t Sequence.t
  val terms : t -> FOTerm.t Sequence.t
end

(** {2 IO} *)

include Interfaces.PRINT with type t := t

module TPTP : sig
  include Interfaces.PRINT with type t := t
end


