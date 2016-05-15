
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 BBox (Boolean Box)}

    This module defines a way to encapsulate clauses and some meta-level
    properties into boolean literals, and maintains a bijection between
    encapsulated values and boolean literals *)

val section : Libzipperposition.Util.Section.t

type inductive_cst = Ind_cst.cst
type inductive_case = Ind_cst.case

type payload = private
  | Fresh (* fresh literal with no particular payload *)
  | Clause_component of Literals.t
  | Case of inductive_cst * inductive_case  (* [i = t] *)

module Lit : Bool_lit_intf.S with type payload = payload

type t = Lit.t

val dummy : t

val pp_payload : payload CCFormat.printer

val inject_lits : Literals.t -> t
(** Inject a clause into a boolean literal. No other clause will map
    to the same literal unless it is alpha-equivalent to this one.
    The boolean literal can be negative is the argument is a
    unary negative clause *)

val inject_case : inductive_cst -> inductive_case -> t
(** Inject [cst = case] *)

val payload : t -> payload
(** Obtain the payload of this boolean literal, that is, what the literal
    represents *)

val as_case : t -> inductive_path option
(** If [payload t = Case p], then return [Some p], else return [None] *)

val must_be_kept : t -> bool
(** [must_be_kept lit] means that [lit] should survive in boolean splitting,
    that is, that if [C <- lit, Gamma] then any clause derived from [C]
    recursively will have [lit] in its trail. *)

val inductive_cst : t -> inductive_cst option
(** Obtain the inductive constant from this boolean lit, if any *)

(** {2 Printers}
    Those printers print the content (injection) of a boolean literal, if any *)

val pp : t CCFormat.printer

val pp_bclause : t list CCFormat.printer
