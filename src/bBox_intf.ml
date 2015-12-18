
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Interface of BBox} *)

type bool_lit = Bool_lit.t
(** Abstract boolean literal *)

module type TERM = sig
  type t
  val to_term : t -> Logtk.FOTerm.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val pp : t CCFormat.printer
end

module type S = sig
  module I : TERM
  module Case : TERM

  type inductive_cst = I.t
  type inductive_case = Case.t

  type t = Sat_solver.Lit.t

  type injected = private
    | Clause_component of Literals.t
    | Case of inductive_cst * inductive_case  (* [i = t] *)

  val pp_injected : injected CCFormat.printer

  val inject_lits : Literals.t -> t
  (** Inject a clause into a boolean literal. No other clause will map
      to the same literal unless it is alpha-equivalent to this one.
      The boolean literal can be negative is the argument is a
      unary negative clause *)

  val inject_case : inductive_cst -> inductive_case -> t
  (** Inject [cst = case] *)

  val extract : t -> injected option
  (** Recover the value that was injected into the literal, if any
      @raise Failure if the literal is <= 0 *)

  val extract_exn : t -> injected
  (** Recover the value that was injected into the literal
      @raise Failure if the literal is <= 0 of it's not a proper boolean lit *)

  val inductive_cst : t -> inductive_cst option
  (** Obtain the inductive constant from this boolean lit, if any *)

  (** {2 Printers}
      Those printers print the content (injection) of a boolean literal, if any *)

  val pp : t CCFormat.printer
end
