
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inference and simplification rules for Algebraic types} *)

open Libzipperposition

type term = FOTerm.t

(** {2 Inference rules} *)

(* TODO: rename and make this a calculus on Ind_types (and Ind_cst), with:

  - exhaustivity (inference):
    if some term [t : tau] is maximal in a clause, [tau] is inductive,
    and [t] was never split on, then introduce
    [t = c1(...) or t = c2(...) or ... or t = ck(...)] where the [ci] are
    constructors of [tau], and [...] are new Skolems of [t];
    if [t] is ground then Avatar splitting (with xor) should apply directly
      instead, as an optimization, with [k] unary clauses and 1 bool clause

  - disjointness (simplification):
    * an equation [c1(...) = c2(...)] becomes false;
    * a disequation [c1(...) != c2(...)] becomes true

  - injectivity (simplification):
    * an equation [c(t1...tn) = c(u1...un)] simplifies into
      [t1 = u1 & ... & tn = un]
    * a disequation [c(t1...tn) != c(u1...un)] simplifies into
      [t1 != u1 || ... || tn != un] (XXX is it really needed?)

  XXX actually this is less specific.
  Keep this as "enum types" and add inductive-specific rules for
  disjointness. Inductive types should be declared as enum types
    and get their own rules (exhaustivity + disjointness)
*)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  val declare_type : proof:C.t ProofStep.of_ -> ty:Type.t -> var:Type.t HVar.t -> term list -> unit
  (** Declare that the given type's domain is the given list of cases
      for the given variable [var] (whose type must be [ty].
      Will be ignored if the type already has a enum declaration. *)

  val instantiate_vars : Env.multi_simpl_rule
  (** Instantiate variables whose type is a known enumerated type,
      with all variables of this type. *)

  (** {6 Registration} *)

  val register : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E

(** {2 As Extension} *)

val extension : Extensions.t
