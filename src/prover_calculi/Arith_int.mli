
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Cancellative Inferences on Integer Arithmetic}

    Superposition, chaining and modulo reasoning for linear expressions, with
    congruence classes of terms and literals. Inferences are typically done with
    "scaled" literals, i.e. literals that are multiplied by numeric coefficients so
    as to bring the unified terms to the same coefficient. *)
open Libzipperposition

val case_switch_limit : int ref
(** Positive integer: maximum width of an inequality case switch. Default: 30 *)

val div_case_switch_limit : int ref
(** Positive integer: maximum prime number suitable for div_case_switch
    (ie maximum n for enumeration of cases in n^k | x) *)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  (** {3 Equations and Inequations} *)

  val canc_sup_active: Env.binary_inf_rule
  (** cancellative superposition where given clause is active *)

  val canc_sup_passive: Env.binary_inf_rule
  (** cancellative superposition where given clause is passive *)

  val cancellation: Env.unary_inf_rule
  (** cancellation (unifies some terms on both sides of a
      comparison operator) *)

  val canc_equality_factoring: Env.unary_inf_rule
  (** cancellative equality factoring *)

  val canc_ineq_chaining : Env.binary_inf_rule
  (** cancellative inequality chaining.

      Also does case switch if conditions are present:
          C1 or a < b     C2 or b < c
      -------------------------------------
          C1 or C2 or or_{i=a+1....c-1} (b = i)
      if a and c are integer linear expressions whose difference is
      a constant. If a > c, then the range a...c is empty and the literal
      is just removed. *)

  val canc_ineq_factoring : Env.unary_inf_rule
  (** Factoring between two inequation literals *)

  val canc_less_to_lesseq : Env.lit_rewrite_rule
  (** Simplification:  a < b  ----> a+1 ≤ b *)

  (** {3 Divisibility} *)

  val canc_div_chaining : Env.binary_inf_rule
  (** Chain together two divisibility literals, assuming they share the
      same prime *)

  val canc_div_case_switch : Env.unary_inf_rule
  (** Eliminate negative divisibility literals within a power-of-prime
      quotient of Z:
      not (d^i | m) -----> *)

  val canc_div_prime_decomposition : Env.multi_simpl_rule
  (** Eliminate divisibility literals with a non-power-of-prime
      quotient of Z (for instance  [6 | a ---> { 2 | a, 3 | a }]) *)

  val canc_divisibility : Env.unary_inf_rule
  (** Infer divisibility constraints from integer equations,
      for instace   C or  2a=b ---->  C or 2 | b    if a is maximal *)

  (** {3 Other} *)

  val is_tautology : C.t -> bool
  (** is the clause a tautology w.r.t linear expressions? *)

  val eliminate_unshielded : Env.multi_simpl_rule
  (** Eliminate unshielded variables using an adaptation of
      Cooper's algorithm *)

  (** {2 Contributions to Env} *)

  val register : unit -> unit
end

module Make(E : Env.S) : S with module Env = E

val extension : Extensions.t
