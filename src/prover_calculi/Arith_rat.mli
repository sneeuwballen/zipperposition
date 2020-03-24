
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Cancellative Inferences on Rational Arithmetic}

    Following Uwe Waldmann's work.

    Superposition, chaining and modulo reasoning for linear expressions, with
    congruence classes of terms and literals. Inferences are typically done with
    "scaled" literals, i.e. literals that are multiplied by numeric coefficients so
    as to bring the unified terms to the same coefficient.
*)
open Libzipperposition

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  (** {2 Contributions to Env} *)

  val register : unit -> unit
end

module Make(E : Env.S) : S with module Env = E

val extension : Extensions.t

