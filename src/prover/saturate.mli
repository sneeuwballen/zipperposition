
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Main saturation algorithm.}
    It uses inference rules and simplification rules from Superposition. *)

open Logtk

val check_timeout : float option -> bool
(** check whether we still have some time w.r.t timeout *)

(** The SZS status of a state *)
type szs_status =
  | Unsat of Proof.S.t
  | Sat
  | Unknown
  | Error of string
  | Timeout

module type S = sig
  module Env : Env.S

  val given_clause_step : ?generating:bool -> int -> szs_status
  (** Perform one step of the given clause algorithm.
      It performs generating inferences only if [generating] is true (default);
      other parameters are the iteration number and the environment *)

  val given_clause:
    ?generating:bool -> ?steps:int -> ?timeout:float ->
    unit -> szs_status * int
  (** run the given clause until a timeout occurs or a result
      is found. It returns a tuple (new state, result, number of steps done).
      It performs generating inferences only if [generating] is true (default) *)

  val presaturate : unit -> szs_status * int
  (** Interreduction of the given state, without generating inferences. Returns
      the number of steps done for presaturation, with status of the set. *)
end

module Make(E : Env.S) : S with module Env = E
