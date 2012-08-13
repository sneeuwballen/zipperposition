(* main saturation algorithm *)

open Types

(** the status of a state *)
type szs_status = 
  | Unsat of hclause
  | Sat
  | Unknown
  | Error of string 
  | Timeout

val set_debug : bool -> unit   (** enable debug *)

(** Perform one step of the given clause algorithm *)
val given_clause_step : ProofState.state -> ProofState.state * szs_status

(** run the given clause until a timeout occurs or a result
    is found *)
val given_clause :  ?max_steps:int -> ?timeout:float
                  -> ProofState.state
                  -> ProofState.state * szs_status

