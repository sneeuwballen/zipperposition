(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Interface to MSat} *)

include Sat_solver_intf.S

val create : unit -> t
(** Create a fresh SAT solver instance *)

val set_compact : bool -> unit
(** Toggle compact proofs. if true, collapse internal resolution nodes in proofs
*)

module Make () : Sat_solver_intf.STATIC
(** Backward-compatible generative functor. Creates a fresh solver and binds all
    functions to a hidden internal state. Use {!create} for new code. *)
