(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

val conv : get_clause:(Proof.t -> Literal.t array) -> Proof.t -> LLProof.t
(** Convert a [Proof.t] to [LLProof.t]. [get_clause] extracts the clause
    (literal array) from each proof step. This is typically
    [fun p -> SClause.lits c] where [c] is the SClause stored as the proof
    step's result. *)
