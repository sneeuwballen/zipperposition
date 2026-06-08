(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Clause} *)

open Logtk

type bool_lit = BBox.Lit.t
type t = bool_lit list

val proof_tc : t Proof.Result.tc
val mk_proof_res : t -> Proof.Result.t

type Proof.result_view += Bool_clause_view of t

val proof_res_as_bc : Proof.Result.t -> t option
