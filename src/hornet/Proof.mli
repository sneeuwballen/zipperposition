
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proofs} *)

(** Each clause, Horn clause, etc. contains its own proof,
    that is, a derivation from axioms through instanciations/resolution/… *)

open Libzipperposition
open Hornet_types

type t = proof

val from_stmt : Statement.clause_t -> t
val instance : clause -> Subst.t -> t
val avatar_split : clause -> t
val split : clause -> t

val bool_tauto : t
val bool_res : bool_clause -> t -> bool_clause -> t -> t

val hc_sup : Hornet_types.hc_superposition_step -> t
val hc_simplify : horn_clause -> t

include Interfaces.PRINT with type t := t

(** {2 Print Proof DAG} *)

val pp_dag : t CCFormat.printer
