
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proofs} *)

(** Each clause, Horn clause, etc. contains its own proof,
    that is, a derivation from axioms through instanciations/resolution/… *)

open Libzipperposition

type t = Hornet_types.proof
type clause = Hornet_types.clause
type bool_atom = Hornet_types.bool_atom

val from_stmt : Statement.clause_t -> t
val instance : clause -> Subst.t -> t
val avatar_split : clause -> t
val split : clause -> t

(* TODO: hc proofs *)

include Interfaces.PRINT with type t := t
