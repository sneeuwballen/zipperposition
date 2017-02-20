
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Proofs} *)

(** Each clause, Horn clause, etc. contains its own proof,
    that is, a derivation from axioms through instanciations/resolution/… *)

open Libzipperposition
open Hornet_types

type t = proof

val trivial : t
val from_stmt : Statement.clause_t -> t
val instance : clause -> Subst.t -> t
val avatar_split : clause -> t
val split : clause -> select_lit -> c_constraint -> t
val avatar_cut : horn_clause -> (bool_lit * proof) list -> t

val bool_tauto : t
val bool_res : bool_lit -> bool_clause -> t -> bool_clause -> t -> t
val bool_grounding : clause -> t

val hc_sup : Hornet_types.hc_superposition_step -> t
val hc_eq_res : horn_clause -> Subst.t -> t
val hc_simplify : horn_clause -> t
val hc_demod : horn_clause -> horn_clause list -> t

include Interfaces.PRINT with type t := t

val name : t -> string
(** Name of the rule, no other info *)

val parents : t -> proof_with_res list
(** Immediate parents of this proof step, i.e. the subproofs it uses *)

val get : ?compress:bool -> t -> string * proof_with_res list
(** Get name and parent
    @param compress if true, skip some uninteresting steps *)


