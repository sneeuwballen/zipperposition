
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 State} *)

(** The global state of a proof attempt *)

open Libzipperposition

type term = FOTerm.t
type ty = Type.t
type statement = (Clause.t, term, ty) Statement.t

(** {2 Proofs} *)

module Proof : sig
  type t
end

(** {2 Boolean Literals} *)

module type BOOL_LIT = State_intf.BOOL_LIT with type proof = Proof.t

(** {2 Context for Theories} *)

(** Each theory is given this context, which serves to communicate
    with the SAT solver *)

module type CONTEXT = State_intf.CONTEXT with type proof = Proof.t

type context = (module CONTEXT)

(** {2 Theory} *)

module type THEORY = State_intf.THEORY

module type THEORY_FUN = functor(C:CONTEXT) -> THEORY with module Ctx = C

type theory_fun = (module THEORY_FUN)

(** {2 State} *)

type t

val create :
  conf:Flex_state.t ->
  ord:Ordering.t ->
  signature:Type.t ID.Map.t ->
  theories:theory_fun list ->
  statements:statement CCVector.ro_vector ->
  max_depth:int ->
  unit ->
  t

val context : t -> context

(** {2 Result} *)

type res =
  | Sat
  | Unsat
  | Unknown

val pp_res : res CCFormat.printer

val run : t -> res
(** Main loop. It calls the SAT solver which takes care of
    (trying to) find a model *)

