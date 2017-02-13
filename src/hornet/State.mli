
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 State} *)

(** The global state of a proof attempt *)

open Libzipperposition
open Hornet_types

type term = FOTerm.t
type ty = Type.t
type statement = (Clause.t, term, ty) Statement.t

(** {2 Context for Theories} *)

(** Each theory is given this context, which serves to communicate
    with the SAT solver *)

module type CONTEXT = State_intf.CONTEXT

type context = (module CONTEXT)

(** {2 Theory} *)

module type THEORY = State_intf.THEORY
module type THEORY_FUN = State_intf.THEORY_FUN

type theory_fun = State_intf.theory_fun

(** {2 State} *)

type t

module type ARGS = State_intf.ARGS

val create : (module ARGS) -> t

val context : t -> context

(** {2 Result} *)

type res =
  | Sat
  | Unsat of proof_with_res
  | Unknown

val pp_res : res CCFormat.printer

val run : t -> res
(** Main loop. It calls the SAT solver which takes care of
    (trying to) find a model *)

