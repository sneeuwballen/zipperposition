
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Compute Precedence} *)

open Libzipperposition

type t

val create : unit -> t

val add_constr : t -> int -> [`partial] Precedence.Constr.t -> unit
(** Add a precedence constraint with its priority. The lower the
    priority, the stronger influence the constraint will have. *)

val add_constrs : t -> (int * [`partial] Precedence.Constr.t) list -> unit

(** Some values are parametrized by the list of statements *)
type 'a parametrized = Statement.clause_t Sequence.t -> 'a

val add_constr_rule :
  t ->
  int ->
  [`partial] Precedence.Constr.t parametrized ->
  unit
(** Add a precedence constraint rule *)

val set_weight_rule : t -> (ID.t -> int) parametrized -> unit
(** Choose the way weights are computed *)

val add_status : t -> (ID.t * Precedence.symbol_status) list -> unit
(** Specify explicitely the status of some symbols *)

val mk_precedence :
  t ->
  Statement.clause_t Sequence.t ->
  Precedence.t
(** Make a precedence *)
