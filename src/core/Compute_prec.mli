
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Compute Precedence} *)

(** This module computes precedences that satisfy a list of
    constraints.
    See {!Precedence.Constr} for more details on constraints. *)

type t

val empty : t

val add_constr : int -> [`partial] Precedence.Constr.t -> t -> t
(** Add a precedence constraint with its priority. The lower the
    priority, the stronger influence the constraint will have. *)

val add_constrs : (int * [`partial] Precedence.Constr.t) list -> t -> t

(** Some values are parametrized by the list of statements *)
type 'a parametrized = Statement.clause_t Sequence.t -> 'a

val add_constr_rule :
  int ->
  [`partial] Precedence.Constr.t parametrized ->
  t -> t
(** Add a precedence constraint rule *)

val set_weight_rule : Precedence.weight_fun parametrized -> t -> t
(** Choose the way weights are computed *)

val add_status : (ID.t * Precedence.symbol_status) list -> t -> t
(** Specify explicitely the status of some symbols *)

val mk_precedence :
  t ->
  Statement.clause_t Sequence.t ->
  Precedence.t
(** Make a precedence *)
