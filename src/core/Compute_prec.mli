
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
type 'a parametrized = Statement.clause_t Iter.t -> 'a

val add_constr_rule :
  int ->
  [`partial] Precedence.Constr.t parametrized ->
  t -> t
(** Add a precedence constraint rule *)

val set_weight_rule : Precedence.weight_fun parametrized -> t -> t
(** Choose the way weights are computed *)

val add_status : (ID.t * Precedence.symbol_status) list -> t -> t
(** Specify explicitly the status of some symbols *)

(** Parameters db_w and lmb_w correspond to the weight de-Bruijn
    and lambda abstraction given for computation of KBO. *)  
val mk_precedence :
  db_w:int -> lmb_w:int -> signature:Signature.t -> t ->
  Statement.clause_t Iter.t ->
  Precedence.t
(** Make a precedence *)
