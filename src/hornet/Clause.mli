
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 General representation of Clauses} *)

open Libzipperposition
open Hornet_types

(** An arbitrary clause, typically obtained from the input or by
    instantiation *)
type t = Hornet_types.clause
type clause = t

type idx = Hornet_types.clause_idx

(* constraint on the literals *)
type constraint_ = Hornet_types.c_constraint_ =
  | C_dismatch of Dismatching_constr.t

(** How to build a clause from a ['a] and other parameters *)
type 'a builder =
  ?trail:bool_trail ->
  ?constr:constraint_ list ->
  ?depth:int ->
  'a ->
  proof ->
  t

val make : Lit.t IArray.t builder
val make_l : Lit.t list builder

val proof : t -> proof
val lits : t -> Lit.t IArray.t
val trail : t -> bool_trail
val constr : t -> constraint_ list
val depth : t -> int

val dismatch_constr : t -> Dismatching_constr.t list

val set_select : t -> select_lit -> unit
val clear_select : t -> unit
val select : t -> select_lit option
val select_exn : t -> select_lit

val grounding : t -> bool_lit IArray.t option
val grounding_exn : t -> bool_lit IArray.t
val set_grounding : t -> bool_lit IArray.t -> unit

val is_empty : t -> bool

include Interfaces.EQ with type t := t
include Interfaces.HASH with type t := t
include Interfaces.PRINT with type t := t

val hash_mod_alpha : t -> int

(** The index of a literal in the clause *)

val lits_seq : t -> (idx * Lit.t) Sequence.t

val vars_seq : t -> Type.t HVar.t Sequence.t

val vars_l : t -> Type.t HVar.t list

(** {2 Classification} *)

(** Some clauses are Horn, some are unit equations, some are unit,
    and the others are general *)

type kind =
  | Horn of horn_clause
  | General

val classify : t -> kind

val is_ground : t -> bool
val is_unit_ground : t -> bool
val is_horn : t -> bool

(** {2 Utils} *)

val of_slit_l :
  stmt:Statement.clause_t ->
  FOTerm.t SLiteral.t list ->
  t
(** Conversion from some clause in the given statement *)

val is_trivial : t -> bool
(** Is the clause trivial? *)

val add_dismatch_constr : t -> Dismatching_constr.t -> unit
(** Add a dismatching constraint to the clause
    @raise Util.Error if the clause is Horn (not splitted then) *)

(** {2 Unif} *)

val variant :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

val equal_mod_alpha : t -> t -> bool

module Tbl_mod_alpha : CCHashtbl.S with type key = clause
