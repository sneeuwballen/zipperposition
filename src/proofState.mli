(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** The state of a proof, contains a set of active clauses (processed),
    a set of passive clauses (to be processed), and an ordering
    that is used for redundancy elimination. *)

open Types
open Symbols

(** association name -> index *)
val choose_index : string -> Index.index
(** list of index names *)
val names_index : unit -> string list

(** set of active clauses *)
type active_set =
  < ctx : context;
    clauses : Clauses.CSet.t;           (** set of active clauses *)
    idx_sup_into : Index.index;         (** index for superposition into the set *)
    idx_sup_from : Index.index;         (** index for superposition from the set *)
    idx_back_demod : Index.index;       (** index for backward demodulation/simplifications *)
    idx_fv : FeatureVector.fv_index;    (** index for subsumption *)

    add : hclause list -> unit;         (** add clauses *)
    remove : hclause list -> unit;      (** remove clauses *)
  >

(** set of simplifying (unit) clauses *)
type simpl_set =
  < ctx : context;
    idx_simpl : Index.unit_index;       (** index for forward simplifications TODO split into pos-orientable/others *)

    add : hclause list -> unit;
    remove : hclause list -> unit;
  >

(** set of passive clauses *)
type passive_set =
  < ctx : context;
    clauses : Clauses.CSet.t;           (** set of clauses *)
    queues : (ClauseQueue.queue * int) list;

    add : hclause list -> unit;         (** add clauses *)
    remove : int -> unit;               (** remove clause by ID *)
    next : unit -> hclause option;      (** next passive clause, if any *)
    clean : unit -> unit;               (** cleanup internal queues *)
  >


(** state of a superposition calculus instance.
    It contains a set of active clauses, a set of passive clauses,
    and is parametrized by an ordering. *)
type state =
  < ctx : context;
    simpl_set : simpl_set;              (** index for forward demodulation *)
    active_set : active_set;            (** active clauses *)
    passive_set : passive_set;          (** passive clauses *)
    meta_prover : Theories.meta_prover option;
  >

val mk_active_set : ctx:context -> Index.index -> signature -> active_set
val mk_simpl_set : ctx:context -> Index.unit_index -> simpl_set
val mk_passive_set : ctx:context -> (ClauseQueue.queue * int) list -> passive_set

(** create a state from the given ordering, and parameters *)
val mk_state : ctx:context -> ?meta:Theories.meta_prover ->
               Params.parameters -> signature -> state

(** statistics on the state (num active, num passive) *)
type state_stats = int * int
val stats : state -> state_stats

(** pretty print the content of the state *)
val pp_state : Format.formatter -> state -> unit
(** debug functions: much more detailed printing *)
val debug_state : Format.formatter -> state -> unit
