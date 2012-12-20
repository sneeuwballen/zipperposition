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

open Types
open Symbols

(** Module for distributed (pipelined) given clause *)

(* ----------------------------------------------------------------------
 * basics
 * ---------------------------------------------------------------------- *)

type net_symbol = string
type net_sort = string

type net_term =
  | NVar of int * net_sort
  | NNode of net_symbol * net_sort * net_term list

(** A clause that can be sent through the network *)
type net_clause = {
  nc_maxlits : int;                 (** bitvector of maximal literals *)
  nc_selected : int;                (** bitvector of selected literals *)
  nc_lits : net_literal array;      (** array of literals *)
}
(** A serializable literal *)
and net_literal =
  | NEquation of net_term * net_term * bool * comparison

(** A proof that can be sent through the network (some loss of information)*)
type net_proof =
  | NpAxiom of string * string
  | NpProof of string * (net_clause * position) list

(** A state, threaded through the network *)
type net_state = {
  ns_num : int;                     (** unique timestamp of the state *)
  ns_ord : net_symbol list option;  (** update to new precedence, if needed *)
  ns_given : net_clause option;     (** the current given clause, if still useful *)
  ns_simplified : net_clause list;  (** clauses simplified, to remove from active set *)
  ns_new : net_clause list;         (** new clauses produced with given clause *)
}

(* ----------------------------------------------------------------------
 * access to global variables
 * ---------------------------------------------------------------------- *)

val socketname : Unix.sockaddr
  (** name for the communication socket *)

val ddebug : int -> string Lazy.t -> unit
  (** Process-localized debug *)

val get_add_parents : unit -> (net_clause * net_clause list) Join.chan
val get_add_proof : unit -> (net_clause * net_proof) Join.chan
val get_get_descendants : unit -> net_clause -> net_clause list
val get_get_proof : unit -> net_clause -> net_proof option
val get_publish_redundant : unit -> net_clause list Join.chan
val get_subscribe_redundant : unit -> net_clause list Join.chan -> unit
val get_subscribe_exit : unit -> unit Join.chan -> unit
val get_send_result : unit -> net_clause Saturate.szs_status -> unit

val mk_queue : unit -> 'a Join.chan * (('a -> unit) -> unit)
  (** Create a queue. It returns a channel to send input objects (type 'a) in,
      and a channel to send a synchronous chan in to register to the queue.
      
      The semantics is that subscribers will receive messages in the correct
      order, and the queue will wait for all recipients to receive a message
      before it processes the next one. *)

val mk_global_queue : string -> 'a Join.chan * (('a -> unit) -> unit)
  (** Same as mk_queue, but registers (send, subscribe) with
      the given global name. *)

val get_queue : string -> 'a Join.chan * (('a -> unit) -> unit)
  (** Get a handle on the remote queue by name *)

(* ----------------------------------------------------------------------
 * utils
 * ---------------------------------------------------------------------- *)

(** Access to global utils *)
type globals =
  < subscribe_redundant: (net_clause list -> unit) -> unit;
    publish_redundant: net_clause list Join.chan;
    subscribe_exit: (unit -> unit) -> unit;
    publish_exit: unit Join.chan;
    convert: novel:bool -> hclause -> net_clause;
    get_descendants: net_clause -> net_clause list;
    send_result: net_clause Saturate.szs_status -> unit;
  >

val proof_parents_process : unit ->
                             (net_clause * net_clause list) Join.chan *
                             (net_clause * net_proof) Join.chan *
                             (net_clause -> net_clause list) *
                             (net_clause -> net_proof option)
  (** The process responsible for keeping track of
      parents and proofs of clauses *)

val connect : (net_state -> unit) -> string list -> unit
  (** connects the given input to queues whose names are in the list. *)

val setup_globals : (net_clause Saturate.szs_status -> unit) -> globals
  (** Setup global components in this process. Also setup the network server.
      The parameter is the chan to send results on *)

val get_globals : unit -> globals
  (** Create a globals object, using (possibly remote) components *)
  
(* ----------------------------------------------------------------------
 * components
 * ---------------------------------------------------------------------- *)

val fwd_rewrite_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: net_state Join.chan ->
                          globals:globals -> Index.unit_index ->
                          (net_state -> unit)

val fwd_active_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: net_state Join.chan ->
                          globals:globals -> Index.index -> signature ->
                          (net_state -> unit)

val bwd_subsume_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: net_state Join.chan ->
                          globals:globals -> Index.index -> signature ->
                          (net_state -> unit)

val bwd_rewrite_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: net_state Join.chan ->
                          globals:globals -> Index.unit_index -> Index.index ->
                          signature -> (net_state -> unit)

val generate_process : calculus:Calculus.calculus -> select:selection_fun ->
                      ord:ordering -> output: net_state Join.chan ->
                      globals:globals -> Index.index ->
                      (net_state -> unit)

val post_rewrite_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: net_state Join.chan ->
                          globals:globals -> Index.unit_index ->
                          (net_state -> unit)

val pipeline_capacity : int ref
  (** Maximum number of clauses to be sent down the pipeline at the same time *)

val passive_process : calculus:Calculus.calculus -> select:selection_fun ->
                      ord:ordering -> output: net_state Join.chan ->
                      globals:globals -> 
                      ?step:int -> ?timeout:float ->
                      (ClauseQueue.queue * int) list ->
                      net_clause list ->
                      (net_state -> unit)

val layout_one_process : calculus:Calculus.calculus -> select:selection_fun ->
                        ord:ordering ->
                        send_result: (net_clause Saturate.szs_status -> unit) ->
                        ?step:int -> ?timeout:float -> net_clause list ->
                        (ClauseQueue.queue * int) list ->
                        unit
  (** Create a pipeline within the same process, without forking *)

val layout_standard : calculus:Calculus.calculus -> select:selection_fun ->
                      ord:ordering ->
                      send_result: (net_clause Saturate.szs_status -> unit) ->
                      ?step:int -> ?timeout:float -> net_clause list ->
                      (ClauseQueue.queue * int) list ->
                      unit
  (** Create a pipeline with several forked processes *)

val given_clause: ?parallel:bool -> ?steps:int -> ?timeout:float ->
                  ?progress:bool ->
                  calculus:Calculus.calculus ->
                  select:selection_fun -> ord:ordering ->
                  hclause list ->
                  hclause Saturate.szs_status
  (** run the given clause until a timeout occurs or a result
      is found. It returns the result. *)
