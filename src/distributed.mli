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
  nc_selected_done : bool;          (** are literals already selected? *)
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

val ddebug : int -> string -> string Lazy.t -> unit
  (** Process-localized debug. A level must be provided, and also a context
      name that indicates which component is outputing the message. *)

val socketname : Unix.sockaddr
  (** name for the communication socket *)

val mk_queue : unit -> ('a -> unit) * (('a -> unit) -> unit)
  (** Create a queue. It returns a channel to send input objects (type 'a) in,
      and a channel to send a synchronous chan in to register to the queue.
      
      The semantics is that subscribers will receive messages in the correct
      order, and the queue will wait for all recipients to receive a message
      before it processes the next one. *)

val mk_global_queue : ns:Join.Ns.t -> string -> ('a -> unit) * (('a -> unit) -> unit)
  (** Same as mk_queue, but registers (send, subscribe) with
      the given global name. *)

val get_queue : ns:Join.Ns.t -> string -> ('a -> unit) * (('a -> unit) -> unit)
  (** Get a handle on the remote queue by name *)

val mk_barrier : ns:Join.Ns.t -> string -> int -> (string -> unit)
  (** Make a global barrier to synchronize [n] processes. The arguments
      are the (global) name, and the number of processes involved. The
      semantics is: [mk_barrier name n] registers a barrier on given name;
      It returns a function sync which is blocking until [n] calls to [sync name]
      have been made. Then [sync] is ready for [n] other calls, and so on. *)

val get_barrier : ns:Join.Ns.t -> string -> (string -> unit)
  (** Get the barrier registered under this name *)

(* ----------------------------------------------------------------------
 * utils
 * ---------------------------------------------------------------------- *)

(** Access to global utils *)
type globals =
  < subscribe_redundant: (net_clause list -> unit) -> unit;
    publish_redundant: net_clause list -> unit;
    subscribe_exit: (unit -> unit) -> unit;
    publish_exit: unit -> unit;
    convert: novel:bool -> hclause list -> net_clause list;
    convert_one: novel:bool -> hclause -> net_clause;
    get_descendants: net_clause -> net_clause list;
    send_result: net_clause Saturate.szs_status * int -> unit;
    sync_barrier: string -> unit;
  >

val proof_parents_process : unit ->
                             ((net_clause list * net_clause list list * net_proof list) -> unit) *
                             (net_clause -> net_clause list) *
                             (net_clause -> net_proof option)
  (** The process responsible for keeping track of
      parents and proofs of clauses *)

val setup_globals : (net_clause Saturate.szs_status * int -> unit) -> int -> globals
  (** Setup global components in this process. Also setup the network server.
      The parameter is the chan to send results on *)

val get_globals : ns:Join.Ns.t -> string -> globals
  (** Create a globals object, using (possibly remote) components *)
  
(* ----------------------------------------------------------------------
 * components
 * ---------------------------------------------------------------------- *)

val fwd_rewrite_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: (net_state -> unit) ->
                          globals:globals -> Index.unit_index ->
                          (net_state -> unit) * (unit -> unit)

val fwd_active_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: (net_state -> unit) ->
                          globals:globals -> Index.index -> signature ->
                          (net_state -> unit) * (unit -> unit)

val bwd_subsume_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: (net_state -> unit) ->
                          globals:globals -> Index.index -> signature ->
                          (net_state -> unit) * (unit -> unit)

val bwd_rewrite_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: (net_state -> unit) ->
                          globals:globals -> Index.unit_index -> Index.index ->
                          signature ->
                          (net_state -> unit) * (unit -> unit)

val generate_process : calculus:Calculus.calculus -> select:selection_fun ->
                      ord:ordering -> output: (net_state -> unit) ->
                      globals:globals -> Index.index -> signature ->
                      (net_state -> unit) * (unit -> unit)

val post_rewrite_process : calculus:Calculus.calculus -> select:selection_fun ->
                          ord:ordering -> output: (net_state -> unit) ->
                          globals:globals -> Index.unit_index ->
                          (net_state -> unit) * (unit -> unit)

val pipeline_capacity : int ref
  (** Maximum number of clauses to be sent down the pipeline at the same time *)

val passive_process : calculus:Calculus.calculus -> select:selection_fun ->
                      ord:ordering -> output: (net_state -> unit) ->
                      globals:globals -> 
                      ?steps:int -> ?timeout:float ->
                      (ClauseQueue.queue * int) list ->
                      net_clause list ->
                      (net_state -> unit) * (unit -> unit)

(* ----------------------------------------------------------------------
 * pipeline setup and wiring
 * ---------------------------------------------------------------------- *)

val assume_role : calculus:Calculus.calculus ->
                  select:selection_fun -> ord:ordering ->
                  string -> unit
  (** Assume the given role.
      The role is a string "role,input_name,output_name,name,bool" where
      the bool is used to choose whether to create the output or not *)

val given_clause: ?parallel:bool -> ?steps:int -> ?timeout:float ->
                  ?progress:bool ->
                  calculus:Calculus.calculus ->
                  params:Params.parameters ->
                  ProofState.state ->
                  hclause Saturate.szs_status * int
  (** run the given clause until a timeout occurs or a result
      is found. It returns the result. *)
