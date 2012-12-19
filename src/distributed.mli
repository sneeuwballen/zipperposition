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
val get_get_parents : unit -> net_clause -> net_clause list
val get_get_proof : unit -> net_clause -> net_proof option
val get_publish_redundant : unit -> net_clause list Join.chan
val get_subscribe_redundant : unit -> net_clause list Join.chan -> unit
val get_subscribe_exit : unit -> unit Join.chan -> unit

val mk_queue : unit -> 'a Join.chan * ('a -> unit) Join.chan
  (** Create a queue. It returns a channel to send input objects (type 'a) in,
      and a channel to send a synchronous chan in to register to the queue.
      
      The semantics is that subscribers will receive messages in the correct
      order, and the queue will wait for all recipients to receive a message
      before it processes the next one. *)

val mk_global_queue : string -> 'a Join.chan * ('a -> unit) Join.chan
  (** Same as mk_queue, but registers (send, subscribe) with
      the given global name. *)

(* ----------------------------------------------------------------------
 * utils
 * ---------------------------------------------------------------------- *)

val get_convert_clause : unit -> novel:bool -> hclause -> net_clause
  (** Convert the clause in net_clause, also sending its proof and
      parents to the proof collector if [novel] is true *)
  
(* ----------------------------------------------------------------------
 * components
 * ---------------------------------------------------------------------- *)

