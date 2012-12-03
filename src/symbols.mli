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

(** Symbols and signature *)

type symbol
  (** abstract type of a symbol *)

type sort = symbol
  (** a sort for terms (only the return sort is kept) *)

val compare_symbols : symbol -> symbol -> int
  (** total ordering on symbols (equality is ==) *)

val hash_symbol : symbol -> int
  (** hash the symbol *)

type symbol_attribute = int
  (** attributes of the symbol *)

val attr_skolem : symbol_attribute     (** skolem symbol? *)
val attr_split : symbol_attribute      (** symbol used for splitting? *)

val mk_symbol : ?attrs:symbol_attribute -> string -> symbol
  (** construction of a symbol *)

val attrs_symbol : symbol -> symbol_attribute
  (** access attributes of a symbol *)

val name_symbol : symbol -> string
  (** deconstruction of a symbol *)

val sig_version : int ref
  (** current version of the signature (updated upon symbol creation) *)

module SHashtbl : Hashtbl.S with type key = symbol

(** Hashset for symbols *)
module SHashSet :
  sig
    type t
    val create : unit -> t
    val member : t -> symbol -> bool
    val add : t -> symbol -> unit
    val from_list : symbol list -> t
  end

(* connectives *)
val true_symbol : symbol
val false_symbol : symbol
val eq_symbol : symbol
val exists_symbol : symbol
val forall_symbol : symbol
val lambda_symbol : symbol
val not_symbol : symbol
val imply_symbol : symbol
val and_symbol : symbol
val or_symbol : symbol

(* De Bruijn *)
val db_symbol : symbol
val succ_db_symbol : symbol

(* sorts *)
val bool_sort : sort
val type_sort : sort
val univ_sort : sort

val base_signature : unit -> (sort SHashtbl.t * int SHashtbl.t * symbol list)
  (** the signature composed of predefined symbols *)
