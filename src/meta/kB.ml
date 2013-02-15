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

(** {2 Persistent Knowledge Base} *)

open Types

(* TODO *)

module Utils = FoUtils

type t = Pattern.item list

let empty = []

let add_item kb i = i :: kb

let to_seq kb = Sequence.of_list kb

let of_seq kb items =
  Sequence.fold add_item kb items

let to_json kb : json = `List (List.map Pattern.item_to_json kb)

let of_json kb (json : json) : t =
  let l = Json.Util.to_list json in
  of_seq kb (Sequence.map Pattern.item_of_json (Sequence.of_list l))

let pp formatter kb =
  Utils.pp_list Pattern.pp_item formatter kb 

(** {2 Saving/restoring KB from disk} *)

let save ~file kb =
  let json = to_json kb in
  Json.to_file file json

let restore ~file kb =
  let json = Json.from_file file in
  of_json kb json
