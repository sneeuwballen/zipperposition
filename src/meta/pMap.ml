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

(** {2 Indexing on patterns} *)

(* TODO make it efficient *)

open Types

type +'a t = (Pattern.pattern * 'a) list
  (** the type of the map that has values of type 'a *)

let empty = []

let add p value l = (p, value) :: l

let fold f l acc =
  List.fold_left (fun acc (p,value) -> f p value acc) acc l

(** [retrieve map lits k] calls [k] on every list [l] of terms
    such that [l = matching lits p] for some [p] that is a key of [map].
    [k] receives as arguments the pattern [p], the arguments [l]
    and the value associated to [p] *)
let retrieve l lits k =
  List.iter
    (fun (p, value) ->
      let lists = Pattern.matching p lits in
      Sequence.iter
        (fun term_list -> k p term_list value)
        lists)
    l

let to_seq t = Sequence.of_list t

let of_seq t seq =
  List.rev_append (Sequence.to_list seq) t

let to_json f t : json =
  `List (List.map (fun (p,v) -> `List [Pattern.pattern_to_json p; f v]) t) 

let of_json f idx (json : json) =
  let l = Json.Util.to_list json in
  let l' = List.map
    (function
    | `List [p;v] ->
      let p = Pattern.pattern_of_json p
      and v = f v in
      p, v
    | _ -> raise (Json.Util.Type_error ("expected pattern map", json)))
    l
  in List.rev_append l' idx

let pp f formatter t = 
  Format.fprintf formatter "{%a}"
    (FoUtils.pp_list
      (fun formatter (p,v) -> Format.fprintf formatter
        "@[<h>%a -> %a@]" Pattern.pp_pattern p f v))
    t
