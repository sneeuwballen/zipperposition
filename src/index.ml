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

(** Generic term indexing *)

open Basic

module T = Terms
module Utils = FoUtils

module IntMap = Map.Make(struct
  type t = int
  let compare i j = i - j
end)

(** A leaf maps terms to a set of 'a *)
module Leaf = struct
  type 'a t = {
    map : (term * 'a SmallSet.t) IntMap.t;
    cmp : 'a -> 'a -> int;
  }

  let empty ~cmp =
    { map = IntMap.empty;
      cmp; 
    }

  let add leaf t data =
    let set =
      try snd (IntMap.find t.tag leaf.map)
      with Not_found ->
        SmallSet.empty ~cmp:leaf.cmp in
    let set = SmallSet.add set data in
    let map = IntMap.add t.tag (t, set) leaf.map in
    { leaf with map; }

  let remove leaf t data =
    try
      let t', set = IntMap.find t.tag leaf.map in
      assert (T.eq_term t t');
      let set = SmallSet.remove set data in
      let map = if SmallSet.is_empty set
        then IntMap.remove t.tag leaf.map
        else IntMap.add t.tag (t, set) leaf.map
      in
      { leaf with map; }
    with Not_found ->
      leaf

  let is_empty leaf = IntMap.is_empty leaf.map

  let iter leaf f =
    IntMap.iter (fun _ (t, set) -> f t set) leaf.map

  let fold leaf f acc =
    IntMap.fold (fun _ (t, set) acc -> f acc t set) leaf.map acc

  let size leaf =
    let cnt = ref 0 in
    IntMap.iter (fun _ (_, set) -> cnt := !cnt + SmallSet.size set) leaf.map;
    !cnt

  let fold_unify (leaf,o_leaf) (t,o_t) acc k =
    IntMap.fold
      (fun _ (t', set) acc ->
        try
          let subst = FoUnif.unification FoSubst.id_subst (t',o_leaf) (t,o_t) in
          SmallSet.fold
            (fun acc data -> k acc t' data subst)
            acc set
        with UnificationFailure -> acc)
      leaf.map acc

  let fold_match (leaf,o_leaf) (t,o_t) acc k =
    IntMap.fold
      (fun _ (t', set) acc ->
        try
          let subst = FoUnif.matching FoSubst.id_subst (t',o_leaf) (t,o_t) in
          SmallSet.fold
            (fun acc data -> k acc t' data subst)
            acc set
        with UnificationFailure -> acc)
      leaf.map acc

  let fold_matched (leaf,o_leaf) (t,o_t) acc k =
    IntMap.fold
      (fun _ (t', set) acc ->
        try
          let subst = FoUnif.matching FoSubst.id_subst (t,o_t) (t',o_leaf) in
          SmallSet.fold
            (fun acc data -> k acc t' data subst)
            acc set
        with UnificationFailure -> acc)
      leaf.map acc
end

(** A term index, that contains values of type 'a in its leaves *)
type 'a t =
  <
    name : string ;
    add : term -> 'a -> 'a t ;
    remove: term -> 'a -> 'a t ;
    is_empty : bool ;

    iter : (term -> 'a SmallSet.t -> unit) -> unit ;
    fold : 'b. ('b -> term -> 'a SmallSet.t -> 'b) -> 'b -> 'b ;

    retrieve_unifiables : 'b. offset -> term bind -> 'b ->
                          ('b -> term -> 'a -> substitution -> 'b) -> 'b ;
    retrieve_generalizations : 'b. offset -> term bind -> 'b ->
                                ('b -> term -> 'a -> substitution -> 'b) -> 'b ;
    retrieve_specializations : 'b. offset -> term bind -> 'b ->
                              ('b -> term -> 'a -> substitution -> 'b) -> 'b ;

    pp : ?all_clauses:bool ->
          (Format.formatter -> 'a -> unit) ->
          Format.formatter -> unit -> unit ;
    to_dot : ('a -> string) -> Format.formatter -> unit
      (** print oneself in DOT into the given file *)
  >

(** A subsumption index (non perfect!) *)
type subsumption_t =
  < name : string;
    add : clause -> subsumption_t ;
    remove : clause -> subsumption_t ;
    retrieve_subsuming : literal array -> (hclause -> unit) -> unit ;
    retrieve_subsumed : literal array -> (hclause -> unit) -> unit ;
  >

(** A simplification index *)
type unit_t =
  <
    name : string ;
    maxvar : int ;
    is_empty : bool ;
    add_clause : hclause -> unit_t ;
    remove_clause : hclause -> unit_t ;
    add : term -> term -> bool -> hclause -> unit_t ;
    remove : term -> term -> bool -> hclause -> unit_t ;
    size : int ;
    retrieve : 'a. sign:bool -> offset -> term bind -> 'a ->
              ('a -> term bind -> term bind -> hclause -> substitution -> 'a) -> 'a;
      (** fold on (in)equations of given sign l=r where subst(l) = query term *)

    pp : Format.formatter -> unit -> unit ;
    to_dot : Format.formatter -> unit
      (** print oneself in DOT into the given file *)
  >

(** From an implementation of a general index, make a unit index. *)
let mk_unit_index (idx : (term * hclause) t) =
  assert (idx#is_empty);
  (* constructor of the object *)
  let rec mk_idx (neg : (term * hclause) t) (pos : (term * hclause) t) =
    (object
      method name = "unit" ^ idx#name
      method maxvar = 2000   (* TODO remove, obsolete *)
      method is_empty = neg#is_empty && pos#is_empty

      method add_clause hc =
        match hc.hclits with
        | [|Equation (l,r,true,Gt)|] -> mk_idx neg (pos#add l (r,hc))
        | [|Equation (l,r,true,Lt)|] -> mk_idx neg (pos#add r (l,hc))
        | [|Equation (l,r,true,Incomparable)|]
        | [|Equation (l,r,true,Eq)|] -> (* equal modulo symmetry of =, or incomparable *)
          let pos = (pos#add l (r,hc))#add r (l,hc) in
          mk_idx neg pos
        | [|Equation (l,r,false,_)|] ->
          let neg = (neg#add l (r,hc))#add r (l,hc) in
          mk_idx neg pos
        | _ ->
          mk_idx neg pos  (* do not add other clauses *)

      method remove_clause hc =
        match hc.hclits with
        | [|Equation (l,r,true,Gt)|] -> mk_idx neg (pos#remove l (r,hc))
        | [|Equation (l,r,true,Lt)|] -> mk_idx neg (pos#remove r (l,hc))
        | [|Equation (l,r,true,Incomparable)|]
        | [|Equation (l,r,true,Eq)|] -> (* equal modulo symmetry of =, or incomparable *)
          let pos = (pos#remove l (r,hc))#remove r (l,hc) in
          mk_idx neg pos
        | [|Equation (l,r,false,_)|] ->
          let neg = (neg#remove l (r,hc))#remove r (l,hc) in
          mk_idx neg pos
        | _ -> 
          mk_idx neg pos  (* do not remove anything *)

      method add l r b hc =
        if b
          then mk_idx neg (pos#add l (r,hc))
          else mk_idx (neg#add l (r,hc)) pos

      method remove l r b hc =
        if b
          then mk_idx neg (pos#remove l (r,hc))
          else mk_idx (neg#remove l (r,hc)) pos

      method size =
        let r = ref 0 in
        neg#iter (fun _ set -> r := !r + SmallSet.size set);
        pos#iter (fun _ set -> r := !r + SmallSet.size set);
        !r

      method retrieve :'a. sign:bool -> offset -> term bind -> 'a ->
              ('a -> term bind -> term bind -> hclause -> substitution -> 'a) -> 'a
      = fun ~sign offset (t, o_t) acc k ->
        let handler acc l (r, hc) subst =
          k acc (l,offset) (r,offset) hc subst
        in
        if sign
          then pos#retrieve_generalizations offset (t, o_t) acc handler
          else neg#retrieve_generalizations offset (t, o_t) acc handler

      method pp fmt () =
        let pp_leaf fmt (r,hc) =
          Format.fprintf fmt "@[<h>%a -> %a@]" !T.pp_term#pp r !Clauses.pp_clause#pp_h hc
        in
        Format.fprintf fmt "unit index@ %a@ %a"
          (pos#pp ?all_clauses:None pp_leaf) ()
          (neg#pp ?all_clauses:None pp_leaf) ();
        ()

      method to_dot fmt =
        let to_string (r,hc) =
          FoUtils.sprintf "@[<h>%a -> %a@]" !T.pp_term#pp r !Clauses.pp_clause#pp_h hc
        in
        pos#to_dot to_string fmt;
        neg#to_dot to_string fmt;
        ()
    end :unit_t :> unit_t)
  in mk_idx idx idx

