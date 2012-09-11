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

open Types
open Hashcons

module T = Terms
module C = Clauses
module Utils = FoUtils

type data = hclause * position * foterm

(** a set of (hashconsed clause, position in clause, term). *)
module ClauseSet : Set.S with type elt = data
  = Set.Make(
      struct 
      type t = data

      let compare (c1, p1, t1) (c2, p2, t2) = 
        let c = Pervasives.compare p1 p2 in
        if c <> 0 then c else
        let c = C.compare_hclause c1 c2 in
        if c <> 0 then c else
        (assert (T.eq_foterm t1 t2); 0)
    end)

(** a leaf of an index is generally a map of terms to data *)
type index_leaf = (foterm * ClauseSet.t) Ptmap.t

let empty_leaf = Ptmap.empty

let add_leaf leaf t data =
  let set =
    try snd (Ptmap.find t.tag leaf)
    with Not_found -> ClauseSet.empty in
  let set = ClauseSet.add data set in
  Ptmap.add t.tag (t, set) leaf

let remove_leaf leaf t data =
  try
    let t', set = Ptmap.find t.tag leaf in
    assert (T.eq_foterm t t');
    let set = ClauseSet.remove data set in
    if ClauseSet.is_empty set
      then Ptmap.remove t.tag leaf
      else Ptmap.add t.tag (t, set) leaf
  with Not_found -> leaf

let is_empty_leaf leaf = Ptmap.is_empty leaf

let iter_leaf leaf f =
  Ptmap.iter (fun _ (t, set) -> f t set) leaf

let fold_leaf leaf f acc =
  Ptmap.fold (fun _ (t, set) acc -> f acc t set) leaf acc

let size_leaf leaf =
  let cnt = ref 0 in
  Ptmap.iter (fun _ _ -> incr cnt) leaf;
  !cnt

(** A term index *)
class type index =
  object ('b)
    method name : string
    method add : foterm -> data -> 'b
    method remove: foterm -> data -> 'b

    method iter : (foterm -> ClauseSet.t -> unit) -> unit
    method fold : 'a. ('a -> foterm -> ClauseSet.t -> 'a) -> 'a -> 'a

    method retrieve_unifiables : 'a. foterm -> 'a ->
                                 ('a -> foterm -> ClauseSet.t -> 'a) -> 'a
    method retrieve_generalizations : 'a. foterm -> 'a ->
                                      ('a -> foterm -> ClauseSet.t -> 'a) -> 'a
    method retrieve_specializations : 'a. foterm -> 'a ->
                                      ('a -> foterm -> ClauseSet.t -> 'a) -> 'a

    method pp : all_clauses:bool -> Format.formatter -> unit -> unit
  end

(** A global index, that operates on hashconsed clauses *)
class type clause_index =
  object ('a)
    method index_clause : hclause -> 'a
    method remove_clause : hclause -> 'a

    method root_index : index
    method unit_root_index : index
    method ground_rewrite_index : (foterm * data) Ptmap.t (** to rewrite ground terms *)
    method subterm_index : index

    method pp : all_clauses:bool -> Format.formatter -> unit -> unit
  end


(** process the literal (only its maximal side(s)) *)
let process_lit op c tree (lit, pos) =
  match lit with
  | Equation (l,_,_,Gt) -> 
      op tree l (c, [C.left_pos; pos])
  | Equation (_,r,_,Lt) -> 
      op tree r (c, [C.right_pos; pos])
  | Equation (l,r,_,Incomparable)
  | Equation (l,r,_,Invertible) ->
      let tmp_tree = op tree l (c, [C.left_pos; pos]) in
      op tmp_tree r (c, [C.right_pos; pos])
  | Equation (l,r,_,Eq) ->
    Utils.debug 4 (lazy (Utils.sprintf "add %a = %a to index"
                   !T.pp_term#pp l !T.pp_term#pp r));
    op tree l (c, [C.left_pos; pos])  (* only index one side *)

(** apply op to the maximal literals of the clause, and only to
    the maximal side(s) of those. *)
let process_clause op tree c =
  (* index literals with their position *)
  let lits_pos = Utils.list_pos c.clits in
  let new_tree = List.fold_left (process_lit op c) tree lits_pos in
  new_tree

(** apply (op tree) to all subterms, folding the resulting tree *)
let rec fold_subterms op tree t (c, path) = match t.term with
  | Var _ -> tree  (* variables are not indexed *)
  | Leaf _ -> op tree t (c, List.rev path, t) (* reverse path now *)
  | Node (_::l) ->
      (* apply the operation on the term itself *)
      let tmp_tree = op tree t (c, List.rev path, t) in
      let _, new_tree = List.fold_left
        (* apply the operation on each i-th subterm with i::path
           as position. i starts at 1 and the function symbol is ignored. *)
        (fun (idx, tree) t -> idx+1, fold_subterms op tree t (c, idx::path))
        (1, tmp_tree) l
      in new_tree
  | _ -> assert false

(** apply (op tree) to the root term, after reversing the path *)
let apply_root_term op tree t (c, path) = op tree t (c, List.rev path, t)

(** size of a Ptmap *)
let ptmap_size m =
  let size = ref 0 in
  Ptmap.iter (fun _ _ -> incr size) m;
  !size

let mk_clause_index (index : index) =
  object (_: 'self)
    val _root_index = index
    val _subterm_index = index
    val _unit_root_index = index 
    val _ground_rewrite_index = Ptmap.empty

    (** add root terms and subterms to respective indexes *)
    method index_clause hc =
      let op tree = tree#add in
      let new_subterm_index = process_clause (fold_subterms op) _subterm_index hc
      and new_unit_root_index = match hc.clits with
          | [(Equation (_,_,true,_)) as lit] ->
              process_lit (apply_root_term op) hc _unit_root_index (lit, 0)
          | _ -> _unit_root_index
      and new_ground_rewrite_index = match hc.clits with
          | [(Equation (l,r,true,Gt))] when T.is_ground_term l ->
              Ptmap.add l.tag (r, (hc, [0; C.left_pos], l)) _ground_rewrite_index
          | [(Equation (l,r,true,Lt))] when T.is_ground_term r ->
              Ptmap.add r.tag (l, (hc, [0; C.right_pos], r)) _ground_rewrite_index
          | _ -> _ground_rewrite_index
      and new_root_index = process_clause (apply_root_term op) _root_index hc
      in ({< _root_index=new_root_index;
            _unit_root_index=new_unit_root_index;
            _ground_rewrite_index=new_ground_rewrite_index;
            _subterm_index=new_subterm_index >} :> 'self)

    (** remove root terms and subterms from respective indexes *)
    method remove_clause hc =
      let op tree = tree#remove in
      let new_subterm_index = process_clause (fold_subterms op) _subterm_index hc
      and new_unit_root_index = match hc.clits with
          | [(Equation (_,_,true,_)) as lit] ->
              process_lit (apply_root_term op) hc _unit_root_index (lit, 0)
          | _ -> _unit_root_index
      and new_ground_rewrite_index = match hc.clits with
          | [(Equation (l,r,true,Gt))] when T.is_ground_term l ->
              Ptmap.remove l.tag _ground_rewrite_index
          | [(Equation (l,r,true,Lt))] when T.is_ground_term r ->
              Ptmap.remove r.tag _ground_rewrite_index
          | _ -> _ground_rewrite_index
      and new_root_index = process_clause (apply_root_term op) _root_index hc
      in ({< _root_index=new_root_index;
            _unit_root_index=new_unit_root_index;
            _ground_rewrite_index=new_ground_rewrite_index;
            _subterm_index=new_subterm_index >} :> 'self)

    method root_index = _root_index
    method unit_root_index = _unit_root_index
    method ground_rewrite_index = _ground_rewrite_index
    method subterm_index = _subterm_index

    method pp ~all_clauses formatter () =
      Format.fprintf formatter
        ("clause_index:@.root_index=@[<v>%a@]@.unit_root_index=@[<v>%a@]@." ^^
         "ground_rewrite_index=%d rules@.subterm_index=@[<v>%a@]@.")
        (_root_index#pp ~all_clauses) ()
        (_unit_root_index#pp ~all_clauses) ()
        (ptmap_size _ground_rewrite_index)
        (_subterm_index#pp ~all_clauses) ()
  end
