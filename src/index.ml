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

module T = Terms
module C = Clauses
module Utils = FoUtils

type data = hclause * position * term

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
        (assert (T.eq_term t1 t2); 0)
    end)

(** a leaf of an index is generally a map of terms to data *)
type index_leaf = (term * ClauseSet.t) Ptmap.t

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
    assert (T.eq_term t t');
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
    method add : term -> data -> 'b
    method remove: term -> data -> 'b

    method iter : (term -> ClauseSet.t -> unit) -> unit
    method fold : 'a. ('a -> term -> ClauseSet.t -> 'a) -> 'a -> 'a

    method retrieve_unifiables : 'a. term -> 'a ->
                                 ('a -> term -> ClauseSet.t -> 'a) -> 'a
    method retrieve_generalizations : 'a. term -> 'a ->
                                      ('a -> term -> ClauseSet.t -> 'a) -> 'a
    method retrieve_specializations : 'a. term -> 'a ->
                                      ('a -> term -> ClauseSet.t -> 'a) -> 'a

    method pp : all_clauses:bool -> Format.formatter -> unit -> unit
  end

(** A simplification index *)
class type unit_index = 
  object ('b)
    method name : string
    method maxvar : int
    method is_empty : bool
    method add_clause : hclause -> 'b
    method remove_clause : hclause -> 'b
    method add : term -> term -> bool -> hclause -> 'b
    method remove : term -> term -> bool -> hclause ->'b
    method retrieve : sign:bool -> term ->
                      (term -> term -> substitution -> hclause -> unit) ->
                      unit        (** iter on (in)equations of given sign l=r
                                      where subst(l) = query term *)
    method pp : Format.formatter -> unit -> unit
  end

(** A global index, that operates on hashconsed clauses *)
class type clause_index =
  object ('a)
    method index_clause : hclause -> 'a
    method remove_clause : hclause -> 'a

    method root_index : index
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
  | Equation (l,r,_,Incomparable) ->
      let tmp_tree = op tree l (c, [C.left_pos; pos]) in
      op tmp_tree r (c, [C.right_pos; pos])
  | Equation (l,r,_,Eq) ->
    Utils.debug 4 (lazy (Utils.sprintf "add %a = %a to index"
                   !T.pp_term#pp l !T.pp_term#pp r));
    op tree l (c, [C.left_pos; pos])  (* only index one side *)

(** apply op to the maximal literals of the clause, and only to
    the maximal side(s) of those, if restruct is true. Otherwise
    process all literals *)
let process_clause ~restrict op tree c =
  (* which literals to process? *)
  let lits_pos =
    if restrict && C.selected c = [] then C.maxlits c
    else if restrict then C.selected_lits c
    else Utils.list_pos c.clits
  in
  (* index literals with their position *)
  let new_tree = List.fold_left (process_lit op c) tree lits_pos in
  new_tree

(** apply (op tree) to all subterms, folding the resulting tree *)
let rec fold_subterms op tree t (c, path) = match t.term with
  | Var _ -> tree  (* variables are not indexed *)
  | Node (_, []) -> op tree t (c, List.rev path, t) (* reverse path now *)
  | Node (_, l) ->
      (* apply the operation on the term itself *)
      let tmp_tree = op tree t (c, List.rev path, t) in
      let _, new_tree = List.fold_left
        (* apply the operation on each i-th subterm with i::path
           as position. i starts at 0 and the function symbol is ignored. *)
        (fun (idx, tree) t -> idx+1, fold_subterms op tree t (c, idx::path))
        (0, tmp_tree) l
      in new_tree

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

    (** add root terms and subterms to respective indexes *)
    method index_clause hc =
      let op tree = tree#add in
      let new_subterm_index = process_clause ~restrict:true (fold_subterms op) _subterm_index hc
      and new_root_index = process_clause ~restrict:true (apply_root_term op) _root_index hc
      in ({< _root_index=new_root_index;
            _subterm_index=new_subterm_index >} :> 'self)

    (** remove root terms and subterms from respective indexes *)
    method remove_clause hc =
      let op tree = tree#remove in
      let new_subterm_index = process_clause ~restrict:true (fold_subterms op) _subterm_index hc
      and new_root_index = process_clause ~restrict:true (apply_root_term op) _root_index hc
      in ({< _root_index=new_root_index;
            _subterm_index=new_subterm_index >} :> 'self)

    method root_index = _root_index
    method subterm_index = _subterm_index

    method pp ~all_clauses formatter () =
      Format.fprintf formatter
        ("clause_index:@.root_index=@[<v>%a@]@.subterm_index=@[<v>%a@]@.")
        (_root_index#pp ~all_clauses) ()
        (_subterm_index#pp ~all_clauses) ()
  end
