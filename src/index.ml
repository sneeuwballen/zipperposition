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

(** A term index *)
class type index =
  object ('b)
    method name : string
    method add : foterm -> data -> 'b
    method remove: foterm -> data -> 'b

    method iter : (data -> unit) -> unit
    method fold : 'a. ('a -> data -> 'a) -> 'a -> 'a

    method retrieve_unifiables : 'a. foterm -> 'a -> ('a -> data -> 'a) -> 'a
    method retrieve_generalizations : 'a. foterm -> 'a -> ('a -> data -> 'a) -> 'a
    method retrieve_specializations : 'a. foterm -> 'a -> ('a -> data -> 'a) -> 'a

    method pp : all_clauses:bool -> Format.formatter -> unit -> unit
  end

(** A global index, that operates on hashconsed clauses *)
class type clause_index =
  object ('a)
    method index_clause : hclause -> 'a
    method remove_clause : hclause -> 'a

    method root_index : index
    method unit_root_index : index
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

let mk_clause_index (index : index) =
  object (_: 'self)
    val _root_index = index
    val _subterm_index = index
    val _unit_root_index = index 

    (** add root terms and subterms to respective indexes *)
    method index_clause hc =
      let op tree = tree#add in
      let new_subterm_index = process_clause (fold_subterms op) _subterm_index hc
      and new_unit_root_index = match hc.clits with
          | [(Equation (_,_,true,_)) as lit] ->
              process_lit (apply_root_term op) hc _unit_root_index (lit, 0)
          | _ -> _unit_root_index
      and new_root_index = process_clause (apply_root_term op) _root_index hc
      in ({< _root_index=new_root_index;
            _unit_root_index=new_unit_root_index;
            _subterm_index=new_subterm_index >} :> 'self)

    (** remove root terms and subterms from respective indexes *)
    method remove_clause hc =
      let op tree = tree#remove in
      let new_subterm_index = process_clause (fold_subterms op) _subterm_index hc
      and new_unit_root_index = match hc.clits with
          | [(Equation (_,_,true,_)) as lit] ->
              process_lit (apply_root_term op) hc _unit_root_index (lit, 0)
          | _ -> _unit_root_index
      and new_root_index = process_clause (apply_root_term op) _root_index hc
      in ({< _root_index=new_root_index;
            _unit_root_index=new_unit_root_index;
            _subterm_index=new_subterm_index >} :> 'self)

    method root_index = _root_index
    method unit_root_index = _unit_root_index
    method subterm_index = _subterm_index

    method pp ~all_clauses formatter () =
      Format.fprintf formatter
        "clause_index:@.root_index=@[<v>%a@]@.unit_root_index=@[<v>%a@]@.subterm_index=@[<v>%a@]@."
        (_root_index#pp ~all_clauses) ()
        (_unit_root_index#pp ~all_clauses) ()
        (_subterm_index#pp ~all_clauses) ()
  end
