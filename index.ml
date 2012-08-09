(*
    ||M||  This file is part of HELM, an Hypertextual, Electronic        
    ||A||  Library of Mathematics, developed at the Computer Science     
    ||T||  Department, University of Bologna, Italy.                     
    ||I||                                                                
    ||T||  HELM is free software; you can redistribute it and/or         
    ||A||  modify it under the terms of the GNU General Public License   
    \   /  version 2 or (at your option) any later version.      
     \ /   This software is distributed as is, NO WARRANTY.     
      V_______________________________________________________________ *)

(* $Id: index.ml 11696 2011-11-21 09:42:44Z asperti $ *)

open Terms
open Hashcons

module Index(Ord : Orderings.S) = struct
  module U = FoUtils
  module Unif = FoUnif

  (* an order on clauses+positions *)
  module ClauseOT =
    struct 
      type t = Terms.clause * Terms.position
 
      let compare (c1, p1) (c2, p2) = 
        let c = Pervasives.compare p1 p2 in
        if c <> 0 then c else
        U.compare_clause c1 c2
    end

  (* a set of (clause, position in clause). A position is a
   * list, that, once reversed, is [lit index, 1|2 (left or right), ...]
   * where ... is the path in the term *)
  module ClauseSet : Set.S with 
    type elt = Terms.clause * Terms.position
    = Set.Make(ClauseOT)

  open Discrimination_tree

  module FotermIndexable = struct
    type input = Terms.foterm
    type constant_name = Signature.symbol

    (* convert into a path string *)
    let path_string_of t =
      let rec aux arity t = match t.node.term with
        | Terms.Leaf a -> [Constant (a, arity)]
        | Terms.Var i -> (* assert (arity = 0); *) [Variable]
        | Terms.Node ([] | [ _ ] )
        (* FIXME : should this be allowed or not ? *)
        | Terms.Node ({node={term=Terms.Var _}}::_)
        | Terms.Node ({node={term=Terms.Node _}}::_) -> assert false
        | Terms.Node (hd::tl) ->
            aux (List.length tl) hd @ List.flatten (List.map (aux 0) tl) 
      in 
        aux 0 t

    (* compare two path string elements *)
    let compare e1 e2 = 
      match e1,e2 with 
      | Constant (a1,ar1), Constant (a2,ar2) ->
          let c = Signature.compare a1 a2 in
          if c <> 0 then c else Pervasives.compare ar1 ar2
      | Variable, Variable -> 0
      | Constant _, Variable -> ~-1
      | Variable, Constant _ -> 1
      | Proposition, _ | _, Proposition
      | Datatype, _ | _, Datatype
      | Dead, _ | _, Dead
      | Bound _, _ | _, Bound _ -> assert false

    (* print path into string *)
    let string_of_path l = String.concat "." (List.map (fun _ -> "*") l)
  end

  (* the discrimination trees used for indexing *)
  module DT : DiscriminationTree with
    type constant_name = Signature.symbol and 
    type input = Terms.foterm and 
    type data = ClauseSet.elt and 
    type dataset = ClauseSet.t
  = Make(FotermIndexable)(ClauseSet)

  type input = DT.input
  type data = DT.data
  type dataset = DT.dataset

  (* the main index type. It contains two trees, that are
   * used to index all subterms of a clause, and terms
   * that occur directly under an equation. *)
  type t = {
    root_index : DT.t;
    subterm_index : DT.t;
  }

  (* empty index *)
  let empty = { root_index=DT.empty; subterm_index=DT.empty }
  
  (* apply op to some of the literals of the clause. *)
  let process op tree ((_, lits, _, _) as c) =
    let process_lit (pos, tree) lit =
      let new_tree = match lit with
      | Terms.Equation (l,_,_,Terms.Gt) -> 
          op tree l (c, [Terms.left_pos; pos])
      | Terms.Equation (_,r,_,Terms.Lt) -> 
          op tree r (c, [Terms.right_pos; pos])
      | Terms.Equation (l,r,_,Terms.Incomparable) ->
          let tmp_tree = op tree l (c, [Terms.left_pos; pos]) in
          op tmp_tree r (c, [Terms.right_pos; pos])
      | Terms.Equation (l,r,_,Terms.Invertible) ->
          op tree l (c, [Terms.left_pos; pos])
      | Terms.Equation (_,r,_,Terms.Eq) -> assert false
      and new_pos = pos+1
      in (new_pos, new_tree)
    in
    let _, new_tree = List.fold_left process_lit (1,tree) lits in
    new_tree
  
  (* apply (op tree) to all subterms, folding the resulting tree *)
  let rec fold_subterms op tree t (c, path) = match t.node.term with
    | Terms.Var _ -> tree  (* variables are not indexed *)
    | Terms.Leaf _ -> op tree t (c, List.rev path)
    | Terms.Node l ->
        (* apply the operation on the term itself *)
        let tmp_tree = op tree t (c, List.rev path) in
        let _, new_tree = List.fold_left
          (* apply the operation on each i-th subterm with i::path as position *)
          (fun (idx, tree) t -> idx+1, fold_subterms op tree t (c, idx::path))
          (1, tmp_tree) l
        in new_tree

  (* add root terms and subterms to respective indexes *)
  let index_clause {root_index; subterm_index} clause =
    let new_subterm_index = process (fold_subterms DT.index) subterm_index clause
    and new_root_index = process DT.index root_index clause
    in {root_index=new_root_index; subterm_index=new_subterm_index}
 
  (* remove root terms and subterms from respective indexes *)
  let remove_clause {root_index; subterm_index} clause =
    let new_subterm_index = process (fold_subterms DT.remove_index) subterm_index clause
    and new_root_index = process DT.remove_index root_index clause
    in {root_index=new_root_index; subterm_index=new_subterm_index}

  let fold = DT.fold 

  let elems index =
    DT.fold index (fun _ dataset acc -> ClauseSet.union dataset acc)
      ClauseSet.empty
    
  type active_set = Terms.clause list * DT.t
end
