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

(* $Id: terms.ml 10720 2010-02-08 07:24:34Z asperti $ *)
open Hashcons

type leaf = Signature.symbol

(* a sort for terms *)
type sort = leaf

(* some special sorts *)
let bool_sort = Signature.bool_symbol
let univ_sort = Signature.univ_symbol

(* exception raised when sorts are mismatched *)
exception SortError of string

(* simple term *)
type foterm = typed_term Hashcons.hash_consed
and typed_term = {
  term : foterm_cell;   (* the term itself *)
  sort : sort;          (* the sort of the term *)
}
and foterm_cell = 
  | Leaf of leaf  (* constant *)
  | Var of int  (* variable *)
  | Node of foterm list  (* term application *)

(* hashconsing *)
module H = Hashcons.Make(struct
  type t = typed_term
  let rec equal = fun x y -> match (x.term, y.term) with
    | (Var i, Var j) -> i = j
    | (Leaf a, Leaf b) -> Signature.eq a b
    | (Node a, Node b) -> eq_subterms a b
    | (_, _) -> false
  and eq_subterms a b = match (a, b) with
    | ([],[]) -> true
    | (a::a1, b::b1) -> if equal a.node b.node
      then eq_subterms a1 b1 else false
    | (_, _) -> false
  let hash = Hashtbl.hash
end)

(* the terms table *)
let terms = H.create 251

let iter_terms f = H.iter f terms

(* smart constructors, with type-checking *)
let mk_var idx sort = H.hashcons terms {term = Var idx; sort=sort}
let mk_leaf leaf sort = H.hashcons terms {term = Leaf leaf; sort=sort}
let rec mk_node = function
  | [] -> failwith "cannot build empty node term"
  | (head::_) as subterms -> 
        H.hashcons terms {term = (Node subterms); sort=head.node.sort}

(* special terms *)
let eq_term = mk_leaf Signature.eq_symbol bool_sort (* equality, returns bool *)
let true_term = mk_leaf Signature.true_symbol bool_sort (* tautology term *)

(* membership: [a] [b] checks if a subterm of b *)
let rec member_term a b = a == b || match b.node.term with
  | Leaf _ | Var _ -> false
  | Node subterms -> List.exists (member_term a) subterms
(* hashconsing! *)
let eq_foterm x y = x == y

(* cast (change sort) *)
let cast t sort = H.hashcons terms { t.node with sort=sort; }

(* list of variables *)
type varlist = foterm list

(* free variables in the term *)
let vars_of_term t =
  let rec aux acc t = match t.node.term with
    | Leaf _ -> acc
    | Var _ -> if (List.mem t acc) then acc else t::acc
    | Node l -> List.fold_left aux acc l
  in aux [] t

(* substitution, a list of variables -> term *)
type substitution = (foterm * foterm) list

(* partial order comparison *)
type comparison = Lt | Eq | Gt | Incomparable | Invertible
(* direction of an equation (for rewriting) *)
type direction = Left2Right | Right2Left | Nodir
(* position in a term *)
type position = int list

(* left and right position in equation *)
let left_pos = 1
let right_pos = 2

(* a literal, that is, a signed equation *)
type literal = 
 | Equation of    foterm  (* lhs *)
                * foterm  (* rhs *)
                * bool    (* sign *)
                * comparison (* orientation *)

(* a first order clause *)
type clause =
    int (* ID *)
  * literal list  (* the equations *)
  * foterm list  (* the free variables *)
  * proof (* the proof for this clause *)
(* a proof step for a clause *)
and proof = Axiom of string | Proof of rule * proof_clauses
(* an inference rule name *)
and rule = string
(* a list of terms in clauses involved in an inference *)
and proof_clauses = (clause * position * substitution) list

module OT =
 struct
   type t = int 
   let compare = Pervasives.compare
 end

module M : Map.S with type key = int 
  = Map.Make(OT) 

(* multiset of clauses *)
type bag = {
  bag_id : int; (* max ID  *)
  bag_clauses : (clause * bool * int) M.t;
}

(* also gives a fresh ID to the clause *)
let add_to_bag (_,lit,vl,proof) bag =
  let id = bag.bag_id+1 in
  let clause = (id, lit, vl, proof) in
  let new_bag = {bag_id=id;
                 bag_clauses=M.add id (clause,false,0) bag.bag_clauses} in
  new_bag, clause 

let replace_in_bag ((id,_,_,_),_,_ as cl) bag =
  let new_clauses = M.add id cl bag.bag_clauses in
    { bag with bag_clauses = new_clauses }

let get_from_bag id bag =
  M.find id bag.bag_clauses
  
let empty_bag = {bag_id=0; bag_clauses=M.empty}
