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
  vars : foterm list Lazy.t;   (* the variables of the term *)
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

let compute_vars t =  (* compute free vars of the term *)
  let rec aux acc t = match t.node.term with
    | Leaf _ -> acc
    | Var _ -> if (List.mem t acc) then acc else t::acc
    | Node l -> List.fold_left aux acc l
  in aux [] t

(* smart constructors, with type-checking *)
let mk_var idx sort =
  let my_v = {term = Var idx; sort=sort; vars=lazy []} in
  let v = H.hashcons terms
    {my_v with vars=lazy [H.hashcons terms my_v]} in
  ignore (Lazy.force v.node.vars); v

let mk_leaf leaf sort =
  H.hashcons terms {term = Leaf leaf; sort=sort; vars=lazy []}

let rec mk_node = function
  | [] -> failwith "cannot build empty node term"
  | (head::_) as subterms ->
      let my_t = {term=(Node subterms); sort=head.node.sort; vars=lazy []} in
      let lazy_vars = lazy (compute_vars (H.hashcons terms my_t)) in
      let t = H.hashcons terms { my_t with vars=lazy_vars } in
      ignore (Lazy.force t.node.vars); t

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
let vars_of_term t = Lazy.force t.node.vars

(* is the term ground? *)
let is_ground_term t = match vars_of_term t with
  | [] -> true
  | _ -> false

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
type clause = {
  cid : int;            (* ID *)
  clits : literal list; (* the equations *)
  cvars : foterm list;  (* the free variables *)
  cproof : proof;      (* the proof for this clause *)
}
(* a proof step for a clause *)
and proof = Axiom of string
          | Proof of string * (clause * position * substitution) list

module M : Map.S with type key = int
  = Map.Make(
     struct
       type t = int
       let compare = Pervasives.compare
     end)

(* multiset of clauses *)
type bag = {
  bag_id : int; (* max ID  *)
  bag_clauses : (clause * bool * int) M.t;
}

(* also gives a fresh ID to the clause *)
let add_to_bag c bag =
  let id = bag.bag_id+1 in
  let clause = {c with cid=id} in
  let new_bag = {bag_id=id;
                 (* FIXME is this false,0 or true,id ? *)
                 bag_clauses=M.add id (clause,false,0) bag.bag_clauses} in
  new_bag, clause

let replace_in_bag ({cid=id},_,_ as cl) bag =
  let new_clauses = M.add id cl bag.bag_clauses in
    { bag with bag_clauses = new_clauses }

let get_from_bag id bag =
  M.find id bag.bag_clauses

let empty_bag = {bag_id=0; bag_clauses=M.empty}
