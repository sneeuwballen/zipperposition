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

open Types
module T = Terms
module S = FoSubst

(** Efficient perfect discrimination trees for matching *)

(* --------------------------------------------------------
 * term traversal in prefix order
 * -------------------------------------------------------- *)

(** index of subterm in prefix traversal *)
type position = int

(** get subterm by its position *)(
let rec get_pos t pos = 
  match t.term, pos with
  | _, 0 -> t
  | Node (_::l) -> get_subpos l pos
and get_subpos l pos =
  match l, pos with
  | t::l', _ when t.tsize <= pos -> get_subpos l' (pos - t.tsize)
  | t::l', _ -> get_pos t pos  (* search inside the term *)
  | [], _ -> assert false

(** get position of next term *)
let next t pos = pos+1

(** skip subterms, got to next term that is not a subterm of t|pos *)
let skip t pos =
  let t = get_pos t pos in
  pos + t.tsize

(** arity of the subterm of t which is at given pos *)
let arity t pos =
  match get_pos t pos with
  | Leaf _ | Var _ -> 0
  | Node (_::l) -> List.length l
  | Node [] -> assert false

(** maximum position in the term *)
let maxpos t = t.tsize - 1

type character =
  | CVar of sort * int * term           (** var index, var *)
  | CSymb of sort * int * string        (** symbol, arity *)

let eq_character c1 c2 =
  match c1, c2 with
  | CVar (_, _, t1), CVar (_, _, t2) -> t1 == t2
  | CSymb (s1, i1, f1), CSymb (s2, i2, f2) ->
    s1 = s2 && i1 = i2 && f1 = f2
  | _ -> false

(** convert a term into a character (the first character of the term) *)
let rec term_to_char t =
  match t.term with
  | Var i -> CVar (t.sort, i, t)
  | Leaf s -> CSymb (t.sort, arity t 0, s)
  | Node (hd::_) ->  (* recurse to get the symbol *)
    (match term_to_char hd with
    | CVar _ -> assert false
    | CSymb (_, _, s) -> (t.sort, arity t 0, s))

(** convert term to list of char *)
let to_list t =
  let l = ref []
  and pos = ref 0 in
  for i = 0 to maxpos t do
    let c = term_to_char (get_pos t !pos) in
    l := c :: !l;
    incr pos;
  done;
  List.rev !l

(* --------------------------------------------------------
 * discrimination tree
 * -------------------------------------------------------- *)

type 'a trie =
  | Node of (character * 'a trie) list  (** map char -> trie *)
  | Leaf of (term * 'a * int) list      (** leaf with (term, value, priority) list *)

(** get/add/remove the leaf for the given flatterm. The
    continuation k takes the leaf, and returns a leaf option
    that replaces the old leaf. 
    This function returns the new trie. *)
let goto_leaf dt t k =
  (* the root of the tree *)
  let root = dt in
  (* function to go to the given leaf, building it if needed *)
  let rec goto dt t rebuild =
    match dt, t with
    | (Leaf l) as leaf, [] -> (* found leaf *)
      (match k l with
      | Leaf [] -> rebuild (Leaf [])
      | new_leaf when leaf == new_leaf -> root  (* no change, return same tree *)
      | new_leaf -> rebuild new_leaf)           (* replace by new leaf *)
    | Node l, c::t' ->
      search_list [] l c t' rebuild  (* search a subnode that matches *)
    | Node _, [] -> assert false (* ill-formed term *)
    | Leaf, _ -> assert false  (* wrong arity *)
  (* traverse the list of subtries to find one that match c *)
  and search_list left right c t' rebuild =
    match right with
    | [] ->
      (* no subtrie found. Continue with empty tree; If something is returned,
         build a node that points to it *)
      let rebuild' subtrie =
        let l = match subtrie with
          | Leaf [] | Node [] -> List.rev left
          | _ -> List.rev ((c, subtrie)::left)
        in rebuild (Node l) in
        rebuild (Node l)
      and empty_trie = if t' = [] then Leaf [] else Node [] in
      goto empty_trie t' rebuild'
    | (c', subtrie)::right' when eq_character c c' ->
      (* subtrie found, go into it. If it changes, just rebuild *)
      let rebuild' subtrie =
        let l = match subtrie with
          | Leaf [] | Node [] -> (List.rev left) @ right
          | _ -> (List.rev ((c, subtrie)::left)) @ right
        in rebuild (Node l) in
      goto subtrie t' rebuild'
    | c'::right' ->
      (* continue to the next subtrie*)
      search_list (c'::left) right' c t' rebuild
  in
  goto dt t (fun t -> t)
      
(** the tree itself, with metadata *)
type 'a dtree = {
  size : int;
  min_var : int;
  max_var : int;
  cmp : 'a -> 'a -> bool;
  tree : 'a trie;
}

(** empty discrimination tree (with a comparison function) *)
let empty f = {
  size = 0;
  min_var = max_int;
  max_var = min_int;
  cmp = f;
  tree = Node [];
}

(** add a term and a value to the discrimination tree. The priority
    is used to sort index values (by increasing number, the lowest
    are iterated on the first). *)
let add dt ?(priority=0) t v =
  let chars = to_list t in
  let k leaf = match leaf with
    | Leaf l ->
      let l' = ((t, v, priority)::l in
      let l' = List.stable_sort (fun (_, _, p1) (_, _, p2) -> p1 - p2) in
      Leaf l'
    | _ -> assert false
  in
  let tree = goto_leaf dt.tree chars k in
  {dt with tree=tree}  (* TODO update max var and min var *)

(** remove the term -> value from the tree *)
let remove dt t v =
  let chars = to_list t in
  let k leaf = match leaf with
    | Leaf l ->
      (* remove tuples that match *)
      let l' = List.filter (fun (t', v', _) -> t' != t || not (dt.cmp v v')) l in
      Leaf l'
    | _ -> assert false
  in
  let tree = goto_leaf dt.tree chars k in
  {dt with tree=tree}   (* TODO update max var and min var *)

(** maximum variable in the tree *)
let min_var dt = dt.min_var

(** minimum variable in the tree *)
let max_var dt = dt.max_var

(** iterate on all (term -> value) such that subst(term) = input_term *)
let iter_match dt t k = failwith "not implemented"  (* TODO *)

(** iterate on all (term -> value) in the tree *)
let iter dt k =
  let rec iter dt =
    match dt with
    | Node l -> List.iter (fun (_, sub_dt) -> iter sub_dt) l
    | Leaf l -> List.iter (fun (t, v, _) -> k t v) l
  in iter dt
  

(* --------------------------------------------------------
 * pretty printing
 * -------------------------------------------------------- *)

let pp_tree formatter t = failwith "not implemented"
