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

open Symbols
open Types

module T = Terms
module S = FoSubst
module Utils = FoUtils

(** Efficient perfect discrimination trees for matching *)

(* --------------------------------------------------------
 * term traversal in prefix order
 * -------------------------------------------------------- *)

(** get position of next term *)
let next t pos = pos+1

(** skip subterms, got to next term that is not a subterm of t|pos *)
let skip t pos =
  let t_pos = T.at_cpos t pos in
  pos + t_pos.tsize

type character = Symbol of symbol | BoundVariable of int * sort | Variable of term

let compare_char c1 c2 =
  (* compare variables by index *)
  let compare_vars v1 v2 = match v1.term, v2.term with
    | Var i, Var j -> if i <> j then i - j else compare_sort v1.sort v2.sort
    | _ -> assert false
  in
  match c1, c2 with
  | Symbol s1, Symbol s2 -> Symbols.compare_symbols s1 s2
  | BoundVariable (i, si), BoundVariable (j, sj) when i = j -> compare_sort si sj
  | BoundVariable (i, _), BoundVariable (j, _) -> i - j
  | Variable v1, Variable v2 -> compare_vars v1 v2
  (* symbol < bound_variable < variable *)
  | Variable _, _ -> 1
  | BoundVariable _, _ -> 1
  | Symbol _, _ -> -1

let eq_char c1 c2 = compare_char c1 c2 = 0
 
(** first symbol of t, or variable *)
let rec term_to_char t =
  match t.term with
  | Var _ -> Variable t
  | BoundVar i -> BoundVariable (i, t.sort)
  | Bind (f, _, _) -> Symbol f
  | Node (f, _) -> Symbol f

(** convert term to list of var/symbol *)
let to_list t =
  let l = ref []
  and pos = ref 0 in
  for i = 0 to T.max_cpos t do
    let c = term_to_char (T.at_cpos t !pos) in
    l := c :: !l;
    incr pos;
  done;
  List.rev !l

module CharMap = Map.Make(
  struct
    type t = character
    let compare = compare_char
  end)

(* --------------------------------------------------------
 * discrimination tree
 * -------------------------------------------------------- *)

type 'a trie =
  | TrieNode of 'a trie CharMap.t        (** map atom -> trie *)
  | TrieLeaf of (term * 'a * int) list      (** leaf with (term, value, priority) list *)

let empty_trie n = match n with
  | TrieNode m when CharMap.is_empty m -> true
  | TrieLeaf [] -> true
  | _ -> false

(** get/add/remove the leaf for the given flatterm. The
    continuation k takes the leaf, and returns a leaf option
    that replaces the old leaf. 
    This function returns the new trie. *)
let goto_leaf trie t k =
  (* the root of the tree *)
  let root = trie in
  (* function to go to the given leaf, building it if needed *)
  let rec goto trie t rebuild =
    match trie, t with
    | (TrieLeaf l) as leaf, [] -> (* found leaf *)
      (match k l with
      | new_leaf when leaf == new_leaf -> root  (* no change, return same tree *)
      | new_leaf -> rebuild new_leaf)           (* replace by new leaf *)
    | TrieNode m, c::t' ->
      (try  (* insert in subtrie *)
        let subtrie = CharMap.find c m in
        let rebuild' subtrie = match subtrie with
          | _ when empty_trie subtrie -> rebuild (TrieNode (CharMap.remove c m))
          | _ -> rebuild (TrieNode (CharMap.add c subtrie m))
        in
        goto subtrie t' rebuild'
      with Not_found -> (* no subtrie found *)
        let subtrie = if t' = [] then TrieLeaf [] else TrieNode CharMap.empty
        and rebuild' subtrie = match subtrie with
          | _ when empty_trie subtrie -> rebuild (TrieNode (CharMap.remove c m))
          | _ -> rebuild (TrieNode (CharMap.add c subtrie m))
        in
        goto subtrie t' rebuild')
    | TrieNode _, [] -> assert false (* ill-formed term *)
    | TrieLeaf _, _ -> assert false  (* wrong arity *)
  in
  goto trie t (fun t -> t)
      
(** the tree itself, with metadata *)
type 'a dtree = {
  min_var : int;  (* TODO remove, useless now *)
  max_var : int;
  cmp : 'a -> 'a -> bool;
  tree : 'a trie;
}

(** empty discrimination tree (with a comparison function) *)
let empty f = {
  min_var = max_int;
  max_var = min_int;
  cmp = f;
  tree = TrieNode CharMap.empty;
}

(** is the dtree empty? *)
let is_empty f = empty_trie f.tree

(** add a term and a value to the discrimination tree. The priority
    is used to sort index values (by increasing number, the lowest
    are iterated on the first). *)
let add dt ?(priority=0) t v =
  let chars = to_list t in
  let k l =
    let l' = (t, v, priority)::l in
    TrieLeaf (List.stable_sort (fun (_, _, p1) (_, _, p2) -> p1 - p2) l')
  in
  let tree = goto_leaf dt.tree chars k in
  let vars = T.vars t in
  let max_var = max (T.max_var vars) dt.max_var
  and min_var = min (T.min_var vars) dt.min_var in
  {dt with tree; max_var; min_var;}

(** remove the term -> value from the tree *)
let remove dt t v =
  let chars = to_list t in
  let k l =
    (* remove tuples that match *)
    let l' = List.filter (fun (t', v', _) -> t' != t || not (dt.cmp v v')) l in
    TrieLeaf l'
  in
  let tree = goto_leaf dt.tree chars k in
  (* we assume the (term->value) was in the tree; we also do not
    update max and min vars, so they are an approximation *)
  {dt with tree;}

(** maximum variable in the tree *)
let min_var dt = max dt.min_var 0

(** minimum variable in the tree *)
let max_var dt = max dt.max_var 0

(** iterate on all (term -> value) such that subst(term) = input_term *)
let iter_match (dt, o_dt) (t, o_t) k =
  (* recursive traversal of the trie, following paths compatible with t *)
  let rec traverse trie pos subst =
    match trie with
    | TrieLeaf l ->  (* yield all answers *)
      List.iter (fun (t', v, _) -> k (t', o_dt) v subst) l
    | TrieNode m ->
      (* "lazy" transformation to flatterm *)
      let t_pos = T.at_cpos t pos in
      let t1 = term_to_char t_pos in
      CharMap.iter
        (fun t1' subtrie ->
          (* explore branch that has the same symbol, if any *)
          (if eq_char t1' t1 && not (T.is_var t_pos) then
            traverse subtrie (next t pos) subst);
          (* if variable, try to bind it and continue *)
          (match t1' with
           | Variable v1' when v1'.sort == t_pos.sort && S.is_in_subst subst (v1', o_dt) ->
             (* already bound, check consistency *)
             let t_matched = S.apply_subst subst (t_pos, o_t) in
             let t_bound = S.apply_subst subst (v1', o_dt) in
             Utils.debug 1 "  bind @[<h>%a to %a@]" !T.pp_term#pp v1' !T.pp_term#pp t_pos;
             if t_matched == t_bound
                then traverse subtrie (skip t pos) subst  (* skip term *)
                else () (* incompatible bindings of the variable *)
          | Variable v1' when v1'.sort == t_pos.sort ->
            (* t1' not bound, so we bind it and continue in subtree *)
            let subst' = S.bind subst (v1', o_dt) (t_pos, o_t) in
            traverse subtrie (skip t pos) subst'
          | _ -> ()))
        m
  in
  traverse dt.tree 0 S.id_subst

(** iterate on all (term -> value) in the tree *)
let iter dt k =
  let rec iter trie =
    match trie with
    | TrieNode m -> CharMap.iter (fun _ sub_dt -> iter sub_dt) m
    | TrieLeaf l -> List.iter (fun (t, v, _) -> k t v) l
  in iter dt.tree

(* --------------------------------------------------------
 * pretty printing
 * -------------------------------------------------------- *)

let char_to_str = function
  | Variable v -> Utils.sprintf "%a" !T.pp_term#pp v
  | BoundVariable (i, s) -> Utils.sprintf "Y%d" i
  | Symbol s -> name_symbol s

module PrintTHCTree = Prtree.Make(
  struct
    type t = string * (term * hclause) trie

    (* get a list of (key, sub-node) *)
    let get_values map =
      let l : t list ref = ref [] in
      CharMap.iter
        (fun key node -> 
          let key_repr = char_to_str key in
          l := (key_repr, node) :: !l) map;
      !l
    and pp_rule formatter (_, (_, hc), _) =
      Format.fprintf formatter "@[<h>%a@]" !Clauses.pp_clause#pp_h hc

    (* recurse in subterms *)
    let decomp (prefix, t) = match t with
      | TrieNode m -> prefix, get_values m
      | TrieLeaf l ->
        let rules_repr = Utils.sprintf "%s @[<h>{%a}@]"
          prefix (Utils.pp_list ~sep:"; " pp_rule) l in
        rules_repr, []
  end)

let pp_term_hclause_tree formatter dt = PrintTHCTree.print formatter ("", dt.tree)

module PrintTTree = Prtree.Make(
  struct
    type t = string * term trie

    (* get a list of (key, sub-node) *)
    let get_values map =
      let l : t list ref = ref [] in
      CharMap.iter
        (fun key node -> 
          let key_repr = char_to_str key in
          l := (key_repr, node) :: !l) map;
      !l
    and pp_rule formatter (l, r, _) =
      Format.fprintf formatter "@[<h>%a → %a@]" !T.pp_term#pp l !T.pp_term#pp r

    (* recurse in subterms *)
    let decomp (prefix, t) = match t with
      | TrieNode m -> prefix, get_values m
      | TrieLeaf l ->
        let rules_repr = Utils.sprintf "%s @[<h>{%a}@]"
          prefix (Utils.pp_list ~sep:"; " pp_rule) l in
        rules_repr, []
  end)

let pp_term_tree formatter dt = PrintTTree.print formatter ("", dt.tree)

(* --------------------------------------------------------
 * index
 * -------------------------------------------------------- *)

let unit_index =
  let eq_term_hclause (t1, hc1) (t2, hc2) =
    T.eq_term t1 t2 && Clauses.eq_hclause hc1 hc2
  in
  object (self : 'self)
    val pos = empty eq_term_hclause
    val neg = empty eq_term_hclause

    method name = "dtree_unit_index"

    method maxvar = max (max_var pos) (max_var neg)

    method is_empty = is_empty pos && is_empty neg

    method add_clause hc =
      match hc.hclits with
      | [|Equation (l,r,true,Gt)|] -> self#add l r true hc
      | [|Equation (l,r,true,Lt)|] -> self#add r l true hc
      | [|Equation (l,r,true,Incomparable)|]
      | [|Equation (l,r,true,Eq)|] -> (* equal modulo symmetry of =, or incomparable *)
        let self' = self#add l r true hc in
        self'#add r l true hc
      | [|Equation (l,r,false,_)|] ->
        let self' = self#add l r false hc in
        self'#add r l false hc
      | _ -> self  (* do not add other clauses *)

    method remove_clause hc =
      match hc.hclits with
      | [|Equation (l,r,true,Gt)|] -> self#remove l r true hc
      | [|Equation (l,r,true,Lt)|] -> self#remove r l true hc
      | [|Equation (l,r,true,Incomparable)|]
      | [|Equation (l,r,true,Eq)|] ->
        let self' = self#remove l r true hc in
        self'#remove r l true hc
      | [|Equation (l,r,false,_)|] ->
        let self' = self#remove l r false hc in
        self'#remove r l false hc
      | _ -> self  (* do not remove anything *)

    method add l r sign hc =
      if sign
        then ({< pos = add pos l (r, hc) >} :> 'self)
        else ({< neg = add neg l (r, hc) >} :> 'self)

    method remove l r sign hc =
      if sign
        then ({< pos = remove pos l (r, hc) >} :> 'self)
        else ({< neg = remove neg l (r, hc) >} :> 'self)

    method retrieve ~sign offset (t, o_t) k =
      let handler l (r, hc) subst = k l (r, offset) subst hc in
      if sign
        then iter_match (pos, offset) (t, o_t) handler
        else iter_match (neg, offset) (t, o_t) handler

    method pp formatter () =
      Format.fprintf formatter "@[<hv>pos: %a@.neg:%a@]"
        pp_term_hclause_tree pos pp_term_hclause_tree neg
  end
