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

(** index of subterm in prefix traversal *)
type position = int

(** get subterm by its position *)
let rec get_pos t pos = 
  match t.term, pos with
  | _, 0 -> t
  | Node (_, l), _ -> get_subpos l (pos - 1)
  | _ -> assert false
and get_subpos l pos =
  match l, pos with
  | t::l', _ when t.tsize > pos -> get_pos t pos  (* search inside the term *)
  | t::l', _ -> get_subpos l' (pos - t.tsize) (* continue to next term *)
  | [], _ -> assert false

(** get position of next term *)
let next t pos = pos+1

(** skip subterms, got to next term that is not a subterm of t|pos *)
let skip t pos =
  let t_pos = get_pos t pos in
  pos + t_pos.tsize

(** maximum position in the term *)
let maxpos t = t.tsize - 1

type character = Symbol of symbol | Variable of term

let compare_char c1 c2 =
  match c1, c2 with
  | Symbol s1, Symbol s2 -> Symbols.compare_symbols s1 s2
  | Variable v1, Variable v2 -> T.compare_term v1 v2
  | Symbol _, Variable _ -> -1
  | Variable _, Symbol _ -> 1

let eq_char c1 c2 = compare_char c1 c2 = 0
 
(** first symbol of t, or variable *)
let rec term_to_char t =
  match t.term with
  | Var _ -> Variable t
  | Node (f, _) -> Symbol f

(** convert term to list of var/symbol *)
let to_list t =
  let l = ref []
  and pos = ref 0 in
  for i = 0 to maxpos t do
    let c = term_to_char (get_pos t !pos) in
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
  | Node of 'a trie CharMap.t        (** map atom -> trie *)
  | Leaf of (term * 'a * int) list      (** leaf with (term, value, priority) list *)

let empty_trie n = match n with
  | Node m when CharMap.is_empty m -> true
  | Leaf [] -> true
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
    | (Leaf l) as leaf, [] -> (* found leaf *)
      (match k l with
      | new_leaf when leaf == new_leaf -> root  (* no change, return same tree *)
      | new_leaf -> rebuild new_leaf)           (* replace by new leaf *)
    | Node m, c::t' ->
      (try  (* insert in subtrie *)
        let subtrie = CharMap.find c m in
        let rebuild' subtrie = match subtrie with
          | _ when empty_trie subtrie -> rebuild (Node (CharMap.remove c m))
          | _ -> rebuild (Node (CharMap.add c subtrie m))
        in
        goto subtrie t' rebuild'
      with Not_found -> (* no subtrie found *)
        let subtrie = if t' = [] then Leaf [] else Node CharMap.empty
        and rebuild' subtrie = match subtrie with
          | _ when empty_trie subtrie -> rebuild (Node (CharMap.remove c m))
          | _ -> rebuild (Node (CharMap.add c subtrie m))
        in
        goto subtrie t' rebuild')
    | Node _, [] -> assert false (* ill-formed term *)
    | Leaf _, _ -> assert false  (* wrong arity *)
  in
  goto trie t (fun t -> t)
      
(** the tree itself, with metadata *)
type 'a dtree = {
  min_var : int;
  max_var : int;
  cmp : 'a -> 'a -> bool;
  tree : 'a trie;
}

(** empty discrimination tree (with a comparison function) *)
let empty f = {
  min_var = max_int;
  max_var = min_int;
  cmp = f;
  tree = Node CharMap.empty;
}

(** add a term and a value to the discrimination tree. The priority
    is used to sort index values (by increasing number, the lowest
    are iterated on the first). *)
let add dt ?(priority=0) t v =
  let chars = to_list t in
  let k l =
    let l' = (t, v, priority)::l in
    Leaf (List.stable_sort (fun (_, _, p1) (_, _, p2) -> p1 - p2) l')
  in
  let tree = goto_leaf dt.tree chars k
  and max_var = max (T.max_var t.vars) dt.max_var
  and min_var = min (T.min_var t.vars) dt.min_var in
  {dt with tree; max_var; min_var;}

(** remove the term -> value from the tree *)
let remove dt t v =
  let chars = to_list t in
  let k l =
    (* remove tuples that match *)
    let l' = List.filter (fun (t', v', _) -> t' != t || not (dt.cmp v v')) l in
    Leaf l'
  in
  let tree = goto_leaf dt.tree chars k in
  (* we assume the (term->value) was in the tree; we also do not
    update max and min vars, so they are an approximation *)
  {dt with tree;}

(** maximum variable in the tree *)
let min_var dt = dt.min_var

(** minimum variable in the tree *)
let max_var dt = dt.max_var

(** iterate on all (term -> value) such that subst(term) = input_term *)
let iter_match dt t k =
  (* variable collision check *)
  assert (T.is_ground_term t || T.max_var t.vars < dt.min_var || T.min_var t.vars > dt.max_var);
  (* recursive traversal of the trie, following paths compatible with t *)
  let rec traverse trie pos subst =
    match trie with
    | Leaf l ->  (* yield all answers *)
      List.iter (fun (t', v, _) -> k t' v subst) l
    | Node m ->
      (* "lazy" transformation to flatterm *)
      let t_pos = get_pos t pos in
      let t1 = term_to_char t_pos in
      CharMap.iter
        (fun t1' subtrie ->
          (* explore branch that has the same symbol, if any *)
          (if eq_char t1' t1 then (assert (match t1 with Variable _ -> false | _ -> true);
                                   traverse subtrie (next t pos) subst));
          (* if variable, try to bind it and continue *)
          (match t1' with
           | Variable v1' when v1'.sort = t_pos.sort && S.is_in_subst v1' subst ->
             (* already bound, check consistency *)
             let t_matched = T.expand_bindings t_pos
             and t_bound = T.expand_bindings v1'.binding in
             if T.eq_term t_matched t_bound
               then traverse subtrie (skip t pos) subst  (* skip term *)
                else () (* incompatible bindings of the variable *)
          | Variable v1' when v1'.sort = t_pos.sort ->
            (* t1' not bound, so we bind it and continue in subtree *)
            T.set_binding v1' (T.expand_bindings t_pos);
            let subst' = S.update_binding subst v1' in
            traverse subtrie (skip t pos) subst';
            T.reset_binding v1'  (* cleanup the variable *)
          | _ -> ()))
        m
  in
  T.reset_vars t;
  traverse dt.tree 0 S.id_subst

(** iterate on all (term -> value) in the tree *)
let iter dt k =
  let rec iter trie =
    match trie with
    | Node m -> CharMap.iter (fun _ sub_dt -> iter sub_dt) m
    | Leaf l -> List.iter (fun (t, v, _) -> k t v) l
  in iter dt.tree
  

(* --------------------------------------------------------
 * pretty printing
 * -------------------------------------------------------- *)

let char_to_str = function
  | Variable v -> Utils.sprintf "%a" !T.pp_term#pp v
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
      | Node m -> prefix, get_values m
      | Leaf l ->
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
      | Node m -> prefix, get_values m
      | Leaf l ->
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

    method add_clause hc =
      match hc.clits with
      | [Equation (l,r,true,Gt)] -> self#add l r true hc
      | [Equation (l,r,true,Lt)] -> self#add r l true hc
      | [Equation (l,r,true,Incomparable)] ->
        let self' = self#add l r true hc in
        self'#add r l true hc
      | [Equation (l,r,true,Eq)] -> (assert (l == r); self)
      | [Equation (l,r,false,_)] ->
        let self' = self#add l r false hc in
        self'#add r l false hc
      | _ -> self  (* do not add other clauses *)

    method remove_clause hc =
      match hc.clits with
      | [Equation (l,r,true,Gt)] -> self#remove l r true hc
      | [Equation (l,r,true,Lt)] -> self#remove r l true hc
      | [Equation (l,r,true,Incomparable)] ->
        let self' = self#remove l r true hc in
        self'#remove r l true hc
      | [Equation (l,r,true,Eq)] -> (assert (l == r); self)
      | [Equation (l,r,false,_)] ->
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

    method retrieve ~sign t k =
      if sign
        then iter_match pos t (fun l (r, hc) subst -> k l r subst hc)
        else iter_match neg t (fun l (r, hc) subst -> k l r subst hc)

    method pp formatter () =
      Format.fprintf formatter "@[<hv>pos: %a@.neg:%a@]"
        pp_term_hclause_tree pos pp_term_hclause_tree neg
  end
