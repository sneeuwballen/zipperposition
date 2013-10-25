(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Perfect Discrimination Tree} *)

module T = FOTerm
module S = Substs.FO

let prof_dtree_retrieve = Util.mk_profiler "dtree_retrieve"

(** {2 Term traversal} *)

(** Term traversal in prefix order. This is akin to lazy transformation
    to a flatterm. *)

(** get position of next term *)
let next t pos = pos+1

(** skip subterms, got to next term that is not a subterm of t|pos *)
let skip t pos =
  let t_pos = T.at_cpos t pos in
  pos + t_pos.T.tsize

type character =
  | Symbol of Symbol.t
  | BoundVariable of int
  | Variable of T.t

let compare_char c1 c2 =
  (* compare variables by index *)
  let compare_vars v1 v2 = match v1.T.term, v2.T.term with
    | T.Var i, T.Var j ->
      if i <> j then i - j else Type.cmp (T.get_type v1) (T.get_type v2)
    | _ -> assert false
  in
  match c1, c2 with
  | Symbol s1, Symbol s2 -> Symbol.compare s1 s2
  | BoundVariable i, BoundVariable j -> i - j
  | Variable v1, Variable v2 -> compare_vars v1 v2
  (* symbol < bound_variable < variable *)
  | Variable _, _ -> 1
  | BoundVariable _, _ -> 1
  | Symbol _, _ -> -1

let eq_char c1 c2 = compare_char c1 c2 = 0
 
(** first symbol of t, or variable *)
let rec term_to_char t =
  match t.T.term with
  | T.Var _ -> Variable t
  | T.BoundVar i -> BoundVariable i
  | T.Node (f, _) -> Symbol f

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

module CharMap = Map.Make(struct
  type t = character
  let compare = compare_char
end)

let char_to_str c = match c with
  | Symbol s -> Util.sprintf "%a" Symbol.pp s
  | BoundVariable i -> Util.sprintf "Y%d" i
  | Variable t -> Util.sprintf "%a" T.pp t

(** {2 Discrimination tree} *)

module Make(E : Index.EQUATION) = struct
  module E = E

  type rhs = E.rhs

  type trie =
    | TrieNode of trie CharMap.t       (** map atom -> trie *)
    | TrieLeaf of (T.t * E.t * int) list  (** leaf with (term, value, priority) list *)

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
      
  type t = trie

  let empty = TrieNode CharMap.empty

  let is_empty = empty_trie

  let add dt eqn =
    let t, _, _ = E.extract eqn in
    let priority = E.priority eqn in
    let chars = to_list t in
    let k l =
      let l' = (t, eqn, priority)::l in
      TrieLeaf (List.stable_sort (fun (_, _, p1) (_, _, p2) -> p1 - p2) l')
    in
    let tree = goto_leaf dt chars k in
    tree

  let remove dt eqn =
    let t, _, _ = E.extract eqn in
    let chars = to_list t in
    let k l =
      (* remove tuples that match *)
      let l' = List.filter (fun (t', eqn', _) -> t' != t || E.compare eqn eqn' <> 0) l in
      TrieLeaf l'
    in
    let tree = goto_leaf dt chars k in
    tree

  let add_seq dt seq =
    Sequence.fold add dt seq

  let remove_seq dt seq =
    Sequence.fold remove dt seq

  let retrieve ~sign dt sc_dt t sc_t acc k =
    Util.enter_prof prof_dtree_retrieve;
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie acc pos subst =
      match trie with
      | TrieLeaf l ->  (* yield all equations, they all match *)
        List.fold_left
          (fun acc (t', eqn, _) ->
            let l, r, sign' = E.extract eqn in
            if sign = sign'
              then k acc l r eqn subst
              else acc)
          acc l
      | TrieNode m ->
        (* "lazy" transformation to flatterm *)
        let t_pos = T.at_cpos t pos in
        let t1 = term_to_char t_pos in
        CharMap.fold
          (fun t1' subtrie acc ->
            (* explore branch that has the same symbol, if any *)
            let acc = if eq_char t1' t1 && not (T.is_var t_pos)
              then traverse subtrie acc (next t pos) subst
              else acc
            in
            (* if variable, try to bind it and continue *)
            match t1' with
            | Variable v1' when S.is_in_subst subst v1' sc_dt ->
               (* already bound, check consistency *)
              begin try
                let subst = FOUnif.matching ~subst v1' sc_dt t_pos sc_t in
                traverse subtrie acc (skip t pos) subst
              with FOUnif.Fail -> acc (* incompatible binding *)
              end
            | Variable v1' ->
              (* t1' not bound, so we bind it and continue in subtree *)
              let subst' = FOUnif.matching ~subst v1' sc_dt t_pos sc_t in
              traverse subtrie acc (skip t pos) subst'
            | _ -> acc)
          m acc
    in
    let acc = traverse dt acc 0 S.empty in
    Util.exit_prof prof_dtree_retrieve;
    acc

  (** iterate on all (term -> value) in the tree *)
  let iter dt k =
    let rec iter trie =
      match trie with
      | TrieNode m -> CharMap.iter (fun _ sub_dt -> iter sub_dt) m
      | TrieLeaf l -> List.iter (fun (t, v, _) -> k t v) l
    in iter dt

  let size dt =
    let n = ref 0 in
    iter dt (fun _ _ -> incr n);
    !n
  
  let to_dot buf t =
    failwith "DTree.to_dot: not implemented"
end
