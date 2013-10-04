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

(** {1 Non-Perfect Discrimination Tree} *)

module T = Term
module S = Substs

let prof_npdtree_retrieve = Util.mk_profiler "NPDtree_retrieve"

(** {2 Term traversal} *)

(** Term traversal in prefix order. This is akin to lazy transformation
    to a flatterm. *)

(** get position of next term *)
let next t pos = pos+1

(** skip subterms, got to next term that is not a subterm of t|pos *)
let skip t pos =
  let t_pos = T.at_cpos t pos in
  pos + t_pos.T.tsize

(** {2 Discrimination tree} *)

module Make(E : Index.EQUATION) = struct
  module E = E

  type rhs = E.rhs

  module Leaf = Index.MakeLeaf(E)

  module SMap = Symbol.SMap

  type t =
  | Leaf of Leaf.t
  | Node of node
  (** The discrimination tree *)

  and node = {
    star : t option;  (* by variable *)
    map : t SMap.t;   (* by symbol *)
  } (** A node of the tree, pointing to subtrees *)

  let empty = Node {map=SMap.empty; star=None;}

  let is_empty n = match n with
  | Leaf l -> Leaf.is_empty l
  | Node n -> n.star = None && SMap.is_empty n.map

  (** get/add/remove the leaf for the given term. The
      continuation k takes the leaf, and returns a leaf option
      that replaces the old leaf. 
      This function returns the new trie. *)
  let goto_leaf trie t k =
    (* the root of the tree *)
    let root = trie in
    (* function to go to the given leaf, building it if needed.
        [t] is the same term, [i] is the index in the term *)
    let rec goto trie t i rebuild =
      if T.size t = i
        then match trie with
          | Leaf l ->
            begin match k l with
            | l' when l == l' -> root (* no change, return same tree *)
            | l' -> rebuild (Leaf l')
            end
          | Node _ -> failwith "end of term is not in a NPDtree leaf"
        else match trie, (T.at_cpos t i).T.term with
          | Leaf _, _ -> failwith "middle of term is in a NPDtree leaf"
          | Node n, (T.Var _ | T.BoundVar _) ->
            let subtrie = match n.star with
              | None -> Node {star=None; map=SMap.empty;}
              | Some trie' -> trie'
            in
            let rebuild subtrie =
              if is_empty subtrie
                then rebuild (Node {n with star=None; })
                else rebuild (Node {n with star=Some subtrie ;})
            in
            goto subtrie t (i+1) rebuild
          | Node n, (T.Node (s, _) | T.Bind (s, _)) ->
            let subtrie =
              try SMap.find s n.map
              with Not_found -> Node{star=None; map=SMap.empty;}
            in
            let rebuild subtrie =
              if is_empty subtrie
                then rebuild (Node {n with map=SMap.remove s n.map; })
                else rebuild (Node {n with map=SMap.add s subtrie n.map ;})
            in
            goto subtrie t (i+1) rebuild
          | Node n, T.At _ ->
            failwith "NPDtree: unable to deal with curried terms" (* TODO? *)
  in
  goto trie t 0 (fun t -> t)

  let add trie eqn =
    let t, _, _ = E.extract eqn in
    let k leaf = Leaf.add leaf t eqn in
    goto_leaf trie t k

  let remove trie eqn =
    let t, _, _ = E.extract eqn in
    let k leaf = Leaf.remove leaf t eqn in
    goto_leaf trie t k

  let add_seq dt seq =
    Sequence.fold add dt seq

  let remove_seq dt seq =
    Sequence.fold remove dt seq

  let retrieve ~sign dt sc_dt t sc_t acc k =
    Util.enter_prof prof_npdtree_retrieve;
    (* extended callback *)
    let k' acc t' eqn subst =
      let _, r, _ = E.extract eqn in
      k acc t' r eqn subst
    in
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie acc i =
      if i = T.size t
        then match trie with
          | Leaf l ->
            Leaf.fold_match l sc_dt t sc_t acc k'
          | Node _ -> failwith "wrong arity in NPDtree.retrieve"
        else match trie, (T.at_cpos t i).T.term with
          | Leaf _, _ -> failwith "wrong structure in NPDtree.retrieve"
          | Node n, (T.Var _ | T.BoundVar _) ->
            begin match n.star with
            | None -> acc
            | Some subtrie ->
              traverse subtrie acc (i+1)  (* match "*" against "*" *)
            end
          | Node n, (T.Node (s, _) | T.Bind (s, _)) ->
            let acc =
              try
                let subtrie = SMap.find s n.map in
                traverse subtrie acc (i+1)
              with Not_found -> acc
            in
            begin match n.star with
              | None -> acc
              | Some subtrie ->
                traverse subtrie acc (skip t i)  (* skip subterm *)
            end
          | Node n, T.At _ ->
            failwith "NPDtree: unable to deal with curried terms" (* TODO? *)
    in
    let acc = traverse dt acc 0 in
    Util.exit_prof prof_npdtree_retrieve;
    acc

  (** iterate on all (term -> value) in the tree *)
  let rec iter dt k =
    match dt with
    | Leaf leaf -> Leaf.iter leaf (fun t set -> Leaf.S.iter (fun v -> k t v) set)
    | Node n ->
      begin match n.star with
      | None -> ()
      | Some trie' -> iter trie' k
      end;
      SMap.iter (fun _ trie' -> iter trie' k) n.map

  let size dt =
    let n = ref 0 in
    iter dt (fun _ _ -> incr n);
    !n
  
  let to_dot buf t =
    failwith "NPDTree.to_dot: not implemented"
end
