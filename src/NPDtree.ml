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

  type t = {
    star : t option;  (* by variable *)
    map : t SMap.t;   (* by symbol *)
    leaf : Leaf.t;    (* leaves *)
  }  (** The discrimination tree *)

  let empty = {map=SMap.empty; star=None; leaf=Leaf.empty;}

  let is_empty n = n.star = None && SMap.is_empty n.map && Leaf.is_empty n.leaf

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
        then match k trie.leaf with
          | leaf' when leaf' == trie.leaf -> root (* no change, return same tree *)
          | leaf' -> rebuild {trie with leaf=leaf'; }
        else match (T.at_cpos t i).T.term with
          | (T.Var _ | T.BoundVar _) ->
            let subtrie = match trie.star with
              | None -> empty
              | Some trie' -> trie'
            in
            let rebuild subtrie =
              if is_empty subtrie
                then rebuild {trie with star=None; }
                else rebuild {trie with star=Some subtrie ;}
            in
            goto subtrie t (i+1) rebuild
          | (T.Node (s, _) | T.Bind (s, _)) ->
            let subtrie =
              try SMap.find s trie.map
              with Not_found -> empty
            in
            let rebuild subtrie =
              if is_empty subtrie
                then rebuild {trie with map=SMap.remove s trie.map; }
                else rebuild {trie with map=SMap.add s subtrie trie.map ;}
            in
            goto subtrie t (i+1) rebuild
          | T.At _ ->
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
      let _, r, sign' = E.extract eqn in
      if sign = sign' then k acc t' r eqn subst else acc
    in
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie acc i =
      if i = T.size t
        then Leaf.fold_match trie.leaf sc_dt t sc_t acc k'
        else match (T.at_cpos t i).T.term with
          | (T.Var _ | T.BoundVar _) ->
            begin match trie.star with
            | None -> acc
            | Some subtrie ->
              traverse subtrie acc (i+1)  (* match "*" against "*" *)
            end
          | (T.Node (s, _) | T.Bind (s, _)) ->
            let acc =
              try
                let subtrie = SMap.find s trie.map in
                traverse subtrie acc (i+1)
              with Not_found -> acc
            in
            begin match trie.star with
              | None -> acc
              | Some subtrie ->
                traverse subtrie acc (skip t i)  (* skip subterm *)
            end
          | T.At _ ->
            failwith "NPDtree: unable to deal with curried terms" (* TODO? *)
    in
    let acc = traverse dt acc 0 in
    Util.exit_prof prof_npdtree_retrieve;
    acc

  (** iterate on all (term -> value) in the tree *)
  let rec iter dt k =
    Leaf.iter dt.leaf (fun t set -> Leaf.S.iter (fun v -> k t v) set);
    begin match dt.star with
    | None -> ()
    | Some trie' -> iter trie' k
    end;
    SMap.iter (fun _ trie' -> iter trie' k) dt.map

  let size dt =
    let n = ref 0 in
    iter dt (fun _ _ -> incr n);
    !n
  
  let to_dot buf t =
    failwith "NPDTree.to_dot: not implemented"
end
