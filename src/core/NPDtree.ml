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

module T = FOTerm
module S = Substs

let prof_npdtree_retrieve = Util.mk_profiler "NPDtree_retrieve"
let prof_npdtree_term_unify = Util.mk_profiler "NPDtree_term_unify"
let prof_npdtree_term_generalizations =
  Util.mk_profiler "NPDtree_term_generalizations"
let prof_npdtree_term_specializations =
  Util.mk_profiler "NPDtree_term_specializations"

(** {2 Term traversal} *)

(** Term traversal in prefix order. This is akin to lazy transformation
    to a flatterm. *)

type iterator = {
  cur_term : T.t;
  stack : T.t list list; (* skip: drop head, next: first of head *)
}

let open_term ~stack t = match T.view t with
  | T.Var _
  | T.DB _
  | T.AppBuiltin _
  | T.Const _ ->
      Some {cur_term=t; stack=[]::stack;}
  | T.App (_, l) ->
      Some {cur_term=t; stack=l::stack;}

let rec next_rec stack = match stack with
  | [] -> None
  | []::stack' -> next_rec stack'
  | (t::next')::stack' ->
      open_term ~stack:(next'::stack') t

let skip iter = match iter.stack with
  | [] -> None
  | _next::stack' -> next_rec stack'
and next iter = next_rec iter.stack

(** Iterate on a term *)
let iterate term = open_term ~stack:[] term

(** {2 Unix index} *)

module Make(E : Index.EQUATION) = struct
  module E = E

  type rhs = E.rhs

  module Leaf = Index.MakeLeaf(E)

  type t = {
    star : t option;  (* by variable *)
    map : t ID.Map.t;   (* by symbol *)
    leaf : Leaf.t;    (* leaves *)
  }  (** The discrimination tree *)

  let empty () = {map=ID.Map.empty; star=None; leaf=Leaf.empty;}

  let is_empty n = n.star = None && ID.Map.is_empty n.map && Leaf.is_empty n.leaf

  exception NoSuchTrie

  let find_sub map key =
    try ID.Map.find key map
    with Not_found -> raise NoSuchTrie

  (** get/add/remove the leaf for the given term. The
      continuation k takes the leaf, and returns a leaf option
      that replaces the old leaf.
      This function returns the new trie. *)
  let goto_leaf trie t k =
    (* the root of the tree *)
    let root = trie in
  (* function to go to the given leaf, building it if needed.
        [iter] is an iterator on the current subterm *)
    let rec goto trie iter rebuild =
      match iter with
      | None ->
        begin match k trie.leaf with
          | leaf' when leaf' == trie.leaf -> root (* no change, return same tree *)
          | leaf' -> rebuild {trie with leaf=leaf'; }
        end
      | Some i ->
        match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.DB _) ->
            let subtrie = match trie.star with
              | None -> empty ()
              | Some trie' -> trie'
            in
            let rebuild subtrie =
              if is_empty subtrie
                then rebuild {trie with star=None; }
                else rebuild {trie with star=Some subtrie ;}
            in
            goto subtrie (next i) rebuild
          | T.Classic.App (s, _, _) ->
            let subtrie =
              try find_sub trie.map s
              with NoSuchTrie -> empty ()
            in
            let rebuild subtrie =
              if is_empty subtrie
                then rebuild {trie with map=ID.Map.remove s trie.map; }
                else rebuild {trie with map=ID.Map.add s subtrie trie.map ;}
            in
            goto subtrie (next i) rebuild
          | T.Classic.AppBuiltin _
          | T.Classic.NonFO -> assert false (* TODO: consider like a var *)
  in
  goto trie (iterate t) (fun t -> t)

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

  let retrieve ?(subst=S.empty) ~sign dt t k =
    Util.enter_prof prof_npdtree_retrieve;
    (* extended callback *)
    let k' (t', eqn, subst) =
      let _, r, sign' = E.extract eqn in
      if sign = sign' then k (t', r, eqn, subst)
    in
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie iter =
      match iter with
      | None ->
          Util.exit_prof prof_npdtree_retrieve;
          Leaf.fold_match ~subst (Scoped.set dt trie.leaf) t k';
          Util.enter_prof prof_npdtree_retrieve;
      | Some i ->
        match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.DB _) ->
            begin match trie.star with
            | None -> ()
            | Some subtrie ->
              traverse subtrie (next i)  (* match "*" against "*" *)
            end
          | T.Classic.App (s, _, _) ->
            begin try
              let subtrie = find_sub trie.map s in
              traverse subtrie (next i)
            with NoSuchTrie -> ()
            end;
            begin match trie.star with
              | None -> ()
              | Some subtrie ->
                traverse subtrie (skip i)  (* skip subterm *)
            end
          | T.Classic.AppBuiltin _
          | T.Classic.NonFO -> assert false (* TODO: consder like a var? *)
    in
    try
      traverse (fst dt) (iterate (fst t));
      Util.exit_prof prof_npdtree_retrieve;
    with e ->
      Util.exit_prof prof_npdtree_retrieve;
      raise e

  (** iterate on all (term -> value) in the tree *)
  let rec iter dt k =
    Leaf.iter dt.leaf k;
    begin match dt.star with
    | None -> ()
    | Some trie' -> iter trie' k
    end;
    ID.Map.iter (fun _ trie' -> iter trie' k) dt.map

  let size dt =
    let n = ref 0 in
    iter dt (fun _ _ -> incr n);
    !n

  let _as_graph =
    LazyGraph.make ~eq:(==)
      (fun t ->
        let s1 =
          (match t.star with
            | None -> Sequence.empty
            | Some t' -> Sequence.singleton ("*", t')
          )
        and s2 = ID.Map.to_seq t.map
          |> Sequence.map (fun (sym, t') -> ID.to_string sym, t')
        in
        LazyGraph.Node(t, t, Sequence.append s1 s2)
      )

  let _as_dot_graph =
    LazyGraph.map
      ~vertices:(fun t ->
        let len = Leaf.size t.leaf in
        [`Shape "circle"; `Label (string_of_int len)]
      )
      ~edges:(fun e -> [`Label e])
      _as_graph

  let to_dot out t =
    LazyGraph.Dot.pp ~name:"NPDtree" _as_dot_graph out (Sequence.singleton t);
    Format.pp_print_flush out ();
    ()
end

(** {2 General purpose index} *)

module SIMap = Sequence.Map.Make(struct
  type t = ID.t * int
  let compare (s1,i1) (s2,i2) =
    if i1 = i2 then ID.compare s1 s2 else i1-i2
end)

module MakeTerm(X : Set.OrderedType) = struct
  module Leaf = Index.MakeLeaf(X)

  type elt = X.t

  type t = {
    star : t option;  (* by variable *)
    map : t SIMap.t;  (* by symbol+arity *)
    leaf : Leaf.t;    (* leaves *)
  }  (** The discrimination tree *)

  let empty () = {map=SIMap.empty; star=None; leaf=Leaf.empty;}

  let is_empty n = n.star = None && SIMap.is_empty n.map && Leaf.is_empty n.leaf

  exception NoSuchTrie

  let find_sub map key =
    try SIMap.find key map
    with Not_found -> raise NoSuchTrie

  (** get/add/remove the leaf for the given term. The
      continuation k takes the leaf, and returns a leaf option
      that replaces the old leaf.
      This function returns the new trie. *)
  let goto_leaf trie t k =
    (* the root of the tree *)
    let root = trie in
    (* function to go to the given leaf, building it if needed. *)
    let rec goto trie iter rebuild = match iter with
      | None ->
        begin match k trie.leaf with
          | leaf' when leaf' == trie.leaf -> root (* no change, return same tree *)
          | leaf' -> rebuild {trie with leaf=leaf'; }
        end
      | Some i ->
        match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.DB _) ->
            let subtrie = match trie.star with
              | None -> empty ()
              | Some trie' -> trie'
            in
            let rebuild subtrie =
              if is_empty subtrie
                then rebuild {trie with star=None; }
                else rebuild {trie with star=Some subtrie ;}
            in
            goto subtrie (next i) rebuild
          | T.Classic.App (s, _, l) ->
            let arity = List.length l in
            let subtrie =
              try find_sub trie.map (s,arity)
              with NoSuchTrie -> empty ()
            in
            let rebuild subtrie =
              if is_empty subtrie
                then rebuild {trie with map=SIMap.remove (s,arity) trie.map; }
                else rebuild {trie with map=SIMap.add (s,arity) subtrie trie.map ;}
            in
            goto subtrie (next i) rebuild
          | T.Classic.AppBuiltin _
          | T.Classic.NonFO -> assert false
  in
  goto trie (iterate t) (fun t -> t)

  let add trie t data =
    let k leaf = Leaf.add leaf t data in
    goto_leaf trie t k

  let remove trie t data =
    let k leaf = Leaf.remove leaf t data in
    goto_leaf trie t k

  (* skip one term in the tree. Calls [k] with [acc] on corresponding
    subtries. *)
  let skip_tree trie k =
    (* [n]: number of branches to skip (corresponding to subterms) *)
    let rec skip trie n k =
      if n = 0
        then k trie
        else
          begin match trie.star with
          | None -> ()
          | Some trie' -> skip trie' (n-1) k
          end;
          SIMap.iter
            (fun (_,arity) trie' -> skip trie' (n+arity-1) k)
            trie.map
    in
    skip trie 1 k

  let retrieve_unifiables ?(subst=S.empty) dt t k =
    Util.enter_prof prof_npdtree_term_unify;
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie iter = match iter with
      | None ->
        Util.exit_prof prof_npdtree_term_unify;
        Leaf.fold_unify ~subst (Scoped.set dt trie.leaf) t k;
        Util.enter_prof prof_npdtree_term_unify;
      | Some i ->
        match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.DB _) ->
            (* skip one term in all branches of the trie *)
            skip_tree trie
              (fun subtrie -> traverse subtrie (next i))
          | T.Classic.App (s, _, l) ->
            let arity = List.length l in
            begin try
              let subtrie = SIMap.find (s,arity) trie.map in
              traverse subtrie (next i)
            with Not_found -> ()
            end;
            begin match trie.star with
              | None -> ()
              | Some subtrie ->
                traverse subtrie (skip i)  (* skip subterm of [t] *)
            end
          | T.Classic.AppBuiltin _
          | T.Classic.NonFO -> assert false
    in
    try
      traverse (fst dt) (iterate (fst t));
      Util.exit_prof prof_npdtree_term_unify;
    with e ->
      Util.exit_prof prof_npdtree_term_unify;
      raise e

  let retrieve_generalizations ?(subst=S.empty) dt t k =
    Util.enter_prof prof_npdtree_term_generalizations;
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie iter = match iter with
      | None ->
        Util.exit_prof prof_npdtree_term_generalizations;
        Leaf.fold_match ~subst (Scoped.set dt trie.leaf) t k;
        Util.enter_prof prof_npdtree_term_generalizations;
      | Some i ->
          match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.DB _) ->
            begin match trie.star with
            | None -> ()
            | Some subtrie ->
              traverse subtrie (next i) (* match "*" against "*" only *)
            end
          | T.Classic.App (s, _, l) ->
            let arity = List.length l in
            begin try
              let subtrie = SIMap.find (s,arity) trie.map in
              traverse subtrie (next i)
            with Not_found -> ()
            end;
            begin match trie.star with
              | None -> ()
              | Some subtrie ->
                traverse subtrie (skip i)  (* skip subterm *)
            end
          | T.Classic.AppBuiltin _
          | T.Classic.NonFO -> assert false
    in
    try
      traverse (fst dt) (iterate (fst t));
      Util.exit_prof prof_npdtree_term_generalizations;
    with e ->
      Util.exit_prof prof_npdtree_term_generalizations;
      raise e

  let retrieve_specializations ?(subst=S.empty) dt t k =
    Util.enter_prof prof_npdtree_term_specializations;
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie iter = match iter with
      | None ->
        Util.exit_prof prof_npdtree_term_specializations;
        Leaf.fold_matched ~subst (Scoped.set dt trie.leaf) t k;
        Util.enter_prof prof_npdtree_term_specializations;
      | Some i ->
          match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.DB _) ->
            (* match * against any subterm *)
            skip_tree trie
              (fun subtrie -> traverse subtrie (next i))
          | T.Classic.App (s, _, l) ->
            (* only same symbol *)
            let arity = List.length l in
            begin try
              let subtrie = SIMap.find (s,arity) trie.map in
              traverse subtrie (next i)
            with Not_found -> ()
            end
          | T.Classic.AppBuiltin _
          | T.Classic.NonFO -> assert false
    in
    try
      traverse (fst dt) (iterate (fst t));
      Util.exit_prof prof_npdtree_term_specializations;
    with e ->
      Util.exit_prof prof_npdtree_term_specializations;
      raise e

  (** iterate on all (term -> value) in the tree *)
  let rec iter dt k =
    Leaf.iter dt.leaf k;
    begin match dt.star with
    | None -> ()
    | Some trie' -> iter trie' k
    end;
    SIMap.iter (fun _ trie' -> iter trie' k) dt.map

  let rec fold dt k acc =
    let acc = Leaf.fold dt.leaf acc k in
    let acc = match dt.star with
    | None -> acc
    | Some trie' -> fold trie' k acc
    in
    SIMap.fold (fun _ trie' acc -> fold trie' k acc) dt.map acc

  let size dt =
    let n = ref 0 in
    iter dt (fun _ _ -> incr n);
    !n

  let name = "npdtree"

  let _as_graph =
    LazyGraph.make ~eq:(==)
      (fun t ->
        let s1 =
          (match t.star with
            | None -> Sequence.empty
            | Some t' -> Sequence.singleton ("*", t')
          )
        and s2 = SIMap.to_seq t.map
          |> Sequence.map
              (fun ((sym,i), t') ->
               CCFormat.sprintf "%a/%d" ID.pp sym i, t')
        in
        LazyGraph.Node(t, t, Sequence.append s1 s2)
      )

  (* TODO: print leaf itself *)
  let as_graph_ =
    LazyGraph.map
      ~vertices:(fun t ->
        let len = Leaf.size t.leaf in
        [`Shape "circle"; `Label (string_of_int len)]
      )
      ~edges:(fun e -> [`Label e])
      _as_graph

  let to_dot _ out t =
    LazyGraph.Dot.pp ~name as_graph_ out (Sequence.singleton t);
    Format.pp_print_flush out ();
    ()
end
