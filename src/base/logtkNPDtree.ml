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

module T = LogtkFOTerm
module S = LogtkSubsts
module SMap = LogtkSymbol.Map

let prof_npdtree_retrieve = LogtkUtil.mk_profiler "NPLogtkDtree_retrieve"
let prof_npdtree_term_unify = LogtkUtil.mk_profiler "NPLogtkDtree_term_unify"
let prof_npdtree_term_generalizations =
  LogtkUtil.mk_profiler "NPLogtkDtree_term_generalizations"
let prof_npdtree_term_specializations =
  LogtkUtil.mk_profiler "NPLogtkDtree_term_specializations"

(** {2 Term traversal} *)

(** Term traversal in prefix order. This is akin to lazy transformation
    to a flatterm. *)

type iterator = {
  cur_term : T.t;
  stack : T.t list list; (* skip: drop head, next: first of head *)
}

let open_term ~stack t = match T.view t with
  | T.Var _
  | T.BVar _
  | T.TyApp _
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

module Make(E : LogtkIndex.EQUATION) = struct
  module E = E

  type rhs = E.rhs

  module Leaf = LogtkIndex.MakeLeaf(E)

  type t = {
    star : t option;  (* by variable *)
    map : t SMap.t;   (* by symbol *)
    leaf : Leaf.t;    (* leaves *)
  }  (** The discrimination tree *)

  let empty () = {map=SMap.empty; star=None; leaf=Leaf.empty;}

  let is_empty n = n.star = None && SMap.is_empty n.map && Leaf.is_empty n.leaf

  exception NoSuchTrie

  let find_sub map key =
    try SMap.find key map
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
          | (T.Classic.Var _ | T.Classic.BVar _) ->
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
                then rebuild {trie with map=SMap.remove s trie.map; }
                else rebuild {trie with map=SMap.add s subtrie trie.map ;}
            in
            goto subtrie (next i) rebuild
          | T.Classic.NonFO -> assert false (* TODO *)
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

  let retrieve ?(allow_open=false) ?(subst=S.empty) ~sign dt sc_dt t sc_t acc k =
    LogtkUtil.enter_prof prof_npdtree_retrieve;
    (* extended callback *)
    let k' acc t' eqn subst =
      let _, r, sign' = E.extract eqn in
      if sign = sign' then k acc t' r eqn subst else acc
    in
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie acc iter =
      match iter with
      | None ->
          LogtkUtil.exit_prof prof_npdtree_retrieve;
          let acc = Leaf.fold_match ~allow_open ~subst trie.leaf sc_dt t sc_t acc k' in
          LogtkUtil.enter_prof prof_npdtree_retrieve;
          acc
      | Some i ->
        match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.BVar _) ->
            begin match trie.star with
            | None -> acc
            | Some subtrie ->
              traverse subtrie acc (next i)  (* match "*" against "*" *)
            end
          | T.Classic.App (s, _, _) ->
            let acc =
              try
                let subtrie = find_sub trie.map s in
                traverse subtrie acc (next i)
              with NoSuchTrie -> acc
            in
            begin match trie.star with
              | None -> acc
              | Some subtrie ->
                traverse subtrie acc (skip i)  (* skip subterm *)
            end
          | T.Classic.NonFO -> assert false
    in
    try
      let acc = traverse dt acc (iterate t) in
      LogtkUtil.exit_prof prof_npdtree_retrieve;
      acc
    with e ->
      LogtkUtil.exit_prof prof_npdtree_retrieve;
      raise e

  (** iterate on all (term -> value) in the tree *)
  let rec iter dt k =
    Leaf.iter dt.leaf k;
    begin match dt.star with
    | None -> ()
    | Some trie' -> iter trie' k
    end;
    SMap.iter (fun _ trie' -> iter trie' k) dt.map

  let size dt =
    let n = ref 0 in
    iter dt (fun _ _ -> incr n);
    !n

  let _as_graph =
    LogtkLazyGraph.make ~eq:(==)
      (fun t ->
        let s1 =
          (match t.star with
            | None -> Sequence.empty
            | Some t' -> Sequence.singleton ("*", t')
          )
        and s2 = SMap.to_seq t.map
          |> Sequence.map (fun (sym, t') -> LogtkSymbol.to_string sym, t')
        in
        LogtkLazyGraph.Node(t, t, Sequence.append s1 s2)
      )

  let _as_dot_graph =
    LogtkLazyGraph.map
      ~vertices:(fun t ->
        let len = Leaf.size t.leaf in
        [`Shape "circle"; `Label (string_of_int len)]
      )
      ~edges:(fun e -> [`Label e])
      _as_graph

  let to_dot buf t =
    let fmt = Format.formatter_of_buffer buf in
    LogtkLazyGraph.Dot.pp ~name:"NPLogtkDtree" _as_dot_graph fmt (Sequence.singleton t);
    Format.pp_print_flush fmt ();
    ()
end

(** {2 General purpose index} *)

module SIMap = Sequence.Map.Make(struct
  type t = LogtkSymbol.t * int
  let compare (s1,i1) (s2,i2) =
    if i1 = i2 then LogtkSymbol.cmp s1 s2 else i1-i2
end)

module MakeTerm(X : Set.OrderedType) = struct
  module Leaf = LogtkIndex.MakeLeaf(X)

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
          | (T.Classic.Var _ | T.Classic.BVar _) ->
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
  let skip_tree trie acc k =
    (* [n]: number of branches to skip (corresponding to subterms) *)
    let rec skip trie n acc k =
      if n = 0
        then k acc trie
        else
          let acc = match trie.star with
          | None -> acc
          | Some trie' -> skip trie' (n-1) acc k
          in
          SIMap.fold
            (fun (_,arity) trie' acc -> skip trie' (n+arity-1) acc k)
            trie.map acc
    in
    skip trie 1 acc k

  let retrieve_unifiables ?(subst=S.empty) dt sc_dt t sc_t acc k =
    LogtkUtil.enter_prof prof_npdtree_term_unify;
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie acc iter = match iter with
      | None ->
        LogtkUtil.yield_prof prof_npdtree_term_unify;
        let acc = Leaf.fold_unify ~subst trie.leaf sc_dt t sc_t acc k in
        LogtkUtil.enter_prof prof_npdtree_term_unify;
        acc
      | Some i ->
        match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.BVar _) ->
            (* skip one term in all branches of the trie *)
            skip_tree trie acc
              (fun acc subtrie -> traverse subtrie acc (next i))
          | T.Classic.App (s, _, l) ->
            let arity = List.length l in
            let acc =
              try
                let subtrie = SIMap.find (s,arity) trie.map in
                traverse subtrie acc (next i)
              with Not_found -> acc
            in
            begin match trie.star with
              | None -> acc
              | Some subtrie ->
                traverse subtrie acc (skip i)  (* skip subterm of [t] *)
            end
          | T.Classic.NonFO -> assert false
    in
    try
      let acc = traverse dt acc (iterate t) in
      LogtkUtil.exit_prof prof_npdtree_term_unify;
      acc
    with e ->
      LogtkUtil.exit_prof prof_npdtree_term_unify;
      raise e

  let retrieve_generalizations ?(allow_open=false) ?(subst=S.empty)
  dt sc_dt t sc_t acc k =
    LogtkUtil.enter_prof prof_npdtree_term_generalizations;
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie acc iter = match iter with
      | None ->
        LogtkUtil.yield_prof prof_npdtree_term_generalizations;
        let acc = Leaf.fold_match ~allow_open ~subst trie.leaf sc_dt t sc_t acc k in
        LogtkUtil.enter_prof prof_npdtree_term_generalizations;
        acc
      | Some i ->
          match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.BVar _) ->
            begin match trie.star with
            | None -> acc
            | Some subtrie ->
              traverse subtrie acc (next i) (* match "*" against "*" only *)
            end
          | T.Classic.App (s, _, l) ->
            let arity = List.length l in
            let acc =
              try
                let subtrie = SIMap.find (s,arity) trie.map in
                traverse subtrie acc (next i)
              with Not_found -> acc
            in
            begin match trie.star with
              | None -> acc
              | Some subtrie ->
                traverse subtrie acc (skip i)  (* skip subterm *)
            end
          | T.Classic.NonFO -> assert false
    in
    try
      let acc = traverse dt acc (iterate t) in
      LogtkUtil.exit_prof prof_npdtree_term_generalizations;
      acc
    with e ->
      LogtkUtil.exit_prof prof_npdtree_term_generalizations;
      raise e

  let retrieve_specializations ?(allow_open=false) ?(subst=S.empty)
  dt sc_dt t sc_t acc k =
    LogtkUtil.enter_prof prof_npdtree_term_specializations;
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie acc iter = match iter with
      | None ->
        LogtkUtil.yield_prof prof_npdtree_term_specializations;
        let acc = Leaf.fold_matched ~allow_open ~subst trie.leaf sc_dt t sc_t acc k in
        LogtkUtil.enter_prof prof_npdtree_term_specializations;
        acc
      | Some i ->
          match T.Classic.view i.cur_term with
          | (T.Classic.Var _ | T.Classic.BVar _) ->
            (* match * against any subterm *)
            skip_tree trie acc
              (fun acc subtrie -> traverse subtrie acc (next i))
          | T.Classic.App (s, _, l) ->
            (* only same symbol *)
            let arity = List.length l in
            begin try
              let subtrie = SIMap.find (s,arity) trie.map in
              traverse subtrie acc (next i)
            with Not_found -> acc
            end
          | T.Classic.NonFO -> assert false
    in
    try
      let acc = traverse dt acc (iterate t) in
      LogtkUtil.exit_prof prof_npdtree_term_specializations;
      acc
    with e ->
      LogtkUtil.exit_prof prof_npdtree_term_specializations;
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
    LogtkLazyGraph.make ~eq:(==)
      (fun t ->
        let s1 =
          (match t.star with
            | None -> Sequence.empty
            | Some t' -> Sequence.singleton ("*", t')
          )
        and s2 = SIMap.to_seq t.map
          |> Sequence.map
              (fun ((sym,i), t') ->
               LogtkUtil.sprintf "%a/%d" LogtkSymbol.pp sym i, t')
        in
        LogtkLazyGraph.Node(t, t, Sequence.append s1 s2)
      )

  (* TODO: print leaf itself *)
  let _as_dot_graph pp_elem =
    LogtkLazyGraph.map
      ~vertices:(fun t ->
        let len = Leaf.size t.leaf in
        [`Shape "circle"; `Label (string_of_int len)]
      )
      ~edges:(fun e -> [`Label e])
      _as_graph

  let to_dot pp_elem buf t =
    let fmt = Format.formatter_of_buffer buf in
    LogtkLazyGraph.Dot.pp ~name (_as_dot_graph pp_elem) fmt (Sequence.singleton t);
    Format.pp_print_flush fmt ();
    ()
end
