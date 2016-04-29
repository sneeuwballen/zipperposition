
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Perfect Discrimination Tree} *)

module ST = InnerTerm
module T = FOTerm
module S = Substs

let prof_dtree_retrieve = Util.mk_profiler "dtree_retrieve"

(** {2 Term traversal}
Term traversal in prefix order. This is akin to lazy transformation
to a flatterm. *)

type character =
  | Symbol of ID.t
  | BoundVariable of int
  | Variable of Type.t HVar.t
  | NonFO

type iterator = {
  cur_char : character;
  cur_term : T.t;
  stack : T.t list list; (* skip: drop head, next: first of head *)
}

let char_to_int_ = function
  | Symbol _ -> 0
  | BoundVariable _ -> 1
  | Variable _ -> 2
  | NonFO -> 3

let compare_char c1 c2 =
  (* compare variables by index *)
  let compare_vars v1 v2 =
    let c = HVar.compare v1 v2 in
    if c=0 then Type.compare (HVar.ty v1) (HVar.ty v2) else c
  in
  match c1, c2 with
  | Symbol s1, Symbol s2 -> ID.compare s1 s2
  | BoundVariable i, BoundVariable j -> i - j
  | Variable v1, Variable v2 -> compare_vars v1 v2
  | NonFO, NonFO -> 0
  | _ -> char_to_int_ c1 - char_to_int_ c2

let eq_char c1 c2 = compare_char c1 c2 = 0

(** first symbol of t, or variable *)
let term_to_char t =
  match T.Classic.view t with
  | T.Classic.Var v -> Variable v
  | T.Classic.DB i -> BoundVariable i
  | T.Classic.App (f, _) -> Symbol f
  | T.Classic.AppBuiltin _
  | T.Classic.NonFO -> NonFO

let pp_char out = function
  | Variable v -> HVar.pp out v
  | BoundVariable i -> Format.fprintf out "Y%d" i
  | Symbol f -> ID.pp out f
  | NonFO -> CCFormat.string out "<nonfo>"

let open_term ~stack t =
  let cur_char = term_to_char t in
  match T.view t with
  | T.Var _
  | T.DB _
  | T.AppBuiltin _
  | T.Const _ ->
      Some {cur_char; cur_term=t; stack=[]::stack;}
  | T.App (_, l) ->
      Some {cur_char; cur_term=t; stack=l::stack;}

let rec next_rec stack = match stack with
  | [] -> None
  | []::stack' -> next_rec stack'
  | (t::next')::stack' ->
      open_term ~stack:(next'::stack') t

let skip iter = match iter.stack with
  | [] -> None
  | _next::stack' -> next_rec stack'

let next iter = next_rec iter.stack

(* Iterate on a term *)
let iterate term = open_term ~stack:[] term

(* convert term to list of var/symbol *)
let to_list t =
  let rec getnext acc iter =
    let acc' = iter.cur_char :: acc in
    match next iter with
    | None -> List.rev acc'
    | Some iter' ->
        getnext acc' iter'
  in
  match iterate t with
  | None -> assert false
  | Some i -> getnext [] i

module CharMap = CCMap.Make(struct
  type t = character
  let compare = compare_char
end)

(** {2 Discrimination tree} *)

module Make(E : Index.EQUATION) = struct
  module E = E

  type rhs = E.rhs

  type trie =
    | TrieNode of trie CharMap.t       (** map atom -> trie *)
    | TrieLeaf of (T.t * E.t * int) list
      (** leaf with (term, value, priority) list *)

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
        begin match k l with
        | new_leaf when leaf == new_leaf -> root  (* no change, return same tree *)
        | new_leaf -> rebuild new_leaf           (* replace by new leaf *)
        end
      | TrieNode m, c::t' ->
        begin try  (* insert in subtrie *)
          let subtrie = CharMap.find c m in
          let rebuild' subtrie = match subtrie with
            | _ when empty_trie subtrie -> rebuild (TrieNode (CharMap.remove c m))
            | _ -> rebuild (TrieNode (CharMap.add c subtrie m))
          in
          goto subtrie t' rebuild'
        with Not_found -> (* no subtrie found *)
          let subtrie = if t' = [] then TrieLeaf [] else TrieNode CharMap.empty
          and rebuild' subtrie = match subtrie with
            | _ when empty_trie subtrie -> root  (* same tree *)
            | _ -> rebuild (TrieNode (CharMap.add c subtrie m))
          in
          goto subtrie t' rebuild'
        end
      | TrieNode _, [] -> assert false (* ill-formed term *)
      | TrieLeaf _, _ -> assert false  (* wrong arity *)
    in
    goto trie t (fun t -> t)

  type t = trie

  let empty () = TrieNode CharMap.empty

  let is_empty = empty_trie

  let add dt eqn =
    let t, _, _ = E.extract eqn in
    let priority = E.priority eqn in
    let chars = to_list t in
    let k l =
      let l' = (t, eqn, priority)::l in
      TrieLeaf (List.stable_sort (fun (_, _, p1) (_, _, p2) -> p1 - p2) l')
      (* TODO: linear-time insertion into a sorted list *)
    in
    let tree = goto_leaf dt chars k in
    tree

  let remove dt eqn =
    let t, _, _ = E.extract eqn in
    let chars = to_list t in
    let k l =
      (* remove tuples that match *)
      let l' = List.filter
        (fun (t', eqn', _) -> t' != t || E.compare eqn eqn' <> 0) l
      in
      TrieLeaf l'
    in
    let tree = goto_leaf dt chars k in
    tree

  let add_seq = Sequence.fold add
  let add_list = List.fold_left add

  let remove_seq dt seq =
    Sequence.fold remove dt seq

  let retrieve ?(subst=S.empty) ~sign dt t k =
    Util.enter_prof prof_dtree_retrieve;
    (* recursive traversal of the trie, following paths compatible with t *)
    let rec traverse trie iter subst =
      match trie, iter with
      | TrieLeaf l, None ->  (* yield all equations, they all match *)
        List.iter
          (fun (_, eqn, _) ->
            let l, r, sign' = E.extract eqn in
            if sign = sign' then k (l, r, eqn, subst))
          l
      | TrieNode m, Some i ->
        (* "lazy" transformation to flatterm *)
        let t_pos = i.cur_term in
        let c1 = i.cur_char in
        CharMap.iter
          (fun c2 subtrie ->
            match c2 with
            | Variable v2 ->
              (* deal with the variable branche in the trie *)
              begin match S.FO.get_var subst (Scoped.set dt (v2:>ST.t HVar.t)) with
                | None ->
                  (* not bound, try to bind and continue *)
                  begin
                    try
                      let subst =
                        Unif.FO.bind subst
                          (Scoped.set dt v2) (Scoped.set t t_pos) in
                      traverse subtrie (skip i) subst
                    with Unif.Fail -> () (* incompatible binding, or occur check *)
                  end
                | Some t' ->
                  (* already bound, check consistency *)
                  if Unif.FO.equal ~subst (Scoped.set t t_pos) t'
                  then traverse subtrie (skip i) subst
              end
            | _ when eq_char c2 c1 ->
            (* explore branch that has the same symbol, if any *)
              assert (not (T.is_var t_pos));
              traverse subtrie (next i) subst;
            | _ -> ())
          m
      | TrieNode _, None
      | TrieLeaf _, Some _ -> ()
    in
    traverse (fst dt) (iterate (fst t)) subst;
    Util.exit_prof prof_dtree_retrieve;
    ()

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

  let _as_graph =
    CCGraph.make_labelled_tuple
      (function
        | TrieLeaf _ -> Sequence.empty
        | TrieNode m -> CharMap.to_seq m)

  (* TODO: print leaf itself *)

  let rec equal_ a b = match a, b with
    | TrieLeaf l1, TrieLeaf l2 -> l1==l2
    | TrieNode m1, TrieNode m2 -> m1==m2 || CharMap.equal equal_ m1 m2
    | TrieLeaf _, _ | TrieNode _, _ -> false

  let to_dot out t =
    Util.debugf ~section:Util.Section.zip 2
      "@[<2>print graph of size %d@]" (fun k->k (size t));
    let pp = CCGraph.Dot.pp
      ~eq:equal_
      ~tbl:(CCGraph.mk_table ~eq:equal_ ~hash:Hashtbl.hash 128)
      ~attrs_v:(function
        | TrieLeaf l ->
            let len = List.length l in
            [`Shape "circle"; `Label (string_of_int len)]
        | TrieNode _ -> [`Shape "circle"])
      ~attrs_e:(fun (_, e ,_) ->
        let e = CCFormat.to_string pp_char e in
        [`Label e])
      ~name:"NPDtree" ~graph:_as_graph
    in
    Format.fprintf out "@[<2>%a@]@." pp t;
end

module Default = Make(Index.BasicEquation)
