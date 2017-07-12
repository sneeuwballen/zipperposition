
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Perfect Discrimination Tree} *)

module ST = InnerTerm
module T = Term
module S = Subst

let prof_dtree_retrieve = Util.mk_profiler "dtree_retrieve"

(** {2 Term traversal}

    Term traversal in prefix order. This is akin to lazy transformation
    to a flatterm. *)

type character =
  | Symbol of ID.t
  | Variable of Type.t HVar.t
  | Subterm of T.t (* opaque term, just do matching *)

type iterator = {
  cur_char : character;
  cur_term : T.t;
  stack : T.t list list; (* skip: drop head, next: first of head *)
}

let char_to_int_ = function
  | Symbol _ -> 0
  | Variable _ -> 2
  | Subterm _ -> 3

let compare_char c1 c2 =
  (* compare variables by index *)
  let compare_vars v1 v2 =
    let c = HVar.compare Type.compare v1 v2 in
    if c=0 then Type.compare (HVar.ty v1) (HVar.ty v2) else c
  in
  match c1, c2 with
    | Symbol s1, Symbol s2 -> ID.compare s1 s2
    | Variable v1, Variable v2 -> compare_vars v1 v2
    | Subterm t1, Subterm t2 -> T.compare t1 t2
    | _ -> char_to_int_ c1 - char_to_int_ c2

let eq_char c1 c2 = compare_char c1 c2 = 0

(** first symbol of t, or variable *)
let term_to_char t : character * T.t list =
  match T.Classic.view t with
    | T.Classic.Var v -> Variable v, []
    | _ when Type.is_fun (T.ty t) -> Subterm t, [] (* partial app *)
    | T.Classic.App (f, l) -> Symbol f, l
    | T.Classic.DB _
    | T.Classic.AppBuiltin _
    | T.Classic.NonFO -> Subterm t, []

let pp_char out = function
  | Variable v -> Type.pp_typed_var out v
  | Symbol f -> ID.pp out f
  | Subterm t -> CCFormat.hbox T.pp out t

(* parameter: maximum depth before we start using Subterm *)
let max_depth_ = ref 3

let open_term ~stack t =
  if List.length stack > !max_depth_
  then (
    (* opaque. Do not enter the term. *)
    let cur_char = Subterm t in
    {cur_char; cur_term=t; stack=[]::stack}
  ) else (
    let cur_char, l = term_to_char t in
    {cur_char; cur_term=t; stack=l::stack;}
  )

let rec next_rec stack = match stack with
  | [] -> None
  | []::stack' -> next_rec stack'
  | (t::next')::stack' ->
    Some (open_term ~stack:(next'::stack') t)

let skip iter = match iter.stack with
  | [] -> None
  | _next::stack' -> next_rec stack'

let next iter = next_rec iter.stack

(* Iterate on a term *)
let iterate term = Some (open_term ~stack:[] term)

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

  type trie = {
    map : trie CharMap.t; (** map atom -> trie *)
    leaf : (T.t * E.t * int) list; (** leaf with (term, value, priority) list *)
  } (* The discrimination tree *)

  let empty_trie = {map=CharMap.empty; leaf=[]}

  let is_empty n = n.leaf = [] && CharMap.is_empty n.map

  (** get/add/remove the leaf for the given flatterm. The
      continuation k takes the leaf, and returns a leaf option
      that replaces the old leaf.
      This function returns the new trie. *)
  let goto_leaf trie t k =
    (* the root of the tree *)
    let root = trie in
    (* function to go to the given leaf, building it if needed *)
    let rec goto trie t rebuild =
      match t with
        | [] -> (* look at leaf *)
          begin match k trie.leaf with
            | new_leaf when trie.leaf == new_leaf -> root (* no change, return same tree *)
            | new_leaf -> rebuild {trie with leaf=new_leaf} (* replace by new leaf *)
          end
        | c::t' ->
          begin match CharMap.get c trie.map with
            | Some subtrie ->
              let rebuild' subtrie = match subtrie with
                | _ when is_empty subtrie ->
                  rebuild {trie with map=CharMap.remove c trie.map}
                | _ -> rebuild {trie with map=CharMap.add c subtrie trie.map}
              in
              goto subtrie t' rebuild'
            | None ->
              let subtrie = empty_trie in
              let rebuild' subtrie = match subtrie with
                | _ when is_empty subtrie -> root  (* same tree *)
                | _ -> rebuild {trie with map=CharMap.add c subtrie trie.map}
              in
              goto subtrie t' rebuild'
          end
    in
    goto trie t (fun t -> t)

  type t = trie

  let empty () = empty_trie

  let add dt eqn =
    let t, _, _ = E.extract eqn in
    let priority = E.priority eqn in
    let chars = to_list t in
    let k l =
      let l' = (t, eqn, priority)::l in
      List.stable_sort (fun (_, _, p1) (_, _, p2) -> p1 - p2) l'
      (* TODO: linear-time insertion into a sorted list *)
    in
    let tree = goto_leaf dt chars k in
    tree

  let remove dt eqn =
    let t, _, _ = E.extract eqn in
    let chars = to_list t in
    let k l =
      (* remove tuples that match *)
      List.filter
        (fun (t', eqn', _) -> t' != t || E.compare eqn eqn' <> 0) l
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
      match iter with
        | None ->  (* yield all equations, they all match *)
          List.iter
            (fun (_, eqn, _) ->
               let l, r, sign' = E.extract eqn in
               if sign = sign' then k (l, r, eqn, subst))
            trie.leaf
        | Some i ->
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
                       (* not bound: try to match types + bind, then continue *)
                       begin
                         try
                           let subst =
                             Unif.Ty.matching ~subst
                               ~pattern:(Scoped.set dt (HVar.ty v2))
                               (Scoped.set t (T.ty t_pos))
                           in
                           let subst =
                             Unif.FO.bind ~check:false subst
                               (Scoped.set dt v2) (Scoped.set t t_pos) in
                           traverse subtrie (skip i) subst
                         with Unif.Fail -> () (* incompatible binding, or occur check *)
                       end
                     | Some t' ->
                       (* already bound, check consistency *)
                       if Unif.FO.equal ~subst (Scoped.set t t_pos) t'
                       then traverse subtrie (skip i) subst
                   end
                 | Subterm t2 ->
                   (* fallback to matching *)
                   begin
                     try
                       let subst =
                         Unif.FO.matching
                           ~subst ~pattern:(Scoped.set dt t2) (Scoped.set t t_pos)
                       in
                       traverse subtrie (skip i) subst
                     with Unif.Fail -> ()
                   end
                 | _ when eq_char c2 c1 ->
                   (* explore branch that has the same symbol, if any *)
                   assert (not (T.is_var t_pos));
                   traverse subtrie (next i) subst;
                 | _ -> ())
            trie.map
    in
    traverse (fst dt) (iterate (fst t)) subst;
    Util.exit_prof prof_dtree_retrieve;
    ()

  (** iterate on all (term -> value) in the tree *)
  let iter dt k =
    let rec iter trie =
      List.iter (fun (t, v, _) -> k t v) trie.leaf;
      CharMap.iter (fun _ sub_dt -> iter sub_dt) trie.map
    in iter dt

  let size dt =
    let n = ref 0 in
    iter dt (fun _ _ -> incr n);
    !n

  let _as_graph =
    CCGraph.make (fun trie -> CharMap.to_seq trie.map)

  (* TODO: print leaf itself *)

  let rec equal_ a b =
    a==b ||
    (a.leaf == b.leaf && CharMap.equal equal_ a.map b.map)

  let to_dot out t =
    Util.debugf ~section:Util.Section.base 2
      "@[<2>print graph of size %d@]" (fun k->k (size t));
    let pp = CCGraph.Dot.pp
        ~eq:equal_
        ~tbl:(CCGraph.mk_table ~eq:equal_ ~hash:Hashtbl.hash 128)
        ~attrs_v:(fun trie ->
          let shape = if CharMap.is_empty trie.map then "box" else "circle" in
          let len = List.length trie.leaf in
          [`Shape shape; `Label (string_of_int len)])
        ~attrs_e:(fun e ->
          let e = CCFormat.to_string pp_char e in
          [`Label e])
        ~name:"NPDtree" ~graph:_as_graph
    in
    Format.fprintf out "@[<2>%a@]@." pp t;
end

module Default = Make(Index.BasicEquation)

let () =
  Options.add_opts
    [ "--dtree-max-depth", Arg.Set_int max_depth_,
      " set maximal depth of terms for Dtree (demodulation)"
    ]
