
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Equational literals} *)

module T = Term
module S = Subst
module PB = Position.Build
module P = Position
module AL = Int_lit
module US = Unif_subst

type term = Term.t

type t =
  | True
  | False
  | Equation of term * term * bool
  | Prop of term * bool
  | Int of Int_lit.t
  | Rat of Rat_lit.t

type lit = t

let equal l1 l2 =
  match l1, l2 with
    | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
      sign1 = sign2 && l1 == l2 && r1 == r2
    | Prop (p1, sign1), Prop(p2, sign2) -> sign1 = sign2 && T.equal p1 p2
    | True, True
    | False, False -> true
    | Int o1, Int o2 -> Int_lit.equal o1 o2
    | Rat o1, Rat o2 -> Rat_lit.equal o1 o2
    | Equation _, _
    | Prop _, _
    | True, _
    | False, _
    | Int _, _
    | Rat _, _
      -> false

let equal_com l1 l2 =
  match l1, l2 with
    | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
      sign1 = sign2 &&
      ((T.equal l1 l2 && T.equal r1 r2) ||
       (T.equal l1 r2 && T.equal r1 l2))
    | Prop (p1, sign1), Prop(p2, sign2) -> sign1 = sign2 && T.equal p1 p2
    | True, True
    | False, False -> true
    | Int o1, Int o2 -> Int_lit.equal_com o1 o2
    | _ -> equal l1 l2  (* regular comparison *)

let compare l1 l2 =
  let __to_int = function
    | False -> 0
    | True -> 1
    | Equation _ -> 2
    | Prop _ -> 3
    | Int _ -> 5
    | Rat _ -> 6
  in
  match l1, l2 with
    | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
      let c = T.compare l1 l2 in
      if c <> 0 then c else
        let c = T.compare r1 r2 in
        if c <> 0 then c else
          Pervasives.compare sign1 sign2
    | Prop (p1, sign1), Prop(p2, sign2) ->
      let c = T.compare p1 p2 in
      if c <> 0 then c else Pervasives.compare sign1 sign2
    | True, True
    | False, False -> 0
    | Int o1, Int o2 -> Int_lit.compare o1 o2
    | Rat o1, Rat o2 -> Rat_lit.compare o1 o2
    | _, _ -> __to_int l1 - __to_int l2

let fold f acc lit = match lit with
  | Equation (l, r, _) -> f (f acc l) r
  | Prop (p, _) -> f acc p
  | Int o -> Int_lit.fold f acc o
  | Rat o -> Rat_lit.fold f acc o
  | True
  | False -> acc

let for_all f lit = fold (fun b t -> b && f t) true lit

let hash lit =
  match lit with
    | Int o -> Int_lit.hash o
    | Rat o -> Rat_lit.hash o
    | Prop (p, sign) -> Hash.combine3 20 (Hash.bool sign) (T.hash p)
    | Equation (l, r, sign) ->
      Hash.combine4 30 (Hash.bool sign) (T.hash l) (T.hash r)
    | True -> 40
    | False -> 50

let weight lit =
  fold (fun acc t -> acc + T.size t) 0 lit

let heuristic_weight weight = function
  | Prop (p, _) -> weight p
  | Equation (l, r, _) -> weight l + weight r
  | True
  | False -> 0
  | Int alit ->
    (* sum of weights of terms, without the (naked) variables *)
    AL.Seq.terms alit
    |> Sequence.filter (fun t -> not (T.is_var t))
    |> Sequence.fold (fun acc t -> acc + weight t) 0
  | Rat alit ->
    Rat_lit.Seq.terms alit
    |> Sequence.filter (fun t -> not (T.is_var t))
    |> Sequence.fold (fun acc t -> acc + weight t) 0

let depth lit =
  fold (fun acc t -> max acc (T.depth t)) 0 lit

let sign = function
  | Prop (_, sign)
  | Equation (_, _, sign) -> sign
  | False -> false
  | Int o -> Int_lit.sign o
  | Rat _ -> true
  | True -> true

module Set = CCSet.Make(struct type t = lit let compare = compare end)

(* specific: for the term comparison *)
let polarity = function
  | Int o -> Int_lit.polarity o
  | lit -> sign lit

let is_pos = sign

let is_neg lit = not (is_pos lit)

let is_eqn = function
  | Equation _
  | Prop _ -> true
  | Int _
  | Rat _
  | True
  | False -> false

let is_eq lit = is_eqn lit && is_pos lit
let is_neq lit = is_eqn lit && is_neg lit

let is_prop = function
  | Prop _
  | True
  | False -> true
  | Int _
  | Rat _
  | Equation _ -> false

let is_arith = function
  | Int _ -> true
  | _ -> false

let _on_arith p lit = match lit with
  | Int o -> p o
  | _ -> false

let is_arith_eqn = _on_arith Int_lit.is_eqn
let is_arith_eq = _on_arith Int_lit.is_eq
let is_arith_neq = _on_arith Int_lit.is_neq
let is_arith_ineq = _on_arith Int_lit.is_ineq
let is_arith_less = _on_arith Int_lit.is_less
let is_arith_lesseq = _on_arith Int_lit.is_lesseq
let is_arith_divides = _on_arith Int_lit.is_divides

let _on_rat p lit = match lit with
  | Rat o -> p o
  | _ -> false

let is_rat = function Rat _ -> true | _ -> false
let is_rat_eq = _on_rat Rat_lit.is_eq
let is_rat_less = _on_rat Rat_lit.is_less


let ty_error_ a b =
  let msg =
    CCFormat.sprintf
      "@[<2>Literal: incompatible types in equational lit@ for `@[%a : %a@]`@ and `@[%a : %a@]`@]"
      T.pp a Type.pp (T.ty a) T.pp b Type.pp (T.ty b)
  in
  raise (Type.ApplyError msg)

let has_num_ty t =
  Type.equal Type.int (T.ty t) ||
  Type.equal Type.rat (T.ty t)

(* primary constructor for equations and predicates *)
let rec mk_lit a b sign =
  if not (Type.equal (T.ty a) (T.ty b)) then ty_error_ a b;
  match T.view a, T.view b with
    | T.AppBuiltin (Builtin.True, []), T.AppBuiltin (Builtin.False, []) -> if sign then False else True
    | T.AppBuiltin (Builtin.False, []), T.AppBuiltin (Builtin.True, []) -> if sign then False else True
    | T.AppBuiltin (Builtin.True, []), _ -> Prop (b, sign)
    | _, T.AppBuiltin (Builtin.True, []) -> Prop (a, sign)
    | T.AppBuiltin (Builtin.False, []), _ -> Prop (b, not sign)
    | _, T.AppBuiltin (Builtin.False, []) -> Prop (a, not sign)
    (* NOTE: keep negation for higher-order unification constraints
       | T.AppBuiltin (Builtin.Not, [a']), _ -> mk_lit a' b (not sign)
       | _, T.AppBuiltin (Builtin.Not, [b']) -> mk_lit a b' (not sign)
    *)
    | _ when has_num_ty a ->
      begin match mk_num_eq a b sign with
        | None -> Equation (a,b,sign)
        | Some lit -> lit
      end
    | _ -> Equation (a, b, sign)

and mk_num_eq t u sign =
  let open CCOpt in
  if Type.equal Type.int (T.ty t) then (
    let module AL = Int_lit in
    Monome.Int.of_term t >>= fun m1 ->
    Monome.Int.of_term u >>= fun m2 ->
    return (Int (if sign then AL.mk_eq m1 m2 else AL.mk_neq m1 m2))
  ) else if Type.equal Type.rat (T.ty t) then (
    let module AL = Rat_lit in
    if sign then (
      Monome.Rat.of_term t >>= fun m1 ->
      Monome.Rat.of_term u >>= fun m2 ->
      return (Rat (AL.mk_eq m1 m2))
    ) else None (* no "neq" literal for rationals *)
  ) else assert false

and mk_prop p sign = match T.view p with
  | T.AppBuiltin (Builtin.True, []) -> if sign then True else False
  | T.AppBuiltin (Builtin.False, []) -> if sign then False else True
  | T.AppBuiltin (Builtin.Not, [p']) -> mk_prop p' (not sign)
  | T.AppBuiltin (Builtin.Eq, [a;b]) -> mk_lit a b sign
  | T.AppBuiltin (Builtin.Neq, [a;b]) -> mk_lit a b (not sign)
  | T.AppBuiltin
      ((Builtin.Less | Builtin.Lesseq | Builtin.Greater | Builtin.Greatereq) as b,
       [_; t; u]) when has_num_ty t ->
    (* arith conversion *)
    begin match mk_num_prop b t u sign with
      | None -> Prop (p, sign)
      | Some lit -> lit
    end
  | _ ->
    if not (Type.equal (T.ty p) Type.prop) then ty_error_ p T.true_;
    Prop (p, sign)

(* [sign (builtin t u)] *)
and mk_num_prop builtin t u sign: t option =
  let open CCOpt in
  if Type.equal Type.int (T.ty t) then (
    let module AL = Int_lit in
    Monome.Int.of_term t >>= fun m1 ->
    Monome.Int.of_term u >|= fun m2 ->
    let mk_pred t u = match builtin, sign with
      | Builtin.Less, true
      | Builtin.Greatereq, false -> AL.mk_less t u
      | Builtin.Lesseq, true
      | Builtin.Greater, false -> AL.mk_lesseq t u
      | Builtin.Greater, true
      | Builtin.Lesseq, false -> AL.mk_less u t
      | Builtin.Greatereq, true
      | Builtin.Less, false -> AL.mk_lesseq u t
      | _ -> assert false
    in
    Int (mk_pred m1 m2)
  ) else if Type.equal Type.rat (T.ty t) then (
    let module AL = Rat_lit in
    Monome.Rat.of_term t >>= fun t ->
    Monome.Rat.of_term u >>= fun u ->
    begin match builtin, sign with
      | Builtin.Less, true
      | Builtin.Greatereq, false -> Some (Rat (AL.mk_less t u))
      | Builtin.Greater, true
      | Builtin.Lesseq, false -> Some (Rat (AL.mk_less u t))
      | Builtin.Lesseq, true
      | Builtin.Greater, false
      | Builtin.Greatereq, true
      | Builtin.Less, false -> None (* cannot encode this, would yield 2 lits *)
      | _ -> assert false
    end
  ) else assert false

let mk_eq a b = mk_lit a b true

let mk_neq a b = mk_lit a b false

let mk_true p = mk_prop p true

let mk_false p = mk_prop p false

let mk_tauto = True

let mk_absurd = False

let mk_arith x = Int x

let mk_arith_op op m1 m2 =
  let alit = Int_lit.make op m1 m2 in
  if Int_lit.is_trivial alit then mk_tauto
  else if Int_lit.is_absurd alit then mk_absurd
  else Int alit
let mk_arith_eq m1 m2 = mk_arith_op Int_lit.Equal m1 m2
let mk_arith_neq m1 m2 = mk_arith_op Int_lit.Different m1 m2
let mk_arith_less m1 m2 = mk_arith_op Int_lit.Less m1 m2
let mk_arith_lesseq m1 m2 = mk_arith_op Int_lit.Lesseq m1 m2

let mk_divides ?(sign=true) n ~power m =
  let alit = Int_lit.mk_divides ~sign n ~power m in
  (* simplify things like  not (5 | 10) ---> false *)
  if Int_lit.is_trivial alit then mk_tauto
  else if Int_lit.is_absurd alit then mk_absurd
  else Int alit

let mk_rat x = Rat x

let mk_rat_op op m1 m2 = Rat (Rat_lit.make op m1 m2)
let mk_rat_eq m1 m2 = mk_rat_op Rat_lit.Equal m1 m2
let mk_rat_less m1 m2 = mk_rat_op Rat_lit.Less m1 m2

let mk_not_divides n ~power m = mk_divides ~sign:false n ~power m

let mk_constraint l r = mk_neq l r

module Seq = struct
  let terms lit k = match lit with
    | Equation(l, r, _) -> k l; k r
    | Prop(p, _) -> k p
    | Int o -> Int_lit.Seq.terms o k
    | Rat o -> Rat_lit.Seq.terms o k
    | True
    | False -> ()

  let vars lit = Sequence.flat_map T.Seq.vars (terms lit)

  let symbols lit =
    Sequence.flat_map T.Seq.symbols (terms lit)

  (* used to represent arithmetic lits... *)
  let _arith_term =
    let s = ID.make "$arith_term" in
    T.const ~ty:Type.(const s) s
end

let symbols lit = Seq.symbols lit |> ID.Set.of_seq

(** Unification-like operation on components of a literal. *)
module UnifOp = struct
  type 'subst op = {
    term : subst:'subst -> term Scoped.t -> term Scoped.t ->
      'subst Sequence.t;
    monomes : 'a. subst:'subst -> 'a Monome.t Scoped.t -> 'a Monome.t
        Scoped.t -> 'subst Sequence.t;
  }
end

(* match {x1,y1} in scope 1, with {x2,y2} with scope2 *)
let unif4 op ~subst x1 y1 sc1 x2 y2 sc2 k =
  op ~subst (Scoped.make x1 sc1) (Scoped.make x2 sc2)
    (fun subst -> op ~subst (Scoped.make y1 sc1) (Scoped.make y2 sc2) k);
  op ~subst (Scoped.make y1 sc1) (Scoped.make x2 sc2)
    (fun subst -> op ~subst (Scoped.make x1 sc1) (Scoped.make y2 sc2) k);
  ()

(* generic unification structure *)
let unif_lits op ~subst (lit1,sc1) (lit2,sc2) k =
  let open UnifOp in
  match lit1, lit2 with
    | Prop (p1, sign1), Prop (p2, sign2) when sign1 = sign2 ->
      op.term ~subst (p1,sc1) (p2,sc2) (fun s -> k (s,[]))
    | True, True
    | False, False -> k (subst,[])
    | Equation (l1, r1, sign1), Equation (l2, r2, sign2) when sign1 = sign2 ->
      unif4 op.term ~subst l1 r1 sc1 l2 r2 sc2 (fun s -> k (s,[]))
    | Int o1, Int o2 ->
      Int_lit.generic_unif op.monomes ~subst (o1,sc1) (o2,sc2)
        (fun s -> k(s,[Builtin.Tag.T_lia]))
    | Rat o1, Rat o2 ->
      Rat_lit.generic_unif op.monomes ~subst (o1,sc1) (o2,sc2)
        (fun s -> k(s,[Builtin.Tag.T_lra]))
    | _, _ -> ()

let variant ?(subst=S.empty) lit1 lit2 k =
  let op = UnifOp.({
      term=(fun ~subst t1 t2 k ->
        try k (Unif.FO.variant ~subst t1 t2)
        with Unif.Fail -> ());
      monomes=(fun ~subst m1 m2 k -> Monome.variant ~subst m1 m2 k)
    })
  in
  unif_lits op ~subst lit1 lit2
    (fun (subst,tags) -> if Subst.is_renaming subst then k (subst,tags))

let are_variant lit1 lit2 =
  not (Sequence.is_empty (variant (Scoped.make lit1 0) (Scoped.make lit2 1)))

let matching ?(subst=Subst.empty) ~pattern:lit1 lit2 k =
  let op = UnifOp.({
      term=(fun ~subst t1 t2 k ->
        try k (Unif.FO.matching_adapt_scope ~subst ~pattern:t1 t2)
        with Unif.Fail -> ());
      monomes=(fun ~subst m1 m2 k -> Monome.matching ~subst m1 m2 k)
    })
  in
  unif_lits op ~subst lit1 lit2 k

(* find substitutions such that subst(l1=r1) implies l2=r2 *)
let _eq_subsumes ~subst l1 r1 sc1 l2 r2 sc2 k =
  (* make l2 and r2 equal using l1 = r2 (possibly several times) *)
  let rec equate_terms ~subst l2 r2 k =
    (* try to make the terms themselves equal *)
    equate_root ~subst l2 r2 k;
    (* decompose *)
    match T.view l2, T.view r2 with
      | _ when T.equal l2 r2 -> k subst
      | T.App (f, ss), T.App (g, ts) when List.length ss = List.length ts ->
        equate_terms ~subst f g
          (fun subst -> equate_lists ~subst ss ts k)
      | _ -> ()
  and equate_lists ~subst l2s r2s k = match l2s, r2s with
    | [], [] -> k subst
    | [], _
    | _, [] -> ()
    | l2::l2s', r2::r2s' ->
      equate_terms ~subst l2 r2 (fun subst -> equate_lists ~subst l2s' r2s' k)
  (* make l2=r2 by a direct application of l1=r1, if possible. This can
      enrich [subst] *)
  and equate_root ~subst l2 r2 k =
    begin try
        let subst = Unif.FO.matching_adapt_scope
            ~subst ~pattern:(Scoped.make l1 sc1) (Scoped.make l2 sc2) in
        let subst = Unif.FO.matching_adapt_scope
            ~subst ~pattern:(Scoped.make r1 sc1) (Scoped.make r2 sc2) in
        k subst
      with Unif.Fail -> ()
    end;
    begin try
        let subst = Unif.FO.matching_adapt_scope
            ~subst ~pattern:(Scoped.make l1 sc1) (Scoped.make r2 sc2) in
        let subst = Unif.FO.matching_adapt_scope
            ~subst ~pattern:(Scoped.make r1 sc1) (Scoped.make l2 sc2) in
        k subst
      with Unif.Fail -> ()
    end;
    ()
  in
  equate_terms ~subst l2 r2 k

let subsumes ?(subst=Subst.empty) (lit1,sc1) (lit2,sc2) k =
  match lit1, lit2 with
    | Int o1, Int o2 ->
      (* use the more specific subsumption mechanism *)
      Int_lit.subsumes ~subst (o1,sc1) (o2,sc2)
        (fun s -> k(s,[Builtin.Tag.T_lia]))
    | Equation (l1, r1, true), Equation (l2, r2, true) ->
      _eq_subsumes ~subst l1 r1 sc1 l2 r2 sc2 (fun s -> k(s,[]))
    | _ -> matching ~subst ~pattern:(lit1,sc1) (lit2,sc2) k

let unify ?(subst=US.empty) lit1 lit2 k =
  let op = UnifOp.({
      term=(fun ~subst t1 t2 k ->
        try k (Unif.FO.unify_full ~subst t1 t2)
        with Unif.Fail -> ());
      monomes=(fun ~subst m1 m2 k -> Monome.unify ~subst m1 m2 k)
    })
  in
  unif_lits op ~subst lit1 lit2 k

let map_ ~simp f = function
  | Equation (left, right, sign) ->
    let new_left = f left
    and new_right = f right in
    if simp
    then mk_lit new_left new_right sign
    else Equation (new_left, new_right, sign)
  | Prop (p, sign) ->
    let p' = f p in
    if simp
    then mk_prop p' sign
    else Prop (p', sign)
  | Int o -> Int (Int_lit.map f o)
  | Rat o -> Rat (Rat_lit.map f o)
  | True -> True
  | False -> False

let map f lit = map_ ~simp:true f lit
let map_no_simp f lit = map_ ~simp:false f lit

let apply_subst_ ~f_term ~f_arith_lit ~f_rat subst (lit,sc) =
  match lit with
    | Equation (l,r,sign) ->
      let new_l = f_term subst (l,sc)
      and new_r = f_term subst (r,sc) in
      mk_lit new_l new_r sign
    | Prop (p, sign) ->
      let p' = f_term subst (p,sc) in
      mk_prop p' sign
    | Int o -> Int (f_arith_lit subst (o,sc))
    | Rat o -> Rat (f_rat subst (o,sc))
    | True
    | False -> lit

let apply_subst renaming subst (lit,sc) =
  apply_subst_ subst (lit,sc)
    ~f_term:(S.FO.apply renaming)
    ~f_arith_lit:(Int_lit.apply_subst renaming)
    ~f_rat:(Rat_lit.apply_subst renaming)

let apply_subst_no_simp renaming subst (lit,sc) =
  match lit with
    | Int o -> Int (Int_lit.apply_subst_no_simp renaming subst (o,sc))
    | Rat o -> Rat (Rat_lit.apply_subst_no_simp renaming subst (o,sc))
    | Equation (l,r,sign) ->
      Equation (S.FO.apply renaming subst (l,sc),
        S.FO.apply renaming subst (r,sc), sign)
    | Prop (p, sign) ->
      Prop (S.FO.apply renaming subst (p,sc), sign)
    | True
    | False -> lit

let apply_subst_list renaming subst (lits,sc) =
  List.map
    (fun lit -> apply_subst renaming subst (lit,sc))
    lits

exception Lit_is_constraint

let is_ho_constraint = function
  | Equation (l, r, false) -> T.is_ho_at_root l || T.is_ho_at_root r
  | _ -> false

let is_constraint = function
  | Equation (t, u, false) -> T.is_var t || T.is_var u
  | _ -> false

let negate lit = match lit with
  | Equation (l,r,sign) -> Equation (l,r,not sign)
  | Prop (p, sign) -> Prop (p, not sign)
  | True -> False
  | False -> True
  | Int o -> Int (Int_lit.negate o)
  | Rat o -> mk_false (Rat_lit.to_term o)

let vars lit =
  Seq.vars lit |> T.VarSet.of_seq |> T.VarSet.to_list

let var_occurs v lit = match lit with
  | Prop (p,_) -> T.var_occurs ~var:v p
  | Equation (l,r,_) -> T.var_occurs ~var:v l || T.var_occurs ~var:v r
  | Int _
  | Rat _ -> Sequence.exists (T.var_occurs ~var:v) (Seq.terms lit)
  | True
  | False -> false

let is_ground lit = match lit with
  | Equation (l,r,_) -> T.is_ground l && T.is_ground r
  | Prop (p, _) -> T.is_ground p
  | Int _
  | Rat _ -> Sequence.for_all T.is_ground (Seq.terms lit)
  | True
  | False -> true

let root_terms l =
  Seq.terms l |> Sequence.to_rev_list

let to_multiset lit = match lit with
  | Prop (p,_) -> Multisets.MT.singleton p
  | Equation (l, r, _) -> Multisets.MT.doubleton l r
  | True
  | False -> Multisets.MT.singleton T.true_
  | Int alit ->
    AL.Seq.to_multiset alit
    |> Multisets.MT.Seq.of_coeffs Multisets.MT.empty
  | Rat o ->
    Rat_lit.Seq.to_multiset o |> Sequence.map fst
    |> Multisets.MT.Seq.of_seq Multisets.MT.empty

let is_trivial lit = match lit with
  | True -> true
  | False -> false
  | Equation (l, r, true) -> T.equal l r
  | Equation (_, _, false) -> false
  | Int o -> Int_lit.is_trivial o
  | Rat o -> Rat_lit.is_trivial o
  | Prop (_, _) -> false

(* is it impossible for these terms to be equal? check if a cstor-only
     path leads to distinct constructors/constants *)
let rec cannot_be_eq (t1:term)(t2:term): Builtin.Tag.t list option =
  let module TC = T.Classic in
  begin match TC.view t1, TC.view t2 with
    | TC.AppBuiltin (Builtin.Int z1,[]), TC.AppBuiltin (Builtin.Int z2,[]) ->
      if Z.equal z1 z2 then None else Some [Builtin.Tag.T_lia]
    | TC.AppBuiltin (Builtin.Rat n1,[]), TC.AppBuiltin (Builtin.Rat n2,[]) ->
      if Q.equal n1 n2 then None else Some [Builtin.Tag.T_lra]
    | TC.App (c1, l1), TC.App (c2, l2)
      when Ind_ty.is_constructor c1 && Ind_ty.is_constructor c2 ->
      (* two constructor applications cannot be equal if they
         don't have the same constructor *)
      if ID.equal c1 c2 && List.length l1=List.length l2 then (
        List.combine l1 l2
        |> Sequence.of_list
        |> Sequence.find_map (fun (a,b) -> cannot_be_eq a b)
      ) else Some [Builtin.Tag.T_data]
    | _ -> None
  end

let is_absurd lit = match lit with
  | Equation (l, r, false) when T.equal l r -> true
  | Equation (l, r, true) -> CCOpt.is_some (cannot_be_eq l r)
  | Prop (p, false) when T.equal p T.true_ -> true
  | Prop (p, true) when T.equal p T.false_ -> true
  | False -> true
  | Int o -> Int_lit.is_absurd o
  | Rat o -> Rat_lit.is_absurd o
  | Equation _ | Prop _ | True -> false

let is_absurd_tags lit = match lit with
  | Equation (l,r,true) -> cannot_be_eq l r |> CCOpt.get_or ~default:[]
  | Equation _ | Prop _ | False -> []
  | True -> assert false
  | Int _ -> [Builtin.Tag.T_lia]
  | Rat _ -> [Builtin.Tag.T_lra]

let fold_terms ?(position=Position.stop) ?(vars=false) ?ty_args ~which ?(ord=Ordering.none) ~subterms lit k =
  (* function to call at terms *)
  let at_term ~pos t =
    if subterms
    then T.all_positions ?ty_args ~vars ~pos t k
    else if T.is_var t && not vars
    then () (* ignore *)
    else k (t, pos)
  in
  begin match lit with
    | Equation (l, r, _) ->
      begin match which with
        | `All ->
          at_term ~pos:P.(append position (left stop)) l;
          at_term ~pos:P.(append position (right stop)) r
        | `Max ->
          begin match Ordering.compare ord l r with
            | Comparison.Gt ->
              at_term ~pos:P.(append position (left stop)) l
            | Comparison.Lt ->
              at_term ~pos:P.(append position (right stop)) r
            | Comparison.Eq | Comparison.Incomparable ->
              (* visit both sides, they are both (potentially) maximal *)
              at_term ~pos:P.(append position (left stop)) l;
              at_term ~pos:P.(append position (right stop)) r
          end
      end
    | Prop (p, _) ->
      (* p is the only term, and it's maximal *)
      at_term ~pos:P.(append position (left stop)) p
    | Int o ->
      Int_lit.fold_terms ~pos:position ?ty_args ~vars ~which ~ord ~subterms o k
    | Rat o  ->
      Rat_lit.fold_terms ~pos:position ?ty_args ~vars ~which ~ord ~subterms o k
    | True
    | False -> ()
  end

(* try to convert a literal into a term *)
let to_ho_term (lit:t): T.t option = match lit with
  | Prop (t, true) -> Some t
  | Prop (t, false) -> Some (T.Form.not_ t)
  | True -> Some T.true_
  | False -> Some T.false_
  | Equation (t, u, sign) ->
    Some (if sign then T.Form.eq t u else T.Form.neq t u)
  | Int _
  | Rat _ -> None

let as_ho_predicate (lit:t) : _ option = match lit with
  | Prop (t, sign) ->
    let hd_t, args_t = T.as_app t in
    begin match T.view hd_t, args_t with
      | T.Var v, _::_ -> Some (v, hd_t, args_t, sign)
      | _ -> None
    end
  | _ -> None

let is_ho_predicate lit = CCOpt.is_some (as_ho_predicate lit)

let is_ho_unif lit = match lit with
  | Equation (t, u, false) -> Term.is_ho_app t || Term.is_ho_app u
  | _ -> false

let of_unif_subst renaming (s:Unif_subst.t) : t list =
  Unif_subst.constr_l_subst renaming s
  |> List.map
    (fun (t,u) ->
       (* upcast *)
       let t = T.of_term_unsafe t in
       let u = T.of_term_unsafe u in
       mk_constraint t u)

(** {2 IO} *)

let pp_debug ?(hooks=[]) out lit =
  if List.for_all (fun h -> not (h out lit)) hooks
  then match lit with
    | Prop (p, true) -> Format.fprintf out "@[%a@]" T.pp p
    | Prop (p, false) -> Format.fprintf out "¬@[%a@]" T.pp p
    | True -> CCFormat.string out "Τ"
    | False -> CCFormat.string out "⊥"
    | Equation (l, r, true) ->
      Format.fprintf out "@[<1>%a@ = %a@]" T.pp l T.pp r
    | Equation (l, r, false) ->
      Format.fprintf out "@[<1>%a@ ≠ %a@]" T.pp l T.pp r
    | Int o -> CCFormat.within "(" ")" Int_lit.pp out o
    | Rat o -> CCFormat.within "(" ")" Rat_lit.pp out o

let pp_tstp out lit =
  match lit with
    | Prop (p, true) -> T.TPTP.pp out p
    | Prop (p, false) -> Format.fprintf out "~ %a" T.TPTP.pp p
    | True -> CCFormat.string out "$true"
    | False -> CCFormat.string out "$false"
    | Equation (l, r, true) ->
      Format.fprintf out "@[<1>%a@ = %a@]" T.TPTP.pp l T.TPTP.pp r
    | Equation (l, r, false) ->
      Format.fprintf out "@[<1>%a@ != %a@]" T.TPTP.pp l T.TPTP.pp r
    | Int o -> Int_lit.pp_tstp out o
    | Rat o -> Rat_lit.pp_tstp out o

let pp_zf out lit =
  match lit with
    | Prop (p, true) -> T.ZF.pp out p
    | Prop (p, false) -> Format.fprintf out "~ %a" T.ZF.pp p
    | True -> CCFormat.string out "true"
    | False -> CCFormat.string out "false"
    | Equation (l, r, true) ->
      Format.fprintf out "@[<1>%a@ = %a@]" T.ZF.pp l T.ZF.pp r
    | Equation (l, r, false) ->
      Format.fprintf out "@[<1>%a@ != %a@]" T.ZF.pp l T.ZF.pp r
    | Int o -> Int_lit.pp_zf out o
    | Rat o -> Rat_lit.pp_zf out o

type print_hook = CCFormat.t -> t -> bool
let __hooks = ref []
let add_default_hook h = __hooks := h :: !__hooks

let pp buf lit = pp_debug ~hooks:!__hooks buf lit

let to_string t = CCFormat.to_string pp t

(* comparison should live in its scope *)
module Comp = struct
  module O = Ordering
  module C = Comparison

  let _maxterms2 ~ord l r =
    match O.compare ord l r with
      | C.Gt -> [l]
      | C.Lt -> [r]
      | C.Eq -> [l]
      | C.Incomparable -> [l; r]

  (* maximal terms of the literal *)
  let max_terms ~ord lit =
    match lit with
      | Prop (p, _) -> [p]
      | Equation (l, r, _) -> _maxterms2 ~ord l r
      | Int a -> Int_lit.max_terms ~ord a
      | Rat a -> Rat_lit.max_terms ~ord a
      | True
      | False -> []

  (* general comparison is a bit complicated.
     - First we compare literals l1 and l2
        by their (set of potential) maximal terms.
     - then by their polarity (neg > pos)
     - then by their kind (regular equation/prop on bottom)
     - then, l1 and l2 must be of the same kind, so we use a
        kind-specific comparison.
  *)

  (* is there an element of [l1] that dominates all elements of [l2]? *)
  let _some_term_dominates f l1 l2 =
    List.exists
      (fun x -> List.for_all (fun y -> f x y = Comparison.Gt) l2)
      l1

  let _cmp_by_maxterms ~ord l1 l2 =
    match l1, l2 with
      | Prop (p1, _), Prop (p2, _) -> Ordering.compare ord p1 p2
      | _ ->
        let t1 = max_terms ~ord l1 and t2 = max_terms ~ord l2 in
        let f = Ordering.compare ord in
        match _some_term_dominates f t1 t2, _some_term_dominates f t2 t1 with
          | false, false ->
            let t1' = CCList.fold_right T.Set.add t1 T.Set.empty
            and t2' = CCList.fold_right T.Set.add t2 T.Set.empty in
            if T.Set.equal t1' t2'
            then C.Eq (* next criterion *)
            else C.Incomparable
          | true, true -> assert false
          | true, false -> C.Gt
          | false, true -> C.Lt

  (* negative literals dominate *)
  let _cmp_by_polarity l1 l2 =
    let p1 = polarity l1 in
    let p2 = polarity l2 in
    match p1, p2 with
      | true, true
      | false, false -> Comparison.Eq
      | true, false -> Comparison.Lt
      | false, true -> Comparison.Gt

  let _cmp_by_kind l1 l2 =
    let open Int_lit in
    let _to_int = function
      | False
      | True -> 0
      | Int (Binary (Equal, _, _)) -> 13
      | Int (Binary (Different, _, _)) -> 14
      | Int (Binary (Less, _, _)) -> 15
      | Int (Binary (Lesseq, _, _)) -> 16
      | Int (Divides _) -> 17
      | Rat {Rat_lit.op=Rat_lit.Equal; _} -> 20
      | Rat {Rat_lit.op=Rat_lit.Less; _} -> 21
      | Equation _
      | Prop _ -> 30  (* eqn and prop are really the same thing *)
    in
    C.of_total (Pervasives.compare (_to_int l1) (_to_int l2))

  (* by multiset of terms *)
  let _cmp_by_term_multiset ~ord l1 l2 =
    let m1 = to_multiset l1 and m2 = to_multiset l2 in
    Multisets.MT.compare_partial (Ordering.compare ord) m1 m2

  let _cmp_specific ~ord l1 l2 =
    match l1, l2 with
      | True, True
      | True, False
      | True, Prop _
      | True, Equation _
      | False, False
      | False, True
      | False, Prop _
      | False, Equation _
      | Prop _, Prop _
      | Prop _, Equation _
      | Prop _, True
      | Prop _, False
      | Equation _, Equation _
      | Equation _, Prop _
      | Equation _, True
      | Equation _, False ->
        _cmp_by_term_multiset ~ord l1 l2
      | Int (AL.Binary(op1, x1, y1)), Int (AL.Binary(op2, x2, y2)) ->
        assert (op1 = op2);
        let module MI = Monome.Int in
        let left = Multisets.MMT.doubleton (MI.to_multiset x1) (MI.to_multiset y1) in
        let right = Multisets.MMT.doubleton (MI.to_multiset x2) (MI.to_multiset y2) in
        Multisets.MMT.compare_partial
          (Multisets.MT.compare_partial (Ordering.compare ord))
          left right
      | Int(AL.Divides d1), Int(AL.Divides d2) ->
        assert (d1.AL.sign=d2.AL.sign);
        let c = Z.compare d1.AL.num d2.AL.num in
        if c <> 0 then C.of_total c  (* live in totally distinct Z/nZ *)
        else
        if is_ground l1 && is_ground l2
        then
          C.Incomparable
          (* TODO: Bezout-normalize, then actually compare Monomes. *)
        else C.Incomparable
      | Rat {Rat_lit.op=o1;left=l1;right=r1}, Rat {Rat_lit.op=o2;left=l2;right=r2} ->
        assert (o1=o2);
        let module M = Monome.Rat in
        let m1 = Multisets.MT.union (M.to_multiset l1) (M.to_multiset r1) in
        let m2 = Multisets.MT.union (M.to_multiset l2) (M.to_multiset r2) in
        Multisets.MT.compare_partial (Ordering.compare ord) m1 m2
      | _, _ ->
        Util.debugf 5 "(@[bad_compare %a %a@])" (fun k->k pp l1 pp l2);
        assert false

  let compare ~ord l1 l2 =
    let f = Comparison.(
        _cmp_by_maxterms ~ord @>>
        _cmp_by_polarity @>>
        _cmp_by_kind @>>
        _cmp_specific ~ord
      ) in
    let res = f l1 l2 in
    res
end

module Pos = struct
  type split = {
    lit_pos : Position.t;
    term_pos : Position.t;
    term : term;
  }

  let _fail_lit lit pos =
    let msg =
      CCFormat.sprintf "@[<2>invalid position @[%a@]@ in lit @[%a@]@]"
        Position.pp pos pp lit
    in invalid_arg msg

  let split lit pos =
    let module AL = Int_lit in
    match lit, pos with
      | True, P.Stop ->
        {lit_pos=P.stop; term_pos=P.stop; term=T.true_; }
      | False, P.Stop ->
        {lit_pos=P.stop; term_pos=P.stop; term=T.false_; }
      | Equation (l,_,_), P.Left pos' ->
        {lit_pos=P.(left stop); term_pos=pos'; term=l; }
      | Equation (_,r,_), P.Right pos' ->
        {lit_pos=P.(right stop); term_pos=pos'; term=r; }
      | Prop (p,_), P.Left pos' ->
        {lit_pos=P.(left stop); term_pos=pos'; term=p; }
      | Int(AL.Divides d), P.Arg (i, pos') ->
        let term = try snd(Monome.nth d.AL.monome i) with _ -> _fail_lit lit pos in
        {lit_pos=P.(arg i stop); term_pos= pos'; term; }
      | Int(AL.Binary (_, m1, _)), P.Left (P.Arg (i, pos')) ->
        let term = try snd(Monome.nth m1 i) with _ -> _fail_lit lit pos in
        {lit_pos=P.(left @@ arg i stop); term_pos=pos'; term; }
      | Int(AL.Binary(_, _, m2)), P.Right (P.Arg (i, pos')) ->
        let term = try snd(Monome.nth m2 i) with _ -> _fail_lit lit pos in
        {lit_pos=P.(right @@ arg i stop); term_pos=pos'; term; }
      | _ -> _fail_lit lit pos

  let cut lit pos =
    let s = split lit pos in
    s.lit_pos, s.term_pos

  let at lit pos =
    let s = split lit pos in
    T.Pos.at s.term s.term_pos

  let replace lit ~at ~by =
    let module AL = Int_lit in
    match lit, at with
      | Equation (l, r, sign), P.Left pos' ->
        Equation (T.Pos.replace l pos' ~by, r, sign)
      | Equation (l, r, sign), P.Right pos' ->
        Equation (l, T.Pos.replace r pos' ~by, sign)
      | Prop (p, sign), P.Left pos' ->
        Prop (T.Pos.replace p pos' ~by, sign)
      | True, _
      | False, _ -> lit  (* flexible, lit can be the result of a simplification *)
      | Int (AL.Binary (op, m1, m2)), P.Left (P.Arg(i,pos')) ->
        let _, t = Monome.nth m1 i in
        let m1' = Monome.set_term m1 i (T.Pos.replace t pos' ~by) in
        Int (AL.make_no_simp op m1' m2)
      | Int (AL.Binary(op, m1, m2)), P.Right (P.Arg(i,pos')) ->
        let _, t = Monome.nth m2 i in
        let m2' = Monome.set_term m2 i (T.Pos.replace t pos' ~by) in
        Int (AL.make_no_simp op m1 m2')
      | Int (AL.Divides d), P.Arg (i, pos') ->
        let _, t = Monome.nth d.AL.monome i in
        let m' = Monome.set_term d.AL.monome i (T.Pos.replace t pos' ~by) in
        Int (AL.mk_divides ~sign:d.AL.sign ~power:d.AL.power d.AL.num m')
      | _ -> _fail_lit lit at

  let root_term lit pos =
    at lit (fst (cut lit pos))

  let term_pos lit pos = snd (cut lit pos)

  let is_max_term ~ord lit pos =
    let module AL = Int_lit in
    match lit, pos with
      | Equation (l, r, _), P.Left _ ->
        Ordering.compare ord l r <> Comparison.Lt
      | Equation (l, r, _), P.Right _ ->
        Ordering.compare ord r l <> Comparison.Lt
      | Prop _, _ -> true
      | Int (AL.Binary(_, _m1, _m2)), _ ->
        (* [t] dominates all atomic terms? *)
        let t = root_term lit pos in
        Sequence.for_all
          (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
          (Seq.terms lit)
      | Int (AL.Divides d), _ ->
        let t = root_term lit pos in
        Sequence.for_all
          (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
          (Monome.Seq.terms d.AL.monome)
      | Rat _, _ ->
        let t = root_term lit pos in
        Sequence.for_all
          (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
          (Seq.terms lit)
      | True, _
      | False, _ -> true  (* why not. *)
      | Equation _, _ -> _fail_lit lit pos
end

let replace lit ~old ~by = map (T.replace ~old ~by) lit

module Conv = struct
  type hook_from = term SLiteral.t -> t option
  type hook_to = t -> term SLiteral.t option

  let rec try_hooks x hooks = match hooks with
    | [] -> None
    | h::hooks' ->
      match h x with
        | None -> try_hooks x hooks'
        | (Some _) as res -> res

  let of_form ?(hooks=[]) f =
    match try_hooks f hooks with
      | Some lit -> lit
      | None ->
        begin match f with
          | SLiteral.True -> True
          | SLiteral.False -> False
          | SLiteral.Atom (t,b) -> mk_prop t b
          | SLiteral.Eq (l,r) -> mk_eq l r
          | SLiteral.Neq (l,r) -> mk_neq l r
        end

  let to_form ?(hooks=[]) lit =
    begin match try_hooks lit hooks with
      | Some f -> f
      | None ->
        begin match lit with
          | Equation (l, r, true) -> SLiteral.eq l r
          | Equation (l, r, false) -> SLiteral.neq l r
          | Prop (p, sign) -> SLiteral.atom p sign
          | True -> SLiteral.true_
          | False -> SLiteral.false_
          | Int o -> Int_lit.to_form o
          | Rat o -> Rat_lit.to_form o
        end
    end

  let to_s_form ?allow_free_db ?(ctx=T.Conv.create()) ?hooks lit =
    to_form ?hooks lit
    |> SLiteral.map ~f:(T.Conv.to_simple_term ?allow_free_db ctx)
    |> SLiteral.to_form
end

module View = struct
  let as_eqn lit = match lit with
    | Equation (l,r,sign) -> Some (l, r, sign)
    | Prop (p, sign) -> Some (p, T.true_, sign)
    | True
    | False
    | Rat _
    | Int _ -> None

  let get_eqn lit position =
    match lit, position with
      | Equation (l,r,sign), P.Left _ -> Some (l, r, sign)
      | Equation (l,r,sign), P.Right _ -> Some (r, l, sign)
      | Prop (p, sign), P.Left _ -> Some (p, T.true_, sign)
      | True, _
      | False, _
      | Int _, _
      | Rat _, _ -> None
      | _ -> invalid_arg "get_eqn: wrong literal or position"

  let get_arith = function
    | Int o -> Some o
    | _ -> None

  let focus_arith lit pos = match lit with
    | Int o -> Int_lit.Focus.get o pos
    | _ -> None

  let unfocus_arith x = Int (Int_lit.Focus.unfocus x)

  let get_rat = function
    | Rat o -> Some o
    | _ -> None

  let focus_rat lit pos = match lit with
    | Rat o -> Rat_lit.Focus.get o pos
    | _ -> None

  let unfocus_rat x = Rat (Rat_lit.Focus.unfocus x)
end
