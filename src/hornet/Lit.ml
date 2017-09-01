
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Literal} *)

open Logtk

module Fmt = CCFormat
module T = Term
module TI = InnerTerm
module P = Position
module PW = Position.With
module S = Subst

type ty = Type.t
type term = T.t

type t = Hornet_types.lit =
  | Bool of bool
  | Atom of term * bool
  | Eq of term * term * bool

type lit = t

let true_ = Bool true
let false_ = Bool false
let bool b = Bool b

let ty_error_ a b =
  let msg =
    CCFormat.sprintf
      "@[<2>Literal: incompatible types in equational lit@ \
       for `@[%a : %a@]`@ and `@[%a : %a@]`@]"
      T.pp a Type.pp (T.ty a) T.pp b Type.pp (T.ty b)
  in
  raise (Type.ApplyError msg)

(* primary constructor for equations and predicates *)
let rec mk_lit t u sign =
  if not (Type.equal (T.ty t) (T.ty u)) then ty_error_ t u;
  begin match TI.view (t:T.t:>TI.t), TI.view (u:T.t:>TI.t) with
    | TI.AppBuiltin (Builtin.True, []), TI.AppBuiltin (Builtin.False, []) -> bool sign
    | TI.AppBuiltin (Builtin.False, []), TI.AppBuiltin (Builtin.True, []) -> bool (not sign)
    | TI.AppBuiltin (Builtin.True, []), _ -> Atom (u, sign)
    | _, TI.AppBuiltin (Builtin.True, []) -> Atom (t, sign)
    | TI.AppBuiltin (Builtin.False, []), _ -> Atom (u, not sign)
    | _, TI.AppBuiltin (Builtin.False, []) -> Atom (t, not sign)
    | TI.AppBuiltin (Builtin.Not, [t']), _ ->
      mk_lit (T.of_term_unsafe t') u (not sign)
    | _, TI.AppBuiltin (Builtin.Not, [u']) ->
      mk_lit t (T.of_term_unsafe u') (not sign)
    | _ -> Eq (t, u, sign)
  end

let eq ?(sign=true) t u = mk_lit t u sign

let rec mk_atom t sign = match T.view t with
  | T.AppBuiltin (Builtin.True, []) -> bool sign
  | T.AppBuiltin (Builtin.False, []) -> bool (not sign)
  | T.AppBuiltin (Builtin.Not, [t']) -> mk_atom t' (not sign)
  | _ ->
    if not (Type.equal (T.ty t) Type.prop) then ty_error_ t T.true_;
    Atom (t, sign)

let atom ?(sign=true) t = mk_atom t sign

let sign = function
  | Atom (_, b)
  | Eq (_,_,b)
  | Bool b -> b

let is_pos = sign
let is_neg l = not (sign l)

let equal = Hornet_types_util.equal_lit
let hash = Hornet_types_util.hash_lit

let compare a b: int =
  let to_int = function Bool _ -> 0 | Atom _ -> 1 | Eq _ -> 2 in
  begin match a, b with
    | Bool b1, Bool b2 -> CCOrd.bool b1 b2
    | Atom (t1,sign1), Atom (t2,sign2) ->
      CCOrd.( T.compare t1 t2 <?> (bool, sign1, sign2))
    | Eq (t1,u1,sign1), Eq (t2,u2,sign2) ->
      CCOrd.( T.compare t1 t2
        <?> (T.compare, u1, u2)
        <?> (bool, sign1, sign2))
    | Bool _, _
    | Atom _, _
    | Eq _, _
      -> CCInt.compare (to_int a)(to_int b)
  end

let pp = Hornet_types_util.pp_lit
let to_string = Fmt.to_string pp

(** {2 Helpers} *)

let neg lit = match lit with
  | Eq (l,r,sign) -> Eq (l,r,not sign)
  | Atom (p, sign) -> Atom (p, not sign)
  | Bool b -> Bool (not b)

let terms (lit:t): term Sequence.t = match lit with
  | Bool _ -> Sequence.empty
  | Atom (t,_) -> Sequence.return t
  | Eq (l,r,_) -> Sequence.doubleton l r

let vars_seq = Hornet_types_util.vars_of_lit

let vars_list l = vars_seq l |> Sequence.to_rev_list

let vars_set l =
  vars_seq l
  |> Sequence.to_rev_list
  |> CCList.sort_uniq ~cmp:(HVar.compare Type.compare)

let is_ground t : bool = vars_seq t |> Sequence.is_empty

let depth t : int =
  terms t |> Sequence.map T.depth |> Sequence.max |> CCOpt.get_or ~default:0

let weight = function
  | Bool _ -> 0
  | Atom (t, _) -> T.weight t
  | Eq (t,u,_) -> T.weight t + T.weight u

let var_occurs ~var t = terms t |> Sequence.exists (T.var_occurs ~var)

let hash_mod_alpha = function
  | Bool b -> Hash.combine2 10 (Hash.bool b)
  | Atom (t,sign) -> Hash.combine3 20 (T.hash_mod_alpha t) (Hash.bool sign)
  | Eq (t,u,sign) ->
    let h_t = T.hash_mod_alpha t in
    let h_u = T.hash_mod_alpha u in
    let h1 = min h_t h_u in
    let h2 = max h_t h_u in
    Hash.combine4 30 h1 h2 (Hash.bool sign)

let is_trivial = function
  | Bool true -> true
  | Bool false
  | Atom _ -> false
  | Eq (a,b,true) -> T.equal a b
  | Eq (_,_,false) ->
    false (* TODO: check if distinct cstors/distinct dom elements *)

let is_absurd lit = match lit with
  | Eq (l, r, false) when T.equal l r -> true
  | Atom (p, false) when T.equal p T.true_ -> true
  | Atom (p, true) when T.equal p T.false_ -> true
  | Bool false -> true
  | _ -> false

let to_slit lit: term SLiteral.t = match lit with
  | Bool true -> SLiteral.true_
  | Bool false -> SLiteral.false_
  | Atom (t, sign) -> SLiteral.atom t sign
  | Eq (t, u, true) -> SLiteral.eq t u
  | Eq (t, u, false) -> SLiteral.neq t u

(** {2 Containers} *)

module As_key = struct
  type t = lit
  let compare = compare
  let equal = equal
  let hash = hash
end

module Set = CCSet.Make(As_key)
module Tbl = CCHashtbl.Make(As_key)

(** {2 Positions} *)

module With_pos = struct
  type t = lit Position.With.t

  let pp = PW.pp pp
  let compare = PW.compare compare
  let to_string = Fmt.to_string pp
end

let direction ord = function
  | Bool _ -> None
  | Atom _ -> None
  | Eq (t,u,_) -> Ordering.compare ord t u |> CCOpt.return

let at_pos_exn pos lit = match lit, pos with
  | Bool b, P.Stop -> if b then T.true_ else T.false_
  | Atom (t,_), P.Left pos' -> T.Pos.at t pos'
  | Eq (t,_,_), P.Left pos' -> T.Pos.at t pos'
  | Eq (_,u,_), P.Right pos' -> T.Pos.at u pos'
  | _, _ -> raise Not_found

let active_terms ?(pos=P.stop) ord lit =
  let yield_term t pos = PW.make t pos in
  begin match lit with
    | Atom (t,true) ->
      Sequence.return (yield_term t (P.append pos (P.left P.stop)))
    | Eq (t,u,true) ->
      begin match Ordering.compare ord t u with
        | Comparison.Eq -> Sequence.empty (* trivial *)
        | Comparison.Incomparable ->
          Sequence.doubleton
            (yield_term t (P.append pos (P.left P.stop)))
            (yield_term u (P.append pos (P.right P.stop)))
        | Comparison.Gt ->
          Sequence.return (yield_term t (P.append pos (P.left P.stop)))
        | Comparison.Lt ->
          Sequence.return (yield_term u (P.append pos (P.right P.stop)))
      end
    | Bool _
    | Atom (_,false)
    | Eq (_,_,false) -> Sequence.empty
  end

let passive_terms ?(pos=P.stop) ord lit =
  let explore_term t pos =
    T.all_positions ~pos ~vars:false ~ty_args:false t
    |> Sequence.map PW.of_pair
  in
  begin match lit with
    | Atom (t,_) -> explore_term t (P.append pos (P.left P.stop))
    | Eq (t,u,_) ->
      begin match Ordering.compare ord t u with
        | Comparison.Eq -> Sequence.empty (* trivial *)
        | Comparison.Incomparable ->
          Sequence.append
            (explore_term t (P.append pos (P.left P.stop)))
            (explore_term u (P.append pos (P.right P.stop)))
        | Comparison.Gt -> explore_term t (P.append pos (P.left P.stop))
        | Comparison.Lt -> explore_term u (P.append pos (P.right P.stop))
      end
    | Bool _ -> Sequence.empty
  end

let seq_terms = function
  | Atom (t,_) -> Sequence.return t
  | Eq (a,b,_) -> Sequence.doubleton a b
  | Bool _ -> Sequence.empty

module Pos = struct
  type split = {
    lit_pos : P.t;
    term_pos : P.t;
    term : term;
  }

  let _fail_lit lit pos =
    Util.errorf ~where:"Lit.Pos"
      "@[<2>invalid position `@[%a@]`@ in lit `@[%a@]`@]"
        P.pp pos pp lit

  let split lit pos = match lit, pos with
    | Bool true, P.Stop ->
      {lit_pos=P.stop; term_pos=P.stop; term=T.true_; }
    | Bool false, P.Stop ->
      {lit_pos=P.stop; term_pos=P.stop; term=T.false_; }
    | Eq(l,_,_), P.Left pos' ->
      {lit_pos=P.(left stop); term_pos=pos'; term=l; }
    | Eq(_,r,_), P.Right pos' ->
      {lit_pos=P.(right stop); term_pos=pos'; term=r; }
    | Atom (p,_), P.Left pos' ->
      {lit_pos=P.(left stop); term_pos=pos'; term=p; }
    | _ -> _fail_lit lit pos

  let cut lit pos =
    let s = split lit pos in
    s.lit_pos, s.term_pos

  let at lit pos =
    let s = split lit pos in
    T.Pos.at s.term s.term_pos

  let replace lit ~at ~by = match lit, at with
    | Eq(l, r, sign), P.Left pos' ->
      eq (T.Pos.replace l pos' ~by) r ~sign
    | Eq(l, r, sign), P.Right pos' ->
      eq l (T.Pos.replace r pos' ~by) ~sign
    | Atom (p, sign), P.Left pos' ->
      atom (T.Pos.replace p pos' ~by) ~sign
    | Bool _, _ -> lit (* flexible, lit can be the result of a simplification *)
    | _ -> _fail_lit lit at
end

let as_eqn lit = match lit with
  | Eq (l,r,sign) -> Some (l, r, sign)
  | Atom (p, sign) -> Some (p, T.true_, sign)
  | Bool _ -> None

let get_eqn lit position = match lit, position with
  | Eq (l,r,sign), P.Left _ -> Some (l, r, sign)
  | Eq (l,r,sign), P.Right _ -> Some (r, l, sign)
  | Atom (p, sign), P.Left _ -> Some (p, T.true_, sign)
  | Bool _, _ -> None
  | _ -> Util.errorf ~where:"Lit.get_eqn"
           "wrong literal `%a` or position `%a`" pp lit P.pp position

let get_eqn_exn lit pos = match get_eqn lit pos with
  | Some x -> x
  | None -> Util.errorf ~where:"Lit.get_eqn_exn"
           "non equational literal `%a`" pp lit P.pp pos

(** {2 Unif} *)

(** Unification-like operation on components of a literal. *)
module Unif_gen = struct
  type op = {
    term : subst:Subst.t -> term Scoped.t -> term Scoped.t ->
      Subst.t Sequence.t;
  }

  let op_matching : op = {
    term=(fun ~subst t1 t2 k ->
      try k (Unif.FO.matching_adapt_scope ~subst ~pattern:t1 t2)
      with Unif.Fail -> ());
  }

  let op_variant : op = {
    term=(fun ~subst t1 t2 k ->
      try k (Unif.FO.variant ~subst t1 t2)
      with Unif.Fail -> ());
  }

  let op_unif : op = {
    term=(fun ~subst t1 t2 k ->
      try k (Unif.FO.unification ~subst t1 t2)
      with Unif.Fail -> ());
  }

  (* match {x1,y1} in scope 1, with {x2,y2} with scope2 *)
  let unif4 f ~subst x1 y1 sc1 x2 y2 sc2 k =
    f ~subst (Scoped.make x1 sc1) (Scoped.make x2 sc2)
      (fun subst -> f ~subst (Scoped.make y1 sc1) (Scoped.make y2 sc2) k);
    f ~subst (Scoped.make y1 sc1) (Scoped.make x2 sc2)
      (fun subst -> f ~subst (Scoped.make x1 sc1) (Scoped.make y2 sc2) k);
    ()

  (* generic unification structure *)
  let unif_lits (op:op) ~subst (lit1,sc1) (lit2,sc2) k =
    begin match lit1, lit2 with
      | Atom (p1, sign1), Atom (p2, sign2) when sign1 = sign2 ->
        op.term ~subst (p1,sc1) (p2,sc2) k
      | Bool b1, Bool b2 -> if b1=b2 then k subst
      | Eq (l1, r1, sign1), Eq (l2, r2, sign2) when sign1 = sign2 ->
        unif4 op.term ~subst l1 r1 sc1 l2 r2 sc2 k
      | _, _ -> ()
    end
end

let variant ?(subst=S.empty) lit1 lit2 k =
  Unif_gen.unif_lits Unif_gen.op_variant ~subst lit1 lit2 k

let are_variant lit1 lit2 =
  not (Sequence.is_empty (variant (Scoped.make lit1 0) (Scoped.make lit2 1)))

let matching ?(subst=Subst.empty) ~pattern:lit1 lit2 k =
  let op = Unif_gen.op_matching in
  Unif_gen.unif_lits op ~subst lit1 lit2 k

(* find substitutions such that subst(l1=r1) implies l2=r2 *)
let eq_subsumes_ ~subst l1 r1 sc1 l2 r2 sc2 k =
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
    | Eq (l1, r1, true), Eq (l2, r2, true) ->
      eq_subsumes_ ~subst l1 r1 sc1 l2 r2 sc2 k
    | _ -> matching ~subst ~pattern:(lit1,sc1) (lit2,sc2) k

let subsumes_pred lit1 lit2 : bool =
  not (subsumes (lit1,0)(lit2,1) |> Sequence.is_empty)

let unify ?(subst=Subst.empty) lit1 lit2 k =
  let op = Unif_gen.op_unif in
  Unif_gen.unif_lits op ~subst lit1 lit2 k

let map f = function
  | Eq (left, right, sign) ->
    let new_left = f left
    and new_right = f right in
    eq ~sign new_left new_right
  | Atom (p, sign) ->
    let p' = f p in
    atom ~sign p'
  | Bool b -> bool b

let apply_subst_ ~f_term subst (lit,sc) = match lit with
  | Eq (l,r,sign) ->
    let new_l = f_term subst (l,sc) in
    let new_r = f_term subst (r,sc) in
    eq ~sign new_l new_r
  | Atom (p, sign) ->
    let p' = f_term subst (p,sc) in
    atom ~sign p'
  | Bool _ -> lit

let apply_subst ~renaming subst (lit,sc) =
  apply_subst_ subst (lit,sc)
    ~f_term:(S.FO.apply ~renaming)

let apply_subst_no_renaming subst (lit,sc) =
  apply_subst_ subst (lit,sc)
    ~f_term:S.FO.apply_no_renaming

let apply_subst_no_simp ~renaming subst (lit,sc) = match lit with
  | Eq (l,r,sign) ->
    Eq (
      S.FO.apply ~renaming subst (l,sc),
      S.FO.apply ~renaming subst (r,sc),
      sign)
  | Atom (p, sign) ->
    Atom (S.FO.apply ~renaming subst (p,sc), sign)
  | Bool _ -> lit

let variant_arr ?(subst=S.empty) (lits1,sc1)(lits2,sc2): S.t Sequence.t =
  Unif.unif_array_com subst
    (IArray.to_array_unsafe lits1,sc1)
    (IArray.to_array_unsafe lits2,sc2)
    ~op:(fun subst x y -> variant ~subst x y)

let apply_subst_arr ~renaming subst (lits,sc): t IArray.t =
  IArray.map (fun lit -> apply_subst ~renaming subst (lit,sc)) lits

let apply_subst_arr_no_renaming subst (lits,sc): t IArray.t =
  IArray.map (fun lit -> apply_subst_no_renaming subst (lit,sc)) lits
