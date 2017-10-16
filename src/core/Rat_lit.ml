
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

type term = Term.t

module T = Term
module S = Subst
module P = Position
module M = Monome
module MF = Monome.Focus

(** {2 Type Decls} *)

type op =
  | Equal
  | Less

type t = {
  op: op;
  left: Q.t Monome.t;
  right: Q.t Monome.t;
}

type lit = t

(** {2 Basics} *)

let equal lit1 lit2 =
  lit1.op = lit2.op &&
  M.equal lit1.left lit2.left &&
  M.equal lit1.right lit2.right

let equal_com lit1 lit2 = match lit1.op with
  | Less -> equal lit1 lit2
  | Equal ->
    lit1.op = lit2.op &&
    ( (M.equal lit1.left lit2.left &&
       M.equal lit1.right lit2.right) ||
      (M.equal lit1.left lit2.right &&
       M.equal lit1.right lit2.left))

let cmp_op : op -> op -> int = CCOrd.compare

let compare lit1 lit2 =
  let open CCOrd.Infix in
  cmp_op lit1.op lit2.op
  <?> (M.compare, lit1.left, lit2.left)
  <?> (M.compare, lit1.right, lit2.right)

let hash lit =
  CCHash.combine3
    (CCHash.poly lit.op)
    (M.hash lit.left)
    (M.hash lit.right)

let is_eq t = t.op = Equal
let is_less t = t.op = Less

(* main constructor *)
let make op m1 m2 =
  let m1, m2 = Monome.normalize m1, Monome.normalize m2 in
  let m = M.difference m1 m2 in
  (* build from a single monome *)
  let m1, m2 = M.split m in
  {op; left=m1; right=m2}

let mk_eq = make Equal
let mk_less = make Less

let pp out m =
  Format.fprintf out "%a %s %a"
    M.pp m.left
    (match m.op with Equal -> "=" | Less -> "<")
    M.pp m.right

let pp_tstp out m = match m.op with
  | Equal ->
    Format.fprintf out "%a = %a" M.pp_tstp m.left M.pp_tstp m.right
  | Less ->
    Format.fprintf out "$less(%a, %a)" M.pp_tstp m.left M.pp_tstp m.right

let pp_zf out m = match m.op with
  | Equal -> Format.fprintf out "%a = %a" M.pp_zf m.left M.pp_zf m.right
  | Less -> Format.fprintf out "(%a < %a)" M.pp_zf m.left M.pp_zf m.right

let to_string = CCFormat.to_string pp_tstp

(** {2 Operators} *)

let map f m = make m.op (M.map f m.left)(M.map f m.right)

let fold f acc m =
  let acc = Sequence.fold f acc (Monome.Seq.terms m.left) in
  Sequence.fold f acc (Monome.Seq.terms m.right)

type ('subst,'a) unif =
  subst:'subst -> 'a Scoped.t -> 'a Scoped.t -> 'subst Sequence.t

(* match {x1,y1} in scope 1, with {x2,y2} with scope2 *)
let unif4 op ~subst x1 y1 sc1 x2 y2 sc2 k =
  op ~subst (Scoped.make x1 sc1) (Scoped.make x2 sc2)
    (fun subst -> op ~subst (Scoped.make y1 sc1) (Scoped.make y2 sc2) k);
  op ~subst (Scoped.make y1 sc1) (Scoped.make x2 sc2)
    (fun subst -> op ~subst (Scoped.make x1 sc1) (Scoped.make y2 sc2) k);
  ()

let generic_unif m_unif ~subst (lit1,sc1) (lit2,sc2) k =
  let x1 = lit1.left
  and y1 = lit1.right
  and x2 = lit2.left
  and y2 = lit2.right in
  begin match lit1.op, lit2.op with
    | Equal, Equal ->
      (* try both ways *)
      unif4 m_unif ~subst x1 y1 sc1 x2 y2 sc2 k
    | Less, Less ->
      m_unif ~subst (x1,sc1) (x2,sc2)
        (fun subst -> m_unif ~subst (y1,sc1) (y2,sc2) k)
    | Equal, Less
    | Less, Equal -> ()
  end

let unify ?(subst=Unif_subst.empty) lit1 lit2 =
  generic_unif (fun ~subst -> M.unify ~subst) ~subst lit1 lit2

let matching ?(subst=Subst.empty) lit1 lit2 =
  generic_unif (fun ~subst -> M.matching ~subst) ~subst lit1 lit2

let variant ?(subst=Subst.empty) lit1 lit2 =
  generic_unif (fun ~subst -> M.variant ~subst) ~subst lit1 lit2

(* Interesting sub-part of the prover. This subsumption relation must be
   a subset of the implication relation, be decidable, but yet be as powerful
   as possible. A few examples:

    a ≤ 10 subsumes  2.a ≤ 21
    a = 1  subsumes a ≥ 0
    a ≥ 0  subsumes a ≥ -10
    2.a ≤ 10 subsumes a ≤ 11
*)
module Subsumption = struct
  (* verify postcondition of [matching] *)
  let _matching_postcond (m1,sc1) (m2,sc2) (subst,c1,c2) =
    let m1 = M.apply_subst Subst.Renaming.none subst (m1,sc1) in
    let m1 = M.product m1 c1
    and m2 = M.product m2 c2 in
    let d = M.difference m1 m2 in
    M.is_const d
    ||
    (
      Util.debugf 5 "@[<2>postcond:@ %a[%d] scaled %s,@ and %a[%d] scaled %s@ with %a@]"
        (fun k->k M.pp m1 sc1 (Q.to_string c1) M.pp m2 sc2 (Q.to_string c2) S.pp subst);
      false
    )

  (* match lists together exactly. No scaling authorized *)
  let rec match_lists ~protect ~subst l1 rest1 sc1 l2 sc2 k = match l1, l2 with
    | [], [] ->
      if rest1 = []
      then k subst
    | _, []
    | [], _ -> ()
    | (c1,t1)::l1', (c2,t2)::l2' when Q.leq c1 c2  ->
      begin try
          let subst =
            Unif.FO.matching_adapt_scope
              ~protect ~subst ~pattern:(Scoped.make t1 sc1) (Scoped.make t2 sc2)
          in
          if Q.equal c1 c2
          then match_lists ~protect ~subst
              (rest1 @ l1') [] sc1 l2' sc2 k
          else match_lists ~protect ~subst
              l1' rest1 sc1 ((Q.(c2 - c1),t2)::l2') sc2 k
        with Unif.Fail -> ()
      end;
      (* ignore [t1] for now *)
      match_lists ~protect ~subst l1' ((c1,t1)::rest1) sc1 l2 sc2 k
    | (c1,t1)::l1', (c2,_t2)::_l2' ->
      (* cannot match, c1 too high *)
      assert Q.(gt c1 zero);
      assert Q.(gt c2 zero);
      match_lists ~protect ~subst l1' ((c1,t1)::rest1) sc1 l2 sc2 k

  (* matching that is allowed to scale m1, and, if [scale2] is true, to
     scale [m2] too. Constants are not taken into account.
     [k] is called with [subst, c1, c2] where [c_i] is the scaling coefficient
     for [m_i].
     postcondition: for a result (subst,c1,c2),
      c1 * subst(m1.terms) = c2 * subst(m2.terms) *)
  let matching ~protect ~subst m1 sc1 m2 sc2 k =
    (* match some terms of [l1] with the given term [t2].
       [c1] is the accumulated coefficient for terms of [l1] so far. *)
    let rec init_with_coeff ~subst c1 l1 rest1 t2 c2 rest2 =
      begin match l1 with
        | [] when Q.(equal c1 zero) ->
          () (* no match with [t2]: no term matches [t2] *)
        | [] ->
          (* ok, we did match some terms with [t2]. Scale coefficients if
              possible, possibly changing their sign, otherwise fail  *)
          assert Q.(gt c1 zero);
          assert Q.(gt c2 zero);
          (* we take the [max] of coefficients, and multiply
             the first monome with [c2/max] and the second with [c1/max] *)
          let g = Q.max c1 c2 in
          check_other_terms ~subst
            ~scale1:(Q.div c2 g)
            ~scale2:(Q.div c1 g)
            rest1 rest2
        | (c1',t1) :: l1' ->
          (* choose [t1], if possible, and then extend the substitution for t2 *)
          begin try
              let subst = Unif.FO.matching_adapt_scope ~protect ~subst
                  ~pattern:(Scoped.make t1 sc1) (Scoped.make t2 sc2) in
              init_with_coeff ~subst Q.(c1 + c1') l1' rest1 t2 c2 rest2
            with Unif.Fail -> ()
          end;
          (* disregard [t1] *)
          init_with_coeff ~subst c1 l1' ((c1',t1)::rest1) t2 c2 rest2
      end
    (* match other terms with the given scaling coeff *)
    and check_other_terms ~subst ~scale1 ~scale2 l1 l2 =
      let l1 = List.map (fun (c,t) -> Q.(c * scale1), t) l1
      and l2 = List.map (fun (c,t) -> Q.(c * scale2), t) l2 in
      match_lists ~protect ~subst l1 [] sc1 l2 sc2
        (fun subst ->
           assert (_matching_postcond (Scoped.make m1 sc1) (Scoped.make m2 sc2) (subst, scale1, scale2));
           k (subst, scale1, scale2))
    in
    begin match M.coeffs m2 with
      | [] ->
        begin match M.coeffs m1 with
          | [] -> k (subst,Q.one,Q.one)
          | _::_ -> ()  (* fail *)
        end
      | (c2,t2)::l2 ->
        (* start with matching terms of [m1] with [t2] *)
        init_with_coeff ~subst Q.zero (M.coeffs m1) [] t2 c2 l2
    end

  (* match l1 with l2, and r1 with r2, with the same scaling coefficient *)
  let matching2 ~subst l1 r1 sc1 l2 r2 sc2 k =
    let protect = Sequence.append (M.Seq.vars l2) (M.Seq.vars r2) in
    if M.is_const l1 && M.is_const l2 then
      (* only one problem *)
      matching ~protect ~subst r1 sc1 r2 sc2 k
    else
      matching ~protect ~subst l1 sc1 l2 sc2
        (fun (subst,c1,c2) ->
           let r1 = List.map (fun (c,t) -> Q.(c * c1), t) (M.coeffs r1)
           and r2 = List.map (fun (c,t) -> Q.(c * c2), t) (M.coeffs r2) in
           match_lists ~protect ~subst r1 [] sc1 r2 sc2
             (fun subst -> k (subst,c1,c2)))

  let check ~subst lit1 sc1 lit2 sc2 k =
    let l1 = lit1.left
    and r1 = lit1.right
    and l2 = lit2.left
    and r2 = lit2.right in
    begin match lit1.op, lit2.op with
      | Equal, Equal ->
        (* match modulo commutativity *)
        matching2 ~subst l1 r1 sc1 l2 r2 sc2
          (fun (subst, c1, c2) ->
             if Q.(equal
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const r2 - M.const l2)))
             then k subst);
        matching2 ~subst l1 r1 sc1 r2 l2 sc2
          (fun (subst, c1, c2) ->
             if Q.(equal
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const l2 - M.const r2)))
             then k subst)
      | Equal, Less ->
        (* l1=r1  can subsume l2<r2 if
           r1.const-l1.const = subst(l1-r1) = l2-r2 < r2.const - l2.const
           with r1.const-l1.const < r2.const - l2.const (tighter bound) *)
        matching2 ~subst l1 r1 sc1 l2 r2 sc2
          (fun (subst, c1, c2) ->
             if Q.(lt
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const r2 - M.const l2)))
             then k subst)
      | Less, Less ->
        (* if subst(r1 - l1) = r2-l2 - k where k≥0, then l1<r1 => l2+k<r2 => l2<r2
            so l1<r1 subsumes l2<r2. *)
        matching2 ~subst l1 r1 sc1 l2 r2 sc2
          (fun (subst, c1, c2) ->
             (* we removed all terms but the constants,
                 l1-r1 = l2-r2. Now lit1= l1-r1 < r1.const-l1.const,
                 so if r1.const-l1.const < r2.const - l2.const then lit1 => lit2 *)
             if Q.(leq
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const r2 - M.const l2)))
             then k subst);
      | _ -> () (* fail *)
    end
end

let subsumes ?(subst=Subst.empty) (lit1,sc1) (lit2, sc2) k =
  Subsumption.check ~subst lit1 sc1 lit2 sc2 k

let are_variant lit1 lit2 =
  not (Sequence.is_empty (variant (lit1, 0)(lit2, 1)))

let apply_subst renaming subst (lit,scope) =
  make lit.op
    (M.apply_subst renaming subst (lit.left, scope))
    (M.apply_subst renaming subst (lit.right, scope))

let apply_subst_no_simp renaming subst (lit,sc) =
  {lit with
     left=M.apply_subst_no_simp renaming subst (lit.left, sc);
     right=M.apply_subst_no_simp renaming subst (lit.right, sc);
  }

let is_trivial lit = match lit.op with
  | Equal -> M.equal lit.left lit.right
  | Less -> M.dominates ~strict:true lit.right lit.left

let is_absurd lit = match lit.op with
  | Equal ->
    let m = M.difference lit.left lit.right in
    M.is_const m && M.sign m <> 0
  | Less -> M.dominates ~strict:false lit.left lit.right

let fold_terms ?(pos=P.stop) ?(vars=false) ?ty_args ~which ~ord ~subterms lit k =
  (* function to call at terms *)
  let at_term ~pos t k =
    if subterms
    then T.all_positions ?ty_args ~vars ~pos t k
    else (* don't do anything if [t] is a var and [vars=false] *)
    if vars || not (T.is_var t) then k (t,pos)
  and fold_monome = match which with
    | `All -> M.fold
    | `Max -> M.fold_max ~ord
  in
  fold_monome
    (fun () i _ t -> at_term ~pos:P.(append pos (left (arg i stop))) t k)
    () lit.left;
  fold_monome
    (fun () i _ t -> at_term ~pos:P.(append pos (right (arg i stop))) t k)
    () lit.right;
  ()

let _to_coeffs lit =
  Sequence.append (M.Seq.coeffs_swap lit.left) (M.Seq.coeffs_swap lit.right)

(* FIXME: ignore multiplicities, for now, but would need to
   bring everything to same numerator? *)
let to_multiset lit =
  _to_coeffs lit |> Sequence.map fst
  |> Multisets.MT.Seq.of_seq Multisets.MT.empty

let max_terms ~ord lit =
  let m = to_multiset lit in
  Multisets.MT.max (Ordering.compare ord) m
  |> Multisets.MT.to_list
  |> List.map fst

let to_form m =
  let t1 = M.Rat.to_term m.left in
  let t2 = M.Rat.to_term m.right in
  begin match m.op with
    | Equal -> SLiteral.eq t1 t2
    | Less ->
      let t = T.app_builtin Builtin.Less ~ty:Type.prop [T.ty t1 |> T.of_ty; t1; t2] in
      SLiteral.atom t true
  end

let to_term m: term =
  let m1 = M.Rat.to_term m.left in
  let m2 = M.Rat.to_term m.right in
  begin match m.op with
    | Equal -> T.app_builtin ~ty:Type.prop Builtin.Eq [m1; m2]
    | Less -> T.app_builtin ~ty:Type.prop Builtin.Less [m1; m2]
  end

(** {2 Iterators} *)

module Seq = struct
  let terms lit k = M.Seq.terms lit.left k; M.Seq.terms lit.right k
  let vars lit = terms lit |> Sequence.flat_map T.Seq.vars
  let to_multiset = _to_coeffs
end

(** {2 Focus on a Term} *)

module Focus = struct
  (** focus on a term in one of the two monomes *)
  type t =
    | Left of op * Q.t Monome.Focus.t * Q.t Monome.t
    | Right of op * Q.t Monome.t * Q.t Monome.Focus.t

  let mk_left op mf m = Left (op, mf, m)
  let mk_right op m mf = Right (op, m, mf)

  let get lit pos = match pos with
    | P.Left (P.Arg (i, _)) ->
      Some (Left (lit.op, M.Focus.get lit.left i, lit.right))
    | P.Right (P.Arg (i, _)) ->
      Some (Right (lit.op, lit.left, M.Focus.get lit.right i))
    | _ -> None

  let get_exn lit pos = match get lit pos with
    | None ->
      invalid_arg
        (CCFormat.sprintf "wrong position %a for focused arith lit %a"
           P.pp pos pp lit)
    | Some x -> x

  let focus_term lit t =
    begin match M.Focus.focus_term lit.left t with
      | Some mf1 ->
        assert (not (M.mem lit.right t));
        Some (Left (lit.op, mf1, lit.right))
      | None ->
        match M.Focus.focus_term lit.right t with
          | None -> None
          | Some mf2 -> Some (Right (lit.op, lit.left, mf2))
    end

  let focus_term_exn lit t = match focus_term lit t with
    | None -> failwith "ALF.focus_term_exn"
    | Some lit' -> lit'

  let replace a by = match a with
    | Left (op, mf, m) -> make op (M.sum (MF.rest mf) by) m
    | Right (op, m, mf) -> make op m (M.sum (MF.rest mf) by)

  let focused_monome = function
    | Left (_, mf, _)
    | Right (_, _, mf) -> mf

  let opposite_monome = function
    | Left (_, _, m)
    | Right (_, m, _) -> m

  let term lit = MF.term (focused_monome lit)

  let fold_terms ?(pos=P.stop) lit k =
    MF.fold_m ~pos:P.(append pos (left stop)) lit.left ()
      (fun () mf pos -> k (Left (lit.op, mf, lit.right), pos));
    MF.fold_m ~pos:P.(append pos (right stop)) lit.right ()
      (fun () mf pos -> k (Right (lit.op, lit.left, mf), pos))

  (* is the focused term maximal in the arithmetic literal? *)
  let is_max ~ord = function
    | Left (_, mf, m)
    | Right (_, m, mf) ->
      let t = MF.term mf in
      let terms = Sequence.append (M.Seq.terms m) (MF.rest mf |> M.Seq.terms) in
      Sequence.for_all
        (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
        terms

  (* is the focused term maximal in the arithmetic literal? *)
  let is_strictly_max ~ord = function
    | Left (_, mf, m)
    | Right (_, m, mf) ->
      let t = MF.term mf in
      Sequence.for_all
        (fun t' -> Ordering.compare ord t t' = Comparison.Gt)
        (M.Seq.terms m)
      &&
      Sequence.for_all
        (fun t' -> Ordering.compare ord t t' = Comparison.Gt)
        (MF.rest mf |> M.Seq.terms)

  let map_lit ~f_m ~f_mf lit = match lit with
    | Left (op, mf, m) ->
      Left (op, f_mf mf, f_m m)
    | Right (op, m, mf) ->
      Right (op, f_m m, f_mf mf)

  let product lit z =
    map_lit
      ~f_mf:(fun mf -> MF.product mf z)
      ~f_m:(fun m -> M.product m z)
      lit

  let apply_subst renaming subst (lit,sc) =
    map_lit
      ~f_mf:(fun mf -> MF.apply_subst renaming subst (mf,sc))
      ~f_m:(fun m -> M.apply_subst renaming subst (m,sc))
      lit

  let unify ?(subst=Unif_subst.empty) (lit1,sc1) (lit2,sc2) k =
    let _set_mf lit mf = match lit with
      | Left (op, _, m) -> Left (op, mf, m)
      | Right (op, m, _) -> Right (op, m, mf)
    in
    MF.unify_ff ~subst (focused_monome lit1,sc1) (focused_monome lit2,sc2)
      (fun (mf1, mf2, subst) ->
         k (_set_mf lit1 mf1, _set_mf lit2 mf2, subst))

  (* scale focused literals to have the same coefficient *)
  let scale l1 l2 =
    let z1 = MF.coeff (focused_monome l1)
    and z2 = MF.coeff (focused_monome l2) in
    let max = Q.max z1 z2 in
    product l1 (Q.div z2 max), product l2 (Q.div z1 max)

  let op = function
    | Left (op, _, _)
    | Right (op, _, _) -> op

  let unfocus = function
    | Left (op, m1_f, m2) -> make op (MF.to_monome m1_f) m2
    | Right (op, m1, m2_f) -> make op m1 (MF.to_monome m2_f)

  let pp out lit =
    let op2str = function
      | Equal -> "="
      | Less -> "<"
    in
    begin match lit with
      | Left (op, mf, m) ->
        Format.fprintf out "@[%a %s@ %a@]" MF.pp mf (op2str op) M.pp m
      | Right (op, m, mf) ->
        Format.fprintf out "@[%a %s@ %a@]" M.pp m (op2str op) MF.pp mf
    end

  let to_string = CCFormat.to_string pp
end
