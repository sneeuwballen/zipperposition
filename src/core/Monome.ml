
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Polynomes of order 1, over several variables}. *)

module US = Unif_subst
module T = Term

type term = Term.t

(** Typeclass num *)
type 'a num = {
  ty : Type.t;
  equal : 'a -> 'a -> bool;
  sign : 'a -> int;
  abs : 'a -> 'a;
  cmp : 'a -> 'a -> int;
  raise_to_lcm: 'a -> 'a -> 'a * 'a; (* factors to reach lcm *)
  hash : 'a Hash.t;
  zero : 'a;
  one : 'a;
  add : 'a -> 'a -> 'a;
  sub : 'a -> 'a -> 'a;
  mult : 'a -> 'a -> 'a;
  uminus : 'a -> 'a;
  minus : 'a -> 'a -> 'a;
  to_term : 'a -> term;
  to_string : 'a -> string;
  of_int: Z.t -> 'a;
  of_rat: Q.t -> 'a option;
}

let z : Z.t num = {
  ty = Type.int;
  equal = Z.equal;
  sign = Z.sign;
  abs = Z.abs;
  cmp = Z.compare;
  raise_to_lcm = (fun a b ->
    let gcd = Z.gcd a b in
    Z.divexact b gcd, Z.divexact a gcd);
  hash = Z.hash;
  zero = Z.zero;
  one = Z.one;
  add = Z.add;
  sub = Z.sub;
  mult = Z.mul;
  uminus = Z.neg;
  minus = Z.sub;
  to_term = (fun n -> T.builtin ~ty:Type.int (Builtin.mk_int n));
  to_string = Z.to_string;
  of_int=(fun x->x);
  of_rat=(fun _ -> None);
}

let q : Q.t num = {
  ty = Type.rat;
  equal = Q.equal;
  sign = Q.sign;
  abs = Q.abs;
  cmp = Q.compare;
  raise_to_lcm = (fun a b ->
    assert (Q.sign a <> 0);
    assert (Q.sign b <> 0);
    let gcd = Q.max a b in
    Q.div b gcd, Q.div a gcd);
  hash = (fun q-> CCHash.string (Q.to_string q));
  zero = Q.zero;
  one = Q.one;
  add = Q.add;
  sub = Q.sub;
  mult = Q.mul;
  uminus = Q.neg;
  minus = Q.sub;
  to_term = (fun n -> T.builtin ~ty:Type.rat (Builtin.mk_rat n));
  to_string = Q.to_string;
  of_int = Q.of_bigint;
  of_rat = (fun x->Some x);
}

type 'a t = {
  num : 'a num;
  const : 'a;
  terms : ('a * term) list;
}
type 'a monome = 'a t

let equal m1 m2 =
  assert (m1.num == m2.num);
  m1.num.cmp m1.const m2.const = 0 &&
  CCList.equal
    (fun (a1,t1) (a2, t2) -> m1.num.cmp a1 a2 = 0 && T.equal t1 t2)
    m1.terms m2.terms

let compare m1 m2 =
  let open CCOrd in
  let cmp_pair (s1,t1) (s2,t2) =
    m1.num.cmp s1 s2 <?> (T.compare, t1, t2)
  in
  m1.num.cmp m1.const m2.const
  <?> (CCOrd.list cmp_pair, m1.terms, m2.terms)

let hash m =
  Hash.combine3 42
    (m.num.hash m.const)
    (Hash.list (Hash.pair m.num.hash T.hash) m.terms)

let ty m = m.num.ty

let const m = m.const
let coeffs m = m.terms

(* merge two lists and maintain them sorted. Symbols for a given term
    are combined using [op]. terms occurring only one one side
    are preserved. *)
let rec _merge ~num op l1 l2 = match l1, l2 with
  | l, []
  | [], l -> l
  | (s1, t1)::l1', (s2, t2)::l2' ->
    match T.compare t1 t2 with
      | 0 ->
        let s' = op s1 s2 in
        if num.cmp num.zero s' = 0
        then _merge ~num op l1' l2'  (* t disappears *)
        else (s', t1) :: _merge ~num op l1' l2'
      | n when n < 0 -> (s1, t1) :: _merge ~num op l1' l2
      | _ -> (s2, t2) :: _merge ~num op l1 l2'

(* map [f] on all symbols of [e] *)
let _fmap f e =
  let terms = CCList.filter_map
      (fun (s,t) ->
         let s' = f s in
         if e.num.cmp e.num.zero s' = 0
         then None  (* [t] doesn't occur anymore *)
         else Some (s', t))
      e.terms
  in
  { e with const=f e.const; terms; }

let map_num = _fmap

let mk_const ~num s =
  { num; const=s; terms=[]; }

let singleton ~num coeff t =
  assert (Type.equal (T.ty t) num.ty);
  if num.cmp num.zero coeff = 0
  then mk_const ~num coeff  (* 0 *)
  else
    let terms = [coeff, t] in
    let const = num.zero in
    { num; terms; const; }

let find e t =
  let rec find l t = match l with
    | [] -> None
    | (s, t')::_ when T.equal t t' -> Some s
    | _::l' -> find l' t
  in
  find e.terms t

let find_exn e t = match find e t with
  | None -> raise Not_found
  | Some s -> s

let mem e t =
  match find e t with
    | None -> false
    | Some _ -> true

let add e s t =
  assert (Type.equal (T.ty t) e.num.ty);
  (* sorted insertion *)
  let rec add l s t = match l with
    | [] -> [s, t]
    | (s', t')::l' ->
      begin match T.compare t t' with
        | 0 ->
          let s'' = e.num.add s s' in
          if e.num.cmp e.num.zero s'' = 0
          then l'
          else (s'', t) :: l'
        | n when n < 0 -> (s, t) :: l
        | _ -> (s', t') :: add l' s t
      end
  in
  { e with terms = add e.terms s t; }

let mk_const ~num const = { num; const; terms=[]; }

let of_list ~num s l =
  List.fold_left
    (fun e (s,t) -> add e s t)
    (mk_const ~num s) l

let map f e =
  let const = {e with terms = []} in
  List.fold_left
    (fun e (n, t) -> add e n (f t))
    const e.terms

let add_const e s =
  { e with const = e.num.add e.const s; }

let remove e t =
  { e with
      terms = List.filter (fun (_, t') -> not (T.equal t t')) e.terms; }

let remove_const e =
  { e with const = e.num.zero; }

let add_list m l = List.fold_left (fun m (c,t) -> add m c t) m l

module Seq = struct
  let terms m =
    fun k -> List.iter (fun (_, t) -> k t) m.terms

  let vars m =
    Sequence.flat_map T.Seq.vars (terms m)

  let coeffs m =
    fun k -> List.iter k m.terms

  let coeffs_swap m k =
    List.iter (fun (x,y) -> k (y,x)) m.terms
end

let is_const e = match e.terms with | [] -> true | _ -> false

let is_zero e = is_const e && e.num.sign e.const = 0

let sign m =
  if not (is_const m) then invalid_arg "Monome.sign";
  m.num.sign m.const

let size e = List.length e.terms

let terms m = List.map snd m.terms

let to_list e = e.terms

let var_occurs ~var e =
  List.exists (fun (_, t) -> T.var_occurs ~var t) e.terms

let sum e1 e2 =
  assert (e1.num == e2.num);
  let const = e1.num.add e1.const e2.const in
  let terms = _merge ~num:e1.num e1.num.add e1.terms e2.terms in
  { e1 with const; terms; }

let uminus e = _fmap e.num.uminus e

let difference e1 e2 =
  sum e1 (uminus e2)

let product e s = _fmap (fun s' -> e.num.mult s s') e

let succ e = add_const e e.num.one

let pred e = add_const e (e.num.uminus e.num.one)

let rec sum_list = function
  | [] -> failwith "Monome.sum_list"
  | [m] -> m
  | m::l' -> sum m (sum_list l')

let comparison m1 m2 =
  assert (m1.num == m2.num);
  (* if m1-m2 is a constant, they are comparable, otherwise it
      depends on the model/instance *)
  let m = difference m1 m2 in
  match is_const m, m.num.sign m.const with
    | false, _ -> Comparison.Incomparable
    | true, 0 -> Comparison.Eq
    | true, n when n < 0 -> Comparison.Lt
    | true, _ -> Comparison.Gt

let dominates ~strict m1 m2 = match comparison m1 m2 with
  | Comparison.Eq -> not strict
  | Comparison.Gt -> true
  | Comparison.Lt | Comparison.Incomparable -> false

let split m =
  let const1, const2 = if m.num.sign m.const >= 0
    then m.const, m.num.zero
    else m.num.zero, m.num.abs m.const
  in
  let rec partition = function
    | [] -> [], []
    | (c,t)::l' ->
      let l1, l2 = partition l' in
      if m.num.sign c > 0
      then (c,t)::l1, l2
      else l1, (m.num.uminus c, t)::l2
  in
  let terms1, terms2 = partition m.terms in
  let m1 = {m with terms=terms1; const=const1; } in
  let m2 = {m with terms=terms2; const=const2; } in
  m1, m2

let apply_subst renaming subst (m,sc) =
  map
    (fun t -> Subst.FO.apply renaming subst (t,sc))
    m

let apply_subst_no_simp renaming subst (m,sc) = {
  m with
    terms=List.map (fun (c,t) -> c, Subst.FO.apply renaming subst (t,sc)) m.terms;
}

let is_ground m =
  List.for_all (fun (_, t) -> T.is_ground t) m.terms

let fold f acc m =
  CCList.foldi
    (fun acc i (n, t) -> f acc i n t)
    acc m.terms

module MT = Multisets.MT

let fold_max ~ord f acc m =
  (* set of max terms *)
  let max =
    Seq.terms m
    |> MT.Seq.of_seq MT.empty
    |> MT.max_seq (Ordering.compare ord)
    |> Sequence.map fst
    |> T.Seq.add_set T.Set.empty
  in
  CCList.foldi
    (fun acc i (c, t) -> if T.Set.mem t max then f acc i c t else acc)
    acc m.terms

let pp_ ~mult ~add ~pp_t out e =
  let pp_pair out (s, t) =
    if e.num.cmp s e.num.one = 0
    then pp_t out t
    else Format.fprintf out "%s %s %a" (e.num.to_string s) mult pp_t t
  in
  match e.terms with
    | [] -> CCFormat.string out (e.num.to_string e.const)
    | _::_ when e.num.sign e.const = 0 ->
      Util.pp_list ~sep:add pp_pair out e.terms
    | _::_ ->
      Format.fprintf out "%a%s%s"
        (Util.pp_list ~sep:add pp_pair) e.terms
        add
        (e.num.to_string e.const)

let pp out e = pp_ ~mult:"×" ~add:" + " ~pp_t:T.pp out e
let pp_zf out e = pp_ ~mult:" * " ~add:" + " ~pp_t:T.ZF.pp out e

let to_string m = CCFormat.to_string pp m

let pp_tstp out e =
  let rec pp_pair out (s, t) =
    if e.num.cmp s e.num.one = 0
    then T.TPTP.pp out t
    else Format.fprintf out "$product(%s, %a)" (e.num.to_string s) T.TPTP.pp t
  and pp_list buf l = match l with
    | [] -> ()
    | [s, t] -> pp_pair buf (s, t)
    | (s, t)::l' ->
      Format.fprintf buf "$sum(%a, %a)" pp_pair (s,t) pp_list l'
  in
  match e.terms with
    | [] -> CCFormat.string out (e.num.to_string e.const)
    | _::_ when e.num.sign e.const = 0 -> pp_list out e.terms
    | _::_ ->
      Format.fprintf out "$sum(%s, %a)" (e.num.to_string e.const) pp_list e.terms

let _fail_idx m i =
  invalid_arg (CCFormat.sprintf "invalid index %d in %a" i pp m)

let nth m n =
  try List.nth m.terms n
  with _ -> _fail_idx m n

let set m n (c,t) =
  try
    let terms = CCList.set_at_idx n (c,t) m.terms in
    {m with terms; }
  with _ -> _fail_idx m n

let set_term m n t =
  try
    let (c, _) = List.nth m.terms n in
    let terms = CCList.set_at_idx n (c,t) m.terms in
    {m with terms; }
  with _ -> _fail_idx m n

module Focus = struct
  type 'a t = {
    term : term;
    coeff : 'a;
    rest : 'a monome;
  }

  let get m i =
    try
      let coeff, term = List.nth m.terms i in
      assert (m.num.sign coeff <> 0);
      let rest = {m with terms=CCList.remove_at_idx i m.terms} in
      { term; coeff; rest; }
    with _ -> _fail_idx m i

  (* TODO: optimize *)
  let focus_term m term =
    match find m term with
      | None -> None
      | Some coeff ->
        let rest = remove m term in
        Some {coeff; rest; term; }

  let focus_term_exn m t = match focus_term m t with
    | None -> failwith "focus_term_exn"
    | Some x -> x

  let sum t m =
    assert (t.rest.num == m.num);
    { t with rest = sum t.rest m; }

  let difference t m =
    assert (t.rest.num == m.num);
    { t with rest = difference t.rest m; }

  let uminus t =
    let num = t.rest.num in
    { t with coeff = num.uminus t.coeff; rest=uminus t.rest; }

  let product t z =
    let num = t.rest.num in
    if num.sign z = 0 then invalid_arg "Monome.Lit.product";
    { t with coeff=num.mult t.coeff z; rest=product t.rest z; }

  let to_monome t =
    add t.rest t.coeff t.term

  let coeff t = t.coeff
  let term t = t.term
  let rest t = t.rest

  (* scale focused monomes to have the same coefficient *)
  let scale m1 m2 =
    let num = m1.rest.num in
    let n1, n2 = num.raise_to_lcm m1.coeff m2.coeff in
    product m1 n1, product m2 n2

  let pp out t =
    let num = t.rest.num in
    (* print the focused part *)
    let pp_focused out t =
      if num.cmp num.one t.coeff = 0
      then T.pp out t.term
      else Format.fprintf out "%s·%a" (num.to_string t.coeff) T.pp t.term
    in
    if is_zero t.rest
    then Format.fprintf out "[%a]" pp_focused t
    else Format.fprintf out "[%a] + %a" pp_focused t pp t.rest

  let is_max ~ord mf =
    List.for_all
      (fun (_, t) -> match Ordering.compare ord mf.term t with
         | Comparison.Lt -> false  (* [t > mf.term] *)
         | _ -> true)
      mf.rest.terms

  let fold_m ~pos m acc f =
    CCList.foldi
      (fun acc i (c,t) ->
         let pos = Position.(append pos (arg i stop)) in
         let rest = {m with terms=CCList.remove_at_idx i m.terms} in
         let mf = {coeff=c; term=t; rest;} in
         f acc mf pos
      ) acc m.terms

  let _apply_subst how subst (mf,sc) =
    let rest = map (fun t -> how subst (t,sc)) mf.rest in
    let term = how subst (mf.term,sc) in
    (* if [term] occurs in the new [rest], remove it and add its
       coefficient. *)
    let coeff, rest =
      if mem rest term
      then (rest.num.add mf.coeff (find_exn rest term), remove rest term)
      else (mf.coeff, rest)
    in
    if rest.num.sign coeff = 0 then failwith "Monome.Focus.apply_subst: coeff 0";
    {coeff; rest; term; }

  let apply_subst renaming subst mf =
    _apply_subst (Subst.FO.apply renaming) subst mf

  let _id x = x
  let map ?(term=_id) ?(coeff=_id) ?(rest=_id) mf =
    { term=term mf.term; coeff=coeff mf.coeff; rest=rest mf.rest; }

  (* unification between terms of the same monome *)
  let rec _iter_self ~num ~subst c t l rest const scope k =
    match l with
      | [] ->
        let mf' = { coeff=c; term=t; rest=of_list ~num const rest;} in
        if num.sign c <> 0 then k (mf', subst)
      | (c', t') :: l' ->
        if Unif.FO.equal ~subst:(US.subst subst)
            (Scoped.make t scope) (Scoped.make t' scope)
        then (
          (* we do not have a choice, [t = t'] is true *)
          _iter_self ~num ~subst (num.add c c') t l' rest const scope k
        ) else (
          begin
            try
              (* maybe we can merge [t] and [t'] *)
              let subst' = Unif.FO.unify_full ~subst
                  (Scoped.make t scope) (Scoped.make t' scope)
              in
              (* move back [rest] into the main list, some terms might be equal
                 to [t] now *)
              _iter_self ~num ~subst:subst' (num.add c c') t (l'@ rest) [] const scope k
            with Unif.Fail -> ()
          end;
          (* we can also choose not to unify [t] and [t']. *)
          _iter_self ~num ~subst c t l' ((c',t')::rest) const scope k
        )

  let unify_self ?(subst=US.empty) (mf,sc) k =
    let num = mf.rest.num in
    _iter_self ~num ~subst mf.coeff mf.term mf.rest.terms [] mf.rest.const sc k

  let unify_self_monome ?(subst=US.empty) (m,sc) k: unit =
    let num = m.num in
    let rec choose_first subst l rest = match l with
      | [] -> ()
      | (c,t)::l' ->
        choose_second subst c t l' rest;
        choose_first subst l' ((c,t)::rest)
    and choose_second subst c t l rest = match l with
      | [] -> ()
      | (c',t')::l' ->
        (* see whether we can unify t and t' *)
        begin
          try
            let subst = Unif.FO.unify_full ~subst (t,sc) (t',sc) in
            (* extend the unifier *)
            _iter_self ~num ~subst (num.add c c') t (l'@rest) [] m.const sc k
          with Unif.Fail -> ()
        end;
        (* ignore t' and search another partner *)
        choose_second subst c t l' ((c',t')::rest)
    in
    choose_first subst m.terms []

  let unify_ff ?(subst=US.empty) (mf1,sc1) (mf2,sc2) k =
    assert(mf1.rest.num == mf2.rest.num);
    let num = mf1.rest.num in
    try
      let subst = Unif.FO.unify_full ~subst (mf1.term,sc1) (mf2.term,sc2) in
      _iter_self ~num ~subst mf1.coeff mf1.term mf1.rest.terms
        [] mf1.rest.const sc1
        (fun (mf1, subst) ->
           _iter_self ~num ~subst mf2.coeff mf2.term mf2.rest.terms
             [] mf2.rest.const sc2
             (fun (mf2, subst) -> k (mf1, mf2, subst)))
    with Unif.Fail -> ()

  let unify_mm ?(subst=US.empty) (m1,sc1) (m2,sc2) k =
    assert(m1.num==m2.num);
    let num = m1.num in
    (* unify a term of [m1] with a term of [m2] *)
    let rec choose_first subst l1 rest1 cst1 l2 rest2 cst2 k = match l1, l2 with
      | [], _
      | _, [] -> ()
      | (c1,t1)::l1', (c2,t2)::l2' ->
        (* first, choose [t1] and [t2] if they are unifiable, and extend
            the unifier to the other terms if needed. *)
        assert (num.sign c1 <> 0 && num.sign c2 <> 0);
        begin
          try
            let subst = Unif.FO.unify_full ~subst (t1,sc1) (t2,sc2) in
            Util.debugf 5 "@[<2>unify_mm :@ @[%a = %a@]@ with @[%a@]@]"
              (fun k->k T.pp t1 T.pp t2 US.pp subst);
            _iter_self ~num ~subst c1 t1 (l1'@rest1) [] m1.const sc1
              (fun (mf1, subst) ->
                 _iter_self ~num ~subst c2 t2 (l2'@rest2) [] m2.const sc2
                   (fun (mf2, subst) -> k (mf1, mf2, subst))
              )
          with Unif.Fail -> ()
        end;
        (* don't choose [t1] *)
        choose_first subst l1' ((c1,t1)::rest1) cst1 l2 rest2 cst2 k;
        (* don't choose [t2] *)
        choose_first subst l1 rest1 cst1 l2' ((c2,t2)::rest2) cst2 k
    in
    choose_first subst m1.terms [] m1.const m2.terms [] m2.const k

  (*
  let unify_fm ?(subst=Subst.empty) mf1 s1 m2 s2 k =
    assert false  (* TODO? unify_fm *)
  *)
end

let variant ?(subst=Subst.empty) (m1,sc1) (m2,sc2) k =
  assert (m1.num == m2.num);
  let rec traverse_lists subst (c1,t1) l1' rest2 l2 = match l2 with
    | [] -> ()  (* fail *)
    | (c2,t2)::l2' ->
      if m1.num.cmp c1 c2 = 0
      then
        try
          let subst = Unif.FO.variant ~subst (t1,sc1) (t2,sc2) in
          start subst l1' (rest2 @ l2')
        with Unif.Fail -> ();
          traverse_lists subst (c1,t1) l1' ((c2,t2)::rest2) l2'
  and start subst l1 l2 = match l1, l2 with
    | [], [] -> k subst
    | [], _ | _, [] -> ()
    | (c1,t1)::l1', _ -> traverse_lists subst (c1,t1) l1' [] l2
  in
  if m1.num.cmp m1.const m2.const <> 0 then ()
  else start subst m1.terms m2.terms

(* ok, matching is going to be slightly more complicated. For instance,
   a monome   f(X)+f(Y)+a matches 2.f(b) + a   with X=Y=b.
   note that we don't implement correctly matching variables against monomes,
   for instance X = a+b will not work (although X+Y=a+b will yield two substs).
   Also, matching X+f(a) with 1+f(a) will not work.

   In summary naked variables are evil. *)
let matching ?(subst=Subst.empty) (m1,sc1)(m2,sc2) k =
  assert (m1.num == m2.num);
  let rec traverse_lists subst (c1,t1) l1' rest2 l2 = match l2 with
    | [] -> ()
    | (c2,t2)::l2' ->
      if m1.num.cmp c1 c2 <= 0
      then (
        try
          let subst = Unif.FO.matching_adapt_scope ~subst ~pattern:(t1,sc1) (t2,sc2) in
          if m1.num.cmp c1 c2 = 0
          then start subst l1' (rest2 @ l2')
          else
            (* some instances of t2 remain to be matched *)
            start subst l1' ((m1.num.sub c2 c1, t2) :: l2' @ rest2)
        with Unif.Fail -> ()
      );
      traverse_lists subst (c1,t1) l1' ((c2,t2)::rest2) l2'
  and start subst l1 l2 = match l1, l2 with
    | [], [] -> k subst
    | [], _ | _, [] -> ()
    | (c1,t1)::l1', _ -> traverse_lists subst (c1,t1) l1' [] l2
  in
  if m1.num.cmp m1.const m2.const <> 0 then ()
  else start subst m1.terms m2.terms

let unify ?(subst=Unif_subst.empty) (m1,sc1)(m2,sc2) k =
  assert (m1.num == m2.num);
  let rec traverse_lists subst (c1,t1) l1' rest2 l2 = match l2 with
    | [] -> ()
    | (c2,t2)::l2' ->
      begin
        try
          let subst = Unif.FO.unify_full ~subst (t1,sc1) (t2,sc2) in
          match m1.num.cmp c1 c2 with
            | 0 -> start subst l1' (rest2 @ l2')  (* t1 removed *)
            | n when n<0 ->
              (* t1 removed *)
              start subst l1' ((m1.num.sub c2 c1, t2) :: l2' @ rest2)
            | _ ->
              (* t2 removed *)
              start subst ((m1.num.sub c1 c2, t1) :: l1') (l2' @ rest2)
        with Unif.Fail -> ()
      end;
      traverse_lists subst (c1,t1) l1' ((c2,t2)::rest2) l2'
  and start subst l1 l2 = match l1, l2 with
    | [], [] -> k subst
    | [], _ | _, [] -> ()
    | (c1,t1)::l1', _ -> traverse_lists subst (c1,t1) l1' [] l2
  in
  if m1.num.cmp m1.const m2.const <> 0 then ()
  else start subst m1.terms m2.terms

exception NotLinear

(* convert term to monome, or raise NotLinear *)
let of_term_exn (type a)(num:a num) t =
  let of_rat_exn q = match num.of_rat q with
    | None -> raise NotLinear
    | Some n -> n
  in
  let rec of_term t = match T.view t with
    | T.AppBuiltin (Builtin.Sum, [_; t1; t2]) ->
      let m1 = of_term t1 in
      let m2 = of_term t2 in
      sum m1 m2
    | T.AppBuiltin (Builtin.Difference, [_;t1; t2]) ->
      let m1 = of_term t1 in
      let m2 = of_term t2 in
      difference m1 m2
    | T.AppBuiltin (Builtin.Uminus, [_;t']) ->
      let m = of_term t' in
      uminus m
    | T.AppBuiltin (Builtin.Product, [_;t1; t2]) ->
      begin match T.view t1, T.view t2 with
        | T.AppBuiltin (Builtin.Int n, []), _ ->
          product (of_term t2) (num.of_int n)
        | T.AppBuiltin (Builtin.Rat n, []), _ ->
          product (of_term t2) (of_rat_exn n)
        | _, T.AppBuiltin (Builtin.Int n, []) ->
          product (of_term t1) (num.of_int n)
        | _, T.AppBuiltin (Builtin.Rat n, []) ->
          product (of_term t1) (of_rat_exn n)
        | _ -> raise NotLinear
      end
    | T.AppBuiltin (Builtin.Succ, [_;t']) -> succ (of_term t')
    | T.AppBuiltin (Builtin.Prec, [_;t']) -> pred (of_term t')
    | T.AppBuiltin (Builtin.Int n, []) -> mk_const ~num (num.of_int n)
    | T.AppBuiltin (Builtin.Rat n, []) -> mk_const ~num (of_rat_exn n)
    | T.AppBuiltin (b, _) when Builtin.is_arith b ->
      raise NotLinear
    | T.AppBuiltin _
    | T.Var _
    | T.Const _
    | T.Fun _
    | T.App _
    | T.DB _ -> singleton ~num num.one t
  in
  of_term t

(* to normalize, convert every coefficient to a monome and sum everything
   together *)
let normalize (type a) (m:a t): a t =
  let acc = mk_const ~num:m.num m.const in
  List.fold_left
    (fun acc (c,t) ->
       (* flatten this term into a full monome *)
       try
         let m = of_term_exn m.num t in
         sum acc (product m c)
       with NotLinear ->
         add acc c t)
    acc m.terms

module Int = struct
  let num = z
  type t = Z.t monome

  let const = mk_const ~num
  let singleton = singleton ~num
  let of_list = of_list ~num

  let of_term_exn t = of_term_exn num t

  let of_term t =
    try Some (of_term_exn t)
    with NotLinear -> None

  let mk_const n = T.builtin ~ty:num.ty (Builtin.mk_int n)

  (* a.t *)
  let mk_product a t =
    if num.equal a num.one then t
    else
      T.app_builtin Builtin.Product ~ty:num.ty [T.of_ty num.ty; mk_const a; t]

  (* a.t + b *)
  let mk_sum a t b =
    if num.equal a num.zero then b
    else
      T.app_builtin Builtin.Sum ~ty:num.ty [T.of_ty num.ty; mk_product a t; b]

  (* a.1 + b *)
  let mk_sum_const a b =
    if num.equal a num.zero then b
    else
      T.app_builtin Builtin.Sum ~ty:num.ty [T.of_ty num.ty; mk_const a; b]

  let to_term e =
    let t = match e.terms with
      | [] -> mk_const e.const
      | (c, t)::rest ->
        (* remove one coeff to make the basic sum *)
        let sum = mk_product c t in
        (* add coeff*term for the remaining terms *)
        let sum = List.fold_left
            (fun sum (coeff, _t') ->
               assert (num.sign coeff <> 0);
               mk_sum coeff t sum
            ) sum rest
        in
        (* add the constant (if needed) *)
        mk_sum_const e.const sum
    in
    t

  let normalize_wrt_zero m =
    if is_const m
    then m
    else
      (* divide by common gcd of coeffs and constant *)
      let gcd = if Z.(equal m.const zero) then Z.one else m.const in
      let gcd = List.fold_left (fun gcd (c,_) -> Z.gcd c gcd) gcd m.terms in
      let gcd = Z.abs gcd in
      if Z.equal Z.one gcd
      then m
      else _fmap (fun c -> Z.div c gcd) m

  let pp_z out n = CCFormat.string out (Z.to_string n)

  (* manage so that m1[t] = m2[t] *)
  let reduce_same_factor m1 m2 t =
    try
      let n1 = find_exn m1 t in
      let n2 = find_exn m2 t in
      let gcd = Z.gcd n1 n2 in
      assert (Z.sign n1 > 0);
      assert (Z.sign n2 > 0);
      assert (Z.sign gcd > 0);
      (* n1 × n2 = gcd × lcm, so we need to raise both n1 and n2 to lcm.
         to do that, let us introduce  n1 = gcd × d1, and n2 = gcd × d2.
         Then
            n1 × d2 = gcd × d1 × d2, and
            n2 × d1 = gcd × d2 × d1
         so we multiply m1 by d2, and m2 by d1.
      *)
      let d1 = Z.div n1 gcd in
      let d2 = Z.div n2 gcd in
      Util.debugf 5 "@[reduce same factor:@ %a, %a have gcd %a,@ mult by %a, %a@]"
        (fun k->k pp m1 pp m2 pp_z gcd pp_z d2 pp_z d1);
      product m1 d2, product m2 d1
    with Not_found ->
      raise (Invalid_argument "Monome.reduce_same_factor")

  let to_multiset m =
    Seq.coeffs_swap m |> Multisets.MT.Seq.of_coeffs Multisets.MT.empty

  (* multiset-like comparison *)
  let compare f m1 m2 =
    let m1 = to_multiset m1 and m2 = to_multiset m2 in
    Multisets.MT.compare_partial f m1 m2

  (** {2 Specific to Int} *)

  let has_instances m =
    let res = match m.terms with
      | [] -> Z.sign m.const = 0
      | (g,_) :: l ->
        let g = List.fold_left (fun g (c,_) -> Z.gcd c g) g l in
        Z.sign (Z.rem m.const g) = 0
    in
    Util.debugf 5 "@[monome @[%a@]@ has instances: %B@]" (fun k->k pp m res);
    res

  let quotient e c =
    if Z.sign c <= 0
    then None
    else try Some
               (_fmap
                  (fun s -> if Z.(equal (erem s c) zero) then Z.divexact s c else raise Exit)
                  e)
      with Exit -> None

  let divisible e c =
    Z.sign (Z.rem e.const c) = 0
    &&
    List.for_all (fun (c',_) -> Z.sign (Z.rem c' c) = 0) e.terms

  let factorize e =
    let gcd =
      if Z.equal e.const Z.zero
      then match e.terms with
        | [] -> Z.one
        | (c,_)::terms' ->
          List.fold_left
            (fun gcd (c, _) -> Z.gcd c gcd)
            c terms'
      else
        List.fold_left
          (fun gcd (c, _) -> Z.gcd c gcd)
          e.const e.terms
    in
    let gcd = Z.abs gcd in
    if Z.equal Z.one gcd || Z.sign gcd = 0
    then None
    else match quotient e gcd with
      | None -> assert false
      | Some e' -> Some (e', gcd)

  (** {2 Modular Computations} *)

  module Modulo = struct
    let modulo ~n c = Z.rem c n

    let sum ~n c1 c2 = modulo ~n (Z.add c1 c2)

    let uminus ~n c = modulo ~n (Z.mul Z.minus_one c)
  end

  (** {2 Find Solutions} *)

  module Solve = struct
    type solution = (Term.t * t) list
    (** List of constraints (term = monome). It means that
        if all those constraints are satisfied, then a solution
        to the given problem has been found *)

    let split_solution s =
      let vars, nonvars = List.partition (fun (t, _) -> T.is_var t) s in
      let vars = List.map
          (fun (t,m) -> match T.view t with
             | T.Var v -> v, m
             | _ -> assert false)
          vars
      in
      let subst =
        List.fold_left
          (fun subst (v, m) ->
             let v = (v : T.var :> InnerTerm.t HVar.t) in
             Subst.FO.bind subst (Scoped.make v 0) (Scoped.make (to_term m) 0))
          Subst.empty vars
      in
      subst, nonvars

    (** Solving diophantine equations: see
        http://mathworld.wolfram.com/DiophantineEquation.html
        for the solution for 2 variables *)

    (* solve the diophantine equation [a * x + b * y = const] *)
    let diophant2 a b const =
      (* Euclid's algorithm, enriched to find the pair of Bezout integers
          [u,v] such that [a*u + b*v = g].
          Here we find a list of pairs that have the same GCD as [a, b],
          the last element of which is [_, 1]; In addition we also keep
          the quotients.
          We assume that a and b are > 0 and that a >= b. *)
      let solve a b _const =
        let rec recurse a b acc =
          let q, r = Z.div_rem a b in
          if Z.equal r Z.zero
          then (a,b,q) :: acc  (* done *)
          else
            recurse b r ((a,b,q) :: acc)
        in
        (* a list of pairs with the same GCD as [a, b], and their
            quotients *)
        assert (Z.geq a b);
        let u, v = match recurse a b [] with
          | [] -> assert false
          | (_a,b,_) :: l ->
            let u, v = Z.zero, b in
            List.fold_left
              (fun (u, v) (_a, _b, q) ->
                 let u' = Z.sub u (Z.mul v q) in
                 v, u')
              (u, v) l
        in
        u, v
      in
      let sign1 = Z.sign a > 0 in
      let sign2 = Z.sign b > 0 in
      let sign_const = Z.sign const >= 0 in
      (* use positive integers *)
      let a, b = Z.abs a, Z.abs b in
      let const = Z.abs const in
      (* [a] must be bigger *)
      let swap = Z.gt b a in
      let a, b = if swap then b, a else a, b in
      let u, v, gcd =
        let gcd = Z.gcd a b in
        let q, r = Z.div_rem const gcd in
        if Z.sign r <> 0
        then
          failwith
            (CCFormat.sprintf "unsolvable diophantine equation %a x + %a y = %a"
               pp_z a pp_z b pp_z const)
        else
          let a' = Z.div a gcd in
          let b' = Z.div b gcd in
          (* solve for coprime numbers *)
          let u, v = solve a' b' Z.one in
          Z.mul u q, Z.mul v q, gcd
      in
      let u, v = if swap then v, u else u, v in
      (* put sign back *)
      let u = if sign1 <> sign_const then Z.neg u else u in
      let v = if sign2 <> sign_const then Z.neg v else v in
      (* return solution *)
      u, v, gcd

    (* solve equation [l1 * x1 + l2 * x2 + .. + ln * xn = const *)
    let rec diophant_l l const = match l with
      | []
      | [_] -> failwith "diophant_l: expect at least 2 coefficients"
      | [a; b] ->
        let u, v, gcd = diophant2 a b const in
        [u; v], gcd
      | a1 :: a2 :: l' ->
        let gcd_1_2 = Z.gcd a1 a2 in
        let u1, u2, _ = diophant2 a1 a2 gcd_1_2 in
        (* first, solve [a1 * u1 + a2 * u2 = gcd_1_2]. We then
            find u1_2, u' such that  [gcd_1_2 * u1_2 + u' * l' = const],
            after which [a1 * u1 * u1_2 + a2 * u1_2 * u2 + u' * l' = const]
            and we're done. *)
        begin match diophant_l (gcd_1_2 :: l') const with
          | [], _ -> assert false
          | (u_1_2 :: u'), gcd ->
            let u1' = Z.mul u1 u_1_2 in
            let u2' = Z.mul u2 u_1_2 in
            u1' :: u2' :: u', gcd
        end

    (* least common multiple of a and b *)
    let _lcm a b =
      (* a * b = gcd * lcm *)
      let gcd = Z.gcd a b in
      Z.div (Z.abs (Z.mul a b)) gcd

    (* find solutions that equate zero *)
    let coeffs_n l _gcd =
      let n = List.length l in
      if n < 2 then failwith "coeffs_n: expected list of at least 2 elements";
      (* array, for faster lookup of coefficient i *)
      let a = Array.of_list l in
      fun k ->
        assert (List.length k + 1 = List.length l);
        (* let's build a linear combination of the variables that are going to
            be provided. for this, we build smaller linear combinations
           {[x1 = lcm(1,2)/l1 k1
                 ...
                   xi = -lcm(i-1,i)/li k(i-1) + lcm(i,i+1)/li ki
                   ...
                     xn = -lcm(n-1,n)/ln k(n-1)
           ]}
            where lcm(i,j) = lcm(li, lj).
            This linear combination is of dimension n-1, and is always solution
            of [sum_i (li * xi) = 0].
        *)
        let k = Array.of_list k in
        List.mapi
          (fun i _li ->
             if i = 0
             then
               (* lcm(0,1) / l0 * k0 *)
               let lcm12 = _lcm a.(0) a.(1) in
               let coeff = Z.div lcm12 a.(0) in
               singleton coeff k.(0)
             else if i = n-1
             then
               (* -lcm(n-1,n-2) / l(n-1) * k(n-2) *)
               let lcm_last = _lcm a.(n-1) a.(n-2) in
               let coeff = Z.neg (Z.div lcm_last a.(n-1)) in
               singleton coeff k.(n-2)
             else
               (* general case: -lcm(i-1,i)/li * k(i-1) + lcm(i,i+1)/li * ki *)
               let lcm_prev = _lcm a.(i-1) a.(i) in
               let c_prev = Z.neg (Z.div lcm_prev a.(i)) in
               let lcm_i = _lcm a.(i) a.(i+1) in
               let c_i = Z.div lcm_i a.(i) in
               sum
                 (singleton c_prev k.(i-1))
                 (singleton c_i k.(i))
          )
          l

    (* default generator of fresh variables *)
    let __fresh_var m =
      let count = ref (T.Seq.max_var (Seq.vars m) + 1) in
      fun _ty ->
        let n = !count in
        incr count;
        T.var_of_int ~ty:num.ty n

    (* is the constant +/- 1? *)
    let _is_one_abs (s, _) = Z.equal Z.one (Z.abs s)

    let eq_zero ?fresh_var m =
      (* generation of fresh variables, with default function *)
      let fresh_var = match fresh_var with
        | None -> __fresh_var m
        | Some f -> f
      in
      if is_const m
      then []
      else
        let m = normalize_wrt_zero m in
        (* need to solve a diophantine equation *)
        let terms = to_list m in
        begin match terms with
          | [] when Z.sign m.const = 0 -> [[]]  (* trivial *)
          | [c, t] when Z.sign (Z.rem m.const c) = 0 ->
            (* [c * x + constant = 0], let [x = - constant / c] *)
            let n = Z.div (Z.neg m.const) c in
            [ [t, const n] ]
          | _::_::_ as l when List.exists _is_one_abs l ->
            (* at leat one of the coefficients is +/- 1. Extract
                the corresponding terms *)
            let unit_terms = List.filter _is_one_abs l in
            List.map
              (fun (c, t) ->
                 let m' = remove m t in
                 (* t = -m' if the coefficient of t was 1, m' otherwise *)
                 let m' = if Z.sign c > 0 then uminus m' else m' in
                 [ t, m' ])
              unit_terms
          | _::_::_ as l ->
            (* extract coefficients *)
            let l' = List.map fst l in
            let c = m.const in
            begin try
                let gcd = List.fold_left Z.gcd (List.hd l') (List.tl l') in
                (* coefficients for the solution hyperplane *)
                let coeffs = coeffs_n l' gcd in
                (* initial solution *)
                let init, _gcd = diophant_l l' (Z.neg c) in
                (* generate fresh vars to describe the solution space *)
                let n = List.length l in
                let vars = Sequence.(repeat () |> take (n-1) |> to_rev_list) in
                let vars = List.map (fun () -> fresh_var num.ty) vars in
                (* build general solution by summing variable part and initial solution *)
                let monomes = List.map2
                    (fun var_part const_part -> sum var_part (const const_part))
                    (coeffs vars)
                    init
                in
                [ List.combine (List.map snd l) monomes ]
              with Failure _ -> []
            end
          | _ ->  []  (* cannot do much otherwise *)
        end

    let lower_zero ?fresh_var ~strict m =
      if is_const m
      then []
      else
        let m = normalize_wrt_zero m in
        begin match m.terms with
          | [] -> []
          | [c, t] when Z.sign (Z.rem c m.const) = 0 ->
            (* c * t + m < 0 ----> t = (-m / c) - 1 *)
            let v = Z.div (Z.neg m.const) c in
            let v = if Z.sign c > 0
              then Z.pred v
              else Z.succ v
            in
            [ [t, const v] ]
          | [c, t] ->
            (* must be integer, take the quotient itself *)
            let v = Z.div (Z.neg m.const) c in
            let v = if Z.sign c < 0 then Z.succ v else v in
            [ [t, const v] ]
          | _::_::_ when List.exists _is_one_abs m.terms ->
            if strict
            then
              (* there is some coefficient equal to one, just extract the
                 corresponding terms and make them equal to monome + 1 *)
              let terms = List.filter _is_one_abs m.terms in
              List.map
                (fun (c,t) ->
                   let m' = remove m t in
                   let m' = if Z.sign c > 0
                     then pred (uminus m') (* t + m < 0 ---> t = -m - 1 *)
                     else succ m'  (* -t + m < 0 ---> t = m + 1 *)
                   in
                   [ t, m' ]
                )
                terms
            else
              (* equality is ok, and here we know there are always solutions *)
              eq_zero ?fresh_var m
          | _::_::_ ->
            (* the idea: to find instances of m <= 0, we find the smallest positive n
               such that m = n is solvable, then we call {!eq_zero}. *)
            let gcd = List.fold_left
                (fun gcd (c,_) -> Z.gcd gcd c)
                m.const m.terms
            in
            (* now we shift the constant until it is a multiple of the gcd.
               m < const  ----> m = const' with const' < const *)
            let c = Z.neg m.const in
            let q, r = Z.div_rem c gcd in
            let c' = if Z.sign r = 0
              then if strict
                then (* already a multiple of gcd. take the previous one, gcd * (q-1) *)
                  Z.mul (Z.pred q) gcd
                else (* equality has solutions *)
                  c
              else (* gcd * q < gcd * q + r, ok for both strict and non-strict *)
                Z.mul q gcd
            in
            let c' = Z.neg c' in
            let m' = { m with const = c'; } in
            eq_zero ?fresh_var m'
        end

    let lt_zero ?fresh_var m =
      lower_zero ?fresh_var ~strict:true m

    let leq_zero ?fresh_var m =
      lower_zero ?fresh_var ~strict:false m

    let neq_zero ?fresh_var m =
      lt_zero ?fresh_var m
  end
end

module Rat = struct
  let num = q
  type t = Q.t monome

  let const = mk_const ~num
  let singleton = singleton ~num
  let of_list = of_list ~num

  let of_term_exn t = of_term_exn num t

  let of_term t =
    try Some (of_term_exn t)
    with NotLinear -> None

  let mk_const n = T.builtin ~ty:num.ty (Builtin.mk_rat n)

  let divide m q: t = product m (Q.inv q)

  (* a.t *)
  let mk_product a t =
    if num.equal a num.one then t
    else
      T.app_builtin Builtin.Product ~ty:num.ty
        [T.of_ty num.ty; mk_const a; t]

  (* a.t + b *)
  let mk_sum a t b =
    if num.equal a num.zero then b
    else
      T.app_builtin Builtin.Sum ~ty:num.ty [T.of_ty num.ty; mk_product a t; b]

  (* a.1 + b *)
  let mk_sum_const a b =
    if num.equal a num.zero then b
    else
      T.app_builtin Builtin.Sum ~ty:num.ty [T.of_ty num.ty; mk_const a; b]

  let to_term e =
    let t = match e.terms with
      | [] -> mk_const e.const
      | (c, t)::rest ->
        (* remove one coeff to make the basic sum *)
        let sum = mk_product c t in
        (* add coeff*term for the remaining terms *)
        let sum = List.fold_left
            (fun sum (coeff, _t') ->
               assert (num.sign coeff <> 0);
               mk_sum coeff t sum
            ) sum rest
        in
        (* add the constant (if needed) *)
        mk_sum_const e.const sum
    in
    t

  let to_multiset m =
    Seq.terms m |> Multisets.MT.Seq.of_seq Multisets.MT.empty
end
