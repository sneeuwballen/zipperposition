
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
  | Different
  | Less
  | Lesseq

type 'm divides = {
  num : Z.t;
  power : int;
  monome : 'm;
  sign : bool;
} (** [num^power divides monome] or not. *)

type t =
  | Binary of op * Z.t Monome.t * Z.t Monome.t
  | Divides of Z.t Monome.t divides
  (** Arithmetic literal (on integers) *)

type lit = t

(** {2 Basics} *)

let equal lit1 lit2 = match lit1, lit2 with
  | Binary (op1, x1, y1), Binary (op2, x2, y2) ->
    op1 = op2 && M.equal x1 x2 && M.equal y1 y2
  | Divides d1, Divides d2 ->
    d1.sign = d2.sign && d1.power = d2.power &&
    Z.equal d1.num d2.num && M.equal d1.monome d2.monome
  | _, _ -> false

let equal_com lit1 lit2 = match lit1, lit2 with
  | Binary (op1, x1, y1), Binary (op2, x2, y2)
    when op1 = op2 && (op1 = Equal || op1 = Different) ->
    (M.equal x1 x2 && M.equal y1 y2) || (M.equal x1 y2 && M.equal x2 y1)
  | _ -> equal lit1 lit2

let compare lit1 lit2 = match lit1, lit2 with
  | Binary (op1, x1, y1), Binary (op2, x2, y2) ->
    let c = compare op1 op2 in
    if c <> 0 then c
    else let c = M.compare x1 x2 in
      if c <> 0 then c else M.compare y1 y2
  | Divides d1, Divides d2 ->
    let open CCOrd in
    compare d1.sign d2.sign
    <?> (compare, d1.power, d2.power)
    <?> (Z.compare, d1.num, d2.num)
    <?> (M.compare, d1.monome, d2.monome)
  | Binary _,  Divides _ -> 1
  | Divides _, Binary _ -> -1

let hash lit = match lit with
  | Binary (op, m1, m2) ->
    Hash.combine4 10 (Hash.poly op) (M.hash m1) (M.hash m2)
  | Divides d ->
    Hash.combine5 20
      (Hash.bool d.sign) (Z.hash d.num) (M.hash d.monome) (Hash.int d.power)

let sign = function
  | Binary ((Equal | Lesseq | Less), _, _) -> true
  | Binary (Different, _, _) -> false
  | Divides d -> d.sign

(* polarity used for the literal ordering *)
let polarity = function
  | Binary ((Less | Lesseq), _, _) -> false
  | Binary (Different, _, _) -> false
  | Binary (Equal,_,_) -> true
  | Divides d -> d.sign

let is_pos = sign
let is_neg l = not (is_pos l)

let _is_bin p = function
  | Binary (op, _, _) -> p op
  | Divides _ -> false

let is_eq = _is_bin ((=) Equal)
let is_neq = _is_bin ((=) Different)
let is_eqn = _is_bin (function Equal | Different -> true | _ -> false)
let is_less = _is_bin ((=) Less)
let is_lesseq = _is_bin ((=) Lesseq)
let is_ineq = _is_bin (function Less | Lesseq -> true | _ -> false)
let is_divides = function
  | Divides _ -> true
  | Binary _ -> false

let make_no_simp op m1 m2 = Binary (op, m1, m2)

(* main constructor *)
let make op m1 m2 =
  let m1, m2 = M.normalize m1, M.normalize m2 in
  let m = M.difference m1 m2 in
  (* build from a single monome *)
  let _make_split op m =
    let m1, m2 = M.split m in
    make_no_simp op m1 m2
  in
  match op with
    | Equal
    | Different ->
      (* divide by gcd *)
      let m = M.Int.normalize_wrt_zero m in
      _make_split op m
    | Less ->
      (* should be removed *)
      _make_split op m
    | Lesseq ->
      let c = M.const m in
      let m' = M.remove_const m in
      begin match Monome.Int.factorize m' with
        | Some (m'', g) when Z.gt g Z.one ->
          if Z.sign c > 0
          then
            (* a constant occurs in m1, so m1' + k ≤ m2. In this
               case we check whether m2 - m1' can be factored by some d,
               and we replace k with ceil (k/d).
               Example: 3 ≤ 2a  ----> 3/2 ≤ a ---> 2 ≤ a  *)
            let c' = Z.cdiv c g in
            _make_split op (M.add_const m'' c')
          else if Z.equal c Z.zero
          then
            (* no constant, just divide by gcd *)
            _make_split op m''
          else
            (* m1 ≤ k + m2'. If g is the gcd of m1 and m2' then
                we replace k with floor(k/g) *)
            let c' = Z.neg (Z.fdiv (Z.abs c) g) in
            _make_split op (M.add_const m'' c')
        | _ ->
          (* no gcd other than 1 *)
          _make_split op m
      end

let mk_eq = make Equal
let mk_neq = make Different
let mk_less = make Less
let mk_lesseq = make Lesseq

module U = struct
  module ZTbl = Hashtbl.Make(Z)

  type divisor = {
    prime : Z.t;
    power : int;
  }

  let two = Z.of_int 2

  (* table from numbers to some of their divisor (if any) *)
  let _table = lazy (
    let t = ZTbl.create 256 in
    ZTbl.add t two None;
    t)

  let _divisors n = ZTbl.find (Lazy.force _table) n

  let _add_prime n =
    ZTbl.replace (Lazy.force _table) n None

  (* add to the table the fact that [d] is a divisor of [n] *)
  let _add_divisor n d =
    assert (not (ZTbl.mem (Lazy.force _table) n));
    ZTbl.add (Lazy.force _table) n (Some d)

  (* primality test, modifies _table *)
  let _is_prime n0 =
    let n = ref two in
    let bound = Z.succ (Z.sqrt n0) in
    let is_prime = ref true in
    while !is_prime && Z.leq !n bound do
      if Z.sign (Z.rem n0 !n) = 0
      then begin
        is_prime := false;
        _add_divisor n0 !n;
      end;
      n := Z.succ !n;
    done;
    if !is_prime then _add_prime n0;
    !is_prime

  let is_prime n =
    try
      begin match _divisors n with
        | None -> true
        | Some _ -> false
      end
    with Not_found ->
    match Z.probab_prime n 7 with
      | 0 -> false
      | 2 -> (_add_prime n; true)
      | 1 ->
        _is_prime n
      | _ -> assert false

  let rec _merge l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | p1::l1', p2::l2' ->
      match Z.compare p1.prime p2.prime with
        | 0 ->
          {prime=p1.prime; power=p1.power+p2.power} :: _merge l1' l2'
        | n when n < 0 ->
          p1 :: _merge l1' l2
        | _ -> p2 :: _merge l1 l2'

  let rec _decompose n =
    try
      begin match _divisors n with
        | None -> [{prime=n; power=1;}]
        | Some q1 ->
          let q2 = Z.divexact n q1 in
          _merge (_decompose q1) (_decompose q2)
      end
    with Not_found ->
      ignore (_is_prime n);
      _decompose n

  let prime_decomposition n =
    if is_prime n
    then [{prime=n; power=1;}]
    else _decompose n

  let primes_leq n0 k =
    let n = ref two in
    while Z.leq !n n0 do
      if is_prime !n then k !n
    done
end

(* reduce 8^1 to 2^3 *)
let _normalize_n n power =
  let divisors = U.prime_decomposition (Z.pow n power) in
  match divisors with
    | [] -> assert false
    | [d] -> d.U.prime, d.U.power
    | _::_::_ -> n, power

(* normalize coefficients of [m] in Z/n^power Z *)
let _normalize_in_div n ~power m =
  let nk = Z.pow n power in
  (* normalize coefficients so that they are within [0...nk-1] *)
  let norm_coeff c = Z.erem c nk in
  M.map_num norm_coeff m

let mk_divides ?(sign=true) n ~power m =
  let m = Monome.normalize m in
  let n, power = _normalize_n n power in
  let m = _normalize_in_div n ~power m in
  (* factorize m by some k; if k is n^p, then make the literal
     n^{power-p} | m/k *)
  let rec factor m power =
    if power <= 1 then m,power
    else match M.Int.quotient m n with
      | None -> m, power
      | Some m' ->  factor m' (power-1)
  in
  let m, power = factor m power in
  Divides { sign; num=n; power; monome=m; }

let mk_not_divides = mk_divides ~sign:false

let negate = function
  | Binary (op, m1, m2) ->
    begin match op with
      | Equal -> Binary (Different, m1, m2)
      | Different -> Binary (Equal, m1, m2)
      | Less -> make Lesseq m2 m1 (* a<b --> b≤a *)
      | Lesseq -> make Lesseq (M.succ m2) m1 (* a≤b --> b<a --> b+1≤a *)
    end
  | Divides d -> Divides { d with sign=not d.sign; }

let pp out = function
  | Binary (op, l, r) ->
    Format.fprintf out "@[%a %s@ %a@]"
      M.pp l
      (match op with Equal -> "=" | Different -> "≠"
                   | Less -> "<" | Lesseq -> "≤")
      M.pp r
  | Divides d when d.sign ->
    let nk = Z.pow d.num d.power in
    Format.fprintf out "@[<2>%s div@ %a@]" (Z.to_string nk) M.pp d.monome
  | Divides d ->
    let nk = Z.pow d.num d.power in
    Format.fprintf out "@<1>¬(%s div %a)" (Z.to_string nk) M.pp d.monome

let pp_tstp out = function
  | Binary (Equal, l, r) ->
    Format.fprintf out "%a = %a" M.pp_tstp l M.pp_tstp r
  | Binary (Different, l, r) ->
    Format.fprintf out "%a != %a" M.pp_tstp l M.pp_tstp r
  | Binary (Less, l, r) ->
    Format.fprintf out "$less(%a, %a)" M.pp_tstp l M.pp_tstp r
  | Binary (Lesseq, l, r) ->
    Format.fprintf out "$lesseq(%a, %a)" M.pp_tstp l M.pp_tstp r
  | Divides d when d.sign ->
    let nk = Z.pow d.num d.power in
    Format.fprintf out "$remainder_e(%a, %s) = 0" M.pp_tstp d.monome (Z.to_string nk)
  | Divides d ->
    let nk = Z.pow d.num d.power in
    Format.fprintf out "$remainder_e(%a, %s) != 0" M.pp_tstp d.monome (Z.to_string nk)

let pp_zf out = function
  | Binary (Equal, l, r) ->
    Format.fprintf out "%a = %a" M.pp_zf l M.pp_zf r
  | Binary (Different, l, r) ->
    Format.fprintf out "%a != %a" M.pp_zf l M.pp_zf r
  | Binary (Less, l, r) ->
    Format.fprintf out "(%a < %a)" M.pp_zf l M.pp_zf r
  | Binary (Lesseq, l, r) ->
    Format.fprintf out "(%a <= %a)" M.pp_zf l M.pp_zf r
  | Divides d when d.sign ->
    let nk = Z.pow d.num d.power in
    Format.fprintf out "(%a mod %s) = 0" M.pp_zf d.monome (Z.to_string nk)
  | Divides d ->
    let nk = Z.pow d.num d.power in
    Format.fprintf out "(%a mod %s) != 0" M.pp_zf d.monome (Z.to_string nk)

let to_string = CCFormat.to_string pp_tstp

(** {2 Operators} *)

let map f = function
  | Binary (op, m1, m2) -> make op (M.map f m1) (M.map f m2)
  | Divides d -> mk_divides ~sign:d.sign d.num ~power:d.power (M.map f d.monome)

let fold f acc = function
  | Binary (_, m1, m2) ->
    let acc = Sequence.fold f acc (Monome.Seq.terms m1) in
    Sequence.fold f acc (Monome.Seq.terms m2)
  | Divides d ->
    Sequence.fold f acc (Monome.Seq.terms d.monome)

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
  match lit1, lit2 with
    | Binary (((Equal | Different) as op1), x1, y1),
      Binary (((Equal | Different) as op2), x2, y2) when op1 = op2 ->
      (* try both ways *)
      unif4 m_unif ~subst x1 y1 sc1 x2 y2 sc2 k
    | Binary (op1, x1, y1), Binary (op2, x2, y2) ->
      if op1 = op2
      then m_unif ~subst (x1,sc1) (x2,sc2)
          (fun subst -> m_unif ~subst (y1,sc1) (y2,sc2) k)
    | Divides d1, Divides d2 ->
      if Z.equal d1.num d2.num && d1.power = d2.power && d1.sign = d2.sign
      then m_unif ~subst (d1.monome,sc1) (d2.monome,sc2) k
    | Binary _, Divides _
    | Divides _, Binary _ -> ()

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
        (fun k->k M.pp m1 sc1 (Z.to_string c1) M.pp m2 sc2 (Z.to_string c2) S.pp subst);
      false
    )

  (* match lists together exactly. No scaling authorized *)
  let rec match_lists ~protect ~subst l1 rest1 sc1 l2 sc2 k = match l1, l2 with
    | [], [] ->
      if rest1 = []
      then k subst
    | _, []
    | [], _ -> ()
    | (c1,t1)::l1', (c2,t2)::l2' when Z.leq c1 c2  ->
      begin try
          let subst =
            Unif.FO.matching_adapt_scope
              ~protect ~subst ~pattern:(Scoped.make t1 sc1) (Scoped.make t2 sc2)
          in
          if Z.equal c1 c2
          then match_lists ~protect ~subst
              (rest1 @ l1') [] sc1 l2' sc2 k
          else match_lists ~protect ~subst
              l1' rest1 sc1 ((Z.(c2 - c1),t2)::l2') sc2 k
        with Unif.Fail -> ()
      end;
      (* ignore [t1] for now *)
      match_lists ~protect ~subst l1' ((c1,t1)::rest1) sc1 l2 sc2 k
    | (c1,t1)::l1', (c2,_t2)::_l2' ->
      (* cannot match, c1 too high *)
      assert Z.(gt c1 zero);
      assert Z.(gt c2 zero);
      match_lists ~protect ~subst l1' ((c1,t1)::rest1) sc1 l2 sc2 k

  (* matching that is allowed to scale m1, and, if [scale2] is true, to
     scale [m2] too. Constants are not taken into account.
     [k] is called with [subst, c1, c2] where [c_i] is the scaling coefficient
     for [m_i].
     @param [scale2] allow to multiply m2 with a (positive) constant
     postcondition: for a result (subst,c1,c2),
      c1 * subst(m1.terms) = c2 * subst(m2.terms) *)
  let matching ~protect ~subst m1 sc1 ~scale2 m2 sc2 k =
    (* match some terms of [l1] with the given term [t2].
       [c1] is the accumulated coefficient for terms of [l1] so far. *)
    let rec init_with_coeff ~subst c1 l1 rest1 t2 c2 rest2 =
      match l1 with
        | [] when Z.(equal c1 zero) ->
          () (* no match with [t2]: no term matches [t2] *)
        | [] ->
          (* ok, we did match some terms with [t2]. Scale coefficients if
              possible, possibly changing their sign, otherwise fail  *)
          assert Z.(gt c1 zero);
          assert Z.(gt c2 zero);
          if scale2
          then
            (* can scale both, so we take the [gcd], and multiply
               the first monome with [c2/gcd] and the second with [c1/gcd] *)
            let g = Z.(gcd c1 c2) in
            check_other_terms ~subst
              ~scale1:(Z.divexact c2 g)
              ~scale2:(Z.divexact c1 g)
              rest1 rest2
          else
            (* can only scale [c1], so it only works if [c1] divides [c2] *)
          if Z.(equal (c2 mod c1) zero)
          then
            check_other_terms ~subst
              ~scale1:(Z.divexact c2 c1)
              ~scale2:Z.one
              rest1 rest2
          else ()
        | (c1',t1) :: l1' ->
          (* choose [t1], if possible, and then extend the substitution for t2 *)
          begin try
              let subst = Unif.FO.matching_adapt_scope ~protect ~subst
                  ~pattern:(Scoped.make t1 sc1) (Scoped.make t2 sc2) in
              init_with_coeff ~subst Z.(c1 + c1') l1' rest1 t2 c2 rest2
            with Unif.Fail -> ()
          end;
          (* disregard [t1] *)
          init_with_coeff ~subst c1 l1' ((c1',t1)::rest1) t2 c2 rest2
    and
      (* match other terms with the given scaling coeff *)
      check_other_terms ~subst ~scale1 ~scale2 l1 l2 =
      let l1 = List.map (fun (c,t) -> Z.(c * scale1), t) l1
      and l2 = List.map (fun (c,t) -> Z.(c * scale2), t) l2
      in
      match_lists ~protect ~subst l1 [] sc1 l2 sc2
        (fun subst ->
           assert (_matching_postcond (Scoped.make m1 sc1) (Scoped.make m2 sc2) (subst, scale1, scale2));
           k (subst, scale1, scale2))
    in
    match M.coeffs m2 with
      | [] ->
        begin match M.coeffs m1 with
          | [] -> k (subst,Z.one,Z.one)
          | _::_ -> ()  (* fail *)
        end
      | (c2,t2)::l2 ->
        (* start with matching terms of [m1] with [t2] *)
        init_with_coeff ~subst Z.zero (M.coeffs m1) [] t2 c2 l2

  (* match l1 with l2, and r1 with r2, with the same scaling coefficient *)
  let matching2 ~subst l1 r1 sc1 ~scale2 l2 r2 sc2 k =
    let protect = Sequence.append (M.Seq.vars l2) (M.Seq.vars r2) in
    if M.is_const l1 && M.is_const l2
    then
      (* only one problem *)
      matching ~protect ~subst r1 sc1 ~scale2 r2 sc2 k
    else
      matching ~protect ~subst l1 sc1 ~scale2 l2 sc2
        (fun (subst,c1,c2) ->
           let r1 = List.map (fun (c,t) -> Z.(c * c1), t) (M.coeffs r1)
           and r2 = List.map (fun (c,t) -> Z.(c * c2), t) (M.coeffs r2) in
           match_lists ~protect ~subst r1 [] sc1 r2 sc2
             (fun subst -> k (subst,c1,c2)))

  let check ~subst lit1 sc1 lit2 sc2 k =
    match lit1, lit2 with
      | Binary (Equal, l1, r1), Binary (Equal, l2, r2)
      | Binary (Different, l1, r1), Binary (Different, l2, r2) ->
        (* careful with equality, don't scale right literal because
           it might involve divisibility issues *)
        matching2 ~subst l1 r1 sc1 ~scale2:false l2 r2 sc2
          (fun (subst, c1, c2) ->
             if Z.(equal
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const r2 - M.const l2)))
             then k subst);
        matching2 ~subst l1 r1 sc1 ~scale2:false r2 l2 sc2
          (fun (subst, c1, c2) ->
             if Z.(equal
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const l2 - M.const r2)))
             then k subst)
      | Binary (Equal, l1, r1), Binary (Lesseq, l2, r2) ->
        (* l1=r1  can subsume l2≤r2 if
           r1.const-l1.const = subst(l1-r1) = l2-r2 ≤ r2.const - l2.const
           with r1.const-l1.const ≤ r2.const - l2.const (tighter bound) *)
        matching2 ~subst l1 r1 sc1 ~scale2:true l2 r2 sc2
          (fun (subst, c1, c2) ->
             if Z.(leq
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const r2 - M.const l2)))
             then k subst)
      | Binary (Lesseq, l1, r1), Binary (Different, l2, r2) ->
        (* l1≤r1 can subsume l2 != r2 if
           l1-r1 ≤ r1.const-l1.const and l2-r2 = r2.const-l2.const
           with subst(l1-r1) = l2-r2 and r1.const-l1.const < r2.const-l2.const
           (same with r2-l2 and l2.const-r2.const of course)*)
        matching2 ~subst l1 r1 sc1 ~scale2:true l2 r2 sc2
          (fun (subst, c1, c2) ->
             if Z.(lt
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const r2 - M.const l2)))
             then k subst);
        matching2 ~subst l1 r1 sc1 ~scale2:true r2 l2 sc2
          (fun (subst, c1, c2) ->
             if Z.(lt
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const l2 - M.const r2)))
             then k subst);
        (* l1≤r1 can also subsume l2!=r2 if
           r1-l1 ≥ l1.const-r1.const with
           subst(r1-l1)=l2-r2  and l1.const-r1.const > r2.const-l2.const *)
        matching2 ~subst r1 l1 sc1 ~scale2:true r2 l2 sc2
          (fun (subst, c1, c2) ->
             if Z.(gt
                   (c1 * (M.const l1 - M.const r1))
                   (c2 * (M.const l2 - M.const r2)))
             then k subst);
        matching2 ~subst r1 l1 sc1 ~scale2:true l2 r2 sc2
          (fun (subst, c1, c2) ->
             if Z.(gt
                   (c1 * (M.const l1 - M.const r1))
                   (c2 * (M.const r2 - M.const l2)))
             then k subst);
      | Binary (Lesseq, l1, r1), Binary (Lesseq, l2, r2) ->
        (* if subst(r1 - l1) = r2-l2 - k where k≥0, then l1≤r1 => l2+k≤r2 => l2≤r2
            so l1≤r1 subsumes l2≤r2. *)
        matching2 ~subst l1 r1 sc1 ~scale2:true l2 r2 sc2
          (fun (subst, c1, c2) ->
             (* we removed all terms but the constants,
                 l1-r1 = l2-r2. Now lit1= l1-r1 < r1.const-l1.const,
                 so if r1.const-l1.const < r2.const - l2.const then lit1 => lit2 *)
             if Z.(leq
                   (c1 * (M.const r1 - M.const l1))
                   (c2 * (M.const r2 - M.const l2)))
             then k subst);
        (* XXX: bad, because  2a = b will subsume 2|b but cannot participate
           in some inferences 2|b will, e.g. in
           2|b   2|b+1
           -----------
              false
           so, it removes any chance of completeness
           | Binary (Equal, l1, r1), Divides d when d.sign ->
            let m1 = _normalize_in_div d.num ~power:d.power (M.difference l1 r1) in
            let protect = M.Seq.vars d.monome in
            matching ~protect ~subst m1 sc1 ~scale2:false d.monome sc2
              (fun (subst, c1, c2) ->
                (* l1-r1 = d.monome + something.num^power *)
                if Z.(equal
                  ((c1 * M.const m1) mod (d.num ** d.power))
                  (c2 * M.const d.monome)) then k subst)
        *)
      | Divides d1, Divides d2 when d1.sign = d2.sign
                                 && Z.equal d1.num d2.num &&
                                 ((d1.sign && d1.power >= d2.power)
                                  || not d1.sign && d1.power = d2.power) ->
        (* n^{k+k'} | m1  can subsume n^k | m2
           (with k' = d1.power - d2.power) if
            c1.m1 = c2.m2 + something.n^k
           for "not divides" relation we require the same power and no scaling
           on lhs. *)
        let protect = M.Seq.vars d2.monome in
        matching ~protect ~subst d1.monome sc1 ~scale2:false d2.monome sc2
          (fun (subst, c1, c2) ->
             if Z.(equal
                   ((c1 * M.const d1.monome) mod (d2.num ** d2.power))
                   (c2 * M.const d2.monome))
             && (d1.sign || Z.equal c1 Z.one) then k subst)
      | Divides d1, Divides d2 when d1.sign && not d2.sign
                                    && Z.equal d1.num d2.num && d1.power >= d2.power ->
        (* n^{k+k'} | m1 can subsume not(n^k | m2) if
           c1.m1 = c2.m2 + k with k not divisible by n^k *)
        let protect = M.Seq.vars d2.monome in
        matching ~protect ~subst d1.monome sc1 ~scale2:false d2.monome sc2
          (fun (subst, c1, c2) ->
             if Z.(gt
                   ((c1 * M.const d1.monome - c2 * M.const d2.monome) mod (d2.num ** d2.power))
                   zero)
             then k subst)
      | _ -> () (* fail *)
end

let subsumes ?(subst=Subst.empty) (lit1,sc1) (lit2, sc2) k =
  Subsumption.check ~subst lit1 sc1 lit2 sc2 k

let are_variant lit1 lit2 =
  not (Sequence.is_empty (variant (lit1, 0)(lit2, 1)))

let apply_subst renaming subst (lit,scope) = match lit with
  | Binary (op, m1, m2) ->
    make op
      (M.apply_subst renaming subst (m1, scope))
      (M.apply_subst renaming subst (m2, scope))
  | Divides d ->
    mk_divides ~sign:d.sign d.num ~power:d.power
      (M.apply_subst renaming subst (Scoped.make d.monome scope))

let apply_subst_no_simp renaming subst (lit,sc) = match lit with
  | Binary (op, m1, m2) ->
    make_no_simp op
      (M.apply_subst_no_simp renaming subst (m1,sc))
      (M.apply_subst_no_simp renaming subst (m2,sc))
  | Divides d ->
    Divides {d with monome=M.apply_subst_no_simp renaming subst (d.monome,sc); }

let is_trivial = function
  | Divides d when d.sign && (Z.equal d.num Z.one || d.power = 0) ->
    true  (* 1 | x tauto *)
  | Divides d when d.sign ->
    M.is_const d.monome && Z.sign (Z.erem (M.const d.monome) d.num) = 0
  | Divides d ->
    M.is_const d.monome && Z.sign (Z.erem (M.const d.monome) d.num) <> 0
  | Binary (Equal, m1, m2) -> M.equal m1 m2
  | Binary (Less, m1, m2) -> M.dominates ~strict:true m2 m1
  | Binary (Lesseq, m1, m2) -> M.dominates ~strict:false m2 m1
  | Binary (Different, m1, m2) ->
    let m = M.difference m1 m2 in
    (* gcd of all the coefficients *)
    let gcd = M.coeffs m
              |> List.fold_left (fun c1 (c2,_) -> Z.gcd c1 c2) Z.one in
    (* trivial if: either it's a!=0, with a a constant, or if
       the GCD of all coefficients does not divide the constant
       (unsolvable diophantine equation) *)
    (M.is_const m && Z.sign (M.const m) <> 0) ||
    (Z.sign (Z.rem (M.const m) gcd) <> 0)

let is_absurd = function
  | Binary (Equal, m1, m2) ->
    let m = M.difference m1 m2 in
    let gcd = M.coeffs m
              |> List.fold_left (fun c1 (c2,_) -> Z.gcd c1 c2) Z.one in
    (* absurd if: either it's a=0, with a a constant, or if
       the GCD of all coefficients does not divide the constant
       (unsolvable diophantine equation) *)
    (M.is_const m && M.sign m <> 0)
    || (Z.sign (Z.rem (M.const m) gcd) <> 0)
  | Binary (Different, m1, m2) -> M.equal m1 m2
  | Binary (Less, m1, m2) ->
    let m = M.difference m1 m2 in
    M.is_const m && M.sign m >= 0
  | Binary (Lesseq, m1, m2) ->
    let m = M.difference m1 m2 in
    M.is_const m && M.sign m > 0
  | Divides d when not (d.sign) && (Z.equal d.num Z.one || d.power=0)->
    true  (* 1 not| x  is absurd *)
  | Divides d when d.sign ->
    (* n^k should divide a non-zero constant *)
    M.is_const d.monome && Z.sign (Z.rem (M.const d.monome) d.num) <> 0
  | Divides d ->
    (* n^k doesn't divide 0 is absurd *)
    M.is_const d.monome && Z.sign (Z.rem (M.const d.monome) d.num) = 0

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
  match lit with
    | Binary (_op, m1, m2) ->
      fold_monome
        (fun () i _ t -> at_term ~pos:P.(append pos (left (arg i stop))) t k)
        () m1;
      fold_monome
        (fun () i _ t -> at_term ~pos:P.(append pos (right (arg i stop))) t k)
        () m2
    | Divides d ->
      fold_monome
        (fun () i _ t -> at_term ~pos:P.(append pos (arg i stop)) t k)
        () d.monome

let _to_coeffs lit =
  match lit with
    | Binary (_, m1, m2) ->
      Sequence.append (M.Seq.coeffs_swap m1) (M.Seq.coeffs_swap m2)
    | Divides d ->
      M.Seq.coeffs_swap d.monome

let to_multiset lit =
  _to_coeffs lit
  |> Multisets.MT.Seq.of_coeffs Multisets.MT.empty

let max_terms ~ord lit =
  let m = to_multiset lit in
  Multisets.MT.max (Ordering.compare ord) m
  |> Multisets.MT.to_list
  |> List.map fst

let to_form = function
  | Binary (op, m1, m2) ->
    let t1 = M.Int.to_term m1 in
    let t2 = M.Int.to_term m2 in
    begin match op with
      | Equal -> SLiteral.eq t1 t2
      | Different -> SLiteral.neq t1 t2
      | Less ->
        let t = T.app_builtin Builtin.Less ~ty:Type.prop [T.ty t1 |> T.of_ty; t1; t2] in
        SLiteral.atom t true
      | Lesseq ->
        let t = T.app_builtin Builtin.Lesseq ~ty:Type.prop [T.ty t1 |> T.of_ty; t1; t2] in
        SLiteral.atom t true
    end
  | Divides d ->
    let nk = Z.pow d.num d.power in
    let t = M.Int.to_term d.monome in
    (* $remainder_e(t, nk) = 0 *)
    let nk = T.builtin  ~ty:Type.int (Builtin.mk_int nk) in
    let t1 = T.app_builtin ~ty:Type.prop Builtin.Remainder_e [T.of_ty (T.ty t); t; nk] in
    let z = T.builtin ~ty:Type.int (Builtin.of_int 0) in
    if d.sign then SLiteral.eq t1 z else SLiteral.neq t1 z

(** {2 Iterators} *)

module Seq = struct
  let terms lit k = match lit with
    | Binary (_, m1, m2) -> M.Seq.terms m1 k; M.Seq.terms m2 k
    | Divides d -> M.Seq.terms d.monome k

  let vars lit = terms lit |> Sequence.flat_map T.Seq.vars

  let to_multiset = _to_coeffs
end

(** {2 Focus on a Term} *)

module Focus = struct
  (** focus on a term in one of the two monomes *)
  type t =
    | Left of op * Z.t Monome.Focus.t * Z.t Monome.t
    | Right of op * Z.t Monome.t * Z.t Monome.Focus.t
    | Div of Z.t Monome.Focus.t divides

  let mk_left op mf m = Left (op, mf, m)
  let mk_right op m mf = Right (op, m, mf)
  let mk_div ?(sign=true) num ~power m =
    Div {power;num;sign;monome=m;}

  let get lit pos =
    match lit, pos with
      | Binary (op, m1, m2), P.Left (P.Arg (i, _)) ->
        Some (Left (op, M.Focus.get m1 i, m2))
      | Binary (op, m1, m2), P.Right (P.Arg (i, _)) ->
        Some (Right (op, m1, M.Focus.get m2 i))
      | Divides d, P.Arg (i, _) ->
        let d' = {
          sign=d.sign; power=d.power; num=d.num;
          monome=M.Focus.get d.monome i;
        } in
        Some (Div d')
      | _ -> None

  let get_exn lit pos = match get lit pos with
    | None ->
      invalid_arg
        (CCFormat.sprintf "wrong position %a for focused arith lit %a"
           P.pp pos pp lit)
    | Some x -> x

  let focus_term lit t =
    match lit with
      | Binary (op, m1, m2) ->
        begin match M.Focus.focus_term m1 t with
          | Some mf1 ->
            assert (not (M.mem m2 t));
            Some (Left (op, mf1, m2))
          | None ->
            match M.Focus.focus_term m2 t with
              | None -> None
              | Some mf2 -> Some (Right (op, m1, mf2))
        end
      | Divides d ->
        begin match M.Focus.focus_term d.monome t with
          | None -> None
          | Some mf ->
            Some (Div {d with monome=mf; })
        end

  let focus_term_exn lit t = match focus_term lit t with
    | None -> failwith "ALF.focus_term_exn"
    | Some lit' -> lit'

  let replace a by = match a with
    | Left (op, mf, m) -> make op (M.sum (MF.rest mf) by) m
    | Right (op, m, mf) -> make op m (M.sum (MF.rest mf) by)
    | Div d -> mk_divides
                 ~sign:d.sign d.num ~power:d.power (M.sum (MF.rest d.monome) by)

  let focused_monome = function
    | Left (_, mf, _)
    | Right (_, _, mf) -> mf
    | Div d -> d.monome

  let opposite_monome = function
    | Left (_, _, m)
    | Right (_, m, _) -> Some m
    | Div _ -> None

  let opposite_monome_exn l =
    match opposite_monome l with
      | None -> invalid_arg "ALF.opposite_monome_exn"
      | Some m -> m

  let term lit = MF.term (focused_monome lit)

  let fold_terms ?(pos=P.stop) lit k =
    match lit with
      | Binary (op, m1, m2) ->
        MF.fold_m ~pos:P.(append pos (left stop)) m1 ()
          (fun () mf pos -> k (Left (op, mf, m2), pos));
        MF.fold_m ~pos:P.(append pos (right stop)) m2 ()
          (fun () mf pos -> k (Right (op, m1, mf), pos))
      | Divides d ->
        MF.fold_m ~pos d.monome ()
          (fun () mf pos -> k (Div {d with monome=mf}, pos))

  (* is the focused term maximal in the arithmetic literal? *)
  let is_max ~ord = function
    | Left (_, mf, m)
    | Right (_, m, mf) ->
      let t = MF.term mf in
      let terms = Sequence.append (M.Seq.terms m) (MF.rest mf |> M.Seq.terms) in
      Sequence.for_all
        (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
        terms
    | Div d ->
      let t = MF.term d.monome in
      Sequence.for_all
        (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
        (MF.rest d.monome |> M.Seq.terms)

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
    | Div d ->
      let t = MF.term d.monome in
      Sequence.for_all
        (fun t' -> Ordering.compare ord t t' = Comparison.Gt)
        (MF.rest d.monome |> M.Seq.terms)

  let map_lit ~f_m ~f_mf lit = match lit with
    | Left (op, mf, m) ->
      Left (op, f_mf mf, f_m m)
    | Right (op, m, mf) ->
      Right (op, f_m m, f_mf mf)
    | Div d ->
      Div { d with monome=f_mf d.monome; }

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
      | Div d ->
        Div { d with monome=mf; }
    in
    MF.unify_ff ~subst (focused_monome lit1,sc1) (focused_monome lit2,sc2)
      (fun (mf1, mf2, subst) ->
         k (_set_mf lit1 mf1, _set_mf lit2 mf2, subst))

  (* scale focused literals to have the same coefficient *)
  let scale l1 l2 =
    let z1 = MF.coeff (focused_monome l1)
    and z2 = MF.coeff (focused_monome l2) in
    let gcd = Z.gcd z1 z2 in
    product l1 (Z.divexact z2 gcd), product l2 (Z.divexact z1 gcd)

  let scale_power lit power = match lit with
    | Div d ->
      if d.power > power then invalid_arg "scale_power: cannot scale down";
      (* multiply monome by d.num^(power-d.power) *)
      let diff = power - d.power in
      if diff = 0
      then lit
      else
        let monome = MF.product d.monome Z.(pow d.num diff) in
        Div { d with monome; power;}
    | Left _
    | Right _ -> invalid_arg "scale_power: not a divisibility lit"

  let op = function
    | Left (op, _, _)
    | Right (op, _, _) -> `Binary op
    | Div _ -> `Divides

  let unfocus = function
    | Left (op, m1_f, m2) -> Binary (op, MF.to_monome m1_f, m2)
    | Right (op, m1, m2_f) -> Binary (op, m1, MF.to_monome m2_f)
    | Div d ->
      let d' = {
        num=d.num; power=d.power; sign=d.sign;
        monome=MF.to_monome d.monome;
      } in
      Divides d'

  let pp out lit =
    let op2str = function
      | Equal -> "="
      | Different -> "≠"
      | Less -> "<"
      | Lesseq -> "≤"
    in
    match lit with
      | Left (op, mf, m) ->
        Format.fprintf out "@[%a %s@ %a@]" MF.pp mf (op2str op) M.pp m
      | Right (op, m, mf) ->
        Format.fprintf out "@[%a %s@ %a@]" M.pp m (op2str op) MF.pp mf
      | Div d when d.sign ->
        let nk = Z.pow d.num d.power in
        Format.fprintf out "@[%s div %a@]" (Z.to_string nk) MF.pp d.monome
      | Div d ->
        let nk = Z.pow d.num d.power in
        Format.fprintf out "¬(@[%s div %a@])" (Z.to_string nk) MF.pp d.monome

  let to_string = CCFormat.to_string pp
end

module Util = U
