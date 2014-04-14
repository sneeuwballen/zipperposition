
(*
Zipperposition: a functional superposition prover for prototyping
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

(** {1 Polynomes of order 1, over several variables}. *)

open Logtk

module T = FOTerm
module S = Symbol

type term = FOTerm.t

(** Typeclass num *)
type 'a num = {
  ty : Type.t;
  sign : 'a -> int;
  abs : 'a -> 'a;
  cmp : 'a -> 'a -> int;
  hash : 'a -> int;
  zero : 'a;
  one : 'a;
  add : 'a -> 'a -> 'a;
  mult : 'a -> 'a -> 'a;
  uminus : 'a -> 'a;
  minus : 'a -> 'a -> 'a;
  to_term : 'a -> term;
  to_string : 'a -> string;
}

let z = {
  ty = Type.TPTP.int;
  sign = Z.sign;
  abs = Z.abs;
  cmp = Z.compare;
  hash = Z.hash;
  zero = Z.zero;
  one = Z.one;
  add = Z.add;
  mult = Z.mul;
  uminus = (fun x -> Z.mul x Z.minus_one);
  minus = Z.sub;
  to_term = (fun n -> T.const ~ty:Type.TPTP.int (Symbol.mk_int n));
  to_string = Z.to_string;
}

type 'a t = {
  num : 'a num;
  const : 'a;
  terms : ('a * term) list;
  mutable to_term : term option;
}
type 'a monome = 'a t

let eq m1 m2 =
  assert (m1.num == m2.num);
  m1.num.cmp m1.const m2.const = 0
  &&
  try
    List.for_all2
      (fun (a1,t1) (a2, t2) -> m1.num.cmp a1 a2 = 0 && T.eq t1 t2)
      m1.terms m2.terms
  with Invalid_argument _ -> false

let compare m1 m2 =
  let cmp_pair (s1,t1) (s2,t2) =
    Util.lexicograph_combine [m1.num.cmp s1 s2; T.cmp t1 t2]
  in
  Util.lexicograph_combine
    [ m1.num.cmp m1.const m2.const
    ; Util.lexicograph cmp_pair m1.terms m2.terms;
    ]

let hash m =
  let hash_pair (s,t) = Hash.combine (m.num.hash s) (T.hash t) in
  Hash.hash_list hash_pair (m.num.hash m.const) m.terms

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
    match T.cmp t1 t2 with
    | 0 ->
      let s' = op s1 s2 in
      if num.cmp num.zero s' = 0
        then _merge ~num op l1' l2'  (* t disappears *)
        else (s', t1) :: _merge ~num op l1' l2'
    | n when n < 0 -> (s1, t1) :: _merge ~num op l1' l2
    | _ -> (s2, t2) :: _merge ~num op l1 l2'

(* map [f] on all symbols of [e] *)
let _fmap f e =
  let terms = Util.list_fmap
    (fun (s,t) ->
      let s' = f s in
      if e.num.cmp e.num.zero s' = 0
        then None  (* [t] doesn't occur anymore *)
        else Some (s', t))
    e.terms
  in
  { e with const=f e.const; terms; to_term=None; }

let map_num = _fmap

let mk_const ~num s =
  { num; const=s; terms=[]; to_term=None; }

let singleton ~num coeff t =
  if num.cmp num.zero coeff = 0
    then mk_const ~num coeff  (* 0 *)
    else
      let terms = [coeff, t] in
      let const = num.zero in
      { num; terms; const; to_term=None; }

let find e t =
  let rec find l t = match l with
    | [] -> None
    | (s, t')::_ when T.eq t t' -> Some s
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
  let rec add l s t = match l with
    | [] -> [s, t]
    | (s', t')::l' ->
      if T.eq t t'
        then
          let s'' = e.num.add s s' in
          if e.num.cmp e.num.zero s'' = 0
            then l'
            else (s'', t) :: l'
        else (s', t') :: add l' s t
  in
  { e with terms = add e.terms s t; to_term=None; }

let of_list ~num s l =
  List.fold_left
    (fun e (s,t) -> add e s t)
    (mk_const ~num s) l

let map f e =
  let terms = List.map (fun (n,t) -> n, f t) e.terms in
  let const = {e with terms = []} in
  List.fold_left
    (fun e (n, t) -> add e n t)
    const terms

let add_const e s =
  { e with const = e.num.add e.const s; to_term=None; }

let remove e t =
  { e with
    terms = List.filter (fun (_, t') -> not (T.eq t t')) e.terms;
    to_term=None; }

let remove_const e =
  { e with const = e.num.zero; to_term=None; }

module Seq = struct
  let terms m =
    fun k -> List.iter (fun (_, t) -> k t) m.terms

  let vars m =
    Sequence.flatMap T.Seq.vars (terms m)

  let coeffs m =
    fun k -> List.iter k m.terms
end

let is_const e = match e.terms with | [] -> true | _ -> false

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
  { e1 with const; terms; to_term=None; }

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

let dominates m1 m2 = match comparison m1 m2 with
  | Comparison.Eq | Comparison.Gt -> true
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
  let m1 = {m with terms=terms1; const=const1; to_term=None; } in
  let m2 = {m with terms=terms2; const=const2; to_term=None; } in
  m1, m2

let apply_subst ~renaming subst m sc_m =
  map
    (fun t -> Substs.FO.apply ~renaming subst t sc_m)
    m

let is_ground m =
  List.for_all (fun (_, t) -> T.is_ground t) m.terms

let pp buf e =
  let pp_pair buf (s, t) =
    if e.num.cmp s e.num.one = 0
      then T.pp buf t
      else Printf.bprintf buf "%s×%a" (e.num.to_string s) T.pp t
  in
  match e.terms with
  | [] -> Buffer.add_string buf (e.num.to_string e.const)
  | _::_ when e.num.sign e.const = 0 ->
    Util.pp_list ~sep:" + " pp_pair buf e.terms
  | _::_ ->
    Printf.bprintf buf "%s + %a" (e.num.to_string e.const)
      (Util.pp_list ~sep:" + " pp_pair) e.terms

let to_string monome = Util.on_buffer pp monome

let fmt fmt m = Format.pp_print_string fmt (to_string m)

let pp_tstp buf e =
  let rec pp_pair buf (s, t) =
    if e.num.cmp s e.num.one = 0
      then T.pp buf t
      else Printf.bprintf buf "$product(%s, %a)" (e.num.to_string s) T.pp t
  and pp_list buf l = match l with
    | [] -> ()
    | [s, t] -> pp_pair buf (s, t)
    | (s, t)::l' ->
      Printf.bprintf buf "$sum(%a, %a)" pp_pair (s,t) pp_list l'
  in
  match e.terms with
  | [] -> Buffer.add_string buf (e.num.to_string e.const)
  | _::_ when e.num.sign e.const = 0 -> pp_list buf e.terms
  | _::_ ->
    Printf.bprintf buf "$sum(%s, %a)" (e.num.to_string e.const) pp_list e.terms

exception NotLinear

module Int = struct
  let num = z
  type t = Z.t monome

  let const = mk_const ~num
  let singleton = singleton ~num
  let of_list = of_list ~num

  module TC = T.Classic
  module SA = Symbol.TPTP.Arith

  let of_term_exn t =
    let rec of_term t : t = match TC.view t with
    | TC.App (s, _, [t1; t2]) when S.eq s SA.sum ->
      let m1 = of_term t1 in
      let m2 = of_term t2 in
      sum m1 m2
    | TC.App (s, _, [t1; t2]) when S.eq s SA.difference ->
      let m1 = of_term t1 in
      let m2 = of_term t2 in
      difference m1 m2
    | TC.App (s, _, [t']) when S.eq s SA.uminus ->
      let m = of_term t' in
      uminus m
    | TC.App (s, _, [t1; t2]) when S.eq s SA.product ->
        begin match TC.view t1, TC.view t2 with
        | TC.App (S.Int n, _, []), _ ->
          let m = of_term t2 in
          product m n
        | _, TC.App (S.Int n, _, []) ->
          let m = of_term t1 in
          product m n
        | _ -> raise NotLinear
        end
    | TC.App (s, _, [t']) when S.eq s SA.succ ->
      let m = of_term t' in
      succ m
    | TC.App (s, _, [t']) when S.eq s SA.prec ->
      let m = of_term t' in
      pred m
    | TC.App (S.Int n, _, []) -> const n
    | TC.App (s, _, [_; _]) when SA.is_arith s ->
      raise NotLinear
    | TC.App _
    | TC.Var _
    | TC.BVar _ -> singleton num.one t
    | TC.NonFO -> raise NotLinear
    in
    of_term t

  let of_term t =
    try Some (of_term_exn t)
    with NotLinear -> None

  let mk_product a b =
    T.app_full
      (T.const ~ty:num.ty SA.product) [num.ty]
      [a; b]

  let mk_sum a b =
    T.app_full
      (T.const ~ty:num.ty SA.sum) [num.ty]
      [a; b]

  let mk_const n = T.const ~ty:num.ty (Symbol.mk_int n)

  let to_term e =
    match e.to_term with
    | Some t -> t
    | None ->
      let t = match e.terms with
      | [] -> mk_const e.const
      | (c, t)::rest ->
        (* remove one coeff to make the basic sum *)
        let sum = mk_sum (mk_const c) t in
        (* add coeff*term for the remaining terms *)
        let sum = List.fold_left
          (fun sum (coeff, t') ->
            assert (num.sign coeff <> 0);
            mk_sum sum (mk_product (mk_const coeff) t))
          sum rest
        in
        (* add the constant (if needed) *)
        mk_sum (mk_const e.const) sum
      in
      e.to_term <- Some t;
      t

  let normalize_wrt_zero m =
    if is_const m
    then m
    else
      (* divide by common gcd of coeffs and constant *)
      let gcd = List.fold_left (fun gcd (c,_) -> Z.gcd c gcd) m.const m.terms in
      let gcd = Z.abs gcd in
      if Z.equal Z.one gcd
        then m
        else _fmap (fun c -> Z.div c gcd) m

  let pp_z buf n = Buffer.add_string buf (Z.to_string n)

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
      Util.debug 5 "reduce same factor: %a, %a have gcd %a, mult by %a, %a"
        pp m1 pp m2 pp_z gcd pp_z d2 pp_z d1;
      product m1 d2, product m2 d1
    with Not_found ->
      raise (Invalid_argument "Monome.reduce_same_factor")

  (** {2 Specific to Int} *)

  let has_instances m =
    let res = match m.terms with
      | [] -> Z.sign m.const = 0
      | (g,_) :: l ->
        let g = List.fold_left (fun g (c,_) -> Z.gcd c g) g l in
        Z.sign (Z.rem m.const g) = 0
    in
    Util.debug 5 "monome %a has instances: %B" pp m res;
    res

  let quotient e c =
    if Z.sign c <= 0
    then None
    else try Some (_fmap (fun s -> Z.divexact s c) e)
    with _ -> None

  let divisible e c =
    Z.sign (Z.rem e.const c) = 0
    &&
    List.for_all (fun (c',_) -> Z.sign (Z.rem c' c) = 0) e.terms

  let factorize e =
    let gcd = List.fold_left
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

    let inverse ~n c = failwith "Monome.Modulo.inverse: not implemented"
  end

  (** {2 Find Solutions} *)

  module Solve = struct
    type solution = (FOTerm.t * t) list
      (** List of constraints (term = monome). It means that
          if all those constraints are satisfied, then a solution
          to the given problem has been found *)

    let split_solution s =
      let vars, nonvars = List.partition (fun (t, _) -> T.is_var t) s in
      let subst = List.fold_left
        (fun subst (v, m) -> Substs.FO.bind subst v 0 (to_term m) 0)
        Substs.empty vars
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
      let solve a b const =
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
        | (a,b,_) :: l ->
          let u, v = Z.zero, b in
          List.fold_left
            (fun (u, v) (a, b, q) ->
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
              (Util.sprintf "unsolvable diophantine equation %a x + %a y = %a"
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
    let coeffs_n l gcd =
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
      fun ty ->
        let n = !count in
        incr count;
        T.var ~ty:num.ty n

    (* is the constant +/- 1? *)
    let _is_one_abs (s, _) = Z.equal Z.one (Z.abs s)

    let eq_zero ?fresh_var m =
      let open Sequence.Infix in
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
