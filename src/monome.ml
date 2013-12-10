
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

type t = {
  const : Symbol.t;
  terms : (Symbol.t * FOTerm.t) list;
}

let eq t1 t2 =
  S.eq t1.const t2.const &&
  try List.for_all2 (fun (a1,t1) (a2, t2) -> S.eq a1 a2 && T.eq t1 t2) t1.terms t2.terms
  with Invalid_argument _ -> false

let compare m1 m2 =
  let cmp_pair (s1,t1) (s2,t2) = Util.lexicograph_combine [T.compare t1 t2; S.compare s1 s2] in
  Util.lexicograph_combine
    [ S.compare m1.const m2.const
    ; Util.lexicograph cmp_pair m1.terms m2.terms;
    ]

let hash m =
  let hash_pair (s,t) = Hash.combine (S.hash s) (T.hash t) in
  Hash.hash_list hash_pair (S.hash m.const) m.terms

let ty m = S.ty m.const

(* merge two lists and maintain them sorted. Symbols for a given term
    are combined using [op]. terms occurring only one one side
    are preserved. *)
let rec _merge op l1 l2 = match l1, l2 with
  | l, []
  | [], l -> l
  | (s1, t1)::l1', (s2, t2)::l2' ->
    match T.compare t1 t2 with
    | 0 ->
      let s' = op s1 s2 in
      if S.Arith.is_zero s'
        then _merge op l1' l2'  (* t disappears *)
        else (s', t1) :: _merge op l1' l2'
    | n when n < 0 -> (s1, t1) :: _merge op l1' l2
    | _ -> (s2, t2) :: _merge op l1 l2'

(* map [f] on all symbols of [e] *)
let _fmap f e =
  let terms = Util.list_fmap
    (fun (s,t) ->
      let s' = f s in
      if S.Arith.is_zero s'
        then None  (* [t] doesn't occur anymore *)
        else Some (s', t))
    e.terms
  in
  { const=f e.const; terms; }

  let const s =
    assert (S.is_numeric s);
    { const=s; terms=[]; }

let singleton coeff t =
  if S.Arith.is_zero coeff
    then const coeff  (* 0 *)
    else
      let terms = [coeff, t] in
      let const = S.Arith.zero_of_ty (S.ty coeff) in
      { terms; const; }

let find e t =
  let rec find l t = match l with
    | [] -> raise Not_found
    | (s, t')::_ when T.eq t t' -> s
    | _::l' -> find l' t
  in
  find e.terms t

let mem e t =
  try ignore (find e t); true
  with Not_found -> false

let add e s t =
  let rec add l s t = match l with
    | [] -> [s, t]
    | (s', t')::l' ->
      if T.eq t t'
        then
          let s'' = S.Arith.Op.sum s s' in
          if S.Arith.is_zero s''
            then l'
            else (s'', t) :: l'
        else (s', t') :: add l' s t
  in
  { e with terms = add e.terms s t; }

let add_const e s =
  { e with const = S.Arith.Op.sum e.const s; }

let remove e t =
  { e with terms = List.filter (fun (_, t') -> not (T.eq t t')) e.terms; }

let remove_const e =
  { e with const = S.Arith.zero_of_ty (ty e); }

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
  S.Arith.sign m.const

let of_list s l =
  List.fold_left
    (fun e (s,t) -> add e s t)
    (const s) l

let size e = List.length e.terms

let terms m = List.map snd m.terms

let to_list e = e.terms

let var_occurs ~var e =
  List.exists (fun (_, t) -> T.var_occurs ~var t) e.terms

let sum e1 e2 =
  let const = S.Arith.Op.sum e1.const e2.const in
  let terms = _merge S.Arith.Op.sum e1.terms e2.terms in
  { const; terms; }

let uminus e = _fmap S.Arith.Op.uminus e

let difference e1 e2 =
  sum e1 (uminus e2)

let product e s = _fmap (fun s' -> S.Arith.Op.product s s') e

let succ e = add_const e (S.Arith.one_of_ty (ty e))

let pred e = add_const e (S.Arith.Op.uminus (S.Arith.one_of_ty (ty e)))

let rec sum_list = function
  | [] -> failwith "Monome.sum_list"
  | [m] -> m
  | m::l' -> sum m (sum_list l')

let comparison m1 m2 =
  (* same type *)
  if Type.eq (ty m1) (ty m2)
  then
    (* if m1-m2 is a constant, they are comparable, otherwise it
        depends on the model/instance *)
    let m = difference m1 m2 in
    match is_const m, S.Arith.sign m.const with
    | false, _ -> Comparison.Incomparable
    | true, 0 -> Comparison.Eq
    | true, n when n < 0 -> Comparison.Lt
    | true, _ -> Comparison.Gt
  else Comparison.Incomparable

let dominates m1 m2 = match comparison m1 m2 with
  | Comparison.Eq | Comparison.Gt -> true
  | Comparison.Lt | Comparison.Incomparable -> false

let normalize_wrt_zero m =
  match m.const with
  | S.Int _ when not (is_const m) ->
    (* divide by common gcd of coeffs and constant *)
    let gcd = List.fold_left (fun gcd (c,_) -> S.Arith.Op.gcd c gcd) m.const m.terms in
    let gcd = S.Arith.Op.abs gcd in
    let const = S.Arith.Op.quotient m.const gcd in
    let terms = List.map (fun (c,t) -> S.Arith.Op.quotient c gcd, t) m.terms in
    { const; terms; }
  | S.Int _ 
  | S.Rat _
  | S.Real _ -> m
  | _ -> failwith "Monome.normalize_wrt_zero"

let split m =
  let const1, const2 = if S.Arith.sign m.const >= 0
    then m.const, S.Arith.zero_of_ty (ty m)
    else S.Arith.zero_of_ty (ty m), S.Arith.Op.abs m.const
  in
  let rec partition = function
    | [] -> [], []
    | (c,t)::l' ->
      let l1, l2 = partition l' in
      if S.Arith.sign c > 0
        then (c,t)::l1, l2
        else l1, (S.Arith.Op.uminus c, t)::l2
  in
  let terms1, terms2 = partition m.terms in
  let m1 = {terms=terms1; const=const1; } in
  let m2 = {terms=terms2; const=const2; } in
  m1, m2

exception NotLinear

let of_term t =
  let rec of_term t = match t.T.term with
  | T.Node (s, _, [t1; t2]) when S.eq s S.Arith.sum ->
    let m1 = of_term t1 in
    let m2 = of_term t2 in
    sum m1 m2
  | T.Node (s, _, [t1; t2]) when S.eq s S.Arith.difference ->
    let m1 = of_term t1 in
    let m2 = of_term t2 in
    difference m1 m2
  | T.Node (s, _, [t']) when S.eq s S.Arith.uminus ->
    let m = of_term t' in
    uminus m
  | T.Node (s, _, [{T.term=T.Node (s',_,[])}; t2])
    when S.eq s S.Arith.product && S.is_numeric s' ->
    let m = of_term t2 in
    product m s'
  | T.Node (S.Const("$succ",_), _, [t']) ->
    let m = of_term t' in
    succ m
  | T.Node (S.Const("$pred",_), _, [t']) ->
    let m = of_term t' in
    pred m
  | T.Node (s, _,[t2; {T.term=T.Node (s',_,[])}])
    when S.eq s S.Arith.product && S.is_numeric s' ->
    let m = of_term t2 in
    product m s'
  | T.Node (s, _,[t2; {T.term=T.Node (s',_,[])}])
    when not (Type.eq (T.ty t2) Type.int)
    && (S.eq s S.Arith.quotient || S.eq s S.Arith.quotient_e)
    && S.is_numeric s' && not (S.Arith.is_zero s') ->
    (* division of coefficients and constant *)
    let m = of_term t2 in
    _fmap (fun s -> S.Arith.Op.quotient s s') m
  | T.Node (s, _, []) when S.is_numeric s -> const s
  | T.Node (s, _, [_; _]) when S.Arith.is_arith s ->
    raise NotLinear 
  | T.Var _
  | T.BoundVar _ ->
    let ty = t.T.ty in
    let one = S.Arith.one_of_ty ty in
    singleton one t
  | T.Node _ ->
    let ty = t.T.ty in
    let one = S.Arith.one_of_ty ty in
    singleton one t
  in
  try of_term t
  with Symbol.Arith.TypeMismatch msg ->
    raise NotLinear

let of_term_opt t =
  try Some (of_term t)
  with NotLinear -> None

let to_term e = 
  match e.terms with
  | [] -> T.mk_const e.const
  | (c, t)::rest ->
    (* remove one coeff to make the basic sum *)
    let sum = ArithTerm.mk_product (T.mk_const c) t in
    (* add coeff*term for the remaining terms *)
    let sum = List.fold_left
      (fun sum (coeff, t') ->
        assert (not (S.Arith.is_zero coeff));
        ArithTerm.mk_sum sum (ArithTerm.mk_product (T.mk_const coeff) t'))
      sum rest
    in
    (* add the constant (if needed) *)
    ArithTerm.mk_sum (T.mk_const e.const) sum

let apply_subst ~renaming subst m sc_m =
  match m.terms with
  | [] -> m
  | _::_ ->
    List.fold_left
      (fun m (s,t) -> add m s (Substs.FO.apply ~renaming subst t sc_m))
      (const m.const) m.terms

let is_ground m =
  List.for_all (fun (_, t) -> T.is_ground t) m.terms

let pp buf e =
  let pp_pair buf (s, t) =
    if S.Arith.is_one s
      then T.pp buf t
      else Printf.bprintf buf "%a×%a" S.pp s T.pp t
  in
  match e.terms with
  | [] -> S.pp buf e.const
  | _::_ when S.Arith.is_zero e.const ->
    Util.pp_list ~sep:" + " pp_pair buf e.terms
  | _::_ ->
    Printf.bprintf buf "%a + %a" S.pp e.const (Util.pp_list ~sep:" + " pp_pair) e.terms

let to_string monome = Util.on_buffer pp monome

let fmt fmt m = Format.pp_print_string fmt (to_string m)

(* manage so that m1[t] = m2[t] *)
let reduce_same_factor m1 m2 t =
  try
    let s1 = find m1 t in
    let s2 = find m2 t in
    match s1, s2 with
    | S.Int n1, S.Int n2 ->
      let gcd = Big_int.gcd_big_int n1 n2 in
      assert (Big_int.sign_big_int n1 > 0);
      assert (Big_int.sign_big_int n2 > 0);
      assert (Big_int.sign_big_int gcd > 0);
      (* n1 × n2 = gcd × lcm, so we need to raise both n1 and n2 to lcm.
         to do that, let us introduce  n1 = gcd × d1, and n2 = gcd × d2.
         Then
            n1 × d2 = gcd × d1 × d2, and
            n2 × d1 = gcd × d2 × d1
         so we multiply m1 by d2, and m2 by d1.
      *)
      let d1 = S.mk_bigint (Big_int.div_big_int n1 gcd) in
      let d2 = S.mk_bigint (Big_int.div_big_int n2 gcd) in
      Util.debug 5 "reduce same factor: %a, %a have gcd %s, mult by %a, %a"
        pp m1 pp m2 (Big_int.string_of_big_int gcd) S.pp d2 S.pp d1;
      product m1 d2, product m2 d1
    | S.Rat _, S.Rat _
    | S.Real _, S.Real _ ->
      m1, product m2 (S.Arith.Op.quotient s1 s2)
    | _ -> raise (Invalid_argument "Monome.reduce_same_factor")
  with Not_found ->
    raise (Invalid_argument "Monome.reduce_same_factor")

(** {2 Specific to Int} *)

let has_instances m =
  let res = match m.const with
  | S.Real _
  | S.Rat _ -> true
  | S.Int c when is_const m -> Big_int.sign_big_int c = 0
  | S.Int _ ->
    begin match m.terms with
    | [] -> assert false
    | (g,_) :: l ->
      let g = List.fold_left (fun g (c,_) -> S.Arith.Op.gcd c g) g l in
      S.Arith.Op.divides g m.const
    end
  | _ -> assert false
  in
  Util.debug 5 "monome %a has instances: %B" pp m res;
  res

let quotient e c =
  if S.Arith.sign c <= 0
  then None
  else try Some (_fmap (fun s -> S.Arith.Op.quotient s c) e)
  with S.Arith.TypeMismatch _ -> None

let divisible e c =
  S.Arith.Op.divides c e.const &&
  List.for_all (fun (c',_) -> S.Arith.Op.divides c c') e.terms

let factorize e =
  if Type.eq Type.int (ty e)
  then
    let gcd = List.fold_left
      (fun gcd (c, _) -> S.Arith.Op.gcd c gcd)
      e.const e.terms
    in
    let gcd = S.Arith.Op.abs gcd in
    if S.Arith.is_one gcd || S.Arith.is_zero gcd
      then None
      else match quotient e gcd with
      | None -> assert false
    | Some e' -> Some (e', gcd)
  else None

(** {2 Fields *)

let exact_quotient e c =
  _fmap (fun s -> S.Arith.Op.quotient s c) e

(** {2 Modular Computations} *)

module Modulo = struct
  let modulo ~n c = S.Arith.Op.remainder_e c n

  let sum ~n c1 c2 = modulo ~n (S.Arith.Op.sum c1 c2)

  let uminus ~n c = modulo ~n (S.Arith.Op.uminus c)

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
      Substs.FO.empty vars
    in
    subst, nonvars

  module B = Big_int

  (** Solving diophantine equations: see
      http://mathworld.wolfram.com/DiophantineEquation.html
      for the solution for 2 variables *)

  let __one = B.big_int_of_int 1
  let pp_bigint buf b = Buffer.add_string buf (B.string_of_big_int b)

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
        let q, r = B.quomod_big_int a b in
        if B.eq_big_int r B.zero_big_int
          then (a,b,q) :: acc  (* done *)
          else
            recurse b r ((a,b,q) :: acc)
      in
      (* a list of pairs with the same GCD as [a, b], and their
          quotients *)
      assert (B.ge_big_int a b);
      let u, v = match recurse a b [] with
      | [] -> assert false
      | (a,b,_) :: l ->
        let u, v = B.zero_big_int, b in
        List.fold_left
          (fun (u, v) (a, b, q) ->
            let u' = B.sub_big_int u (B.mult_big_int v q) in
            v, u')
          (u, v) l
      in
      u, v
    in
    let sign1 = B.sign_big_int a > 0 in
    let sign2 = B.sign_big_int b > 0 in
    let sign_const = B.sign_big_int const >= 0 in
    (* use positive integers *)
    let a, b = B.abs_big_int a, B.abs_big_int b in
    let const = B.abs_big_int const in
    (* [a] must be bigger *)
    let swap = B.gt_big_int b a in
    let a, b = if swap then b, a else a, b in
    let u, v, gcd =
      let gcd = B.gcd_big_int a b in
      let q, r = B.quomod_big_int const gcd in
      if B.sign_big_int r <> 0
        then
          failwith
            (Util.sprintf "unsolvable diophantine equation %a x + %a y = %a"
              pp_bigint a pp_bigint b pp_bigint const)
        else
          let a' = B.div_big_int a gcd in
          let b' = B.div_big_int b gcd in
          (* solve for coprime numbers *)
          let u, v = solve a' b' __one in
          B.mult_big_int u q, B.mult_big_int v q, gcd
    in
    let u, v = if swap then v, u else u, v in
    (* put sign back *)
    let u = if sign1 <> sign_const then B.minus_big_int u else u in
    let v = if sign2 <> sign_const then B.minus_big_int v else v in
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
      let gcd_1_2 = Big_int.gcd_big_int a1 a2 in
      let u1, u2, _ = diophant2 a1 a2 gcd_1_2 in
      (* first, solve [a1 * u1 + a2 * u2 = gcd_1_2]. We then
          find u1_2, u' such that  [gcd_1_2 * u1_2 + u' * l' = const],
          after which [a1 * u1 * u1_2 + a2 * u1_2 * u2 + u' * l' = const]
          and we're done. *)
      begin match diophant_l (gcd_1_2 :: l') const with
      | [], _ -> assert false
      | (u_1_2 :: u'), gcd ->
        let u1' = Big_int.mult_big_int u1 u_1_2 in
        let u2' = Big_int.mult_big_int u2 u_1_2 in
        u1' :: u2' :: u', gcd
      end

  (* least common multiple of a and b *)
  let _lcm a b =
    (* a * b = gcd * lcm *)
    let gcd = Big_int.gcd_big_int a b in
    Big_int.div_big_int (Big_int.abs_big_int (Big_int.mult_big_int a b)) gcd

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
              let coeff = Big_int.div_big_int lcm12 a.(0) in
              singleton (S.mk_bigint coeff) k.(0)
          else if i = n-1
            then
              (* -lcm(n-1,n-2) / l(n-1) * k(n-2) *)
              let lcm_last = _lcm a.(n-1) a.(n-2) in
              let coeff = Big_int.minus_big_int (Big_int.div_big_int lcm_last a.(n-1)) in
              singleton (S.mk_bigint coeff) k.(n-2)
          else
            (* general case: -lcm(i-1,i)/li * k(i-1) + lcm(i,i+1)/li * ki *)
            let lcm_prev = _lcm a.(i-1) a.(i) in
            let c_prev = S.mk_bigint (Big_int.minus_big_int
              (Big_int.div_big_int lcm_prev a.(i))) in
            let lcm_i = _lcm a.(i) a.(i+1) in
            let c_i = S.mk_bigint (Big_int.div_big_int lcm_i a.(i)) in
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
      T.mk_var ~ty n

  (* is the constant +/- 1? *)
  let _is_one_abs (s, _) =
    S.Arith.is_one s || S.Arith.is_one (S.Arith.Op.uminus s)
  and _of_symb = function
    | S.Int i -> i
    | _ -> assert false

  let eq_zero ?fresh_var m =
    let open Sequence.Infix in
    (* generation of fresh variables, with default function *)
    let fresh_var = match fresh_var with
      | None -> __fresh_var m
      | Some f -> f
    in
    match m.const with
    | S.Rat _
    | S.Real _ ->
      (* eliminate variables by extracting them *)
      let terms = to_list m in
      Util.list_fmap
        (fun (c, t) ->
          if T.is_var t
            then try
              let m = if S.Arith.sign c > 0
                then exact_quotient (uminus (remove m t)) c
                else exact_quotient (remove m t) (S.Arith.Op.uminus c)
              in
              let _ = FOUnif.unification t 0 (to_term m) 0 in
              Some [ t, m ]
            with FOUnif.Fail -> None
            else None)
        terms
    | S.Int _ when is_const m -> []
    | S.Int _ ->
      let m = normalize_wrt_zero m in
      (* need to solve a diophantine equation *)
      let terms = to_list m in
      begin match terms with
      | [] when S.Arith.is_zero m.const -> [[]]  (* trivial *)
      | [c, t] when S.Arith.Op.divides c m.const ->
        (* [c * x + constant = 0], let [x = - constant / c] *)
        let n = S.Arith.Op.quotient (S.Arith.Op.uminus m.const) c in
        [ [t, const n] ]
      | _::_::_ as l when List.exists _is_one_abs l ->
        (* at leat one of the coefficients is +/- 1. Extract
            the corresponding terms *)
        let unit_terms = List.filter _is_one_abs l in
        List.map
          (fun (c, t) ->
            let m' = remove m t in
            (* t = -m' if the coefficient of t was 1, m' otherwise *)
            let m' = if S.Arith.sign c > 0 then uminus m' else m' in
            [ t, m' ])
          unit_terms
      | _::_::_ as l ->
        (* extract coefficients *)
        let l' = List.map (fun (c,_) -> _of_symb c) l in
        let c = _of_symb m.const in
        begin try
          let gcd = List.fold_left Big_int.gcd_big_int (List.hd l') (List.tl l') in
          (* coefficients for the solution hyperplane *)
          let coeffs = coeffs_n l' gcd in
          (* initial solution *)
          let init, _gcd = diophant_l l' (Big_int.minus_big_int c) in
          let init = List.map S.mk_bigint init in
          (* generate fresh vars to describe the solution space *)
          let n = List.length l in
          let vars = Sequence.(repeat () |> take (n-1) |> to_rev_list) in
          let vars = List.map (fun () -> fresh_var Type.int) vars in
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
    | _ -> failwith "bad type for a monome"

  let lower_zero ?fresh_var ~strict m =
    match m.const with
    | S.Rat _
    | S.Real _ ->
      if strict
      then
        (* eliminate variables by extracting them *)
        let terms = to_list m in
        Util.list_fmap
          (fun (c, t) ->
            if T.is_var t
              then try
                let m = if S.Arith.sign c > 0
                  then
                    (* c * t + m < 0 reachable by t = -m/c - 1 *)
                    pred (exact_quotient (uminus (remove m t)) c)
                  else
                    (* -c * t + m < 0 reachable by t = m/c + 1 *)
                    succ (exact_quotient (remove m t) (S.Arith.Op.uminus c))
                in
                let _ = FOUnif.unification t 0 (to_term m) 0 in
                Some [ t, m ]
              with FOUnif.Fail -> None
              else None)
          terms
      else (* non-strict implies that equality is ok *)
        eq_zero ?fresh_var m
    | S.Int _ when is_const m -> []
    | S.Int _ ->
      let m = normalize_wrt_zero m in
      begin match m.terms with
      | [] -> []
      | [c, t] when S.Arith.Op.divides c m.const ->
        (* c * t + m < 0 ----> t = (-m / c) - 1 *)
        let v = S.Arith.Op.(quotient (uminus m.const) c) in
        let v = if S.Arith.sign c > 0
          then S.Arith.Op.prec v
          else S.Arith.Op.succ v
        in
        [ [t, const v] ]
      | [c, t] ->
        (* must be integer, take the quotient itself *)
        let v = S.Arith.Op.(quotient_f (uminus m.const) c) in
        let v = if S.Arith.sign c < 0 then S.Arith.Op.succ v else v in
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
              let m' = if S.Arith.sign c > 0
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
          (fun gcd (c,_) -> Big_int.gcd_big_int gcd (_of_symb c))
          (_of_symb m.const) m.terms
        in
        (* now we shift the constant until it is a multiple of the gcd.
           m < const  ----> m = const' with const' < const *)
        let c = Big_int.minus_big_int (_of_symb m.const) in
        let q, r = Big_int.quomod_big_int c gcd in
        let c' = if Big_int.sign_big_int r = 0
          then if strict
            then (* already a multiple of gcd. take the previous one, gcd * (q-1) *)
              Big_int.mult_big_int (Big_int.pred_big_int q) gcd
            else (* equality has solutions *)
              c
          else (* gcd * q < gcd * q + r, ok for both strict and non-strict *)
            Big_int.mult_big_int q gcd
        in
        let c' = S.mk_bigint (Big_int.minus_big_int c') in
        let m' = { m with const = c'; } in
        eq_zero ?fresh_var m'
      end
    | _ -> failwith "bad type for a monome"

  let lt_zero ?fresh_var m =
    lower_zero ?fresh_var ~strict:true m

  let leq_zero ?fresh_var m =
    lower_zero ?fresh_var ~strict:false m

  let neq_zero ?fresh_var m =
    lt_zero ?fresh_var m
end

(** {2 Lib} *)

let bij =
  Bij.(map
    ~inject:to_term
    ~extract:of_term
    T.bij)
