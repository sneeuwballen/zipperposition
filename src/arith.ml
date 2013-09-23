
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

(** {6 Arithmetic Manipulations} *)

open Logtk

module S = Symbol
module Lits = Literal.Arr

(** {2 Utils} *)

let is_arith_ty ty =
  Type.eq ty Type.int || Type.eq ty Type.rat || Type.eq ty Type.real

(** {2 Terms} *)

module T = struct
  include Term  (* for the rest of arith *)

  let rec is_arith t = match t.term with
    | Node ((S.Int _ | S.Rat _ | S.Real _), []) -> true
    | Node (s, _) when Symbol.Arith.is_arith s -> true
    | Var _
    | BoundVar _ ->
      begin match t.type_ with
      | Some ty -> is_arith_ty ty
      | None -> assert false
      end
    | _ -> false

  let rec sum_list l = match l with
    | [] -> failwith "Arith.sum_list: got empty list"
    | [x] -> x
    | x::l' -> mk_node S.Arith.sum [x; sum_list l']

  let mk_sum t1 t2 = mk_node S.Arith.sum [t1; t2]
  let mk_difference t1 t2 = mk_node S.Arith.difference [t1; t2]
  let mk_product t1 t2 = mk_node S.Arith.product [t1; t2]
  let mk_quotient t1 t2 = mk_node S.Arith.quotient [t1; t2]
  let mk_uminus t = mk_node S.Arith.uminus [t]

  let simplify ~signature t =
    (* recursive function with cache *)
    let rec simplify recurse t = match t.term with
    | Bind (s, t') -> mk_bind s (recurse t')
    | At (t1, t2) -> mk_at (recurse t1) (recurse t2)
    | Var _
    | BoundVar _ -> t
    | Node (s, [t']) ->
      let t' = recurse t' in
      try_reduce_unary recurse s t'
    | Node (s, [t1; t2]) ->
      let t1 = recurse t1 in
      let t2 = recurse t2 in
      try_reduce_binary recurse s t1 t2
    | Node (s, l) ->
      let t = mk_node s (List.map recurse l) in
      t
    (** unary builtins *)
    and try_reduce_unary recurse s a =
      match s, a.term with
      | S.Const ("$uminus", _), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.uminus n)
      | S.Const ("$uminus",_), Node (S.Const ("$uminus",_), [x]) -> x
      | S.Const ("$floor",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.floor n)
      | S.Const ("$ceiling",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.ceiling n)
      | S.Const ("$round",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.round n)
      | S.Const ("$truncate",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.truncate n)
      | S.Const ("$is_int",_), Node (n, []) when S.is_numeric n ->
        if S.is_int n then true_term else false_term
      | S.Const ("$is_rat",_), Node (n, []) when S.is_numeric n ->
        if S.is_rat n then true_term else false_term
      | S.Const ("$is_real",_), Node (n, []) when S.is_numeric n ->
        if S.is_real n then true_term else false_term
      | S.Const ("$to_rat",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.to_rat n)
      | S.Const ("$to_real",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.to_real n)
      | S.Const ("$to_int",_), Node (n, []) when S.is_numeric n ->
        mk_const (S.Arith.Op.to_int n)
      | _ -> mk_node s [a]  (* default case *)
    (** binary builtins *)
    and try_reduce_binary recurse s a b =
      try begin match s, a.term, b.term with
      | S.Const ("$sum",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.sum na nb)
      | S.Const ("$difference",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.difference na nb)
      | S.Const ("$product",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.product na nb)
      | S.Const ("$quotient",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.quotient na nb)
      | S.Const ("$quotient_e",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.quotient_e na nb)
      | S.Const ("$quotient_t",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.quotient_t na nb)
      | S.Const ("$quotient_f",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.quotient_f na nb)
      | S.Const ("$remainder_e",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.remainder_e na nb)
      | S.Const ("$remainder_t",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.remainder_t na nb)
      | S.Const ("$remainder_f",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        mk_const (S.Arith.Op.remainder_f na nb)
      | S.Const ("$less",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        if S.Arith.Op.less na nb then true_term else false_term
      | S.Const ("$lesseq",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        if S.Arith.Op.lesseq na nb then true_term else false_term
      | S.Const ("$greater",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        if S.Arith.Op.greater na nb then true_term else false_term
      | S.Const ("$greatereq",_), Node (na, []), Node (nb, [])
        when S.is_numeric na && S.is_numeric nb ->
        if S.Arith.Op.greatereq na nb then true_term else false_term
      | S.Const ("$sum",_), _, Node (nb,[]) when S.Arith.is_zero nb -> a
      | S.Const ("$sum",_), Node (na,[]), _ when S.Arith.is_zero na -> b
      | S.Const ("$difference",_), _, Node (nb,[]) when S.Arith.is_zero nb -> a
      | S.Const ("$difference",_), Node (na,[]), _ when S.Arith.is_zero na ->
        recurse (mk_uminus b)
      | S.Const ("$difference",_), _, _ when eq a b ->
        (* need to infer types so  that we know which zero to return *)
        let ty = TypeInference.infer_sig signature a in
        mk_const (S.Arith.zero_of_ty ty)
      | S.Const ("$product",_), _, Node (nb,[]) when S.Arith.is_one nb -> a
      | S.Const ("$product",_), Node (na,[]), _ when S.Arith.is_one na -> b
      | S.Const ("$product",_), Node (na,[]), _ when S.Arith.is_zero na -> a
      | S.Const ("$product",_), _, Node (nb,[]) when S.Arith.is_zero nb -> b
      | S.Const ("$quotient",_), _, Node (nb,[]) when S.Arith.is_one nb -> a
      | S.Const ("$quotient",_), Node (na,[]), _ when S.Arith.is_zero na -> a
      | _ -> mk_node s [a; b]  (* default case *)
      end with Division_by_zero ->
        mk_node s [a; b]
    in
    let __cache = TCache.create 9 in
    TCache.with_cache_rec __cache simplify t
end

(** {2 Polynomials of Order 1} *)

module Monome = struct
  type t = {
    coeffs : Symbol.t Term.TMap.t;
    constant : Symbol.t;
    divby : Symbol.t;  (* divide everything by this constant (cool for ints) *)
  }

  let const constant =
    assert (S.is_numeric constant);
    {
      coeffs = T.TMap.empty;
      constant;
      divby = S.Arith.one_of_ty (S.Arith.typeof constant);
    }
  
  let singleton ?divby coeff t =
    if S.Arith.is_zero coeff
      then const coeff  (* 0 *)
      else
        let coeffs = T.TMap.singleton t coeff in
        let constant = S.Arith.zero_of_ty (S.Arith.typeof coeff) in
        let divby = match divby with
        | Some d -> d
        | None -> S.Arith.one_of_ty (S.Arith.typeof coeff)
        in
        { coeffs; constant; divby; }

  let of_list constant l =
    let divby = S.Arith.one_of_ty (S.Arith.typeof constant) in
    let coeffs = List.fold_left
      (fun m (coeff, t) ->
        if S.Arith.is_zero coeff
          then m
          else T.TMap.add t coeff m)
      T.TMap.empty l
    in
    { constant; coeffs; divby; }

  let pp buf monome =
    Buffer.add_char buf '(';
    T.TMap.iter
      (fun t coeff -> Printf.bprintf buf "%a × %a +" S.pp coeff T.pp t)
      monome.coeffs;
    S.pp buf monome.constant;
    if S.Arith.is_one monome.divby
      then Buffer.add_char buf ')'
      else Printf.bprintf buf ")/%a" S.pp monome.divby

  let to_string monome = Util.on_buffer pp monome

  let mem m t = T.TMap.mem t m.coeffs

  let find m t = T.TMap.find t m.coeffs

  (* scale: multiply all coeffs by constant, multiply divby by same constant.
    This yields the very same monome *)
  let _scale m c =
    assert (not (S.Arith.is_zero c));
    if S.Arith.is_one c
      then m
      else
        let constant = S.Arith.Op.product c m.constant in
        let coeffs = T.TMap.map (fun c' -> S.Arith.Op.product c c') m.coeffs in
        let divby = S.Arith.Op.product m.divby c in
        { constant; coeffs; divby; }

  (* reduce to same divby (if any) *)
  let reduce_same_divby m1 m2 =
    match m1.divby, m2.divby with
    | S.Int n1, S.Int n2 ->
      let gcd = Big_int.gcd_big_int n1 n2 in
      let d1 = S.mk_bigint (Big_int.div_big_int n1 gcd) in
      let d2 = S.mk_bigint (Big_int.div_big_int n2 gcd) in
      _scale m1 d1, _scale m2 d2
    | c1, c2 ->
      _scale m1 (S.Arith.Op.quotient c1 c2), m2

  let sum m1 m2 =
    let m1, m2 = reduce_same_divby m1 m2 in
    let constant = S.Arith.Op.sum m1.constant m2.constant in
    let coeffs = T.TMap.merge
      (fun t c1 c2 -> match c1, c2 with
      | None, Some c
      | Some c, None -> Some c
      | Some c1, Some c2 ->
        let c = S.Arith.Op.sum c1 c2 in
        if S.Arith.is_zero c
          then None
          else Some c
      | None, None -> assert false)
      m1.coeffs m2.coeffs
    in
    { m1 with constant; coeffs; }

  let difference m1 m2 =
    let m1, m2 = reduce_same_divby m1 m2 in
    let constant = S.Arith.Op.sum m1.constant m2.constant in
    let coeffs = T.TMap.merge
      (fun t c1 c2 -> match c1, c2 with
      | None, Some c -> Some (S.Arith.Op.uminus c)
      | Some c, None -> Some c
      | Some c1, Some c2 ->
        let c = S.Arith.Op.difference c1 c2 in
        if S.Arith.is_zero c
          then None
          else Some c
      | None, None -> assert false)
      m1.coeffs m2.coeffs
    in
    { m1 with constant; coeffs; }

  let uminus m =
    let constant = S.Arith.Op.uminus m.constant in
    let coeffs = T.TMap.map S.Arith.Op.uminus m.coeffs in
    { m with constant; coeffs; }

  (* product by constant *)
  let product m c =
    if S.Arith.is_zero c
      then const c  (* 0 *)
      else  (* itemwise product *)
        let constant = S.Arith.Op.product m.constant c in
        let coeffs = T.TMap.map (fun c' -> S.Arith.Op.product c c') m.coeffs in
        { m with constant; coeffs; }

  let divby m const =
    if S.Arith.is_zero const
      then raise Division_by_zero
      else
        let divby = S.Arith.Op.product const m.divby in
        { m with divby; }

  exception NotLinear
    (** Used by [of_term] *)

  let of_term ~signature t =
    let rec of_term t = match t.T.term with
    | T.Node (s, [t1; t2]) when S.eq s S.Arith.sum ->
      let m1 = of_term t1 in
      let m2 = of_term t2 in
      sum m1 m2
    | T.Node (s, [t1; t2]) when S.eq s S.Arith.difference ->
      let m1 = of_term t1 in
      let m2 = of_term t2 in
      difference m1 m2
    | T.Node (s, [t']) when S.eq s S.Arith.uminus ->
      let m = of_term t' in
      uminus m
    | T.Node (s, [{T.term=T.Node (s',[])}; t2])
      when S.eq s S.Arith.product && S.is_numeric s' ->
      let m = of_term t2 in
      product m s'
    | T.Node (s, [t2; {T.term=T.Node (s',[])}])
      when S.eq s S.Arith.product && S.is_numeric s' ->
      let m = of_term t2 in
      product m s'
    | T.Node (s, [t2; {T.term=T.Node (s',[])}])
      when S.eq s S.Arith.quotient && S.is_numeric s' && not (S.Arith.is_zero s') ->
      let m = of_term t2 in
      divby m s'
    | T.Node (s, [_; _]) when S.Arith.is_arith s ->
      raise NotLinear  (* failure *)
    | T.Var _
    | T.BoundVar _ ->
      let ty = match t.T.type_ with Some ty -> ty | None -> assert false in
      let one = S.Arith.one_of_ty ty in
      singleton one t
    | T.Node _
    | T.At _
    | T.Bind _ ->
      let ty = TypeInference.infer_sig signature t in
      let one = S.Arith.one_of_ty ty in
      singleton one t
    in
    try Some (of_term t)
    with NotLinear -> None
      
  let to_term m =
    let sum = T.mk_const m.constant in
    let sum = T.TMap.fold
      (fun t' coeff sum -> T.mk_sum (T.mk_product (T.mk_const coeff) t') sum)
      m.coeffs sum
    in
    if S.Arith.is_one m.divby
      then sum
      else T.mk_quotient sum (T.mk_const m.divby)
end

(** {2 View a Literal as an arithmetic Literal}. *)

module Lit = struct
  type t =
  | Eq of Term.t * Monome.t
  | Neq of Term.t * Monome.t
  | L_less of Term.t * Monome.t   (* term < monome *)
  | L_lesseq of Term.t * Monome.t
  | R_less of Monome.t * Term.t
  | R_lesseq of Monome.t * Term.t

  let is_arith lit = match lit with
  | Literal.Equation (l, r, _, _) -> T.is_arith l || T.is_arith r
  | Literal.Prop (p, _) -> T.is_arith p
  | Literal.True
  | Literal.False -> false

  let extract lit = []  (* TODO *)
end

(** {2 Other transformations} *)

let purify ~ord lits = lits (* TODO *)

