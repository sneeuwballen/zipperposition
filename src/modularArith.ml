
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

(** {1 Modular integer Arithmetic} *)

open Logtk

(** {2 Linear int expressions}

All symbols are supposed to be integer constants, and all terms must be
of type int. *)

module S = Symbol
module T = FOTerm

module Expr = struct
  type t = {
    const : Symbol.t;
    terms : (Symbol.t * FOTerm.t) list;
  }

  let eq t1 t2 =
    S.eq t1.const t2.const &&
    try List.for_all2 (fun (a1,t1) (a2, t2) -> S.eq a1 a2 && T.eq t1 t2) t1.terms t2.terms
    with Invalid_argument _ -> false

  (* merge two lists and maintain them sorted. Symbols for a given term
      are combined using [op] *)
  let rec _merge op l1 l2 = match l1, l2 with
    | [], l
    | l, [] -> l
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

  let const s = { const=s; terms=[]; }

  let singleton coeff t =
    if S.Arith.is_zero coeff
      then { const=S.Arith.zero_i; terms=[]; }
      else { const=S.Arith.zero_i; terms=[coeff, t]; }

  let sum e1 e2 =
    let const = S.Arith.Op.sum e1.const e2.const in
    let terms = _merge S.Arith.Op.sum e1.terms e2.terms in
    { const; terms; }

  let diff e1 e2 =
    let const = S.Arith.Op.difference e1.const e2.const in
    let terms = _merge S.Arith.Op.difference e1.terms e2.terms in
    { const; terms; }

  let add_const e s =
    { e with const = S.Arith.Op.sum e.const s; }

  let remove_const e =
    { e with const = S.Arith.zero_i; }

  let product e s = _fmap (fun s' -> S.Arith.Op.product s s') e

  let uminus e = _fmap S.Arith.Op.uminus e

  let succ e = add_const e S.Arith.one_i

  let pred e = add_const e S.Arith.(Op.uminus one_i)

  let is_const e = match e.terms with | [] -> true | _ -> false

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
      diff m1 m2
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

  let to_term e = 
    match e.terms with
    | [] -> T.mk_const e.const
    | (c, t)::rest ->
      (* remove one coeff to make the basic sum *)
      let sum = Arith.T.mk_product (T.mk_const c) t in
      (* add coeff*term for the remaining terms *)
      let sum = List.fold_left
        (fun sum (coeff, t') ->
          assert (not (S.Arith.is_zero coeff));
          Arith.T.mk_sum sum (Arith.T.mk_product (T.mk_const coeff) t'))
        sum rest
      in
      (* add the constant (if needed) *)
      Arith.T.mk_sum (T.mk_const e.const) sum

  let quotient e c =
    if S.Arith.sign c <= 0
    then None
    else try Some (_fmap (fun s -> S.Arith.Op.quotient s c) e)
    with S.Arith.TypeMismatch _ -> None

  let divisible e c =
    S.Arith.Op.divides c e.const &&
    List.for_all (fun (c',_) -> S.Arith.Op.divides c c') e.terms

  let factorize e =
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

  let to_string = Util.on_buffer pp

  let fmt fmt e = Format.pp_print_string fmt (to_string e)
end

(** {2 Modular computations} *)

let modulo ~n c = S.Arith.Op.remainder_e c n

let sum ~n c1 c2 = modulo ~n (S.Arith.Op.sum c1 c2)

let uminus ~n c = modulo ~n (S.Arith.Op.uminus c)

let inverse ~n c = failwith "ModularArith.inverse: not implemented"
