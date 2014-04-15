
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

(** {1 Equational literals} *)

open Logtk

module T = FOTerm
module F = Formula.FO
module S = Substs
module TO = Theories.TotalOrder
module PB = Position.Build

type scope = Substs.scope
type term = FOTerm.t
type form = Formula.FO.t

type arith_op =
  | Equal
  | Different
  | Less
  | Lesseq

type t =
  | True
  | False
  | Equation of term * term * bool
  | Prop of term * bool
  | Ineq of Theories.TotalOrder.lit
  | Arith of arith_op * Z.t Monome.t * Z.t Monome.t
  | Divides of Z.t * int * Z.t Monome.t * bool

let eq l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
    sign1 = sign2 && l1 == l2 && r1 == r2
  | Prop (p1, sign1), Prop(p2, sign2) -> sign1 = sign2 && T.eq p1 p2
  | True, True
  | False, False -> true
  | _, _ -> false

let eq_com l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
    sign1 = sign2 &&
    ((T.eq l1 l2 && T.eq r1 r2) ||
     (T.eq l1 r2 && T.eq r1 l2))
  | Prop (p1, sign1), Prop(p2, sign2) -> sign1 = sign2 && T.eq p1 p2
  | True, True
  | False, False -> true
  | _, _ -> false

let __to_int = function
  | False -> 0
  | True -> 1
  | Equation _ -> 2
  | Prop _ -> 3
  | Ineq _ -> 4
  | Arith _ -> 5
  | Divides _ -> 6

let compare l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
      let c = T.cmp l1 l2 in
      if c <> 0 then c else
        let c = T.cmp r1 r2 in
        if c <> 0 then c else
          Pervasives.compare sign1 sign2
  | Prop (p1, sign1), Prop(p2, sign2) ->
    let c = T.cmp p1 p2 in
    if c <> 0 then c else Pervasives.compare sign1 sign2
  | True, True
  | False, False -> 0
  | _, _ -> __to_int l1 - __to_int l2

let variant ?(subst=S.empty) lit1 sc1 lit2 sc2 =
  match lit1, lit2 with
  | Prop (p1, sign1), Prop (p2, sign2) when sign1 = sign2 ->
    Unif.FO.variant ~subst p1 sc1 p2 sc2
  | True, True
  | False, False -> subst
  | Equation (l1, r1, sign1), Equation (l2, r2, sign2) when sign1 = sign2 ->
    begin try
      let subst = Unif.FO.variant ~subst l1 sc1 l2 sc2 in
      Unif.FO.variant ~subst r1 sc1 r2 sc2
    with Unif.Fail ->
      let subst = Unif.FO.variant ~subst l1 sc1 r2 sc2 in
      Unif.FO.variant ~subst r1 sc1 l2 sc2
    end
  | _ -> raise Unif.Fail

let are_variant lit1 lit2 =
  try
    let _ = variant lit1 0 lit2 1 in
    true
  with Unif.Fail ->
    false

let fold f acc lit = match lit with
  | Equation (l, r, _) -> f (f acc l) r
  | Prop (p, _) -> f acc p
  | Ineq olit -> f (f acc olit.TO.left) olit.TO.right
  | Arith (_, m1, m2) ->
      let acc = Sequence.fold f acc (Monome.Seq.terms m1) in
      Sequence.fold f acc (Monome.Seq.terms m2)
  | Divides (_, _, m, _) ->
      Sequence.fold f acc (Monome.Seq.terms m)
  | True
  | False -> acc

let hash lit =
  fold (fun acc t -> Hash.combine (T.hash t) acc) 17 lit

let weight lit =
  fold (fun acc t -> acc + T.size t) 0 lit

let depth lit =
  fold (fun acc t -> max acc (T.depth t)) 0 lit

let sign = function
  | Prop (_, sign)
  | Equation (_, _, sign)
  | Divides (_, _, _, sign) -> sign
  | Arith (Different, _, _)
  | False -> false
  | Ineq _
  | True
  | Arith (Equal, _, _)
  | Arith (Lesseq, _, _)
  | Arith (Less, _, _) -> true

let is_pos lit = match lit with
  | Equation (_,_,sign)
  | Prop (_, sign) -> sign
  | Ineq _
  | Arith _
  | Divides _ -> true
  | True -> true
  | False -> false

let is_neg lit = not (is_pos lit)

let is_eqn = function
  | Equation _ -> true
  | Prop _
  | Ineq _
  | Arith _
  | Divides _
  | True
  | False -> false

let is_eq lit = is_eqn lit && is_pos lit
let is_neq lit = is_eqn lit && is_neg lit

let is_ineq lit = match lit with
  | Ineq _ -> true
  | _ -> false

let is_strict_ineq lit = match lit with
  | Ineq olit -> olit.TO.strict
  | _ -> false

let is_nonstrict_ineq lit = match lit with
  | Ineq olit -> not olit.TO.strict
  | _ -> false

let is_ineq_of ~instance lit =
  match lit with
  | Ineq olit -> TO.eq olit.TO.order instance
  | _ -> false

let __ty_error a b =
  let msg = Util.sprintf
    "Literal: incompatible types in lit for %a : %a and %a : %a" T.pp a
    Type.pp (T.ty a) T.pp b Type.pp (T.ty b)
  in
  raise (Type.Error msg)

(* primary constructor for equations and predicates *)
let mk_lit a b sign =
  if not (Type.eq (T.ty a) (T.ty b)) then __ty_error a b;
  match a, b with
  | _ when T.eq a b -> if sign then True else False
  | _ when T.eq a T.TPTP.true_ && T.eq b T.TPTP.false_ -> if sign then False else True
  | _ when T.eq a T.TPTP.false_ && T.eq b T.TPTP.true_ -> if sign then False else True
  | _ when T.eq a T.TPTP.true_ -> Prop (b, sign)
  | _ when T.eq b T.TPTP.true_ -> Prop (a, sign)
  | _ when T.eq a T.TPTP.false_ -> Prop (b, not sign)
  | _ when T.eq b T.TPTP.false_ -> Prop (a, not sign)
  | _ -> Equation (a, b, sign)

let mk_eq a b = mk_lit a b true

let mk_neq a b = mk_lit a b false

let mk_prop p sign = match p with
  | _ when p == T.TPTP.true_ -> if sign then True else False
  | _ when p == T.TPTP.false_ -> if sign then False else True
  | _ ->
    if not (Type.eq (T.ty p) Type.TPTP.o)
      then __ty_error p T.TPTP.true_;
    Prop (p, sign)

let mk_true p = mk_prop p true

let mk_false p = mk_prop p false

let mk_tauto = True

let mk_absurd = False

let __ty_less = Type.(forall [var 0] (TPTP.o <== [var 0; var 0]))

let mk_less instance l r =
  let olit = TO.( {order=instance; left=l; right=r; tyargs=[T.ty l]; strict=true; } ) in
  Ineq olit

let mk_lesseq instance l r =
  let olit = TO.( {order=instance; left=l; right=r; tyargs=[T.ty l]; strict=false; } ) in
  Ineq olit

let mk_arith op m1 m2 =
  Arith (op, m1, m2)

let mk_arith_eq m1 m2 = mk_arith Equal m1 m2
let mk_arith_neq m1 m2 = mk_arith Different m1 m2
let mk_arith_less m1 m2 = mk_arith Less m1 m2
let mk_arith_lesseq m1 m2 = mk_arith Lesseq m1 m2

let mk_divides ?(sign=true) n ~power m =
  Divides (n, power, m, sign)

let mk_not_divides n ~power m = mk_divides ~sign:false n ~power m

module Seq = struct
  let terms lit k = match lit with
    | Equation(l, r, _) -> k l; k r
    | Prop(p, _) -> k p
    | Ineq olit -> k olit.TO.left; k olit.TO.right
    | Arith (_, m1, m2) -> Monome.Seq.terms m1 k; Monome.Seq.terms m2 k
    | Divides (_, _, m, _) -> Monome.Seq.terms m k
    | True
    | False -> ()

  let vars lit = Sequence.flatMap T.Seq.vars (terms lit)

  let symbols lit =
    Sequence.flatMap T.Seq.symbols (terms lit)

  let abstract = function
    | Equation (l, r, sign) -> sign, Sequence.of_list [l; r]
    | Prop (p, sign) -> sign, Sequence.singleton p
    | True -> true, Sequence.singleton T.TPTP.true_
    | False -> false, Sequence.singleton T.TPTP.true_
    | Ineq olit -> true, Sequence.of_list [olit.TO.left; olit.TO.right]
    | Arith (_, m1, m2) ->
      true, Sequence.append (Monome.Seq.terms m1) (Monome.Seq.terms m2)
    | Divides (_, _, m, sign) ->
      sign, Monome.Seq.terms m
end

let symbols lit =
  Sequence.fold
    (fun set s -> Symbol.Set.add s set)
    Symbol.Set.empty (Seq.symbols lit)

let matching ?(subst=Substs.empty) lit_a sc_a lit_b sc_b k =
  (* match t1 with t2, then t1' with t2' *)
  let match4 subst t1 t2 t1' t2' =
    try
      let subst = Unif.FO.matching_adapt_scope ~subst ~pattern:t1 sc_a t2 sc_b in
      k (Unif.FO.matching_adapt_scope ~subst ~pattern:t1' sc_a t2' sc_b)
    with Unif.Fail -> ()
  and match2 subst t1 sc1 t2 sc2 =
    try k (Unif.FO.matching_adapt_scope ~subst ~pattern:t1 sc1 t2 sc2)
    with Unif.Fail -> ()
  in
  match lit_a, lit_b with
  | Prop (pa, signa), Prop (pb, signb) ->
    if signa = signb
      then match2 subst pa sc_a pb sc_b
      else ()
  | Equation (_, _, signa), Equation (_, _, signb)
    when signa <> signb -> ()  (* different sign *)
  | Equation (la, ra, _), Equation (lb, rb, _) ->
    match4 subst la lb ra rb ; match4 subst la rb ra lb
  | True, True
  | False, False -> k subst
  | _ -> ()

let map f = function
  | Equation (left, right, sign) ->
    let new_left = f left
    and new_right = f right in
    mk_lit new_left new_right sign
  | Prop (p, sign) ->
    let p' = f p in
    mk_prop p' sign
  | Ineq olit -> Ineq (TO.map f olit)
  | Arith (op, m1, m2) ->
    Arith (op, Monome.map f m1, Monome.map f m2)
  | Divides (n, k, m, sign) ->
    Divides (n, k, Monome.map f m, sign)
  | True -> True
  | False -> False

let apply_subst ~renaming subst lit scope =
  match lit with
  | Equation (l,r,sign) ->
    let new_l = S.FO.apply ~renaming subst l scope
    and new_r = S.FO.apply ~renaming subst r scope in
    mk_lit new_l new_r sign
  | Prop (p, sign) ->
    let p' = S.FO.apply ~renaming subst p scope in
    mk_prop p' sign
  | Ineq olit -> Ineq (TO.apply_subst ~renaming subst olit scope)
  | Arith (op, m1, m2) ->
    Arith (op,
      Monome.apply_subst ~renaming subst m1 scope,
      Monome.apply_subst ~renaming subst m1 scope)
  | Divides (n, k, m, sign) ->
    Divides (n, k, Monome.apply_subst ~renaming subst m scope, sign)
  | True
  | False -> lit

let apply_subst_list ~renaming subst lits scope =
  List.map
    (fun lit -> apply_subst ~renaming subst lit scope)
    lits

let negate lit = match lit with
  | Equation (l,r,sign) -> Equation (l,r,not sign)
  | Prop (p, sign) -> Prop (p, not sign)
  | True -> False
  | False -> True
  | Ineq olit -> Ineq (TO.neg olit)
  | Arith (op, m1, m2) ->
      begin match op with
      | Equal -> Arith (Different, m1, m2)
      | Different -> Arith (Equal, m1, m2)
      | Less -> Arith (Lesseq, m2, m1)
      | Lesseq -> Arith (Less, m2, m1)
      end
  | Divides (n, k, m, sign) -> Divides (n, k, m, not sign)

let add_vars set lit = match lit with
  | Equation (l, r, _) ->
    T.add_vars set l;
    T.add_vars set r
  | Prop (p, _) -> T.add_vars set p
  | Divides _
  | Ineq _
  | Arith _ ->
      Seq.terms lit (T.add_vars set)
  | True
  | False -> ()

let vars lit =
  let set = T.Tbl.create 7 in
  add_vars set lit;
  T.Tbl.to_list set

let var_occurs v lit = match lit with
  | Prop (p,_) -> T.var_occurs v p
  | Equation (l,r,_) -> T.var_occurs v l || T.var_occurs v r
  | Divides _
  | Ineq _
  | Arith _ -> Sequence.exists (T.var_occurs ~var:v) (Seq.terms lit)
  | True
  | False -> false

let is_ground lit = match lit with
  | Equation (l,r,_) -> T.is_ground l && T.is_ground r
  | Prop (p, _) -> T.is_ground p
  | Divides _
  | Ineq _
  | Arith _ -> Sequence.for_all T.is_ground (Seq.terms lit)
  | True
  | False -> true

let root_terms l =
  Seq.terms l |> Sequence.to_rev_list

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
    | Ineq olit -> _maxterms2 ~ord olit.TO.left olit.TO.right
    | Divides _
    | Arith _ ->
        Multiset.max_l (Ordering.compare ord) (root_terms lit)
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

  let _cmp_by_maxterms ~ord l1 l2 =
    match l1, l2 with
    | Prop (p1, _), Prop (p2, _) -> Ordering.compare ord p1 p2
    | _ ->
        let t1 = root_terms l1 and t2 = root_terms l2 in
        Multiset.compare_l (Ordering.compare ord) t1 t2

  (* negative literals dominate *)
  let _cmp_by_polarity l1 l2 =
    let sign1 = sign l1 in
    let sign2 = sign l2 in
    match sign1, sign2 with
    | true, true
    | false, false -> Comparison.Eq
    | true, false -> Comparison.Lt
    | false, true -> Comparison.Gt

  let _cmp_by_kind l1 l2 =
    let _to_int = function
      | False -> 0
      | True -> 1
      | Equation _
      | Prop _ -> 2  (* eqn and prop are really the same thing *)
      | Ineq _ -> 4
      | Arith _ -> 5
      | Divides _ -> 6
    and _op_to_int = function
      | Less -> 3
      | Lesseq -> 2
      | Different -> 1
      | Equal -> 0
    in
    match l1, l2 with
      | Arith (op1, _, _), Arith (op2, _, _) ->
        C.of_total (_op_to_int op1 - _op_to_int op2)
      | _ ->
        C.of_total (_to_int l1 - _to_int l2)

  (* by multiset of terms *)
  let _cmp_by_term_multiset ~ord l1 l2 =
    let t1 = root_terms l1 and t2 = root_terms l2 in
    Multiset.compare_l (Ordering.compare ord) t1 t2

  let _cmp_specific ~ord l1 l2 =
    match l1, l2 with
    | Prop _, Prop _
    | Equation _, Equation _
    | Prop _, Equation _
    | Equation _, Prop _ ->
        _cmp_by_term_multiset ~ord l1 l2
    | Ineq olit1, Ineq olit2 ->
        begin match olit1.TO.strict, olit2.TO.strict with
        | true, true
        | false, false -> _cmp_by_term_multiset ~ord l1 l2
        | true, false -> C.Gt
        | false, true -> C.Lt
        end
    | Arith (op1, x1, x2), Arith (op2, y1, y2) ->
        assert (op1 = op2);
        let cmp_term = Ordering.compare ord in
        let cmp_monomes = Monome.Int.compare cmp_term in
        let left = Multiset.create (IArray.doubleton x1 x2) in
        let right = Multiset.create (IArray.doubleton y1 y2) in
        Multiset.compare cmp_monomes left right
    | Divides (n1, k1, m1, sign1), Divides (n2, k2, m2, sign2) ->
        assert (sign1=sign2);
        let c = Z.compare n1 n2 in
        if c <> 0 then C.of_total c  (* live in totally distinct Z/nZ *)
        else
          if is_ground l1 && is_ground l2
          then
            C.Incomparable
            (* TODO: Bezout-normalize, then actually compare Monomes. *)
          else C.Incomparable
    | _, _ ->
        assert false


  let compare ~ord l1 l2 =
    let f = Comparison.(
      _cmp_by_maxterms ~ord @>>
      _cmp_by_polarity @>>
      _cmp_by_kind @>>
      _cmp_specific ~ord
    ) in
    f l1 l2
end

module Pos = struct
  let at lit pos = match lit, pos with
    | Equation (l, _, _), Position.Left pos' -> T.Pos.at l pos'
    | Equation (_, r, _), Position.Right pos' -> T.Pos.at r pos'
    | Prop (p, _), Position.Left pos' -> T.Pos.at p pos'
    | True, Position.Left Position.Stop -> T.TPTP.true_
    | False, Position.Left Position.Stop -> T.TPTP.false_
    | _ -> raise Not_found

  let replace lit ~at ~by = match lit, at with
    | Equation (l, r, sign), Position.Left pos' ->
      mk_lit (T.Pos.replace l pos' ~by) r sign
    | Equation (l, r, sign), Position.Right pos' ->
      mk_lit l (T.Pos.replace r pos' ~by) sign
    | Prop (p, sign), Position.Left pos' ->
      mk_prop (T.Pos.replace p pos' ~by) sign
    | True, Position.Left Position.Stop -> lit
    | False, Position.Left Position.Stop -> lit
    | _ -> invalid_arg (Util.sprintf "wrong pos %a" Position.pp at)

  module P = Position

  let cut lit pos =
    match lit, pos with
    | Equation _, P.Left pos' -> P.(left stop), pos'
    | Equation _, P.Right pos' -> P.(right stop), pos'
    | Prop _, P.Left pos' -> P.(left stop), pos'
    | Ineq _, P.Left pos' -> P.(left stop), pos'
    | Ineq _, P.Right pos' -> P.(right stop), pos'
    | Divides _, P.Arg (i, pos') -> P.(arg i stop), pos'
    | Arith (_, _, _), P.Left (P.Arg (i, pos')) ->
        P.(left @@ arg i stop), pos'
    | Arith (_, _, _), P.Right (P.Arg (i, pos')) ->
        P.(right @@ arg i stop), pos'
    | _ -> invalid_arg "cut: not a proper literal position"

  let root_term lit pos =
    at lit (fst (cut lit pos))

  let term_pos lit pos = snd (cut lit pos)

  let is_max_term ~ord lit pos =
    match lit, pos with
    | Equation (l, r, _), P.Left _ ->
        Ordering.compare ord l r <> Comparison.Lt
    | Equation (l, r, _), P.Right _ ->
        Ordering.compare ord r l <> Comparison.Lt
    | Prop _, _ -> true
    | Ineq olit, P.Left _ ->
        Ordering.compare ord olit.TO.left olit.TO.right <> Comparison.Lt
    | Ineq olit, P.Right _ ->
        Ordering.compare ord olit.TO.right olit.TO.left <> Comparison.Lt
    | Arith (_, m1, m2), _ ->
        (* [t] dominates all atomic terms? *)
        let t = root_term lit pos in
        Sequence.for_all
          (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
          (Seq.terms lit)
    | Divides (_, _, m, _), _ ->
        let t = root_term lit pos in
        Sequence.for_all
          (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
          (Monome.Seq.terms m)
    | True, _
    | False, _ -> true  (* why not. *)
    | Equation _, _
    | Ineq _, _ -> invalid_arg "is_max_term: not a proper literal position"
end

module Conv = struct
  type hook_from = form -> t option
  type hook_to = t -> form option

  let arith_hook_from f =
    None (* TODO: try to convert form into a arith lit *)

  let total_order_hook_from ~instance f =
    None   (* TODO: try to convert into a total order. *)

  let rec try_hooks x hooks = match hooks with
    | [] -> None
    | h::hooks' ->
        match h x with
        | None -> try_hooks x hooks'
        | (Some _) as res -> res

  let of_form ?(hooks=[]) f =
    let f = F.simplify f in
    match try_hooks f hooks with
    | Some lit -> lit
    | None ->
      begin match F.view f with
      | F.True -> True
      | F.False -> False
      | F.Atom a -> mk_true a
      | F.Eq (l,r) -> mk_eq l r
      | F.Neq (l,r) -> mk_neq l r
      | F.Not f' ->
          begin match F.view f' with
          | F.Atom a -> mk_false a
          | F.Eq (l,r) -> mk_neq l r
          | F.Neq (l,r) -> mk_eq l r
          | _ -> failwith (Util.sprintf "not a literal: %a" F.pp f)
          end
      | _ -> failwith (Util.sprintf "not a literal: %a" F.pp f)
      end

  let to_form ?(hooks=[]) lit =
    match try_hooks lit hooks with
    | Some f -> f
    | None ->
      match lit with
      | Equation (l, r, true) -> F.Base.eq l r
      | Equation (l, r, false) -> F.Base.neq l r
      | Prop (p, true) -> F.Base.atom p
      | Prop (p, false) -> F.Base.not_ (F.Base.atom p)
      | True -> F.Base.true_
      | False -> F.Base.false_
      | Ineq olit ->
        let l = olit.TO.left and r = olit.TO.right in
        let p = if olit.TO.strict
          then T.app_full (TO.less_const olit.TO.order) olit.TO.tyargs [l; r]
          else T.app_full (TO.lesseq_const olit.TO.order) olit.TO.tyargs [l; r]
        in F.Base.atom p
      | Arith(op, m1, m2) ->
        let t1 = Monome.Int.to_term m1 in
        let t2 = Monome.Int.to_term m2 in
        begin match op with
          | Equal -> F.Base.eq t1 t2
          | Different -> F.Base.neq t1 t2
          | Less ->
            let sym = Symbol.TPTP.Arith.less in
            let ty = Signature.find_exn Signature.TPTP.Arith.base sym in
            let cst = T.const ~ty sym in
            F.Base.atom (T.app_full cst [Type.TPTP.int] [t1; t2])
          | Lesseq ->
            let sym = Symbol.TPTP.Arith.lesseq in
            let ty = Signature.find_exn Signature.TPTP.Arith.base sym in
            let cst = T.const ~ty sym in
            F.Base.atom (T.app_full cst [Type.TPTP.int] [t1; t2])
        end
      | Divides (n, k, m, sign) ->
        let nk = Z.pow n k in
        let t = Monome.Int.to_term m in
        let sym = Symbol.TPTP.Arith.remainder_e in
        let ty = Signature.find_exn Signature.TPTP.Arith.base sym in
        let cst = T.const ~ty sym in
        (* $remainder_e(t, nk) = 0 *)
        let f = F.Base.eq
          (T.const ~ty:Type.TPTP.int (Symbol.of_int 0))
          (T.app cst [t; T.const ~ty:Type.TPTP.int (Symbol.mk_int nk)])
        in
        if sign then f else F.Base.not_ f
end

module View = struct
  let as_eqn lit = match lit with
    | Equation (l,r,sign) -> Some (l, r, sign)
    | Prop (p, sign) -> Some (p, T.TPTP.true_, sign)
    | True
    | False
    | Ineq _
    | Arith _
    | Divides _ -> None

  let get_eqn lit position =
    match lit, position with
    | Equation (l,r,sign), Position.Left _ -> Some (l, r, sign)
    | Equation (l,r,sign), Position.Right _ -> Some (r, l, sign)
    | Prop (p, sign), Position.Left _ -> Some (p, T.TPTP.true_, sign)
    | True, _
    | False, _
    | Ineq _, _
    | Arith _, _
    | Divides _, _ -> None
    | _ -> invalid_arg "get_eqn: wrong literal or position"

  let get_ineq = function
    | Ineq lit' -> Some lit'
    | _ -> None

  let get_ineq_of ~instance lit = match lit with
    | Ineq olit ->
        if TO.eq olit.TO.order instance
        then Some olit
        else None
    | _ -> None
end

let is_trivial lit = match lit with
  | True -> true
  | False -> false
  | Equation (l, r, true) -> T.eq l r
  | Equation (l, r, false) -> false
  | Ineq olit ->
      not olit.TO.strict && T.eq olit.TO.left olit.TO.right
  | Divides (d, k, m, true) -> Monome.is_const m && Z.sign (Monome.const m) = 0
  | Divides (d, k, m, false) -> Monome.is_const m && Z.sign (Monome.const m) > 0
  | Arith _ -> false  (* TODO *)
  | Prop (_, _) -> false

let fold_terms ?(position=Position.stop) ?(vars=false) ~which ~ord ~subterms lit acc f =
  (* function to call at terms *)
  let at_term ~pos acc t =
    if subterms
      then T.all_positions ~vars ~pos t acc f
      else f acc t pos
  in
  match lit with
  | Equation (l,r,sign) ->
    begin match Ordering.compare ord l r, which with
    | Comparison.Gt, (`Max | `One) ->
      at_term
        ~pos:Position.(append position (left stop))
        acc l
    | Comparison.Lt, (`Max | `One) ->
      at_term
        ~pos:Position.(append position (right @@ stop))
        acc r
    | _, `One ->
      (* only visit one side (arbitrary) *)
      at_term ~pos:Position.(append position (left stop)) acc l
    | (Comparison.Eq | Comparison.Incomparable), `Max ->
      (* visit both sides, they are both (potentially) maximal *)
      let acc = at_term ~pos:Position.(append position (left stop)) acc l in
      at_term ~pos:Position.(append position (right stop)) acc r
    | _, `Both ->
      (* visit both sides of the equation *)
      let acc = at_term
        ~pos:Position.(append position (left stop)) acc l in
      let acc = at_term
        ~pos:Position.(append position (right stop)) acc r in
      acc
    end
  | Prop (p, _) ->
    at_term ~pos:Position.(append position (left stop)) acc p
  | Ineq olit ->
    let acc = at_term
      ~pos:Position.(append position (left stop)) acc olit.TO.left in
    let acc = at_term
      ~pos:Position.(append position (right stop)) acc olit.TO.right in
    acc
  | Arith (op, m1, m2) ->
      let acc = Monome.fold
        (fun acc i _ t -> at_term ~pos:Position.(left @@ arg i stop) acc t)
        acc m1
      in
      Monome.fold
        (fun acc i _ t -> at_term ~pos:Position.(right @@ arg i stop) acc t)
        acc m2
  | Divides (_, _, m, _) ->
      Monome.fold
        (fun acc i _ t -> at_term ~pos:Position.(arg i stop) acc t)
        acc m
  | True
  | False -> acc

(** {2 IO} *)

(* TODO: use hooks *)
let pp_debug ?(hooks=[]) buf lit =
  if List.for_all (fun h -> not (h buf lit)) hooks
  then match lit with
  | Prop (p, true) -> T.pp buf p
  | Prop (p, false) -> Printf.bprintf buf "¬%a" T.pp p
  | True -> Buffer.add_string buf "true"
  | False -> Buffer.add_string buf "false"
  | Equation (l, r, true) ->
    Printf.bprintf buf "%a = %a" T.pp l T.pp r
  | Equation (l, r, false) ->
    Printf.bprintf buf "%a ≠ %a" T.pp l T.pp r
  | Ineq olit ->
    Printf.bprintf buf "%a %s %a" T.pp olit.TO.left
      (if olit.TO.strict then "<" else "≤") T.pp olit.TO.right
  | Arith (op, l, r) ->
    Printf.bprintf buf "%a %s %a"
      Monome.pp l
      (match op with Equal -> "=" | Different -> "≠"
        | Less -> "<" | Lesseq -> "≤")
      Monome.pp r
  | Divides (n, k, m, true) ->
    let nk = Z.pow n k in
    Printf.bprintf buf "%s | %a" (Z.to_string nk) Monome.pp m
  | Divides (n, k, m, false) ->
    let nk = Z.pow n k in
    Printf.bprintf buf "¬(%s | %a)" (Z.to_string nk) Monome.pp m

let pp_tstp buf lit =
  match lit with
  | Prop (p, true) -> T.TPTP.pp buf p
  | Prop (p, false) -> Printf.bprintf buf "~ %a" T.TPTP.pp p
  | True -> Buffer.add_string buf "$true"
  | False -> Buffer.add_string buf "$false"
  | Equation (l, r, true) ->
    Printf.bprintf buf "%a = %a" T.TPTP.pp l T.TPTP.pp r
  | Equation (l, r, false) ->
    Printf.bprintf buf "%a != %a" T.TPTP.pp l T.TPTP.pp r
  | Ineq olit ->
    Printf.bprintf buf "%s(%a, %a)"
      (if olit.TO.strict then "$less" else "$lesseq")
      T.TPTP.pp olit.TO.left T.TPTP.pp olit.TO.right
  | Arith (Equal, l, r) ->
    Printf.bprintf buf "%a = %a" Monome.pp_tstp l Monome.pp_tstp r
  | Arith (Different, l, r) ->
    Printf.bprintf buf "%a != %a" Monome.pp_tstp l Monome.pp_tstp r
  | Arith (Less, l, r) ->
    Printf.bprintf buf "$less(%a, %a)" Monome.pp_tstp l Monome.pp_tstp r
  | Arith (Lesseq, l, r) ->
    Printf.bprintf buf "$lesseq(%a, %a)" Monome.pp_tstp l Monome.pp_tstp r
  | Divides (n, k, m, true) ->
    let nk = Z.mul n (Z.of_int k) in
    Printf.bprintf buf "$remainder_e(%a, %s) = 0" Monome.pp_tstp m (Z.to_string nk)
  | Divides (n, k, m, false) ->
    let nk = Z.mul n (Z.of_int k) in
    Printf.bprintf buf "$remainder_e(%a, %s) != 0" Monome.pp_tstp m (Z.to_string nk)

(* TODO: make it a hook
let pp_arith buf lit =
  match lit with
  | Prop ({T.term=T.Node(f, _, [a;b])},true) when Symbol.eq f Symbol.Arith.less ->
    Printf.bprintf buf "%a < %a" T.pp_arith a T.pp_arith b
  | Prop ({T.term=T.Node(f, _, [a;b])},true) when Symbol.eq f Symbol.Arith.lesseq ->
    Printf.bprintf buf "%a ≤ %a" T.pp_arith a T.pp_arith b
  | Prop (p, true) -> T.pp_arith buf p
  | Prop (p, false) -> Printf.bprintf buf "¬%a" T.pp_arith p
  | True -> Buffer.add_string buf "true"
  | False -> Buffer.add_string buf "false"
  | Equation (l, r, true, _) ->
    Printf.bprintf buf "%a = %a" T.pp_arith l T.pp_arith r
  | Equation (l, r, false, _) ->
    Printf.bprintf buf "%a ≠ %a" T.pp_arith l T.pp_arith r
*)

type print_hook = Buffer.t -> t -> bool
let __hooks = ref []
let add_default_hook h = __hooks := h :: !__hooks

let pp buf lit = pp_debug ~hooks:!__hooks buf lit

let to_string t = Util.on_buffer pp t

let fmt fmt lit =
  Format.pp_print_string fmt (to_string lit)

