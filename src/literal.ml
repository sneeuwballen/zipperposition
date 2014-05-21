
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
module P = Position
module AL = ArithLit

type scope = Substs.scope
type term = FOTerm.t
type form = Formula.FO.t

type t =
  | True
  | False
  | Equation of term * term * bool
  | Prop of term * bool
  | Ineq of Theories.TotalOrder.lit
  | Arith of ArithLit.t

let eq l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
    sign1 = sign2 && l1 == l2 && r1 == r2
  | Prop (p1, sign1), Prop(p2, sign2) -> sign1 = sign2 && T.eq p1 p2
  | True, True
  | False, False -> true
  | Ineq olit1, Ineq olit2 ->
      olit1.TO.order == olit2.TO.order &&
      olit1.TO.strict = olit2.TO.strict &&
      T.eq olit1.TO.left olit2.TO.left &&
      T.eq olit1.TO.right olit2.TO.right
  | Arith o1, Arith o2 -> ArithLit.eq o1 o2
  | Equation _, _
  | Prop _, _
  | True, _
  | False, _
  | Ineq _, _
  | Arith _, _ -> false

let eq_com l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
    sign1 = sign2 &&
    ((T.eq l1 l2 && T.eq r1 r2) ||
     (T.eq l1 r2 && T.eq r1 l2))
  | Prop (p1, sign1), Prop(p2, sign2) -> sign1 = sign2 && T.eq p1 p2
  | True, True
  | False, False -> true
  | Arith o1, Arith o2 -> ArithLit.eq_com o1 o2
  | _ -> eq l1 l2  (* regular comparison *)

(* FIXME: total ordering *)
let compare l1 l2 =
  let __to_int = function
    | False -> 0
    | True -> 1
    | Equation _ -> 2
    | Prop _ -> 3
    | Ineq _ -> 4
    | Arith _ -> 5
  in
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
  | Arith o1, Arith o2 -> ArithLit.cmp o1 o2
  | _, _ -> __to_int l1 - __to_int l2

let fold f acc lit = match lit with
  | Equation (l, r, _) -> f (f acc l) r
  | Prop (p, _) -> f acc p
  | Ineq olit -> f (f acc olit.TO.left) olit.TO.right
  | Arith o -> ArithLit.fold f acc o
  | True
  | False -> acc

let hash lit =
  let hash_sign = function true -> 1 | false -> 2 in
  match lit with
  | Arith o -> ArithLit.hash o
  | Prop (p, sign) -> Hash.combine (T.hash p) (hash_sign sign)
  | Equation (l, r, sign) -> Hash.hash_int3 (T.hash l) (T.hash r) (hash_sign sign)
  | _ ->
      fold (fun acc t -> Hash.combine (T.hash t) acc) 17 lit

let weight lit =
  fold (fun acc t -> acc + T.size t) 0 lit

let depth lit =
  fold (fun acc t -> max acc (T.depth t)) 0 lit

let sign = function
  | Prop (_, sign)
  | Equation (_, _, sign) -> sign
  | False -> false
  | Arith o -> ArithLit.sign o
  | Ineq _
  | True -> true

(* specific: for the term comparison *)
let polarity = function
  | Arith o -> ArithLit.polarity o
  | lit -> sign lit

let is_pos = sign

let is_neg lit = not (is_pos lit)

let is_eqn = function
  | Equation _ -> true
  | Prop _
  | Ineq _
  | Arith _
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

let is_arith = function
  | Arith _ -> true
  | _ -> false

let _on_arith p lit = match lit with
  | Arith o -> p o
  | _ -> false

let is_arith_eqn = _on_arith ArithLit.is_eqn
let is_arith_eq = _on_arith ArithLit.is_eq
let is_arith_neq = _on_arith ArithLit.is_neq
let is_arith_ineq = _on_arith ArithLit.is_ineq
let is_arith_less = _on_arith ArithLit.is_less
let is_arith_lesseq = _on_arith ArithLit.is_lesseq
let is_arith_divides = _on_arith ArithLit.is_divides

let __ty_error a b =
  let msg = Util.sprintf
    "Literal: incompatible types in equational lit for %a : %a and %a : %a"
      T.pp a Type.pp (T.ty a) T.pp b Type.pp (T.ty b)
  in
  raise (Type.Error msg)

(* primary constructor for equations and predicates *)
let mk_lit a b sign =
  if not (Type.eq (T.ty a) (T.ty b)) then __ty_error a b;
  match a, b with
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

let mk_arith x = Arith x

let mk_arith_op op m1 m2 = Arith (ArithLit.make op m1 m2)
let mk_arith_eq m1 m2 = mk_arith_op ArithLit.Equal m1 m2
let mk_arith_neq m1 m2 = mk_arith_op ArithLit.Different m1 m2
let mk_arith_less m1 m2 = mk_arith_op ArithLit.Less m1 m2
let mk_arith_lesseq m1 m2 = mk_arith_op ArithLit.Lesseq m1 m2

let mk_divides ?(sign=true) n ~power m =
  let alit = ArithLit.mk_divides ~sign n ~power m in
  (* simplify things like  not (5 | 10) ---> false *)
  if ArithLit.is_trivial alit then mk_tauto
  else if ArithLit.is_absurd alit then mk_absurd
  else Arith alit

let mk_not_divides n ~power m = mk_divides ~sign:false n ~power m

module Seq = struct
  let terms lit k = match lit with
    | Equation(l, r, _) -> k l; k r
    | Prop(p, _) -> k p
    | Ineq olit -> k olit.TO.left; k olit.TO.right
    | Arith o -> ArithLit.Seq.terms o k
    | True
    | False -> ()

  let vars lit = Sequence.flatMap T.Seq.vars (terms lit)

  let symbols lit =
    Sequence.flatMap T.Seq.symbols (terms lit)

  (* used to represent arithmetic lits... *)
  let _arith_term =
    let s = Symbol.of_string "$arith_term" in
    T.const ~ty:Type.(const s) s

  let abstract = function
    | Equation (l, r, sign) -> sign, Sequence.of_list [l; r]
    | Prop (p, sign) -> sign, Sequence.singleton p
    | True -> true, Sequence.singleton T.TPTP.true_
    | False -> false, Sequence.singleton T.TPTP.true_
    | Ineq olit -> true, Sequence.of_list [olit.TO.left; olit.TO.right]
    | Arith o ->
      (* indexing won't really work... *)
      true, Sequence.singleton _arith_term
end

let symbols lit =
  Sequence.fold
    (fun set s -> Symbol.Set.add s set)
    Symbol.Set.empty (Seq.symbols lit)

(** Unification-like operation on components of a literal. *)
module UnifOp = struct
  type op = {
    term : subst:Substs.t -> term -> scope -> term -> scope ->
      Substs.t Sequence.t;
    monomes : subst:Substs.t -> Z.t Monome.t -> scope -> Z.t Monome.t
      -> scope -> Substs.t Sequence.t;
  }
end

(* match {x1,y1} in scope 1, with {x2,y2} with scope2 *)
let unif4 op ~subst x1 y1 sc1 x2 y2 sc2 k =
  op ~subst x1 sc1 x2 sc2
    (fun subst -> op ~subst y1 sc1 y2 sc2 k);
  op ~subst y1 sc1 x2 sc2
    (fun subst -> op ~subst x1 sc1 y2 sc2 k);
  ()

(* generic unification structure *)
let unif_lits op ~subst lit1 sc1 lit2 sc2 k =
  let open UnifOp in
  match lit1, lit2 with
  | Prop (p1, sign1), Prop (p2, sign2) when sign1 = sign2 ->
    op.term ~subst p1 sc1 p2 sc2 k
  | True, True
  | False, False -> k subst
  | Equation (l1, r1, sign1), Equation (l2, r2, sign2) when sign1 = sign2 ->
    unif4 op.term ~subst l1 r1 sc1 l2 r2 sc2 k
  | Ineq olit1, Ineq olit2
    when olit1.TO.order == olit2.TO.order && olit1.TO.strict = olit2.TO.strict ->
    op.term ~subst olit1.TO.left sc1 olit2.TO.left sc2
      (fun subst -> op.term ~subst olit1.TO.right sc1 olit2.TO.right sc2 k)
  | Arith o1, Arith o2 ->
    ArithLit.generic_unif op.monomes ~subst o1 sc1 o2 sc2 k
  | _, _ -> ()

let variant ?(subst=S.empty) lit1 sc1 lit2 sc2 k =
  let op = UnifOp.({
    term=(fun ~subst t1 sc1 t2 sc2 k ->
      try k (Unif.FO.variant ~subst t1 sc1 t2 sc2)
      with Unif.Fail -> ());
    monomes=(fun ~subst m1 sc1 m2 sc2 k ->
      Monome.variant ~subst m1 sc1 m2 sc2 k)
  })
  in
  unif_lits op ~subst lit1 sc1 lit2 sc2 k

let are_variant lit1 lit2 =
  not (Sequence.is_empty (variant lit1 0 lit2 1))

let matching ?(subst=Substs.empty) lit1 sc1 lit2 sc2 k =
  let op = UnifOp.({
    term=(fun ~subst t1 sc1 t2 sc2 k ->
      try k (Unif.FO.matching_adapt_scope ~subst ~pattern:t1 sc1 t2 sc2)
      with Unif.Fail -> ());
    monomes=(fun ~subst m1 sc1 m2 sc2 k ->
      Monome.matching ~subst m1 sc1 m2 sc2 k)
  })
  in
  unif_lits op ~subst lit1 sc1 lit2 sc2 k

let subsumes ?(subst=Substs.empty) lit1 sc1 lit2 sc2 k =
  match lit1, lit2 with
  | Arith o1, Arith o2 ->
      (* use the more specific subsumption mechanism *)
      Util.debug 5 "subsumption check: %a for %a" ArithLit.pp o1 ArithLit.pp o2;
      ArithLit.subsumes ~subst o1 sc1 o2 sc2 k
  | _ -> matching ~subst lit1 sc1 lit2 sc2 k

let unify ?(subst=Substs.empty) lit1 sc1 lit2 sc2 k =
  let op = UnifOp.({
    term=(fun ~subst t1 sc1 t2 sc2 k ->
      try k (Unif.FO.unification ~subst t1 sc1 t2 sc2)
      with Unif.Fail -> ());
    monomes=(fun ~subst m1 sc1 m2 sc2 k ->
      Monome.unify ~subst m1 sc1 m2 sc2 k)
  })
  in
  unif_lits op ~subst lit1 sc1 lit2 sc2 k

let map f = function
  | Equation (left, right, sign) ->
    let new_left = f left
    and new_right = f right in
    mk_lit new_left new_right sign
  | Prop (p, sign) ->
    let p' = f p in
    mk_prop p' sign
  | Ineq olit -> Ineq (TO.map f olit)
  | Arith o -> Arith (ArithLit.map f o)
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
  | Arith o -> Arith (ArithLit.apply_subst ~renaming subst o scope)
  | True
  | False -> lit

let apply_subst_no_simp ~renaming subst lit scope =
  match lit with
  | Arith o -> Arith (ArithLit.apply_subst_no_simp ~renaming subst o scope)
  | _ -> apply_subst ~renaming subst lit scope

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
  | Arith o -> Arith (ArithLit.negate o)

let add_vars set lit = match lit with
  | Equation (l, r, _) ->
    T.add_vars set l;
    T.add_vars set r
  | Prop (p, _) -> T.add_vars set p
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
  | Ineq _
  | Arith _ -> Sequence.exists (T.var_occurs ~var:v) (Seq.terms lit)
  | True
  | False -> false

let is_ground lit = match lit with
  | Equation (l,r,_) -> T.is_ground l && T.is_ground r
  | Prop (p, _) -> T.is_ground p
  | Ineq _
  | Arith _ -> Sequence.for_all T.is_ground (Seq.terms lit)
  | True
  | False -> true

let root_terms l =
  Seq.terms l |> Sequence.to_rev_list

let to_multiset lit = match lit with
  | Prop (p,_) -> Multisets.MT.singleton p
  | Equation (l, r, _) -> Multisets.MT.doubleton l r
  | Ineq olit -> Multisets.MT.doubleton olit.TO.left olit.TO.right
  | True
  | False -> Multisets.MT.singleton T.TPTP.true_
  | Arith alit ->
      AL.Seq.to_multiset alit
        |> Multisets.MT.Seq.of_coeffs Multisets.MT.empty

let is_trivial lit = match lit with
  | True -> true
  | False -> false
  | Equation (l, r, true) -> T.eq l r
  | Equation (l, r, false) -> false
  | Ineq olit ->
      not olit.TO.strict && T.eq olit.TO.left olit.TO.right
  | Arith o -> ArithLit.is_trivial o
  | Prop (_, _) -> false

let is_absurd lit = match lit with
  | Equation (l, r, false) when T.eq l r -> true
  | Prop (p, false) when T.eq p T.TPTP.true_ -> true
  | Prop (p, true) when T.eq p T.TPTP.false_ -> true
  | False -> true
  | Ineq olit ->
      olit.TO.strict && T.eq olit.TO.left olit.TO.right
  | Arith o -> ArithLit.is_absurd o
  | _ -> false

let fold_terms ?(position=Position.stop) ?(vars=false) ~which ~ord ~subterms lit acc f =
  (* function to call at terms *)
  let at_term ~pos acc t =
    if subterms
      then T.all_positions ~vars ~pos t acc f
      else f acc t pos
  in
  match lit, which with
  | Equation (l,r,sign), `All ->
    (* visit both sides of the equation *)
    let acc = at_term ~pos:P.(append position (left stop)) acc l in
    let acc = at_term ~pos:P.(append position (right stop)) acc r in
    acc
  | Equation (l, r, sign), `Max ->
    begin match Ordering.compare ord l r with
    | Comparison.Gt ->
      at_term ~pos:P.(append position (left stop)) acc l
    | Comparison.Lt ->
      at_term ~pos:P.(append position (right @@ stop)) acc r
    | Comparison.Eq | Comparison.Incomparable ->
      (* visit both sides, they are both (potentially) maximal *)
      let acc = at_term ~pos:P.(append position (left stop)) acc l in
      at_term ~pos:P.(append position (right stop)) acc r
    end
  | Prop (p, _), _ ->
    (* p is the only term, and it's maximal *)
    at_term ~pos:P.(append position (left stop)) acc p
  | Ineq olit, `All ->
    let acc = at_term ~pos:P.(append position (left stop)) acc olit.TO.left in
    let acc = at_term ~pos:P.(append position (right stop)) acc olit.TO.right in
    acc
  | Arith o, _ ->
    ArithLit.fold_terms ~pos:position ~vars ~which ~ord ~subterms o acc f
  | Ineq olit, `Max ->
    begin match Ordering.compare ord olit.TO.left olit.TO.right with
    | Comparison.Gt ->
        at_term ~pos:P.(append position (left stop)) acc olit.TO.left
    | Comparison.Lt ->
        at_term ~pos:P.(append position (right stop)) acc olit.TO.right
    | Comparison.Eq | Comparison.Incomparable ->
      let acc = at_term ~pos:P.(append position (left stop)) acc olit.TO.left in
      let acc = at_term ~pos:P.(append position (right stop)) acc olit.TO.right in
      acc
    end
  | True, _
  | False, _ -> acc

(** {2 IO} *)

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
  | Arith o -> ArithLit.pp buf o

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
  | Arith o -> ArithLit.pp_tstp buf o

type print_hook = Buffer.t -> t -> bool
let __hooks = ref []
let add_default_hook h = __hooks := h :: !__hooks

let pp buf lit = pp_debug ~hooks:!__hooks buf lit

let to_string t = Util.on_buffer pp t

let fmt fmt lit =
  Format.pp_print_string fmt (to_string lit)

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
    | Arith a -> ArithLit.max_terms ~ord a
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
        let f = Ordering.compare ord in
        match C.dominates f t1 t2, C.dominates f t2 t1 with
        | false, false
        | true, true -> C.Incomparable
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
    let open ArithLit in
    let _to_int = function
      | False
      | True -> 0
      | Equation _
      | Prop _ -> 1  (* eqn and prop are really the same thing *)
      | Ineq _ -> 2
      | Arith (Binary (Equal, _, _)) -> 3
      | Arith (Binary (Different, _, _)) -> 4
      | Arith (Binary (Less, _, _)) -> 5
      | Arith (Binary (Lesseq, _, _)) -> 6
      | Arith (Divides _) -> 7
    in
    C.of_total (_to_int l1 - _to_int l2)

  (* by multiset of terms *)
  let _cmp_by_term_multiset ~ord l1 l2 =
    let m1 = to_multiset l1 and m2 = to_multiset l2 in
    Multisets.MT.compare_partial (Ordering.compare ord) m1 m2

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
    | Arith (AL.Binary(op1, x1, y1)), Arith (AL.Binary(op2, x2, y2)) ->
        assert (op1 = op2);
        let module MI = Monome.Int in
        let left = Multisets.MMT.doubleton (MI.to_multiset x1) (MI.to_multiset y1) in
        let right = Multisets.MMT.doubleton (MI.to_multiset x2) (MI.to_multiset y2) in
        Multisets.MMT.compare_partial
          (Multisets.MT.compare_partial (Ordering.compare ord))
          left right
    | Arith(AL.Divides d1), Arith(AL.Divides d2) ->
        assert (d1.AL.sign=d2.AL.sign);
        let c = Z.compare d1.AL.num d2.AL.num in
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
    let msg = Util.sprintf "invalid position %a in lit %a"
      Position.pp pos pp lit
    in invalid_arg msg

  let split lit pos =
    let module AL = ArithLit in
    match lit, pos with
    | (True | False), P.Stop ->
        {lit_pos=P.stop; term_pos=P.stop; term=T.TPTP.true_; }
    | Equation (l,_,_), P.Left pos' ->
        {lit_pos=P.(left stop); term_pos=pos'; term=l; }
    | Equation (_,r,_), P.Right pos' ->
        {lit_pos=P.(right stop); term_pos=pos'; term=r; }
    | Prop (p,_), P.Left pos' ->
        {lit_pos=P.(left stop); term_pos=pos'; term=p; }
    | Ineq olit, P.Left pos' ->
        {lit_pos=P.(left stop); term_pos= pos'; term=olit.TO.left; }
    | Ineq olit, P.Right pos' ->
        {lit_pos=P.(right stop); term_pos=pos'; term=olit.TO.right; }
    | Arith(AL.Divides d), P.Arg (i, pos') ->
        let term = try snd(Monome.nth d.AL.monome i) with _ -> _fail_lit lit pos in
        {lit_pos=P.(arg i stop); term_pos= pos'; term; }
    | Arith(AL.Binary (_, m1, _)), P.Left (P.Arg (i, pos')) ->
        let term = try snd(Monome.nth m1 i) with _ -> _fail_lit lit pos in
        {lit_pos=P.(left @@ arg i stop); term_pos=pos'; term; }
    | Arith(AL.Binary(_, _, m2)), P.Right (P.Arg (i, pos')) ->
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
    let module AL = ArithLit in
    match lit, at with
    | Equation (l, r, sign), P.Left pos' ->
      mk_lit (T.Pos.replace l pos' ~by) r sign
    | Equation (l, r, sign), P.Right pos' ->
      mk_lit l (T.Pos.replace r pos' ~by) sign
    | Prop (p, sign), P.Left pos' ->
      mk_prop (T.Pos.replace p pos' ~by) sign
    | True, _
    | False, _ -> lit  (* flexible, lit can be the result of a simplification *)
    | Ineq olit, P.Left pos' ->
      let olit' = {olit with TO.left=T.Pos.replace olit.TO.left pos' ~by} in
      Ineq olit'
    | Ineq olit, P.Right pos' ->
      let olit' = {olit with TO.right=T.Pos.replace olit.TO.right pos' ~by} in
      Ineq olit'
    | Arith (AL.Binary (op, m1, m2)), P.Left (P.Arg(i,pos')) ->
      let _, t = Monome.nth m1 i in
      let m1' = Monome.set_term m1 i (T.Pos.replace t pos' ~by) in
      Arith (AL.make op m1' m2)
    | Arith (AL.Binary(op, m1, m2)), P.Right (P.Arg(i,pos')) ->
      let _, t = Monome.nth m2 i in
      let m2' = Monome.set_term m2 i (T.Pos.replace t pos' ~by) in
      Arith (AL.make op m1 m2')
    | Arith (AL.Divides d), P.Arg (i, pos') ->
      let _, t = Monome.nth d.AL.monome i in
      let m' = Monome.set_term d.AL.monome i (T.Pos.replace t pos' ~by) in
      Arith (AL.mk_divides ~sign:d.AL.sign ~power:d.AL.power d.AL.num m')
    | _ -> _fail_lit lit at

  let root_term lit pos =
    at lit (fst (cut lit pos))

  let term_pos lit pos = snd (cut lit pos)

  let is_max_term ~ord lit pos =
    let module AL = ArithLit in
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
    | Arith (AL.Binary(_, m1, m2)), _ ->
        (* [t] dominates all atomic terms? *)
        let t = root_term lit pos in
        Sequence.for_all
          (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
          (Seq.terms lit)
    | Arith (AL.Divides d), _ ->
        let t = root_term lit pos in
        Sequence.for_all
          (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
          (Monome.Seq.terms d.AL.monome)
    | True, _
    | False, _ -> true  (* why not. *)
    | Equation _, _
    | Ineq _, _ -> _fail_lit lit pos
end

module Conv = struct
  type hook_from = form -> t option
  type hook_to = t -> form option

  let arith_hook_from f =
    let open Monad.Opt in
    let module SA = Symbol.TPTP.Arith in
    let module AL = ArithLit in
    let type_ok t = Type.eq Type.TPTP.int (T.ty t) in
    (* arithmetic conversion! *)
    let rec conv f = match F.view f with
    | F.Not f' -> map (conv f') negate
    | F.Atom p ->
        begin match T.Classic.view p with
        | T.Classic.App (s, _, [l; r]) when Symbol.eq s SA.less && type_ok l ->
          Monome.Int.of_term l >>= fun m1 ->
          Monome.Int.of_term r >>= fun m2 ->
          return (Arith (AL.mk_less m1 m2))
        | T.Classic.App (s, _, [l; r]) when Symbol.eq s SA.lesseq && type_ok l ->
          Monome.Int.of_term l >>= fun m1 ->
          Monome.Int.of_term r >>= fun m2 ->
          return (Arith (AL.mk_lesseq m1 m2))
        | T.Classic.App (s, _, [l; r]) when Symbol.eq s SA.greater && type_ok l ->
          Monome.Int.of_term l >>= fun m1 ->
          Monome.Int.of_term r >>= fun m2 ->
          return (Arith (AL.mk_less m2 m1))
        | T.Classic.App (s, _, [l; r]) when Symbol.eq s SA.greatereq && type_ok l ->
          Monome.Int.of_term l >>= fun m1 ->
          Monome.Int.of_term r >>= fun m2 ->
          return (Arith (AL.mk_lesseq m2 m1))
        | _ -> None
        end
    | F.Eq (l, r) when type_ok l ->
        Monome.Int.of_term l >>= fun m1 ->
        Monome.Int.of_term r >>= fun m2 ->
        return (Arith (AL.mk_eq m1 m2))
    | F.Neq (l, r) when type_ok l ->
        Monome.Int.of_term l >>= fun m1 ->
        Monome.Int.of_term r >>= fun m2 ->
        return (Arith (AL.mk_neq m1 m2))
    | _ -> None
    in conv f

  let total_order_hook_from ~instance f =
    let rec conv f = match F.view f with
      | F.Not f' ->
        Monad.Opt.map (conv f') negate
      | F.Atom p ->
          begin match T.Classic.view p with
          | T.Classic.App (s, tyargs, [l; r]) ->
              if Symbol.eq s instance.TO.less
                then Some
                (Ineq TO.({order=instance; tyargs; left=l; right=r; strict=true;} ))
              else if Symbol.eq s instance.TO.lesseq
                then Some
                (Ineq TO.({order=instance; tyargs; left=l; right=r; strict=false;} ))
              else None
          | _ -> None
          end
      | _ -> None
    in conv f

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
      | Arith o ->
        ArithLit.to_form o
end

module View = struct
  let as_eqn lit = match lit with
    | Equation (l,r,sign) -> Some (l, r, sign)
    | Prop (p, sign) -> Some (p, T.TPTP.true_, sign)
    | True
    | False
    | Ineq _
    | Arith _ -> None

  let get_eqn lit position =
    match lit, position with
    | Equation (l,r,sign), P.Left _ -> Some (l, r, sign)
    | Equation (l,r,sign), P.Right _ -> Some (r, l, sign)
    | Prop (p, sign), P.Left _ -> Some (p, T.TPTP.true_, sign)
    | True, _
    | False, _
    | Ineq _, _
    | Arith _, _ -> None
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

  let get_arith = function
    | Arith o -> Some o
    | _ -> None

  let focus_arith lit pos = match lit with
    | Arith o -> ArithLit.Focus.get o pos
    | _ -> None

  let unfocus_arith x = Arith (ArithLit.Focus.unfocus x)
end
