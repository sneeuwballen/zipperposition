
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
  | Divides of Z.t * int * Z.t Monome.t  (* d^k divides the sum *)

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

(* TODO: remove
let to_multiset lit = match lit with
  | Equation (l, r, true) -> Multiset.create_a [|l; r|]
  | Equation (l, r, false) -> Multiset.create_a [|l; l; r; r|]
  | Prop (p, true) -> Multiset.create_a [|p; T.TPTP.true_ |]
  | Prop (p, false) -> Multiset.create_a [|p; p; T.TPTP.true_ ; T.TPTP.true_ |]
  | True -> Multiset.create_a [|T.TPTP.true_ ; T.TPTP.true_|]
  | False -> Multiset.create_a
    [|T.TPTP.true_; T.TPTP.true_; T.TPTP.true_; T.TPTP.true_|]
*)

let compare_partial ~ord l1 l2 = assert false
(*
  let m1 = to_multiset l1 in
  let m2 = to_multiset l2 in
  Multiset.compare (fun t1 t2 -> Ordering.compare ord t1 t2) m1 m2
  *)

let fold f acc lit = match lit with
  | Equation (l, r, _) -> f (f acc l) r
  | Prop (p, _) -> f acc p
  | Ineq olit -> f (f acc olit.TO.left) olit.TO.right
  | Arith (_, m1, m2) ->
      let acc = Sequence.fold f acc (Monome.Seq.terms m1) in
      Sequence.fold f acc (Monome.Seq.terms m2)
  | Divides (_, _, m) ->
      Sequence.fold f acc (Monome.Seq.terms m)
  | True
  | False -> acc

let hash lit =
  fold (fun acc t -> Hash.combine (T.hash t) acc) 17 lit

let weight lit =
  fold (fun acc t -> acc + T.size t) 0 lit

let depth lit =
  fold (fun acc t -> max acc (T.depth t)) 0 lit

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

let mk_divides n ~power m =
  Divides (n, power, m)

module Seq = struct
  let terms lit k = match lit with
    | Equation(l, r, _) -> k l; k r
    | Prop(p, _) -> k p
    | Ineq olit -> k olit.TO.left; k olit.TO.right
    | Arith (_, m1, m2) -> Monome.Seq.terms m1 k; Monome.Seq.terms m2 k
    | Divides (_, _, m) -> Monome.Seq.terms m k
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
    | Divides (_, _, m) ->
      true, Monome.Seq.terms m
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
  | Divides (n, k, m) ->
    Divides (n, k, Monome.map f m)
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
  | Divides (n, k, m) ->
    Divides (n, k, Monome.apply_subst ~renaming subst m scope)
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

let add_vars set = function
  | Equation (l, r, _) ->
    T.add_vars set l;
    T.add_vars set r
  | Prop (p, _) -> T.add_vars set p
  | True
  | False -> ()

let vars lit =
  let set = T.Tbl.create 7 in
  add_vars set lit;
  T.Tbl.to_list set

let var_occurs v lit = match lit with
  | Prop (p,_) -> T.var_occurs v p
  | Equation (l,r,_) -> T.var_occurs v l || T.var_occurs v r
  | True
  | False -> false

let is_ground lit = match lit with
  | Equation (l,r,_) -> T.is_ground l && T.is_ground r
  | Prop (p, _) -> T.is_ground p
  | True
  | False -> true

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

  let cut lit pos =
    let module P = Position in
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
    | _ -> invalid_arg "not a proper literal position"

  let is_max_term ~ord lit pos =
    assert false
    (* TODO: gather all atomic subterms, compare the one at
     * [fst (cut lit pos)] with them *)
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
      | Divides (n, k, m) ->
        let nk = Z.pow n k in
        let t = Monome.Int.to_term m in
        let sym = Symbol.TPTP.Arith.remainder_e in
        let ty = Signature.find_exn Signature.TPTP.Arith.base sym in
        let cst = T.const ~ty sym in
        (* $remainder_e(t, nk) = 0 *)
        F.Base.eq
          (T.const ~ty:Type.TPTP.int (Symbol.of_int 0))
          (T.app cst [t; T.const ~ty:Type.TPTP.int (Symbol.mk_int nk)])

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
  | Divides (d, k, m) -> Monome.is_const m && Z.sign (Monome.const m) = 0
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
  | True
  | False -> acc

(** {2 IO} *)

(* TODO: use hooks *)
let pp_debug ?(hooks=[]) buf lit =
  match lit with
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
  | Divides (n, k, m) ->
    let nk = Z.pow n k in
    Printf.bprintf buf "%s | %a" (Z.to_string nk) Monome.pp m

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
  | Divides (n, k, m) ->
    let nk = Z.mul n (Z.of_int k) in
    Printf.bprintf buf "$remainder_e(%a, %s) = 0" Monome.pp_tstp m (Z.to_string nk)

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

