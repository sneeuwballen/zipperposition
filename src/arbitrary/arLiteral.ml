
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Arbitrary Basic Terms} *)

open Logtk

module Q = QCheck
module PT = TypedSTerm
module F = PT.Form
module AT = ArTerm

type 'a arbitrary = 'a Q.arbitrary
type 'a gen = 'a QCheck.Gen.t
type lit = Literal.t
type clause = Literals.t

let shrink_lit (lit:lit) =
  let open Q.Iter in
  begin match lit with
    | Literal.Prop (t, sign) ->
      AT.shrink t >|= fun t -> Literal.mk_prop t sign
    | Literal.Equation (t,u,sign) ->
      (AT.shrink t >|= fun t -> Literal.mk_lit t u sign)
      <+> (AT.shrink u >|= fun u -> Literal.mk_lit t u sign)
    | _ -> empty
  end

let mk_lit_ gen =
  Q.make ~print:Literal.to_string ~shrink:shrink_lit gen

let shrink_lits (lits:clause) = Q.Shrink.array ~shrink:shrink_lit lits

let mk_lits gen =
  Q.make ~print:Literals.to_string ~shrink:shrink_lits gen

let lit_g : lit gen =
  let open QCheck.Gen in
  let t = 1 -- 3 >>= AT.default_fuel in
  frequency
    [ 6, return Literal.mk_prop <*> AT.pred.Q.gen <*> bool;
      6, return Literal.mk_lit <*> t <*> t <*> bool;
      1, oneofl [ Literal.mk_tauto; Literal.mk_absurd; ];
    ]

let lit = mk_lit_ lit_g

let clause_g =
  let open QCheck.Gen in
  array_size (0 -- 8) lit_g

let clause = mk_lits clause_g
