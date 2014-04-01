
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
type form = Formula.FO.t

type t =
  | Equation of T.t * T.t * bool * Comparison.t
  | Prop of T.t * bool
  | True
  | False
  (** a literal, that is, a signed equation or a proposition *)

let eq l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,ord1), Equation (l2,r2,sign2,ord2) ->
    sign1 = sign2 && l1 == l2 && r1 == r2 && ord1 = ord2
  | Prop (p1, sign1), Prop(p2, sign2) -> sign1 = sign2 && T.eq p1 p2
  | True, True
  | False, False -> true
  | _, _ -> false

let eq_com l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
    sign1 = sign2 &&
    ((T.eq l1 l2 && T.eq r1 r2 && o1 = o2) ||
     (T.eq l1 r2 && T.eq r1 l2 && o1 = (Comparison.opp o2)))
  | Prop (p1, sign1), Prop(p2, sign2) -> sign1 = sign2 && T.eq p1 p2
  | True, True
  | False, False -> true
  | _, _ -> false

let __to_int = function
  | Equation _ -> 3
  | Prop _ -> 2
  | True -> 1
  | False -> 0

let compare l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
      let c = Pervasives.compare o1 o2 in
      if c <> 0 then c else
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
  | Equation (l1, r1, sign1, _), Equation (l2, r2, sign2, _) when sign1 = sign2 ->
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

let to_multiset lit = match lit with
  | Equation (l, r, true, _) -> Multiset.create_a [|l; r|]
  | Equation (l, r, false, _) -> Multiset.create_a [|l; l; r; r|]
  | Prop (p, true) -> Multiset.create_a [|p; T.TPTP.true_ |]
  | Prop (p, false) -> Multiset.create_a [|p; p; T.TPTP.true_ ; T.TPTP.true_ |]
  | True -> Multiset.create_a [|T.TPTP.true_ ; T.TPTP.true_|]
  | False -> Multiset.create_a
    [|T.TPTP.true_; T.TPTP.true_; T.TPTP.true_; T.TPTP.true_|]

let compare_partial ~ord l1 l2 =
  let m1 = to_multiset l1 in
  let m2 = to_multiset l2 in
  Multiset.compare (fun t1 t2 -> Ordering.compare ord t1 t2) m1 m2

let hash lit = match lit with
  | Equation (l, r, sign, o) ->
    Hash.hash_int3 (22 + Comparison.to_total o)
      (T.hash l) (T.hash r)
  | Prop (p, sign) -> T.hash p
  | True -> 2
  | False -> 1

let weight = function
  | Equation (l, r, _ ,_) -> T.size l + T.size r
  | Prop (p, _) -> T.size p
  | True
  | False -> 1

let depth = function
  | Equation (l, r, _ ,_) -> max (T.depth l) (T.depth r)
  | Prop (p, _) -> T.depth p
  | True
  | False -> 0

let is_pos lit = match lit with
  | Equation (_,_,sign,_)
  | Prop (_, sign) -> sign
  | True -> true
  | False -> false

let is_neg lit = match lit with
  | Equation (_,_,sign,_)
  | Prop (_, sign) -> not sign
  | True -> false
  | False -> true

let equational = function
  | Equation _ -> true
  | Prop _
  | True 
  | False -> false

let orientation_of = function
  | Equation (_, _, _, ord) -> ord
  | Prop _ -> Comparison.Gt
  | True
  | False -> Comparison.Eq

let ineq_lit ~spec lit =
  match lit with
  | Prop (p, true) ->
      begin match T.Classic.view p with
      | T.Classic.App (s, _, [l;r]) ->
        let instance = TO.find ~spec s in
        let strict = Symbol.eq s instance.TO.less in
        TO.({ left=l; right=r; strict; instance; })
      | _ -> raise Not_found
      end
  | _ -> raise Not_found

let is_eq lit = equational lit && is_pos lit
let is_neq lit = equational lit && is_neg lit

let is_ineq ~spec lit =
  try
    let _ = ineq_lit ~spec lit in
    true
  with Not_found ->
    false

let is_strict_ineq ~spec lit =
  try
    let lit' = ineq_lit ~spec lit in
    lit'.TO.strict
  with Not_found ->
    false

let is_nonstrict_ineq ~spec lit =
  try
    let lit' = ineq_lit ~spec lit in
    not lit'.TO.strict
  with Not_found ->
    false

let ineq_lit_of ~instance lit = match lit with
  | Prop (p, true) ->
      begin match T.Classic.view p with
      | T.Classic.App (s, _, [l;r]) when Symbol.eq s instance.TO.less ->
        TO.({ left=l; right=r; strict=true; instance; })
      | T.Classic.App (s, _, [l;r]) when Symbol.eq s instance.TO.lesseq ->
        TO.({ left=l; right=r; strict=false; instance; })
      | _ -> raise Not_found
      end
  | _ -> raise Not_found

let is_ineq_of ~instance lit =
  match lit with
  | Prop (p, true) ->
      begin match T.Classic.view p with
      | T.Classic.App (s, _, [l;r]) ->
        Symbol.eq s instance.TO.less || Symbol.eq s instance.TO.lesseq
      | _ -> false
      end
  | _ -> false

let __ty_error a b =
  let msg = Util.sprintf
    "Literal:incompatible types in lit for %a : %a and %a : %a" T.pp a 
    Type.pp (T.ty a) T.pp b Type.pp (T.ty b)
  in
  raise (Type.Error msg)

(* primary constructor *)
let mk_lit ~ord a b sign =
  if not (Type.eq (T.ty a) (T.ty b)) then __ty_error a b;
  match a, b with
  | _ when T.eq a b -> if sign then True else False
  | _ when T.eq a T.TPTP.true_ && T.eq b T.TPTP.false_ -> if sign then False else True
  | _ when T.eq a T.TPTP.false_ && T.eq b T.TPTP.true_ -> if sign then False else True
  | _ when T.eq a T.TPTP.true_ -> Prop (b, sign)
  | _ when T.eq b T.TPTP.true_ -> Prop (a, sign)
  | _ when T.eq a T.TPTP.false_ -> Prop (b, not sign)
  | _ when T.eq b T.TPTP.false_ -> Prop (a, not sign)
  | _ -> Equation (a, b, sign, Ordering.compare ord a b)

let mk_eq ~ord a b = mk_lit ~ord a b true

let mk_neq ~ord a b = mk_lit ~ord a b false

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
  let open Theories.TotalOrder in
  mk_true (T.app_full (T.const ~ty:__ty_less instance.less) [T.ty l] [l; r])

let mk_lesseq instance l r =
  let open Theories.TotalOrder in
  mk_true (T.app_full (T.const ~ty:__ty_less instance.lesseq) [T.ty l] [l; r])

module Seq = struct
  let terms lit k = match lit with
    | Equation(l, r, _, _) -> k l; k r
    | Prop(p, _) -> k p
    | True
    | False -> ()

  let vars lit = Sequence.flatMap T.Seq.vars (terms lit)

  let symbols lit =
    Sequence.flatMap T.Seq.symbols (terms lit)
end

let apply_subst ~renaming ~ord subst lit scope =
  match lit with
  | Equation (l,r,sign,_) ->
    let new_l = S.FO.apply ~renaming subst l scope
    and new_r = S.FO.apply ~renaming subst r scope in
    mk_lit ~ord new_l new_r sign
  | Prop (p, sign) ->
    let p' = S.FO.apply ~renaming subst p scope in
    mk_prop p' sign
  | True
  | False -> lit

let symbols lit =
  Sequence.fold
    (fun set s -> Symbol.Set.add s set)
    Symbol.Set.empty (Seq.symbols lit)

let reord ~ord lit =
  match lit with
  | Equation (l,r,sign,_) -> mk_lit ~ord l r sign
  | Prop _
  | True
  | False -> lit

let lit_of_form ~ord f =
  let f = F.simplify f in
  match F.view f with
  | F.True -> True
  | F.False -> False
  | F.Atom a -> mk_true a
  | F.Eq (l,r) -> mk_eq ~ord l r
  | F.Neq (l,r) -> mk_neq ~ord l r
  | F.Not f' ->
      begin match F.view f' with
      | F.Atom a -> mk_false a
      | F.Eq (l,r) -> mk_neq ~ord l r
      | F.Neq (l,r) -> mk_eq ~ord l r
      | _ -> failwith (Util.sprintf "not a literal: %a" F.pp f)
      end
  | _ -> failwith (Util.sprintf "not a literal: %a" F.pp f)

let to_tuple = function
  | Equation (l,r,sign,_) -> l,r,sign
  | Prop (p,sign) -> p, T.TPTP.true_, sign
  | True -> T.TPTP.true_, T.TPTP.true_, true
  | False -> T.TPTP.true_, T.TPTP.true_, false

let form_of_lit lit = match lit with
  | Equation (l, r, true, _) -> F.Base.eq l r
  | Equation (l, r, false, _) -> F.Base.neq l r
  | Prop (p, true) -> F.Base.atom p
  | Prop (p, false) -> F.Base.not_ (F.Base.atom p)
  | True -> F.Base.true_
  | False -> F.Base.false_

let term_of_lit lit = F.to_hoterm (form_of_lit lit)

let negate lit = match lit with
  | Equation (l,r,sign,ord) -> Equation (l,r,not sign,ord)
  | Prop (p, sign) -> Prop (p, not sign)
  | True -> False
  | False -> True

let fmap ~ord f = function
  | Equation (left, right, sign, _) ->
    let new_left = f left
    and new_right = f right in
    mk_lit ~ord new_left new_right sign
  | Prop (p, sign) ->
    let p' = f p in
    mk_prop p' sign
  | True -> True
  | False -> False

let add_vars set = function
  | Equation (l, r, _, _) ->
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
  | Equation (l,r,_,_) -> T.var_occurs v l || T.var_occurs v r
  | True
  | False -> false

let is_ground lit = match lit with
  | Equation (l,r,_,_) -> T.is_ground l && T.is_ground r
  | Prop (p, _) -> T.is_ground p
  | True
  | False -> true

let terms lit =
  Sequence.from_iter (fun k ->
    match lit with
    | Equation (l,r,_,_) -> k l; k r
    | Prop (p, _) -> k p
    | True
    | False -> ())

let get_eqn lit position =
  match lit, position with
  | Equation (l,r,sign,_), Position.Left _ -> (l, r, sign)
  | Equation (l,r,sign,_), Position.Right _ -> (r, l, sign)
  | Prop (p, sign), Position.Left _ -> (p, T.TPTP.true_, sign)
  | True, Position.Left _ -> (T.TPTP.true_, T.TPTP.true_, true)
  | False, Position.Left _ -> (T.TPTP.true_, T.TPTP.true_, false)
  | _ -> invalid_arg "wrong side"

module Pos = struct
  let at lit pos = match lit, pos with
    | Equation (l, _, _, _), Position.Left pos' -> T.Pos.at l pos'
    | Equation (_, r, _, _), Position.Right pos' -> T.Pos.at r pos'
    | Prop (p, _), Position.Left pos' -> T.Pos.at p pos'
    | True, Position.Left Position.Stop -> T.TPTP.true_
    | False, Position.Left Position.Stop -> T.TPTP.false_
    | _ -> raise Not_found

  let replace ~ord lit ~at ~by = match lit, at with
    | Equation (l, r, sign, _), Position.Left pos' ->
      mk_lit ~ord (T.Pos.replace l pos' ~by) r sign
    | Equation (l, r, sign, _), Position.Right pos' ->
      mk_lit ~ord l (T.Pos.replace r pos' ~by) sign
    | Prop (p, sign), Position.Left pos' ->
      mk_prop (T.Pos.replace p pos' ~by) sign
    | True, Position.Left Position.Stop -> lit
    | False, Position.Left Position.Stop -> lit
    | _ -> invalid_arg (Util.sprintf "wrong pos %a" Position.pp at)
end

let apply_subst_list ~renaming ~ord subst lits scope =
  List.map
    (fun lit -> apply_subst ~renaming ~ord subst lit scope)
    lits

(** {2 IO} *)

let pp_debug buf lit =
  match lit with
  | Prop (p, true) -> T.pp buf p
  | Prop (p, false) -> Printf.bprintf buf "¬%a" T.pp p
  | True -> Buffer.add_string buf "true"
  | False -> Buffer.add_string buf "false"
  | Equation (l, r, true, _) ->
    Printf.bprintf buf "%a = %a" T.pp l T.pp r
  | Equation (l, r, false, _) ->
    Printf.bprintf buf "%a ≠ %a" T.pp l T.pp r

let pp_tstp buf lit =
  match lit with
  | Prop (p, true) -> T.TPTP.pp buf p
  | Prop (p, false) -> Printf.bprintf buf "~ %a" T.TPTP.pp p
  | True -> Buffer.add_string buf "$true"
  | False -> Buffer.add_string buf "$false"
  | Equation (l, r, true, _) ->
    Printf.bprintf buf "%a = %a" T.TPTP.pp l T.TPTP.pp r
  | Equation (l, r, false, _) ->
    Printf.bprintf buf "%a != %a" T.TPTP.pp l T.TPTP.pp r

(*
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

let __pp = ref pp_debug
let set_default_pp pp = __pp := pp

let pp buf lit = !__pp buf lit

let to_string t = Util.on_buffer pp t

let fmt fmt lit =
  Format.pp_print_string fmt (to_string lit)

(** {2 Arrays of literals} *)

module Arr = struct
  let eq lits1 lits2 =
    let rec check i =
      if i = Array.length lits1 then true else
      eq lits1.(i) lits2.(i) && check (i+1)
    in
    if Array.length lits1 <> Array.length lits2
      then false
      else check 0

  let eq_com lits1 lits2 =
    let rec check i =
      if i = Array.length lits1 then true else
      eq_com lits1.(i) lits2.(i) && check (i+1)
    in
    if Array.length lits1 <> Array.length lits2
      then false
      else check 0

  let compare lits1 lits2 = 
    let rec check i =
      if i = Array.length lits1 then 0 else
        let cmp = compare lits1.(i) lits2.(i) in
        if cmp = 0 then check (i+1) else cmp
    in
    if Array.length lits1 <> Array.length lits2
      then Array.length lits1 - Array.length lits2
      else check 0

  let hash lits =
    Array.fold_left
      (fun h lit -> Hash.combine h (hash lit))
      13 lits

  let variant ?(subst=S.empty) a1 sc1 a2 sc2 =
    if Array.length a1 <> Array.length a2
      then raise Unif.Fail;
    let subst = ref subst in
    for i = 0 to Array.length a1 - 1 do
      subst := variant ~subst:!subst a1.(i) sc1 a2.(i) sc2;
    done;
    !subst

  let are_variant a1 a2 =
    try
      let _ = variant a1 0 a2 1 in
      true
    with Unif.Fail ->
      false

  let weight lits =
    Array.fold_left (fun w lit -> w + weight lit) 0 lits

  let depth lits =
    Array.fold_left (fun d lit -> max d (depth lit)) 0 lits

  let vars lits =
    let set = T.Tbl.create 11 in
    for i = 0 to Array.length lits - 1 do
      add_vars set lits.(i);
    done;
    T.Tbl.to_list set

  let is_ground lits =
    Util.array_forall is_ground lits

  let terms lits =
    Sequence.flatMap terms (Sequence.of_array lits)

  let compact lits =
    lazy (Array.map form_of_lit lits)

  let to_form lits =
    let lits = Array.map form_of_lit lits in
    let lits = Array.to_list lits in
    F.Base.or_ lits

  (** Apply the substitution to the array of literals, with scope *)
  let apply_subst ~renaming ~ord subst lits scope =
    Array.map
      (fun lit -> apply_subst ~renaming ~ord subst lit scope)
      lits

  let fmap ~ord lits f =
    Array.map (fun lit -> fmap ~ord f lit) lits

  (** bitvector of literals that are positive *)
  let pos lits =
    let bv = BV.create ~size:(Array.length lits) false in
    for i = 0 to Array.length lits - 1 do
      if is_pos lits.(i) then BV.set bv i
    done;
    bv

  (** bitvector of literals that are positive *)
  let neg lits =
    let bv = BV.create ~size:(Array.length lits) false in
    for i = 0 to Array.length lits - 1 do
      if is_neg lits.(i) then BV.set bv i
    done;
    bv

  (** Bitvector that indicates which of the literals are maximal *)
  let maxlits ~ord lits =
    let m = Multiset.create_a lits in
    let bv = Multiset.max (fun lit1 lit2 -> compare_partial ~ord lit1 lit2) m in
    bv

  let is_trivial lits =
    Util.array_exists
      (function
      | True -> true
      | False -> false
      | Equation (l, r, true, _) -> T.eq l r
      | Equation (l, r, false, _) -> false
      | Prop (_, _) -> false)
      lits

  (** Convert the lits into a sequence of equations *)
  let to_seq lits =
    Sequence.from_iter
      (fun k ->
        for i = 0 to Array.length lits - 1 do
          match lits.(i) with
          | Equation (l,r,sign,_) -> k (l,r,sign)
          | Prop (p, sign) -> k (p, T.TPTP.true_, sign)
          | True -> k (T.TPTP.true_, T.TPTP.true_, true)
          | False -> k (T.TPTP.true_, T.TPTP.true_, false)
        done)

  let of_forms ~ord forms =
    let forms = Array.of_list forms in
    Array.map (lit_of_form ~ord ) forms

  let to_forms lits =
    Array.to_list (Array.map form_of_lit lits)

  (** {3 High Order combinators} *)

  module Pos = struct
    let at lits pos = match pos with
      | Position.Arg (idx, pos') when idx >= 0 && idx < Array.length lits ->
        Pos.at lits.(idx) pos'
      | _ -> raise Not_found

    let replace ~ord lits ~at ~by = match at with
      | Position.Arg (idx, pos') when idx >= 0 && idx < Array.length lits ->
        lits.(idx) <- Pos.replace ~ord lits.(idx) ~at:pos' ~by
      | _ -> invalid_arg (Util.sprintf "invalid position %a in lits" Position.pp at)
  end

  (** decompose the literal at given position *)
  let get_eqn lits pos = match pos with
    | Position.Arg (idx, pos') when idx < Array.length lits ->
      get_eqn lits.(idx) pos'
    | _ -> invalid_arg "wrong kind of position (needs list of >= 2 elements)"

  (* extract inequation from given position *)
  let get_ineq ~spec lits pos = match pos with
    | Position.Arg (idx, Position.Left _) when idx < Array.length lits ->
      begin try
        let lit = ineq_lit ~spec lits.(idx) in
        lit
      with Not_found ->
        invalid_arg (Util.sprintf "lit %a not an inequation" pp lits.(idx))
      end
    | _ -> invalid_arg "wrong kind of position (needs list of >= 2 elements)"

  let order_instances ~spec lits =
    let rec fold acc i =
      if i = Array.length lits then acc
      else try
        let lit = ineq_lit ~spec lits.(i) in
        let acc = lit.TO.instance :: acc in
        fold acc (i+1)
      with Not_found ->
        fold acc (i+1)
    in
    let l = fold [] 0 in
    Util.list_uniq TO.eq l

  let terms_under_ineq ~instance lits =
    Sequence.from_iter
      (fun k ->
        for i = 0 to Array.length lits - 1 do
          match lits.(i) with
          | Equation (l, r, _, _) -> k l; k r
          | Prop (p, _) ->
            begin try
              let ord_lit = ineq_lit_of ~instance lits.(i) in
              k ord_lit.TO.left; k ord_lit.TO.right
            with Not_found ->
              k p  (* regular predicate *)
            end
          | True
          | False -> ()
        done)

  let fold_lits ~eligible lits acc f =
    let rec fold acc i =
      if i = Array.length lits then acc
      else if not (eligible i lits.(i)) then fold acc (i+1)
      else
        let acc = f acc lits.(i) i in
        fold acc (i+1)
    in
    fold acc 0

  let fold_eqn ?(both=true) ?sign ~eligible lits acc f =
    let sign_ok = match sign with
      | None -> (fun _ -> true)
      | Some sign -> (fun sign' -> sign = sign')
    in
    let rec fold acc i =
      if i = Array.length lits then acc
      else if not (eligible i lits.(i)) then fold acc (i+1)
      else
        let acc = match lits.(i) with
        | Equation (l,r,sign,Comparison.Gt) when sign_ok sign ->
          f acc l r sign Position.(arg i @@ left @@ stop)
        | Equation (l,r,sign,Comparison.Lt) when sign_ok sign ->
          f acc r l sign Position.(arg i @@ right @@ stop)
        | Equation (l,r,sign,_) when sign_ok sign ->
          if both
          then (* visit both sides of the equation *)
            let acc = f acc r l sign Position.(arg i @@ right @@ stop) in
            f acc l r sign Position.(arg i @@ left @@ stop)
          else (* only visit one side (arbitrary) *)
            f acc l r sign Position.(arg i @@ left @@ stop)
        | Prop (p, sign) when sign_ok sign ->
          f acc p T.TPTP.true_ sign Position.(arg i @@ left @@ stop)
        | Prop _
        | Equation _
        | True
        | False -> acc
        in fold acc (i+1)
    in fold acc 0

  let fold_ineq ~spec ~eligible lits acc f =
    let rec fold acc i =
      if i = Array.length lits then acc
      else if not (eligible i lits.(i)) then fold acc (i+1)
      else
        let acc =
          try
            let lit = ineq_lit ~spec lits.(i) in
            let pos = Position.(arg i @@ left @@ stop) in
            f acc lit pos
          with Not_found ->
            acc
        in
        fold acc (i+1)
    in fold acc 0

  (* TODO: new arguments *)
  let fold_terms ?(vars=false) ~(which : [< `Max|`One|`Both]) ~subterms ~eligible lits acc f =
    let rec fold acc i =
      if i = Array.length lits
        then acc
      else if not (eligible i lits.(i))
        then fold acc (i+1)   (* ignore lit *)
      else
        let acc = match lits.(i), which with
        | Equation (l,r,sign,Comparison.Gt), (`Max | `One) ->
          if subterms
            then T.all_positions ~vars ~pos:Position.(arg i @@ left @@ stop) l acc f
            else f acc l Position.(arg i @@ left @@ stop)
        | Equation (l,r,sign,Comparison.Lt), (`Max | `One) ->
          if subterms
            then T.all_positions ~vars ~pos:Position.(arg i @@ right @@ stop) r acc f
            else f acc r Position.(arg i @@ left @@ stop)
        | Equation (l,r,sign,_), `One ->
          (* only visit one side (arbitrary) *)
          f acc l Position.(arg i @@ left @@ stop)
        | Equation (l,r,sign,(Comparison.Eq | Comparison.Incomparable)), `Max ->
          (* visit both sides, they are both (potentially) maximal *)
          let acc = f acc l Position.(arg i @@ left @@ stop) in
          f acc r Position.(arg i @@ right @@ stop)
        | Equation (l,r,sign,_), `Both ->
          (* visit both sides of the equation *)
          if subterms
            then
              let acc = T.all_positions ~vars
                ~pos:Position.(arg i @@ left @@ stop) l acc f in
              let acc = T.all_positions ~vars
                ~pos:Position.(arg i @@ right @@ stop) r acc f in
              acc
            else
              let acc = f acc l Position.(arg i @@ left @@ stop) in
              let acc = f acc r Position.(arg i @@ right @@ stop) in
              acc
        | Prop (p, _), _ ->
          if subterms
            then T.all_positions ~vars ~pos:Position.(arg i @@ left @@ stop) p acc f
            else f acc p Position.(arg i @@ left @@ stop)
        | True, _
        | False, _ -> acc
        in fold acc (i+1)
    in fold acc 0

  let symbols ?(init=Symbol.Set.empty) lits =
    Array.fold_left
      (fun set lit -> Symbol.Set.union set (symbols lit))
      init lits

  (** {3 IO} *)

  let pp buf lits = 
    Util.pp_arrayi ~sep:" | "
      (fun buf i lit -> Printf.bprintf buf "%a" pp lit)
      buf lits

  let pp_tstp buf lits =
    Util.pp_arrayi ~sep:" | "
      (fun buf i lit -> Printf.bprintf buf "%a" pp_tstp lit)
      buf lits

  let to_string a = Util.on_buffer pp a

  let fmt fmt lits =
    Format.pp_print_string fmt (to_string lits)
end

(** {2 Special kinds of array} *)

(** Recognized whether the clause is a Range-Restricted Horn clause *)
let is_RR_horn_clause lits =
  let bv = Arr.pos lits in
  match BV.to_list bv with
  | [i] ->
    (* single positive lit, check variables restrictions, ie all vars
        occur in the head *)
    let vars = vars lits.(i) in
    List.length vars = List.length (Arr.vars lits)
  | _ -> false

(** Recognizes Horn clauses (at most one positive literal) *)
let is_horn lits =
  let bv = Arr.pos lits in
  BV.cardinal bv <= 1

let is_pos_eq lits =
  match lits with
  | [| Equation (l,r,true,_) |] -> Some (l,r)
  | [| Prop(p,true) |] -> Some (p, T.TPTP.true_)
  | [| True |] -> Some (T.TPTP.true_, T.TPTP.true_)
  | _ -> None
