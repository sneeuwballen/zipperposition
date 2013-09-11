
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
open Comparison.Infix

module T = Term
module F = Formula
module S = Substs
module BV = Bitvector

type t =
  | Equation of Term.t * Term.t * bool * Comparison.t
  | Prop of Term.t * bool
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
        let c = T.compare l1 l2 in
        if c <> 0 then c else
          let c = T.compare r1 r2 in
          if c <> 0 then c else
            Pervasives.compare sign1 sign2
  | Prop (p1, sign1), Prop(p2, sign2) ->
    let c = T.compare p1 p2 in
    if c <> 0 then c else Pervasives.compare sign1 sign2
  | True, True
  | False, False -> 0
  | _, _ -> __to_int l1 - __to_int l2

let to_multiset lit = match lit with
  | Equation (l, r, true, _) -> [l; r]
  | Equation (l, r, false, _) -> [l; l; r; r]
  | Prop (p, true) -> [p; T.true_term]
  | Prop (p, false) -> [p; p; T.true_term; T.true_term]
  | True -> [T.true_term; T.true_term]
  | False -> [T.true_term; T.true_term; T.true_term; T.true_term]

let compare_partial ~ord l1 l2 =
  let m1 = to_multiset l1 in
  let m2 = to_multiset l2 in
  Ordering.Multiset.compare (Ordering.compare ord) m1 m2

let hash lit = match lit with
  | Equation (l, r, sign, o) ->
    Hash.hash_int3 (22 + Comparison.to_total o) l.T.tag r.T.tag
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
  | Prop _ -> Gt
  | True
  | False -> Eq

let check_type a b =
  let ok = not (T.has_type a) || not (T.has_type b) || T.compatible_type a b in
  assert ok

(* primary constructor *)
let mk_lit ~ord a b sign =
  match a, b with
  | _ when a == b -> if sign then True else False
  | _ when a == T.true_term && b == T.false_term -> if sign then False else True
  | _ when a == T.false_term && b == T.true_term -> if sign then False else True
  | _ when a == T.true_term -> Prop (b, sign)
  | _ when b == T.true_term -> Prop (a, sign)
  | _ when a == T.false_term -> Prop (b, not sign)
  | _ when b == T.false_term -> Prop (a, not sign)
  | _ ->
    check_type a b;
    Equation (a, b, sign, Ordering.compare ord a b)

let mk_eq ~ord a b = mk_lit ~ord a b true

let mk_neq ~ord a b = mk_lit ~ord a b false

let mk_prop ~ord p sign = mk_lit ~ord p T.true_term sign

let mk_true ~ord p = mk_prop ~ord p true

let mk_false ~ord p = mk_prop ~ord p false

let apply_subst ?(recursive=true) ?renaming ~ord subst lit scope =
  match lit with
  | Equation (l,r,sign,_) ->
    let new_l = S.apply ~recursive ?renaming subst l scope
    and new_r = S.apply ~recursive ?renaming subst r scope in
    mk_lit ~ord new_l new_r sign
  | Prop (p, sign) ->
    let p' = S.apply ~recursive ?renaming subst p scope in
    mk_prop ~ord p' sign
  | True
  | False -> lit

let reord ~ord lit =
  match lit with
  | Equation (l,r,sign,_) -> mk_lit ~ord l r sign
  | Prop _
  | True
  | False -> lit

let lit_of_form ~ord f =
  let f = F.simplify f in
  match f.F.form with
  | F.True -> True
  | F.False -> False
  | F.Atom a -> mk_true ~ord a
  | F.Not {F.form=F.Atom a} -> mk_false ~ord a
  | F.Equal (l,r) -> mk_eq ~ord l r
  | F.Not {F.form=F.Equal (l,r)} -> mk_neq ~ord l r
  | _ -> failwith (Util.sprintf "not a literal: %a" F.pp f)

let to_tuple = function
  | Equation (l,r,sign,_) -> l,r,sign
  | Prop (p,sign) -> p, T.true_term, sign
  | True -> T.true_term, T.true_term, true
  | False -> T.true_term, T.true_term, false

let form_of_lit lit = match lit with
  | Equation (l, r, true, _) -> F.mk_eq l r
  | Equation (l, r, false, _) -> F.mk_neq l r
  | Prop (p, true) -> F.mk_atom p
  | Prop (p, false) -> F.mk_not (F.mk_atom p)
  | True -> F.mk_true
  | False -> F.mk_false

let term_of_lit lit = F.to_term (form_of_lit lit)

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
    mk_prop ~ord p' sign
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
  let set = T.THashSet.create () in
  add_vars set lit;
  T.THashSet.to_list set

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

let infer_type ctx lit =
  match lit with
  | Equation (l,r,_,_) ->
    TypeInference.constrain_term_term ctx l r
  | Prop (p,_) ->
    TypeInference.constrain_term_type ctx p Type.o
  | True
  | False -> ()

let signature ?(signature=Signature.empty) lit =
  let ctx = TypeInference.Ctx.of_signature signature in
  infer_type ctx lit;
  TypeInference.Ctx.to_signature ctx

let apply_subst_list ?(recursive=true) ?renaming ~ord subst lits scope =
  List.map
    (fun lit -> apply_subst ~recursive ?renaming ~ord subst lit scope)
    lits

(** {2 IO} *)

let pp buf lit =
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
  | Prop (p, true) -> T.pp_tstp buf p
  | Prop (p, false) -> Printf.bprintf buf "~ %a" T.pp_tstp p
  | True -> Buffer.add_string buf "$true"
  | False -> Buffer.add_string buf "$false"
  | Equation (l, r, true, _) ->
    Printf.bprintf buf "%a = %a" T.pp_tstp l T.pp_tstp r
  | Equation (l, r, false, _) ->
    Printf.bprintf buf "%a != %a" T.pp_tstp l T.pp_tstp r

let to_string t = Util.on_buffer pp t

let fmt fmt lit =
  Format.pp_print_string fmt (to_string lit)

let bij ~ord =
  let open Bij in
  map
    ~inject:(fun lit -> to_tuple lit)
    ~extract:(fun (l,r,sign) -> mk_lit ~ord l r sign)
    (triple T.bij T.bij bool_)

(** {2 Arrays of literals} *)

module Arr = struct
  let eq lits1 lits2 =
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
      (fun h lit -> Hash.hash_int2 h (hash lit))
      13 lits

  let weight lits =
    Array.fold_left (fun w lit -> w + weight lit) 0 lits

  let depth lits =
    Array.fold_left (fun d lit -> max d (depth lit)) 0 lits

  let vars lits =
    let set = T.THashSet.create () in
    for i = 0 to Array.length lits - 1 do
      add_vars set lits.(i);
    done;
    T.THashSet.to_list set

  let is_ground lits =
    Util.array_forall is_ground lits

  let to_form lits =
    let lits = Array.map form_of_lit lits in
    let lits = Array.to_list lits in
    F.mk_or lits

  (** Apply the substitution to the array of literals, with scope *)
  let apply_subst ?(recursive=true) ?renaming ~ord subst lits scope =
    Array.map
      (fun lit -> apply_subst ~recursive ?renaming ~ord subst lit scope)
      lits

  (** bitvector of literals that are positive *)
  let pos lits =
    let bv = ref BV.empty in
    for i = 0 to Array.length lits - 1 do
      if is_pos lits.(i) then bv := BV.set !bv i
    done;
    !bv

  (** bitvector of literals that are positive *)
  let neg lits =
    let bv = ref BV.empty in
    for i = 0 to Array.length lits - 1 do
      if is_neg lits.(i) then bv := BV.set !bv i
    done;
    !bv

  (** Bitvector that indicates which of the literals are maximal *)
  let maxlits ~ord lits =
    let n = Array.length lits in
    (* at the beginning, all literals are potentially maximal *)
    let bv = ref (BV.make n) in
    for i = 0 to n-1 do
      (* i-th lit is already known not to be max? *)
      if not (BV.get !bv i) then () else
      for j = i+1 to n-1 do
        if not (BV.get !bv j) then () else
        match compare_partial ~ord lits.(i) lits.(j) with
        | Incomparable
        | Eq -> ()     (* no further information about i-th and j-th *)
        | Gt -> bv := BV.clear !bv j  (* j-th cannot be max *)
        | Lt -> bv := BV.clear !bv i  (* i-th cannot be max *)
      done;
    done;
    (* return bitvector *)
    !bv

  (** Convert the lits into a sequence of equations *)
  let to_seq lits =
    Sequence.from_iter
      (fun k ->
        for i = 0 to Array.length lits - 1 do
          match lits.(i) with
          | Equation (l,r,sign,_) -> k (l,r,sign)
          | Prop (p, sign) -> k (p, T.true_term, sign)
          | True -> k (T.true_term, T.true_term, true)
          | False -> k (T.true_term, T.true_term, false)
        done)

  let of_forms ~ord forms =
    let forms = Array.of_list forms in
    Array.map (lit_of_form ~ord ) forms

  let to_forms lits =
    Array.to_list (Array.map form_of_lit lits)

  let infer_type ctx lits =
    Array.iter (infer_type ctx) lits

  let signature ?(signature=Signature.empty) lits =
    let ctx = TypeInference.Ctx.of_signature signature in
    infer_type ctx lits;
    TypeInference.Ctx.to_signature ctx

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

  let bij ~ord =
    let open Bij in
    map
      ~inject:(fun a -> Array.to_list a)
      ~extract:(fun l -> Array.of_list l)
      (list_ (bij ~ord))
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
  BV.size bv <= 1

let is_pos_eq lits =
  match lits with
  | [| Equation (l,r,true,_) |] -> Some (l,r)
  | [| Prop(p,true) |] -> Some (p, T.true_term)
  | [| True |] -> Some (T.true_term, T.true_term)
  | _ -> None
