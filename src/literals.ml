
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

(** {1 Array of literals} *)

open Logtk

module Hash = CCHash
module BV = CCBV
module T = FOTerm
module F = Formula.FO
module S = Substs
module Lit = Literal
module TO = Theories.TotalOrder

type scope = Substs.scope
type term = FOTerm.t
type form = Formula.FO.t

type t = Literal.t array

let prof_maxlits = Util.mk_profiler "lits.maxlits"

let eq lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then true else
    Lit.eq lits1.(i) lits2.(i) && check (i+1)
  in
  if Array.length lits1 <> Array.length lits2
    then false
    else check 0

let eq_com lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then true else
    Lit.eq_com lits1.(i) lits2.(i) && check (i+1)
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

let hash_fun lits h =
  Hash.array_ Lit.hash_fun lits h

let hash lits = Hash.apply hash_fun lits

let variant ?(subst=S.empty) a1 sc1 a2 sc2 k =
  let rec iter2 subst i =
    if i = Array.length a1
      then k subst
      else
        Lit.variant ~subst a1.(i) sc1 a2.(i) sc2
          (fun subst -> iter2 subst (i+1))
  in
  if Array.length a1 = Array.length a2
    then iter2 subst 0

let are_variant a1 a2 =
  not (Sequence.is_empty (variant a1 0 a2 1))

let weight lits =
  Array.fold_left (fun w lit -> w + Lit.weight lit) 0 lits

let depth lits =
  Array.fold_left (fun d lit -> max d (Lit.depth lit)) 0 lits

let vars lits =
  let set = T.Tbl.create 11 in
  for i = 0 to Array.length lits - 1 do
    Lit.add_vars set lits.(i);
  done;
  T.Tbl.fold (fun t () acc -> t::acc) set []

let is_ground lits =
  CCArray.for_all Lit.is_ground lits

let to_form lits =
  let lits = Array.map Lit.Conv.to_form lits in
  let lits = Array.to_list lits in
  F.Base.or_ lits

(** Apply the substitution to the array of literals, with scope *)
let apply_subst ~renaming subst lits scope =
  Array.map
    (fun lit -> Lit.apply_subst ~renaming subst lit scope)
    lits

let map f lits =
  Array.map (fun lit -> Lit.map f lit) lits

(** bitvector of literals that are positive *)
let pos lits =
  let bv = BV.create ~size:(Array.length lits) false in
  for i = 0 to Array.length lits - 1 do
    if Lit.is_pos lits.(i) then BV.set bv i
  done;
  bv

(** bitvector of literals that are positive *)
let neg lits =
  let bv = BV.create ~size:(Array.length lits) false in
  for i = 0 to Array.length lits - 1 do
    if Lit.is_neg lits.(i) then BV.set bv i
  done;
  bv

(** Multiset of literals, with their index *)
module MLI = Multiset.Make(struct
  type t = Lit.t * int
  let compare (l1,i1)(l2,i2) =
    if i1=i2 then Lit.compare l1 l2 else Pervasives.compare i1 i2
end)

let _compare_lit_with_idx ~ord (lit1,i1) (lit2,i2) =
  if i1=i2
    then Comparison.Eq (* ignore collisions *)
    else Lit.Comp.compare ~ord lit1 lit2

let _to_multiset_with_idx lits =
  CCArray.foldi
    (fun acc i x -> MLI.add acc (x,i))
    MLI.empty lits

let maxlits_l ~ord lits =
  Util.enter_prof prof_maxlits;
  let m = _to_multiset_with_idx lits in
  let max = MLI.max_seq (_compare_lit_with_idx ~ord) m
    |> Sequence.map2 (fun x _ -> x)
    |> Sequence.to_list
  in
  Util.exit_prof prof_maxlits;
  max

let maxlits ~ord lits =
  Util.enter_prof prof_maxlits;
  let m = _to_multiset_with_idx lits in
  let max = MLI.max_seq (_compare_lit_with_idx ~ord) m
    |> Sequence.map2 (fun x _ -> snd x)
    |> Sequence.to_list
    |> BV.of_list
  in
  Util.exit_prof prof_maxlits;
  max

let is_max ~ord lits =
  (*
  let max = maxlits_l ~ord lits in
  fun i -> List.exists (fun (_,j) -> i=j) max
  *)
  let m = _to_multiset_with_idx lits in
  fun i ->
    let lit = lits.(i) in
    MLI.is_max (_compare_lit_with_idx ~ord) (lit,i) m

let is_trivial lits =
  CCArray.exists Lit.is_trivial lits

module Seq = struct
  let terms a =
    Sequence.of_array a |> Sequence.flatMap Lit.Seq.terms
  let abstract lits =
    Sequence.of_array lits |> Sequence.map Lit.Seq.abstract
end

(** {3 High Order combinators} *)

module Pos = struct
  let _fail_lits lits pos =
    let msg = Util.sprintf "invalid position %a in lits [%a]"
      Position.pp pos (Util.pp_array Lit.pp) lits
    in invalid_arg msg

  let _fail_pos pos =
    let msg = Util.sprintf "invalid literal-array position %a" Position.pp pos in
    invalid_arg msg

  let at lits pos = match pos with
    | Position.Arg (idx, pos') when idx >= 0 && idx < Array.length lits ->
      Lit.Pos.at lits.(idx) pos'
    | _ -> _fail_lits lits pos

  let lit_at lits pos = match pos with
    | Position.Arg (i, pos') when i >= 0 && i < Array.length lits ->
      lits.(i), pos'
    | _ -> _fail_lits lits pos

  let replace lits ~at ~by = match at with
    | Position.Arg (idx, pos') when idx >= 0 && idx < Array.length lits ->
      lits.(idx) <- Lit.Pos.replace lits.(idx) ~at:pos' ~by
    | _ -> _fail_lits lits at

  let idx = function
    | Position.Arg(i, _) -> i
    | p -> _fail_pos p

  let tail = function
    | Position.Arg (_, pos') -> pos'
    | p -> _fail_pos p

  let cut = function
    | Position.Arg (i, pos') -> i, pos'
    | p -> _fail_pos p
end

module Conv = struct
  let of_forms ?hooks forms =
    let forms = Array.of_list forms in
    Array.map (Lit.Conv.of_form ?hooks) forms

  let to_forms ?hooks lits =
    Array.to_list (Array.map (Lit.Conv.to_form ?hooks) lits)
end

module View = struct
  let get_eqn lits pos = match pos with
    | Position.Arg (idx, pos') when idx < Array.length lits ->
      Lit.View.get_eqn lits.(idx) pos'
    | _ -> None

  let get_ineq lits pos = match pos with
    | Position.Arg (idx, _) when idx < Array.length lits ->
      Lit.View.get_ineq lits.(idx)
    | _ -> None

  let get_arith lits pos = match pos with
    | Position.Arg (idx, pos') when idx < Array.length lits ->
      Lit.View.focus_arith lits.(idx) pos'
    | _ -> None

  let get_subseteq lits pos = match pos with
    | Position.Arg (idx, pos') when idx < Array.length lits ->
      Lit.View.get_subseteq lits.(idx)
    | _ -> None

  let _unwrap2 ~msg f x y = match f x y with
    | Some z -> z
    | None -> invalid_arg msg

  let get_eqn_exn =
    _unwrap2 ~msg:"get_eqn: improper position" get_eqn

  let get_ineq_exn =
    _unwrap2 ~msg:"get_ineq: improper position" get_ineq

  let get_arith_exn =
    _unwrap2 ~msg:"get_arith: improper position" get_arith

  let get_subseteq_exn =
    _unwrap2 ~msg:"get_subseteq: improper position" get_subseteq
end

let order_instances lits =
  let l = Array.fold_left
    (fun acc lit ->
      match Lit.View.get_ineq lit with
      | None -> acc
      | Some olit -> olit.TO.order :: acc)
    [] lits
  in
  CCList.Set.uniq ~eq:TO.eq l

let terms_under_ineq ~instance lits =
  Sequence.from_iter
    (fun k ->
      for i = 0 to Array.length lits - 1 do
        match lits.(i) with
        | Lit.Ineq olit ->
            if TO.eq (olit.TO.order) instance
            then (k olit.TO.left; k olit.TO.right)
        | Lit.Equation (l, r, _) -> k l; k r
        | Lit.Prop (p, _) -> k p
        | Lit.Arith _
        | Lit.Subseteq _
        | Lit.True
        | Lit.False -> ()
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

let fold_eqn ?(both=true) ?sign ~ord ~eligible lits acc f =
  let sign_ok s = match sign with
    | None -> true
    | Some sign -> sign = s
  in
  let rec fold acc i =
    if i = Array.length lits then acc
    else if not (eligible i lits.(i)) then fold acc (i+1)
    else
      let acc = match lits.(i) with
      | Lit.Equation (l,r,sign) when sign_ok sign ->
        begin match Ordering.compare ord l r with
        | Comparison.Gt ->
          f acc l r sign Position.(arg i @@ left @@ stop)
        | Comparison.Lt ->
          f acc r l sign Position.(arg i @@ right @@ stop)
        | Comparison.Eq
        | Comparison.Incomparable ->
          if both
          then
            (* visit both sides of the equation *)
            let acc = f acc r l sign Position.(arg i @@ right @@ stop) in
            f acc l r sign Position.(arg i @@ left @@ stop)
          else
            (* only one side *)
            f acc l r sign Position.(arg i @@ left @@ stop)
        end
      | Lit.Prop (p, sign) when sign_ok sign ->
        f acc p T.TPTP.true_ sign Position.(arg i @@ left @@ stop)
      | Lit.Prop _
      | Lit.Equation _
      | Lit.Ineq _
      | Lit.Arith _
      | Lit.Subseteq _
      | Lit.True
      | Lit.False -> acc
      in fold acc (i+1)
  in fold acc 0

let fold_ineq ~eligible lits acc f =
  let rec fold acc i =
    if i = Array.length lits then acc
    else if not (eligible i lits.(i)) then fold acc (i+1)
    else
      let acc = match Lit.View.get_ineq lits.(i) with
      | None -> acc
      | Some olit ->
          let pos = Position.(arg i stop) in
          f acc olit pos
      in
      fold acc (i+1)
  in fold acc 0

let fold_arith ~eligible lits acc f =
  let rec fold acc i =
    if i = Array.length lits then acc
    else if not (eligible i lits.(i)) then fold acc (i+1)
    else
      let acc = match Lit.View.get_arith lits.(i) with
      | None -> acc
      | Some x ->
          let pos = Position.(arg i stop) in
          f acc x pos
      in
      fold acc (i+1)
  in fold acc 0

let fold_arith_terms ~eligible ~which ~ord lits acc f =
  let module M = Monome in let module MF = Monome.Focus in
  fold_arith ~eligible lits acc
    (fun acc a_lit pos ->
      (* do we use the given term? *)
      let do_term =
        match which with
        | `All -> (fun _ -> true)
        | `Max ->
          let max_terms = ArithLit.max_terms ~ord a_lit in
          fun t -> CCList.Set.mem ~eq:T.eq t max_terms
      in
      ArithLit.Focus.fold_terms ~pos a_lit acc
        (fun acc foc_lit pos ->
          let t = ArithLit.Focus.term foc_lit in
          if do_term t
            then f acc t foc_lit pos
            else acc)
    )

let fold_terms ?(vars=false) ~(which : [< `All|`Max])
~ord ~subterms ~eligible lits acc f =
  let rec fold acc i =
    if i = Array.length lits
      then acc
    else if not (eligible i lits.(i))
      then fold acc (i+1)   (* ignore lit *)
    else
      let acc = Lit.fold_terms ~position:Position.(arg i stop)
        ~vars ~which ~ord ~subterms lits.(i) acc f in
      fold acc (i+1)
  in fold acc 0

let symbols ?(init=Symbol.Set.empty) lits =
  Sequence.of_array lits
    |> Sequence.map Lit.Seq.symbols
    |> Sequence.fold Symbol.Seq.add_set Symbol.Set.empty

let fold_subseteq ?sign ~eligible lits acc f =
  let sign_ok = match sign with
    | None -> (fun _ -> true)
    | Some sign -> (fun sign' -> sign' = sign)
  in
  let rec fold acc i =
    if i = Array.length lits then acc
    else if not(eligible i lits.(i)) then fold acc (i+1)
    else match Literal.View.get_subseteq (lits.(i)) with
      | None -> fold acc (i+1)
      | Some (_,_,_,s) ->
        if sign_ok s then
          let pos = Position.(arg i stop) in
          fold (f acc lits.(i) pos) (i+1)
        else
          fold acc (i+1)
  in fold acc 0

let fold_subseteq_terms ?sign ~eligible ~ord lits acc f =
  fold_subseteq ?sign ~eligible lits acc
  (fun acc lit position ->
    Lit.fold_terms ~vars:true ~position ~which:`All ~ord ~subterms:false lit acc f)


(** {3 IO} *)

let pp buf lits =
  Util.pp_arrayi ~sep:" | "
    (fun buf i lit -> Printf.bprintf buf "%a" Lit.pp lit)
    buf lits

let pp_tstp buf lits =
  Util.pp_arrayi ~sep:" | "
    (fun buf i lit -> Printf.bprintf buf "%a" Lit.pp_tstp lit)
    buf lits

let to_string a = Util.on_buffer pp a

let fmt fmt lits =
  Format.pp_print_string fmt (to_string lits)

(** {2 Special kinds of array} *)

(** Recognized whether the clause is a Range-Restricted Horn clause *)
let is_RR_horn_clause lits =
  let bv = pos lits in
  match BV.to_list bv with
  | [i] ->
    (* single positive lit, check variables restrictions, ie all vars
        occur in the head *)
    let hd_vars = Lit.vars lits.(i) in
    List.length hd_vars = List.length (vars lits)
  | _ -> false

(** Recognizes Horn clauses (at most one positive literal) *)
let is_horn lits =
  let bv = pos lits in
  BV.cardinal bv <= 1

let is_pos_eq lits =
  match lits with
  | [| Lit.Equation (l,r,true) |] -> Some (l,r)
  | [| Lit.Prop(p,true) |] -> Some (p, T.TPTP.true_)
  | [| Lit.True |] -> Some (T.TPTP.true_, T.TPTP.true_)
  | _ -> None
