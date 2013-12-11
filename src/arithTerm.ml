
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

(** {1 Arithmetic Terms} *)

open Logtk

module S = Symbol

let prof_arith_simplify = Util.mk_profiler "arith.simplify"

open FOTerm

(** {2 Utils} *)

let is_arith_ty ty =
  Type.eq ty Type.int || Type.eq ty Type.rat || Type.eq ty Type.real

(** {2 Terms} *)

let arith_kind t = match t.term with
  | Node ((S.Int _ | S.Rat _ | S.Real _), _, []) -> `Const
  | Node (s, _, _) when Symbol.Arith.is_arith s -> `Expr
  | Var _
  | BoundVar _ ->
    if is_arith_ty t.ty then `Var else `Not
  | _ -> `Not

let is_arith t = match arith_kind t with
  | `Const | `Expr | `Var -> true
  | `Not -> false

let is_arith_const t = match arith_kind t with
  | `Const -> true
  | `Var | `Expr | `Not -> false

let is_compound_arith t = match arith_kind t with
  | `Expr -> true
  | `Var | `Const | `Not -> false

(* evaluator for arith *)
let __ev =
  let e = Evaluator.FO.create () in
  Evaluator.FO.with_arith e;
  e

let mk_sum t1 t2 = Evaluator.FO.app ~tyargs:[ty t1] __ev S.Arith.sum [t1; t2]
let mk_difference t1 t2 = Evaluator.FO.app ~tyargs:[ty t1] __ev S.Arith.difference [t1; t2]
let mk_product t1 t2 = Evaluator.FO.app ~tyargs:[ty t1] __ev S.Arith.product [t1; t2]
let mk_quotient t1 t2 = Evaluator.FO.app ~tyargs:[ty t1] __ev S.Arith.quotient [t1; t2]
let mk_uminus t = Evaluator.FO.app ~tyargs:[ty t] __ev S.Arith.uminus [t]

let rec sum_list l = match l with
  | [] -> failwith "Arith.sum_list: got empty list"
  | [x] -> x
  | x::l' -> mk_sum x (sum_list l')

let mk_remainder_e t s =
  assert (Type.eq (ty t) Type.int);
  mk_node ~tyargs:[Type.int] S.Arith.remainder_e [t; mk_const s]

let mk_quotient_e t s =
  assert (Type.eq (ty t) Type.int);
  mk_node ~tyargs:[Type.int] S.Arith.quotient_e [t; mk_const s]

let mk_less t1 t2 =
  if is_arith_const t1 && is_arith_const t2
    then if S.Arith.Op.less (head t1) (head t2)
      then true_term
      else false_term
    else mk_node ~tyargs:[ty t1] S.Arith.less [t1; t2]

let mk_lesseq t1 t2 =
  if is_arith_const t1 && is_arith_const t2
    then if S.Arith.Op.lesseq (head t1) (head t2)
      then true_term
      else false_term
    else mk_node ~tyargs:[ty t1] S.Arith.lesseq [t1; t2]

let mk_greater t1 t2 = mk_less t2 t1
let mk_greatereq t1 t2 = mk_lesseq t2 t1

let extract_subterms t =
  (* recursive function that gathers terms into set *)
  let rec gather set t = match t.term with
  | Var _
  | BoundVar _ -> Tbl.replace set t ()
  | Node (s, _, []) when S.is_numeric s -> ()
  | Node (s, _, l) when S.Arith.is_arith s ->
    List.iter (gather set) l
  | Node _ -> Tbl.replace set t ()
  in
  if is_arith t
    then
      let set = Tbl.create 5 in
      let () = gather set t in
      Tbl.to_list set
    else []

let _var_occur_strict ~var t =
  not (eq var t) && var_occurs var t

let shielded ~var t = match extract_subterms t with
  | [] -> _var_occur_strict ~var t
  | l -> List.exists (fun t' -> _var_occur_strict ~var t') l

let flag_simplified = new_flag ()

let simplify t =
  if get_flag flag_simplified t
    then t
    else begin
      Util.enter_prof prof_arith_simplify;
      let t' = Evaluator.FO.eval __ev t in
      set_flag flag_simplified t' true;
      Util.exit_prof prof_arith_simplify;
      t'
    end

(** {2 Formulas} *)

module Form = struct
  let simplify f =
    FOFormula.map_leaf
      (fun f -> match f.FOFormula.form with
        | FOFormula.Atom p ->
          let p' = simplify p in
          FOFormula.mk_atom p'
        | FOFormula.Equal (t1, t2) ->
          let t1' = simplify t1 in
          let t2' = simplify t2 in
          if is_arith_const t1'  && is_arith_const t2'
            then if eq t1' t2'
              then FOFormula.mk_true
              else FOFormula.mk_false
            else FOFormula.mk_eq t1' t2'
        | _ -> f)
      f
end
