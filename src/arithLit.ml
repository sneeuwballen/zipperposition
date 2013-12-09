
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

(** {1 View a Literal as an arithmetic Literal}. *)

open Logtk

module T = FOTerm
module AT = ArithTerm
module S = Symbol
module M = Monome
module Literals = Literal.Arr

let prof_arith_extract = Util.mk_profiler "arith.extract"

let is_arith lit = match lit with
| Literal.Equation (l, r, _, _) -> AT.is_arith l || AT.is_arith r
| Literal.Prop (p, _) -> AT.is_arith p
| Literal.True
| Literal.False -> false

let mk_less t1 t2 =
  Literal.mk_true (AT.mk_less t1 t2)

let mk_lesseq t1 t2 =
  Literal.mk_true (AT.mk_lesseq t1 t2)

let mk_eq ~ord t1 t2 =
  if AT.is_arith_const t1 && AT.is_arith_const t2
    then if T.eq t1 t2
      then Literal.mk_tauto
      else Literal.mk_absurd
    else Literal.mk_eq ~ord t1 t2

let mk_neq ~ord t1 t2 =
  if AT.is_arith_const t1 && AT.is_arith_const t2
    then if T.eq t1 t2
      then Literal.mk_absurd
      else Literal.mk_tauto
    else Literal.mk_neq ~ord t1 t2

let _uminus = S.Arith.Op.uminus

(** Comparison operator *)
type op =
  | Eq
  | Neq
  | Lt
  | Leq

let _pp_op buf op = match op with
  | Eq -> Buffer.add_string buf "="
  | Neq -> Buffer.add_string buf "≠"
  | Lt -> Buffer.add_string buf "<"
  | Leq -> Buffer.add_string buf "≤"

let _mk_op ~ord op l r = match op with
  | Eq -> mk_eq ~ord l r
  | Neq -> mk_neq ~ord l r
  | Lt -> mk_less l r
  | Leq -> mk_lesseq l r

(** Side of a particular term w.r.t the comparison operator *)
type side =
  | Left
  | Right

let flip = function
  | Left -> Right
  | Right -> Left

(** {3 Canonical Representation} *)

module Canonical = struct
  type t =
  | True
  | False
  | Compare of op * Monome.t * Monome.t

  let monomes = function
    | True
    | False -> raise (Invalid_argument "Canonical.monomes")
    | Compare (_, m1, m2) -> m1, m2

  let op = function
    | True | False -> raise (Invalid_argument "Canonical.op")
    | Compare (op, _, _) -> op

  let pp buf lit = match lit with
  | True -> Buffer.add_string buf "true"
  | False -> Buffer.add_string buf "false"
  | Compare (op, m1, m2) ->
    Printf.bprintf buf "%a %a %a" M.pp m1 _pp_op op M.pp m2

  let to_string = Util.on_buffer pp

  let _extract lit =
    (* extract literal from (l=r | l!=r) *)
    let extract_eqn l r sign =
      let m1 = M.of_term l in
      let m2 = M.of_term r in
      let m = M.difference m1 m2 in
      (* remove denominator, it doesn't matter *)
      let m = M.normalize_wrt_zero m in
      if M.is_const m
      then if M.sign m = 0
        then if sign then True else False
        else if sign then False else True
      else if not (M.has_instances m) && sign
        then False
      else
        let m1, m2 = M.split m in
        if sign
          then Compare(Eq, m1, m2)
          else Compare(Neq, m1, m2)
    (* extract lit from (l <= r | l < r) *)
    and extract_less ~strict l r =
      let m1 = M.of_term l in
      let m2 = M.of_term r in
      let m = M.difference m1 m2 in
      let m = M.normalize_wrt_zero m in
      if M.is_const m then match M.sign m with
        | 0 -> if strict then False else True
        | n when n < 0 -> True
        | _ -> False
      else if strict && Type.eq (M.ty m) Type.int
        then
          let m1, m2 = M.split (M.succ m) in  (* m1 < m2 ---> m1+1 <= m2 *)
          Compare(Leq, m1, m2)
      else
        let m1, m2 = M.split m in
        if strict
          then Compare(Lt, m1, m2)
          else Compare(Leq, m1, m2)
    in
    let extract_le a b = extract_less ~strict:false a b in
    let extract_lt a b = extract_less ~strict:true a b in
    let ans = match lit with
    | Literal.True -> True
    | Literal.False -> False
    | Literal.Equation (l, r, sign, _) ->
      if AT.is_arith l || AT.is_arith r
        then extract_eqn l r sign
        else raise M.NotLinear
    | Literal.Prop ({T.term=T.Node (S.Const ("$less",_), _, [a; b])}, true) ->
      extract_lt a b
    | Literal.Prop ({T.term=T.Node (S.Const ("$less",_), _, [a; b])}, false) ->
      extract_le b a
    | Literal.Prop ({T.term=T.Node (S.Const ("$lesseq",_), _, [a; b])}, true) ->
      extract_le a b
    | Literal.Prop ({T.term=T.Node (S.Const ("$lesseq",_), _, [a; b])}, false) ->
      extract_lt b a
    | Literal.Prop ({T.term=T.Node (S.Const ("$greater",_), _, [a; b])}, true) ->
      extract_lt b a
    | Literal.Prop ({T.term=T.Node (S.Const ("$greater",_), _, [a; b])}, false) ->
      extract_le a b
    | Literal.Prop ({T.term=T.Node (S.Const ("$greatereq",_), _, [a; b])}, true) ->
      extract_le b a
    | Literal.Prop ({T.term=T.Node (S.Const ("$greatereq",_), _, [a; b])}, false) ->
      extract_lt a b
    | Literal.Prop _ -> raise M.NotLinear
    in
    Util.debug 5 "arith extraction of %a gives %a" Literal.pp lit pp ans;
    Util.exit_prof prof_arith_extract;
    ans

  module LitCache = Cache.Replacing(struct
    type t = Literal.t
    let equal = Literal.eq
    let hash = Literal.hash
  end)

  (* cache for literal extraction (because it will be called often
      on the same literals when distinct inference/simplification
      rules are run on a given clause *)
  let __cache = LitCache.create 512

  let extract lit =
    Util.enter_prof prof_arith_extract;
    let ans = LitCache.with_cache __cache _extract lit in
    Util.debug 5 "arith extraction of %a gives %a" Literal.pp lit pp ans;
    Util.exit_prof prof_arith_extract;
    ans

  let extract_opt lit =
    try Some (extract lit)
    with M.NotLinear -> None

  let size lit = match lit with
    | True
    | False -> 0
    | Compare (_, m1, m2) ->
      M.size m1 + M.size m2  (* empty intersection *)

  let to_lit ~ord lit =
    match lit with
    | True -> Literal.mk_tauto
    | False -> Literal.mk_absurd
    | Compare (op, m1, m2) ->
      _mk_op ~ord op (M.to_term m1) (M.to_term m2)

  (* unify non-arith subterms pairwise *)
  let factor lit = match lit with
  | True
  | False -> []
  | Compare (_, m1, m2) ->
    (* all terms occurring immediately under the linear expression *)
    let l = Util.list_union T.eq (M.terms m1) (M.terms m2) in
    let l = Util.list_diagonal l in
    Util.list_fmap
      (fun (t1, t2) ->
        try Some (FOUnif.unification t1 0 t2 0)
        with FOUnif.Fail -> None)
      l

  let eliminate ?fresh_var lit = match lit with
  | True
  | False -> []
  | Compare(Eq, m1, m2) ->
    M.Solve.neq_zero ?fresh_var (M.difference m1 m2)
  | Compare(Neq, m1, m2) ->
    M.Solve.eq_zero ?fresh_var (M.difference m1 m2)
  | Compare(Lt, m1, m2) ->
    M.Solve.leq_zero ?fresh_var (M.difference m2 m1)
  | Compare(Leq, m1, m2) ->
    M.Solve.lt_zero ?fresh_var (M.difference m2 m1)
end

(** {3 Single-term literal} *)

module Single = struct
  type t =
  | True
  | False
  | Compare of op * side * FOTerm.t * Symbol.t

  let pp buf lit = match lit with
  | True -> Buffer.add_string buf "true"
  | False -> Buffer.add_string buf "false"
  | Compare(op,side,t,s) ->
    match side with
    | Left -> Printf.bprintf buf "%a %a %a" T.pp t _pp_op op S.pp s
    | Right -> Printf.bprintf buf "%a %a %a" S.pp s _pp_op op T.pp t

  let to_string = Util.on_buffer pp

  let simplify = function
    | Compare(Lt, Left, t, s) when S.is_int s ->
      (* t < c ----> t <= c - 1 *)
      Compare(Leq, Left, t, S.Arith.Op.prec s)
    | Compare(Lt, Right, t, s) when S.is_int s ->
      (* c < t ----> c+1 <= t  *)
      Compare(Leq, Right, t, S.Arith.Op.succ s)
    | lit -> lit

  let of_canonical lit =
    (* op m: comparison to 0 *)
    let _mk_single op m =
      let is_int = S.is_int m.M.const in
      let const = m.M.const in
      let c, t = match M.to_list m with
        | [c, t] -> c, t
        | _ -> assert false
      in
      (* manage sign. we have (c*t + const) R 0, we can multiply by -1
          if needed, but [swap] is here to remember swapping sides in
          case R is an inequality. *)
      let swap, c, const = if S.Arith.sign c < 0
        then true, _uminus c, _uminus const
        else false, c, const
      in
      (* divide [const] by [c] if possible *)
      let c, const = if S.Arith.Op.divides c const
        then S.Arith.one_of_ty (S.ty c), S.Arith.Op.quotient const c
        else c, const
      in
      match op with
      | Eq when not (S.Arith.is_one c) -> False  (* 3 * t = 7 ----> false *)
      | Eq -> Compare(Eq, Left, t, _uminus const)
      | Neq when not (S.Arith.is_one c) -> True   (* 3 * t != 7 ----> true *)
      | Neq -> Compare(Neq, Left, t, _uminus const)
      | Lt when not (S.Arith.is_one c) ->
        assert is_int;
        if swap
          then
            (* 3 * t > 7 -----> t > 7/3 ---> t >= 3 *)
            let c = S.Arith.Op.(succ (quotient_f (_uminus const) c)) in
            Compare(Leq, Right, t, c)
          else
            (* 3 * t < 7 --->  t < 7/3 ----> t <= 2 *)
            let c = S.Arith.Op.quotient_f (_uminus const) c in
            Compare(Leq, Left, t, c)
      | Lt ->
        if swap
          then Compare(Lt, Right, t, _uminus const)
          else Compare(Lt, Left, t, _uminus const)
      | Leq when not (S.Arith.is_one c)->
        assert is_int;
        if swap
          then
            (* 3 * t >= 7 ---> t >= 7/3 ----> t >= 3 *)
            let c = S.Arith.Op.(succ (quotient_f (_uminus const) c)) in
            Compare(Leq, Right, t, c)
          else
            (* 3 * t <= 7 ---> t <= 7/3 ----> t <= 2 *)
            let c = S.Arith.Op.quotient_f (_uminus const) c in
            Compare(Leq, Left, t, c)
      | Leq ->
        if swap
          then Compare(Leq, Right, t, _uminus const)
          else Compare(Leq, Left, t, _uminus const)
    in
    match lit with
    | Canonical.True
    | Canonical.False -> None
    | Canonical.Compare(op, m1, m2) when M.size m1 + M.size m2 = 1 ->
      let lit = _mk_single op (M.difference m1 m2) in
      Some (simplify lit)
    | Canonical.Compare _ -> None

  let to_lit ~ord lit = match lit with
    | True -> Literal.mk_tauto
    | False -> Literal.mk_absurd
    | Compare(op, side, t, s) ->
      match side with
      | Left -> _mk_op ~ord op t (T.mk_const s)
      | Right -> _mk_op ~ord op (T.mk_const s) t
end

(** {3 Focused literal *)

module Focused = struct
  (** literal with focus on a single term within. The
      term always has a stricly positive coefficient. *)
  type t = {
    side : side;      (* which side of the operator is the term? *)
    op : op;          (* comparison operator *)
    coeff : Symbol.t; (* strictly positive coeff of term *)
    term : FOTerm.t;  (* focused term *)
    same_side : Monome.t;   (* monome on the same side of comparison *)
    other_side : Monome.t;  (* monome on the other side of comparison *)
  }

  let term t = t.term
  let monomes t = t.same_side, t.other_side
  let coeff t = t.coeff
  let op t = t.op

  let ty t = S.ty t.coeff

  let pp buf t = match t.side with
    | Left ->
      Printf.bprintf buf "(%a·%a) + %a %a %a"
        S.pp t.coeff T.pp t.term M.pp t.same_side _pp_op t.op M.pp t.other_side
    | Right ->
      Printf.bprintf buf "%a %a (%a·%a) + %a"
        M.pp t.other_side _pp_op t.op S.pp t.coeff T.pp t.term M.pp t.same_side

  let to_string = Util.on_buffer pp

  let cmp t1 t2 =
    let c = Util.lexicograph_combine
      [ compare t1.side t2.side
      ; compare t1.op t2.op
      ; S.compare t1.coeff t2.coeff
      ; T.compare t1.term t2.term
      ]
    in
    if c = 0
      then Util.lexicograph M.compare
        [t1.same_side; t1.other_side] [t2.same_side; t2.other_side]
      else c

  let to_lit ~ord l = match l.side with
    | Left ->
      _mk_op ~ord l.op
        (AT.mk_sum (AT.mk_product (T.mk_const l.coeff) l.term) (M.to_term l.same_side))
        (M.to_term l.other_side)
    | Right ->
      _mk_op ~ord l.op
        (M.to_term l.other_side)
        (AT.mk_sum (AT.mk_product (T.mk_const l.coeff) l.term) (M.to_term l.same_side))

  let product s t =
    let side = match S.Arith.sign s with
      | 0 -> raise (Invalid_argument "product by 0")
      | n when n < 0 -> flip t.side  (* multiply by negative constant -> flip inequality *)
      | _ -> t.side
    in
    let s = S.Arith.Op.abs s in
    { t with
      side;
      coeff=S.Arith.Op.product s t.coeff;
      same_side=M.product t.same_side s;
      other_side=M.product t.other_side s;
    }

  let of_canonical ~ord e =
    (* extract maximal terms from list *)
    let max_terms m =
      let terms = Multiset.create (M.terms m) in
      let bv = Multiset.max (Ordering.compare ord) terms in
      BV.select bv (Multiset.to_array terms)
    in
    match e with
    | Canonical.True
    | Canonical.False -> []
    | _ ->
      let m1, m2 = Canonical.monomes e in
      let op = Canonical.op e in
      List.map
        (fun term ->
          let coeff = M.find m1 term in
          let m1' = M.remove m1 term in
          { term; side=Left; op; coeff; same_side=m1'; other_side=m2; })
        (max_terms m1)
      @
      List.map
        (fun term ->
          let coeff = M.find m2 term in
          let m2' = M.remove m2 term in
          { term; side=Right; op; coeff; same_side=m2'; other_side=m1; })
        (max_terms m2)

  let scale l1 l2 =
    let lcm = S.Arith.Op.lcm l1.coeff l2.coeff in
    (* we find d1 and d2 such that
      l1.coeff * d1 = lcm, and  l2.coeff * d2 = lcm,
       therefore to scale we return l1 * d1, and l2 * d2 *)
    product (S.Arith.Op.quotient lcm l1.coeff) l1,
    product (S.Arith.Op.quotient lcm l2.coeff) l2
end

(** {3 High level operations} *)

let is_trivial lit =
  match Canonical.extract_opt lit with
  | Some Canonical.True -> true
  | _ -> false

let has_instances lit =
  match Canonical.extract_opt lit with
  | Some Canonical.False -> false
  | _ -> true

let simplify ~ord lit =
  try
    let e = Canonical.extract lit in
    match e with
    | Canonical.True -> Literal.mk_tauto
    | Canonical.False -> Literal.mk_absurd
    | _ ->
      begin match Single.of_canonical e with
      | Some lit' ->
        (* simplified into a "single" monome, much better *)
        Util.debug 5 "arith.simplify %a into %a" Literal.pp lit Single.pp lit';
        Single.to_lit ~ord lit'
      | None ->
        (* just simplify into canonical form *)
        Canonical.to_lit ~ord e
      end
  with M.NotLinear ->
    Literal.fmap ~ord AT.simplify lit

(* find instances of variables that eliminate the literal *)
let eliminate ?(elim_var=(fun v -> true)) ?fresh_var lit =
  (* find some solutions *)
  let solutions =
    try
      let elit = Canonical.extract lit in
      Canonical.eliminate ?fresh_var elit
    with M.NotLinear -> []
  in
  let unif_arith ~subst t1 sc_t m sc_m =
    FOUnif.unification ~subst t1 sc_t (M.to_term m) sc_m
  in
  Util.list_fmap
    (fun sol ->
      try
        (* make a substitution out of the solution *)
        let subst = List.fold_left
          (fun subst (t, m) ->
            (* check whether we can eliminate a variable *)
            if T.is_var t && not (elim_var t) then raise Exit;
            unif_arith ~subst t 0 m 0)
          Substs.FO.empty sol
        in
        Some subst
      with FOUnif.Fail | Exit -> None)
    solutions

let heuristic_eliminate lit =
  match lit with
  | ( Literal.Equation ({T.term=T.Node(prod, _, [x1; x2])}, {T.term=T.Node(n,_,[])}, false, _)
    | Literal.Equation ({T.term=T.Node(n,_,[])}, {T.term=T.Node(prod, _,[x1; x2])}, false, _))
    when S.eq prod S.Arith.product && T.is_var x1 && T.eq x1 x2 && S.is_numeric n ->
    (* ahah, square root spotted! *)
    Util.debug 5 "heuristic_elim tries sqrt of %a" S.pp n;
    begin match n with
    | S.Int n ->
      if Big_int.sign_big_int n >= 0
        then
          let s = Big_int.sqrt_big_int n in
          if Big_int.eq_big_int (Big_int.square_big_int s) n
            then
              (* n is positive, and has an exact square root, try both
                  the positive and negative square roots*)
              [ Substs.FO.bind Substs.FO.empty x1 0 (T.mk_const (S.mk_bigint s)) 0
              ; Substs.FO.bind Substs.FO.empty x1 0
                (T.mk_const (S.mk_bigint (Big_int.minus_big_int s))) 0 ]
            else []
        else []
    | S.Rat n -> []  (* TODO *)
    | S.Real n ->
      if n >= 0.
        then
          let s = sqrt n in
          [ Substs.FO.bind Substs.FO.empty x1 0 (T.mk_const (S.mk_real s)) 0
          ; Substs.FO.bind Substs.FO.empty x1 0 (T.mk_const (S.mk_real (~-. s))) 0
          ]
        else []
    | _ -> failwith "unknown numeric type!?"
    end
  | _ -> []

(** {2 Arrays of literals} *)

module Arr = struct
  let purify ~ord ~eligible lits =
    let new_lits = ref [] in
    let _add_lit lit = new_lits := lit :: !new_lits in
    let varidx = ref (T.max_var (Literals.vars lits) + 1) in
    (* purify a term (adding constraints to the list). [root] is true only
        if the term occurs in the outermost arith expression *)
    let rec purify_term ~root t = match t.T.term with
    | T.Var _
    | T.BoundVar _ -> t
    | T.Node (s, _, []) when S.is_numeric s -> t
    | T.Node (s, tyargs, l) when S.Arith.is_arith s ->
      if root
        then (* recurse, still in root arith expression *)
          T.mk_node ~tyargs s (List.map (purify_term ~root) l)
        else begin
          (* purify this term out! *)
          let ty = t.T.ty in
          let v = T.mk_var ~ty !varidx in
          incr varidx;
          (* purify the term and add a constraint *)
          let t' = purify_term ~root:true t in
          let lit = Literal.mk_neq ~ord v t' in
          _add_lit lit;
          (* return variable instead of literal *)
          v
        end
    | T.Node (s, tyargs, l) ->
      T.mk_node ~tyargs s (List.map (purify_term ~root:false) l)
    in
    (* purify each literal *)
    Array.iteri
      (fun i lit ->
        if eligible i lit
          then match lit with
          | Literal.Equation (l, r, sign, _) ->
            let l = purify_term ~root:true l in
            let r = purify_term r ~root:true in
            let lit = Literal.mk_lit ~ord l r sign in
            _add_lit lit
          | Literal.Prop (p, sign) ->
            let p = purify_term ~root:true p in
            let lit = Literal.mk_prop p sign in
            _add_lit lit
          | Literal.True -> _add_lit lit
          | Literal.False -> ()  (* useless *)
          else _add_lit lit (* keep *)
      )
      lits;
    Array.of_list (List.rev !new_lits)

  let shielded ?(filter=(fun _ _ -> true)) lits var =
    if not (T.is_var var) then failwith "ArithLit.shielded";
    try
      for i = 0 to Array.length lits - 1 do
        if filter i lits.(i) &&
        match lits.(i) with
        | Literal.Prop (p, _) -> AT.shielded ~var p
        | Literal.Equation (l, r, _, _) -> AT.shielded ~var l || AT.shielded ~var r
        | _ -> false
        then raise Exit
      done;
      false
    with Exit ->
      true

  let naked_vars ?filter lits =
    let vars = Literals.vars lits in
    List.filter (fun v -> not (shielded ?filter lits v)) vars

  let eliminate ~ord ~eligible lits =
    let results = ref [] in
    let lits' = Array.to_list lits in
    let add_res a = results := a :: !results in
    (* how to build fresh variables *)
    let fresh_var =
      let offset = ref (T.max_var (Literals.vars lits) + 1) in
      fun ty ->
        incr offset;
        T.mk_var ~ty !offset
    in
    (* instantiate with [subst]. Simplifications should then remove
        the literal; making the instantiation a step makes the proof
        more readable *)
    let eliminate_lit i subst =
      let renaming = Substs.FO.Renaming.create 5 in
      let lits' = Literal.apply_subst_list ~ord ~renaming subst lits' 0 in
      let lits' = Array.of_list lits' in
      add_res lits'
    in
    for i = 0 to Array.length lits - 1 do
      if eligible i lits.(i) then begin
        (* can eliminate only naked vars *)
        let elim_var =
          let vars = naked_vars ~filter:(fun i' _ -> i<>i') lits in
          fun v -> List.memq v vars
        in
        (* try heuristic substitutions *)
        let substs = heuristic_eliminate lits.(i) in
        (* try to eliminate literal as a linear expression *)
        let substs' = eliminate ~elim_var ~fresh_var lits.(i) in
        List.iter
          (fun subst -> eliminate_lit i subst)
          (substs @ substs');
      end
    done;
    !results
end
