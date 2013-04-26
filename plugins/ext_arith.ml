(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Plugin for TSTP arithmetic} *)

(** See http://www.cs.miami.edu/~tptp/TPTP/TR/TPTPTR.shtml#TypeSystem *)

open Basic
open Symbols

module T = Terms
module C = Clauses
module S = FoSubst
    
(** {2 Helpers} *)

let plus a b = T.mk_node (mk_symbol "$sum") a.sort [a; b]
let minus a b = T.mk_node (mk_symbol "$difference") a.sort [a; b]
let times a b = T.mk_node (mk_symbol "$product") a.sort [a; b]
let over a b =
  if a.sort == int_
    then  (* to_int (quotient (to_rat a, to_rat b)) *)
      T.mk_node (mk_symbol "$to_int") int_
        [T.mk_node (mk_symbol "$quotient") rat_
          [T.mk_node (mk_symbol "$to_rat") rat_ [a];
           T.mk_node (mk_symbol "$to_rat") rat_ [b]]]
    else T.mk_node (mk_symbol "$quotient") a.sort [a; b]

let succ a =
  if a.sort == real_
    then plus a (T.mk_const Arith.one_f real_)
    else plus a (T.mk_const Arith.one_i a.sort)

let pred a =
  if a.sort == real_
    then minus a (T.mk_const Arith.one_f real_)
    else minus a (T.mk_const Arith.one_i a.sort)

let epsilon_real =
  T.mk_const (mk_real (epsilon_float *. 50.)) real_ (* not too small *)

let epsilon_rat =
  let open Num in
  T.mk_const (mk_num (num_of_int 1 // num_of_int 100_000_000)) rat_ (* not too small *)

(* some term slightly bigger than t *)
let next t =
  if t.sort == int_ then succ t
  else if t.sort == rat_ then plus t epsilon_rat
  else plus t epsilon_real

(* some term slightly smaller than t *)
let prev t =
  if t.sort == int_ then pred t
  else if t.sort == rat_ then minus t epsilon_rat
  else minus t epsilon_real

let zero sort =
  if sort == real_
    then T.mk_const Arith.zero_f sort
    else T.mk_const Arith.zero_i sort

let one sort =
  if sort == real_
    then T.mk_const Arith.one_f sort
    else T.mk_const Arith.one_i sort

let to_int t =
  if t.sort == int_ then t else T.mk_node (mk_symbol "$to_int") int_ [t]

let is_zero t = match t.term with
  | Node (n, []) when is_numeric n -> Arith.is_zero n
  | _ -> false

let is_one t = match t.term with
  | Node ({symb_val=Num n}, []) ->
    n = Num.num_of_int 1
  | Node ({symb_val=Real f}, []) ->
    f = 1.
  | _ -> false

(** Is [t] a number? *)
let rec is_number t = match t.term with
  | Node (s, []) when Symbols.is_numeric s -> true
  | _ -> false

(** {2 Evaluation} *)

(** simplification function for arithmetic *)
let rec arith_canonize_rec t =
  match t.term with
  | Var _
  | BoundVar _ -> t
  | Bind (s, a_sort, t') ->
    let new_t' = arith_canonize_rec t' in
    T.mk_bind s t.sort a_sort new_t'
  | Node (s, [a]) ->
    let a' = arith_canonize_rec a in
    try_reduce_unary s t.sort a'
  | Node (s, [a; b]) ->
    let a' = arith_canonize_rec a
    and b' = arith_canonize_rec b in
    try_reduce_binary s t.sort a' b'
  (*
  | Node (s, l) when Symbols.symbol_val s == Const "$distinct" ->
    try_reduce_distinct l
  *)
  | Node (s, l) ->
    let l' = List.map arith_canonize_rec l in
    T.mk_node s t.sort l'
(** unary builtins *)
and try_reduce_unary s sort a =
  match Symbols.get_val s, a.term with
  | Const "$uminus", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.uminus n) a.sort
  | Const "$uminus", Node ({symb_val=Const "$uminus"}, [x]) -> x
  | Const "$floor", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.floor n) int_
  | Const "$ceiling", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.ceiling n) int_
  | Const "$round", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.round n) int_
  | Const "$truncate", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.truncate n) int_
  | Const "$is_int", Node (n, []) when is_numeric n ->
    if is_int n then T.true_term else T.false_term
  | Const "$is_rat", Node (n, []) when is_numeric n ->
    if is_rat n then T.true_term else T.false_term
  | Const "$is_real", Node (n, []) when is_numeric n ->
    if is_real n then T.true_term else T.false_term
  | Const "$to_rat", Node (n, []) when is_numeric n ->
    begin match get_val n with
    | Num _ -> T.cast a rat_  (* conversion trivial *)
    | _ -> T.mk_node s rat_ [a]  (* no conversion *)
    end
  | Const "$to_real", Node (n, []) when is_numeric n ->
    begin match get_val n with
    | Num n -> T.mk_const (mk_real (Num.float_of_num n)) real_  (* conversion ok *)
    | Real _ -> a  (* conversion trivial *)
    | _ -> T.mk_node s rat_ [a]  (* no conversion *)
    end
  | Const "$to_int", Node (n, []) when is_numeric n ->
    begin match get_val n with
    | Num n -> T.mk_const (mk_num (Num.floor_num n)) int_  (* conversion ok *)
    | _ -> T.mk_node s rat_ [a]  (* no conversion *)
      (* XXX note: for reals, round? problem with very big ints *)
    end
  | _ ->
    T.mk_node s sort [a]  (* default case *)
(** binary builtins *)
and try_reduce_binary s sort a b =
  try begin match Symbols.get_val s, a.term, b.term with
  | Const "$sum", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.sum na nb) sort
  | Const "$difference", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.difference na nb) sort
  | Const "$product", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.product na nb) sort
  | Const "$quotient", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.quotient na nb) sort
  | Const "$quotient_e", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.quotient_e na nb) sort
  | Const "$quotient_t", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.quotient_t na nb) sort
  | Const "$quotient_f", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.quotient_f na nb) sort
  | Const "$remainder_e", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.remainder_e na nb) sort
  | Const "$remainder_t", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.remainder_t na nb) sort
  | Const "$remainder_f", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.remainder_f na nb) sort
  | Const "$less", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    if Arith.less na nb then T.true_term else T.false_term
  | Const "$lesseq", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    if Arith.lesseq na nb then T.true_term else T.false_term
  | Const "$greater", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    if Arith.greater na nb then T.true_term else T.false_term
  | Const "$greatereq", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    if Arith.greatereq na nb then T.true_term else T.false_term
  | Const "$sum", _, _ when is_zero (arith_canonize_rec b) -> a
  | Const "$sum", _, _ when is_zero (arith_canonize_rec a) -> b
  | Const "$difference", _, _ when is_zero (arith_canonize_rec b) -> a
  | Const "$difference", _, _ when is_zero (arith_canonize_rec a) ->
    arith_canonize_rec (T.mk_node (mk_symbol "$uminus") b.sort [b])
  | Const "$difference", _, _ when arith_canonize_rec a == arith_canonize_rec b ->
    zero a.sort
  | Const "$product", _, _ when is_one (arith_canonize_rec b) -> a
  | Const "$product", _, _ when is_one (arith_canonize_rec a) -> b
  | Const "$quotient", _, _ when is_one (arith_canonize_rec b) -> a
  | Const "$quotient", _, _ when is_zero (arith_canonize_rec a) -> zero a.sort
  | _ ->
    T.mk_node s sort [a; b]  (* default case *)
  end with Division_by_zero ->
    T.mk_node s sort [a; b]

(* TODO associativity/distributivity? *)

(** Main evaluation function *)
let arith_canonize t =
  let t' = arith_canonize_rec t in
  (if t != t'
    then FoUtils.debug 3 "%% @[<h>arith_canonize %a to %a@]"
      !T.pp_term#pp t !T.pp_term#pp t');
  t'

(** {2 Expert for arithmetic} *)

(** Some basic axioms to help *)
let axioms ~ctx =
  let module Lits = Literals in
  let mk_proof name c = Axiom (c, "axiom_arith", name) in
  [ C.mk_hclause ~ctx
      [ Lits.mk_neq ~ord:ctx.ctx_ord (zero int_) (one int_) ]
    (mk_proof "not_0_eq_1_int");
    C.mk_hclause ~ctx
      [ Lits.mk_neq ~ord:ctx.ctx_ord (zero rat_) (one rat_) ]
    (mk_proof "not_0_eq_1_rat");
    C.mk_hclause ~ctx
      [ Lits.mk_neq ~ord:ctx.ctx_ord (zero real_) (one real_) ]
    (mk_proof "not_0_eq_1_reat");
    C.mk_hclause ~ctx
      [ Lits.mk_eq ~ord:ctx.ctx_ord
        (T.mk_node (mk_symbol "$greater") bool_ [one int_; zero int_]) T.true_term ]
    (mk_proof "smaller_0_1_int");
    C.mk_hclause ~ctx
      [ Lits.mk_eq ~ord:ctx.ctx_ord
        (T.mk_node (mk_symbol "$greater") bool_ [one rat_; zero rat_]) T.true_term ]
    (mk_proof "smaller_0_1_rat");
    C.mk_hclause ~ctx
      [ Lits.mk_eq ~ord:ctx.ctx_ord
        (T.mk_node (mk_symbol "$greater") bool_ [one real_; zero real_]) T.true_term ]
    (mk_proof "smaller_0_1_real");
  ]

  
(** Expert that evaluates arithmetic expressions *)
let rec expert ~ctx =
  let open Experts in
  let signature = SSet.empty in
  { expert_name = "arith";
    expert_descr = "evaluation for TSTP arithmetic";
    expert_equal = (fun t1 t2 -> arith_canonize t1 == arith_canonize t2);
    expert_sig = signature;
    expert_clauses = axioms ~ctx;
    expert_canonize = arith_canonize;
    expert_ord = (fun _ -> true);
    expert_ctx = ctx;
    expert_update_ctx = (fun ctx -> [expert ~ctx]);
    expert_solve = None;
  }

(** {2 Canonization of term} *)

type condition =
  | EqZero of term
  | NeqZero of term
  | LessZero of term  (* t < 0 *)
  | BiggerZero of term

type elim_result =
  | ElimOk of term * term * condition list  (* a, b, conditions *)
  | ElimFail

(** Try to eliminate [x] in [e], returning a monomial (a,b) representing
    [a*x + b] where [x] does not occur in [a] nor [b]. Also returns a list
    of side conditions. *)
let elim_var ~var:x e =
  assert (x.sort == e.sort);
  assert (T.is_var x);
  let sort = e.sort in
  (* conditions required for elimination *)
  let conditions = ref [] in
  (* recursive elimination *)
  let rec elim e = match e.term with
    | Var _ when x == e -> one sort, zero sort  (* (1,0) *)
    | Var _
    | BoundVar _ -> zero sort, e  (* (0,e) *)
    | _ when not (T.var_occurs x e) -> zero sort, e  (* (0, e) *)
    | Bind _ -> raise Exit   (* [x] occurs in binder, too difficult *)
    | Node ({symb_val=Const "$sum"}, [e1; e2]) ->
      let a1, b1 = elim e1 in
      let a2, b2 = elim e2 in
      plus a1 a2, plus b1 b2  (* a1+a2, b1+b2 *)
    | Node ({symb_val=Const "$difference"}, [e1; e2]) ->
      let a1, b1 = elim e1 in
      let a2, b2 = elim e2 in
      minus a1 a2, minus b1 b2  (* a1-a2, b1-b2 *)
    | Node ({symb_val=Const "$product"}, [e1; e2]) ->
      let a1, b1 = elim e1 in
      let a2, b2 = elim e2 in
      (* a1 * a2 = 0 necessary for keeping a monomial (otherwise polynomial) *)
      (if zero sort != arith_canonize (times a1 a2)
        then conditions := (EqZero (times a1 a2)) :: !conditions);
      plus (times a1 b2) (times a2 b1), times b1 b2  (* a1b2+a2b1, b1b2 *)
    | Node ({symb_val=Const "$quotient"}, [e1; e2]) ->
      let a1, b1 = elim e1 in
      let a2, b2 = elim e2 in
      (* a2 = zero necessary *)
      (if zero sort != arith_canonize a2
        then conditions := (EqZero a2) :: !conditions);
      (* b2 != zero necessary *)
      conditions := (NeqZero b2) :: !conditions;
      over a1 b2, over b1 b2  (* a1/b2, b1/b2 *)
    | _ ->
      raise Exit  (* other cases not handled *)
  in
  try
    let a, b = elim e in
    ElimOk (arith_canonize a, arith_canonize b, !conditions)
  with Exit ->
    ElimFail

(** Instantiate [hc], minus its i-th component, with given binding
    and side-conditions *)
let rebuild ?(rule="arith") ?(conditions=[]) ?i hc bindings =
  let ctx = hc.hcctx in
  let subst = List.fold_left
    (fun s (x,t) -> S.bind s (x,0) (arith_canonize t,0)) S.id_subst bindings in
  (* literals from [hc], excepted the [i-th] one if [i] is specified *)
  let hc_lits =
    match i with
    | None -> Array.to_list hc.hclits
    | Some i -> FoUtils.array_except_idx hc.hclits i in
  (* literals for side conditions *)
  let lits' =
    List.map
      (function
        | EqZero e ->
          Literals.mk_neq ~ord:ctx.ctx_ord (arith_canonize e) (zero e.sort)
        | NeqZero e ->
          Literals.mk_eq ~ord:ctx.ctx_ord (arith_canonize e) (zero e.sort)
        | LessZero e -> (* e < 0 ---> literal e >= 0 *)
          Literals.mk_eq ~ord:ctx.ctx_ord
            (T.mk_node (mk_symbol "$greatereq") bool_ [arith_canonize e; zero e.sort])
            T.true_term
        | BiggerZero e -> (* e > 0 ---> literal e <= 0 *)
          Literals.mk_eq ~ord:ctx.ctx_ord
            (T.mk_node (mk_symbol "$lesseq") bool_ [arith_canonize e; zero e.sort])
            T.true_term
      )
    conditions
  in
  let lits = hc_lits @ lits' in
  (* make a new clause *)
  let lits = Literals.apply_subst_list ~ord:ctx.ctx_ord subst (lits,0) in
  let proof c = Proof (c, rule, [hc.hcproof]) in
  let parents = [hc] in
  let new_clause = C.mk_hclause ~parents ~ctx lits proof in
  FoUtils.debug 3 "%%  arith deduction (%s with @[<h>%a@]): @[<h>%a@]"
    rule S.pp_substitution subst !C.pp_clause#pp_h new_clause;
  new_clause

(** Make [e1] <= [e2], please. *)
let mk_smaller_eq hc i e1 e2 =
  let vars = T.vars_list [e1; e2] in
  List.fold_left
    (fun acc v ->  (* eliminate [v] *)
      match elim_var ~var:v e1, elim_var ~var:v e2 with
      | ElimOk (a1, _, _), ElimOk (a2, _, _)
        when is_zero (arith_canonize (minus a1 a2)) ->
        acc  (* impossible constraint *)
      | ElimOk (a1, b1, conds1), ElimOk (a2, b2, conds2) ->
        (* a1 x + b1 <= a2 x + b2 ---> 
           x <= or >= (b2 - b1)/(a1 - a2) --->
           x = prev ((b2 - b1)/(a1 - a2)) if a1-a2 > 0
           x = next ((b2 - b1)/(a1 - a2)) if a1-a2 < 0 *)
        (* a1 - a2 > 0 *)
        (let conditions = (BiggerZero (minus a1 a2)) :: conds1 @ conds2 in
        let bindings =
          if e1.sort == int_
          then [v, prev (to_int (over (minus b2 b1) (minus a1 a2)))]
          else [v, prev (over (minus b2 b1) (minus a1 a2))]
        in
        rebuild ~rule:"arith_lesseq_inst_if_pos" ~conditions ~i hc bindings) ::
        (* a1 - a2 < 0 *)
        (let conditions = (LessZero (minus a1 a2)) :: conds1 @ conds2 in
        let bindings =
          if e1.sort == int_
          then [v, next (to_int (over (minus b2 b1) (minus a1 a2)))]
          else [v, next (over (minus b2 b1) (minus a1 a2))]
        in
        rebuild ~rule:"arith_lesseq_inst_if_neg" ~conditions ~i hc bindings) ::
        acc
      | _ -> acc)
    [] vars

(** Would you be so kind as to make [e1] = [e2]? *)
let mk_eq hc i e1 e2 =
  let vars = T.vars_list [e1; e2] in
  List.fold_left
    (fun acc v ->  (* eliminate [v] *)
      match elim_var ~var:v e1, elim_var ~var:v e2 with
      | ElimOk (a1, _, _), ElimOk (a2, _, _)
        when is_zero (arith_canonize (minus a1 a2)) ->
        acc  (* impossible constraint *)
      | ElimOk (a1, b1, conds1), ElimOk (a2, b2, conds2) ->
        (* a1 x + b1 = a2 x + b2 ---> 
           x = (b2 - b1)/(a1 - a2) *)
        let conditions = (NeqZero (minus a1 a2)) :: conds1 @ conds2 in
        let bindings, i =
          if e1.sort == int_  (* not sure it will be right *)
            then [v, to_int (over (minus b2 b1) (minus a1 a2))], None
          else if e1.sort == rat_
            then [v, over (minus b2 b1) (minus a1 a2)], Some i
          else
            [v, over (minus b2 b1) (minus a1 a2)], Some i
        in
        rebuild ~rule:"arith_eq_inst" ~conditions ?i hc bindings :: acc
      | _ -> acc)
    [] vars

(** Propose some clauses, derived from [hc], where the [i-th] literal is
    removed (and proposition [t] is satisfied) *)
let try_satisfy hc i t =
  match t.term with
  | Node ({symb_val=Const "$lesseq"}, [a; b]) 
  | Node ({symb_val=Const "$greatereq"}, [b; a])
    when (not (T.is_ground_term a) || not (T.is_ground_term b)) ->
    mk_smaller_eq hc i a b
  | Node ({symb_val=Const "$less"}, [a; b])
  | Node ({symb_val=Const "$greater"}, [b; a]) 
    when (not (T.is_ground_term a) || not (T.is_ground_term b)) ->
    (* a < b is implied by (next a) <= b. Careful with the ordering relation. *)
    mk_smaller_eq hc i (next a) b
  | _ -> []

(** Propose some clauses, derived from [hc], where the [i-th] literal is
    removed (and proposition [t] is insatisfiable) *)
let try_contradict hc i t =
  match t.term with
  | Node ({symb_val=Const "$lesseq"}, [a; b]) 
  | Node ({symb_val=Const "$greatereq"}, [b; a])
    when (not (T.is_ground_term a) || not (T.is_ground_term b)) ->
    (* a <= b is insatisfiable if a > b, which is implied by b <= prev a *)
    mk_smaller_eq hc i b (prev a)
  | Node ({symb_val=Const "$less"}, [a; b])
  | Node ({symb_val=Const "$greater"}, [b; a]) 
    when (not (T.is_ground_term a) || not (T.is_ground_term b)) ->
    (* a < b insatifiable if a >= b *)
    mk_smaller_eq hc i b a
  | _ -> []

let try_make_eq hc i l r =
  if l == r
    then [rebuild ~rule:"arith_basic" ~i hc []]  (* n == m is trivial *)
    else mk_eq hc i l r

let try_make_neq hc i l r =
  if is_number l && is_number r && l != r
    then [rebuild ~rule:"arith_basic" ~i hc []] (* n != m is trivial *)
    else
      (* l != r  is implied by l = r+1, try to make l = r+1 *)
      mk_eq hc i l (succ r)

(** inference rule that tries some basic hacks *)
let unary_inf_rule hc =
  (* try to eliminate one literal *)
  let basic_solve_lit acc i lit = match lit with
  | Equation (t, true_, false, _) when true_ == T.true_term ->
    try_satisfy hc i t @ acc
  | Equation (true_, t, false, _) when true_ == T.true_term ->
    try_satisfy hc i t @ acc
  | Equation (t, true_, true, _) when true_ == T.true_term ->
    try_contradict hc i t @ acc
  | Equation (true_, t, true, _) when true_ == T.true_term ->
    try_contradict hc i t @ acc
  | Equation (l, r, false, _) when l.sort != bool_ ->
    try_make_eq hc i l r
  | Equation (l, r, true, _) when l.sort != bool_ ->
    try_make_neq hc i l r @ acc
  | _ -> acc
  in
  FoUtils.array_foldi basic_solve_lit [] hc.hclits

(** Literal rewrite rule *)
let lit_rewrite ~ctx lit =
  match lit with
  | Equation (l, r, true, _) when l.sort != bool_ ->
    let l' = arith_canonize l
    and r' = arith_canonize r in
    if is_number l' && is_number r' &&  l != r
      then Literals.mk_eq ~ord:ctx.ctx_ord T.false_term T.true_term  (* false lit *)
      else lit
  | Equation (l, r, false, _) when l.sort != bool_ ->
    let l' = arith_canonize l
    and r' = arith_canonize r in
    if is_number l' && is_number r' &&  l == r
      then Literals.mk_eq ~ord:ctx.ctx_ord T.false_term T.true_term  (* false lit *)
      else lit
  | _ -> lit

(** The extension itself *)
let ext =
  let open Extensions in
  let actions =
    [ Ext_expert expert;
      Ext_unary_inf_rule ("arith_inst", unary_inf_rule);
      Ext_term_rewrite ("arith_eval", arith_canonize);
      Ext_lit_rewrite ("arith_lit", lit_rewrite);
      Ext_signal_incompleteness]
  in
  { name = "arith";
    actions;
  }

let _ =
  Extensions.register ext

