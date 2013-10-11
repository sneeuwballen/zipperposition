
(*
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

(** {1 Lambda-Calculus} *)

let prof_beta_reduce = Util.mk_profiler "HO.beta_reduce"
let prof_lambda_abstract = Util.mk_profiler "HO.lambda_abstract"

type term = HOTerm.t

module T = HOTerm

(* TODO: flag to check whether a term is beta-reduced *)

let beta_reduce ?(depth=0) t =
  Util.enter_prof prof_beta_reduce;
  (* recursive reduction in call by value. [env] contains the environment for
  De Bruijn indexes. *)
  let rec beta_reduce ~depth env t = match t.T.term with
  | T.Var _ -> t
  | T.BoundVar n when n < List.length env ->
    (* look for the possible binding for [n] *)
    begin match List.nth env n with
    | None -> t
    | Some t' when T.eq t t' -> t
    | Some t' -> T.db_lift ~depth depth t' (* need to lift free vars *)
    end
  | T.Const _
  | T.BoundVar _ -> t
  | T.Bind (s, t') -> T.mk_bind s (beta_reduce ~depth:(depth+1) (None::env) t')
  | T.At ({T.term=T.Bind (s, t1)}, t2) when Symbol.eq s Symbol.lambda_symbol ->
    (* a beta-redex! Fire!! First evaluate t2, then
       remplace db0 by [t2] in [t1] *)
    let t2' = beta_reduce ~depth env t2 in
    let env' = Some t2' :: env in
    beta_reduce ~depth env' t1
  | T.At (t1, t2) ->
    let t1' = beta_reduce ~depth env t1 in
    let t2' = beta_reduce ~depth env t2 in
    if T.eq t1 t1' && T.eq t2 t2'
      then t
      else beta_reduce ~depth env (T.mk_at t1' t2')  (* new redex? *)
  in
  let t' = beta_reduce ~depth [] t in
  Util.exit_prof prof_beta_reduce;
  t'

let rec eta_reduce t =
  match t.T.term with
  | T.Var _ | T.BoundVar _ | T.Const _ -> t
  | T.Bind (s, {T.term=T.At (t', {T.term=T.BoundVar 0})})
    when Symbol.eq s Symbol.lambda_symbol && not (T.db_contains t' 0) ->
    eta_reduce (T.db_unlift t')  (* remove the lambda and variable *)
  | T.Bind (s, t') -> T.mk_bind s (eta_reduce t')
  | T.At (t1, t2) -> T.mk_at (eta_reduce t1) (eta_reduce t2)

let lambda_abstract ~signature t sub_t =
  Util.enter_prof prof_lambda_abstract;
  (* infer type of [sub_t] *)
  let ctx = TypeInference.Ctx.of_signature signature in
  let ty = TypeInference.HO.infer ctx sub_t in
  (* abstract the term *)
  let t' = T.db_from_term ~ty t sub_t in
  let t' = T.mk_lambda t' in
  Util.exit_prof prof_lambda_abstract;
  t'

let lambda_abstract_list ~signature t args =
  List.fold_left (lambda_abstract ~signature) t args

let lambda_apply_list t args =
  let t' = T.mk_at_list t args in
  let t' = beta_reduce t' in
  t'
