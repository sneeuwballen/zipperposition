
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

(** {1 Higher Order operations} *)

open Term

let prof_beta_reduce = Util.mk_profiler "HO.beta_reduce"
let prof_lambda_abstract = Util.mk_profiler "HO.lambda_abstract"

(* Curry all subterms *)
let rec curry t =
  match t.term with
  | Var _ | BoundVar _ -> t
  | Bind (s, t') -> mk_bind s (curry t')
  | Node (f, []) -> t
  | Node (f, l) when Symbol.is_connective f ->
    mk_node f (List.map curry l)
  | Node (f, [t']) ->
    mk_at (mk_const f) (curry t')
  | Node (f, l) ->
    (* build the curryfied application of [f] to [l] *)
    List.fold_left
      (fun left t' -> mk_at left (curry t'))
      (mk_const f) l
  | At (t1, t2) -> mk_at (curry t1) (curry t2)  (* already curried *)

let uncurry t =
  (* uncurry any kind of term, except the '@' terms that are
     handled over to unfold_left *)
  let rec uncurry t =
    match t.term with
    | Var _ | BoundVar _ -> t
    | Bind (s, t') -> mk_bind s (uncurry t')
    | Node (_, []) -> t  (* constant *)
    | Node (f, l) -> mk_node f (List.map uncurry l)
    | At (t1, t2) -> unfold_left t1 [uncurry t2]
  (* transform "(((f @ a) @ b) @ c) into f(a,b,c)". Here, we
     deconstruct "f @ a" into "unfold f (a :: args)"*)
  and unfold_left head args = match head.term with
    | At (a, b) -> unfold_left a (uncurry b :: args)
    | Node (f, []) ->
      mk_node f args  (* constant symbol, ok *)
    | _ -> failwith "not a curried term"
  in
  uncurry t

let rec curried t = match t.term with
  | Var _
  | BoundVar _
  | Node (_, []) -> true
  | Node (_, _::_) -> false
  | Bind (_, t') -> curried t'
  | At (t1, t2) -> curried t1 && curried t2

let rec is_fo t = match t.term with
  | Var _ | BoundVar _ -> true
  | Bind (s, t') when Symbol.eq s Symbol.lambda_symbol -> false
  | Bind (_, t') -> is_fo t'
  | At ({term=(Var _ | BoundVar _)}, _) -> false (* X @ _ is not first-order  *)
  | At (a, b) -> is_fo a && is_fo b
  | Node (_, l) -> List.for_all is_fo l

let beta_reduce ?(depth=0) t =
  Util.enter_prof prof_beta_reduce;
  (* recursive reduction in call by value. [env] contains the environment for
  De Bruijn indexes. *)
  let rec beta_reduce ~depth env t = match t.term with
  | Var _ -> t
  | BoundVar n when n < List.length env ->
    (* look for the possible binding for [n] *)
    begin match List.nth env n with
    | None -> t
    | Some t' when t == t' -> t
    | Some t' -> db_lift ~depth depth t' (* need to lift free vars *)
    end
  | BoundVar _ -> t
  | Bind (s, t') -> mk_bind s (beta_reduce ~depth:(depth+1) (None::env) t')
  | At ({term=Bind (s, t1)}, t2) when Symbol.eq s Symbol.lambda_symbol ->
    (* a beta-redex! Fire!! First evaluate t2, then
       remplace db0 by [t2] in [t1] *)
    let t2' = beta_reduce ~depth env t2 in
    let env' = Some t2' :: env in
    beta_reduce ~depth env' t1
  | At (t1, t2) ->
    let t1' = beta_reduce ~depth env t1 in
    let t2' = beta_reduce ~depth env t2 in
    if t1 == t1' && t2 == t2'
      then t
      else beta_reduce ~depth env (mk_at t1' t2')  (* new redex? *)
  | Node (f, l) ->
    mk_node f (List.map (fun t -> beta_reduce ~depth env t) l)
  in
  let t' = beta_reduce ~depth [] t in
  Util.exit_prof prof_beta_reduce;
  t'

let rec eta_reduce t =
  match t.term with
  | Var _ | BoundVar _ -> t
  | Bind (s, {term=Node (a, [t'; {term=BoundVar 0}])})
    when Symbol.eq s Symbol.lambda_symbol && not (db_contains t' 0) ->
    eta_reduce (db_unlift t')  (* remove the lambda and variable *)
  | Bind (s, t') ->
    mk_bind s (eta_reduce t')
  | Node (f, l) ->
    mk_node f (List.map eta_reduce l)
  | At (t1, t2) -> mk_at (eta_reduce t1) (eta_reduce t2)

let lambda_abstract ~signature t sub_t =
  Util.enter_prof prof_lambda_abstract;
  (* infer type of [sub_t] *)
  let ctx = TypeInference.Ctx.of_signature signature in
  let ty = TypeInference.infer ctx sub_t in
  (* abstract the term *)
  let t' = db_from_term ~ty t sub_t in
  let t' = mk_lambda t' in
  Util.exit_prof prof_lambda_abstract;
  t'

let lambda_abstract_list ~signature t args =
  List.fold_left (lambda_abstract ~signature) t args

let lambda_apply_list t args =
  let t' = List.fold_right (fun arg t -> mk_at t arg) args t in
  let t' = beta_reduce t' in
  t'

