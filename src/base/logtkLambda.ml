
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

let prof_beta_reduce = LogtkUtil.mk_profiler "HO.beta_reduce"
let prof_lambda_abstract = LogtkUtil.mk_profiler "HO.lambda_abstract"

type term = LogtkHOTerm.t
type scope = LogtkSubsts.scope

module T = LogtkHOTerm

(* TODO: flag to check whether a term is beta-reduced *)

let rec match_types ?(subst=LogtkSubsts.empty) ty s_ty args s_args =
  match LogtkType.view ty, args with
  | _, [] -> subst
  | LogtkType.Fun (expected, ret), arg::args' ->
    (* match expected type with argument *)
    begin try
      let subst = LogtkUnif.Ty.unification ~subst expected s_ty arg s_args in
      match_types ~subst ret s_ty args' s_args
    with LogtkUnif.Fail ->
      let msg = CCFormat.sprintf "error: could not unify expected type %a with %a"
        LogtkType.pp expected LogtkType.pp arg in
      raise (LogtkType.Error msg)
    end
  | _ ->
    (* raise some type error *)
    let msg = CCFormat.sprintf "error: expected function type, got %a" LogtkType.pp ty in
    raise (LogtkType.Error msg)

(* recursive reduction in call by value. [env] contains the environment for
    De Bruijn indexes. *)
let rec beta_reduce_rec ~depth env t =
  let ty = T.ty t in
  match T.view t with
  | T.Var _
  | T.Const _ -> t
  | T.BVar n ->
    (* look for the possible binding for [n] *)
    begin match LogtkDBEnv.find env n with
    | None -> t
    | Some t' ->
      assert (LogtkType.eq ty (T.ty t'));
      assert (LogtkScopedTerm.DB.closed (t' : T.t :> LogtkScopedTerm.t));
      t'  (* must be closed, because it's already evaluated *)
    end
  | T.Lambda (varty, t') ->
    let env' = LogtkDBEnv.push_none env in
    let t'' = beta_reduce_rec ~depth:(depth+1) env' t' in
    T.__mk_lambda ~varty t''
  | T.Forall (varty, t') ->
    let env' = LogtkDBEnv.push_none env in
    let t'' = beta_reduce_rec ~depth:(depth+1) env' t' in
    T.__mk_forall ~varty t''
  | T.Exists (varty, t') ->
    let env' = LogtkDBEnv.push_none env in
    let t'' = beta_reduce_rec ~depth:(depth+1) env' t' in
    T.__mk_exists ~varty t''
  | T.At (l, r) ->
    begin match T.view l with
    | T.Lambda (_, l') ->
      (* beta-reduction *)
      LogtkUtil.debug 4 "beta-reduce: %a @ %a" (fun k->k T.pp l T.pp r);
      let r' = beta_reduce_rec ~depth env r in
      let env = LogtkDBEnv.push env r' in
      beta_reduce_rec ~depth env l'
    | _ ->
      let l' = beta_reduce_rec ~depth env l in
      let r' = beta_reduce_rec ~depth env r in
      app_with_reduce ~depth env l' r'
    end
  | T.TyLift _ -> t
  | T.Record (l, rest) ->
    let rest = CCOpt.map (beta_reduce_rec ~depth env) rest in
    let l = List.map
      (fun (name,t) -> name, beta_reduce_rec ~depth env t)
      l
    in
    T.record l ~rest
  | T.Multiset (ty, l) ->
    let l = List.map (beta_reduce_rec ~depth env) l in
    T.multiset ~ty l
(* apply [a] to [b], where both are in beta-normal form, reducing the
   redex at root if there is one *)
and app_with_reduce ~depth env a b = match T.view a with
  | T.Lambda (_, a') ->
      let env = LogtkDBEnv.push env b in
      beta_reduce_rec ~depth env a'
  | _ -> T.at a b



let beta_reduce ?(depth=0) t =
  LogtkUtil.enter_prof prof_beta_reduce;
  try
    let t' = beta_reduce_rec ~depth LogtkDBEnv.empty t in
    LogtkUtil.exit_prof prof_beta_reduce;
    t'
  with e ->
    LogtkUtil.exit_prof prof_beta_reduce;
    raise e

let rec eta_reduce t =
  match T.view t with
  | T.Var _ | T.BVar _ | T.Const _ -> t
  | T.At (l,r) ->
    T.at (eta_reduce l) (eta_reduce r)
  | T.TyLift _ -> t
  | T.Lambda (varty, t') ->
    begin match T.view t' with
      | T.BVar 0 when not (LogtkScopedTerm.DB.contains (t' :> LogtkScopedTerm.t) 0) ->
          let t' = T.of_term_exn (LogtkScopedTerm.DB.unshift 1 (t' : T.t :> LogtkScopedTerm.t)) in
          eta_reduce t'
      | _ -> T.__mk_lambda ~varty (eta_reduce t')
    end
  | T.Forall (varty, t') -> T.__mk_forall ~varty (eta_reduce t')
  | T.Exists (varty, t') -> T.__mk_exists ~varty (eta_reduce t')
  | T.Record (l, rest) ->
    let rest = CCOpt.map eta_reduce rest in
    let l = List.map (fun (n,t) -> n, eta_reduce t) l in
    T.record l ~rest
  | T.Multiset (ty, l) -> T.multiset ~ty (List.map eta_reduce l)

let lambda_abstract t ~sub =
  LogtkUtil.enter_prof prof_lambda_abstract;
  (* abstract the term *)
  let t' = T.of_term_exn (LogtkScopedTerm.DB.replace
    (t:T.t :> LogtkScopedTerm.t) ~sub:(sub:T.t:>LogtkScopedTerm.t)) in
  let varty = T.ty sub in
  let t' = T.__mk_lambda ~varty t' in
  LogtkUtil.exit_prof prof_lambda_abstract;
  t'

let lambda_abstract_list t args =
  List.fold_right (fun sub t -> lambda_abstract t ~sub) args t

let can_apply ty args =
  try ignore (match_types ty 0 args 0); true
  with LogtkType.Error _ -> false

let lambda_apply_list ?(depth=0) t args =
  LogtkUtil.enter_prof prof_beta_reduce;
  try
    let f = beta_reduce_rec ~depth LogtkDBEnv.empty t in
    let res = List.fold_left
      (fun t arg ->
        (* evaluate argument and apply [t] to it. *)
        let arg = beta_reduce_rec ~depth LogtkDBEnv.empty arg in
        app_with_reduce ~depth LogtkDBEnv.empty t arg)
      f args
    in
    LogtkUtil.exit_prof prof_beta_reduce;
    res
  with e ->
    LogtkUtil.exit_prof prof_beta_reduce;
    raise e
