
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
type scope = Substs.scope

module T = HOTerm

(* TODO: flag to check whether a term is beta-reduced *)

(* match l_expected against l_arg. All types of l_arg must have a
  corresponding generalization in l_expected (but l_expected can be
  longer, it's a partial application) *)
let rec _match_list subst l_expected s_e l_arg s_a = match l_expected, l_arg with
  | [], [] -> subst
  | [], _ -> failwith "lambda_apply_list: too many arguments"
  | _, [] -> subst   (* all arguments pass *)
  | ty::l_expected', arg::l_arg' ->
    (* match expected type with argument *)
    let subst = TypeUnif.unify ~subst ty s_e arg s_a in 
    _match_list subst l_expected' s_e l_arg' s_a

let match_types ?(subst=Substs.Ty.empty) ty s_ty args s_args =
  match ty.Type.ty, args with
  | _, [] -> subst
  | Type.Fun (_, expected), _ ->
    (* match expected types against provided types *)
    _match_list subst expected s_ty args s_args
  | _ ->
    (* raise some type error *)
    TypeUnif.fail subst ty s_ty Type.((__var ~-1) <== args) s_args

(* recursive reduction in call by value. [env] contains the environment for
De Bruijn indexes. [subst] is a substitution on types. *)
let rec beta_reduce_rec ~depth ~subst env t =
  let ty = Substs.Ty.apply_no_renaming subst t.T.ty 0 in
  match t.T.term with
  | T.Var i -> T.mk_var ~ty i
  | T.BoundVar n when n < List.length env ->
    (* look for the possible binding for [n] *)
    begin match List.nth env n with
    | None -> T.mk_bound_var ~ty n
    | Some t' ->
      assert (Type.eq ty t'.T.ty);
      T.db_lift ~depth depth t' (* need to lift free vars *)
    end
  | T.BoundVar n -> T.mk_bound_var ~ty n
  | T.Const s -> T.mk_const ~ty s
  | T.Lambda t' ->
    let varty = Substs.Ty.apply_no_renaming subst (T.lambda_var_ty t) 0 in
    let t'' = beta_reduce_rec ~depth:(depth+1) ~subst (None::env) t' in
    T.mk_lambda ~varty t''
  | T.At (t, l) ->
    let t' = beta_reduce_rec ~depth ~subst env t in
    let l' = List.map (beta_reduce_rec ~depth ~subst env) l in
    mk_at_check ~depth ~subst env t' l'
(* apply term to arguments, beta-reducing at root. [l] is assumed to be
  fully evaluated. *)
and mk_at_check ~depth ~subst env t l =
  (* specialize types if needed *)
  let ty_args = List.map T.get_type l in
  let subst = match_types ~subst t.T.ty 0 ty_args 0 in
  match t.T.term, l with
  | T.Lambda t1, t2::l ->
    (* a beta-redex! Fire!! Remplace db0 by [t2] in [t1] and beta reduce [t1] *)
    Util.debug 4 "beta-reduce: %a @ [%a]" T.pp t (Util.pp_list T.pp) (t2::l);
    let env' = Some t2 :: env in
    let t1' = beta_reduce_rec ~depth ~subst env' t1 in
    Util.debug 4 " ---> %a @ [%a]" T.pp t1' (Util.pp_list T.pp) l;
    (* now reduce t1' @ l, if l not empty *)
    mk_at_check ~depth ~subst:Substs.Ty.empty env t1' l
  | _, [] ->
    (* no argument, no need to do anything but type specialization *)
    let subst = Substs.HO.of_ty subst in
    Substs.HO.apply_no_renaming ~depth subst t 0
  | _, _ ->
    let subst = Substs.HO.of_ty subst in
    let t' = Substs.HO.apply_no_renaming ~depth subst t 0 in
    let l' = List.map (fun t -> Substs.HO.apply_no_renaming ~depth subst t 0) l in
    T.mk_at t' l'

let beta_reduce ?(depth=0) t =
  Util.enter_prof prof_beta_reduce;
  try
    let t' = beta_reduce_rec ~depth ~subst:Substs.Ty.empty [] t in
    Util.exit_prof prof_beta_reduce;
    t'
  with e ->
    Util.exit_prof prof_beta_reduce;
    raise e

let rec eta_reduce t =
  match t.T.term with
  | T.Var _ | T.BoundVar _ | T.Const _ -> t
  | T.Lambda {T.term=T.At (t', [{T.term=T.BoundVar 0}])} when not (T.db_contains t' 0) ->
    eta_reduce (T.db_unlift t')  (* remove the lambda and variable *)
  | T.Lambda t' ->
    let varty = T.lambda_var_ty t in
    T.mk_lambda ~varty (eta_reduce t')
  | T.At (t, l) ->
    T.mk_at (eta_reduce t) (List.map eta_reduce l)

let lambda_abstract t ~sub =
  Util.enter_prof prof_lambda_abstract;
  (* abstract the term *)
  let t' = T.db_from_term t sub in
  let varty = sub.T.ty in
  let t' = T.mk_lambda ~varty t' in
  Util.exit_prof prof_lambda_abstract;
  t'

let lambda_abstract_list t args =
  List.fold_right (fun sub t -> lambda_abstract t ~sub) args t

let can_apply ty args =
  try ignore (match_types ty 0 args 0); true
  with TypeUnif.Error _ -> false

(* TODO: efficient, type safe apply_reduce (see above) that carries subst
  and apply it to terms *)

let lambda_apply_list ?(depth=0) t args =
  Util.enter_prof prof_beta_reduce;
  try
    let t' = mk_at_check ~depth ~subst:Substs.Ty.empty [] t args in
    Util.exit_prof prof_beta_reduce;
    t'
  with e ->
    Util.exit_prof prof_beta_reduce;
    raise e
