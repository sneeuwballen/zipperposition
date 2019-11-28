
(* This file is free software. See file "license" for more details. *)

(** {1 Lambda-Calculus} *)

let prof_whnf = Util.mk_profiler "term.whnf"
let prof_snf = Util.mk_profiler "term.snf"
let prof_eta_expand = Util.mk_profiler "term.eta_expand"
let prof_eta_reduce = Util.mk_profiler "term.eta_reduce"


module OptionSet = Set.Make(
  struct 
    let compare x y = Pervasives.compare x y
    type t = int option
  end)


module Inner = struct
  module T = InnerTerm

  type term = T.t

  type state = {
    head: T.t;  (* not an app *)
    env: T.t DBEnv.t; (* env for the head *)
    args: T.t list; (* arguments, with their own env *)
    ty: T.t; (* type *)
  }

  (* evaluate term in environment *)
  let eval_in_env_ env t : T.t = T.DB.eval env t

  let normalize st = match T.view st.head with
    | T.App (f, l) ->
      (* the arguments in [l] might contain variables *)
      let l = List.rev_map (eval_in_env_ st.env) l in
      { st with head=f; args=List.rev_append l st.args; }
    | T.AppBuiltin (b, l) ->
      (* the arguments in [l] might contain variables *)
      let arg_tys = List.rev_map T.ty_exn l in
      let ret_ty = T.ty_exn st.head in
      let ty = T.arrow arg_tys ret_ty in
      let l = List.rev_map (eval_in_env_ st.env) l in
      { st with head=T.app_builtin ~ty b []; args=List.rev_append l st.args; }
    | _ -> st

  let st_of_term ~env ~ty t = {head=t; args=[]; env; ty; } |> normalize

  let term_of_st st : T.t =
    let f = eval_in_env_ st.env st.head in
    T.app ~ty:st.ty f st.args

  (* recursive reduction in call by value. [env] contains the environment for
      De Bruijn indexes. *)
  let rec whnf_rec st =
    begin match T.view st.head, st.args with
      | T.App _, _ -> assert false
      | T.Var _, _
      | T.Const _, _ -> st
      | T.DB _, _ ->
        let t' = eval_in_env_ st.env st.head in
        if T.equal st.head t' then st
        else (
          (* evaluate [db n], then reduce again *)
          { st with head=t'; env=DBEnv.empty; }
          |> normalize
          |> whnf_rec
        )
      | T.Bind (Binder.Lambda, ty_var, body), a :: args' ->
        (* beta-reduce *)
        Util.debugf 50 "(@[<2>beta-reduce@ @[%a@ %a@]@])"
          (fun k->k T.pp st.head T.pp a);
        assert (not (T.is_ground ty_var) || not (T.is_ground (T.ty_exn a)) 
                || T.equal ty_var (T.ty_exn a));
        let st' =
          { head=body;
            env=DBEnv.push st.env a;
            args=args';
            ty=st.ty;
          } |> normalize
        in
        whnf_rec st'
      | T.AppBuiltin _, _ | T.Bind _, _ -> st
    end

  let whnf_term_aux ?(env=DBEnv.empty) t = 
    match T.ty t with
    | T.HasType ty->
      let st = st_of_term ~ty ~env t in
      let st = whnf_rec st in
      term_of_st st
    | _ -> t

  let rec whnf_term ?(env=DBEnv.empty) t =
    ignore(env);
    let pref, tt = T.open_bind Binder.Lambda t in
    assert(not (T.is_lambda tt));
    let hd, args = T.as_app tt in
    if T.is_lambda hd && not (CCList.is_empty args) then (
      let tt' = whnf_term_aux tt in
      if T.equal tt' tt then t
      else whnf_term (T.fun_l pref tt')
    ) else t

  let rec snf_rec t =
    if T.is_beta_reducible t then (
      let t = whnf_term t in
      match T.ty t with
      | T.NoType -> t
      | T.HasType ty ->
        begin match T.view t with
          | T.App (f, l) ->
            let f' = snf_rec f in
            if not (T.equal f f') then snf_rec (T.app ~ty f' l)
            else (
              let l' = List.map snf_rec l in
              if T.equal f f' && T.same_l l l' then t else T.app ~ty f' l'
            )
          | T.AppBuiltin (b, l) ->
            let l' = List.map snf_rec l in
            if T.same_l l l' then t else T.app_builtin ~ty b l'
          | T.Var _ | T.Const _ | T.DB _ -> t
          | T.Bind (b, varty, body) ->
            let body' = snf_rec body in
            if T.equal body body' then t else T.bind b ~ty ~varty body'
        end) 
    else t

  let eta_expand_rec t =
    let rec aux t = match T.ty t with
      | T.NoType -> t
      | T.HasType ty ->
        let n, ty_args, ty_ret = T.open_poly_fun ty in
        if n!=0 then t (* polymorhpic eta expansion not implemented *)
        else
          (* first, WHNF *)
          let t = whnf_term t in
          (* see how many arguments are missing, and what type *)
          let args, body = T.open_bind Binder.Lambda t in
          let n_args = List.length ty_args in
          let n_missing = n_args - List.length args in
          if n_missing>0 then (
            Util.debugf 50 "(@[eta_expand_rec `%a`,@ missing %d args@ in %a@])"
              (fun k->k T.pp t n_missing (CCFormat.Dump.list T.pp) ty_args);
            (* missing args: suffix of length [n_missing] *)
            let missing_args = CCList.drop (n_args-n_missing) ty_args in
            (* shift body to accommodate for new binders *)
            let body = T.DB.shift n_missing body in
            (* build the fully-abstracted term *)
            let dbvars =
              List.mapi (fun i ty_arg -> T.bvar (n_missing-i-1) ~ty:ty_arg) missing_args
            in
            T.fun_l ty_args (aux (T.app ~ty:ty_ret body dbvars))
          ) else (
            let ty = T.ty_exn body in
            (* traverse body *)
            let body = match T.view body with
              | T.Const _ | T.Var _ | T.DB _ -> body
              | T.App (f, l) ->
                let l' = List.map aux l in
                if T.same_l l l' then body else T.app ~ty f l'
              | T.AppBuiltin (b, l) ->
                let l' = List.map aux l in
                if T.same_l l l' then body else T.app_builtin ~ty b l'
              | T.Bind (b, varty, body') ->
                assert (b <> Binder.Lambda);
                let body_reduced = aux body' in
                if body' = body_reduced then body else T.bind ~ty ~varty b body_reduced
            in
            T.fun_l ty_args body)
    in
    aux t

  (* compute eta-reduced normal form *)
  let eta_reduce_aux ?(full=true) t =      
    let q_reduce ~pref_len t =
      let hd, args = T.as_app t in
      let n = List.length args in
      let _, r_bvars = 
        List.fold_right (fun arg (idx, vars) -> 
            if idx = -1 then (idx, vars)
            else (
              if idx < pref_len && T.is_bvar_i idx arg then 
                (idx+1, arg :: vars)
              else (-1, vars)
            )
          ) args (0, []) in
      let redundant = List.length r_bvars in
      if redundant = 0 then 0, t
      else (
        let non_redundant = hd :: CCList.take (n-redundant) args in
        let _, m = List.fold_right (fun arg (idx, m) ->
            if idx = -1 then (idx, m)
            else (
              if not @@ List.exists (fun tt -> 
                  T.DB.contains tt (T.as_bvar_exn arg)) non_redundant then
                (idx+1, m+1) 
              else (-1, m))
          ) r_bvars (0, 0) in
        if m > 0 then (
          let args = CCList.take (n-m) args in 
          let ty = Type.apply_unsafe (Type.of_term_unsafe @@ T.ty_exn hd) args in
          m, T.DB.unshift m (T.app ~ty:(ty :> T.t) hd args)
        ) else 0, t
      ) in
    let rec aux t =
      if T.has_lambda t then (      
        match T.ty t with
        | T.NoType -> t
        | T.HasType ty ->
          begin match T.view t with
            | T.Var _ | T.DB _ | T.Const _ -> t
            | T.Bind(Binder.Lambda,_,_) ->
              let pref, body = T.open_bind Binder.Lambda t in
              let body' = if full then aux body else body in
              let n, reduced = q_reduce ~pref_len:(List.length pref) body' in
              assert(Type.equal (Type.of_term_unsafe @@ T.ty_exn body) (Type.of_term_unsafe @@ T.ty_exn body'));
              if n = 0 && T.equal body body' then t
              else (
                T.fun_l (CCList.take (List.length pref - n) pref) reduced
              )
            | T.Bind(_,_,_) -> t
            | T.App (_,[]) -> assert false
            | T.App (f, l) ->
              let f' = aux f in
              let l' = List.map aux l in
              if T.equal f f' && T.same_l l l'
              then t
              else T.app ~ty (aux f) (List.map aux l)
            | T.AppBuiltin (b,l) ->
              T.app_builtin ~ty b (List.map aux l)
          end)
      else t
    in
    let t' = aux t in
    t'

  let whnf t =
    Util.enter_prof prof_whnf;
    let t' = whnf_term t in
    Util.exit_prof prof_whnf;
    t'


  let beta_red_head t = 
    let pref, body = T.open_fun t in
    let res = T.fun_l pref (whnf body) in
    res

  let add_args_tail ~ty st args : state =
    { st with args = st.args @ args; ty; }

  let snf t =
    Util.enter_prof prof_snf;
    let t' = snf_rec t in
    Util.exit_prof prof_snf;
    t'

  let eta_expand t = Util.with_prof prof_eta_expand eta_expand_rec t

  let eta_reduce ?(full=true) t = Util.with_prof prof_eta_reduce (eta_reduce_aux ~full) t

end

module T = Term
module IT = InnerTerm

type term = Term.t

let whnf t =
  Inner.whnf (t : T.t :> IT.t) |> T.of_term_unsafe

let whnf_list t args =
  let st =
    Inner.st_of_term ~env:DBEnv.empty ~ty:(T.ty t : Type.t :> IT.t) (t:T.t :> IT.t)
  in
  let ty = Type.apply_unsafe (T.ty t) (args : T.t list :> IT.t list) in
  let st =
    Inner.add_args_tail st (args : T.t list :> IT.t list)
      ~ty:(ty : Type.t :> IT.t)
  in
  let st = Inner.whnf_rec st in
  let t' = Inner.term_of_st st |> T.of_term_unsafe in
  t'

let snf t =
  Inner.snf_rec (t:T.t :> IT.t) |> T.of_term_unsafe

let eta_expand t =
  Inner.eta_expand (t:T.t :> IT.t) |> T.of_term_unsafe
(*|> CCFun.tap (fun t' ->
  if t != t' then Format.printf "@[eta_expand `%a`@ into `%a`@]@." T.pp t T.pp t')*)

let eta_reduce ?(full=true) t =
  Inner.eta_reduce ~full (t:T.t :> IT.t) |> T.of_term_unsafe
(*|> CCFun.tap (fun t' ->
  if t != t' then Format.printf "@[eta_reduce `%a`@ into `%a`@]@." T.pp t T.pp t')*)

let beta_red_head t =
  Inner.beta_red_head (t:T.t :> IT.t) |> T.of_term_unsafe

let rec is_lambda_pattern t = match T.view (whnf t) with
  | T.AppBuiltin (_, ts) -> List.for_all is_lambda_pattern ts
  | T.DB _ | T.Var _ | T.Const _ -> true
  | T.App (hd, args) -> if T.is_var hd 
    then all_distinct_bound args 
    else List.for_all is_lambda_pattern args 
  | T.Fun (_, body) -> is_lambda_pattern body
and all_distinct_bound args =
  List.map (fun arg -> match T.view (eta_reduce arg) with T.DB i -> Some i | _ -> None) args
  |> OptionSet.of_list
  |> (fun set -> not (OptionSet.mem None set) && OptionSet.cardinal set = List.length args)

let rec is_properly_encoded t = match T.view t with
  | Var _ | DB _ | Const _ -> true
  | AppBuiltin (hd,l) when Builtin.equal hd Builtin.ForallConst 
                        || Builtin.equal hd Builtin.ExistsConst ->
    let res = begin match l with
      | [body] -> let ty = Term.ty body in
        Type.is_fun ty && Type.returns_prop ty
      | _ -> false end in
    if not res then CCFormat.printf "Failed for %a.\n" T.pp t;
    res
  | AppBuiltin(_,l) -> List.for_all is_properly_encoded l
  | App (hd, l) -> List.for_all is_properly_encoded (hd::l)
  | Fun (_,u) -> is_properly_encoded u 

