
(* This file is free software. See file "license" for more details. *)

(** {1 Lambda-Calculus} *)

let prof_whnf = Util.mk_profiler "term.whnf"
let prof_snf = Util.mk_profiler "term.snf"
let prof_eta_expand = Util.mk_profiler "term.eta_expand"
let prof_eta_reduce = Util.mk_profiler "term.eta_reduce"

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
        Util.debugf 10 "(@[<2>beta-reduce@ @[%a@ %a@]@])"
          (fun k->k T.pp st.head T.pp a);
        assert (T.equal ty_var (T.ty_exn a));
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

  let whnf_term ?(env=DBEnv.empty) t = match T.ty t with
    | T.NoType -> t
    | T.HasType ty ->
      let st = st_of_term ~ty ~env t in
      let st = whnf_rec st in
      term_of_st st

  let rec snf_rec t =
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
        end

  let eta_expand_rec t =
    let rec aux t = match T.ty t with
      | T.NoType -> t
      | T.HasType ty ->
        let n, ty_args, ty_ret = T.open_poly_fun ty in
        assert (n=0);
        (* first, WHNF *)
        let t = whnf_term t in
        (* see how many arguments are missing, and what type *)
        let args, body = T.open_bind Binder.Lambda t in
        let n_args = List.length ty_args in
        let n_missing = n_args - List.length args in
        if n_missing>0 then (
          Util.debugf 5 "(@[eta_expand_rec `%a`,@ missing %d args@ in %a@])"
            (fun k->k T.pp t n_missing (CCFormat.Dump.list T.pp) ty_args);
          (* missing args: suffix of length [n_missing] *)
          let missing_args = CCList.drop (n_args-n_missing) ty_args in
          (* shift body to accomodate for new binders *)
          let body = T.DB.shift n_missing body in
          (* build the fully-abstracted term *)
          let dbvars =
            List.mapi (fun i ty_arg -> T.bvar (n_missing-i-1) ~ty:ty_arg) missing_args
          in
          T.fun_l ty_args (T.app ~ty:ty_ret body dbvars)
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
              T.bind ~ty ~varty b (aux body')
          in
          T.fun_l ty_args body
        )
    in
    aux t

  (* do one step of eta-reduction *)
  let eta_reduce_rec t =
    let rec aux t =  match T.ty t with
      | T.NoType -> t
      | T.HasType ty ->
        begin match T.view t with
          | T.Var _ | T.DB _ | T.Const _ -> t
          | T.Bind (Binder.Lambda, _, {T.term=T.App (f, [{T.term=T.DB 0; _}]); _})
            when not (T.DB.contains f 0) ->
            (* [fun x. f x --> f] *)
            T.DB.unshift 1 f
          | T.Bind (b, varty, body) ->
            T.bind ~ty ~varty b (aux body)
          | T.App (_,[]) -> assert false
          | T.App (f, l) ->
            let f' = aux f in
            let l' = List.map aux l in
            if T.equal f f' && T.same_l l l'
            then t
            else T.app ~ty (aux f) (List.map aux l)
          | T.AppBuiltin (b,l) ->
            T.app_builtin ~ty b (List.map aux l)
        end
    in
    aux t

  let whnf t = match T.view t with
    | T.App (f, _) when T.is_lambda f ->
      Util.enter_prof prof_whnf;
      let t' = whnf_term t in
      Util.exit_prof prof_whnf;
      t'
    | _ -> t

  let add_args_tail ~ty st args : state =
    { st with args = st.args @ args; ty; }

  let snf t =
    Util.enter_prof prof_snf;
    let t' = snf_rec t in
    Util.exit_prof prof_snf;
    t'

  let eta_expand t = Util.with_prof prof_eta_expand eta_expand_rec t

  let eta_reduce t = Util.with_prof prof_eta_reduce eta_reduce_rec t
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

let eta_reduce t =
  Inner.eta_reduce (t:T.t :> IT.t) |> T.of_term_unsafe
(*|> CCFun.tap (fun t' ->
  if t != t' then Format.printf "@[eta_reduce `%a`@ into `%a`@]@." T.pp t T.pp t')*)
