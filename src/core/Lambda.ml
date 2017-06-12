
(* This file is free software. See file "license" for more details. *)

(** {1 Lambda-Calculus} *)

let prof_whnf = Util.mk_profiler "term.whnf"
let prof_snf = Util.mk_profiler "term.snf"

module T = Term

type term = Term.t

type state = {
  head: T.t;  (* not an app *)
  env: T.t DBEnv.t; (* env for the head *)
  args: T.t list; (* arguments, with their own env *)
}

(* evaluate term in environment *)
let eval_in_env_ env t = T.DB.eval env t

let normalize st = match T.view st.head with
  | T.App (f, l) ->
    (* the arguments in [l] might contain variables *)
    let l = List.rev_map (eval_in_env_ st.env) l in
    { st with head=f; args=List.rev_append l st.args; }
  | _ -> st

let st_of_term ~env t = {head=t; args=[]; env;} |> normalize

let term_of_st st =
  let f = eval_in_env_ st.env st.head in
  T.app f st.args

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
    | T.AppBuiltin _, _
    | T.Fun _, [] -> st
    | T.Fun (_, body), a :: args' ->
      (* beta-reduce *)
      Util.debugf 10 "(@[<2>beta-reduce@ @[%a@ %a@]@])"
        (fun k->k T.pp st.head T.pp a);
      let st' =
        { head=body;
          env=DBEnv.push st.env a;
          args=args';
        } |> normalize
      in
      whnf_rec st'
  end

let whnf_term t =
  let st = st_of_term ~env:DBEnv.empty t in
  let st = whnf_rec st in
  term_of_st st

let rec snf_rec ~env t =
  let ty = T.ty t in
  begin match T.view t with
    | T.App (f, l) ->
      let f' = snf_rec ~env f in
      let l' = List.map (snf_rec ~env) l in
      let t' = if T.equal f f' && T.same_l l l' then t else T.app f' l' in
      begin match T.view f' with
        | T.Fun _ ->
          let t' = whnf_term t' in
          (* beta reduce *)
          assert (not (T.equal t t'));
          assert (l<>[]);
          snf_rec ~env t'
        | _ -> t'
      end
    | T.AppBuiltin (b, l) ->
      let l' = List.map (snf_rec ~env) l in
      if T.same_l l l' then t else T.app_builtin ~ty b l'
    | T.Var _ | T.Const _ | T.DB _ -> t
    | T.Fun (ty_arg, body) ->
      let env = DBEnv.push_none env in
      let body' = snf_rec ~env body in
      if T.equal body body' then t else T.fun_ ty_arg body'
  end

let whnf t =
  Util.enter_prof prof_whnf;
  let t' = whnf_term t in
  Util.exit_prof prof_whnf;
  t'

let whnf_list t args =
  Util.enter_prof prof_whnf;
  let st = st_of_term ~env:DBEnv.empty t in
  let st = { st with args = st.args @ args; } in
  let st = whnf_rec st in
  let t' = term_of_st st in
  Util.exit_prof prof_whnf;
  t'

let snf t =
  Util.enter_prof prof_snf;
  let t' = snf_rec ~env:DBEnv.empty t in
  Util.exit_prof prof_snf;
  t'
