
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Lambda-Calculus} *)

let prof_beta_reduce = Util.mk_profiler "HO.beta_reduce"

type term = HOTerm.t

module T = HOTerm

type state = {
  head: T.t;  (* not an app *)
  args: T.t list;
  env: T.t DBEnv.t;
}

let st_of_term ~env t = match T.view t  with
  | T.App (f, l) -> { head=f; args=l; env; }
  | _ -> {head=t; args=[]; env; }

let set_head st t = match T.view t with
  | T.App (f, l) -> { st with head=f; args= l @ st.args; }
  | _ -> {st with head=t; }

let term_of_st st =
  let f = InnerTerm.DB.eval
    (st.env : T.t DBEnv.t :> InnerTerm.t DBEnv.t)
    (st.head : T.t :> InnerTerm.t) in
  let f = T.of_term_unsafe f in
  T.app f st.args

(* recursive reduction in call by value. [env] contains the environment for
    De Bruijn indexes. *)
let rec whnf_rec st =
  let ty = T.ty st.head in
  match T.view st.head, st.args with
  | T.App _, _ -> assert false
  | T.Var _, _
  | T.Const _, _ -> st
  | T.DB n, _ ->
      (* look for the possible binding for [n] *)
      begin match DBEnv.find st.env n with
        | None -> st
        | Some t' ->
            (* FIXME: equality modulo env? *)
            assert (Type.equal ty (T.ty t'));
            assert (InnerTerm.DB.closed (t' : T.t :> InnerTerm.t));
            (* must be closed, because it's already evaluated *)
            whnf_rec (set_head st t')
      end
  | T.AppBuiltin _, _
  | T.Record _, _
  | T.Multiset _, _
  | T.Forall _, _
  | T.Exists _, _
  | T.Lambda _, [] -> st
  | T.Lambda (_, body), a :: args' ->
      (* beta-reduce *)
      Util.debugf 4 "@[<2>beta-reduce:@ @[%a@ %a@]" (fun k->k T.pp st.head T.pp a);
      let st' = { st with
        env=DBEnv.push st.env a;
        args=args';
      } in
      let st' = set_head st' body in
      whnf_rec st'

let whnf t =
  Util.enter_prof prof_beta_reduce;
  let st = st_of_term ~env:DBEnv.empty t in
  let st = whnf_rec st in
  let t' = term_of_st st in
  Util.exit_prof prof_beta_reduce;
  t'

let whnf_list t args =
  Util.enter_prof prof_beta_reduce;
  let st = st_of_term ~env:DBEnv.empty t in
  let st = { st with args = st.args @ args; } in
  let st = whnf_rec st in
  let t' = term_of_st st in
  Util.exit_prof prof_beta_reduce;
  t'
