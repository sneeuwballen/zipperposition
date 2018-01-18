
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Higher-Order Unification} *)

module RW = Rewrite
module T = Term
module US = Unif_subst

let stat_unif_calls = Util.mk_stat "ho_unif.calls"
let stat_unif_steps = Util.mk_stat "ho_unif.steps"

let prof_norm_subst = Util.mk_profiler "ho_unif.norm_subst"

let section = Util.Section.make "ho_unif"

type term = Term.t

type penalty = int
(** penalty on the search space *)

type pair = Type.t list * term * term

(* options *)

let default_fuel = ref 15

let enable_norm_subst = ref true

(* number of ty and non-ty arguments *)
let term_arity args =
  args
  |> Util.take_drop_while (fun t -> T.is_type t)
  |> CCPair.map List.length List.length

let enum_prop ?(mode=`Full) ((v:Term.var), sc_v) ~offset : (Subst.t * penalty) list =
  let ty_v = HVar.ty v in
  let n, ty_args, ty_ret = Type.open_poly_fun ty_v in
  assert (Type.is_prop ty_ret);
  if n>0 then [] (* FIXME: what to do? *)
  else (
    (* local variables to build the λ-term *)
    let vars = List.mapi (fun i ty -> HVar.make ~ty i) ty_args in
    (* projection with "¬": [λvars. ¬ (F vars)] *)
    let l_not = match mode with
      | `None -> None
      | `Neg | `Full ->
        let f = HVar.make offset ~ty:ty_v in
        T.fun_of_fvars vars
          (T.Form.not_ (T.app (T.var f) (List.map T.var vars)))
        |> CCOpt.return
    (* projection with "∧": [λvars. (F1 vars) ∧ (F2 vars)] *)
    and l_and = match mode with
      | `Neg | `None -> None
      | `Full ->
        let f = HVar.make offset ~ty:ty_v in
        let g = HVar.make (offset+1) ~ty:ty_v in
        T.fun_of_fvars vars
          (T.Form.and_
             (T.app (T.var f) (List.map T.var vars))
             (T.app (T.var g) (List.map T.var vars)))
        |> CCOpt.return
    (* projection with "=": [λvars. (F1 vars) = (F2 vars)]
       where [F1 : Πa. ty_args -> a] *)
    and l_eq = match mode with
      | `Neg | `None -> None
      | `Full ->
        let a = HVar.make offset ~ty:Type.tType in
        let ty_fun = Type.arrow ty_args (Type.var a) in
        let f = HVar.make (offset+1) ~ty:ty_fun in
        let g = HVar.make (offset+2) ~ty:ty_fun in
        T.fun_of_fvars vars
          (T.Form.eq
             (T.app (T.var f) (List.map T.var vars))
             (T.app (T.var g) (List.map T.var vars)))
        |> CCOpt.return
    in
    CCList.filter_map
      (fun (o,penalty) -> match o with
         | None -> None
         | Some t ->
           assert (T.DB.is_closed t);
           let subst = Subst.FO.bind' Subst.empty (v,sc_v) (t,sc_v) in
           Some (subst, penalty))
      [ l_not, 2;
        l_and, 5;
        l_eq, 10;
      ]
  )

let pp_pair out ((env,t,u):pair) =
  Format.fprintf out "(@[@[`%a` =?=@]@ `%a`@ :env [@[%a@]]@])"
    T.pp t T.pp u (Util.pp_list ~sep:", " Type.pp) env

module U = struct
  type pb = {
    pairs: pair list;
    subst: US.t;
    penalty: penalty;
    offset: int;
  }

  type state = {
    sc: Scoped.scope;
    mutable fuel: int;
    queue: pb Queue.t;
    offset0: int; (* initial offset *)
    mutable sols: (US.t * penalty) list; (* totally solved *)
  }

  let empty sc fuel offset =
    { sc; fuel; queue=Queue.create(); sols=[]; offset0=offset; }

  let add (st:state) pb : unit = Queue.push pb st.queue

  let pp_pb out (pb:pb) =
    Format.fprintf out "(@[pb :subst %a@ :pairs (@[<hv>%a@])@])"
      US.pp pb.subst (Util.pp_list ~sep:" " pp_pair) pb.pairs

  let pp out (t:state): unit =
    Format.fprintf out
      "(@[<hv2>ho_unif_pb@ %a@])"
      (Util.pp_seq ~sep:" " pp_pb) (Sequence.of_queue t.queue)

  (** {6 normalization of pairs} *)

  type pair_kind =
    | P_rigid_rigid
    | P_flex_rigid
    | P_flex_flex

  let classify_pair (_,t,u) = match T.is_ho_app t, T.is_ho_app u with
    | false, false -> P_rigid_rigid
    | false, true
    | true, false -> P_flex_rigid
    | true, true -> P_flex_flex

  let is_flex_flex p = classify_pair p = P_flex_flex

  (* comparison of pairs that put the flex/rigid or rigid/rigid in front *)
  let cmp_pairs p1 p2 : int =
    let hardness =
      function P_rigid_rigid -> 0 | P_flex_rigid -> 1 | P_flex_flex -> 2
    in
    CCOrd.int (classify_pair p1 |> hardness) (classify_pair p2 |> hardness)

  let whnf_deref (subst:US.t) (t,sc) =
    let t = match T.view t with
      | T.Var _ -> US.FO.deref subst (t,sc) |> fst
      | T.App (f, l) when T.is_var f ->
        T.app (US.FO.deref subst (f,sc) |> fst) l
      | _ -> t
    in
    Lambda.whnf t

  let mk_pairs env l1 l2 = List.map2 (fun t u -> env, t, u) l1 l2

  (* perform syntactic unification aggressively on rigid/rigid pairs *)
  let flatten_rigid_rigid sc subst pairs : pair list option =
    try
      let rec aux acc l = match l with
        | [] -> acc
        | (env,t, u) :: tail ->
          (* deref+normalize terms *)
          let t = whnf_deref subst (t,sc) in
          let u = whnf_deref subst (u,sc) in
          begin match T.Classic.view t, T.Classic.view u with
            | _ when T.equal t u -> aux acc tail (* drop trivial *)
            | T.Classic.App (id1, l1), T.Classic.App (id2, l2) ->
              if ID.equal id1 id2 && List.length l1 = List.length l2
              then aux acc (mk_pairs env l1 l2 @ tail)
              else raise Exit (* failure *)
            | T.Classic.AppBuiltin (b1,l1), T.Classic.AppBuiltin (b2,l2) ->
              if Builtin.equal b1 b2 && List.length l1=List.length l2
              then aux acc (mk_pairs env l1 l2 @ tail)
              else raise Exit
            | _ -> aux ((env,t,u) :: acc) tail
          end
      in
      Some (aux [] pairs)
    with Exit ->
      None

  let mk_pb ~subst ~penalty ~offset pairs : pb =
    { subst; penalty; offset; pairs; }

  (* normalize problem w.r.t rigid/rigid equations *)
  let normalize_pb sc (pb:pb): pb option =
    begin match flatten_rigid_rigid sc pb.subst pb.pairs with
      | None -> None
      | Some pairs ->
        let pairs = List.sort cmp_pairs pairs in
        Some { pb with pairs; }
    end

  (** {6 Main loop} *)

  let mk_fresh_var offset ty = offset+1, HVar.make offset ~ty

  let mk_fresh_vars offset ty_l = CCList.fold_map mk_fresh_var offset ty_l

  (* unify [v args] and [t], where [t] is a lambda-term.
     If [t = λy1…yn. body], then the new pair is [v' args…y1…yn = body]
     where [v = λy1…yn. v' args…y1…yn] *)
  let unif_lambda ~offset env v args t : pair list * int =
    assert (T.is_fun t);
    let ty_t_args, t_body = T.open_fun t in
    assert (ty_t_args<>[]);
    assert (not (T.is_fun t_body));
    let all_args = List.map T.ty args @ ty_t_args in
    (* allocate new variable *)
    let offset, v' =
      let ty_v' = Type.arrow all_args (T.ty t_body) in
      mk_fresh_var offset ty_v'
    in
    (* bind [v] to [λavars…y1…yn. v' avars…y1…yn] where [avars] are fresh
       variables standing for [args] *)
    let bind_v =
      let n = List.length all_args in
      let rhs =
        T.app (T.var v')
          (List.mapi (fun i ty -> T.bvar ~ty (n-i-1)) all_args)
        |> T.fun_l all_args
      in
      env, T.var v, rhs
    (* unify [v' args…t_vars = t_body] *)
    and new_pair_body =
      let n = List.length ty_t_args in
      let lhs =
        T.app (T.var v')
          (List.map (T.DB.shift n) args @
             List.mapi (fun i ty -> T.bvar ~ty (n-i-1)) ty_t_args) in
      ty_t_args @ env, lhs, t_body
    in
    let new_pairs = [ bind_v; new_pair_body ] in
    new_pairs, offset

  let delay_pair (p:pair) sc : Unif_constr.t =
    let env, t1, t2 = p in
    let t1 = T.fun_l env t1 in
    let t2 = T.fun_l env t2 in
    let tags = [Proof.Tag.T_ho] in
    Unif_constr.FO.make ~tags (t1,sc)(t2,sc)

  (* unify [v args = t], where [t] is rigid *)
  let unif_rigid ~sc ~subst ~offset env v args t : (pair list * _ * _ * _) Sequence.t =
    assert (args<>[]);
    (* eta-expand locally *)
    let n_params, ty_args, ty_ret = Type.open_poly_fun (T.ty t) in
    assert (n_params=0);
    let n_ty_args = List.length ty_args in
    let all_ty_args = List.map T.ty args @ ty_args in
    let hd_t = T.head_term t in
    (* bound variables for [ty_args] *)
    let vars_right =
      List.mapi (fun i ty -> T.bvar ~ty (n_ty_args-i-1)) ty_args
    (* bound variables that abstract over [args].
       Careful about shifting them, they are inside the scope of [vars_right]. *)
    and vars_left =
      let n = n_ty_args + List.length args in
      List.mapi (fun i arg -> T.bvar ~ty:(T.ty arg) (n-i-1)) args
    in
    let all_vars = vars_left @ vars_right in
    (* now unify [v args…vars_right = t vars_right] *)
    let rhs = T.app (T.DB.shift n_ty_args t) vars_right in
    let lhs_args = List.map (T.DB.shift n_ty_args) args @ vars_right in
    (* projections: if [k]-th element of [args…vars] has type [τ1…τm → ty_ret],
       then we can try [v := λx1…xn. x_k (F1 x1…xn)…(Fm x1…xn)]
       where the [F] are fresh,
       and return the pair [arg_k (F1 x1…xn)…(Fm x1…xn) = t args] *)
    let proj =
      Sequence.of_list all_ty_args |> Util.seq_zipi
      |> Sequence.filter_map
        (fun (i,ty_arg_i) ->
           let ty_args_i, ty_ret_i = Type.open_fun ty_arg_i in
           try
             let subst = Unif.Ty.unify_full ~subst (ty_ret_i,sc) (ty_ret,sc) in
             (* now make fresh variables for [ty_args_i] *)
             let offset, f_vars =
               ty_args_i
               |> List.map (Type.arrow all_ty_args)
               |> mk_fresh_vars offset
             in
             (* [λall_vars. (F1 all_vars)…(Fm all_vars)] *)
             let lambda =
               let f_vars_applied =
                 List.map (fun f_var -> T.app (T.var f_var) all_vars) f_vars
               in
               T.app (List.nth all_vars i) f_vars_applied
               |> T.fun_l (List.map T.ty all_vars)
             (* [x_k (F1 args…vars_right)…(Fm args…vars_right] *)
             and lhs =
               let f_vars_applied =
                 List.map (fun f_var -> T.app (T.var f_var) lhs_args) f_vars
               in
               T.app (List.nth lhs_args i) f_vars_applied
             in
             let subst = US.FO.bind subst (v,sc) (lambda,sc) in
             Some ([ty_args@env,lhs,rhs],subst,offset,"proj")
           with Unif.Fail ->
             None)
    (* imitate: if [t=f u1…um],
       create new variables [F1…Fm] and try
       [v := λall_vars. f (F1 all_vars)…(Fm all_vars)] *)
    and imitate = match T.view hd_t with
      | T.AppBuiltin (b,l) when vars_right=[] && T.args t=[] ->
        (* imitate builtin *)
        let ty_params, l = CCList.take_drop_while T.is_type l in
        let ty_args_t = List.map T.ty l in
        let offset, f_vars =
          ty_args_t
          |> List.map (Type.arrow all_ty_args)
          |> mk_fresh_vars offset
        in
        (* [λall_vars. b (F1 all_vars)…(Fm all_vars)] *)
        let lambda =
          let f_vars_applied =
            List.map (fun f_var -> T.app (T.var f_var) all_vars) f_vars
          in
          T.app_builtin ~ty:(T.ty t) b f_vars_applied
          |> T.fun_l (List.map T.ty all_vars)
        (* [b (F1 args…vars_right)…(Fm args…vars_right)] *)
        and lhs =
          let f_vars_applied =
            List.map (fun f_var -> T.app (T.var f_var) lhs_args) f_vars
          in
          T.app_builtin ~ty:(T.ty t) b (ty_params @ f_vars_applied)
        in
        (* imitate constant *)
        let subst = US.FO.bind subst (v,sc) (lambda,sc) in
        Sequence.return ([ty_args@env, lhs, rhs],subst,offset,"imitate_b")
      | T.Const _ ->
        (* now make fresh variables as arguments of [id]. Each variable
           is parametrized by [all_vars] and returns the type of the k-th arg *)
        let t_mono = T.head_term_mono t in
        let n, ty_args_t, _ = Type.open_poly_fun (T.ty t_mono) in
        assert (n=0);
        let offset, f_vars =
          ty_args_t
          |> List.map (Type.arrow all_ty_args)
          |> mk_fresh_vars offset
        in
        (* [λall_vars. f (F1 all_vars)…(Fm all_vars)] *)
        let lambda =
          let f_vars_applied =
            List.map (fun f_var -> T.app (T.var f_var) all_vars) f_vars
          in
          T.app t_mono f_vars_applied
          |> T.fun_l (List.map T.ty all_vars)
        (* [f (F1 args…vars_right)…(Fm args…vars_right)] *)
        and lhs =
          let f_vars_applied =
            List.map (fun f_var -> T.app (T.var f_var) lhs_args) f_vars
          in
          T.app t_mono f_vars_applied
        in
        let subst = US.FO.bind subst (v,sc) (lambda,sc) in
        Sequence.return ([ty_args@env,lhs,rhs],subst,offset,"imitate")
      | _ ->
        Sequence.empty
    in
    Sequence.append imitate proj

  (* TODO: flex/flex (if they remain, then it means there are only flex/flex
     and we can bind to [λx1…xn. A] where [A] is fresh *)

  (* TODO: special handling of prop-typed pairs (list of true cases/list of
     false cases? subsume elim_pred_var?) *)

  (* TODO: introduce `if/then/else` for imitating several distinct
     symbols: [F a = f t, F b = g u] could be imitation with
     [F := λx. if x=a then f (F1 x) else if x=b then g (F2 x) else F3 x]
     and constraint [a != b]
     and new pairs [(F1 a = t), (F2 b = u)]
  *)

  (* main unification loop *)
  let unif_loop (st:state): unit =
    let sc = st.sc in
    let add_sol subst penalty =
      Util.debugf ~section 5 "(@[add_sol@ :subst %a@])" (fun k->k US.pp subst);
      st.sols <- (subst, penalty) :: st.sols
    in
    while st.fuel > 0 && not (Queue.is_empty st.queue) do
      let pb = Queue.pop st.queue in
      let pb = normalize_pb sc pb in
      begin match pb with
        | None -> () (* fail *)
        | Some {pairs=[]; subst; penalty; _} ->
          (* total solution! *)
          add_sol subst penalty;
        | Some ({penalty; offset; subst; pairs=(env,t1,t2) :: pairs_tl} as pb) ->
          (* try to unify the first pair *)
          Util.debugf ~section 5 "(@[ho_unif.try_pair %a@ :subst %a@])"
            (fun k->k pp_pair (env,t1,t2) US.pp subst);
          begin
            try
              let fail() = raise Unif.Fail in
              let consume_fuel() = st.fuel <- st.fuel - 1 in
              let push_new ~penalty ~subst ~offset rule pairs : unit =
                (* unify types of pairs *)
                let subst =
                  List.fold_left
                    (fun subst (_,t,u) ->
                       Unif.Ty.unify_full ~subst (T.ty t,sc) (T.ty u,sc))
                    subst pairs
                in
                let pb' =
                  mk_pb ~penalty ~subst ~offset (pairs @ pairs_tl)
                  |> normalize_pb sc
                in
                begin match pb' with
                  | None ->  ()
                  | Some pb' ->
                    Util.debugf ~section 5 "(@[ho_unif.push@ :rule %s@ %a@])"
                      (fun k->k rule pp_pb pb');
                    Queue.push pb' st.queue
                end
              in
              (* unify types *)
              let subst = Unif.Ty.unify_full ~subst (T.ty t1,sc) (T.ty t2,sc) in
              (* unify terms *)
              let t1 = whnf_deref subst (t1,sc) in
              let t2 = whnf_deref subst (t2,sc) in
              let hd1, l1 = T.as_app t1 in
              let hd2, l2 = T.as_app t2 in
              begin match T.view hd1, T.view hd2 with
                | _ when T.equal t1 t2 ->
                  push_new "triv" ~penalty ~offset ~subst [] (* trivial *)
                | T.Const id1, T.Const id2 ->
                  if ID.equal id1 id2 && List.length l1=List.length l2
                  then (
                    (* unify arguments pairwise *)
                    let new_pairs = mk_pairs env l1 l2 in
                    push_new "rigid" ~penalty ~offset ~subst new_pairs
                  ) else fail()
                | T.DB a, T.DB b ->
                  if a=b then push_new "db" ~penalty ~offset ~subst [] else fail()
                | T.Var _, _ when l1=[] ->
                  (* just bind or fail (with occur-check) *)
                  let subst = Unif.FO.unify_full ~subst (t1,0) (t2,0) in
                  push_new "bind" ~penalty ~offset ~subst []
                | _, T.Var _ when l2=[] ->
                  (* just bind or fail (with occur-check) *)
                  let subst = Unif.FO.unify_full ~subst (t1,0) (t2,0) in
                  push_new "bind" ~penalty ~offset ~subst []
                | T.AppBuiltin (b1, l1'), T.AppBuiltin (b2,l2') ->
                  assert (l1=[]);
                  assert (l2=[]);
                  if Builtin.equal b1 b2 && List.length l1'=List.length l2'
                  then (
                    (* unify arguments pairwise *)
                    let new_pairs = mk_pairs env l1 l2 in
                    push_new "rigid" ~penalty ~offset ~subst new_pairs
                  ) else fail()
                | T.Var v, T.Fun _ ->
                  (* eta-expand *)
                  assert (l2=[]); (* whnf *)
                  let pairs, offset = unif_lambda ~offset env v l1 hd2 in
                  push_new "bind" ~penalty ~subst ~offset pairs
                | T.Fun _, T.Var v ->
                  (* eta-expand *)
                  assert (l1=[]); (* whnf *)
                  let pairs, offset = unif_lambda ~offset env v l2 hd1 in
                  push_new "bind" ~penalty ~subst ~offset pairs
                | T.Fun _, T.Fun _ ->
                  (* eta-expand and unify bodies: to unify [λx.t = λx.u]
                     just unify [t=u] *)
                  assert (l1 = []);
                  assert (l2 = []);
                  let n_params, ty_args, _ = Type.open_poly_fun (T.ty t1) in
                  assert (n_params=0);
                  (* apply to DB indices *)
                  let n = List.length ty_args in
                  let args = List.mapi (fun i ty -> T.bvar ~ty (n-i-1)) ty_args in
                  let pair = ty_args @ env, T.app hd1 args, T.app hd2 args in
                  push_new "eta" ~penalty ~subst ~offset [pair]
                | T.Var v, (T.Const _ | T.AppBuiltin _ | T.DB _) ->
                  (* project/imitate *)
                  assert (l1<>[]);
                  consume_fuel();
                  unif_rigid ~sc ~subst ~offset env v l1 t2
                    (fun (pairs,subst,offset,rule) ->
                       push_new rule ~penalty ~subst ~offset pairs);
                | (T.Const _ | T.AppBuiltin _ | T.DB _), T.Var v ->
                  (* project/imitate *)
                  assert (l2<>[]);
                  consume_fuel();
                  unif_rigid ~sc ~subst ~offset env v l2 t1
                    (fun (pairs,subst,offset,rule) ->
                       push_new rule ~penalty ~subst ~offset pairs);
                | T.Var _, T.Var _ ->
                  (* flex/flex: all should be flex/flex *)
                  Util.debugf ~section 5
                    "(@[ho_unif.all_flex_flex@ %a@])"
                    (fun k->k pp_pb pb);
                  assert (List.for_all is_flex_flex pairs_tl);
                  (* delay all flex/flex pairs *)
                  let subst =
                    List.fold_left
                      (fun subst p -> US.add_constr (delay_pair p sc) subst)
                      pb.subst pb.pairs
                  in
                  add_sol subst pb.penalty
                | T.App _, _ | _, T.App _ -> assert false (* heads *)
                | T.Const _, _
                | T.Fun _, _
                | T.AppBuiltin _, _
                | T.DB _, _
                  -> fail()
              end
            with Unif.Fail ->
              Util.debugf ~section 5 "(@[ho_unif.drop_pb@ %a@])"
                (fun k->k pp_pb pb);
              () (* drop pair *)
          end
      end
    done

  (* normalize substitution *)
  let norm_subst_ offset sc (us:US.t) () : US.t =
    US.map_subst us
      ~f:(fun subst ->
        Subst.normalize subst
        |> Subst.FO.filter
          (fun (v,sc_v) (t,sc_t) ->
             (* filter out intermediate variables. They are the ones
                that have an index >= offset,
                and only point to other intermediate vars *)
             let is_fvar (v,sc_v) =
               sc_v = sc && HVar.id v >= offset &&
               not (Type.is_tType (HVar.ty v))
             in
             not (is_fvar (v,sc_v)) ||
             (T.Seq.vars t
              |> Sequence.exists (fun v' -> not (is_fvar (v',sc_t)))))
        |> Subst.FO.map Lambda.snf
      )

  let norm_subst offset sc us =
    if !enable_norm_subst
    then Util.with_prof prof_norm_subst (norm_subst_ offset sc us) ()
    else us

  let apply_subst pairs us =
    let renaming = Subst.Renaming.create() in
    let subst = Unif_subst.subst us in
    let pairs =
      List.map
        (fun (env,t,u) ->
           let t = Subst.FO.apply renaming subst (T.fun_l env t,0) in
           let u = Subst.FO.apply renaming subst (T.fun_l env u,0) in
           [], t, u)
        pairs
    in
    pairs, renaming

  (* extract the (partial) solutions *)
  let get_solutions (st:state): (pair list * US.t * penalty * Subst.Renaming.t) list =
    let sols1 =
      st.sols
      |> List.rev_map (fun (subst,p) -> [], norm_subst st.offset0 st.sc subst, p, Subst.Renaming.create())
    and sols2 =
      st.queue
      |> Sequence.of_queue
      |> Sequence.map
        (fun pb ->
           let pairs, renaming = apply_subst pb.pairs pb.subst in
           pairs, norm_subst st.offset0 st.sc pb.subst, pb.penalty, renaming)
      |> Sequence.to_rev_list
    in
    List.rev_append sols1 sols2
end

let unif_pairs ?(fuel= !default_fuel) (pairs,sc) ~offset : _ list =
  let st = U.empty sc fuel offset in
  U.add st (U.mk_pb ~offset ~penalty:0 ~subst:US.empty pairs);
  U.unif_loop st;
  U.get_solutions st

let () =
  Options.add_opts
    [ "--ho-unif-fuel", Arg.Set_int default_fuel, " default amount of fuel for HO unification";
      "--ho-unif-norm", Arg.Set enable_norm_subst, " normalize substitutions in HO unif";
      "--no-ho-unif-norm", Arg.Clear enable_norm_subst, " do not normalize substitutions in HO unif";
    ]
