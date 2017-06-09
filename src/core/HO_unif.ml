
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Higher-Order Unification} *)

module RW = Rewrite
module T = Term

let stat_unif_calls = Util.mk_stat "ho_unif.calls"
let stat_unif_steps = Util.mk_stat "ho_unif.steps"

let section = Util.Section.make "ho_unif"

type term = Term.t

type penalty = int
(** penalty on the search space *)

(* number of ty and non-ty arguments *)
let term_arity args =
  args
  |> Util.take_drop_while (fun t -> T.is_type t)
  |> CCPair.map List.length List.length

let enum_prop ((v:Term.var), sc_v) ~offset : (Subst.t * penalty) list =
  let ty_v = HVar.ty v in
  let n, ty_args, ty_ret = Type.open_poly_fun ty_v in
  assert (Type.is_prop ty_ret);
  if n>0 then [] (* FIXME: what to do? *)
  else (
    (* local variables to build the λ-term *)
    let vars = List.mapi (fun i ty -> HVar.make ~ty i) ty_args in
    (* projection with "¬": [λvars. ¬ (F vars)] *)
    let l_not =
      let f = HVar.make offset ~ty:ty_v in
      T.fun_of_fvars vars
        (T.Form.not_ (T.app (T.var f) (List.map T.var vars)))
    (* projection with "∧": [λvars. (F1 vars) ∧ (F2 vars)] *)
    and l_and =
      let f = HVar.make offset ~ty:ty_v in
      let g = HVar.make (offset+1) ~ty:ty_v in
      T.fun_of_fvars vars
        (T.Form.and_
           (T.app (T.var f) (List.map T.var vars))
           (T.app (T.var g) (List.map T.var vars)))
    (* projection with "=": [λvars. (F1 vars) = (F2 vars)]
       where [F1 : Πa. ty_args -> a] *)
    and l_eq =
      let a = HVar.make offset ~ty:Type.tType in
      let ty_fun = Type.arrow ty_args (Type.var a) in
      let f = HVar.make (offset+1) ~ty:ty_fun in
      let g = HVar.make (offset+2) ~ty:ty_fun in
      T.fun_of_fvars vars
        (T.Form.eq
           (T.app (T.var f) (List.map T.var vars))
           (T.app (T.var g) (List.map T.var vars)))
    in
    List.map
      (fun (t,penalty) ->
         assert (InnerTerm.DB.closed (t: T.t :> InnerTerm.t));
         let subst =
           Subst.FO.of_list [((v:>InnerTerm.t HVar.t),sc_v),(t,sc_v)]
         in
         subst, penalty)
      [ l_not, 2;
        l_and, 4;
        l_eq, 5;
      ]
  )

let default_fuel = 40

module U = struct
  type pair = term * term

  type pb = {
    pairs: pair list;
    subst: Subst.t;
    penalty: penalty;
    offset: int;
  }

  type state = {
    sc: Scoped.scope;
    mutable fuel: int;
    queue: pb Queue.t;
    mutable sols: (Subst.t * penalty) list; (* totally solved *)
  }

  let empty sc fuel = { sc; fuel; queue=Queue.create(); sols=[]; }

  let add (st:state) pb : unit = Queue.push pb st.queue

  let pp_pair out (t,u) =
    Format.fprintf out "(@[@[%a =?=@]@ @[%a@]@])" T.pp t T.pp u

  let pp_pb out (pb:pb) =
    Format.fprintf out "(@[pb :subst %a@ :pairs (@[<hv>%a@])@])"
      Subst.pp pb.subst (Util.pp_list ~sep:" " pp_pair) pb.pairs

  let pp out (t:state): unit =
    Format.fprintf out
      "(@[<hv2>ho_unif_pb@ %a@])"
      (Util.pp_seq ~sep:" " pp_pb) (Sequence.of_queue t.queue)

  (** {6 normalization of pairs} *)

  type pair_kind =
    | P_rigid_rigid
    | P_flex_rigid
    | P_flex_flex

  let classify_pair (t,u) = match T.is_ho_app t, T.is_ho_app u with
    | false, false -> P_rigid_rigid
    | false, true
    | true, false -> P_flex_rigid
    | true, true -> P_flex_flex

  (* comparison of pairs that put the flex/rigid or rigid/rigid in front *)
  let cmp_pairs p1 p2 : int =
    let hardness =
      function P_rigid_rigid -> 0 | P_flex_rigid -> 1 | P_flex_flex -> 2
    in
    CCOrd.int (classify_pair p1 |> hardness) (classify_pair p2 |> hardness)

  (* perform syntactic unification aggressively on rigid/rigid pairs *)
  let flatten_rigid_rigid sc subst pairs : pair list option =
    try
      let rec aux acc l = match l with
        | [] -> acc
        | (t, u) :: tail ->
          (* deref+normalize terms *)
          let t = Subst.FO.deref subst (t,sc) |> fst |> Lambda.whnf in
          let u = Subst.FO.deref subst (u,sc) |> fst |> Lambda.whnf in
          begin match T.Classic.view t, T.Classic.view u with
            | T.Classic.App (id1, l1), T.Classic.App (id2, l2) ->
              if ID.equal id1 id2 && List.length l1 = List.length l2
              then aux acc (List.combine l1 l2 @ tail)
              else raise Exit (* failure *)
            | _ -> aux ((t,u) :: acc) tail
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

  (* unify [v args] and [t], where [t] is a lambda-term.
     If [t = λy1…yn. body], then the new pair is [v args…y1…yn = body] *)
  let unif_lambda ~offset v args t : pair * int =
    assert (T.is_fun t);
    let t_vars, t_body, offset = T.open_fun ~offset t in
    assert (t_vars<>[]);
    assert (not (T.is_fun t_body));
    (* the new pair is [v args…t_vars = t_body] *)
    let new_pair = T.app (T.var v) (args @ List.map T.var t_vars), t_body in
    new_pair, offset

  let mk_fresh_vars offset ty_l =
    CCList.fold_map
      (fun offset ty -> offset+1, HVar.make offset ~ty)
      offset ty_l

  (* unify [v args = t], where [t] is rigid *)
  let unif_rigid ~sc ~subst ~offset v args t : (pair list * Subst.t * int) Sequence.t =
    assert (args<>[]);
    (* eta-expand locally *)
    let n, ty_args, ty_ret = Type.open_poly_fun (T.ty t) in
    assert (n=0);
    let offset, vars_left = mk_fresh_vars offset (List.map T.ty args) in
    let offset, vars_right = mk_fresh_vars offset ty_args in
    let all_vars = vars_left @ vars_right in
    let all_vars_t = List.map T.var all_vars in
    let hd_t = T.head_term t in
    (* now unify [v args…vars_right = t vars_right] *)
    let rhs = T.app t (List.map T.var vars_right) in
    let lhs_args = args @ List.map T.var vars_right in
    let all_ty_args = List.map T.ty args @ ty_args in
    (* projections: if [k]-th element of [args…vars] has type [τ1…τm → ty_ret],
       then we can try [v := λx1…xn. x_k (F1 x1…xn)…(Fm x1…xn)]
       where the [F] are fresh,
       and return the pair [arg_k (F1 x1…xn)…(Fm x1…xn) = t args] *)
    let proj =
      Sequence.of_list all_ty_args |> Sequence.zip_i |> Sequence.zip
      |> Sequence.filter_map
        (fun (i,ty_arg_i) ->
           let ty_args_i, ty_ret_i = Type.open_fun ty_arg_i in
           try
             let subst = Unif.Ty.unify_syn ~subst (ty_ret_i,sc) (ty_ret,sc) in
             (* now make fresh variables for [ty_args_i] *)
             let offset, f_vars =
               ty_args_i
               |> List.map (Type.arrow all_ty_args)
               |> mk_fresh_vars offset
             in
             (* [λall_vars. (F1 all_vars)…(Fm all_vars)] *)
             let lambda =
               let f_vars_applied =
                 List.map (fun f_var -> T.app (T.var f_var) all_vars_t) f_vars
               in
               T.fun_of_fvars all_vars
                 (T.app (List.nth all_vars_t i) f_vars_applied)
             (* [x_k (F1 args…vars_right)…(Fm args…vars_right] *)
             and lhs =
               let f_vars_applied =
                 List.map (fun f_var -> T.app (T.var f_var) lhs_args) f_vars
               in
               T.app (List.nth lhs_args i) f_vars_applied
             in
             let subst = Subst.FO.bind' subst (v,sc) (lambda,sc) in
             Some ([lhs,rhs],subst,offset)
           with Unif.Fail ->
             None)
    (* imitate: if [t=f u1…um],
       create new variables [F1…Fm] and try
       [v := λall_vars. f (F1 all_vars)…(Fm all_vars)] *)
    and imitate = match T.view hd_t with
      | T.AppBuiltin _
        when vars_right=[] &&
             T.args t=[] &&
             not (T.var_occurs ~var:v t) ->
        (* imitate constant *)
        let lambda = T.fun_of_fvars all_vars rhs in
        let subst = Subst.FO.bind' subst (v,sc) (lambda,sc) in
        Sequence.return ([], subst, offset)
      | T.Const _ when not (T.var_occurs ~var:v t) ->
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
            List.map (fun f_var -> T.app (T.var f_var) all_vars_t) f_vars
          in
          T.fun_of_fvars all_vars (T.app t_mono f_vars_applied)
        (* [f (F1 args…vars_right)…(Fm args…vars_right)] *)
        and lhs =
          let f_vars_applied =
            List.map (fun f_var -> T.app (T.var f_var) lhs_args) f_vars
          in
          T.app t_mono f_vars_applied
        in
        let subst = Subst.FO.bind' subst (v,sc) (lambda,sc) in
        Sequence.return ([lhs,rhs], subst, offset)
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

  let whnf_deref subst (t,sc) =
    let t = match T.view t with
      | T.Var _ -> Subst.FO.deref subst (t,sc) |> fst
      | T.App (f, l) when T.is_var f ->
        T.app (Subst.FO.deref subst (f,sc) |> fst) l
      | _ -> t
    in
    Lambda.whnf t

  (* main unification loop *)
  let unif_loop (st:state): unit =
    let sc = st.sc in
    while st.fuel > 0 && not (Queue.is_empty st.queue) do
      let pb = Queue.pop st.queue in
      let pb = normalize_pb sc pb in
      begin match pb with
        | None -> () (* fail *)
        | Some {pairs=[]; subst; penalty; _} ->
          (* total solution! *)
          st.sols <- (subst, penalty) :: st.sols
        | Some ({penalty; offset; subst; pairs=(t1,t2) :: pairs_tl} as pb) ->
          (* try to unify the first pair *)
          Util.debugf ~section 5 "(@[ho_unif.try_pair %a@ :subst %a@])"
            (fun k->k pp_pair (t1,t2) Subst.pp subst);
          begin
            try
              let fail() = raise Unif.Fail in
              let consume_fuel() = st.fuel <- st.fuel - 1 in
              let push_new ~penalty ~subst ~offset pairs : unit =
                let pb' = mk_pb ~penalty ~subst ~offset (pairs @ pairs_tl) in
                Util.debugf ~section 5 "(@[ho_unif.push@ %a@])"
                  (fun k->k pp_pb pb');
                Queue.push pb' st.queue
              in
              (* unify types *)
              let subst = Unif.Ty.unify_syn ~subst (T.ty t1,sc) (T.ty t2,sc) in
              (* unify terms *)
              let t1 = whnf_deref subst (t1,sc) in
              let t2 = whnf_deref subst (t2,sc) in
              let hd1, l1 = T.as_app t1 in
              let hd2, l2 = T.as_app t2 in
              begin match T.view hd1, T.view hd2 with
                | _ when T.equal t1 t2 ->
                  push_new ~penalty ~offset ~subst [] (* trivial *)
                | T.Const id1, T.Const id2 ->
                  if ID.equal id1 id2 && List.length l1=List.length l2
                  then (
                    (* unify arguments pairwise *)
                    let new_pairs = List.combine l1 l2 in
                    push_new ~penalty ~offset ~subst new_pairs
                  ) else fail()
                | T.DB a, T.DB b ->
                  if a=b then push_new ~penalty ~offset ~subst [] else fail()
                | T.Var _, _ when l1=[] ->
                  (* just bind or fail (with occur-check) *)
                  let subst = Unif.FO.unify_syn ~subst (t1,0) (t2,0) in
                  push_new ~penalty ~offset ~subst []
                | _, T.Var _ when l2=[] ->
                  (* just bind or fail (with occur-check) *)
                  let subst = Unif.FO.unify_syn ~subst (t1,0) (t2,0) in
                  push_new ~penalty ~offset ~subst []
                | T.AppBuiltin (b1, l1'), T.AppBuiltin (b2,l2') ->
                  assert (l1=[]);
                  assert (l2=[]);
                  if Builtin.equal b1 b2 && List.length l1'=List.length l2'
                  then (
                    (* unify arguments pairwise *)
                    let new_pairs = List.combine l1 l2 in
                    push_new ~penalty ~offset ~subst new_pairs
                  ) else fail()
                | T.Var v, T.Fun _ ->
                  (* eta-expand *)
                  assert (l2=[]); (* whnf *)
                  let pair, offset = unif_lambda ~offset v l1 hd2 in
                  push_new ~penalty ~subst ~offset [pair]
                | T.Fun _, T.Var v ->
                  (* eta-expand *)
                  assert (l1=[]); (* whnf *)
                  let pair, offset = unif_lambda ~offset v l2 hd1 in
                  push_new ~penalty ~subst ~offset [pair]
                | T.Fun _, T.Fun _ ->
                  (* eta-expand and unify bodies: to unify [λx.t = λx.u]
                     just unify [t=u] *)
                  assert (l1 = []);
                  assert (l2 = []);
                  let n, ty_args, _ = Type.open_poly_fun (T.ty t1) in
                  assert (n=0);
                  (* apply to fresh variables *)
                  let offset, vars = mk_fresh_vars offset ty_args in
                  let vars = List.map T.var vars in
                  let pair = T.app hd1 vars, T.app hd2 vars in
                  push_new ~penalty ~subst ~offset [pair]
                | T.Var v, _ ->
                  (* project/imitate *)
                  assert (l1<>[]);
                  consume_fuel();
                  unif_rigid ~sc ~subst ~offset v l1 t2
                    (fun (pairs,subst,offset) ->
                       push_new ~penalty ~subst ~offset pairs);
                | _, T.Var v ->
                  (* project/imitate *)
                  assert (l2<>[]);
                  consume_fuel();
                  unif_rigid ~sc ~subst ~offset v l2 t1
                    (fun (pairs,subst,offset) ->
                       push_new ~penalty ~subst ~offset pairs);
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

  (* extract the (partial) solutions *)
  let get_solutions (st:state): (pair list * _ * _) list =
    let sols1 =
      st.sols
      |> List.rev_map (fun (subst,p) -> [], subst, p)
    and sols2 =
      st.queue
      |> Sequence.of_queue
      |> Sequence.map (fun pb -> pb.pairs, pb.subst, pb.penalty)
      |> Sequence.to_rev_list
    in
  List.rev_append sols1 sols2
end

let unif_pairs ?(fuel=default_fuel) (pairs,sc) ~offset : _ list =
  let st = U.empty sc fuel in
  U.add st (U.mk_pb ~offset ~penalty:0 ~subst:Subst.empty pairs);
  U.unif_loop st;
  U.get_solutions st
