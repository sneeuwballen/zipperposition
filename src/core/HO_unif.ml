
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

(** {2 Set of Combinators} *)

module Combinators = struct
  type rule = Rewrite.Term.rule * int
  (** A rule is a term rewrite rule, plus a penalty on the search space *)

  type rules = rule list

  type conv_fun = Type.t HVar.t list -> term -> term
  (* conversion from (toplevel) lambda to combinators *)

  type t = {
    name: string;
    rules: rules lazy_t;
    decls: (ID.t * Type.t) list lazy_t;
    conv: conv_fun lazy_t;
  }

  let rules t = Lazy.force t.rules
  let name t = t.name
  let decls t = Lazy.force t.decls

  let conv_lambda t vars body = Lazy.force t.conv vars body

  let all_ : t list ref = ref []

  let mk_ name rules decls conv : t =
    let c = { name; rules; decls; conv; } in
    if List.exists (fun c' -> c'.name = name) !all_ then (
      Util.invalid_argf "cannot have two sets of combinators named `%s`" name
    );
    all_ := c :: !all_;
    c

  module PT = STerm
  module TT = TypedSTerm

  let pp_rule out (rule:rule) =
    let r, pen = rule in
    Format.fprintf out "(@[<2>rule :penalty %d@ %a@])" pen RW.Term.Rule.pp r

  (* to be compiled into a set of rules and declarations *)
  type pre_rule = {
    pr_name: string;
    pr_penalty: int;
    pr_ty: PT.t;
    pr_rules: (PT.t list * PT.t) list;
  }

  (* build from a list of pairs of terms (interpreted as [lhs->rhs])
     and a conversion function *)
  let of_rules
      rules_name
      (gen_rules:unit -> pre_rule list)
      (conv_f:(ID.t*Type.t) list -> conv_fun) : t =
    let rules_and_decls = lazy (
      let ctx =
        TypeInference.Ctx.create ~on_var:`Infer ()
      in
      let conv = T.Conv.create () in
      (* do type inference and conversions for each rule *)
      gen_rules ()
      |> List.map
        (fun pr ->
           let id = ID.make pr.pr_name in
           (* perform conversion… *)
           let ty_id = TypeInference.infer_ty_exn ctx pr.pr_ty in
           TypeInference.Ctx.declare ctx id ty_id;
           let rules =
             List.map
               (fun (args,rhs) ->
                  let lhs =
                    TypeInference.infer_exn ctx
                      (PT.app_const pr.pr_name @@ args)
                  and rhs =
                    TypeInference.infer_exn ctx rhs
                  in
                  let ty_lhs = TT.ty_exn lhs in
                  TypeInference.unify ty_lhs (TT.ty_exn rhs);
                  lhs, rhs)
               pr.pr_rules
           in
           (* done with typing *)
           TypeInference.Ctx.exit_scope ctx;
           Util.debugf ~section 5
             "(@[<hv>combinator_rule@ @[:id `%a`@ :ty `%a`@]@ \
              :rules (@[<hv>%a@])@ :penalty %d@])"
             (fun k->
                let pp_rule out (lhs,rhs) =
                  Format.fprintf out "`@[%a -->@ %a@]`" TT.pp lhs TT.pp rhs in
                k ID.pp id TT.pp ty_id (Util.pp_list pp_rule) rules pr.pr_penalty);
           (* conversion to {!Term.t} *)
           let ty_id = Type.Conv.of_simple_term_exn conv @@ ty_id in
           let rules =
             rules |> List.map
               (fun (lhs,rhs) ->
                  let lhs = T.Conv.of_simple_term_exn conv lhs in
                  let rhs = T.Conv.of_simple_term_exn conv rhs in
                  let _, args = T.as_app lhs in
                  RW.Term.Rule.make id ty_id args rhs, pr.pr_penalty)
           in
           (rules, (id, ty_id)))
        |> List.split
        |> CCPair.map1 List.flatten
    ) in
    let rules = lazy (
      fst (Lazy.force rules_and_decls)
      |> CCFun.tap
        (fun rules ->
           Util.debugf ~section 4
             "(@[combinator-rules@ (@[<hv>%a@])@])"
             (fun k->k (Util.pp_list pp_rule) rules))
    ) in
    let decls = lazy (snd (Lazy.force rules_and_decls)) in
    let conv = lazy (conv_f (Lazy.force decls)) in
    mk_ rules_name rules decls conv

  module Rules = struct
    let a = "a"
    let b = "b"
    let c = "c"
    let d = "d"
    let x = "x"
    let y = "y"
    let z = "z"
    let k = "k"
    let pi l ty = PT.forall_ty  (List.map (fun v->PT.V v, Some PT.tType) l) ty
    let prop = PT.prop
    let v x = PT.var x
    let app f l = PT.app f l
    let ($) f t = app f [t]
    let (@->) a b = PT.fun_ty [a] b

    let mk_name s = s ^ "»"
    let _Not = mk_name "N"
    let _Eq = mk_name "Eq"
    let _And = mk_name "And"
    let _Ite = mk_name "Ite"
    let _K = mk_name "K"
    let _I = mk_name "I"
    let _S = mk_name "S"
    let _B = mk_name "B"
    let _C = mk_name "C"

    let pre_ name p ty args rhs : pre_rule =
      { pr_name=name; pr_penalty=p; pr_ty=ty;
        pr_rules=[(List.map PT.var args, rhs)];
      }

    (* predicate combinators.
       [N f x = ¬ f x]
       [Eq x y = x=y]
       [And x y = x∧y]
    *)
    let preds () : pre_rule list =
      let ty_not = prop @-> prop in
      let ty_eq = pi [a] (v a @-> v a @-> prop) in
      let ty_and = prop @-> prop @-> prop in
      [ pre_ _Not 0 ty_not [x] @@ PT.not_ (v x);
        pre_ _Eq 1 ty_eq [x;y] @@ PT.eq (v x)(v y);
        pre_ _And 1 ty_and [x;y] @@ PT.and_ [v x; v y];
      ]

    (* "if-then-else"
       [Ite x y z = if x y z]
    *)
    let if_  (): pre_rule list =
      let ty_if = pi [a] (prop @-> v a @-> v a @-> v a) in
      let pr_rules = [
        ([PT.true_; v x; v y], v x);
        ([PT.false_; v x; v y], v y);
      ] in
      [ {pr_name=_Ite; pr_penalty=2; pr_ty=ty_if; pr_rules}
      ]

    (* rewrite rules for SKI *)
    let ski () : pre_rule list =
      (* now define *)
      let ty_i = pi [a] @@ v a @-> v a in
      let ty_k = pi [a;b] @@ v a @-> v b @-> v a in
      let ty_s =
        pi [a;b;c] @@
        (v a @-> v b @-> v c) @->
          (v a @-> v b) @->
          v a @-> v c
      in
      [ pre_ _I 0 ty_i [x] @@ v x;
        pre_ _K 0 ty_k [x;y] @@ v x;
        pre_ _S 1 ty_s [x;y;z] @@ (v x $ v z $ (v y $ v z));
      ]

    (* Schönfinkel's combinators:
       [B x y z = x (y z)]
       [C x y z = x z y]
    *)
    let skibc () : pre_rule list =
      let ty_b = pi [a;b;c] @@
        (v b @-> v c) @-> (v a @-> v b) @-> v a @-> v c
      and ty_c = pi [a;b;c] @@
        (v a @-> v b @-> v c) @->
          v b @-> v a @-> v c
      in
      ski () @ [
        pre_ _B 1 ty_b [x;y;z] @@ (v x $ (v y $ v z));
        pre_ _C 1 ty_c [x;y;z] @@ (v x $ v z $ v y);
      ]

    (* Turner's combinators
       [S' k x y z = k (x z) (y z)]
       [B' k x y z = k x (y z)]
       [C' k x y z = k (x z) y]
    *)
    let turner () : pre_rule list =
      (* ('a -> 'b -> 'c) -> ('d -> 'a) -> ('d -> 'b) -> 'd -> 'c *)
      let ty_s' = pi [a;b;c;d] @@
        (v a @-> v b @-> v c) @->
          (v d @-> v a) @->
          (v d @-> v b) @->
          v d @->
          v c
      (* ('a -> 'b -> 'c) -> 'a -> ('d -> 'b) -> 'd -> 'c *)
      and ty_b' = pi [a;b;c;d] @@
        (v a @-> v b @-> v c) @->
          v a @->
          (v d @-> v b) @->
          v d @->
          v c
      (* ('a -> 'b -> 'c) -> ('d -> 'a) -> 'b -> 'd -> 'c *)
      and ty_c' = pi [a;b;c;d] @@
        (v a @-> v b @-> v c) @->
          (v d @-> v a) @->
          v b @->
          v d @->
          v c
      in
      skibc () @ [
        pre_ "S'»" 1 ty_s' [k;x;y;z] @@ (v k $ (v x $ v z) $ (v y $ v z));
        pre_ "B'»" 1 ty_b' [k;x;y;z] @@ (v k $ v x $ (v y $ v z));
        pre_ "C'»" 1 ty_c' [k;x;y;z] @@ (v k $ (v x $ v z) $ v y);
      ]

    let (++) f g () = f() @ g ()
  end

  module Conv = struct
    (* find combinator in the list of declarations *)
    let find_comb name decls: term =
      decls
      |> CCList.find_map
        (fun (id,ty) ->
           if ID.name id = name then Some (T.const ~ty id) else None)
      |> CCOpt.get_lazy
        (fun () -> Util.failwithf "cannot find combinator `%s`" name)

    let ty_return1_exn ty = match Type.view ty with
      | Type.Fun ([_], ret) -> ret
      | Type.Fun (_::args, ret) -> Type.arrow args ret
      | _ -> Util.invalid_argf "expected `@[%a@]`@ to be a function type" Type.pp ty

    let ty_return2_exn ty = match Type.view ty with
      | Type.Fun (_::_::args, ret) -> Type.arrow args ret
      | _ -> Util.invalid_argf "expected `@[%a@]`@ to be a function type" Type.pp ty

    (* conversion for SKI *)
    let abf decls : conv_fun =
      let _I = find_comb Rules._I decls in
      let _K = find_comb Rules._K decls in
      let _S = find_comb Rules._S decls in
      let _not = find_comb Rules._Not decls in
      let _eq = find_comb Rules._Eq decls in
      let _and = find_comb Rules._And decls in
      let mk_not t = match T.view t with
        | T.AppBuiltin (Builtin.Not, [u]) -> u
        | T.App (f, [u]) when T.equal f _not -> u
        | _ -> T.app _not [t]
      in
      let mk_and a b = T.app _and [a;b] in
      let mk_eq a b = T.app_full _eq [T.ty a] [a;b] in
      let mk_or a b = mk_not (mk_and (mk_not a) (mk_not b)) in
      let mk_i x = T.app_full _I [HVar.ty x] [] in
      let mk_k x t =
        T.app_full _K [T.ty t; HVar.ty x] [t] in
      let mk_s x t u =
        let ty_b = T.ty u |> ty_return1_exn in
        let ty_c = T.ty t |> ty_return2_exn in
        T.app_full _S [HVar.ty x; ty_b; ty_c] [t; u]
      in
      let rec aux x t = match T.view t with
        | T.Var y when HVar.equal Type.equal x y -> mk_i x
        | _ when not (T.var_occurs ~var:x t) -> mk_k x t
        | T.DB _ | T.Var _ | T.Const _ -> assert false (* see above *)
        | T.App (_, []) -> assert false
        | T.App (f, l) ->
          (* remove type arguments *)
          let ty_args, l =
            Util.take_drop_while (fun t -> Type.is_tType @@ T.ty t) l
          in
          let f = T.app f ty_args in
          if l=[] then f
          else (
            List.fold_left
              (fun f arg -> mk_s x f (aux x arg))
              (aux x f)
              l
          )
        | T.Fun _ -> assert false (* FIXME: remove this! *)
        | T.AppBuiltin (Builtin.And, [a;b]) -> aux x (mk_and a b)
        | T.AppBuiltin (Builtin.Or, [a;b]) -> aux x (mk_or a b)
        | T.AppBuiltin (Builtin.Not, [a]) -> aux x (mk_not a)
        | T.AppBuiltin (Builtin.Eq, ([_;a;b]|[a;b])) -> aux x (mk_eq a b)
        | T.AppBuiltin (Builtin.Neq, ([_;a;b]|[a;b])) -> aux x (mk_not @@ mk_eq a b)
        | T.AppBuiltin _ ->
          Util.failwithf "TODO: convert `@[%a@]` into combinators" T.pp t
      in
      List.fold_right aux
  end

  (* TODO: better conversion algorithms (Schönfickel, Turner) *)

  let ski : t = of_rules "ski" Rules.(ski ++ preds) Conv.abf
  let ski_if : t = of_rules "ski_if" Rules.(ski ++ preds ++ if_) Conv.abf
  let skibc : t = of_rules "skibc" Rules.(skibc ++ preds) Conv.abf
  let skibc_if : t = of_rules "skibc_if" Rules.(skibc ++ preds ++ if_) Conv.abf
  let turner : t = of_rules "turner" Rules.(turner ++ preds) Conv.abf

  let default = skibc

  let by_name s = match CCList.find_pred (fun c -> name c=s) !all_ with
    | Some c -> c
    | None -> Util.failwithf "no such set of combinators: `%s`" s

  let list_names () = List.map name !all_
end

(* number of ty and non-ty arguments *)
let term_arity args =
  args
  |> Util.take_drop_while (fun t -> T.is_type t)
  |> CCPair.map List.length List.length

let unif_step ((c:Combinators.t),sc_combs) ((t,u),sc_pair): _ list =
  assert (Type.equal (T.ty t) (T.ty u));
  let try_heads (t:term): _ Sequence.t =
    let f, args = T.as_app t in
    let n_ty_args, n_l = term_arity args in
    assert (T.equal t @@ T.app f args);
    if T.is_var f && n_l > 0 then (
      (* try to unify LHS of rules with prefixes of [f args] *)
      Combinators.rules c
      |> Sequence.of_list
      |> Sequence.flat_map
        (fun (r,p) ->
           let lhs = RW.Term.Rule.lhs r in
           (* number of term arguments *)
           let n_args = RW.Term.Rule.args r |> term_arity |> snd in
           assert (n_args >= 1);
           (* unify [lhs] with [f args_1…args_{m}] where [1 ≤ m ≤ len(args)]
              (i.e. non empty prefixes of args) *)
           begin
             Sequence.int_range ~start:1 ~stop:(min n_args n_l)
             |> Sequence.filter_map
               (fun i ->
                  let t_prefix = T.app f (CCList.take (n_ty_args+i) args) in
                  try
                    let subst =
                      Unif.FO.unify_syn (t_prefix,sc_pair) (lhs,sc_combs)
                    in
                    Some (subst,p)
                  with Unif.Fail -> None)
           end)
    ) else Sequence.empty
  in
  Util.incr_stat stat_unif_calls;
  begin
    Sequence.append (try_heads t) (try_heads u)
    |> Sequence.to_rev_list
    |> CCList.sort_uniq
      ~cmp:(CCOrd.pair Subst.compare CCOrd.int)
    |> CCFun.tap
      (fun substs ->
         if not @@ CCList.is_empty substs then (
           let pp_subst out s = Subst.pp out (fst s) in
           Util.add_stat stat_unif_steps (List.length substs);
           Util.debugf ~section 4
             "(@[ho_unif.step@ `@[<hv>%a =@ %a@]`@ :substs (@[<hv>%a@])@])"
             (fun k->k T.pp t T.pp u (Util.pp_list pp_subst) substs);
         );
      )
  end
