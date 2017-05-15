
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

  type t = {
    name: string;
    rules: rules lazy_t;
    decls: (ID.t * Type.t) list lazy_t;
  }

  let rules t = Lazy.force t.rules
  let name t = t.name
  let decls t = Lazy.force t.decls

  let all_ : t list ref = ref []

  let mk_ name rules decls : t =
    let c = { name; rules; decls; } in
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

  (* to be compiled into a set of rules *)
  type pre_rule = string * int * PT.t * string list * PT.t

  (* build from a list of pairs of terms (interpreted as [lhs->rhs]) *)
  let of_rules name (l:pre_rule list) : t =
    let rules_and_decls = lazy (
      let ctx =
        TypeInference.Ctx.create ~on_var:`Infer ()
      in
      let conv = T.Conv.create () in
      (* do type inference and conversions for each rule *)
      List.map
        (fun (name,penalty,ty,args,rhs) ->
           let id = ID.make name in
           (* perform conversion… *)
           let ty_id = TypeInference.infer_ty_exn ctx ty in
           TypeInference.Ctx.declare ctx id ty_id;
           let lhs =
             TypeInference.infer_exn ctx
               (PT.app_const name @@ List.map PT.var args)
           in
           let rhs = TypeInference.infer_exn ctx rhs in
           let ty_lhs = TT.ty_exn lhs in
           TypeInference.unify ty_lhs (TT.ty_exn rhs);
           (* done with typing *)
           TypeInference.Ctx.exit_scope ctx;
           (* conversion to {!Term.t} *)
           Util.debugf ~section 5
             "(@[<hv>combinator_rule@ @[:id `%a`@ :ty `%a`@]@ \
              :rule `@[%a -->@ %a@]`@ :penalty %d@])"
             (fun k->k ID.pp id TT.pp ty_id TT.pp lhs TT.pp rhs penalty);
           let lhs = T.Conv.of_simple_term_exn conv lhs in
           let rhs = T.Conv.of_simple_term_exn conv rhs in
           let ty_id = Type.Conv.of_simple_term_exn conv @@ ty_id in
           let _, args = T.as_app lhs in
           (RW.Term.Rule.make id ty_id args rhs, penalty), (id, ty_id))
        l
      |> List.split
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
    mk_ name rules decls

  module Rules_ = struct
    let a = "a"
    let b = "b"
    let c = "c"
    let x = "x"
    let y = "y"
    let z = "z"
    let pi l ty = PT.forall_ty  (List.map (fun v->PT.V v, Some PT.tType) l) ty
    let var x = PT.var x
    let (@->) a b = PT.fun_ty [a] b

    (* rewrite rules for SKI *)
    let ski : pre_rule list =
      (* now define *)
      let ty_i = pi [a] @@ var a @-> var a in
      let ty_k = pi [a;b] @@ var a @-> var b @-> var a in
      let ty_s =
        pi [a;b;c] @@
        (var a @-> var b @-> var c) @->
          (var a @-> var b) @->
          var a @-> var c
      in
      [ ("I", 0, ty_i, [x], PT.var x);
        ("K", 0, ty_k, [x;y], PT.var x);
        ("S", 1, ty_s, [x;y;z],
         PT.app (PT.var x) [PT.var z; PT.app (PT.var y) [PT.var z]]);
      ]
  end

  let ski : t = of_rules "ski" Rules_.ski

  let default = ski

  let by_name s = match CCList.find_pred (fun c -> name c=s) !all_ with
    | Some c -> c
    | None -> Util.failwithf "no such set of combinators: `%s`" s

  let list_names () = List.map name !all_
end

let unif_step ((c:Combinators.t),sc_combs) ((t,u),sc_pair): _ list =
  assert (Type.equal (T.ty t) (T.ty u));
  let try_heads (t:term): _ Sequence.t =
    let f, args = T.as_app t in
    if T.is_var f then (
      (* try to unify LHS of rules with prefixes of [f args] *)
      Combinators.rules c
      |> Sequence.of_list
      |> Sequence.flat_map
        (fun (r,p) ->
           let lhs = RW.Term.Rule.lhs r in
           (* number of term arguments *)
           let n_args =
             RW.Term.Rule.args r
             |> List.filter (fun t -> not (Type.is_tType @@ T.ty t))
             |> List.length
           in
           assert (n_args >= 1);
           (* unify [lhs] with [f args_1…args_{m}] where [1 ≤ m ≤ len(args)]
              (i.e. non empty prefixes of args) *)
           begin
             Sequence.int_range ~start:1 ~stop:(min n_args (List.length args))
             |> Sequence.filter_map
               (fun i ->
                  let t_prefix = T.app f (CCList.take i args) in
                  try
                    Some (Unif.FO.unification (t_prefix,sc_pair) (lhs,sc_combs),p)
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
             "(@[ho_unif@ `@[<hv>%a =@ %a@]`@ :substs (@[<hv>%a@])@])"
             (fun k->k T.pp t T.pp u (Util.pp_list pp_subst) substs);
         );
      )
  end
