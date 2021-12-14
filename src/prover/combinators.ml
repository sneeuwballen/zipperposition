open Logtk
open Extensions
open Combinators_base

module T = Term
module Ty = Type
module Lits = Literals
module Lit = Literal


let section = Util.Section.make ~parent:Const.section "combs"

type conv_rule = T.t -> T.t option

let k_enable_combinators = Flex_state.create_key ()
let k_app_var_narrowing = Flex_state.create_key ()
let k_s_penalty = Flex_state.create_key ()
let k_b_penalty = Flex_state.create_key ()
let k_c_penalty = Flex_state.create_key ()
let k_k_penalty = Flex_state.create_key ()
let k_deep_app_var_penalty = Flex_state.create_key ()
let k_unif_resolve = Flex_state.create_key ()
let k_combinators_max_depth = Flex_state.create_key ()



module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {5 Registration} *)
  val setup : unit -> unit
  val maybe_conv_lams : Env.C.t -> Env.C.t
  val force_conv_lams : Env.C.t -> Env.C.t
  val expand : T.t -> T.t
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx

    let has_lams_aux = 
      Iter.exists (fun t ->
        T.Seq.subterms ~include_builtin:true ~ignore_head:false t 
        |> Iter.exists T.is_fun)

    let has_lams_c c = 
      has_lams_aux @@ C.Seq.terms c
    
    let has_lams_lits lits =
      CCList.to_iter lits 
      |> Iter.flat_map (SLiteral.to_iter)
      |> has_lams_aux

    let enocde_stmt st =
      let rule = Proof.Rule.mk "lams2combs" in
      let rules = curry_optimizations @ bunder_optimizations in
      E.cr_return @@ List.map (fun c -> 
        if has_lams_c c then (
          let proof = Proof.Step.simp [C.proof_parent c] ~rule in
          let lits' = Literals.map (abf ~rules) (C.lits c) in
          C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
          (Array.to_list lits') proof
        ) else c
      )(E.C.of_statement st)
      
    let comb_narrow c =
      let new_lits = Literals.map narrow (C.lits c) in
      if Literals.equal (C.lits c) new_lits then (
        SimplM.return_same c
      ) else (
        let proof = Proof.Step.simp [C.proof_parent c] 
                      ~rule:(Proof.Rule.mk "narrow combinators") in
        let new_ = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
                    (Array.to_list new_lits) proof in
        SimplM.return_new new_
      )

    let tyvarA = HVar.fresh ~ty:Ty.tType ()
    let tyvarB = HVar.fresh ~ty:Ty.tType ()
    let tyvarC = HVar.fresh ~ty:Ty.tType ()

    let type_of_vars ~args ~ret =
      let open Ty in
      if CCList.is_empty args then Ty.var ret
      else List.map Ty.var args ==> Ty.var ret

    (* Create the arguments of type appropriate to be applied to the combinator *)
    let s_arg1 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarA;tyvarB] ~ret:tyvarC) ()
    let s_arg2 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarA] ~ret:tyvarB) ()

    let b_arg1 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarA] ~ret:tyvarB) ()
    let b_arg2 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarC] ~ret:tyvarA) ()

    let c_arg1 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarA;tyvarB] ~ret:tyvarC) ()
    let c_arg2 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[] ~ret:tyvarB) ()

    let k_arg1 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[] ~ret:tyvarB) ()

    (* Partially applies a combinator with arguments
        arguments:
          comb: original combinator with penalty for instantianting clause with it
          args: arguments with corresponding pentalties *)
    let partially_apply ~comb args =
      let orig_comb, penalty = comb in
      let rec aux acc = function 
        | [] -> []
        | (a,p) :: aas ->
          let acc = T.app acc [a] in
          (acc,p) :: aux acc aas in
      (orig_comb,penalty) :: aux orig_comb args

    let alpha = T.var tyvarA 
    let beta = T.var tyvarB
    let gamma = T.var tyvarC

    let partially_applied_s () =
      partially_apply ~comb:(mk_s ~alpha ~beta ~gamma ~args:[], 0)
        [s_arg1, 1; s_arg2, Env.flex_get k_s_penalty]

    let partially_applied_b () =
      partially_apply ~comb:(mk_b ~alpha ~beta ~gamma ~args:[], 0)
        [b_arg1, 1; b_arg2, Env.flex_get k_b_penalty]

    let partially_applied_c () =
      partially_apply ~comb:(mk_c ~alpha ~beta ~gamma ~args:[], 0)
        [c_arg1, 1; c_arg2, Env.flex_get k_c_penalty]
    
    let partially_applied_k () =
      partially_apply ~comb:(mk_k ~alpha ~beta ~args:[], 0)
        [k_arg1, Env.flex_get k_k_penalty]
    
    let partially_applied_i () =
      [mk_i ~alpha ~args:[], 0]

    let partially_applied_combs () =
      partially_applied_s () @ partially_applied_b () @ partially_applied_c () @
      partially_applied_k () @ partially_applied_i ()


    let instantiate_var_w_comb ~var =
      CCList.filter_map (fun (comb, penalty) ->
        try
          Some (Unif.FO.unify_syn (comb, 0) (var, 1), penalty)
        with Unif.Fail -> None
      ) (partially_applied_combs ())

    let narrow_app_var_rule_name = 
      "narrow_applied_variable"


    let narrow_app_vars_applicable cl =
      match Env.flex_get k_combinators_max_depth with
      | None -> true
      | Some max_depth ->
        Proof.Step.count_rules ~name:narrow_app_var_rule_name (C.proof_step cl)
          <= max_depth


    let narrow_app_vars clause =
      (* CCFormat.printf "narrowing:@[%a@]@." C.pp clause; *)
      let rule = Proof.Rule.mk narrow_app_var_rule_name in
      let tags = [Proof.Tag.T_ho] in

      let ord = Env.ord () in 
      let eligible = C.Eligible.(res clause) in
      let lits = C.lits clause in
      (* do the inferences in which clause is passive (rewritten),
        so we consider both negative and positive literals *)
      (if narrow_app_vars_applicable clause then (
        Lits.fold_terms ~vars:(false) ~var_args:(true) ~fun_bodies:(false) 
                        ~subterms:true ~ord ~which:`Max ~eligible ~ty_args:false
        lits)
      else Iter.empty)
      (* Variable has at least one arugment *)
      |> Iter.filter (fun (u_p, _) -> T.is_app_var u_p)
      |> Iter.flat_map_l (fun (u, u_pos) -> 
        (* variable names as in Ahmed's paper (unpublished) *)
        let var = T.head_term u in
        assert(T.is_var var);
        CCList.filter_map (fun (subst, comb_penalty) ->
          let renaming = Subst.Renaming.create () in
          let lit_idx, lit_pos = Lits.Pos.cut u_pos in
          let comb_penalty = max comb_penalty 1 in

          Util.debugf ~section 3  "narrow vars:@[%a@]:@[%d@]" (fun k -> k C.pp clause lit_idx);

          let lit = Lit.apply_subst_no_simp renaming subst (lits.(lit_idx), 1) in
          if not (Lit.Pos.is_max_term ~ord lit lit_pos) ||
             not (CCBV.get (C.eligible_res (clause, 1) subst) lit_idx) then (
            Util.debugf ~section 3 "ordering restriction fail: @[%a@]@." (fun k -> k Subst.pp subst);
            None)
          else (
            let t_depth = Position.size (Literal.Pos.term_pos (lits.(lit_idx)) lit_pos) in
            let depth_mul = 
              if not @@ Env.flex_get k_deep_app_var_penalty then 1
              else max t_depth 1 in
            let lits' = CCArray.to_list @@ Lits.apply_subst renaming subst (lits, 1) in
            let proof = 
              Proof.Step.inference ~rule ~tags
                [C.proof_parent_subst renaming (clause,1) subst] in
            let penalty = depth_mul * comb_penalty * C.penalty clause in
            (* CCFormat.printf "penalty:%d@." penalty; *)
            let new_clause = C.create ~trail:(C.trail clause) ~penalty lits' proof in

            Util.debugf ~section 3 "success: @[%a@]@." (fun k -> k C.pp new_clause);

            Some new_clause
          )) (instantiate_var_w_comb ~var))
        |> Iter.to_list

    let lams2combs_otf c =
      if not @@ has_lams_c c then SimplM.return_same c
      else (
        let rules = curry_optimizations @ bunder_optimizations in
        let proof = Proof.Step.simp [C.proof_parent c] 
                      ~rule:(Proof.Rule.mk "lams2combs on-the-fly") in
        let lits' = Literals.map (abf ~rules) (C.lits c) in
        let new_ = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
                    (Array.to_list lits') proof in
        SimplM.return_new new_)

    let maybe_conv_lams c =
      if E.flex_get k_enable_combinators then (
        SimplM.get (lams2combs_otf c)
      ) else c

    let ho_unif_solve c =
      let module PUnif = 
        PUnif.Make(struct 
          let st = 
            Env.flex_state ()
            |> Flex_state.add PragUnifParams.k_fixpoint_decider true
            |> Flex_state.add PragUnifParams.k_pattern_decider true
            |> Flex_state.add PragUnifParams.k_solid_decider true
            |> Flex_state.add PragUnifParams.k_max_inferences 1
            |> Flex_state.add PragUnifParams.k_max_depth 2
            |> Flex_state.add PragUnifParams.k_max_app_projections 2
            |> Flex_state.add PragUnifParams.k_max_rigid_imitations 2
            |> Flex_state.add PragUnifParams.k_max_elims 2
            |> Flex_state.add PragUnifParams.k_max_identifications 2
          end) in
      
      let get_unif l r =
        try 
          OSeq.nth 0 (PUnif.unify_scoped (l,0) (r,0))
        with Not_found -> None in
      

      match C.lits c with 
      | [| Lit.Equation (lhs,rhs,false) |] ->
        let lhs,rhs = CCPair.map_same comb2lam (lhs,rhs) in
        begin match get_unif lhs rhs with
        | Some subst ->
          let rule = Proof.Rule.mk "ho_unif_resolve" in
          let subst = Unif_subst.subst subst in
          let proof = 
            Proof.Step.simp ~rule 
              [C.proof_parent_subst Subst.Renaming.none (c,0) subst] in
          SimplM.return_new (C.create ~penalty:1 ~trail:(C.trail c) [] proof)
        | _ -> SimplM.return_same c
        end
      | _ -> SimplM.return_same c
    
    let force_conv_lams c =
      assert(C.Seq.terms c |> Iter.for_all T.DB.is_closed);
      let c' = lams2combs_otf c in
      if SimplM.is_same c' then c
      else SimplM.get c'

    (* Expands the chosen term to be of the form 
       \lambda (all type vars). body of prop type *)
    let expand t = 
      if Env.flex_get k_enable_combinators then (
        assert(not (T.is_fun t)); (* no lambdas if combinators are on *)
        let ty_args, ret_ty = Type.open_fun (T.ty t) in
        let n = List.length ty_args in
        let bvars = List.mapi (fun i ty -> T.bvar ~ty (n-i-1)) ty_args in
        let t' = T.app t bvars in
        let body = CCOpt.get_or ~default:t' (comb_normalize t') in
        T.fun_l ty_args body
      ) else Lambda.eta_expand t
    
    let setup () =
      if E.flex_get k_enable_combinators then (
        E.add_clause_conversion enocde_stmt;
        if E.flex_get k_app_var_narrowing then (
          E.add_unary_inf "narrow applied variable" narrow_app_vars
        );
        E.add_basic_simplify lams2combs_otf;
        E.Ctx.set_ord (Ordering.compose cmp_by_max_weak_r_len (E.Ctx.ord ()));
        
        if Env.flex_get k_unif_resolve then (
          E.add_unary_simplify ho_unif_solve
        );

        Unif._allow_pattern_unif := false;
      )

end

let _enable_combinators = ref false
let _app_var_narrowing = ref true
let _s_penalty = ref 1
let _b_penalty = ref 1
let _c_penalty = ref 1
let _k_penalty = ref 1
let _app_var_constraints = ref false
let _deep_app_var_penalty = ref false
let _unif_resolve = ref false
let _combinators_max_depth = ref None


let extension =
  let lam2combs seq = seq in

  let register env =
    let module E = (val env : Env.S) in
    E.flex_add k_enable_combinators !_enable_combinators;
    E.flex_add k_app_var_narrowing !_app_var_narrowing;
    E.flex_add k_s_penalty !_s_penalty;
    E.flex_add k_c_penalty !_c_penalty;
    E.flex_add k_b_penalty !_b_penalty;
    E.flex_add k_k_penalty !_k_penalty;
    E.flex_add k_deep_app_var_penalty !_deep_app_var_penalty;
    E.flex_add k_unif_resolve !_unif_resolve;
    E.flex_add k_combinators_max_depth !_combinators_max_depth;

    let module ET = Make(E) in
    ET.setup ()
  in
  { Extensions.default with
      Extensions.name = "combinators";
      env_actions=[register];
      prio=3;
      post_cnf_modifiers=[lam2combs];
  }

let () =
  Options.add_opts
    [ "--combinator-based-reasoning", Arg.Bool (fun v -> _enable_combinators := v), " enable / disable combinator based reasoning";
     "--app-var-constraints", Arg.Bool (fun v -> _app_var_constraints := v), " enable / disable delaying app var clashes as constraints";
     "--app-var-narrowing", Arg.Bool ((:=) _app_var_narrowing), " enable / disable app_var_narrowing";
     "--penalize-deep-appvars", Arg.Bool (fun v -> _deep_app_var_penalty := v), " enable / disable penalizing narrow app var inferences with deep variables";
     "--comb-max-depth", Arg.Int (fun v -> _combinators_max_depth := Some v), " set the maximal number off variable narrowings allowed. ";
     "--comb-unif-resolve", Arg.Bool ((:=) _unif_resolve), " enable / disable higher-order unit clause resolutions";
     "--comb-s-penalty", Arg.Set_int _s_penalty, "penalty for narrowing with $S X Y";
     "--comb-c-penalty", Arg.Set_int _c_penalty, "penalty for narrowing with $C X Y";
     "--comb-b-penalty", Arg.Set_int _b_penalty, "penalty for narrowing with $B X Y";
     "--comb-k-penalty", Arg.Set_int _k_penalty, "penalty for narrowing with $K X"];
  Params.add_to_mode "ho-comb-complete" (fun () ->
    _enable_combinators := true;
  );
  Extensions.register extension;
