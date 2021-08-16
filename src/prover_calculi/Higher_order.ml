
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module BV = CCBV
module T = Term
module Lit = Literal
module US = Unif_subst
module Lits = Literals

module IntSet = Set.Make(CCInt)
module IntMap = Util.Int_map


let section = Util.Section.make ~parent:Const.section "ho"

let stat_eq_res = Util.mk_stat "ho.eq_res.steps"
let stat_eq_res_syntactic = Util.mk_stat "ho.eq_res_syntactic.steps"
let stat_ext_neg_lit = Util.mk_stat "ho.extensionality-.steps"
let stat_ext_pos = Util.mk_stat "ho.extensionality+.steps"
let stat_beta = Util.mk_stat "ho.beta_reduce.steps"
let stat_eta_normalize = Util.mk_stat "ho.eta_normalize.steps"
let stat_prim_enum = Util.mk_stat "ho.prim_enum.steps"
let stat_elim_pred = Util.mk_stat "ho.elim_pred.steps"
let stat_ho_unif = Util.mk_stat "ho.unif.calls"
let stat_ho_unif_steps = Util.mk_stat "ho.unif.steps"
let stat_neg_ext = Util.mk_stat "ho.neg_ext_success"
let stat_ext_dec = Util.mk_stat "sup.ext_dec calls"
let stat_ext_inst = Util.mk_stat "sup.ext_inst calls"


let prof_eq_res = ZProf.make "ho.eq_res"
let prof_eq_res_syn = ZProf.make "ho.eq_res_syntactic"
let prof_ext_dec = ZProf.make "sup.ext_dec"
let prof_ho_unif = ZProf.make "ho.unif"
let stat_complete_eq = Util.mk_stat "ho.complete_eq.steps"

let k_ext_pos = Flex_state.create_key ()
let k_ext_pos_all_lits = Flex_state.create_key ()
let k_ext_axiom = Flex_state.create_key ()
let k_choice_axiom = Flex_state.create_key ()
let k_elim_pred_var = Flex_state.create_key ()
let k_ext_neg_lit = Flex_state.create_key ()
let k_neg_ext = Flex_state.create_key ()
let k_neg_ext_as_simpl = Flex_state.create_key ()
let k_ext_axiom_penalty = Flex_state.create_key ()
let k_choice_axiom_penalty = Flex_state.create_key ()
let k_instantiate_choice_ax = Flex_state.create_key ()
let k_elim_leibniz_eq = Flex_state.create_key ()
let k_elim_andrews_eq = Flex_state.create_key ()
let k_elim_andrews_eq_simpl = Flex_state.create_key ()
let k_prune_arg_fun = Flex_state.create_key ()
let k_prim_enum_terms = Flex_state.create_key ()
let k_simple_projection = Flex_state.create_key ()
let k_simple_projection_md = Flex_state.create_key ()
let k_check_lambda_free = Flex_state.create_key ()
let k_purify_applied_vars = Flex_state.create_key()
let k_eta = Flex_state.create_key()
let k_diff_const = Flex_state.create_key()
let k_generalize_choice_trigger = Flex_state.create_key ()
let k_prim_enum_add_var = Flex_state.create_key ()
let k_prim_enum_early_bird = Flex_state.create_key ()
let k_resolve_flex_flex = Flex_state.create_key ()
let k_arg_cong = Flex_state.create_key ()
let k_arg_cong_simpl = Flex_state.create_key ()
let k_ext_dec_lits = Flex_state.create_key ()
let k_ext_rules_max_depth = Flex_state.create_key ()
let k_ext_rules_kind = Flex_state.create_key ()
let k_ho_disagremeents = Flex_state.create_key ()
let k_add_ite_axioms = Flex_state.create_key ()


type prune_kind = [`NoPrune | `OldPrune | `PruneAllCovers | `PruneMaxCover]


module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {5 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)

  val prim_enum_tf : Env.C.t -> Env.C.t list
end

let k_some_ho : bool Flex_state.key = Flex_state.create_key()
let k_enabled : bool Flex_state.key = Flex_state.create_key()
let k_enable_def_unfold : bool Flex_state.key = Flex_state.create_key()
let k_enable_ho_unif : bool Flex_state.key = Flex_state.create_key()
let k_ho_prim_mode :
  [ `Combinators | `And | `Or | `Neg | `Quants | `Eq | `TF  
    | `Full | `Pragmatic | `Simple | `None ] Flex_state.key 
   = Flex_state.create_key()
let k_ho_prim_max_penalty : int Flex_state.key = Flex_state.create_key()
let k_ground_app_vars :  [ `Off | `Fresh | `All ] Flex_state.key  = Flex_state.create_key()


module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx
  module Combs = Combinators.Make(E)
  module Stm = E.Stm
  module StmQ = E.StmQ
  module FR = E.FormRename

  (* index for ext-neg, to ensure α-equivalent negative equations have the same skolems *)
  module FV_ext_neg_lit = FV_tree.Make(struct
      type t = Literal.t * T.t list (* lit -> skolems *)
      let compare = CCOrd.(pair Literal.compare (list T.compare))
      let to_lits (l,_) = Iter.return (Literal.Conv.to_form l)
      let labels _ = Util.Int_set.empty
    end)

  let idx_ext_neg_lit_ : FV_ext_neg_lit.t ref = ref (FV_ext_neg_lit.empty())
  let _ext_dec_from_idx = ref (ID.Map.empty)
  let _ext_dec_into_idx = ref (ID.Map.empty)


  (* retrieve skolems for this literal, if any *)
  let find_skolems_ (lit:Literal.t) : T.t list option =
    FV_ext_neg_lit.retrieve_alpha_equiv_c !idx_ext_neg_lit_ (lit, [])
    |> Iter.find_map
      (fun (lit',skolems) ->
         let subst = Literal.variant (lit',0) (lit,1) |> Iter.head in
         begin match subst with
           | Some (subst,_) ->
             let skolems =
               List.map
                 (fun t -> Subst.FO.apply Subst.Renaming.none subst (t,0))
                 skolems
             in
             Some skolems
           | None -> None
         end)

  let remove_ff_constraints c =
    let module VS = Term.VarSet in

    (* assumes literal is negative flex-flex lit *)
    let extract_hd_vars = function
      | Literal.Equation(lhs,rhs,false) ->
        VS.of_list [T.as_var_exn (T.head_term lhs);
                    T.as_var_exn (T.head_term rhs)]
      | _ -> assert false
    in

    let is_neg_ff = function
      | Literal.Equation(lhs,rhs,false) -> 
        T.is_var (T.head_term lhs) &&
        T.is_var (T.head_term rhs)
      | _ -> false 
    in
    
    (* variable is blocked if it is not flex-flex or if it appears as variable head
       in the literal where blocked occurrs as well *)
    let blocked_vars =
      CCArray.filter (fun l -> not @@ is_neg_ff l) (C.lits c)
      |> Literals.vars 
      |> VS.of_list
    in

    (* ad-hoc union find with two equivalence classes -- shares variables
       with blocked_vars or not -- used to compute whether var is blocked or not*)
    let vars_to_remove,_ =
      CCArray.fold (fun ((allowed, bl) as acc) lit -> 
        match lit with 
        | Literal.Equation(lhs, rhs, false) when is_neg_ff lit ->
          let l_var,r_var = CCPair.map_same (fun t -> T.as_var_exn (T.head_term t)) (lhs,rhs) in
          if not (VS.mem l_var bl) && not (VS.mem r_var bl) then (
            (VS.add_list allowed [l_var;r_var]), bl
          ) else (
            if VS.mem l_var allowed || VS.mem r_var allowed then (
              VS.empty, VS.add_list (VS.union allowed bl) [l_var;r_var]
            ) else (allowed, VS.add_list bl [l_var; r_var])
          )
          
        | _ -> acc
      ) (VS.empty, blocked_vars) (C.lits c) in


    if VS.is_empty vars_to_remove then (SimplM.return_same c)
    else (
      let new_lits =
        CCArray.to_list (C.lits c)
        |> CCList.filter_map (fun lit -> 
          if is_neg_ff lit then (
            let c_vars = extract_hd_vars lit in
            if VS.subset c_vars vars_to_remove then None
            else Some lit
          ) else Some lit) in
      let proof =
        Proof.Step.simp 
          ~tags:[Proof.Tag.T_ho] 
          ~rule:(Proof.Rule.mk "remove_ff_constraints")
          [C.proof_parent c] in
      let cl = C.create ~penalty:(C.penalty c) ~trail:(C.trail c) new_lits proof in
      SimplM.return_new cl
    )

  let rec declare_skolems = function
    | [] -> ()
    | (sym,id) :: rest -> Ctx.declare sym id; declare_skolems rest

  (* negative extensionality rule:
     [f != g] where [f : a -> b] becomes [f k != g k] for a fresh parameter [k] *)
  let ext_neg_lit (lit:Literal.t) : _ option = match lit with
    | Literal.Equation (f, g, false)
      when Type.is_fun (T.ty f) &&
           not (T.is_var f) &&
           not (T.is_var g) &&
           not (T.equal f g) ->
      let n_ty_params, ty_args, _ = Type.open_poly_fun (T.ty f) in
      assert (n_ty_params=0);
      let params = match find_skolems_ lit with
        | Some l -> l
        | None ->
          (* create new skolems, parametrized by free variables *)
          let vars = Literal.vars lit in
          let skolems = ref [] in
          let l = List.map (fun ty -> 
              let sk, res =  T.mk_fresh_skolem vars ty in
              skolems := sk :: !skolems;
              res) ty_args in
          (* save list *)
          declare_skolems !skolems;
          idx_ext_neg_lit_ := FV_ext_neg_lit.add !idx_ext_neg_lit_ (lit,l);
          l
      in
      let new_lit = Literal.mk_neq (T.app f params) (T.app g params) in
      Util.incr_stat stat_ext_neg_lit;
      Util.debugf ~section 4
        "(@[ho_ext_neg_lit@ :old `%a`@ :new `%a`@])"
        (fun k->k Literal.pp lit Literal.pp new_lit);
      Some (new_lit,[],[Proof.Tag.T_ho; Proof.Tag.T_ext])
    | _ -> None

  let ext_pos_general ?(all_lits = false) (c:C.t) : C.t list =
    let eligible = if all_lits then C.Eligible.always else C.Eligible.param c in
    let expand_quant = not @@ Env.flex_get Combinators.k_enable_combinators in

    (* Remove recursively variables at the end of the literal t = s if possible.
       e.g. ext_pos_lit (f X Y) (g X Y) other_lits = [f X = g X, f = g]
       if X and Y do not appear in other_lits *)
    let rec ext_pos_lit t s other_lits =
      let f, tt = T.as_app t in
      let g, ss = T.as_app s in
      begin match List.rev tt, List.rev ss with
        | last_t :: tl_rev_t, last_s :: tl_rev_s ->
          if T.equal last_t last_s && not (T.is_type last_t) then
            match T.as_var last_t with
            | Some v ->
              if not (T.var_occurs ~var:v f)
              && not (T.var_occurs ~var:v g)
              && not (List.exists (T.var_occurs ~var:v) tl_rev_t)
              && not (List.exists (T.var_occurs ~var:v) tl_rev_s)
              && not (List.exists (Literal.var_occurs v) other_lits)
              then (
                let butlast = (fun l -> CCList.take (List.length l - 1) l) in
                let t' = T.app f (butlast tt) in
                let s' = T.app g (butlast ss) in
                Literal.mk_eq t' s'
                :: ext_pos_lit t' s' other_lits
              )
              else
                []
            | None -> []
          else []
        | _ -> []
      end
    in
    let new_clauses =
      (* iterate over all literals eligible for paramodulation *)
      C.lits c
      |> Iter.of_array |> Util.seq_zipi
      |> Iter.filter (fun (idx,lit) -> eligible idx lit)
      |> Iter.flat_map_l
        (fun (lit_idx,lit) ->
           let lit = Literal.map (fun t -> Lambda.eta_reduce ~expand_quant t) lit in
           match lit with
           | Literal.Equation (t, s, true) ->
             ext_pos_lit t s (CCArray.except_idx (C.lits c) lit_idx)
             |> Iter.of_list
             |> Iter.flat_map_l
               (fun new_lit ->
                  (* create a clause with new_lit instead of lit *)
                  let new_lits = new_lit :: CCArray.except_idx (C.lits c) lit_idx in
                  let proof =
                    Proof.Step.inference [C.proof_parent c]
                      ~rule:(Proof.Rule.mk "ho_ext_pos_general")
                      ~tags:[Proof.Tag.T_ho; Proof.Tag.T_ext]
                  in
                  let new_c =
                    C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c)
                  in
                  [new_c])
             |> Iter.to_list
           | _ -> [])
      |> Iter.to_rev_list
    in
    if new_clauses<>[] then (
      Util.debugf ~section 4
        "(@[ext-pos-general-eq@ :clause %a@ :yields (@[<hv>%a@])@])"
        (fun k->k C.pp c (Util.pp_list ~sep:" " C.pp) new_clauses);
    );
    new_clauses

  let neg_ext (c:C.t) : C.t list =
    let get_new_lits lhs rhs =
      let calc_skolem lhs rhs =
        let (pref_l, body_l), (pref_r, body_r) =
          CCPair.map_same T.open_fun (lhs, rhs) 
        in
        assert (CCList.equal Type.equal pref_l pref_r);
        assert (not (CCList.is_empty pref_l));
        let hd_var, rest_vars = CCList.hd_tl pref_l in

        let body = 
          T.fun_ hd_var @@ CCList.fold_right (fun ty body -> 
            T.Form.exists (T.fun_ ty body)
          ) rest_vars (T.Form.neq body_l body_r)
        in

        assert(T.DB.is_closed body);
        FR.get_skolem ~parent:c ~mode:`SkolemRecycle body
      in    


      assert (Type.is_fun (T.ty lhs));
      let lhs, rhs = CCPair.map_same Combs.expand (lhs,rhs) in
      
      let rec aux acc lhs rhs =
        assert (Type.equal (T.ty lhs) (T.ty rhs));
        if not (Type.is_fun (T.ty lhs)) then acc
        else (
          let sk = calc_skolem lhs rhs in
          let penalty =
            if List.length (fst @@ Type.open_fun (T.ty lhs)) = 1 then 0 else 1 
          in
          let lhs', rhs' = 
            CCPair.map_same (fun hd -> Lambda.whnf @@ T.app hd [sk]) (lhs,rhs)
          in
          let new_lit = Lit.mk_neq lhs' rhs' in
          aux ((new_lit,penalty)::acc) lhs' rhs'
        )
      in
      aux [] lhs rhs in

    let mk_clause ~lits ~penalty =
      let proof =
        Proof.Step.inference [C.proof_parent c] 
          ~rule:(Proof.Rule.mk "neg_ext")
          ~tags:[Proof.Tag.T_ho; Proof.Tag.T_ext; Proof.Tag.T_dont_increase_depth]
      in
      let new_c =
        C.create ~penalty:(C.penalty c + penalty) ~trail:(C.trail c)
        (CCArray.to_list lits) proof in
      Util.debugf 1 ~section "NegExt: @[%a@] => @[%a@].\n" 
        (fun k -> k C.pp c C.pp new_c);
      Util.incr_stat stat_neg_ext;
      new_c
    in

    let eligible = C.Eligible.res c in
    let new_lits_map =
      IntMap.to_list @@
        CCArray.foldi (fun new_lits idx lit -> 
          match lit with 
          | Literal.Equation (lhs,rhs,false) 
            when eligible idx lit && Type.is_fun @@ T.ty lhs ->
            IntMap.add idx (get_new_lits lhs rhs) new_lits
          | _ -> new_lits
        ) IntMap.empty (C.lits c)
    in

    let compute_results lits_map =
      let lit_arr = CCArray.copy (C.lits c) in
      let rec aux penalty = function
        | [] -> [mk_clause ~lits:lit_arr ~penalty]
        | (idx, repls) :: rest ->
          CCList.flat_map (fun (repl, p) ->
            lit_arr.(idx) <- repl;
            aux (penalty+p) rest
          ) repls
      in
      aux 0 lits_map
    in
    if CCList.is_empty new_lits_map then [] 
    else compute_results new_lits_map

  let neg_ext_simpl (c:C.t) : C.t SimplM.t =
    let is_eligible = C.Eligible.always in 
    let applied_neg_ext = ref false in
    let new_lits = 
      C.lits c
      |> CCArray.mapi (fun i l -> 
          match l with 
          | Literal.Equation (lhs,rhs,false) 
            when is_eligible i l && Type.is_fun @@ T.ty lhs ->
            let arg_types = Type.expected_args @@ T.ty lhs in
            let free_vars = Literal.vars l |> T.VarSet.of_list |> T.VarSet.to_list in
            let skolem_decls = ref [] in
            let skolems = List.map (fun ty -> 
                let sk, res =  T.mk_fresh_skolem free_vars ty in
                skolem_decls := sk :: !skolem_decls;
                res) arg_types in
            applied_neg_ext := true;
            declare_skolems !skolem_decls;
            Literal.mk_neq (T.app lhs skolems) (T.app rhs skolems)
          | _ -> l) in
    if not !applied_neg_ext then SimplM.return_same c
    else (
      let proof = 
        Proof.Step.simp [C.proof_parent c]
          ~rule:(Proof.Rule.mk "neg_ext_simpl") 
          ~tags:[Proof.Tag.T_ho; Proof.Tag.T_ext] in
      let c' = C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
      (* CCFormat.printf "[NE_simpl]: @[%a@] => @[%a@].\n" C.pp c C.pp c'; *)
      SimplM.return_new c'
    )

  let ord = Ctx.ord ()

  let ext_rule_eligible cl =
    Env.flex_get k_ext_rules_max_depth < 0 ||
    C.proof_depth cl < Env.flex_get k_ext_rules_max_depth

  let update_ext_dec_indices f c =
    let ord = Ctx.ord () in
    let which, eligible = if Env.flex_get k_ext_dec_lits = `OnlyMax 
      then `Max, C.Eligible.res c else `All, C.Eligible.always in
    if Env.flex_get k_ext_rules_kind != `Off &&
      ext_rule_eligible c then (
      Lits.fold_terms ~vars:false ~var_args:false ~fun_bodies:false ~ty_args:false 
        ~ord ~which ~subterms:true ~eligible (C.lits c)
      |> Iter.filter (fun (t, _) ->
          not (T.is_var t) || T.is_ho_var t)
      |> Iter.filter (fun (t, _) ->
          not (T.is_var (T.head_term t)) &&
          T.is_const (T.head_term t) && Term.has_ho_subterm t)
      |> Iter.iter
        (fun (t, pos) ->
          f _ext_dec_into_idx (c,pos,t));

      let eligible = if Env.flex_get k_ext_dec_lits = `OnlyMax then C.Eligible.param c 
        else C.Eligible.always in
      Lits.fold_eqn ~ord ~both:true ~sign:true ~eligible (C.lits c)
      |> Iter.iter
        (fun (l, _, sign, pos) ->
          assert sign;
          let hd,_ = T.as_app l in
          if T.is_const hd && Term.has_ho_subterm l then (
            f _ext_dec_from_idx (c,pos,l)
          )));
    Signal.ContinueListening


  let t_type_is_ho s =
      Type.is_prop (T.ty s) || Type.is_fun (T.ty s)

  (* Given terms s and t, identify maximal common context u
     such that s = u[s1,...,sn] and t = u[t1,...,tn]. Then,
     if some of the disagrements are solvable by a weak
     unification algorihtm (e.g., pattern or fixpoint), filter
     them out and create the unifying substitution. Based on
     k_ho_disagremeents at least one or all of s1...sn have
     to be of functional/boolean type *)
  let find_ho_disagremeents ?(unify=true) (orig_s,s_sc) (orig_t,t_sc) =
    let open CCFun in
    let exception StopSearch in
    let counter = ref 0 in

    let cheap_unify ~subst (s,s_sc) (t,t_sc) =
      let unif_alg =
        if Env.flex_get Combinators.k_enable_combinators then
           (fun s t -> Unif_subst.of_subst @@ Unif.FO.unify_syn ~subst:(Unif_subst.subst subst) s t)
        else if T.is_var s || T.is_var t then (
          FixpointUnif.unify_scoped ~subst ~counter
        ) else PatternUnif.unify_scoped ~subst ~counter in
      
      try
        if not unify then None
        else Some (unif_alg (s,s_sc) (t,t_sc))
      with PatternUnif.NotInFragment | PatternUnif.NotUnifiable | Unif.Fail ->
        None
    in
    
    let rec find_diss ~top s t =
      let return ~top res = if top then [] else res in

      if T.equal s t && (s_sc == t_sc || T.is_ground s) then []
      else (
        match T.view s, T.view t with
        | T.App(s_hd, s_args), T.App(t_hd, t_args) 
            when T.is_const s_hd ->
          let (s_hd, s_args), (t_hd, t_args) = 
            CCPair.map_same T.as_app_mono (s,t) in
          if T.equal s_hd t_hd then find_diss_l s_args t_args
          else return ~top [s,t]
        | T.App(s_hd, s_args), T.App(t_hd, t_args) 
            when not (T.equal s_hd t_hd)
                 && T.is_const s_hd && T.is_const t_hd ->
          (* trying to find prefix subterm that is the differing context *)
          let (s_hd, s_args), (t_hd, t_args) = 
            CCPair.map_same T.as_app_mono (s,t) in

          let lhs,rhs,args_lhs,args_rhs = 
            if List.length s_args > List.length t_args then (
              let taken,dropped = 
                CCList.take_drop (List.length s_args - List.length t_args) s_args in
              T.app s_hd taken, t_hd, dropped, t_args
            ) else (
              let taken,dropped = 
                CCList.take_drop (List.length t_args - List.length s_args) t_args in
              s_hd, T.app t_hd taken, s_args, dropped
            ) in
          if T.same_l args_lhs args_rhs && s_sc == t_sc then ([lhs,rhs])
          else return ~top [s,t]
        | _ -> return ~top [s,t])
    and find_diss_l xs ys =
      match xs,ys with
      | [],[] -> []
      | x :: xxs, y :: yys -> find_diss ~top:false x y @ find_diss_l xxs yys
      | _ -> invalid_arg "args must be of the same length" in
    
    try
      if not (Type.equal (T.ty orig_s) (T.ty orig_t)) then raise StopSearch;

      if T.is_true_or_false orig_s || T.is_true_or_false orig_t then raise StopSearch;

      let norm = 
        if Env.flex_get Combinators.k_enable_combinators
        then CCFun.id 
        else Lambda.eta_expand in

      let diss = find_diss ~top:true (norm orig_s) (norm orig_t) in
      let hd_is_var t = 
        let _,body = T.open_fun t in
        T.is_var @@ T.head_term body in
      
      if CCList.is_empty diss 
          || List.for_all (fun (s,t) -> hd_is_var s || hd_is_var t) diss
          || List.for_all (fun (s,_) -> not @@ t_type_is_ho s) diss then (
          raise StopSearch
      );

      let _,_,unifscope,init_subst =
        if not unify then (orig_s,orig_t,0,US.empty)
        else US.FO.rename_to_new_scope ~counter (orig_s,s_sc) (orig_t,t_sc) in
      let app_subst subst =
        if not unify then (fun (s,_) -> s)
        else Subst.FO.apply Subst.Renaming.none (US.subst subst) in
      
      let init_subst = Unif.Ty.unify_syn ~subst:(US.subst init_subst) 
                        ((T.ty (app_subst init_subst (orig_s, s_sc))), unifscope) 
                        ((T.ty (app_subst init_subst (orig_t, t_sc))), unifscope) in

      (* Filter out the pairs that are easy to unify *)
      let diss = 
        List.fold_left (fun (dis_acc, subst) (si, ti) ->
          let si',ti' = CCPair.map_same (app_subst subst) ((si,s_sc), (ti,t_sc)) in
          if not (Type.is_ground (T.ty si')) || not (Type.is_ground (T.ty ti')) then (
            (* polymorphism is currently not supported *)
            raise StopSearch
          );
          
          let app_ty r s ty sc = Subst.Ty.apply r s (ty,sc) in
          match cheap_unify ~subst (si',unifscope) (ti', unifscope) with
          | Some subst' -> 
            assert (
              let r = Subst.Renaming.create () in
              let s = Unif_subst.subst subst' in
              Type.equal (app_ty r s (T.ty si') unifscope) (app_ty r s (T.ty ti') unifscope) 
            );
            dis_acc, subst'
          | None ->
            assert (
              let r = Subst.Renaming.create () in
              let s = Unif_subst.subst subst in
              Type.equal (app_ty r s (T.ty si) s_sc) (app_ty r s (T.ty ti) t_sc) );
            (si,ti) :: dis_acc, subst)
        ([],US.of_subst init_subst) (diss) in


      (* If no constraints are left or all of pairs are flex-flex
         or all of pairs are FO then we could have done all of 
         this with HO unification or FO superposition *)
      if (CCList.is_empty (fst diss)) then (
          raise StopSearch
      );

      if Env.flex_get k_ho_disagremeents == `AllHo &&
         List.exists (fun (si,_) -> not (t_type_is_ho si)) (fst diss) then (
           raise StopSearch
        );
      Some diss
    with StopSearch -> None
        | Unif.Fail -> None

  
    let ext_inst ~parents (s,s_sc) (t,t_sc) =
      assert(not (CCList.is_empty parents));
      assert(CCList.length parents != 2 || s_sc != t_sc);

      let renaming = Subst.Renaming.create () in
      let apply_subst = Subst.FO.apply renaming Subst.empty  in
      let s, t = apply_subst (s,s_sc), apply_subst (t,t_sc) in
      assert(Type.equal (T.ty s) (T.ty t));
      assert(Type.is_fun (T.ty s));
      
      let ty_args, ret  = Type.open_fun (T.ty s) in
      let alpha = T.of_ty @@ List.hd ty_args in
      let beta = T.of_ty @@ Type.arrow (List.tl ty_args) ret in
      let diff_const = Env.flex_get k_diff_const in
      
      let diff_s_t = T.app diff_const [alpha; beta; s; t] in
      let s_diff, t_diff = T.app s [diff_s_t], T.app t [diff_s_t] in

      let neg_lit = Lit.mk_neq s_diff t_diff in
      let pos_lit = Lit.mk_eq s t in
      let new_lits = [neg_lit; pos_lit] in

      let proof =
            Proof.Step.inference (List.map C.proof_parent parents)
              ~rule:(Proof.Rule.mk "ext_inst") in
      let penalty = List.fold_left max 1 (List.map C.penalty parents) in

      C.create ~trail:(C.trail_l parents) ~penalty new_lits proof

  let do_ext_inst ~parents ((from_t,sc_f) as s) ((into_t,sc_t) as t) =
    match find_ho_disagremeents ~unify:false s t  with
    | Some (disagreements, subst) -> 
      assert (US.is_empty subst);
      let ho_dis = List.filter (fun (s,t) -> Type.is_fun (T.ty s)) disagreements in
      (* assert (not (CCList.is_empty ho_dis)); *)

      CCList.map (fun (lhs,rhs) -> ext_inst ~parents (lhs,sc_f) (rhs,sc_t)) ho_dis
    | None -> []

  let ext_inst_or_family_eqfact cl =
    let try_ext_eq_fact (s,t) (u,v) idx =
      let sc = 0 in
      match find_ho_disagremeents (s,sc) (u,sc) with
      | Some (disagrements, subst) ->
        assert(not (US.has_constr subst));
        let subst = US.subst subst in
        let dis_lits = List.map (fun (a,b) -> Lit.mk_neq a b) disagrements in
        let new_lits = 
          dis_lits @ ((Lit.mk_neq t v) :: CCArray.except_idx (C.lits cl) idx)
          |> CCArray.of_list
          |> (fun lits ->
                Literals.apply_subst (Subst.Renaming.create ()) subst (lits, sc))
          |> CCArray.to_list in
        let proof =
          Proof.Step.inference [C.proof_parent cl] 
            ~rule:(Proof.Rule.mk "ext_eqfact") in
        (* ext_eqfact is rarely used *)
        let new_c = C.create ~trail:(C.trail cl) ~penalty:(C.penalty cl + (C.proof_depth cl)) new_lits proof in
        [new_c]
      | None -> [] in

    let try_ext_eq_factinst (s,_) (u,_) =
      do_ext_inst ~parents:[cl] (s,0) (u,0) in

    let try_factorings (s,t) (u,v) idx =
      let ext_family = 
        if (Env.flex_get k_ext_rules_kind = `Both ||
           Env.flex_get k_ext_rules_kind = `ExtFamily) then (
          try_ext_eq_fact (s,t) (u,v) idx
        ) else [] in
      
      let ext_inst = 
        if (Env.flex_get k_ext_rules_kind = `Both ||
           Env.flex_get k_ext_rules_kind = `ExtInst) then (
          try_ext_eq_factinst (s,t) (u,v)
        ) else [] in
      ext_inst @ ext_family in

    let aux_eq_rest (s,t) i lits = 
      CCList.flatten @@ List.mapi (fun j lit -> 
        if i < j then (
          match lit with 
          | Lit.Equation(u,v,_) when Lit.is_positivoid lit ->
            try_factorings (s,t) (u,v) i
            @
            try_factorings (s,t) (v,u) i 
          | _ -> []
        ) else []) lits in

    let lits = CCArray.to_list (C.lits cl) in
    let maximal = C.eligible_param (cl,0) Subst.empty in
    CCList.flatten @@ List.mapi (fun i lit ->
      match lit with
      | Lit.Equation (s,t,_) 
        when Lit.is_positivoid lit &&
             (Env.flex_get k_ext_dec_lits != `OnlyMax ||
             BV.get maximal i) ->
        aux_eq_rest (s,t) i lits
      | _ -> []
    ) lits

  (* Given a "from"-clause C \/ f t1 ... tn = s  and 
     "into"-clause D \/ f u1 .. un (~)= v, where some of the t_i 
     (and consequently u_i) are of functional type, construct
     a clause C \/ D \/ t1 ~= u1 \/ ... tn ~= un \/ s (~)= v.

     Intuitively, we are waiting for efficient extensionality rules
     to kick in and fix the problem of not being able to paramodulate
     with this equation.

     Currently with no restrictions or indexing. After initial evaluation,
     will find ways to restrict it somehow. *)
  let retrieve_from_extdec_idx idx id = 
    let cl_map = ID.Map.find_opt id idx in
    match cl_map with
    | None -> Iter.empty
    | Some cl_map -> 
      C.Tbl.to_iter cl_map 
      |> Iter.flat_map (fun (c, l) -> 
          Iter.of_list l
          |> Iter.map (fun (t,p) -> (c,t,p)))

  let do_ext_sup from_c from_p from_t into_c into_p into_t = 
    let sc_f, sc_i = 0, 1 in
    if Type.equal (Term.ty from_t) (Term.ty into_t) &&
       not (C.id from_c = C.id into_c && Position.equal from_p into_p) then (

      match find_ho_disagremeents (from_t, sc_f) (into_t, sc_i) with 
      | Some (disagreements, subst) ->
        assert(not @@ US.has_constr subst);        
        let renaming = Subst.Renaming.create () in
        let subst = US.subst subst in
        let lits_f = Lits.apply_subst renaming subst (C.lits from_c, sc_f) in
        let lits_i = Lits.apply_subst renaming subst (C.lits into_c, sc_i) in
        
        let app_subst renaming scoped_t =
          Subst.FO.apply renaming subst scoped_t in
        
        let new_neq_lits = 
          List.map (fun (arg_f, arg_i) ->
            Lit.mk_neq (app_subst renaming (arg_f, sc_f)) (app_subst renaming (arg_i, sc_i))) 
          disagreements  in

        
        let (i, pos_f) = Lits.Pos.cut from_p in
        let from_s = Lits.Pos.at lits_f (Position.arg i (Position.opp pos_f)) in
        Lits.Pos.replace lits_i ~at:into_p ~by:(from_s);
        let new_lits = new_neq_lits @ CCArray.except_idx lits_f i  @ CCArray.to_list lits_i in
        let trail = C.trail_l [from_c; into_c] in
        let penalty = max (C.penalty from_c) (C.penalty into_c) in
        let tags = [Proof.Tag.T_ho] in
        let proof =
          Proof.Step.inference
            [C.proof_parent_subst renaming (from_c, sc_f) subst;
              C.proof_parent_subst renaming  (into_c, sc_i) subst] 
            ~rule:(Proof.Rule.mk "ext_sup") ~tags in
        let new_c = C.create ~trail ~penalty new_lits proof in
        Some new_c
      | None -> None
    ) else None

  let ext_sup_act given =
    if ext_rule_eligible given then (
      let eligible = 
        if Env.flex_get k_ext_dec_lits = `OnlyMax then C.Eligible.param given else C.Eligible.always in
      Lits.fold_eqn ~ord ~both:true ~sign:true ~eligible (C.lits given)
      |> Iter.flat_map (fun (l,_,sign,pos) ->
          let hd,args = T.as_app l in
          if T.is_const hd && T.has_ho_subterm l then (
            let inf_partners = retrieve_from_extdec_idx !_ext_dec_into_idx (T.as_const_exn hd) in
            Iter.map (fun (into_c,into_t, into_p) -> 
                do_ext_sup given pos l into_c into_p into_t) inf_partners)
          else Iter.empty)
      |> Iter.filter_map CCFun.id
      |> Iter.to_list)
    else []

  let ext_sup_pas given =
    if ext_rule_eligible given then ( 
      let which, eligible =
        if Env.flex_get k_ext_dec_lits = `OnlyMax then `Max, C.Eligible.res given 
        else `All, C.Eligible.always in
      Lits.fold_terms ~vars:false ~var_args:false ~fun_bodies:false ~ty_args:false 
        ~ord ~which ~subterms:true ~eligible (C.lits given)
      |> Iter.flat_map (fun (t,p) ->
          let hd, args = T.as_app t in
          if T.is_const hd && T.has_ho_subterm t  then (
            let inf_partners = retrieve_from_extdec_idx !_ext_dec_from_idx (T.as_const_exn hd) in
            Iter.map (fun (from_c,from_t, from_p) -> 
                do_ext_sup from_c from_p from_t given p t) inf_partners) 
          else Iter.empty))
      |> Iter.filter_map CCFun.id
      |> Iter.to_list
    else []

  let ext_inst_sup_act given =
    if ext_rule_eligible given then (
      let eligible =
        if Env.flex_get k_ext_dec_lits = `OnlyMax then C.Eligible.param given else C.Eligible.always in
      Lits.fold_eqn ~ord ~both:true ~sign:true ~eligible (C.lits given)
      |> Iter.flat_map (fun (l,_,sign,pos) ->
          let hd,args = T.as_app l in
          if T.is_const hd && T.has_ho_subterm l then (
            let inf_partners = retrieve_from_extdec_idx !_ext_dec_into_idx (T.as_const_exn hd) in
            Iter.map (fun (into_c,into_t, _) -> 
              do_ext_inst ~parents:[given;into_c] (l, 0) (into_t, 1)
          ) inf_partners)
          else Iter.empty)
      |> Iter.to_list
      |> CCList.flatten)
    else []

  let ext_inst_sup_pas given =
    if ext_rule_eligible given then ( 
      let which, eligible =
        if Env.flex_get k_ext_dec_lits = `OnlyMax then `Max, C.Eligible.res given 
        else `All, C.Eligible.always in
      Lits.fold_terms ~vars:false ~var_args:false ~fun_bodies:false ~ty_args:false 
        ~ord ~which ~subterms:true ~eligible (C.lits given)
      |> Iter.flat_map (fun (t,p) ->
          let hd, args = T.as_app t in
          if T.is_const hd && T.has_ho_subterm t  then (
            Iter.map (fun (from_c,from_t, from_p) -> 
              do_ext_inst ~parents:[from_c; given] (from_t,0) (t,1))
            (retrieve_from_extdec_idx !_ext_dec_from_idx (T.as_const_exn hd)))
          else Iter.empty))
      |> Iter.to_list
      |> CCList.flatten
    else []

  let ext_eqres_aux c =
    let eligible = C.Eligible.always in

    if ext_rule_eligible c then (
      let res = 
        Literals.fold_eqn (C.lits c) ~eligible ~ord ~both:false ~sign:false
        |> Iter.to_list
        |> CCList.filter_map (fun (lhs,rhs,sign,pos) ->
            assert(sign = false);
            let idx = Lits.Pos.idx pos in
            if Env.flex_get k_ext_dec_lits != `OnlyMax ||
               BV.get (C.eligible_res_no_subst c) idx then (
              let sc = 0 in
              match find_ho_disagremeents (lhs,sc) (rhs, sc) with
              | Some (disagremeents, subst) ->
                let new_neq_lits =
                  List.map (fun (s,t) -> Lit.mk_neq s t) disagremeents in
                let i, _ = Literals.Pos.cut pos in
                let new_lits =
                  (Array.of_list @@ new_neq_lits @ CCArray.except_idx (C.lits c) i, sc)
                  |> Literals.apply_subst (Subst.Renaming.create()) (US.subst subst)
                  |> Array.to_list in
                let proof =
                  Proof.Step.inference [C.proof_parent c] ~rule:(Proof.Rule.mk "ext_eqres") in
                let new_c =
                  C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
                Some new_c
              | _ -> None)
            else None)
        in
      Util.incr_stat stat_ext_dec;
      res
    ) else []

  let ext_inst_eqres c =
    let eligible = C.Eligible.always in
    if ext_rule_eligible c then (
      let res = 
        Literals.fold_eqn (C.lits c) ~eligible ~ord ~both:false ~sign:false
        |> Iter.to_list
        |> CCList.flat_map (fun (lhs,rhs,sign,pos) ->
            assert(sign = false);
            let idx = Lits.Pos.idx pos in
            if Env.flex_get k_ext_dec_lits != `OnlyMax ||
               BV.get (C.eligible_res_no_subst c) idx then (
              do_ext_inst ~parents:[c] (lhs,0) (rhs,0))
            else [])
        in
      Util.incr_stat stat_ext_inst;
      res
    ) else []

  let ext_eqres given = 
    ZProf.with_prof prof_ext_dec ext_eqres_aux given

  let insert_into_ext_dec_index index (c,pos,t) =
    let key = T.head_exn t in
    let clause_map = ID.Map.find_opt key !index in
    let clause_map = match clause_map with 
      | None -> C.Tbl.create 8
      | Some res -> res in
    let all_pos =
      (try 
         (t,pos) :: (C.Tbl.find clause_map c)
       with _ -> 
         [(t,pos)]) in
    C.Tbl.replace clause_map c all_pos;
    index := ID.Map.add key clause_map !index

  let remove_from_ext_dec_index index (c,_,t) =
    let key = T.head_exn t in
    let clause_map = ID.Map.find_opt key !index in
    match clause_map with
    | None -> Util.debugf ~section 1 "all clauses allready deleted." CCFun.id
    | Some res -> (
        C.Tbl.remove res c;
        index := ID.Map.add key res !index
      )

  (* try to eliminate a predicate variable in one fell swoop *)
  let elim_pred_variable ?(proof_constructor=Proof.Step.inference) (c:C.t) : C.t list =
    (* find unshielded predicate vars *)
    let find_vars(): _ HVar.t Iter.t =
      Literals.vars (C.lits c)
      |> CCList.to_iter
      |> Iter.filter
        (fun v ->
           (Type.is_prop @@ Type.returns @@ HVar.ty v) &&
           not (Literals.is_shielded v (C.lits c)))
    (* find all constraints on [v], also returns the remaining literals.
       returns None if some constraints contains [v] itself. *)
    and gather_lits v  =
      try
        Array.fold_left
          (fun (others,set,pos_lits) lit ->
             begin match lit with
               | Literal.Equation (lhs, rhs, _) when Literal.is_predicate_lit lit->
                 let f, args = T.as_app lhs in
                 begin match T.view f with
                   | T.Var q when HVar.equal Type.equal v q ->
                     (* found an occurrence *)
                     if List.exists (T.var_occurs ~var:v) args then (
                       raise Exit; (* [P … t[v] …] is out of scope *)
                     );
                     others, (args, Literal.is_positivoid lit) :: set, 
                     (if Literal.is_positivoid lit then [lit] else []) @ pos_lits
                   | _ -> lit :: others, set, pos_lits
                 end
               | _ -> lit :: others, set, pos_lits
             end)
          ([], [], [])
          (C.lits c)
        |> CCOpt.return
      with Exit -> None
    in
    (* try to eliminate [v], if it doesn't occur in its own arguments *)
    let try_elim_var v: _ option =
      (* gather constraints on [v] *)
      begin match gather_lits v with
        | None
        | Some (_, [], _) -> None
        | Some (other_lits, constr_l, pos_lits) ->
          (* gather positive/negative args *)
          let pos_args, neg_args =
            CCList.partition_map
              (fun (args,sign) -> if sign then `Left args else `Right args)
              constr_l
          in
          (* build substitution used for this inference *)
          let subst =
            let some_tup = match pos_args, neg_args with
              | tup :: _, _ | _, tup :: _ -> tup
              | [], [] -> assert false
            in
            let offset = C.Seq.vars c |> T.Seq.max_var |> succ in
            let vars =
              List.mapi (fun i t -> HVar.make ~ty:(T.ty t) (i+offset)) some_tup
            in
            let vars_t = List.map T.var vars in
            let body =
              neg_args
              |> List.map
                (fun tup ->
                   assert (List.length tup = List.length vars);
                   List.map2 T.Form.eq vars_t tup |> T.Form.and_l)
              |> T.Form.or_l
            in
            Util.debugf ~section 1
              "(@[elim-pred-with@ (@[@<1>λ @[%a@].@ %a@])@])"
              (fun k->k (Util.pp_list ~sep:" " Type.pp_typed_var) vars T.pp body);
            Util.incr_stat stat_elim_pred;
            let t = T.fun_of_fvars vars body in
            Subst.FO.of_list [((v:>InnerTerm.t HVar.t),0), (t,0)]
          in
          (* build new clause *)
          let renaming = Subst.Renaming.create () in
          let new_lits =
            let l1 = Literal.apply_subst_list renaming subst (other_lits,0) in
            let l2 = Literal.apply_subst_list renaming subst (pos_lits,0) in
            l1 @ l2
          in
          let proof =
            proof_constructor ~rule:(Proof.Rule.mk "ho_elim_pred") ~tags:[Proof.Tag.T_ho]
              [ C.proof_parent_subst renaming (c,0) subst ]
          in
          let new_c =
            C.create new_lits proof
              ~penalty:(C.penalty c) ~trail:(C.trail c)
          in
          Util.debugf ~section 3
            "(@[<2>elim_pred_var %a@ :clause %a@ :yields %a@])"
            (fun k->k T.pp_var v C.pp c C.pp new_c);
          Some new_c
      end
    in
    begin
      find_vars()
      |> Iter.filter_map try_elim_var
      |> Iter.to_rev_list
    end

  (* maximum penalty on clauses to perform Primitive Enum on *)
  let max_penalty_prim_ = Env.flex_get k_ho_prim_max_penalty

  (* rule for primitive enumeration of predicates [P t1…tn]
     (using ¬ and ∧ and =) *)
  let  prim_enum_ ?(proof_constructor = Proof.Step.inference) ~(mode) (c:C.t) : C.t list =
    (* set of variables to refine (only those occurring in "interesting" lits) *)
    let vars =
      Literals.fold_eqn ~both:false ~ord:(Ctx.ord()) ~eligible:(C.Eligible.always)
        (C.lits c)
      |> Iter.flat_map_l (fun (l,r,_,_) -> 
          let extract_var t = 
            let _, body = T.open_fun t in
            match T.view body with
            | T.Var x -> Some x
            | T.App(hd, _) when T.is_var hd ->  Some (T.as_var_exn hd)
            | _ -> None in
          CCOpt.to_list (extract_var l) @ CCOpt.to_list (extract_var r))
      |> Iter.filter (fun v -> Type.returns_prop @@ HVar.ty v)
      |> T.VarSet.of_iter (* unique *)
    in
    if not (T.VarSet.is_empty vars) then (
      Util.debugf ~section 1 "(@[<hv2>ho.refine@ :clause %a@ :terms {@[%a@]}@])"
        (fun k->k C.pp c (Util.pp_iter T.pp_var) (T.VarSet.to_iter vars));
    );
    let sc_c = 0 in
    let offset = C.Seq.vars c |> T.Seq.max_var |> succ in
    begin
      vars
      |> T.VarSet.to_iter
      |> Iter.flat_map_l
        (fun v -> HO_unif.enum_prop 
            ~add_var:(Env.flex_get k_prim_enum_add_var)
            ~enum_cache:(Env.flex_get k_prim_enum_terms) 
            ~signature:(Ctx.signature ())
            ~mode ~offset (v,sc_c))
      |> Iter.map
        (fun (subst,penalty) ->
          let renaming = Subst.Renaming.create() in
          let lits = Literals.apply_subst renaming subst (C.lits c,sc_c) in
          let proof =
            Proof.Step.inference ~rule:(Proof.Rule.mk "ho.refine") ~tags:[Proof.Tag.T_ho]
              [C.proof_parent_subst renaming (c,sc_c) subst]
          in
          let new_c =
            C.create_a lits proof
              ~penalty:(C.penalty c + penalty) ~trail:(C.trail c)
          in
          Util.debugf ~section 1
            "(@[<hv2>ho.refine@ :from %a@ :subst %a@ :yields %a@])"
            (fun k->k C.pp c Subst.pp subst C.pp new_c);
          Util.incr_stat stat_prim_enum;
          new_c)
      |> Iter.to_rev_list
    end

  let prim_enum ~(mode) c =
    if C.proof_depth c < max_penalty_prim_ 
    then prim_enum_ ~mode c
    else []
    (* prim_enum_ ~mode c *)

  let prim_enum_tf c =
    prim_enum_ ~mode:`TF c

  let choice_ops = ref Term.Map.empty
  let new_choice_counter = ref 0

  let insantiate_choice ?(proof_constructor=Proof.Step.inference) ?(inst_vars=true) ?(choice_ops=choice_ops) c =
    let max_var = 
      ref ((C.Seq.vars c |> Iter.map HVar.id
            |> Iter.max |> CCOpt.get_or ~default: 0) + 1) in

    let is_choice_subterm t =
      match T.view t with
      | T.App(hd, [arg]) when T.is_var hd || Term.Map.mem hd !choice_ops ->
        let ty = T.ty arg in
        Type.is_fun ty && List.length (Type.expected_args ty) = 1 &&
        Type.equal (Term.ty t) (List.hd (Type.expected_args ty)) &&
        Type.returns_prop ty && T.DB.is_closed t
      | T.AppBuiltin(Builtin.ChoiceConst, l) -> 
        CCList.length l == 2 && T.DB.is_closed t
      | _ -> false in

    let neg_trigger t =
      assert(T.DB.is_closed t);
      let arg_ty = List.hd (Type.expected_args (T.ty t)) in
      let negated = T.Form.not_ (Lambda.whnf (T.app t [T.bvar ~ty:arg_ty 0])) in
      let res = T.fun_ arg_ty negated in
      assert(T.DB.is_closed res);
      res in

    let generalize_trigger t =
      assert(T.DB.is_closed t);
      let arg_ty = List.hd (Type.expected_args (T.ty t)) in
      let applied_to_0 = Lambda.whnf (T.app t [T.bvar ~ty:arg_ty 0]) in
      let ty = Type.arrow [Type.prop] Type.prop in
      let fresh_var = T.var @@ HVar.fresh_cnt ~counter:max_var ~ty () in
      let res = T.fun_ arg_ty (T.app fresh_var [applied_to_0]) in
      assert(T.DB.is_closed res);
      res in

    let choice_inst_of_hd ~def_clause hd arg =
      let arg_ty = Term.ty arg in
      let ty = List.hd (Type.expected_args arg_ty) in
      let x = T.var @@ HVar.fresh_cnt ~counter:max_var ~ty () in
      let choice_x = Lambda.whnf (T.app arg [x]) in
      let choice_arg = Lambda.snf (T.app arg [T.app hd [arg]]) in
      let new_lits = [Literal.mk_prop choice_x false;
                      Literal.mk_prop choice_arg true] in
      let arg_str = CCFormat.sprintf "(%a)" T.TPTP.pp arg in
      let parents = match def_clause with
        | Some def -> [C.proof_parent def; C.proof_parent c]
        | None -> [C.proof_parent c] in
      let proof = 
        proof_constructor ~tags:[Proof.Tag.T_cannot_orphan] 
                          ~rule:(Proof.Rule.mk ("inst_choice" ^ arg_str)) 
                          parents in
      C.create ~penalty:1 ~trail:Trail.empty new_lits proof
    in

    let choice_of_ty ty =
      assert (Type.is_fun ty);
      let args,_ = Type.open_fun ty in
      let alpha_to_prop = List.hd args in
      assert(Type.is_fun alpha_to_prop);
      let alpha = List.hd (fst (Type.open_fun alpha_to_prop)) in
      let res = T.app_builtin Builtin.ChoiceConst ~ty [T.of_ty alpha] in
      res 
    in

    (* def_clause is the clause that defined the symbol hd *)
    let generate_instances_of_hd ~def_clause hd arg =
      choice_inst_of_hd ~def_clause hd arg 
      :: choice_inst_of_hd ~def_clause hd (neg_trigger arg)
      :: (if not @@  Env.flex_get k_generalize_choice_trigger then []
          else [choice_inst_of_hd ~def_clause hd (generalize_trigger arg)]) 
    in

    let build_choice_inst t =
      match T.view t with
      | T.App(hd, [arg]) ->
        if Term.is_var hd && inst_vars then (
          let hd_ty = Term.ty hd in
          let choice_ops = 
            Term.Map.filter (fun t _ -> Type.equal (Term.ty t) hd_ty) !choice_ops
            |> Term.Map.to_list
            |> (fun l -> if CCList.is_empty l then [choice_of_ty hd_ty, None] else l) in
          CCList.flat_map (fun (hd,def_clause) -> generate_instances_of_hd ~def_clause hd arg) 
            choice_ops
        ) else (
          match Term.Map.find_opt hd !choice_ops with
          | Some def_clause ->  generate_instances_of_hd ~def_clause hd arg
          | None -> [])
      | T.AppBuiltin(ChoiceConst, [ty_arg;ch_arg]) ->
        let ty = Type.arrow [T.ty ch_arg] (T.ty t) in
        let hd = T.app_builtin ~ty ChoiceConst [ty_arg] in
        generate_instances_of_hd ~def_clause:None hd ch_arg
      | _ -> assert (false) in

    let res = 
      C.Seq.terms c 
      |> Iter.flat_map (Term.Seq.subterms ~include_builtin:true)
      |> Iter.filter is_choice_subterm
      |> Iter.flat_map_l build_choice_inst
      |> Iter.to_list
    in
    if not (CCList.is_empty res) then (
      Util.debugf ~section 1 "inst(@[%a@])=@.@[%a@]@." 
        (fun k -> k C.pp c (CCList.pp C.pp) res);
    );
    res

  (* Given a clause C, project all its applied variables to base-type arguments 
     if there is a variable occurrence in which at least one of base-type arguments is
     not a bound variable.
     Penalty of the resulting clause is penalty of the original clause + penalty_inc *)
  let simple_projection ~penalty_inc ~max_depth c =
    if C.proof_depth c > max_depth then []
    else (
      C.Seq.terms c
      |> Iter.flat_map (Term.Seq.subterms ~include_builtin:true ~ignore_head:true)
      |> Iter.fold (fun var_map subterm -> 
          match T.view subterm with 
          | T.App(hd, args) when T.is_var hd && 
                                 List.exists (fun t -> 
                                     not (Type.is_fun (T.ty t)) && 
                                     not (T.is_bvar t)) 
                                   args -> 
            let var_arg_tys, var_ret_ty = Type.open_fun (T.ty hd) in
            assert (not (Type.is_fun var_ret_ty));

            let new_bindings = 
              CCList.foldi (fun acc idx arg -> 
                  let arg_ty = T.ty arg in
                  if Type.is_ground arg_ty && Type.equal arg_ty var_ret_ty then (
                    let db = T.bvar ~ty:arg_ty (List.length var_arg_tys - 1 - idx) in
                    let binding = T.fun_l var_arg_tys db in
                    Term.Set.add binding acc
                  ) else acc) Term.Set.empty args in
            let old_bindings = Term.Map.get_or hd ~default:Term.Set.empty var_map in

            Term.Map.add hd (Term.Set.union old_bindings new_bindings) var_map
          | _ -> var_map) Term.Map.empty
      |> (fun var_map -> 
          Term.Map.fold (fun var bindings acc -> 
              let var = Term.as_var_exn var in

              Term.Set.fold (fun binding acc ->
                  let subst = (Subst.FO.bind' Subst.empty (var, 0) (binding, 0)) in
                  let proof =
                    Some 
                      (Proof.Step.inference ~rule:(Proof.Rule.mk "simp.projection") 
                         ~tags:[Proof.Tag.T_ho]
                         [C.proof_parent_subst Subst.Renaming.none (c,0) subst]) in
                  let res = C.apply_subst ~penalty_inc:(Some penalty_inc) ~proof (c,0) subst in
                  res :: acc
                ) bindings acc
            ) var_map []))

  let recognize_choice_ops c =
    let extract_not_p_x l = match l with
      | Literal.Equation(lhs,_,_) 
        when Literal.is_negativoid l && Literal.is_predicate_lit l && T.is_app_var lhs ->
        begin match T.view lhs with
          | T.App(hd, [var]) when T.is_var var -> Some hd
          | _ -> None end
      | _ -> None in

    let extract_p_choice_p p l = match l with 
      | Literal.Equation(lhs,_,_) when Literal.is_positivoid l && Literal.is_predicate_lit l ->
        begin match T.view lhs with
          | T.App(hd, [ch_p]) when T.equal hd p ->
            begin match T.view ch_p with 
              | T.App(sym, [var]) when T.is_const sym && T.equal var p -> Some sym
              | _ -> None end
          | _ -> None end
      | _ -> None in

    if C.length c == 2 then (
      let px = CCArray.find_map extract_not_p_x (C.lits c) in
      match px with 
      | Some p ->
        let p_ch_p = CCArray.find_map (extract_p_choice_p p) (C.lits c) in
        begin match p_ch_p with
        | Some sym ->
          if not (Term.Map.mem sym !choice_ops) then (
            choice_ops := Term.Map.add sym (Some c) !choice_ops;
            let new_cls = 
              Env.get_active ()
              |> Iter.flat_map_l (fun pas_cl -> 
                  if C.id pas_cl = C.id c then []
                  else (
                  insantiate_choice ~inst_vars:false 
                                    ~choice_ops:(ref (Term.Map.singleton sym (Some c))) 
                                    pas_cl
                  ))
              |> Iter.map Combs.maybe_conv_lams
            in
            
            Env.add_passive new_cls);
          C.mark_redundant c;
          true
        | None -> false end
      | None -> false
    ) else false

  let elim_leibniz_eq_ ?(proof_constructor=Proof.Step.inference) c =
    let ord = Env.ord () in
    let eligible = C.Eligible.always in
    let pos_pred_vars, neg_pred_vars, occurrences = 
      Lits.fold_eqn ~both:false ~ord ~eligible (C.lits c)
      |> Iter.fold (fun (pos_vs,neg_vs,occ) (lhs,rhs,_,pos) ->
          let i, _ = Literals.Pos.cut pos in
          let lit = (C.lits c).(i) in
          if Literal.is_predicate_lit lit && Term.is_app_var lhs then (
            let var_hd = Term.as_var_exn (Term.head_term lhs) in
            let sign = Literal.is_positivoid lit in
            if sign then (Term.VarSet.add var_hd pos_vs, neg_vs, Term.Map.add lhs true occ)
            else (pos_vs, Term.VarSet.add var_hd neg_vs, Term.Map.add lhs false occ)
          ) else (pos_vs, neg_vs, occ)
        ) (Term.VarSet.empty,Term.VarSet.empty,Term.Map.empty) in
    let pos_neg_vars = Term.VarSet.inter pos_pred_vars neg_pred_vars in
    let res = 
      if Term.VarSet.is_empty pos_neg_vars then []
      else (
        CCList.flat_map (fun (t,sign) -> 
            let hd, args = T.as_app t in
            let var_hd = T.as_var_exn hd in
            if Term.VarSet.mem (Term.as_var_exn hd) pos_neg_vars then (
              let tyargs, _ = Type.open_fun (Term.ty hd) in
              let n = List.length tyargs in
              CCList.filter_map (fun (i,arg) ->
                  if T.var_occurs ~var:var_hd arg then None 
                  else (
                    let body = (if sign then T.Form.neq else T.Form.eq) 
                        arg (T.bvar ~ty:(T.ty arg) (n-i-1)) in
                    let subs_term = T.fun_l tyargs body in 
                    (let cached_t = Subst.FO.canonize_all_vars subs_term in
                      E.flex_add k_prim_enum_terms 
                        (ref (Term.Set.add cached_t !(Env.flex_get k_prim_enum_terms))));
                    let subst = Subst.FO.bind' (Subst.empty) (var_hd, 0) (subs_term, 0) in
                    let rule = Proof.Rule.mk ("elim_leibniz_eq_" ^ (if sign then "+" else "-")) in
                    let tags = [Proof.Tag.T_ho] in
                    let proof = Some (proof_constructor ~rule ~tags [C.proof_parent_subst Subst.Renaming.none (c,0) subst]) in
                    Some (C.apply_subst ~proof (c,0) subst))
                ) (CCList.mapi (fun i arg -> (i, arg)) args)
            ) else [] 
          ) (Term.Map.to_list occurrences)) in
    res


  let elim_leibniz_equality c =
    if C.proof_depth c < Env.flex_get k_elim_leibniz_eq then (
      elim_leibniz_eq_ c
    ) else []

  let elim_andrews_eq_ ?(proof_constructor=Proof.Step.inference) c =
    let ord = Env.ord () in
    let eligible = C.Eligible.always in
    Lits.fold_eqn ~both:false ~ord ~eligible (C.lits c)
    |> Iter.fold (fun cmd_list (lhs,rhs,_,pos) ->
      let i, _ = Lits.Pos.cut pos in
      let lit = (C.lits c).(i) in
      if Lit.is_predicate_lit lit && T.is_app_var lhs then (
        let hd, args = T.as_app lhs in
        assert (T.is_var hd);
        let lam_pref, _ = Type.open_fun (T.ty hd) in
        let return i j  =
          let mk_db idx = 
            let ty = List.nth lam_pref idx in
            let idx = (List.length lam_pref) - idx - 1 in
            T.bvar ~ty idx
          in
          let mk_body ~sign i j = sign (mk_db i) (mk_db j) in
          let sign = if Lit.is_positivoid lit then T.Form.neq else T.Form.eq in
          let subst_t = T.fun_l lam_pref (mk_body ~sign i j) in
          let var = T.as_var_exn hd in
          Some (Subst.FO.bind' Subst.empty (var,0) (subst_t, 0))
        in
        (CCList.filter_map CCFun.id @@ 
          CCList.flat_map_i (fun i arg_i ->
            CCList.mapi (fun j arg_j -> 
              if j > i && T.equal arg_i arg_j then return i j else None
            ) args
          ) args) @ cmd_list
        ) else cmd_list) ([])
    |> List.map (fun subst -> 
      let rule = Proof.Rule.mk "elim_andrews_eq" in
      let tags = [Proof.Tag.T_ho] in
      let proof = Some (proof_constructor ~rule ~tags [C.proof_parent c]) in
      C.apply_subst ~proof (c,0) subst
    )

  let elim_andrews_equality c =
    if C.proof_depth c < Env.flex_get k_elim_andrews_eq then (
      elim_andrews_eq_ c
    ) else []
  
  let elim_andrews_equality_simpls c =
    if C.proof_depth c < Env.flex_get k_elim_andrews_eq then (
      let res = elim_andrews_eq_ c in
      CCOpt.return_if (not (CCList.is_empty res)) res
    ) else None
  

  let pp_pairs_ out =
    let open CCFormat in
    Format.fprintf out "(@[<hv>%a@])"
      (Util.pp_list ~sep:" " @@ hvbox @@ HO_unif.pp_pair)

  (* perform HO unif on [pairs].
     invariant: [C.lits c = pairs @ other_lits] *)
  let ho_unif_real_ c pairs other_lits : C.t list =
    Util.debugf ~section 5
      "(@[ho_unif.try@ :pairs (@[<hv>%a@])@ :other_lits %a@])"
      (fun k->k pp_pairs_ pairs (Util.pp_list~sep:" " Literal.pp) other_lits);
    Util.incr_stat stat_ho_unif;
    let offset = C.Seq.vars c |> T.Seq.max_var |> succ in
    begin
      HO_unif.unif_pairs ?fuel:None (pairs,0) ~offset
      |> List.map
        (fun (new_pairs, us, penalty, renaming) ->
           let subst = Unif_subst.subst us in
           let c_guard = Literal.of_unif_subst renaming us in
           let new_pairs =
             List.map
               (fun (env,t,u) ->
                  assert (env == []);
                  Literal.mk_constraint t u)
               new_pairs
           and other_lits =
             Literal.apply_subst_list renaming subst (other_lits,0)
           in
           let all_lits = c_guard @ new_pairs @ other_lits in
           let proof =
             Proof.Step.inference ~rule:(Proof.Rule.mk "ho_unif") ~tags:[Proof.Tag.T_ho]
               [C.proof_parent_subst renaming (c,0) subst]
           in
           let new_c =
             C.create all_lits proof
               ~trail:(C.trail c) ~penalty:(C.penalty c + penalty)
           in
           Util.debugf ~section 5
             "(@[ho_unif.step@ :pairs (@[%a@])@ :subst %a@ :yields %a@])"
             (fun k->k pp_pairs_ pairs Subst.pp subst C.pp new_c);
           Util.incr_stat stat_ho_unif_steps;
           new_c
        )
    end

  (* HO unification of constraints *)
  let ho_unif (c:C.t) : C.t list =
    if C.lits c |> CCArray.exists Literal.is_ho_constraint then (
      (* separate constraints from the rest *)
      let pairs, others =
        C.lits c
        |> Array.to_list
        |> CCList.partition_map
          (function
            | Literal.Equation (t,u, false) as lit
              when Literal.is_ho_constraint lit -> `Left ([],t,u)
            | lit -> `Right lit)
      in
      assert (pairs <> []);
      let _span = ZProf.enter_prof prof_ho_unif in
      let r = ho_unif_real_ c pairs others in
      ZProf.exit_prof _span;
      r
    ) else []

  (* rule for β-reduction *)
  let beta_reduce t =
    (* assert (T.DB.is_closed t); *)
    let t' = Lambda.snf t in
    if (T.equal t t') then (
      Util.debugf ~section 50 "(@[beta_reduce `%a`@ failed `@])" (fun k->k T.pp t );
      None)
    else (
      Util.debugf ~section 50 "(@[beta_reduce `%a`@ :into `%a`@])"
        (fun k->k T.pp t T.pp t');
      Util.incr_stat stat_beta;
      (* assert (T.DB.is_closed t'); *)
      Some t'
    )

  let eta_normalize () = match Env.flex_get k_eta with
    | `Reduce -> 
      Lambda.eta_reduce 
        ~expand_quant:(not @@ Env.flex_get Combinators.k_enable_combinators) 
        ~full:true
    | `Expand -> Lambda.eta_expand
    | `None -> (fun t -> t)

  (* rule for eta-expansion/reduction *)
  let eta_normalize t =
    (* assert (T.DB.is_closed t); *)
    let t' = eta_normalize () t in
    if (T.equal t t') then (
      Util.debugf ~section 50 "(@[eta_normalize `%a`@ failed `@])" (fun k->k T.pp t );
      None)
    else (
      Util.debugf ~section 50 "(@[eta_normalize `%a`@ :into `%a`@])"
        (fun k->k T.pp t T.pp t');
      Util.incr_stat stat_eta_normalize;
      (* assert (T.DB.is_closed t'); *)
      Some t'
    )

  module TVar = struct
    type t = Type.t HVar.t
    let equal = HVar.equal Type.equal
    let hash = HVar.hash
    let compare = HVar.compare Type.compare
  end
  module VarTermMultiMap = CCMultiMap.Make (TVar) (Term)
  module VTbl = CCHashtbl.Make(TVar)

  let mk_diff_const () = 
    let diff_id = ID.make("zf_ext_diff") in
    ID.set_payload diff_id (ID.Attr_skolem ID.K_normal); (* make the arguments of diff mandatory *)
    let alpha_var = HVar.make ~ty:Type.tType 0 in
    let alpha = Type.var alpha_var in
    let beta_var = HVar.make ~ty:Type.tType 1 in
    let beta = Type.var beta_var in
    let alpha_to_beta = Type.arrow [alpha] beta in
    let diff_type = Type.forall_fvars [alpha_var;beta_var] (Type.arrow [alpha_to_beta; alpha_to_beta] alpha) in
    let diff = Term.const ~ty:diff_type diff_id in

    Env.Ctx.declare diff_id diff_type;
    Env.flex_add k_diff_const diff

  let mk_extensionality_clause () =
    let diff = Env.flex_get k_diff_const in
    let alpha_var = HVar.make ~ty:Type.tType 0 in
    let alpha = Type.var alpha_var in
    let beta_var = HVar.make ~ty:Type.tType 1 in
    let beta = Type.var beta_var in
    let alpha_to_beta = Type.arrow [alpha] beta in    
    let x = Term.var (HVar.make ~ty:alpha_to_beta 2) in
    let y = Term.var (HVar.make ~ty:alpha_to_beta 3) in
    let x_diff = Term.app x [Term.app diff [T.of_ty alpha; T.of_ty beta; x; y]] in
    let y_diff = Term.app y [Term.app diff [T.of_ty alpha; T.of_ty beta; x; y]] in
    let lits = [Literal.mk_eq x y; Literal.mk_neq x_diff y_diff] in
    Env.C.create ~penalty:(Env.flex_get k_ext_axiom_penalty) ~trail:Trail.empty 
                 lits Proof.Step.trivial

  let mk_choice_clause () =
    let choice_id = ID.make("zf_choice") in
    let alpha_var = HVar.make ~ty:Type.tType 0 in
    let alpha = Type.var alpha_var in
    let alpha_to_prop = Type.arrow [alpha] Type.prop in
    let choice_type = Type.arrow [alpha_to_prop] alpha in
    let choice = Term.const ~ty:choice_type choice_id in
    let p = Term.var (HVar.make ~ty:alpha_to_prop 1) in
    let x = Term.var (HVar.make ~ty:alpha 2) in
    let px = Term.app p [x] in (* p x *)
    let p_choice = Term.app p [Term.app choice [p]] (* p (choice p) *) in
    (* ~ (p x) | p (choice p) *)
    let lits = [Literal.mk_prop px false; Literal.mk_prop p_choice true] in
    Env.Ctx.declare choice_id choice_type;
    Env.C.create ~penalty:(Env.flex_get k_choice_axiom_penalty)
                 ~trail:Trail.empty lits Proof.Step.trivial
  
  let mk_ite_clauses () =
    let ite_id = ID.make("zf_ite") in
    let alpha = Type.var (HVar.make ~ty:Type.tType 0) in
    let ite_ty = Type.arrow [Type.prop; alpha; alpha] alpha in
    let ite_const = Term.const ~ty:ite_ty ite_id in
    let x = Term.var (HVar.make ~ty:alpha 1) in
    let y = Term.var (HVar.make ~ty:alpha 2) in
    let if_t = T.app ite_const [T.true_; x; y] in
    let if_f = T.app ite_const [T.false_; x; y] in
    let if_t_cl = 
      C.create ~penalty:1 ~trail:Trail.empty [Lit.mk_eq if_t x]
               Proof.Step.trivial in
    let if_f_cl = 
      C.create ~penalty:1 ~trail:Trail.empty [Lit.mk_eq if_f y]
               Proof.Step.trivial in
    Env.Ctx.declare ite_id ite_ty;
    Iter.of_list [if_t_cl; if_f_cl]

  let early_bird_prim_enum cl var =
    assert(T.is_var var);
    let offset = C.Seq.vars cl |> T.Seq.max_var |> succ in
    let mode = Env.flex_get k_ho_prim_mode in
    let add_var = Env.flex_get k_prim_enum_add_var in
    let sc = 0 in
    
    HO_unif.enum_prop 
      ~enum_cache:(Env.flex_get k_prim_enum_terms) ~add_var
      ~signature:(Ctx.signature ()) ~mode ~offset (T.as_var_exn var,sc)
    |> CCList.map (fun (subst,p) -> 
      let renaming = Subst.Renaming.create () in
      let lits = Literals.apply_subst renaming subst (C.lits cl, sc) in
      let lits = Literals.map (fun t -> Lambda.snf t) lits in
      let proof =
        Proof.Step.inference ~rule:(Proof.Rule.mk "ho.refine.early.bird") 
          ~tags:[Proof.Tag.T_ho; Proof.Tag.T_cannot_orphan]
          [C.proof_parent_subst renaming (cl, sc) subst] in
      let res = C.create_a lits proof ~penalty:(C.penalty cl + (max (p-1) 0)) ~trail:(C.trail cl) in
      let res = Combs.maybe_conv_lams res in
      (* CCFormat.printf "orig:@[%a@]@.subst:@[%a@]@.res:@[%a@]@." C.pp cl Subst.pp subst C.pp res; *)
      res)
    |> CCList.to_iter
    |> Env.add_passive

   let recognize_injectivity c =
    let exception Fail in
    let module Lit = Literal in

    (* avoiding cascading if-then-elses *)
    let fail_on condition =
      if condition then raise Fail in

    let find_in_args var args =
      fst @@ CCOpt.get_or ~default:(-1, T.true_)
        (CCList.find_idx (T.equal var) args) in

    try 
      fail_on (C.length c != 2);

      match C.lits c with
      | [|lit1; lit2|] ->
        fail_on (not ((Lit.is_positivoid lit1 || Lit.is_positivoid lit2) &&
                     (Lit.is_negativoid lit1 || Lit.is_negativoid lit2)));

        let pos_lit,neg_lit = 
          if Lit.is_positivoid lit1 then lit1, lit2 else lit2,lit1 in
       
        begin match pos_lit, neg_lit with
        | Equation(x,y,true), Equation(lhs,rhs,sign) ->
          fail_on (not (T.is_var x && T.is_var y));
          fail_on (T.equal x y);

          let (hd_lhs, lhs_args), (hd_rhs, rhs_args) = 
            CCPair.map_same T.as_app_mono (lhs,rhs) in
          
          fail_on (not (T.is_const hd_lhs && T.is_const hd_rhs));
          fail_on (not (T.equal hd_lhs hd_rhs));
          fail_on (not (List.length lhs_args == List.length rhs_args));

          fail_on (not ((find_in_args x lhs_args) != (-1) ||
                        (find_in_args x rhs_args) != (-1)));
          fail_on (not ((find_in_args y lhs_args) != (-1) ||
                        (find_in_args y rhs_args) != (-1)));
          
          (* reorient equations so that x appears in lhs *)
          let lhs,rhs,lhs_args,rhs_args =
            if find_in_args x lhs_args != -1 
            then (lhs, rhs, lhs_args, rhs_args)
            else (rhs, lhs, rhs_args, lhs_args) in

          fail_on (find_in_args x lhs_args != find_in_args y rhs_args);
          
          let same_vars, diff_eqns = List.fold_left (fun (same, diff) (s,t) -> 
            fail_on (not (T.is_var s && T.is_var t));
            if T.equal s t then (s :: same, diff)
            else (same, (s,t)::diff)
          ) ([],[]) (List.combine lhs_args rhs_args) in

          let same_set = T.Set.of_list same_vars in
          let diff_lhs_set, diff_rhs_set = 
            CCPair.map_same T.Set.of_list (CCList.split diff_eqns) in
          
          (* variables in each group are unique *)
          fail_on (List.length same_vars != T.Set.cardinal same_set);
          fail_on (List.length diff_eqns != T.Set.cardinal diff_lhs_set);
          fail_on (List.length diff_eqns != T.Set.cardinal diff_rhs_set);

          (* variable groups do not intersect *)
          fail_on (not (T.Set.is_empty (T.Set.inter diff_lhs_set diff_rhs_set)));
          fail_on (not (T.Set.is_empty (T.Set.inter diff_lhs_set same_set)));
          fail_on (not (T.Set.is_empty (T.Set.inter diff_rhs_set same_set)));

          let (sk_id, sk_ty),inv_sk = 
            Term.mk_fresh_skolem 
              (List.map T.as_var_exn same_vars) 
              (Type.arrow [T.ty lhs] (T.ty x)) in
          let inv_sk = T.app inv_sk [lhs] in
          let inv_lit = [Lit.mk_eq inv_sk x] in

           let proof = Proof.Step.inference ~rule:(Proof.Rule.mk "inj_rec") 
              [C.proof_parent c] in
          Ctx.declare sk_id sk_ty;
          let new_clause = 
            C.create ~trail:(C.trail c) ~penalty:(C.penalty c) inv_lit proof in
          Util.debugf ~section 1 "Injectivity recognized: %a |---| %a" 
            (fun k -> k C.pp c C.pp new_clause);
          [new_clause]
        | _ -> assert false; end
      | _ -> assert false;
    with Fail -> []

  (* complete [f = g] into [f x1…xn = g x1…xn] for each [n ≥ 1] *)
  let complete_eq_args (c:C.t) : C.t list =
    let var_offset = C.Seq.vars c |> Type.Seq.max_var |> succ in
    let eligible = C.Eligible.param c in
    let aux ?(start=1) ~poly lits lit_idx t u =
      let n_ty_args, ty_args, _ = Type.open_poly_fun (T.ty t) in
      assert (n_ty_args = 0);
      assert (ty_args <> []);
      let vars =
        List.mapi
          (fun i ty -> T.var @@ HVar.make ~ty (i+var_offset))
          ty_args
      in
      CCList.(start -- List.length vars)
      |> List.map
        (fun prefix_len ->
          let vars_prefix = CCList.take prefix_len vars in
          let new_lit = Literal.mk_eq (T.app t vars_prefix) (T.app u vars_prefix) in
          let new_lits = new_lit :: CCArray.except_idx lits lit_idx in
          let proof =
            Proof.Step.inference [C.proof_parent c]
              (* THIS NAME IS USED IN HEURISTICS -- CHANGE CAREFULLY! *)
              ~rule:(Proof.Rule.mk "ho_complete_eq")
              ~tags:[Proof.Tag.T_ho;Proof.Tag.T_dont_increase_depth]
          in
          let penalty = C.penalty c + (if poly then 1 else 0) in
          let new_c =
            C.create new_lits proof ~penalty ~trail:(C.trail c) in

          if poly then (
            C.set_flag SClause.flag_poly_arg_cong_res new_c true;
          );

          new_c)
      in

      let is_poly_arg_cong_res = C.get_flag SClause.flag_poly_arg_cong_res c in
      let new_c =

        C.lits c
        |> Iter.of_array |> Util.seq_zipi
        |> Iter.filter (fun (idx,lit) -> eligible idx lit)
        |> Iter.flat_map_l
          (fun (lit_idx,lit) -> match lit with
            | Literal.Equation (t, u, true) when Type.is_fun (T.ty t) ->
              aux ~poly:false (C.lits c) lit_idx t u
            | Literal.Equation (t, u, true) 
              when Type.is_var (T.ty t) && not is_poly_arg_cong_res ->
              (* A polymorphic variable might be functional on the ground level *)
              let ty_args = 
                OSeq.iterate [Type.var @@ HVar.fresh ~ty:Type.tType ()] 
                  (fun types_w -> 
                    Type.var (HVar.fresh ~ty:Type.tType ()) :: types_w) in
              let res = 
                ty_args
                |> OSeq.mapi (fun arrarg_idx arrow_args ->
                    let var = Type.as_var_exn (T.ty t) in
                    let funty = 
                      T.of_ty 
                        (Type.arrow arrow_args (Type.var (HVar.fresh ~ty:Type.tType ()))) in
                    let subst = Unif_subst.FO.singleton (var,0) (funty,0) in
                    let renaming, subst = Subst.Renaming.none, Unif_subst.subst subst in
                    let lits' = Lits.apply_subst renaming subst (C.lits c, 0) in
                    let t' = Subst.FO.apply renaming subst (t, 0) in
                    let u' = Subst.FO.apply renaming subst (u, 0) in
                    let new_cl = aux ~poly:true ~start:(arrarg_idx+1) lits' lit_idx t' u' in
                    assert(List.length new_cl == 1);
                    List.hd new_cl
                ) in
              let first_two, rest = 
                OSeq.take 2 res, OSeq.map CCOpt.return (OSeq.drop 2 res) in
              let stm =  Stm.make ~penalty:(C.penalty c + 20) ~parents:[c] rest in
              StmQ.add (Env.get_stm_queue ()) stm;
              
              OSeq.to_list first_two
            | _ -> [])
        |> Iter.to_rev_list
      in
      if new_c<>[] then (
        Util.add_stat stat_complete_eq (List.length new_c);
        Util.debugf ~section 4
          "(@[complete-eq@ :clause %a@ :yields (@[<hv>%a@])@])"
          (fun k->k C.pp c (Util.pp_list ~sep:" " C.pp) new_c);
      );
      new_c

  let arg_cong_simpl c =
    let var_offset = ref (C.Seq.vars c |> Type.Seq.max_var |> succ) in
    let simplified = ref false in
    let new_lits = Array.map(function
      | Lit.Equation(lhs, rhs, sign) when sign && Type.is_fun (T.ty lhs) ->
        let tyargs = fst (Type.open_fun (T.ty lhs)) in
        simplified := true;
        let vars = List.map (fun ty -> 
          incr var_offset;
          T.var @@ HVar.make ~ty (!var_offset)) tyargs in
        let lhs',rhs' = T.app lhs vars, T.app rhs vars in
        Lit.mk_eq lhs' rhs'
      | x -> x) (C.lits c) in
    if not !simplified then SimplM.return_same c
    else (
      let proof = Proof.Step.simp ~rule:(Proof.Rule.mk "arg_cong_simpl") [C.proof_parent c] in
      SimplM.return_new 
        (C.create_a ~penalty:(C.penalty c) ~trail:(C.trail c) new_lits proof)
    )


  type fixed_arg_status =
    | Always of T.t (* This argument is always the given term in all occurrences *)
    | Varies        (* This argument contains different terms in differen occurrences *)

  type dupl_arg_status =
    | AlwaysSameAs of int (* This argument is always the same as some other argument across occurrences (links to the next arg with this property) *)
    | Unique              (* This argument is not always the same as some other argument across occurrences *)

  (** Removal of fixed/duplicate arguments of variables.
      - If within a clause, there exists a variable F that's always applied
        to at least i arguments and the ith argument is always the same DB-free term,
        we can systematically remove the argument (and repair F's type).
      - If within a clause, there exist a variable F, and indices i < j
        such that all occurrences of F are applied to at least j arguments and the
        ith argument is syntactically same as the jth argument, we can
        systematically remove the ith argument (and repair F's type accordingly).
  *)
  let prune_arg_old c =
    let status : (fixed_arg_status * dupl_arg_status) list VTbl.t = VTbl.create 8 in
    C.lits c
    |> Literals.fold_terms ~vars:true ~ty_args:false ~which:`All ~ord:Ordering.none 
      ~subterms:true  ~eligible:(fun _ _ -> true)
    |> Iter.iter
      (fun (t,_) ->
         let head, args = T.as_app t in
         match T.as_var head with
         | Some var ->
           begin match VTbl.get status var with
             | Some var_status ->
               (* We have seen this var before *)
               let update_fas fas arg =
                 match fas with
                 | Always u -> if T.equal u arg then Always u else Varies
                 | Varies -> Varies
               in
               let rec update_das das arg =
                 match das with
                 | AlwaysSameAs j ->
                   begin
                     try
                       if T.equal (List.nth args j) arg
                       then AlwaysSameAs j
                       else update_das (snd (List.nth var_status j)) (List.nth args j)
                     with Failure _ -> Unique
                   end
                 | Unique -> Unique
               in
               (* Shorten the lists to have equal lengths. Arguments positions are only interesting if they appear behind every occurrence of a var.*)
               let minlen = min (List.length var_status) (List.length args) in
               let args = CCList.take minlen args in
               let var_status = CCList.take minlen var_status in
               VTbl.replace status var (CCList.map (fun ((fas, das), arg) -> update_fas fas arg, update_das das arg) (List.combine var_status args))
             | None ->
               (* First time to encounter this var *)
               let rec create_var_status ?(i=0) args : (fixed_arg_status * dupl_arg_status) list =
                 match args with
                 | [] -> []
                 | arg :: args' ->
                   let fas =
                     if T.DB.is_closed arg then Always arg else Varies
                   in
                   (* Find next identical argument *)
                   let das = match CCList.find_idx ((Term.equal) arg) args' with
                     | Some (j, _) -> AlwaysSameAs (i + j + 1)
                     | None -> Unique
                   in
                   (fas, das) :: create_var_status ~i:(i+1) args'
               in
               VTbl.add status var (create_var_status args)
           end
         | None -> ()
           ;
           ()
      );
    let subst =
      VTbl.to_list status
      |> CCList.filter_map (
        fun (var, var_status) ->
          assert (not (Type.is_tType (HVar.ty var)));
          let ty_args, ty_return = Type.open_fun (HVar.ty var) in
          let keep = var_status |> CCList.map
                       (fun (fas, das) ->
                          (* Keep argument if this is true: *)
                          fas == Varies && das == Unique
                       )
          in
          if CCList.for_all ((=) true) keep
          then None
          else (
            (* Keep argument if var_status list is not long enough (This happens when the argument does not appear for some occurrence of var): *)
            let keep = CCList.(append keep (replicate (length ty_args - length keep) true)) in
            (* Create substitution: *)
            let ty_args' = ty_args
                           |> CCList.combine keep
                           |> CCList.filter fst
                           |> CCList.map snd
            in
            let var' = HVar.cast var ~ty:(Type.arrow ty_args' ty_return) in
            let bvars =
              CCList.combine keep ty_args
              |> List.mapi (fun i (k, ty) -> k, T.bvar ~ty (List.length ty_args - i - 1))
              |> CCList.filter fst
              |> CCList.map snd
            in
            let replacement = T.fun_l ty_args (T.app (T.var var') bvars) in
            Some ((var,0), (replacement,1))
          )
      )
      |> Subst.FO.of_list'
    in

    if Subst.is_empty subst
    then SimplM.return_same c
    else (
      let renaming = Subst.Renaming.none in
      let new_lits = Lits.apply_subst renaming subst (C.lits c, 0) in
      let proof =
        Proof.Step.simp
          ~rule:(Proof.Rule.mk "prune_arg")
          ~tags:[Proof.Tag.T_ho]
          [C.proof_parent_subst renaming (c,0) subst] in
      let c' = C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
      Util.debugf ~section 3
        "@[<>@[%a@]@ @[<2>prune_arg into@ @[%a@]@]@ with @[%a@]@]"
        (fun k->k C.pp c C.pp c' Subst.pp subst);
      SimplM.return_new c'
    )
  (* TODO: Simplified flag like in first-order? Profiler?*)

  let prune_arg ~all_covers c =
    let get_covers ?(current_sets=[]) head args = 
      let ty_args, _ = Type.open_fun (T.ty head) in
      let missing = CCList.replicate (List.length ty_args - List.length args) None in 
      let args_opt = List.mapi (fun i a_i ->
          assert(Term.DB.is_closed a_i);
          assert(CCList.is_empty current_sets ||
                 List.length current_sets = (List.length args + List.length missing));
          if CCList.is_empty current_sets ||
             not (Term.Set.is_empty (List.nth current_sets i)) then 
            (Some (List.mapi (fun j a_j -> 
                 if i = j then None else Some a_j) args))
          else None (* ignoring onself *))
          args @ missing in
      let res = List.mapi (fun i arg_opt ->
          if i < List.length args then (
            let t = List.nth args i in 
            begin match arg_opt with 
              | Some arg_l ->
                let res_l = if all_covers then T.cover_with_terms t arg_l 
                  else [t; T.max_cover t arg_l] in
                T.Set.of_list res_l 
              | None -> Term.Set.empty 
            end)
          else Term.Set.empty) args_opt in
      res
    in

    let status = VTbl.create 8 in
    let free_vars = Literals.vars (C.lits c) |> T.VarSet.of_list in
    C.lits c
    |> Literals.map (fun t -> Combs.expand t) (* to make sure that DB indices are everywhere the same *)
    |> Literals.fold_terms ~vars:true ~ty_args:false ~which:`All ~ord:Ordering.none 
                           ~subterms:true  ~eligible:(fun _ _ -> true)
    |> Iter.iter
      (fun (t,_) ->
         let head, _ = T.as_app t in
         match T.as_var head with
         | Some var when T.VarSet.mem var free_vars ->
           begin match VTbl.get status var with
             | Some (current_sets, created_sk) ->
               let t, new_sk = T.DB.skolemize_loosely_bound t in
               let new_skolems = T.IntMap.bindings new_sk 
                                 |> List.map snd |> Term.Set.of_list in
               let covers = get_covers ~current_sets head (T.args t) in
               assert(List.length current_sets = List.length covers);
               let paired = CCList.combine current_sets covers in
               let res = List.map (fun (o,n) -> Term.Set.inter o n) paired in
               VTbl.replace status var (res, Term.Set.union created_sk new_skolems);
             | None ->
               let t', created_sk = T.DB.skolemize_loosely_bound t in
               let created_sk = T.IntMap.bindings created_sk
                                |> List.map snd |> Term.Set.of_list in
               VTbl.add status var (get_covers head (T.args t'), created_sk);
           end
         | _ -> ();
           ()
      );

    let subst =
      VTbl.to_list status
      |> CCList.filter_map (fun (var, (args, skolems)) ->
          let removed = ref IntSet.empty in
          let n = List.length args in 
          let keep = List.mapi (fun i arg_set -> 
              let arg_l = Term.Set.to_list arg_set in
              let arg_l = List.filter (fun t -> 
                  List.for_all (fun idx -> 
                      not @@ IntSet.mem idx !removed) (T.DB.unbound t) &&
                  T.Seq.subterms t
                  |> Iter.for_all (fun subt -> not @@ Term.Set.mem subt skolems)) 
                  arg_l in
              let res = CCList.is_empty arg_l in
              if not res then removed := IntSet.add (n-i-1) !removed;
              res) args in
          if CCList.for_all ((=) true) keep then None
          else (
            let ty_args, ty_return = Type.open_fun (HVar.ty var) in
            let ty_args' = 
              CCList.combine keep ty_args
              |> CCList.filter fst |> CCList.map snd
            in
            let var' = HVar.cast var ~ty:(Type.arrow ty_args' ty_return) in
            let bvars =
              CCList.combine keep ty_args
              |> List.mapi (fun i (k, ty) -> k, T.bvar ~ty (List.length ty_args - i - 1))
              |> CCList.filter fst|> CCList.map snd
            in
            let replacement = T.fun_l ty_args (T.app (T.var var') bvars) in
            Some ((var,0), (replacement,1))))
      |> Subst.FO.of_list'
    in

    if Subst.is_empty subst
    then SimplM.return_same c
    else (
      let renaming = Subst.Renaming.none in
      let new_lits = Lits.apply_subst renaming subst (C.lits c, 0) in
      let proof =
        Proof.Step.simp
          ~rule:(Proof.Rule.mk "prune_arg_fun")
          ~tags:[Proof.Tag.T_ho]
          [C.proof_parent_subst renaming (c,0) subst] in
      let c' = C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in

      Util.debugf ~section 3
        "@[<>@[%a@]@ @[<2>prune_arg_fun into@ @[%a@]@]@ with @[%a@]@]"
        (fun k->k C.pp c C.pp c' Subst.pp subst);
      SimplM.return_new c'
    )
  (* TODO: Simplified flag like in first-order? Profiler?*)

  let groundings = Type.Tbl.create 32
  let ground_app_vars ~mode c =
    assert(mode != `Off);
    let app_var = 
      C.Seq.vars c
      |> Iter.filter (fun v -> Type.is_fun (HVar.ty v) && Type.is_ground (HVar.ty v))
      |> Iter.head in

    let get_groundings var =
      let introduce_new_const ty =
        let (f_id, f_ty), f = Term.mk_fresh_skolem [] ty in
        C.Ctx.add_signature (Signature.declare (C.Ctx.signature ()) f_id f_ty);
        f in
      let ty = T.ty var in
      match mode with
      | `Off -> []
      | `Fresh ->
        [Type.Tbl.get_or_add groundings ~f:introduce_new_const ~k:ty]
      | `All ->
        let ids = Signature.find_by_type (C.Ctx.signature ()) ty in
        if not (ID.Set.is_empty ids) then List.map (Term.const ~ty) (ID.Set.to_list ids)
        else [Type.Tbl.get_or_add groundings ~f:introduce_new_const ~k:ty] 
    in
    CCOpt.map (fun v ->
      let inst repl =
        assert (T.is_ground repl);
        let subst = Subst.FO.bind' Subst.empty (v,0) (repl,0) in
        let renaming = Subst.Renaming.none in
        let p = 
          Proof.Step.simp ~rule:(Proof.Rule.mk "ground_app_vars") ~tags:[Proof.Tag.T_ho]
            [C.proof_parent_subst renaming (c,0) subst] in
        let res = C.apply_subst ~renaming ~proof:(Some p) (c,0) subst in
        (* CCFormat.printf "grond: @[%a@] => @[%a@]@." C.pp c C.pp res;
        CCFormat.printf "proof: @[%a@]@." Proof.S.pp_tstp (C.proof res); *)
        res in
      List.map inst (get_groundings (T.var v))
    ) app_var


  let prim_enum_simpl ~mode c =
    if C.proof_depth c < max_penalty_prim_ then (
      (* Primitive enumeration will replace the original clause with 
         instances. This will chage the shape of the clause and disable some
         instantiating rules (e.g. choice and Leibniz equality removal).
         Therefore, we should apply these rules as part of this
         destrutive simplification.
       *)
      let proof_constructor = Proof.Step.simp in
      
      if Env.flex_get k_instantiate_choice_ax && recognize_choice_ops c then None
      else (
        let other_insts =
          (if Env.flex_get k_instantiate_choice_ax
           then (insantiate_choice ~proof_constructor c) else [])
          @ (if C.proof_depth c < Env.flex_get k_elim_leibniz_eq
             then elim_leibniz_eq_ ~proof_constructor c else [])
          @ (if Env.flex_get k_elim_pred_var
            then elim_pred_variable ~proof_constructor c else []) in

        let res = other_insts @ prim_enum_ ~proof_constructor ~mode c  in
        if CCList.is_empty res then None else Some res
      )
    ) else None


  (* Purify variables
     - if they occur applied and unapplied ("int" mode).
     - if they occur with differen argumetns ("ext" mode).
       Example: g X = X a \/ X a = b becomes g X = Y a \/ Y a = b \/ X != Y.
       Literals with only a variable on both sides are not affected. *)
  let purify_applied_variable c =
    (* set of new literals *)
    let new_lits = ref [] in
    let add_lit_ lit = new_lits := lit :: !new_lits in
    (* cache for term headed by variable -> replacement variable *)
    let cache_replacement_ = T.Tbl.create 8 in
    (* cache for variable -> untouched term (the first term we encounter with a certain variable as head) *)
    let cache_untouched_ = VTbl.create 8 in
    (* index of the next fresh variable *)
    let varidx =
      Literals.Seq.terms (C.lits c)
      |> Iter.flat_map T.Seq.vars
      |> T.Seq.max_var |> succ
      |> CCRef.create
    in
    (* variable used to purify a term *)
    let replacement_var t =
      try T.Tbl.find cache_replacement_ t
      with Not_found ->
        let head, _ = T.as_app t in
        let ty = T.ty head in
        let v = T.var_of_int ~ty (CCRef.get_then_incr varidx) in
        let lit = Literal.mk_neq v head in
        add_lit_ lit;
        T.Tbl.add cache_replacement_ t v;
        v
    in
    (* We make the variables of two (variable-headed) terms different if they are
       in different classes.
       For extensional variable purification, two terms are only in the same class
       if they are identical.
       For intensional variable purification, two terms are in the same class if
       they are both unapplied variables or both applied variables. *)
    let same_class t1 t2 =
      assert (T.is_var (fst (T.as_app t1)));
      assert (T.is_var (fst (T.as_app t2)));
      if Env.flex_get k_purify_applied_vars == `Ext
      then
        T.equal t1 t2
      else (
        assert (Env.flex_get k_purify_applied_vars == `Int);
        match T.view t1, T.view t2 with
        | T.Var x, T.Var y when HVar.equal Type.equal x y -> true
        | T.App (f, _), T.App (g, _) when T.equal f g -> true
        | _ -> false
      )
    in
    (* Term should not be purified if
       - this is the first term we encounter with this variable as head or
       - it is equal to the first term encountered with this variable as head *)
    let should_purify t v =
      try
        if same_class t (VTbl.find cache_untouched_ v) then (
          Util.debugf ~section 5
            "Leaving untouched: %a"
            (fun k->k T.pp t);false
        ) else (
          Util.debugf ~section 5
            "To purify: %a"
            (fun k->k T.pp t);true
        )
      with Not_found ->
        VTbl.add cache_untouched_ v t;
        Util.debugf ~section 5
          "Add untouched term: %a"
          (fun k->k T.pp t);
        false
    in
    (* purify a term *)
    let rec purify_term t =
      let head, args = T.as_app t in
      let res = match T.as_var head with
        | Some v ->
          if should_purify t v then (
            (* purify *)
            Util.debugf ~section 5
              "@[Purifying: %a.@ Untouched is: %a@]"
              (fun k->k T.pp t T.pp (VTbl.find cache_untouched_ v));
            let v' = replacement_var t in
            assert (Type.equal (HVar.ty v) (T.ty v'));
            T.app v' (List.map purify_term args)
          ) else (
            (* dont purify *)
            T.app head (List.map purify_term args)
          )
        | None -> (* dont purify *)
          T.app head (List.map purify_term args)
      in
      assert (Type.equal (T.ty res) (T.ty t));
      res
    in
    (* purify a literal *)
    let purify_lit lit =
      (* don't purify literals with only a variable on both sides *)
      if Literal.for_all T.is_var lit
      then lit
      else Literal.map purify_term lit
    in
    (* try to purify *)
    let lits' = Array.map purify_lit (C.lits c) in
    begin match !new_lits with
      | [] -> SimplM.return_same c
      | _::_ ->
        (* replace! *)
        let all_lits = !new_lits @ (Array.to_list lits') in
        let parent = C.proof_parent c in
        let proof =
          Proof.Step.simp
            ~rule:(Proof.Rule.mk "ho.purify_applied_variable") ~tags:[Proof.Tag.T_ho]
            [parent] in
        let new_clause = (C.create ~trail:(C.trail c) ~penalty:(C.penalty c) all_lits proof) in
        Util.debugf ~section 5
          "@[<hv2>Purified:@ Old: %a@ New: %a@]"
          (fun k->k C.pp c C.pp new_clause);
        SimplM.return_new new_clause
    end

  let () =
    Signal.on E.ProofState.ActiveSet.on_add_clause 
      (fun c -> update_ext_dec_indices insert_into_ext_dec_index c);
    Signal.on E.ProofState.ActiveSet.on_remove_clause 
      (fun c -> update_ext_dec_indices remove_from_ext_dec_index c)

  let setup () =
    mk_diff_const ();
    if not (Env.flex_get k_enabled) then (
      Util.debug ~section 1 "HO rules disabled";
    ) else (
      Util.debug ~section 1 "setup HO rules";
      Env.Ctx.lost_completeness();

      if(Env.flex_get k_neg_ext_as_simpl) then (
        Env.add_unary_simplify neg_ext_simpl;
      )
      else if(Env.flex_get k_neg_ext) then (
        Env.add_unary_inf "neg_ext" neg_ext 
      );

      if Env.flex_get k_ext_rules_kind == `ExtFamily ||
        Env.flex_get k_ext_rules_kind == `Both then (
        Env.add_binary_inf "ext_dec_act" ext_sup_act;
        Env.add_binary_inf "ext_dec_pas" ext_sup_pas;
        Env.add_unary_inf "ext_eqres_dec" ext_eqres;
      );

      if Env.flex_get k_ext_rules_kind = `ExtInst ||
        Env.flex_get k_ext_rules_kind = `Both then (
        Env.add_binary_inf "ext_dec_act" ext_inst_sup_act;
        Env.add_binary_inf "ext_dec_pas" ext_inst_sup_pas;
        Env.add_unary_inf "ext_eqres_dec" ext_inst_eqres;
      );

      if Env.flex_get k_ext_rules_kind != `Off then (
        Env.add_unary_inf "ext_eqfact_both" ext_inst_or_family_eqfact;
      );


      if Env.flex_get k_arg_cong_simpl then (
        Env.add_basic_simplify arg_cong_simpl;
      )else if Env.flex_get k_arg_cong then ( 
        Env.add_unary_inf "ho_complete_eq" complete_eq_args
      );

      if Env.flex_get k_resolve_flex_flex then (
        Env.add_basic_simplify remove_ff_constraints
      );

      if Env.flex_get k_ground_app_vars != `Off then (
        let mode = Env.flex_get k_ground_app_vars in
        E.add_cheap_multi_simpl_rule (ground_app_vars ~mode);
        E.add_multi_simpl_rule ~priority:1000 (ground_app_vars ~mode)
      );

      if Env.flex_get k_elim_pred_var then
        Env.add_unary_inf "ho_elim_pred_var" elim_pred_variable;
      if Env.flex_get k_ext_neg_lit then
        Env.add_lit_rule "ho_ext_neg_lit" ext_neg_lit;

      if Env.flex_get k_elim_leibniz_eq > 0 then (
        Env.add_unary_inf "ho_elim_leibniz_eq" elim_leibniz_equality
      );
      if Env.flex_get k_elim_andrews_eq > 0 then (
        if Env.flex_get k_elim_andrews_eq_simpl then (
          Env.add_multi_simpl_rule ~priority:100 elim_andrews_equality_simpls
        ) else Env.add_unary_inf "ho_elim_leibniz_eq" elim_andrews_equality
      );

      if Env.flex_get k_instantiate_choice_ax then (
        Env.add_redundant recognize_choice_ops;
        Env.add_unary_inf "inst_choice" insantiate_choice;
      );

      if Env.flex_get k_ext_pos then (
        Env.add_unary_inf "ho_ext_pos" 
          (ext_pos_general ~all_lits:(Env.flex_get k_ext_pos_all_lits));
      );

      (* removing unfolded clauses *)
      if Env.flex_get k_enable_def_unfold then (
        Env.add_clause_conversion (
          fun c ->  match Statement.get_rw_rule c with
            | Some _ -> E.CR_drop
            | None -> E.CR_skip ));

      begin match Env.flex_get k_prune_arg_fun with
        | `PruneMaxCover -> Env.add_unary_simplify (prune_arg ~all_covers:false);
        | `PruneAllCovers -> Env.add_unary_simplify (prune_arg ~all_covers:true);
        | `OldPrune -> Env.add_unary_simplify prune_arg_old;
        | `NoPrune -> ();
      end;

      if Env.flex_get Combinators.k_enable_combinators then (
        Env.set_ho_normalization_rule "comb-normalize" Combinators_base.comb_normalize;
      ) else (
        let ho_norm = (fun t -> t |> beta_reduce |> (
            fun opt -> match opt with
                None -> eta_normalize t
              | Some t' ->
                match eta_normalize t' with
                  None -> Some t'
                | Some tt -> Some tt))
        in
        Env.set_ho_normalization_rule "ho_norm" ho_norm);
      

      if(Env.flex_get k_purify_applied_vars != `None) then (
        Env.add_unary_simplify purify_applied_variable
      );

      if Env.flex_get k_enable_ho_unif then (
        Env.add_unary_inf "ho_unif" ho_unif;
      );

      if Env.flex_get k_simple_projection >= 0 then (
        let penalty_inc = Env.flex_get k_simple_projection in
        let max_depth = Env.flex_get k_simple_projection_md in
        Env.add_unary_inf "simple_projection" (simple_projection ~penalty_inc ~max_depth);
      );

      if not (Env.flex_get k_prim_enum_early_bird) then (
        begin match Env.flex_get k_ho_prim_mode with
          | `None -> ()
          | mode -> Env.add_unary_inf "ho_prim_enum" (prim_enum ~mode);
        end
      ) else (
        Signal.on Env.on_pred_var_elimination (fun (cl,var) ->
          early_bird_prim_enum cl var;
          Signal.ContinueListening
        )
      );
      Signal.once Env.on_start (fun () -> 
        if Env.flex_get k_ext_axiom then(
          Env.ProofState.PassiveSet.add (Iter.singleton (mk_extensionality_clause ()))) ;
        if Env.flex_get k_choice_axiom then(
          Env.ProofState.PassiveSet.add (Iter.singleton (mk_choice_clause ())));
        if Env.flex_get k_add_ite_axioms then(
          Env.ProofState.PassiveSet.add (mk_ite_clauses ()));
      ));
    ()
end

let enabled_ = ref true
let def_unfold_enabled_ = ref false
let force_enabled_ = ref false
let enable_unif_ = ref false (* this unification seems very buggy, had to turn it off *)
let prim_mode_ = ref `Neg
let prim_max_penalty = ref 2 (* FUDGE *)

let set_prim_mode_ =
  let l = [
    "and", `And;
    "or", `Or;
    "neg", `Neg;
    "eq", `Eq;
    "quants", `Quants;
    "tf", `TF;
    "combs", `Combinators;
    "full", `Full;
    "pragmatic", `Pragmatic;
    "simple", `Simple;
    "none", `None;
  ] in
  let set_ s = prim_mode_ := List.assoc s l in
  Arg.Symbol (List.map fst l, set_)

(* detection of HO statements *)
let st_contains_ho (st:(_,_,_) Statement.t): bool =
  let is_non_atomic_ty ty =
    let n_ty_vars, args, _ = Type.open_poly_fun ty in
    n_ty_vars > 0 || args<>[]
  and has_prop_in_args ty =
    let n_ty_vars, args, _ = Type.open_poly_fun ty in
    List.exists Type.contains_prop args
  in
  (* is there a HO variable? Any variable with a type that is
     Prop or just not an atomic type is. *)
  let has_ho_var () =
    Statement.Seq.terms st
    |> Iter.flat_map T.Seq.vars
    |> Iter.map HVar.ty
    |> Iter.exists (fun ty -> is_non_atomic_ty ty || Type.contains_prop ty)
  (* is there a HO symbol?
     means the symbol has a higher-order, or contains Prop in a sub-position
     of an argument. *)
  and has_ho_sym () =
    Statement.Seq.ty_decls st
    |> Iter.exists
      (fun (_,ty) -> Type.order ty > 1 || has_prop_in_args ty)
  and has_ho_eq() =
    Statement.Seq.forms st
    |> Iter.exists
      (fun c ->
         c |> List.exists
           (function
             | SLiteral.Eq (t,u) | SLiteral.Neq (t,u) ->
               T.is_ho_at_root t || T.is_ho_at_root u || is_non_atomic_ty (T.ty t)
             | _ -> false))
  in
  has_ho_sym () || has_ho_var () || has_ho_eq()

let _ext_pos = ref true
let _ext_pos_all_lits = ref false
let _ext_axiom = ref false
let _choice_axiom = ref false
let _elim_pred_var = ref true
let _ext_neg_lit = ref false
let _neg_ext = ref true
let _neg_ext_as_simpl = ref false
let _ext_axiom_penalty = ref 5
let _choice_axiom_penalty = ref 1
let _huet_style = ref false
let _cons_elim = ref true
let _imit_first = ref false
let _compose_subs = ref false
let _var_solve = ref false
let _instantiate_choice_ax = ref false
let _elim_leibniz_eq = ref (-1)
let _elim_andrews_eq = ref (-1)
let _elim_andrews_eq_simpl = ref (false)
let _prune_arg_fun = ref `NoPrune
let _check_lambda_free = ref `False
let prim_enum_terms = ref Term.Set.empty
let _simple_projection = ref (-1)
let _simple_projection_md = ref 2
let _purify_applied_vars = ref `None
let _eta = ref `Reduce
let _generalize_choice_trigger = ref false
let _prim_enum_simpl = ref false
let _prim_enum_add_var = ref false
let _prim_enum_early_bird = ref false
let _resolve_flex_flex = ref false
let _ground_app_vars = ref `Off
let _arg_cong = ref true
let _arg_cong_simpl = ref false
let ext_rules_max_depth = ref (-1)
let ext_rules_kind = ref (`Off)
let _ext_dec_lits = ref `All
let _ho_disagremeents = ref `SomeHo
let _ite_axioms = ref false

let extension =
  let register env =
    let module E = (val env : Env.S) in

    E.flex_add k_ext_pos !_ext_pos;
    E.flex_add k_ext_pos_all_lits !_ext_pos_all_lits;
    E.flex_add k_ext_axiom !_ext_axiom;
    E.flex_add k_choice_axiom !_choice_axiom;
    E.flex_add k_elim_pred_var !_elim_pred_var;
    E.flex_add k_ext_neg_lit !_ext_neg_lit;
    E.flex_add k_neg_ext !_neg_ext;
    E.flex_add k_neg_ext_as_simpl !_neg_ext_as_simpl;
    E.flex_add k_ext_axiom_penalty !_ext_axiom_penalty;
    E.flex_add k_choice_axiom_penalty !_choice_axiom_penalty;
    E.flex_add k_instantiate_choice_ax !_instantiate_choice_ax;
    E.flex_add k_elim_leibniz_eq !_elim_leibniz_eq;
    E.flex_add k_elim_andrews_eq !_elim_andrews_eq;
    E.flex_add k_elim_andrews_eq_simpl !_elim_andrews_eq_simpl;
    E.flex_add k_prune_arg_fun !_prune_arg_fun;
    E.flex_add k_prim_enum_terms prim_enum_terms;
    E.flex_add k_simple_projection !_simple_projection;
    E.flex_add k_simple_projection_md !_simple_projection_md;
    E.flex_add k_check_lambda_free !_check_lambda_free;
    E.flex_add k_purify_applied_vars !_purify_applied_vars;
    E.flex_add k_eta !_eta;
    E.flex_add k_generalize_choice_trigger !_generalize_choice_trigger;
    E.flex_add k_prim_enum_add_var !_prim_enum_add_var;
    E.flex_add k_prim_enum_early_bird !_prim_enum_early_bird;
    E.flex_add k_resolve_flex_flex !_resolve_flex_flex;
    E.flex_add k_ground_app_vars !_ground_app_vars;
    E.flex_add k_arg_cong !_arg_cong;
    E.flex_add k_arg_cong_simpl !_arg_cong_simpl;

    E.flex_add k_ho_disagremeents !_ho_disagremeents;
    E.flex_add k_ext_dec_lits !_ext_dec_lits;
    E.flex_add k_ext_rules_max_depth !ext_rules_max_depth;
    E.flex_add k_ext_rules_kind !ext_rules_kind;
    E.flex_add k_add_ite_axioms !_ite_axioms;


    if E.flex_get k_check_lambda_free = `Only 
    then E.flex_add Saturate.k_abort_after_fragment_check true;
    
    if E.flex_get k_check_lambda_free != `False then 
      E.add_fragment_check (fun c ->
          E.C.Seq.terms c |> Iter.for_all Term.in_lfho_fragment
        );
    
    if E.flex_get k_some_ho || !force_enabled_ then (
      let module ET = Make(E) in
      ET.setup ()
    )
  (* check if there are HO variables *)
  and check_ho vec state =
    let is_ho =
      CCVector.to_iter vec
      |> Iter.exists st_contains_ho
    in
    if is_ho then (
      Util.debug ~section 2 "problem is HO"
    );

    if !def_unfold_enabled_ then (
      (* let new_vec = *)
      CCVector.iter (fun c -> match Statement.get_rw_rule c with
            Some (sym, r) -> Util.debugf ~section 1
                               "@[<2> Adding constant def rule: `@[%a@]`@]"
                               (fun k->k Rewrite.Rule.pp r);
            Rewrite.Defined_cst.declare_or_add sym  r;
          | _ -> ()) vec (*vec in*)
    );

    state
    |> Flex_state.add k_some_ho is_ho
    |> Flex_state.add k_enabled !enabled_
    |> Flex_state.add k_enable_def_unfold !def_unfold_enabled_
    |> Flex_state.add k_enable_ho_unif (!enabled_ && !enable_unif_)
    |> Flex_state.add k_ho_prim_mode (if !enabled_ then !prim_mode_ else `None)
    |> Flex_state.add k_ho_prim_max_penalty !prim_max_penalty
  in
  { Extensions.default with
    Extensions.name = "ho";
    post_cnf_actions=[check_ho];
    env_actions=[register];
  }

let purify_opt =
  let set_ n = _purify_applied_vars := n in
  let l = [ "ext", `Ext; "int", `Int; "none", `None] in
  Arg.Symbol (List.map fst l, fun s -> set_ (List.assoc s l))

let eta_opt =
  let set_ n = _eta := n in
  let l = [ "reduce", `Reduce; "expand", `Expand; "none", `None] in
  Arg.Symbol (List.map fst l, fun s -> set_ (List.assoc s l))

let () =
  Options.add_opts
    [ "--ho", Arg.Bool (fun b -> enabled_ := b), " enable/disable HO reasoning";
      "--force-ho", Arg.Bool  (fun b -> force_enabled_ := b), " enable/disable HO reasoning even if the problem is first-order";
      "--arg-cong", Arg.Bool (fun v -> _arg_cong := v), " enable/disable ArgCong"; 
      "--arg-cong-simpl", Arg.Bool (fun v -> _arg_cong_simpl := v), " enable/disable ArgCong as a simplification rule"; 
      "--ho-unif", Arg.Bool (fun v -> enable_unif_ := v), " enable full HO unification";
      "--ho-elim-pred-var", Arg.Bool (fun b -> _elim_pred_var := b), " disable predicate variable elimination";
      "--ho-prim-enum", set_prim_mode_, " set HO primitive enum mode";
      "--ho-prim-max", Arg.Set_int prim_max_penalty, " max penalty for HO primitive enum";
      "--ho-prim-enum-add-var", Arg.Bool ((:=) _prim_enum_add_var), " turn an instantiation %x. t into %x. (F x | t)";
      "--ho-prim-enum-early-bird", Arg.Bool ((:=) _prim_enum_early_bird), " use early-bird primitive enumeration (requires lazy CNF)";
      "--ho-resolve-flex-flex", Arg.Bool ((:=) _resolve_flex_flex), " eagerly remove non-essential flex-flex constraints";
      "--ho-ext-axiom", Arg.Bool (fun v -> _ext_axiom := v), " enable/disable extensionality axiom";
      "--ho-choice-axiom", Arg.Bool (fun v -> _choice_axiom := v), " enable choice axiom";
      "--ho-choice-axiom-penalty", Arg.Int (fun v -> _choice_axiom_penalty := v), " choice axiom penalty";
      "--ho-ext-pos", Arg.Bool (fun v -> _ext_pos := v), " enable/disable positive extensionality rule";
      "--ho-neg-ext", Arg.Bool (fun v -> _neg_ext := v), " turn NegExt on or off";
      "--ho-neg-ext-simpl", Arg.Bool (fun v -> _neg_ext_as_simpl := v), " turn NegExt as simplification rule on or off";
      "--ho-ext-pos-all-lits", Arg.Bool (fun v -> _ext_pos_all_lits := v), " turn ExtPos on for all or only eligible literals";
      "--ho-prune-arg", Arg.Symbol (["all-covers"; "max-covers"; "old-prune"; "off"], (fun s ->
          if s = "all-covers" then _prune_arg_fun := `PruneAllCovers
          else if s = "max-covers" then _prune_arg_fun := `PruneMaxCover
          else if s = "old-prune" then _prune_arg_fun := `OldPrune 
          else _prune_arg_fun := `NoPrune)), " choose arg prune mode";
      "--ho-ext-neg-lit", Arg.Bool (fun  v -> _ext_neg_lit := v), " enable/disable negative extensionality rule on literal level [?]";
      "--ho-elim-leibniz", Arg.String (fun v -> 
        match v with 
        | "inf" ->  _elim_leibniz_eq := max_int
        | "off" -> _elim_leibniz_eq := -1
        | _ ->
          match CCInt.of_string v with
          | None -> invalid_arg "number expected for --ho-elim-leibniz"
          | Some x -> _elim_leibniz_eq := x
         ), " enable/disable treatment of Leibniz equality. inf enables it for infinte depth of clauses"
            ^ "; off disables it; number enables it for a given depth of clause";
      "--ho-elim-andrews", Arg.String (fun v -> 
        match v with 
        | "inf" ->  _elim_andrews_eq := max_int
        | "off" -> _elim_andrews_eq := -1
        | _ ->
          match CCInt.of_string v with
          | None -> invalid_arg "number expected for --ho-elim-leibniz"
          | Some x -> _elim_andrews_eq := x
         ), " enable/disable treatment of Andrews equality. inf enables it for infinte depth of clauses"
            ^ "; off disables it; number enables it for a given depth of clause";
      "--ho-elim-andrews-simpl", Arg.Bool ((:=) _elim_andrews_eq_simpl), " use Andrews equality replacement as simplification ";
      "--ho-def-unfold", Arg.Bool (fun v -> def_unfold_enabled_ := v), " enable ho definition unfolding";
      "--ho-choice-inst", Arg.Bool (fun v -> _instantiate_choice_ax := v), " enable heuristic Hilbert choice instantiation";
      "--ho-simple-projection", Arg.Int (fun v -> _simple_projection := v), 
      " enable simple projection instantiation." ^ 
      " positive argument is increase in clause penalty for the conclusion; " ^
      " negative argument turns the inference off";
      "--ho-simple-projection-max-depth", Arg.Set_int _simple_projection_md, " sets the max depth for simple projection";
      "--ho-ext-axiom-penalty", Arg.Int (fun p -> _ext_axiom_penalty := p), " penalty for extensionality axiom";
      "--ho-purify", purify_opt, " enable purification of applied variables: 'ext' purifies" ^
                                 " whenever a variable is applied to different arguments." ^
                                 " 'int' purifies whenever a variable appears applied and unapplied.";
      "--ho-eta", eta_opt, " eta-expansion/reduction";
      "--ground-app-vars", 
        Arg.Symbol (["off"; "fresh"; "all"], (fun kind ->
          match kind with 
          | "off" -> _ground_app_vars := `Off
          | "fresh" -> _ground_app_vars := `Fresh
          | "all" -> _ground_app_vars := `All
          | _ -> assert false)), " ground all applied variables to either all constants of the right type in signature or a fresh constant";
      "--ho-generalize-choice-trigger", Arg.Bool ((:=) _generalize_choice_trigger), " apply choice trigger to a fresh variable";
      "--check-lambda-free", Arg.Symbol (["true";"false";"only"], fun s -> match s with 
        | "true" -> _check_lambda_free := `True
        | "only" -> _check_lambda_free := `Only
        | _ -> _check_lambda_free := `False), "check whether problem belongs to lambda-free ('only' will abort after the check)";
      "--ext-rules-max-depth", Arg.Set_int ext_rules_max_depth, 
        " Sets the maximal proof depth of the clause eligible for Ext-* or ExtInst inferences";
      "--ext-rules", Arg.Symbol (["off"; "ext-inst"; "ext-family"; "both"], (
        function 
        | "off" -> ext_rules_kind := `Off; ext_rules_max_depth:=-1;
        | "ext-inst" -> ext_rules_kind := `ExtInst;
        | "ext-family" -> ext_rules_kind := `ExtFamily;
        | "both" -> ext_rules_kind := `Both
        |  _ -> assert false)),
        " Chooses the kind of extensionality rules to use";
      "--ite-axioms", Arg.Bool ((:=) _ite_axioms), " include if-then-else definition axioms";
      "--ext-decompose-lits", Arg.Symbol (["all";"max"], (fun str -> 
          _ext_dec_lits := if String.equal str "all" then `All else `OnlyMax))
      , " Sets the maximal number of literals clause can have for ExtDec inference.";
      "--ext-decompose-ho-disagreements", Arg.Symbol (["all-ho";"some-ho"], (fun str -> 
          _ho_disagremeents := if String.equal str "all-ho" then `AllHo else `SomeHo))
      , " Perform Ext-Sup, Ext-EqFact, or Ext-EqRes rules only when all disagreements are HO" ^
        " or when there exists a HO disagremeent";
    ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
      enabled_ := true;
      def_unfold_enabled_ := false;
      force_enabled_ := true;
      _ext_axiom := true;
      _choice_axiom := true;
      _ext_neg_lit := false;
      _neg_ext := true;
      _neg_ext_as_simpl := false;
      _ext_pos := true;
      _ext_pos_all_lits := false;
      prim_mode_ := `None;
      _elim_pred_var := false;
      enable_unif_ := false;
      _prune_arg_fun := `PruneMaxCover;
    );
  Params.add_to_mode "ho-pragmatic" (fun () ->
      enabled_ := true;
      def_unfold_enabled_ := false;
      force_enabled_ := true;
      _ext_axiom := false;
      _ext_neg_lit := false;
      _neg_ext := true;
      _neg_ext_as_simpl := false;
      _ext_pos := true;
      _ext_pos_all_lits := true;
      prim_mode_ := `None;
      _elim_pred_var := true;
      enable_unif_ := false;
      _prune_arg_fun := `PruneMaxCover;
    );
  Params.add_to_mode "ho-competitive" (fun () ->
      enabled_ := true;
      def_unfold_enabled_ := false;
      force_enabled_ := true;
      _ext_axiom := false;
      _ext_neg_lit := false;
      _neg_ext := true;
      _neg_ext_as_simpl := false;
      _ext_pos := true;
      _ext_pos_all_lits := true;
      prim_mode_ := `None;
      _elim_pred_var := true;
      enable_unif_ := false;
      _prune_arg_fun := `PruneMaxCover;
  );
  Params.add_to_mode "ho-comb-complete" (fun () ->
    enabled_ := true;
    def_unfold_enabled_ := false;
    _resolve_flex_flex := false;
    force_enabled_ := true;
    _ext_axiom := false;
    _ext_neg_lit := false;
    _neg_ext := true;
    _neg_ext_as_simpl := false;
    _ext_pos := true;
    _ext_pos_all_lits := true;
    prim_mode_ := `None;
    _elim_pred_var := true;
    enable_unif_ := false;
    _prune_arg_fun := `NoPrune;
    Unif._allow_pattern_unif := false;
    _eta  := `None
  );
  Params.add_to_mode "fo-complete-basic" (fun () ->
      enabled_ := false;
      _resolve_flex_flex := false;
      _arg_cong := false;
      Unif._allow_pattern_unif := false;
      Unif._unif_bool := false;
    );
  Params.add_to_modes 
    [ "lambda-free-intensional"
    ; "lambda-free-extensional"
    ; "lambda-free-purify-intensional"
    ; "lambda-free-purify-extensional"] (fun () ->
        enabled_ := true;
        enable_unif_ := false;
        _resolve_flex_flex := false;
        force_enabled_ := true;
        _elim_pred_var := false;
        _neg_ext_as_simpl := false;
        _prune_arg_fun := `NoPrune;
        prim_mode_ := `None;
        _check_lambda_free := `True;
        Unif._allow_pattern_unif := false;
        Unif._unif_bool := false;
        _eta := `None;
      );
  Params.add_to_modes 
    [ "lambda-free-extensional"
    ; "lambda-free-purify-extensional"] (fun () ->
        _ext_axiom := true;
        _neg_ext := true;
        _ext_pos := true;
        _ext_pos_all_lits := true;
      );
  Params.add_to_modes 
    [ "lambda-free-intensional"
    ; "lambda-free-purify-intensional"] (fun () ->
        _ext_axiom := false;
        _neg_ext := false;
        _ext_pos := false;
        _ext_pos_all_lits := false;
      );
  Params.add_to_mode "lambda-free-purify-intensional" (fun () ->
      _purify_applied_vars := `Int
    );
  Params.add_to_mode "lambda-free-purify-extensional" (fun () ->
      _purify_applied_vars := `Ext
    );

  Extensions.register extension;
