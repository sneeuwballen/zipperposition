
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module BV = CCBV
module T = Term
module Lits = Literals

let section = Util.Section.make ~parent:Const.section "ho"

let stat_eq_res = Util.mk_stat "ho.eq_res.steps"
let stat_eq_res_syntactic = Util.mk_stat "ho.eq_res_syntactic.steps"
let stat_ext_neg = Util.mk_stat "ho.extensionality-.steps"
let stat_ext_pos = Util.mk_stat "ho.extensionality+.steps"
let stat_complete_eq = Util.mk_stat "ho.complete_eq.steps"
let stat_beta = Util.mk_stat "ho.beta_reduce.steps"
let stat_eta_expand = Util.mk_stat "ho.eta_expand.steps"
let stat_eta_reduce = Util.mk_stat "ho.eta_reduce.steps"
let stat_prim_enum = Util.mk_stat "ho.prim_enum.steps"
let stat_elim_pred = Util.mk_stat "ho.elim_pred.steps"
let stat_ho_unif = Util.mk_stat "ho.unif.calls"
let stat_ho_unif_steps = Util.mk_stat "ho.unif.steps"

let prof_eq_res = Util.mk_profiler "ho.eq_res"
let prof_eq_res_syn = Util.mk_profiler "ho.eq_res_syntactic"
let prof_ho_unif = Util.mk_profiler "ho.unif"

let _ext_pos = ref true
let _ext_axiom = ref false
let _elim_pred_var = ref true
let _ext_neg = ref true
let _ext_axiom_penalty = ref 5
let _var_arg_remove = ref true
let _huet_style = ref false
let _cons_elim = ref true
let _imit_first = ref false
let _cons_ff = ref true
let _compose_subs = ref false
let _var_solve = ref false
let _unif_max_depth = ref 11

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

let k_some_ho : bool Flex_state.key = Flex_state.create_key()
let k_enabled : bool Flex_state.key = Flex_state.create_key()
let k_enable_def_unfold : bool Flex_state.key = Flex_state.create_key()
let k_enable_ho_unif : bool Flex_state.key = Flex_state.create_key()
let k_ho_prim_mode : _ Flex_state.key = Flex_state.create_key()
let k_ho_prim_max_penalty : int Flex_state.key = Flex_state.create_key()
let k_eta : [`Reduce | `Expand | `None] Flex_state.key = Flex_state.create_key()

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx


  (* index for ext-neg, to ensure α-equivalent negative equations have the same skolems *)
  module FV_ext_neg = FV_tree.Make(struct
      type t = Literal.t * T.t list (* lit -> skolems *)
      let compare = CCOrd.(pair Literal.compare (list T.compare))
      let to_lits (l,_) = Iter.return (Literal.Conv.to_form l)
      let labels _ = Util.Int_set.empty
    end)

  let idx_ext_neg_ : FV_ext_neg.t ref = ref (FV_ext_neg.empty())

  (* retrieve skolems for this literal, if any *)
  let find_skolems_ (lit:Literal.t) : T.t list option =
    FV_ext_neg.retrieve_alpha_equiv_c !idx_ext_neg_ (lit, [])
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

  (* negative extensionality rule:
     [f != g] where [f : a -> b] becomes [f k != g k] for a fresh parameter [k] *)
  let ext_neg (lit:Literal.t) : _ option = match lit with
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
          let l = List.map (T.mk_fresh_skolem vars) ty_args in
          (* save list *)
          idx_ext_neg_ := FV_ext_neg.add !idx_ext_neg_ (lit,l);
          l
      in
      let new_lit =
        Literal.mk_neq
          (T.app f params)
          (T.app g params)
      in
      Util.incr_stat stat_ext_neg;
      Util.debugf ~section 4
        "(@[ho_ext_neg@ :old `%a`@ :new `%a`@])"
        (fun k->k Literal.pp lit Literal.pp new_lit);
      Some (new_lit,[],[Proof.Tag.T_ho; Proof.Tag.T_ext])
    | _ -> None

  (* positive extensionality `m x = n x --> m = n` *)
  let ext_pos (c:C.t): C.t list =
    begin match C.lits c with
      | [| Literal.Equation (t1, t2, true) |] ->
        let f1, l1 = T.as_app_with_mandatory_args t1 in
        let f2, l2 = T.as_app_with_mandatory_args t2 in
        begin match List.rev l1, List.rev l2 with
          | last1 :: l1, last2 :: l2 ->
            begin match T.view last1, T.view last2 with
              | T.Var x, T.Var y
                when HVar.equal Type.equal x y &&
                     not (Type.is_tType (HVar.ty x)) &&
                     begin
                       Iter.of_list
                         [Iter.doubleton f1 f2;
                          Iter.of_list l1;
                          Iter.of_list l2]
                       |> Iter.flatten
                       |> Iter.flat_map T.Seq.vars
                       |> Iter.for_all
                         (fun v' -> not (HVar.equal Type.equal v' x))
                     end ->
                (* it works! *)
                let new_lit =
                  Literal.mk_eq
                    (T.app f1 (List.rev l1))
                    (T.app f2 (List.rev l2))
                in
                let proof =
                  Proof.Step.inference [C.proof_parent c]
                    ~rule:(Proof.Rule.mk "ho_ext_pos")
                    ~tags:[Proof.Tag.T_ho; Proof.Tag.T_ext]
                in
                let new_c =
                  C.create [new_lit] proof ~penalty:(C.penalty c) ~trail:(C.trail c)
                in
                Util.incr_stat stat_ext_pos;
                Util.debugf ~section 4
                  "(@[ext_pos@ :clause %a@ :yields %a@])"
                  (fun k->k C.pp c C.pp new_c);
                [new_c]
              | _ -> []
            end
          | _ -> []
        end
      | _ -> []
    end

  (* complete [f = g] into [f x1…xn = g x1…xn] for each [n ≥ 1] *)
  let complete_eq_args (c:C.t) : C.t list =
    let var_offset = C.Seq.vars c |> Type.Seq.max_var |> succ in
    let eligible = C.Eligible.param c in
    let aux lits lit_idx t u =
      let n_ty_args, ty_args, _ = Type.open_poly_fun (T.ty t) in
      assert (n_ty_args = 0);
      assert (ty_args <> []);
      let vars =
        List.mapi
          (fun i ty -> HVar.make ~ty (i+var_offset) |> T.var)
          ty_args
      in
      CCList.(1 -- List.length vars)
      |> List.map
        (fun prefix_len ->
          let vars_prefix = CCList.take prefix_len vars in
          let new_lit = Literal.mk_eq (T.app t vars_prefix) (T.app u vars_prefix) in
          let new_lits = new_lit :: CCArray.except_idx lits lit_idx in
          let proof =
            Proof.Step.inference [C.proof_parent c]
              ~rule:(Proof.Rule.mk "ho_complete_eq")
          in
          let new_c =
            C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c)
          in
          new_c)
    in
    let new_c =
      C.lits c
      |> Iter.of_array |> Util.seq_zipi
      |> Iter.filter (fun (idx,lit) -> eligible idx lit)
      |> Iter.flat_map_l
        (fun (lit_idx,lit) -> match lit with
           | Literal.Equation (t, u, true) when Type.is_fun (T.ty t) ->
             aux (C.lits c) lit_idx t u
           | Literal.Equation (t, u, true) when Type.is_var (T.ty t) ->
             (* A polymorphic variable might be functional on the ground level *)
             let var = Type.as_var_exn (T.ty t) in
             let funty = T.of_ty (Type.arrow [Type.var (HVar.fresh ~ty:Type.tType ())]
                                             (Type.var (HVar.fresh ~ty:Type.tType ()))) in
             let subst = Unif_subst.FO.singleton (var,0) (funty,0) in
             let renaming, subst = Subst.Renaming.none, Unif_subst.subst subst in
             let lits' = Lits.apply_subst renaming subst (C.lits c, 0) in
             let t' = Subst.FO.apply renaming subst (t, 0) in
             let u' = Subst.FO.apply renaming subst (u, 0) in
             aux lits' lit_idx t' u'
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

  let neg_cong_fun (c:C.t) : C.t list =
    let find_diffs s t = 
      let rec loop s t =
        let (hd_s, args_s), (hd_t, args_t) = T.as_app s , T.as_app t in
        if T.is_const hd_s && T.is_const hd_t then (
          if T.equal hd_s hd_t then (
            let zipped = CCList.combine args_s args_t in
            let zipped = List.filter (fun (a_s, a_t) -> not (T.equal a_s a_t)) zipped in
            if List.length zipped = 1 then (
              let s,t = List.hd zipped in loop s t 
            ) else zipped) 
          else [(s,t)]) 
        else []
      in

      let (hd_s,_), (hd_t,_) = T.as_app s, T.as_app t in
      if T.is_const hd_s && T.is_const hd_t && T.equal hd_s hd_t then (
        loop s t
      ) else [] 
    in
    

    let is_eligible = C.Eligible.res c in
    C.lits c
    |> CCArray.mapi (fun i l -> 
        match l with 
        | Literal.Equation (lhs,rhs,false) when is_eligible i l ->
          let subterms = find_diffs lhs rhs in
          if not (CCList.is_empty subterms) &&
             List.exists (fun (l,_) -> Type.is_fun (T.ty l)) subterms then
             let subterms_lit = CCList.map (fun (l,r) -> 
               let new_lit = Literal.mk_neq l r in
               match ext_neg new_lit with
                 | Some (nl', _, _) -> nl'
                 | None -> new_lit) subterms in
             let new_lits = CCList.flat_map (fun (j,x) -> 
              if i!=j then [x]
              else subterms_lit) 
              (C.lits c |> Array.mapi (fun j x -> (j,x)) |> Array.to_list) in
             let proof =
              Proof.Step.inference [C.proof_parent c] ~rule:(Proof.Rule.mk "neg_cong_fun") in
             let new_c =
               C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c) in
             Some new_c
          else None
        | _ -> None)
    |> CCArray.filter_map (fun x -> x)
    |> CCArray.to_list

  (* try to eliminate a predicate variable in one fell swoop *)
  let elim_pred_variable (c:C.t) : C.t list =
    (* find unshielded predicate vars *)
    let find_vars(): _ HVar.t Iter.t =
      C.Seq.vars c
      |> T.VarSet.of_seq |> T.VarSet.to_seq
      |> Iter.filter
        (fun v ->
           (Type.is_prop @@ Type.returns @@ HVar.ty v) &&
           not (Literals.is_shielded v (C.lits c)))
    (* find all constraints on [v], also returns the remaining literals.
       returns None if some constraints contains [v] itself. *)
    and gather_lits v : (Literal.t list * (T.t list * bool) list) option =
      try
        Array.fold_left
          (fun (others,set) lit ->
             begin match lit with
               | Literal.Prop (t, sign) ->
                 let f, args = T.as_app t in
                 begin match T.view f with
                   | T.Var q when HVar.equal Type.equal v q ->
                     (* found an occurrence *)
                     if List.exists (T.var_occurs ~var:v) args then (
                       raise Exit; (* [P … t[v] …] is out of scope *)
                     );
                     others, (args, sign) :: set
                   | _ -> lit :: others, set
                 end
               | _ -> lit :: others, set
             end)
          ([], [])
          (C.lits c)
        |> CCOpt.return
      with Exit -> None
    in
    (* try to eliminate [v], if it doesn't occur in its own arguments *)
    let try_elim_var v: _ option =
      (* gather constraints on [v] *)
      begin match gather_lits v with
        | None
        | Some (_, []) -> None
        | Some (other_lits, constr_l) ->
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
            Util.debugf ~section 5
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
            let l2 =
              CCList.product
                (fun args_pos args_neg ->
                   let args_pos = Subst.FO.apply_l renaming subst (args_pos,0) in
                   let args_neg = Subst.FO.apply_l renaming subst (args_neg,0) in
                   List.map2 Literal.mk_eq args_pos args_neg)
                pos_args
                neg_args
              |> List.flatten
            in
            l1 @ l2
          in
          let proof =
            Proof.Step.inference ~rule:(Proof.Rule.mk "ho_elim_pred") ~tags:[Proof.Tag.T_ho]
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
  let max_penalty_prim_ = E.flex_get k_ho_prim_max_penalty

  (* rule for primitive enumeration of predicates [P t1…tn]
     (using ¬ and ∧ and =) *)
  let prim_enum_ ~mode (c:C.t) : C.t list =
    (* set of variables to refine (only those occurring in "interesting" lits) *)
    let vars =
      Literals.fold_lits ~eligible:C.Eligible.always (C.lits c)
      |> Iter.map fst
      |> Iter.flat_map Literal.Seq.terms
      |> Iter.flat_map T.Seq.subterms
      |> Iter.filter (fun t -> Type.is_prop (T.ty t))
      |> Iter.filter_map
        (fun t ->
           let hd = T.head_term t in
           begin match T.as_var hd, Type.arity (T.ty hd) with
             | Some v, Type.Arity (0, n)
               when n>0 && Type.returns_prop (T.ty hd) ->
               Some v
             | _ -> None
           end)
      |> T.VarSet.of_seq (* unique *)
    in
    if not (T.VarSet.is_empty vars) then (
      Util.debugf ~section 5 "(@[<hv2>ho.refine@ :clause %a@ :terms {@[%a@]}@])"
        (fun k->k C.pp c (Util.pp_seq T.pp_var) (T.VarSet.to_seq vars));
    );
    let sc_c = 0 in
    let offset = C.Seq.vars c |> T.Seq.max_var |> succ in
    begin
      vars
      |> T.VarSet.to_seq
      |> Iter.flat_map_l
        (fun v -> HO_unif.enum_prop ~mode (v,sc_c) ~offset)
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
           Util.debugf ~section 3
             "(@[<hv2>ho.refine@ :from %a@ :subst %a@ :yields %a@])"
             (fun k->k C.pp c Subst.pp subst C.pp new_c);
           Util.incr_stat stat_prim_enum;
           new_c)
      |> Iter.to_rev_list
    end

  let prim_enum ~mode c =
    if C.penalty c < max_penalty_prim_
    then prim_enum_ ~mode c
    else []

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
      Util.enter_prof prof_ho_unif;
      let r = ho_unif_real_ c pairs others in
      Util.exit_prof prof_ho_unif;
      r
    ) else []

  (* rule for β-reduction *)
  let beta_reduce t =
    assert (T.DB.is_closed t);
    let t' = Lambda.snf t in
    if (T.equal t t') then (
       Util.debugf ~section 50 "(@[beta_reduce `%a`@ failed `@])" (fun k->k T.pp t );
       None)
    else (
      Util.debugf ~section 50 "(@[beta_reduce `%a`@ :into `%a`@])"
        (fun k->k T.pp t T.pp t');
      Util.incr_stat stat_beta;
      assert (T.DB.is_closed t');
      Some t'
   )

  (* rule for eta-expansion *)
  let eta_expand t =
    assert (T.DB.is_closed t);
    let t' = Lambda.eta_expand t in
    if (T.equal t t') then (
       Util.debugf ~section 50 "(@[eta_expand `%a`@ failed `@])" (fun k->k T.pp t );
       None)
    else (
      Util.debugf ~section 50 "(@[eta_expand `%a`@ :into `%a`@])"
        (fun k->k T.pp t T.pp t');
      Util.incr_stat stat_eta_expand;
      assert (T.DB.is_closed t');
      Some t'
    )

  (* rule for eta-expansion *)
  let eta_reduce t =
    assert (T.DB.is_closed t);
    let t' = Lambda.eta_reduce t in
    if (T.equal t t') then None
    else (
      Util.debugf ~section 50 "(@[eta_reduce `%a`@ :into `%a`@])"
        (fun k->k T.pp t T.pp t');
      Util.incr_stat stat_eta_reduce;
      assert (T.DB.is_closed t');
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

  let extensionality_clause =
    let diff_id = ID.make("zf_ext_diff") in
    ID.set_payload diff_id (ID.Attr_skolem (ID.K_normal, 2)); (* make the arguments of diff mandatory *)
    let alpha_var = HVar.make ~ty:Type.tType 0 in
    let alpha = Type.var alpha_var in
    let beta_var = HVar.make ~ty:Type.tType 1 in
    let beta = Type.var beta_var in
    let alpha_to_beta = Type.arrow [alpha] beta in
    let diff_type = Type.forall_fvars [alpha_var;beta_var] (Type.arrow [alpha_to_beta; alpha_to_beta] alpha) in
    let diff = Term.const ~ty:diff_type diff_id in
    let x = Term.var (HVar.make ~ty:alpha_to_beta 2) in
    let y = Term.var (HVar.make ~ty:alpha_to_beta 3) in
    let x_diff = Term.app x [Term.app diff [T.of_ty alpha; T.of_ty beta; x; y]] in
    let y_diff = Term.app y [Term.app diff [T.of_ty alpha; T.of_ty beta; x; y]] in
    let lits = [Literal.mk_eq x y; Literal.mk_neq x_diff y_diff] in
    Env.C.create ~penalty:!_ext_axiom_penalty ~trail:Trail.empty lits Proof.Step.trivial


  type fixed_arg_status =
    | Always of T.t (* This argument is always the given term in all occurences *)
    | Varies        (* This argument contains different terms in differen occurrences *)

  type dupl_arg_status =
    | AlwaysSameAs of int (* This argument is always the same as some other argument across occurences (links to the next arg with this property) *)
    | Unique              (* This argument is not always the same as some other argument across occurences *)

  (** Removal of fixed/duplicate arguments of variables.
    - If within a clause, there exists a variable F that's always applied
      to at least i arguments and the ith argument is always the same DB-free term,
      we can systematically remove the argument (and repair F's type).
    - If within a clause, there exist a variable F, and indices i < j
      such that all occurrences of F are applied to at least j arguments and the
      ith argument is syntactically same as the jth argument, we can
      systematically remove the ith argument (and repair F's type accordingly).
  *)
  let prune_arg c =
    let status : (fixed_arg_status * dupl_arg_status) list VTbl.t = VTbl.create 8 in
    C.lits c
    |> Literals.fold_terms ~vars:true ~ty_args:false ~which:`All ~ord:Ordering.none ~subterms:true ~eligible:(fun _ _ -> true)
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
                  let das = match CCList.find_idx ((=) arg) args' with
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
            [C.proof_parent_subst renaming (c,0) subst] in
      let c' = C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
      Util.debugf ~section 3
          "@[<>@[%a@]@ @[<2>prune_arg into@ @[%a@]@]@ with @[%a@]@]"
          (fun k->k C.pp c C.pp c' Subst.pp subst);
      SimplM.return_new c'
    )
    (* TODO: Simplified flag like in first-order? Profiler?*)

  let setup () =
    if not (Env.flex_get k_enabled) then (
      Util.debug ~section 1 "HO rules disabled";
    ) else (
      Util.debug ~section 1 "setup HO rules";
      Env.Ctx.lost_completeness();
      Env.add_unary_inf "ho_complete_eq" complete_eq_args;
      if !_elim_pred_var then
        Env.add_unary_inf "ho_elim_pred_var" elim_pred_variable;
      if !_ext_neg then
        Env.add_lit_rule "ho_ext_neg" ext_neg;
      if !_ext_pos then (
        Env.add_unary_inf "ho_ext_pos" ext_pos
      );

      (* removing unfolded clauses *)
      if Env.flex_get k_enable_def_unfold then (
         Env.add_clause_conversion (
            fun c ->  match Statement.get_rw_rule c with
                        | Some _ -> E.CR_drop
                        | None -> E.CR_skip ));


      if !_var_arg_remove then
        Env.add_unary_simplify prune_arg;

      let ho_norm  =
      begin match Env.flex_get k_eta with
        | `Expand -> (fun t -> t |> beta_reduce |> (
                        fun opt -> match opt with
                                    None -> eta_expand t
                                    | Some t' ->
                                       match eta_expand t' with
                                          None -> Some t'
                                          | Some tt -> Some tt))
        | `Reduce -> (fun t -> t |> beta_reduce |> (
                        fun opt -> match opt with
                                    None -> eta_reduce t
                                    | Some t' ->
                                       match eta_reduce t' with
                                          None -> Some t'
                                          | Some tt -> Some tt))
        | `None -> beta_reduce
      end;
      in
      Env.set_ho_normalization_rule ho_norm;

      if (!_huet_style) then
        JP_unif.set_huet_style ();

      if (not !_cons_elim) then
        PragHOUnif.disable_conservative_elim ();

      if (!_imit_first) then
        PragHOUnif.enable_imit_first ();

      if (not !_cons_ff) then
        PragHOUnif.disable_cons_ff ();

      if (!_var_solve) then (
        PragHOUnif.enable_solve_var ();
      );

      PragHOUnif.set_max_depth !_unif_max_depth ();

      if Env.flex_get k_enable_ho_unif then (
        Env.add_unary_inf "ho_unif" ho_unif;
      );
      begin match Env.flex_get k_ho_prim_mode with
        | `None -> ()
        | mode ->
          Env.add_unary_inf "ho_prim_enum" (prim_enum ~mode);
      end;
      if !_ext_axiom then
        Env.ProofState.PassiveSet.add (Iter.singleton extensionality_clause);
    );
    ()
end

let enabled_ = ref true
let def_unfold_enabled_ = ref false
let force_enabled_ = ref false
let enable_unif_ = ref true
let prim_mode_ = ref `Neg
let prim_max_penalty = ref 15 (* FUDGE *)
let eta_ = ref `Reduce

let set_prim_mode_ =
  let l = [
    "neg", `Neg;
    "full", `Full;
    "none", `None;
  ] in
  let set_ s = prim_mode_ := List.assoc s l in
  Arg.Symbol (List.map fst l, set_)

let st_contains_ho (st:(_,_,_) Statement.t): bool =
  let is_non_atomic_ty ty =
    let n_ty_vars, args, _ = Type.open_poly_fun ty in
    n_ty_vars > 0 || args<>[]
  in
  (* is there a HO variable? *)
  let has_ho_var () =
    Statement.Seq.terms st
    |> Iter.flat_map T.Seq.vars
    |> Iter.exists (fun v -> is_non_atomic_ty (HVar.ty v))
  (* is there a HO symbol? *)
  and has_ho_sym () =
    Statement.Seq.ty_decls st
    |> Iter.exists (fun (_,ty) -> Type.order ty > 1)
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

let extension =
  let register env =
    let module E = (val env : Env.S) in
    if E.flex_get k_some_ho || !force_enabled_ then (
      let module ET = Make(E) in
      ET.setup ()
    )
  (* check if there are HO variables *)
  and check_ho vec state =
    let is_ho =
      CCVector.to_seq vec
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
    |> Flex_state.add k_eta !eta_
  in
  { Extensions.default with
      Extensions.name = "ho";
      post_cnf_actions=[check_ho];
      env_actions=[register];
  }

let eta_opt =
  let set_ n = eta_ := n in
  let l = [ "reduce", `Reduce; "expand", `Expand; "none", `None] in
  Arg.Symbol (List.map fst l, fun s -> set_ (List.assoc s l))

let () =
  Options.add_opts
    [ "--ho", Arg.Set enabled_, " enable HO reasoning";
      "--force-ho", Arg.Set force_enabled_, " enable HO reasoning even if the problem is first-order";
      "--no-ho", Arg.Clear enabled_, " disable HO reasoning";
      "--ho-unif", Arg.Set enable_unif_, " enable full HO unification";
      "--no-ho-unif", Arg.Clear enable_unif_, " disable full HO unification";
      "--no-ho-elim-pred-var", Arg.Clear _elim_pred_var, " disable predicate variable elimination";
      "--ho-prim-enum", set_prim_mode_, " set HO primitive enum mode";
      "--ho-prim-max", Arg.Set_int prim_max_penalty, " max penalty for HO primitive enum";
      "--ho-eta", eta_opt, " eta-expansion/reduction";
      "--ho-ext-axiom", Arg.Set _ext_axiom, " enable extensionality axiom";
      "--no-ho-ext-axiom", Arg.Clear _ext_axiom, " disable extensionality axiom";
      "--ho-no-ext-pos", Arg.Clear _ext_pos, " disable positive extensionality rule";
      "--ho-no-ext-neg", Arg.Clear _ext_neg, " enable negative extensionality rule";
      "--ho-def-unfold", Arg.Set def_unfold_enabled_, " enable ho definition unfolding";
      "--ho-huet-style-unif", Arg.Set _huet_style, " enable Huet style projection";
      "--ho-no-conservative-elim", Arg.Clear _cons_elim, "Disables conservative elimination rule in pragmatic unification";
      "--ho-imitation-first",Arg.Set _imit_first, "Use imitation rule before projection rule";
      "--ho-no-conservative-flexflex", Arg.Clear _cons_ff, "Disable conservative dealing with flex-flex pairs";
      "--ho-solve-vars", Arg.Set _var_solve, "Enable solving variables.";
      "--ho-composition", Arg.Set _compose_subs, "Enable composition instead of merging substitutions";
      "--ho-disable-var-arg-removal", Arg.Clear _var_arg_remove, "disable removal of arguments of applied variables";
      "--ho-ext-axiom-penalty", Arg.Int (fun p -> _ext_axiom_penalty := p), " penalty for extensionality axiom";
      "--ho-unif-max-depth", Arg.Set_int _unif_max_depth, "set pragmatic unification max depth";
    ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
    enabled_ := true;
    def_unfold_enabled_ := false;
    force_enabled_ := true;
    _ext_axiom := true;
    _ext_neg := true;
    eta_ := `Expand;
    prim_mode_ := `None;
    _elim_pred_var := false;
    enable_unif_ := false
  );
  Params.add_to_mode "fo-complete-basic" (fun () ->
    enabled_ := false;
  );
  Extensions.register extension;
