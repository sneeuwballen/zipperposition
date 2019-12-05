
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module BV = CCBV
module T = Term
module Lits = Literals

module IntSet = Set.Make(CCInt)
module IntMap = Util.Int_map


let section = Util.Section.make ~parent:Const.section "ho"

let stat_eq_res = Util.mk_stat "ho.eq_res.steps"
let stat_eq_res_syntactic = Util.mk_stat "ho.eq_res_syntactic.steps"
let stat_ext_neg_lit = Util.mk_stat "ho.extensionality-.steps"
let stat_ext_pos = Util.mk_stat "ho.extensionality+.steps"
let stat_complete_eq = Util.mk_stat "ho.complete_eq.steps"
let stat_beta = Util.mk_stat "ho.beta_reduce.steps"
let stat_eta_normalize = Util.mk_stat "ho.eta_normalize.steps"
let stat_prim_enum = Util.mk_stat "ho.prim_enum.steps"
let stat_elim_pred = Util.mk_stat "ho.elim_pred.steps"
let stat_ho_unif = Util.mk_stat "ho.unif.calls"
let stat_ho_unif_steps = Util.mk_stat "ho.unif.steps"
let stat_neg_ext = Util.mk_stat "ho.neg_ext_success"
let stat_neg_cong_fun = Util.mk_stat "ho.neg_cong_fun_success"


let prof_eq_res = Util.mk_profiler "ho.eq_res"
let prof_eq_res_syn = Util.mk_profiler "ho.eq_res_syntactic"
let prof_ho_unif = Util.mk_profiler "ho.unif"

let k_ext_pos = Flex_state.create_key ()
let k_ext_pos_all_lits = Flex_state.create_key ()
let k_ext_axiom = Flex_state.create_key ()
let k_choice_axiom = Flex_state.create_key ()
let k_elim_pred_var = Flex_state.create_key ()
let k_ext_neg_lit = Flex_state.create_key ()
let k_neg_ext = Flex_state.create_key ()
let k_neg_ext_as_simpl = Flex_state.create_key ()
let k_ext_axiom_penalty = Flex_state.create_key ()
let k_neg_cong_fun = Flex_state.create_key ()
let k_instantiate_choice_ax = Flex_state.create_key ()
let k_elim_leibniz_eq = Flex_state.create_key ()
let k_unif_max_depth = Flex_state.create_key ()
let k_prune_arg_fun = Flex_state.create_key ()
let k_prim_enum_terms = Flex_state.create_key ()

type prune_kind = [`NoPrune | `OldPrune | `PruneAllCovers | `PruneMaxCover]


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
let k_ho_prim_mode : [`Full | `Neg | `None | `Pragmatic | `TF ] Flex_state.key = Flex_state.create_key()
let k_ho_prim_max_penalty : int Flex_state.key = Flex_state.create_key()

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx


  (* index for ext-neg, to ensure α-equivalent negative equations have the same skolems *)
  module FV_ext_neg_lit = FV_tree.Make(struct
      type t = Literal.t * T.t list (* lit -> skolems *)
      let compare = CCOrd.(pair Literal.compare (list T.compare))
      let to_lits (l,_) = Iter.return (Literal.Conv.to_form l)
      let labels _ = Util.Int_set.empty
    end)

  let idx_ext_neg_lit_ : FV_ext_neg_lit.t ref = ref (FV_ext_neg_lit.empty())

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


  (* positive extensionality `m x = n x --> m = n` *)
  let ext_pos ?(only_unit=true) (c:C.t): C.t list =
    (* CCFormat.printf "EP: %b\n" only_unit; *)
    let is_eligible = C.Eligible.always in
    if not only_unit || C.lits c |> CCArray.length = 1 then 
      C.lits c
      |> CCArray.mapi (fun i l ->
          let l = Literal.map (fun t -> Lambda.eta_reduce ~full:true t) l in
          match l with 
          | Literal.Equation (t1,t2,true) 
            when is_eligible i l ->
            let f1, l1 = T.as_app t1 in
            let f2, l2 = T.as_app t2 in
            begin match List.rev l1, List.rev l2 with
              | last1 :: l1, last2 :: l2 ->
                begin match T.view last1, T.view last2 with
                  | T.Var x, T.Var y
                    when HVar.equal Type.equal x y &&
                         not (Type.is_tType (HVar.ty x)) &&
                         Iter.of_list
                           [Iter.doubleton f1 f2;
                            Iter.of_list l1;
                            Iter.of_list l2]
                         |> Iter.flatten
                         |> Iter.flat_map T.Seq.vars
                         |> Iter.for_all
                           (fun v' -> not (HVar.equal Type.equal v' x)) ->
                    (* it works! *)
                    let new_lit =
                      Literal.mk_eq
                        (T.app f1 (List.rev l1))
                        (T.app f2 (List.rev l2))
                    in
                    let new_lits = C.lits c |> CCArray.to_list |>
                                   List.mapi (fun j l -> if i = j then new_lit else l) in
                    let proof =
                      Proof.Step.inference [C.proof_parent c]
                        ~rule:(Proof.Rule.mk "ho_ext_pos")
                        ~tags:[Proof.Tag.T_ho; Proof.Tag.T_ext]
                    in
                    let new_c =
                      C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c)
                    in
                    Format.printf "@[EP: @[%a@] => @[%a@]@].\n" C.pp c C.pp new_c;
                    Format.force_newline ();
                    Util.incr_stat stat_ext_pos;
                    Util.debugf ~section 4
                      "(@[ext_pos@ :clause %a@ :yields %a@])"
                      (fun k->k C.pp c C.pp new_c);
                    Some new_c
                  | _,_ -> None
                end
              | _ -> None
            end
          | _ -> None)
      |> CCArray.filter_map (fun x -> x)
      |> CCArray.to_list
    else []

  let ext_pos_general ?(all_lits = false) (c:C.t) : C.t list =
    let eligible = if all_lits then C.Eligible.always else C.Eligible.param c in
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
           let lit = Literal.map (fun t -> Lambda.eta_reduce t) lit in
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
               ~tags:[Proof.Tag.T_ho]
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
        else [(s,t)]
      in

      let (hd_s,_), (hd_t,_) = T.as_app s, T.as_app t in
      if T.is_const hd_s && T.is_const hd_t && T.equal hd_s hd_t then (
        let diffs = loop s t in
        if List.for_all (fun (s,t) -> Type.equal (T.ty s) (T.ty t)) diffs 
        then diffs else [] (* because of polymorphism, it might be possible 
                              that terms will not be of the same type,
                              and that will give rise to wrong term applications*)
      ) else [] 
    in
    let is_eligible = C.Eligible.always in
    C.lits c
    |> CCArray.mapi (fun i l -> 
        match l with 
        | Literal.Equation (lhs,rhs,false) when is_eligible i l ->
          let subterms = find_diffs lhs rhs in
          assert(List.for_all (fun (s,t) -> Type.equal (T.ty s) (T.ty t)) subterms);
          if not (CCList.is_empty subterms) &&
             List.exists (fun (l,_) -> 
                 Type.is_fun (T.ty l) || Type.is_prop (T.ty l)) subterms then
            let subterms_lit = CCList.map (fun (l,r) ->
                let free_vars = T.VarSet.union (T.vars l) (T.vars r) |> T.VarSet.to_list in 
                let arg_types = Type.expected_args @@ T.ty l in
                if CCList.is_empty arg_types then Literal.mk_neq l r
                else (
                  let skolem_decls = ref [] in
                  let skolems = List.map (fun ty -> 
                      let sk, res =  T.mk_fresh_skolem free_vars ty in
                      skolem_decls := sk :: !skolem_decls;
                      res) arg_types in
                  declare_skolems !skolem_decls;
                  Literal.mk_neq (T.app l skolems) (T.app r skolems)
                )
              ) subterms in
            let new_lits = CCList.flat_map (fun (j,x) -> 
                if i!=j then [x]
                else subterms_lit) 
                (C.lits c |> Array.mapi (fun j x -> (j,x)) |> Array.to_list) in
            let proof =
              Proof.Step.inference [C.proof_parent c] 
                ~rule:(Proof.Rule.mk "neg_cong_fun") 
                ~tags:[Proof.Tag.T_ho]
            in
            let new_c =
              C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c) in
            Util.incr_stat stat_neg_cong_fun;
            Some new_c
          else None
        | _ -> None)
    |> CCArray.filter_map (fun x -> x)
    |> CCArray.to_list

  let neg_ext (c:C.t) : C.t list =
    let is_eligible = C.Eligible.res c in 
    C.lits c
    |> CCArray.mapi (fun i l -> 
        match l with 
        | Literal.Equation (lhs,rhs,false) 
          when is_eligible i l && Type.is_fun @@ T.ty lhs ->
          let arg_types = Type.expected_args @@ T.ty lhs in
          let free_vars = Literal.vars l in
          let skolem_decls = ref [] in
          let new_lits = CCList.map (fun (j,x) -> 
              if i!=j then x
              else (
                let skolems = List.map (fun ty -> 
                    let sk, res =  T.mk_fresh_skolem free_vars ty in
                    skolem_decls := sk :: !skolem_decls;
                    res) arg_types in
                Literal.mk_neq (T.app lhs skolems) (T.app rhs skolems))
            ) (C.lits c |> Array.mapi (fun j x -> (j,x)) |> Array.to_list) in
          declare_skolems !skolem_decls;
          let proof =
            Proof.Step.inference [C.proof_parent c] 
              ~rule:(Proof.Rule.mk "neg_ext")
              ~tags:[Proof.Tag.T_ho; Proof.Tag.T_ext]
          in
          let new_c =
            C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c) in
          Util.debugf 1 ~section "NegExt: @[%a@] => @[%a@].\n" (fun k -> k C.pp c C.pp new_c);
          Util.incr_stat stat_neg_ext;
          Some new_c
        | _ -> None)
    |> CCArray.filter_map (fun x -> x)
    |> CCArray.to_list

  let neg_ext_simpl (c:C.t) : C.t SimplM.t =
    let is_eligible = C.Eligible.res c in 
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

  (* try to eliminate a predicate variable in one fell swoop *)
  let elim_pred_variable (c:C.t) : C.t list =
    (* find unshielded predicate vars *)
    let find_vars(): _ HVar.t Iter.t =
      Literals.vars (C.lits c)
      |> CCList.to_seq
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
               | Literal.Equation (lhs, rhs, true) when T.equal rhs T.true_ || T.equal rhs T.false_ ->
                 let f, args = T.as_app lhs in
                 let sign = T.equal rhs T.true_ in
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
  let max_penalty_prim_ = Env.flex_get k_ho_prim_max_penalty

  (* rule for primitive enumeration of predicates [P t1…tn]
     (using ¬ and ∧ and =) *)
  let prim_enum_ ~(mode) (c:C.t) : C.t list =
    let free_vars = Literals.vars (C.lits c) |> T.VarSet.of_list in
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
               when n>0 && Type.returns_prop (T.ty hd) && T.VarSet.mem v free_vars ->
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
        (fun v -> HO_unif.enum_prop ~enum_cache:(Env.flex_get k_prim_enum_terms) 
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
           (* CCFormat.printf "[Prim_enum:] @[%a@]\n=>\n@[%a@].\n" C.pp c C.pp new_c;  *)
           Util.debugf ~section 3
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

  let choice_ops = ref Term.Set.empty
  let new_choice_counter = ref 0

  let insantiate_choice ?(inst_vars=true) ?(choice_ops=choice_ops) c =
    let max_var = C.Seq.vars c
                  |> Iter.map HVar.id
                  |> Iter.max
                  |> CCOpt.get_or ~default: 0 in

    let is_choice_subterm t = 
      match T.view t with
      | T.App(hd, [arg]) when T.is_var hd || Term.Set.mem hd !choice_ops ->
        let ty = T.ty arg in
        Type.is_fun ty && List.length (Type.expected_args ty) = 1 &&
        Type.equal (Term.ty t) (List.hd (Type.expected_args ty)) &&
        Type.returns_prop ty && T.DB.is_closed t
      | _ -> false in

    let neg_trigger t =
      assert(T.DB.is_closed t);
      let arg_ty = List.hd (Type.expected_args (T.ty t)) in
      let applied_to_0 = T.Form.not_ (Lambda.whnf (T.app t [T.bvar ~ty:arg_ty 0])) in
      let res = T.fun_ arg_ty applied_to_0 in
      assert(T.DB.is_closed res);
      res in


    let choice_inst_of_hd hd arg =
      let arg_ty = Term.ty arg in
      let ty = List.hd (Type.expected_args arg_ty) in
      let x = T.var_of_int ~ty (max_var+1) in
      let choice_x = Lambda.whnf (T.app arg [x]) in
      let choice_arg = Lambda.snf (T.app arg [T.app hd [arg]]) in
      let new_lits = [Literal.mk_prop choice_x false;
                      Literal.mk_prop choice_arg true] in
      let arg_str = CCFormat.sprintf "%a" T.TPTP.pp arg in
      let proof = Proof.Step.inference ~rule:(Proof.Rule.mk ("inst_choice" ^ arg_str)) [] in
      C.create ~penalty:1 ~trail:Trail.empty new_lits proof in


    let new_choice_op ty =
      let choice_ty_name = "#_choice_" ^ 
                           CCInt.to_string (CCRef.get_then_incr new_choice_counter) in
      let new_ch_id = ID.make choice_ty_name in
      let new_ch_const = T.const new_ch_id ~ty in
      Ctx.add_signature (Signature.declare (C.Ctx.signature ()) new_ch_id ty);
      Util.debugf 1 "new choice for type %a: %a(%a).\n" 
        (fun k -> k Type.pp ty T.pp new_ch_const Type.pp (T.ty new_ch_const));
      choice_ops := Term.Set.add new_ch_const !choice_ops;
      new_ch_const in

    let build_choice_inst t =
      match T.view t with
      | T.App(hd, [arg]) ->
        if Term.is_var hd && inst_vars then (
          let hd_ty = Term.ty hd in
          let choice_ops = 
            Term.Set.filter (fun t -> Type.equal (Term.ty t) hd_ty) !choice_ops
            |> Term.Set.to_list
            |> (fun l -> if CCList.is_empty l then [new_choice_op hd_ty] else l) in
          CCList.flat_map (fun hd -> 
              [choice_inst_of_hd hd arg; choice_inst_of_hd hd (neg_trigger arg)]) 
            choice_ops
        ) else if Term.Set.mem hd !choice_ops then (
          [choice_inst_of_hd hd arg; choice_inst_of_hd hd (neg_trigger arg)]
        ) else []
      | _ -> assert (false) in

    C.Seq.terms c 
    |> Iter.flat_map Term.Seq.subterms
    |> Iter.filter is_choice_subterm
    |> Iter.flat_map_l build_choice_inst
    |> Iter.to_list

  let recognize_choice_ops c =
    let extract_not_p_x l = match l with
      | Literal.Equation(lhs,rhs,true) when T.equal T.false_ rhs && T.is_app_var lhs ->
        begin match T.view lhs with
          | T.App(hd, [var]) when T.is_var var -> Some hd
          | _ -> None end
      | _ -> None in

    let extract_p_choice_p p l = match l with 
      | Literal.Equation(lhs,rhs,true) when T.equal T.true_ rhs && T.is_app_var lhs ->
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
            choice_ops := Term.Set.add sym !choice_ops;
            let new_cls = 
              Env.get_active ()
              |> Iter.flat_map_l (fun pas_cl -> 
                  if C.id pas_cl = C.id c then []
                  else (
                    insantiate_choice ~inst_vars:false 
                      ~choice_ops:(ref (Term.Set.singleton sym)) 
                      pas_cl
                  )) in
            Env.add_passive new_cls;
            C.mark_redundant c;
            true
          | None -> false end
      | None -> false
    ) else false


  let elim_leibniz_equality c =
    if C.proof_depth c < Env.flex_get k_elim_leibniz_eq then (
      let ord = Env.ord () in
      let eligible = C.Eligible.always in
      let pos_pred_vars, neg_pred_vars, occurences = 
        Lits.fold_eqn ~both:false ~ord ~eligible (C.lits c)
        |> Iter.fold (fun (pos_vs,neg_vs,occ) (lhs,rhs,sign,_) -> 
            if Type.is_prop (Term.ty lhs) && Term.is_app_var lhs && sign
               && Term.is_true_or_false rhs then (
              let var_hd = Term.as_var_exn (Term.head_term lhs) in
              if Term.equal T.true_ rhs then (Term.VarSet.add var_hd pos_vs, neg_vs, Term.Map.add lhs true occ)
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
                      let proof = Some (Proof.Step.inference ~rule ~tags [C.proof_parent c]) in
                      Some (C.apply_subst ~proof (c,0) subst))
                  ) (CCList.mapi (fun i arg -> (i, arg)) args)
              ) else [] 
            ) (Term.Map.to_list occurences)) in
      (* CCFormat.printf "Elim Leibniz eq:@ @[%a@].\n" C.pp c;
         CCFormat.printf "Pos/neg vars:@ @[%a@].\n" (Term.VarSet.pp HVar.pp) pos_neg_vars;
         CCFormat.printf "Res:@ @[%a@].\n" (CCList.pp C.pp) res); *)
      res
    ) else []


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

  (* rule for eta-expansion *)
  let eta_normalize t =
    (* assert (T.DB.is_closed t); *)
    let t' = Ctx.eta_normalize t in
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

  let extensionality_clause =
    let diff_id = ID.make("zf_ext_diff") in
    ID.set_payload diff_id (ID.Attr_skolem ID.K_normal); (* make the arguments of diff mandatory *)
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
    Env.C.create ~penalty:(Env.flex_get k_ext_axiom_penalty) ~trail:Trail.empty 
      lits Proof.Step.trivial

  let choice_clause =
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
    Env.C.create ~penalty:1 ~trail:Trail.empty lits Proof.Step.trivial


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
    |> Literals.map (fun t -> Lambda.eta_expand t) (* to make sure that DB indices are everywhere the same *)
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

  let setup () =
    if not (Env.flex_get k_enabled) then (
      Util.debug ~section 1 "HO rules disabled";
    ) else (
      Util.debug ~section 1 "setup HO rules";
      Env.Ctx.lost_completeness();
      Env.add_unary_inf "ho_complete_eq" complete_eq_args;
      if Env.flex_get k_elim_pred_var then
        Env.add_unary_inf "ho_elim_pred_var" elim_pred_variable;
      if Env.flex_get k_ext_neg_lit then
        Env.add_lit_rule "ho_ext_neg_lit" ext_neg_lit;

      if Env.flex_get k_elim_leibniz_eq > 0 then (
        Env.add_unary_inf "ho_elim_leibniz_eq" elim_leibniz_equality
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

      let ho_norm = (fun t -> t |> beta_reduce |> (
          fun opt -> match opt with
              None -> eta_normalize t
            | Some t' ->
              match eta_normalize t' with
                None -> Some t'
              | Some tt -> Some tt))
      in
      Env.set_ho_normalization_rule ho_norm;
      Ordering.normalize := (fun t -> CCOpt.get_or ~default:t (ho_norm t));

      if(Env.flex_get k_neg_cong_fun) then (
        Env.add_unary_inf "neg_cong_fun" neg_cong_fun 
      );

      if(Env.flex_get k_neg_ext) then (
        Env.add_unary_inf "neg_ext" neg_ext 
      )
      else if(Env.flex_get k_neg_ext_as_simpl) then (
        Env.add_unary_simplify neg_ext_simpl;
      );

      if Env.flex_get k_enable_ho_unif then (
        Env.add_unary_inf "ho_unif" ho_unif;
      );
      begin match Env.flex_get k_ho_prim_mode with
        | `None -> ()
        | mode ->
          Env.add_unary_inf "ho_prim_enum" (prim_enum ~mode);
      end;
      if Env.flex_get k_ext_axiom then
        Env.ProofState.PassiveSet.add (Iter.singleton extensionality_clause);
      if Env.flex_get k_choice_axiom then
        Env.ProofState.PassiveSet.add (Iter.singleton choice_clause);
    );
    ()
end

let enabled_ = ref true
let def_unfold_enabled_ = ref false
let force_enabled_ = ref false
let enable_unif_ = ref false (* this unification seems very buggy, had to turn it off *)
let prim_mode_ = ref `Neg
let prim_max_penalty = ref 1 (* FUDGE *)

let set_prim_mode_ =
  let l = [
    "neg", `Neg;
    "full", `Full;
    "pragmatic", `Pragmatic;
    "tf", `TF;
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

let _ext_pos = ref true
let _ext_pos_all_lits = ref false
let _ext_axiom = ref false
let _choice_axiom = ref false
let _elim_pred_var = ref true
let _ext_neg_lit = ref false
let _neg_ext = ref true
let _neg_ext_as_simpl = ref false
let _ext_axiom_penalty = ref 5
let _huet_style = ref false
let _cons_elim = ref true
let _imit_first = ref false
let _compose_subs = ref false
let _var_solve = ref false
let _neg_cong_fun = ref false
let _instantiate_choice_ax = ref false
let _elim_leibniz_eq = ref (-1)
let _unif_max_depth = ref 11
let _prune_arg_fun = ref `NoPrune
let prim_enum_terms = ref Term.Set.empty


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
    E.flex_add k_neg_cong_fun !_neg_cong_fun;
    E.flex_add k_instantiate_choice_ax !_instantiate_choice_ax;
    E.flex_add k_elim_leibniz_eq !_elim_leibniz_eq;
    E.flex_add k_unif_max_depth !_unif_max_depth;
    E.flex_add k_prune_arg_fun !_prune_arg_fun;
    E.flex_add k_prim_enum_terms prim_enum_terms;



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
  in
  { Extensions.default with
    Extensions.name = "ho";
    post_cnf_actions=[check_ho];
    env_actions=[register];
  }


let () =
  Options.add_opts
    [ "--ho", Arg.Bool (fun b -> enabled_ := b), " enable/disable HO reasoning";
      "--force-ho", Arg.Bool  (fun b -> force_enabled_ := b), " enable/disable HO reasoning even if the problem is first-order";
      "--ho-unif", Arg.Bool (fun v -> enable_unif_ := v), " enable full HO unification";
      "--ho-neg-cong-fun", Arg.Bool (fun v -> _neg_cong_fun := v), "enable NegCongFun";
      "--ho-elim-pred-var", Arg.Bool (fun b -> _elim_pred_var := b), " disable predicate variable elimination";
      "--ho-prim-enum", set_prim_mode_, " set HO primitive enum mode";
      "--ho-prim-max", Arg.Set_int prim_max_penalty, " max penalty for HO primitive enum";
      "--ho-ext-axiom", Arg.Bool (fun v -> _ext_axiom := v), " enable/disable extensionality axiom";
      "--ho-choice-axiom", Arg.Bool (fun v -> _choice_axiom := v), " enable choice axiom";
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
      "--ho-elim-leibniz", Arg.Int (fun v -> _elim_leibniz_eq := v), " enable/disable treatment of Leibniz equality";
      "--ho-def-unfold", Arg.Bool (fun v -> def_unfold_enabled_ := v), " enable ho definition unfolding";
      "--ho-choice-inst", Arg.Bool (fun v -> _instantiate_choice_ax := v), " enable ho definition unfolding";
      "--ho-ext-axiom-penalty", Arg.Int (fun p -> _ext_axiom_penalty := p), " penalty for extensionality axiom";
    ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
      enabled_ := true;
      def_unfold_enabled_ := false;
      force_enabled_ := true;
      _ext_axiom := true;
      _ext_neg_lit := false;
      _neg_ext := false;
      _neg_ext_as_simpl := false;
      _ext_pos := true;
      _ext_pos_all_lits := false;
      prim_mode_ := `None;
      _elim_pred_var := false;
      _neg_cong_fun := false;
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
      _neg_cong_fun := false;
      enable_unif_ := false;
      _prune_arg_fun := `PruneMaxCover;
    );
  Params.add_to_mode "ho-competitive" (fun () ->
      enabled_ := true;
      def_unfold_enabled_ := true;
      force_enabled_ := true;
      _ext_axiom := false;
      _ext_neg_lit := false;
      _neg_ext := true;
      _neg_ext_as_simpl := false;
      _ext_pos := true;
      _ext_pos_all_lits := true;
      prim_mode_ := `None;
      _elim_pred_var := true;
      _neg_cong_fun := false;
      enable_unif_ := false;
      _prune_arg_fun := `PruneMaxCover;
    );
  Params.add_to_mode "fo-complete-basic" (fun () ->
      enabled_ := false;
    );
  Extensions.register extension;