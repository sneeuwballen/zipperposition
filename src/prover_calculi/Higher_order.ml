
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
let stat_beta = Util.mk_stat "ho.beta_reduce.steps"
let stat_eta_normalize = Util.mk_stat "ho.eta_normalize.steps"
let stat_prim_enum = Util.mk_stat "ho.prim_enum.steps"
let stat_elim_pred = Util.mk_stat "ho.elim_pred.steps"
let stat_ho_unif = Util.mk_stat "ho.unif.calls"
let stat_ho_unif_steps = Util.mk_stat "ho.unif.steps"
let stat_neg_ext = Util.mk_stat "ho.neg_ext_success"


let prof_eq_res = ZProf.make "ho.eq_res"
let prof_eq_res_syn = ZProf.make "ho.eq_res_syntactic"
let prof_ho_unif = ZProf.make "ho.unif"

let k_ext_pos = Flex_state.create_key ()
let k_ext_pos_all_lits = Flex_state.create_key ()
let k_ext_axiom = Flex_state.create_key ()
let k_choice_axiom = Flex_state.create_key ()
let k_elim_pred_var = Flex_state.create_key ()
let k_ext_neg_lit = Flex_state.create_key ()
let k_neg_ext = Flex_state.create_key ()
let k_neg_ext_as_simpl = Flex_state.create_key ()
let k_ext_axiom_penalty = Flex_state.create_key ()
let k_instantiate_choice_ax = Flex_state.create_key ()
let k_elim_leibniz_eq = Flex_state.create_key ()
let k_prune_arg_fun = Flex_state.create_key ()
let k_prim_enum_terms = Flex_state.create_key ()
let k_simple_projection = Flex_state.create_key ()
let k_simple_projection_md = Flex_state.create_key ()
let k_check_lambda_free = Flex_state.create_key ()
let k_purify_applied_vars = Flex_state.create_key()
let k_eta = Flex_state.create_key()
let k_diff_const = Flex_state.create_key()
let k_use_diff_for_neg_ext = Flex_state.create_key()
let k_generalize_choice_trigger = Flex_state.create_key ()
let k_prim_enum_simpl = Flex_state.create_key ()
let k_prim_enum_early_bird = Flex_state.create_key ()
let k_resolve_flex_flex = Flex_state.create_key ()


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
                    (* Format.printf "@[EP: @[%a@] => @[%a@]@].\n" C.pp c C.pp new_c; *)
                    (* Format.force_newline (); *)
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

  let neg_ext (c:C.t) : C.t list =
    let diff_const = Env.flex_get k_diff_const in

    let rec app_diff_exhaustively s t = function 
    | alpha :: beta :: [] ->
      let diff_s_t = T.app diff_const [T.of_ty alpha;T.of_ty beta;s;t] in
      T.app s [diff_s_t], T.app t [diff_s_t]
    | alpha :: ((beta :: rest) as xs) ->
      assert(not @@ CCList.is_empty rest);
      let rest', ret = CCList.take_drop (List.length rest -1) rest in
      assert(List.length ret = 1);
      let new_beta = Type.arrow (beta::rest') (List.hd ret) in
      let diff_s_t = T.app diff_const [T.of_ty alpha;T.of_ty new_beta;s;t] in
      app_diff_exhaustively (T.app s [diff_s_t]) (T.app t [diff_s_t]) xs
    | _ -> invalid_arg "argument must be a function type" in

    let is_eligible = C.Eligible.res c in 
    C.lits c
    |> CCArray.mapi (fun i l -> 
        match l with 
        | Literal.Equation (lhs,rhs,false) 
          when is_eligible i l && Type.is_fun @@ T.ty lhs ->
          let arg_types, ret_ty = Type.open_fun (T.ty lhs) in
          let free_vars = Literal.vars l in
          let skolem_decls = ref [] in
          let new_lits = CCList.map (fun (j,x) -> 
              if i!=j then x
              else (
                let lhs,rhs = 
                if Env.flex_get k_use_diff_for_neg_ext then (
                  app_diff_exhaustively lhs rhs (arg_types @ [ret_ty])
                )
                else (
                  let skolems = List.map (fun ty -> 
                      let sk, res =  T.mk_fresh_skolem free_vars ty in
                      skolem_decls := sk :: !skolem_decls;
                      res) arg_types in
                  T.app lhs skolems, T.app rhs skolems) in
                Literal.mk_neq lhs rhs)
            ) (C.lits c |> Array.mapi (fun j x -> (j,x)) |> Array.to_list) in
          declare_skolems !skolem_decls;
          let proof =
            Proof.Step.inference [C.proof_parent c] 
              ~rule:(Proof.Rule.mk "neg_ext")
              ~tags:[Proof.Tag.T_ho; Proof.Tag.T_ext; Proof.Tag.T_dont_increase_depth]
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
               | Literal.Equation (lhs, rhs, sign) when T.equal rhs T.true_->
                 let f, args = T.as_app lhs in
                 begin match T.view f with
                   | T.Var q when HVar.equal Type.equal v q ->
                     (* found an occurrence *)
                     if List.exists (T.var_occurs ~var:v) args then (
                       raise Exit; (* [P … t[v] …] is out of scope *)
                     );
                     others, (args, sign) :: set, 
                     (if sign then [lit] else []) @ pos_lits
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
            ~enum_cache:(Env.flex_get k_prim_enum_terms) 
            ~signature:(Ctx.signature ())
            ~mode ~offset (v,sc_c))
      |> Iter.map
        (fun (subst,penalty) ->
          let penalty = if Env.flex_get k_prim_enum_simpl then 0 else penalty in
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
      C.create ~penalty:1 ~trail:Trail.empty new_lits proof in


    let new_choice_op ty =
      let choice_ty_name = "#_choice_" ^ 
                           CCInt.to_string (CCRef.get_then_incr new_choice_counter) in
      let new_ch_id = ID.make choice_ty_name in
      let new_ch_const = T.const new_ch_id ~ty in
      Ctx.add_signature (Signature.declare (C.Ctx.signature ()) new_ch_id ty);
      Util.debugf 1 "new choice for type %a: %a(%a).\n" 
        (fun k -> k Type.pp ty T.pp new_ch_const Type.pp (T.ty new_ch_const));
      choice_ops := Term.Map.add new_ch_const None !choice_ops;
      new_ch_const in

    (* def_clause is the clause that defined the symbol hd *)
    let generate_instances ~def_clause hd arg =
      choice_inst_of_hd ~def_clause hd arg 
      :: choice_inst_of_hd ~def_clause hd (neg_trigger arg)
      :: (if not @@  Env.flex_get k_generalize_choice_trigger then []
          else [choice_inst_of_hd ~def_clause hd (generalize_trigger arg)]) in

    let build_choice_inst t =
      match T.view t with
      | T.App(hd, [arg]) ->
        if Term.is_var hd && inst_vars then (
          let hd_ty = Term.ty hd in
          let choice_ops = 
            Term.Map.filter (fun t _ -> Type.equal (Term.ty t) hd_ty) !choice_ops
            |> Term.Map.to_list
            |> (fun l -> if CCList.is_empty l then [new_choice_op hd_ty, None] else l) in
          CCList.flat_map (fun (hd,def_clause) -> generate_instances ~def_clause hd arg) 
            choice_ops
        ) else (
          match Term.Map.find_opt hd !choice_ops with
          | Some def_clause ->  generate_instances ~def_clause hd arg
          | None -> [])
      | _ -> assert (false) in

    C.Seq.terms c 
    |> Iter.flat_map (Term.Seq.subterms ~include_builtin:true)
    |> Iter.filter is_choice_subterm
    |> Iter.flat_map_l build_choice_inst
    |> Iter.to_list

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
      | Literal.Equation(lhs,rhs,false) when T.equal T.true_ rhs && T.is_app_var lhs ->
        begin match T.view lhs with
          | T.App(hd, [var]) when T.is_var var -> Some hd
          | _ -> None end
      | _ -> None in

    let extract_p_choice_p p l = match l with 
      | Literal.Equation(lhs,rhs,true) when T.equal T.true_ rhs ->
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
      |> Iter.fold (fun (pos_vs,neg_vs,occ) (lhs,rhs,sign,_) -> 
          if Type.is_prop (Term.ty lhs) && Term.is_app_var lhs && T.equal T.true_ rhs then (
            let var_hd = Term.as_var_exn (Term.head_term lhs) in
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
                    let proof = Some (proof_constructor ~rule ~tags [C.proof_parent c]) in
                    Some (C.apply_subst ~proof (c,0) subst))
                ) (CCList.mapi (fun i arg -> (i, arg)) args)
            ) else [] 
          ) (Term.Map.to_list occurrences)) in
    res


  let elim_leibniz_equality c =
    if C.proof_depth c < Env.flex_get k_elim_leibniz_eq then (
      elim_leibniz_eq_ c
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
      ZProf.enter_prof prof_ho_unif;
      let r = ho_unif_real_ c pairs others in
      ZProf.exit_prof prof_ho_unif;
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
    | `Reduce -> Lambda.eta_reduce ~full:true
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

  let early_bird_prim_enum cl var =
    assert(T.is_var var);
    let offset = C.Seq.vars cl |> T.Seq.max_var |> succ in
    let mode = Env.flex_get k_ho_prim_mode in
    let sc = 0 in
    
    HO_unif.enum_prop 
      ~enum_cache:(Env.flex_get k_prim_enum_terms) 
      ~signature:(Ctx.signature ()) ~mode ~offset (T.as_var_exn var,sc)
    |> CCList.map (fun (subst,p) -> 
      let renaming = Subst.Renaming.create () in
      let lits = Literals.apply_subst renaming subst (C.lits cl, sc) in
      let lits = Literals.map (fun t -> Lambda.eta_reduce @@ Lambda.snf t) lits in
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

  let setup () =
    mk_diff_const ();
    if not (Env.flex_get k_enabled) then (
      Util.debug ~section 1 "HO rules disabled";
    ) else (
      Util.debug ~section 1 "setup HO rules";
      Env.Ctx.lost_completeness();

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
      

      if(Env.flex_get k_neg_ext) then (
        Env.add_unary_inf "neg_ext" neg_ext 
      )
      else if(Env.flex_get k_neg_ext_as_simpl) then (
        Env.add_unary_simplify neg_ext_simpl;
      );

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
          | mode ->
            if Env.flex_get k_prim_enum_simpl then (
              Env.add_single_step_multi_simpl_rule (prim_enum_simpl ~mode)
            ) else Env.add_unary_inf "ho_prim_enum" (prim_enum ~mode);
        end
      ) else (
        Signal.on Env.on_pred_var_elimination (fun (cl,var) ->
          early_bird_prim_enum cl var;
          Signal.ContinueListening
        )
      );
      if Env.flex_get k_ext_axiom then
        Env.ProofState.PassiveSet.add (Iter.singleton (mk_extensionality_clause ())) ;
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
let _huet_style = ref false
let _cons_elim = ref true
let _imit_first = ref false
let _compose_subs = ref false
let _var_solve = ref false
let _instantiate_choice_ax = ref false
let _elim_leibniz_eq = ref (-1)
let _prune_arg_fun = ref `NoPrune
let _check_lambda_free = ref `False
let prim_enum_terms = ref Term.Set.empty
let _oracle_composer = ref (OSeq.merge :> (Logtk.Subst.t option OSeq.t OSeq.t -> Logtk.Subst.t option OSeq.t))
let _simple_projection = ref (-1)
let _simple_projection_md = ref 2
let _purify_applied_vars = ref `None
let _eta = ref `Reduce
let _use_diff_for_neg_ext = ref false
let _generalize_choice_trigger = ref false
let _prim_enum_simpl = ref false
let _prim_enum_early_bird = ref false
let _resolve_flex_flex = ref false
let _ground_app_vars = ref `Off

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
    E.flex_add k_instantiate_choice_ax !_instantiate_choice_ax;
    E.flex_add k_elim_leibniz_eq !_elim_leibniz_eq;
    E.flex_add k_prune_arg_fun !_prune_arg_fun;
    E.flex_add k_prim_enum_terms prim_enum_terms;
    E.flex_add k_simple_projection !_simple_projection;
    E.flex_add k_simple_projection_md !_simple_projection_md;
    E.flex_add k_check_lambda_free !_check_lambda_free;
    E.flex_add k_purify_applied_vars !_purify_applied_vars;
    E.flex_add k_eta !_eta;
    E.flex_add k_use_diff_for_neg_ext !_use_diff_for_neg_ext;
    E.flex_add k_generalize_choice_trigger !_generalize_choice_trigger;
    E.flex_add k_prim_enum_simpl !_prim_enum_simpl;
    E.flex_add k_prim_enum_early_bird !_prim_enum_early_bird;
    E.flex_add k_resolve_flex_flex !_resolve_flex_flex;
    E.flex_add k_ground_app_vars !_ground_app_vars;


    if E.flex_get k_check_lambda_free = `Only 
    then E.flex_add Saturate.k_abort_after_fragment_check true;
    
    if E.flex_get k_check_lambda_free != `False then 
      E.add_fragment_check (fun c ->
          E.C.Seq.terms c |> Iter.for_all Term.in_lfho_fragment
        );
    
    Signal.on_every E.on_start (fun () -> 
      if not !Unif._unif_bool 
      then print_endline 
        ("To remain in the chosen logic fragment, " ^
        "unification with booleans has been disabled."));

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
    |> Flex_state.add PragUnifParams.k_oracle_composer !_oracle_composer
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
      "--ho-unif", Arg.Bool (fun v -> enable_unif_ := v), " enable full HO unification";
      "--ho-elim-pred-var", Arg.Bool (fun b -> _elim_pred_var := b), " disable predicate variable elimination";
      "--ho-prim-enum", set_prim_mode_, " set HO primitive enum mode";
      "--ho-prim-max", Arg.Set_int prim_max_penalty, " max penalty for HO primitive enum";
      "--ho-prim-enum-simpl", Arg.Bool ((:=) _prim_enum_simpl), " use primitive enumeration as simplification rule";
      "--ho-prim-enum-early-bird", Arg.Bool ((:=) _prim_enum_early_bird), " use early-bird primitive enumeration (requires lazy CNF)";
      "--ho-resolve-flex-flex", Arg.Bool ((:=) _resolve_flex_flex), " eagerly remove non-essential flex-flex constraints";
      "--ho-oracle-composer", Arg.Symbol (["merge";"fair"], (fun s -> 
          if s = "merge" then _oracle_composer := (OSeq.merge :> (Logtk.Subst.t option OSeq.t OSeq.t -> Logtk.Subst.t option OSeq.t))
          else _oracle_composer := UnifFramework.take_fair)), " choose either OSeq.merge or Unif.take_fair as the composer";
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
      "--ho-use-diff-for-neg-ext", Arg.Bool ((:=) _use_diff_for_neg_ext), " use diff constant for NegExt rule instead of fresh skolem";
      "--ho-generalize-choice-trigger", Arg.Bool ((:=) _generalize_choice_trigger), " apply choice trigger to a fresh variable";
      "--check-lambda-free", Arg.Symbol (["true";"false";"only"], fun s -> match s with 
        | "true" -> _check_lambda_free := `True
        | "only" -> _check_lambda_free := `Only
        | _ -> _check_lambda_free := `False), "check whether problem belongs to lambda-free ('only' will abort after the check)";
    ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
      enabled_ := true;
      def_unfold_enabled_ := false;
      force_enabled_ := true;
      _ext_axiom := true;
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
