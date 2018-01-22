
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module BV = CCBV
module T = Term

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

let _purify_applied_vars = ref `None
let _general_ext_pos = ref false
let _ext_pos = ref true
let _ext_axiom = ref false
let _elim_pred_var = ref true
let _ext_neg = ref true

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

let k_some_ho : bool Flex_state.key = Flex_state.create_key()
let k_enabled : bool Flex_state.key = Flex_state.create_key()
let k_enable_ho_unif : bool Flex_state.key = Flex_state.create_key()
let k_ho_prim_mode : _ Flex_state.key = Flex_state.create_key()
let k_ho_prim_max_penalty : int Flex_state.key = Flex_state.create_key()
let k_eta : [`Reduce | `Expand | `None] Flex_state.key = Flex_state.create_key()

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx

  (* @param vars the free variables the parameter must depend upon
     @param ty_ret the return type *)
  let mk_parameter =
    let n = ref 0 in
    fun vars ty_ret ->
      let i = CCRef.incr_then_get n in
      let id = ID.makef "#k%d" i in
      ID.set_payload id (ID.Attr_parameter i);
      let ty_vars, vars =
        List.partition (fun v -> Type.is_tType (HVar.ty v)) vars
      in
      let ty =
        Type.forall_fvars ty_vars
          (Type.arrow (List.map HVar.ty vars) ty_ret)
      in
      T.app_full (T.const id ~ty)
        (List.map Type.var ty_vars)
        (List.map T.var vars)

  (* index for ext-neg, to ensure α-equivalent negative equations have the same skolems *)
  module FV_ext_neg = FV_tree.Make(struct
      type t = Literal.t * T.t list (* lit -> skolems *)
      let compare = CCOrd.(pair Literal.compare (list T.compare))
      let to_lits (l,_) = Sequence.return (Literal.Conv.to_form l)
      let labels _ = Util.Int_set.empty
    end)

  let idx_ext_neg_ : FV_ext_neg.t ref = ref (FV_ext_neg.empty())

  (* retrieve skolems for this literal, if any *)
  let find_skolems_ (lit:Literal.t) : T.t list option =
    FV_ext_neg.retrieve_alpha_equiv_c !idx_ext_neg_ (lit, [])
    |> Sequence.find_map
      (fun (lit',skolems) ->
         let subst = Literal.variant (lit',0) (lit,1) |> Sequence.head in
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
          let l = List.map (mk_parameter vars) ty_args in
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
        let f1, l1 = T.as_app t1 in
        let f2, l2 = T.as_app t2 in
        begin match List.rev l1, List.rev l2 with
          | last1 :: l1, last2 :: l2 ->
            begin match T.view last1, T.view last2 with
              | T.Var x, T.Var y
                when HVar.equal Type.equal x y &&
                     not (Type.is_tType (HVar.ty x)) &&
                     begin
                       Sequence.of_list
                         [Sequence.doubleton f1 f2;
                          Sequence.of_list l1;
                          Sequence.of_list l2]
                       |> Sequence.flatten
                       |> Sequence.flat_map T.Seq.vars
                       |> Sequence.for_all
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

  (* More general version of ext_pos:
     e.g. C \/ f X Y = g X Y becomes C \/ f X = g X and C \/ f = g,
       if the variables X and Y occur nowhere else in the clause.
       Removes variables only in literals eligible for paramodulation,
       and only in one literal at a time.
  *)
  let ext_pos_general (c:C.t) : C.t list =
    let eligible = C.Eligible.param c in
    (* Remove recursively variables at the end of the literal t = s if possible.
       e.g. ext_pos_lit (f X Y) (g X Y) other_lits = [f X = g X, f = g]
       if X and Y do not appear in other_lits *)
    let rec ext_pos_lit t s other_lits =
      let f, tt = T.as_app t in
      let g, ss = T.as_app s in
      begin match List.rev tt, List.rev ss with
        | last_t :: tl_rev_t, last_s :: tl_rev_s ->
          if last_t = last_s && not (T.is_type last_t) then
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
      |> Sequence.of_array |> Util.seq_zipi
      |> Sequence.filter (fun (idx,lit) -> eligible idx lit)
      |> Sequence.flat_map_l
        (fun (lit_idx,lit) -> match lit with
           | Literal.Equation (t, s, true) ->
             ext_pos_lit t s (CCArray.except_idx (C.lits c) lit_idx)
             |> Sequence.of_list
             |> Sequence.flat_map_l
               (fun new_lit ->
                  (* create a clause with new_lit instead of lit *)
                  let new_lits = new_lit :: CCArray.except_idx (C.lits c) lit_idx in
                  let proof =
                    Proof.Step.inference [C.proof_parent c]
                      ~rule:(Proof.Rule.mk "ho_ext_pos_general")
                  in
                  let new_c =
                    C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c)
                  in
                  [new_c])
             |> Sequence.to_list
           | _ -> [])
      |> Sequence.to_rev_list
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
    let new_c =
      C.lits c
      |> Sequence.of_array |> Util.seq_zipi
      |> Sequence.filter (fun (idx,lit) -> eligible idx lit)
      |> Sequence.flat_map_l
        (fun (lit_idx,lit) -> match lit with
           | Literal.Equation (t, u, true) when Type.is_fun (T.ty t) ->
             let n_ty_args, ty_args, _ = Type.open_poly_fun (T.ty t) in
             assert (n_ty_args = 0);
             assert (ty_args <> []);
             let vars =
               List.mapi
                 (fun i ty -> HVar.make ~ty (i+var_offset) |> T.var)
                 ty_args
             in
             let new_lit = Literal.mk_eq (T.app t vars) (T.app u vars) in
             let new_lits = new_lit :: CCArray.except_idx (C.lits c) lit_idx in
             let proof =
               Proof.Step.inference [C.proof_parent c]
                 ~rule:(Proof.Rule.mk "ho_complete_eq") ~tags:[Proof.Tag.T_ho]
             in
             let new_c =
               C.create new_lits proof ~penalty:(C.penalty c) ~trail:(C.trail c)
             in
             [new_c]
           | _ -> [])
      |> Sequence.to_rev_list
    in
    if new_c<>[] then (
      Util.add_stat stat_complete_eq (List.length new_c);
      Util.debugf ~section 4
        "(@[complete-eq@ :clause %a@ :yields (@[<hv>%a@])@])"
        (fun k->k C.pp c (Util.pp_list ~sep:" " C.pp) new_c);
    );
    new_c

  (* try to eliminate a predicate variable in one fell swoop *)
  let elim_pred_variable (c:C.t) : C.t list =
    (* find unshielded predicate vars *)
    let find_vars(): _ HVar.t Sequence.t =
      C.Seq.vars c
      |> T.VarSet.of_seq |> T.VarSet.to_seq
      |> Sequence.filter
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
      |> Sequence.filter_map try_elim_var
      |> Sequence.to_rev_list
    end

  (* maximum penalty on clauses to perform Primitive Enum on *)
  let max_penalty_prim_ = E.flex_get k_ho_prim_max_penalty

  (* rule for primitive enumeration of predicates [P t1…tn]
     (using ¬ and ∧ and =) *)
  let prim_enum_ ~mode (c:C.t) : C.t list =
    (* set of variables to refine (only those occurring in "interesting" lits) *)
    let vars =
      Literals.fold_lits ~eligible:C.Eligible.always (C.lits c)
      |> Sequence.map fst
      |> Sequence.flat_map Literal.Seq.terms
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.filter (fun t -> Type.is_prop (T.ty t))
      |> Sequence.filter_map
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
      |> Sequence.flat_map_l
        (fun v -> HO_unif.enum_prop ~mode (v,sc_c) ~offset)
      |> Sequence.map
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
      |> Sequence.to_rev_list
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
    if (T.equal t t') then None
    else (
      Util.debugf ~section 4 "(@[beta_reduce `%a`@ :into `%a`@])"
        (fun k->k T.pp t T.pp t');
      Util.incr_stat stat_beta;
      assert (T.DB.is_closed t');
      Some (t',[])
    )

  (* rule for eta-expansion *)
  let eta_expand t =
    assert (T.DB.is_closed t);
    let t' = Lambda.eta_expand t in
    if (T.equal t t') then None
    else (
      Util.debugf ~section 4 "(@[eta_expand `%a`@ :into `%a`@])"
        (fun k->k T.pp t T.pp t');
      Util.incr_stat stat_eta_expand;
      assert (T.DB.is_closed t');
      Some (t',[])
    )

  (* rule for eta-expansion *)
  let eta_reduce t =
    assert (T.DB.is_closed t);
    let t' = Lambda.eta_reduce t in
    if (T.equal t t') then None
    else (
      Util.debugf ~section 4 "(@[eta_reduce `%a`@ :into `%a`@])"
        (fun k->k T.pp t T.pp t');
      Util.incr_stat stat_eta_reduce;
      assert (T.DB.is_closed t');
      Some (t',[])
    )

  module TVar = struct
    type t = Type.t HVar.t
    let equal = HVar.equal Type.equal
    let hash = HVar.hash
    let compare = HVar.compare Type.compare
  end
  module VarTermMultiMap = CCMultiMap.Make (TVar) (Term)
  module VTbl = CCHashtbl.Make(TVar)

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
      |> Sequence.flat_map T.Seq.vars
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
      if !_purify_applied_vars == `Ext
      then
        t1 = t2
      else (
        assert (!_purify_applied_vars == `Int);
        match T.view t1, T.view t2 with
          | T.Var x, T.Var y when x=y -> true
          | T.App (f, _), T.App (g, _) when f=g -> true
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

  let extensionality_clause =
    let diff_id = ID.make("zf_ext_diff") in
    ID.set_payload diff_id (ID.Attr_skolem (ID.K_normal, 2)); (* make the arguments of diff mandatory *)
    let alpha = Type.var (HVar.make ~ty:Type.tType 0) in
    let beta = Type.var (HVar.make ~ty:Type.tType 1) in
    let alpha_to_beta = Type.arrow [alpha] beta in
    let diff_type = Type.arrow [alpha_to_beta; alpha_to_beta] alpha in
    let diff = Term.const ~ty:diff_type diff_id in
    let x = Term.var (HVar.make ~ty:alpha_to_beta 2) in
    let y = Term.var (HVar.make ~ty:alpha_to_beta 3) in
    let x_diff = Term.app x [Term.app diff [x; y]] in
    let y_diff = Term.app y [Term.app diff [x; y]] in
    let lits = [Literal.mk_eq x y; Literal.mk_neq x_diff y_diff] in
    Env.C.create ~penalty:5 ~trail:Trail.empty lits Proof.Step.trivial

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
      if !_ext_pos && !_general_ext_pos then (
        Env.add_unary_inf "ho_ext_pos_general" ext_pos_general
      )
      else if !_ext_pos then (
        Env.add_unary_inf "ho_ext_pos" ext_pos
      );
      Env.add_rewrite_rule "beta_reduce" beta_reduce;
      begin match Env.flex_get k_eta with
        | `Expand -> Env.add_rewrite_rule "eta_expand" eta_expand
        | `Reduce -> Env.add_rewrite_rule "eta_reduce" eta_reduce
        | `None -> ()
      end;
      if Env.flex_get k_enable_ho_unif then (
        Env.add_unary_inf "ho_unif" ho_unif;
      );
      begin match Env.flex_get k_ho_prim_mode with
        | `None -> ()
        | mode ->
          Env.add_unary_inf "ho_prim_enum" (prim_enum ~mode);
      end;
      if !_purify_applied_vars != `None then
        Env.add_unary_simplify purify_applied_variable;
      if !_ext_axiom then
        Env.ProofState.PassiveSet.add (Sequence.singleton extensionality_clause);
    );
    ()
end

let enabled_ = ref true
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
    |> Sequence.flat_map T.Seq.vars
    |> Sequence.exists (fun v -> is_non_atomic_ty (HVar.ty v))
  (* is there a HO symbol? *)
  and has_ho_sym () =
    Statement.Seq.ty_decls st
    |> Sequence.exists (fun (_,ty) -> Type.order ty > 1)
  and has_ho_eq() =
    Statement.Seq.forms st
    |> Sequence.exists
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
      |> Sequence.exists st_contains_ho
    in
    if is_ho then (
      Util.debug ~section 2 "problem is HO"
    );
    state
    |> Flex_state.add k_some_ho is_ho
    |> Flex_state.add k_enabled !enabled_
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

let purify_opt =
  let set_ n = _purify_applied_vars := n in
  let l = [ "ext", `Ext; "int", `Int; "none", `None] in
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
      "--ho-purify", purify_opt, " enable purification of applied variables: 'ext' purifies" ^
        " whenever a variable is applied to different arguments." ^
        " 'int' purifies whenever a variable appears applied and unapplied.";
      "--ho-general-ext-pos", Arg.Set _general_ext_pos, " enable general positive extensionality rule";
      "--ho-ext-axiom", Arg.Set _ext_axiom, " enable extensionality axiom";
      "--ho-no-ext-pos", Arg.Clear _ext_pos, " disable positive extensionality rule";
      "--ho-no-ext-neg", Arg.Clear _ext_neg, " disable negative extensionality rule"
    ];
  Extensions.register extension;
