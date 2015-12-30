
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

module BV = CCBV
module T = FOTerm
module PF = PFormula
module O = Ordering
module S = Substs
module Lit = Literal
module Lits = Literals
module Comp = Comparison

let section = Util.Section.make ~parent:Const.section "sup"

module type S = sig
  module Env : Env.S
  module C : module type of Env.C with type t = Env.C.t
  module PS : module type of Env.ProofState with type C.t = Env.C.t

  (** {6 Term Indices} *)

  val idx_sup_into : unit -> PS.TermIndex.t
  (** index for superposition into the set *)

  val idx_sup_from : unit -> PS.TermIndex.t
  (** index for superposition from the set *)

  val idx_back_demod : unit -> PS.TermIndex.t
  (** index for backward demodulation/simplifications *)

  val idx_fv : unit -> PS.SubsumptionIndex.t
  (** index for subsumption *)

  val idx_simpl : unit -> PS.UnitIndex.t
  (** index for forward simplifications *)

  (** {6 Inference Rules} *)

  val infer_active: Env.binary_inf_rule
  (** superposition where given clause is active *)

  val infer_passive: Env.binary_inf_rule
  (** superposition where given clause is passive *)

  val infer_equality_resolution: Env.unary_inf_rule

  val infer_equality_factoring: Env.unary_inf_rule

  (** {6 Simplifications rules} *)

  val is_tautology : C.t -> bool
  (** Check whether the clause is a (syntactic) tautology, ie whether
      it contains true or "A" and "not A" *)

  val is_semantic_tautology : C.t -> bool
  (** semantic tautology deletion, using a congruence closure algorithm
      to see if negative literals imply some positive Literal.t *)

  val handle_distinct_constants : Literal.t -> Literal.t
  (** Decide on "quoted" "symbols" (which are all distinct) *)

  val basic_simplify : C.t -> C.t
  (** basic simplifications (remove duplicate literals, trivial literals,
      destructive equality resolution...) *)

  val demodulate : C.t -> C.t
  (** rewrite clause using orientable unit equations *)

  val backward_demodulate : C.CSet.t -> C.t -> C.CSet.t
  (** backward version of demodulation: add to the set active clauses that
      can potentially be rewritten by the given clause *)

  val positive_simplify_reflect : C.t -> C.t
  val negative_simplify_reflect : C.t -> C.t

  val subsumes : Literal.t array -> Literal.t array -> bool
  (** subsumes c1 c2 iff c1 subsumes c2 *)

  val subsumes_with : Literal.t array Scoped.t ->
    Literal.t array Scoped.t ->
    Substs.FO.t option
  (** returns subsuming subst if the first clause subsumes the second one *)

  val eq_subsumes : Literal.t array -> Literal.t array -> bool
  (** equality subsumption *)

  val subsumed_by_active_set : C.t -> bool
  (** check whether the clause is subsumed by any clause in the set *)

  val subsumed_in_active_set : C.t -> C.CSet.t
  (** list of clauses in the active set that are subsumed by the clause *)

  val contextual_literal_cutting : C.t -> C.t
  (** contexual Literal.t cutting of the given clause by the active set  *)

  val condensation : C.t -> C.t
  (** condensation *)

  (** {6 Registration} *)

  val register : unit -> unit
  (** Register rules in the environment *)
end

(* statistics *)
let stat_basic_simplify_calls = Util.mk_stat "basic_simplify calls"
let stat_basic_simplify = Util.mk_stat "basic_simplify"
let stat_superposition_call = Util.mk_stat "superposition calls"
let stat_equality_resolution_call = Util.mk_stat "equality_resolution calls"
let stat_equality_factoring_call = Util.mk_stat "equality_factoring calls"
let stat_subsumption_call = Util.mk_stat "subsumption calls"
let stat_eq_subsumption_call = Util.mk_stat "equality subsumption calls"
let stat_subsumed_in_active_set_call = Util.mk_stat "subsumed_in_active_set calls"
let stat_subsumed_by_active_set_call = Util.mk_stat "subsumed_by_active_set calls"
let stat_clauses_subsumed = Util.mk_stat "clauses subsumed"
let stat_demodulate_call = Util.mk_stat "demodulate calls"
let stat_demodulate_step = Util.mk_stat "demodulate steps"
let stat_semantic_tautology = Util.mk_stat "semantic_tautologies"
let stat_condensation = Util.mk_stat "condensation"
let stat_clc = Util.mk_stat "clc"

let prof_demodulate = Util.mk_profiler "demodulate"
let prof_back_demodulate = Util.mk_profiler "backward_demodulate"
let prof_pos_simplify_reflect = Util.mk_profiler "simplify_reflect+"
let prof_neg_simplify_reflect = Util.mk_profiler "simplify_reflect-"
let prof_clc = Util.mk_profiler "contextual_literal_cutting"
let prof_semantic_tautology = Util.mk_profiler "semantic_tautology"
let prof_condensation = Util.mk_profiler "condensation"
let prof_basic_simplify = Util.mk_profiler "basic_simplify"
let prof_subsumption = Util.mk_profiler "subsumption"
let prof_eq_subsumption = Util.mk_profiler "equality_subsumption"
let prof_subsumption_set = Util.mk_profiler "forward_subsumption"
let prof_subsumption_in_set = Util.mk_profiler "backward_subsumption"
let prof_infer_active = Util.mk_profiler "infer_active"
let prof_infer_passive = Util.mk_profiler "infer_passive"
let prof_infer_equality_resolution = Util.mk_profiler "infer_equality_resolution"
let prof_infer_equality_factoring = Util.mk_profiler "infer_equality_factoring"

let _enable_semantic_tauto = ref false
let _use_simultaneous_sup = ref true
let _dot_sup_into = ref None
let _dot_sup_from = ref None

module Make(Env : Env.S) : S with module Env = Env = struct
  module Env = Env
  module Ctx = Env.Ctx
  module C = Env.C
  module PS = Env.ProofState
  module I = PS.TermIndex
  module TermIndex = PS.TermIndex
  module SubsumIdx = PS.SubsumptionIndex
  module UnitIdx = PS.UnitIndex

  (** {6 Index Management} *)

  let _idx_sup_into = ref (TermIndex.empty ())
  let _idx_sup_from = ref (TermIndex.empty ())
  let _idx_back_demod = ref (TermIndex.empty ())
  let _idx_fv = ref (SubsumIdx.empty ())
  let _idx_simpl = ref (UnitIdx.empty ())

  let idx_sup_into () = !_idx_sup_into
  let idx_sup_from () = !_idx_sup_from
  let idx_back_demod () = !_idx_back_demod
  let idx_fv () = !_idx_fv
  let idx_simpl () = !_idx_simpl

  (* apply operation [f] to some parts of the clause [c] just added/removed
   * from the active set *)
  let _update_active f c =
    let ord = Ctx.ord () in
    (* index subterms that can be rewritten by superposition *)
    _idx_sup_into :=
      Lits.fold_terms ~vars:false ~ord ~which:`Max ~subterms:true
        ~eligible:(C.Eligible.res c) (C.lits c)
        |> Sequence.fold
          (fun tree (t, pos) ->
             assert (not(T.is_var t));
             let with_pos = C.WithPos.({term=t; pos; clause=c;}) in
             f tree t with_pos)
          !_idx_sup_into;
    (* index terms that can rewrite into other clauses *)
    _idx_sup_from :=
      Lits.fold_eqn ~ord ~both:true ~sign:true
        ~eligible:(C.Eligible.param c) (C.lits c)
      |> Sequence.fold
        (fun tree (l, _, sign, pos) ->
           assert sign;
           let with_pos = C.WithPos.({term=l; pos; clause=c;}) in
           f tree l with_pos)
        !_idx_sup_from ;
    (* terms that can be demodulated: all subterms (but vars) *)
    _idx_back_demod :=
      Lits.fold_terms ~vars:false ~ord ~subterms:true ~which:`All
        ~eligible:C.Eligible.always (C.lits c)
      |> Sequence.fold
        (fun tree (t, pos) ->
           let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
           f tree t with_pos)
         !_idx_back_demod;
    Signal.ContinueListening

  (* update simpl. index using the clause [c] just added or removed to
   * the simplification set *)
  let _update_simpl f c =
    let ord = Ctx.ord () in
    let idx = !_idx_simpl in
    let idx' = match C.lits c with
      | [| Lit.Equation (l,r,true) |] ->
          begin match Ordering.compare ord l r with
            | Comparison.Gt ->
                f idx (l,r,true,c)
            | Comparison.Lt ->
                f idx (r,l,true,c)
            | Comparison.Incomparable ->
                let idx = f idx (l,r,true,c) in
                f idx (r,l,true,c)
            | Comparison.Eq -> idx  (* no modif *)
          end
      | [| Lit.Equation (l,r,false) |] ->
          f idx (l,r,false,c)
      | [| Lit.Prop (p, sign) |] ->
          f idx (p,T.true_,sign,c)
      | _ -> idx
    in
    _idx_simpl := idx';
    Signal.ContinueListening

  let () =
    Signal.on PS.ActiveSet.on_add_clause
      (fun c ->
         _idx_fv := SubsumIdx.add !_idx_fv c;
         _update_active TermIndex.add c);
    Signal.on PS.ActiveSet.on_remove_clause
      (fun c ->
         _idx_fv := SubsumIdx.remove !_idx_fv c;
         _update_active TermIndex.remove c);
    Signal.on PS.SimplSet.on_add_clause
      (_update_simpl UnitIdx.add);
    Signal.on PS.SimplSet.on_remove_clause
      (_update_simpl UnitIdx.remove);
    ()

  (** {6 Inference Rules} *)

  (* all the information needed for a superposition inference *)
  module SupInfo = struct
    type t = {
      active : C.t;
      active_pos : Position.t; (* position of [s] *)
      scope_active : int;
      s : T.t; (* lhs of rule *)
      t : T.t; (* rhs of rule *)
      passive : C.t;
      passive_pos : Position.t; (* position of [u_p] *)
      passive_lit : Lit.t;
      scope_passive : int;
      u_p : T.t; (* rewritten subterm *)
      subst : S.t;
    }
  end

  exception ExitSuperposition of string

  (* Helper that does one or zero superposition inference, with all
     the given parameters. Clauses have a scope. *)
  let do_classic_superposition info acc =
    let ord = Ctx.ord () in
    let open SupInfo in
    let module P = Position in
    Util.incr_stat stat_superposition_call;
    let sc_a = info.scope_active in
    let sc_p = info.scope_passive in
    Util.debugf ~section 3
      "@[<2>sup@ @[%a[%d] s=%a t=%a@]@ @[%a[%d] passive_lit=%a p=%a@]@ with subst=@[%a@]@]"
      (fun k->k C.pp info.active sc_a T.pp info.s T.pp info.t
      C.pp info.passive sc_p Lit.pp info.passive_lit
      Position.pp info.passive_pos S.pp info.subst);
    assert (InnerTerm.DB.closed (info.s:>InnerTerm.t));
    assert (InnerTerm.DB.closed (info.u_p:T.t:>InnerTerm.t));
    let active_idx = Lits.Pos.idx info.active_pos in
    let passive_idx, passive_lit_pos = Lits.Pos.cut info.passive_pos in
    try
      let renaming = S.Renaming.create () in
      let subst = info.subst in
      let t' = S.FO.apply ~renaming subst (info.t, sc_a) in
      begin match info.passive_lit, info.passive_pos with
        | Lit.Prop (_, true), P.Arg(_, P.Left P.Stop) ->
            if T.equal t' T.true_
            then raise (ExitSuperposition "will yield a bool tautology")
        | Lit.Equation (_, v, true), P.Arg(_, P.Left P.Stop)
        | Lit.Equation (v, _, true), P.Arg(_, P.Right P.Stop) ->
            (* are we in the specific, but no that rare, case where we
               rewrite s=t using s=t (into a tautology t=t)? *)
            (* TODO: use Unif.FO.eq? *)
            let v' = S.FO.apply ~renaming subst (v, sc_p) in
            if T.equal t' v'
            then raise (ExitSuperposition "will yield a tautology");
        | _ -> ()
      end;
      let passive_lit' = Lit.apply_subst ~renaming subst (info.passive_lit, sc_p) in
      if (
        O.compare ord (S.FO.apply ~renaming info.subst (info.s, sc_a)) t' = Comp.Lt ||
        not (Lit.Pos.is_max_term ~ord passive_lit' passive_lit_pos) ||
        not (BV.get (C.eligible_res (info.passive, sc_p) subst) passive_idx) ||
        not (C.is_eligible_param (info.active, sc_a) subst ~idx:active_idx)
      ) then raise (ExitSuperposition "bad ordering conditions");
      (* ordering constraints are ok *)
      let lits_a = CCArray.except_idx (C.lits info.active) active_idx in
      let lits_p = CCArray.except_idx (C.lits info.passive) passive_idx in
      (* replace s\sigma by t\sigma in u|_p\sigma *)
      let new_passive_lit = Lit.Pos.replace passive_lit'
          ~at:passive_lit_pos ~by:t' in
      (* apply substitution to other literals *)
      let new_lits =
        new_passive_lit ::
        Lit.apply_subst_list ~renaming subst (lits_a, sc_a) @
        Lit.apply_subst_list ~renaming subst (lits_p, sc_p)
      in
      let rule = if Lit.sign passive_lit' then "sup+" else "sup-" in
      let proof c = Proof.mk_c_inference
          ~info:[S.to_string subst] ~rule
          c [C.proof info.active; C.proof info.passive] in
      let parents = [info.active; info.passive] in
      let new_clause = C.create ~parents new_lits proof in
      Util.debugf ~section 3 "@[... ok, conclusion@ @[%a@]@]" (fun k->k C.pp new_clause);
      new_clause :: acc
    with ExitSuperposition reason ->
      Util.debugf ~section 3 "... cancel, %s" (fun k->k reason);
      acc

  (* simultaneous superposition: when rewriting D with C \lor s=t,
      replace s with t everywhere in D rather than at one place. *)
  let do_simultaneous_superposition info acc =
    let ord = Ctx.ord () in
    let open SupInfo in
    let module P = Position in
    Util.incr_stat stat_superposition_call;
    let sc_a = info.scope_active in
    let sc_p = info.scope_passive in
    Util.debugf ~section 3
      "@[<hv2>simultaneous sup@ \
        @[<2>active@ %a[%d]@ s=@[%a@]@ t=@[%a@]@]@ \
        @[<2>passive@ %a[%d]@ passive_lit=@[%a@]@ p=@[%a@]@]@ with subst=@[%a@]@]"
      (fun k->k C.pp info.active sc_a T.pp info.s T.pp info.t
      C.pp info.passive sc_p Lit.pp info.passive_lit
      Position.pp info.passive_pos S.pp info.subst);
    assert (InnerTerm.DB.closed (info.s:>InnerTerm.t));
    assert (InnerTerm.DB.closed (info.u_p:T.t:>InnerTerm.t));
    assert (not(T.is_var info.u_p));
    let active_idx = Lits.Pos.idx info.active_pos in
    let passive_idx, passive_lit_pos = Lits.Pos.cut info.passive_pos in
    try
      let renaming = S.Renaming.create () in
      let subst = info.subst in
      let t' = S.FO.apply ~renaming subst (info.t, sc_a) in
      begin match info.passive_lit, info.passive_pos with
        | Lit.Prop (_, true), P.Arg(_, P.Left P.Stop) ->
            if T.equal t' T.true_
            then raise (ExitSuperposition "will yield a bool tautology")
        | Lit.Equation (_, v, true), P.Arg(_, P.Left P.Stop)
        | Lit.Equation (v, _, true), P.Arg(_, P.Right P.Stop) ->
            (* are we in the specific, but no that rare, case where we
               rewrite s=t using s=t (into a tautology t=t)? *)
            let v' = S.FO.apply ~renaming subst (v, sc_p) in
            if T.equal t' v'
            then raise (ExitSuperposition "will yield a tautology");
        | _ -> ()
      end;
      let passive_lit' = Lit.apply_subst_no_simp ~renaming subst (info.passive_lit, sc_p) in
      if (
        O.compare ord (S.FO.apply ~renaming info.subst (info.s, sc_a)) t' = Comp.Lt ||
        not (Lit.Pos.is_max_term ~ord passive_lit' passive_lit_pos) ||
        not (BV.get (C.eligible_res (info.passive, sc_p) subst) passive_idx) ||
        not (C.is_eligible_param (info.active, sc_a) subst ~idx:active_idx)
      ) then raise (ExitSuperposition "bad ordering conditions");
      (* ordering constraints are ok, build new active lits (excepted s=t) *)
      let lits_a = CCArray.except_idx (C.lits info.active) active_idx in
      let lits_a = Lit.apply_subst_list ~renaming subst (lits_a, sc_a) in
      (* build passive literals and replace u|p\sigma with t\sigma *)
      let u' = S.FO.apply ~renaming subst (info.u_p, sc_p) in
      let lits_p = Array.to_list (C.lits info.passive) in
      let lits_p = Lit.apply_subst_list ~renaming subst (lits_p, sc_p) in
      let lits_p = List.map (Lit.map (fun t-> T.replace t ~old:u' ~by:t')) lits_p in
      (* build clause *)
      let new_lits = lits_a @ lits_p in
      let rule = if Lit.sign passive_lit' then "s_sup+" else "s_sup-" in
      let proof c = Proof.mk_c_inference
          ~info:[S.to_string subst] ~rule
          c [C.proof info.active; C.proof info.passive] in
      let parents = [info.active; info.passive] in
      let new_clause = C.create ~parents new_lits proof in
      Util.debugf ~section 3 "@[... ok, conclusion@ @[%a@]@]" (fun k->k C.pp new_clause);
      new_clause :: acc
    with ExitSuperposition reason ->
      Util.debugf ~section 3 "@[... cancel, %s@]" (fun k->k reason);
      acc

  (* choose between regular and simultaneous superposition *)
  let do_superposition info acc=
    if !_use_simultaneous_sup
    then do_simultaneous_superposition info acc
    else do_classic_superposition info acc

  let infer_active clause =
    Util.enter_prof prof_infer_active;
    (* no literal can be eligible for paramodulation if some are selected.
       This checks if inferences with i-th literal are needed? *)
    let eligible = C.Eligible.param clause in
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    let new_clauses =
      Lits.fold_eqn ~sign:true ~ord:(Ctx.ord ())
        ~both:true ~eligible (C.lits clause)
      |> Sequence.fold
        (fun acc (s, t, _, s_pos) ->
           (* rewrite clauses using s *)
           I.retrieve_unifiables (!_idx_sup_into, 1) (s, 0)
           |> Sequence.fold
             (fun acc (u_p, with_pos, subst) ->
                (* rewrite u_p with s *)
                let passive = with_pos.C.WithPos.clause in
                let passive_pos = with_pos.C.WithPos.pos in
                let passive_lit, _ = Lits.Pos.lit_at (C.lits passive) passive_pos in
                let info = SupInfo.( {
                    s; t; active=clause; active_pos=s_pos; scope_active=0;
                    u_p; passive; passive_lit; passive_pos; scope_passive=1; subst;
                  }) in
                do_superposition info acc)
             acc)
        []
    in
    Util.exit_prof prof_infer_active;
    new_clauses

  let infer_passive clause =
    Util.enter_prof prof_infer_passive;
    (* perform inference on this lit? *)
    let eligible = C.Eligible.(res clause) in
    (* do the inferences in which clause is passive (rewritten),
       so we consider both negative and positive literals *)
    let new_clauses =
      Lits.fold_terms ~vars:false ~subterms:true ~ord:(Ctx.ord ())
        ~which:`All ~eligible (C.lits clause)
      |> Sequence.fold
        (fun acc (u_p, passive_pos) ->
           let passive_lit, _ = Lits.Pos.lit_at (C.lits clause) passive_pos in
           assert (not (T.is_var u_p));
           (* all terms that occur in an equation in the active_set
              and that are potentially unifiable with u_p (u at position p) *)
           I.retrieve_unifiables (!_idx_sup_from, 1) (u_p,0)
           |> Sequence.fold
             (fun acc (_, with_pos, subst) ->
                let active = with_pos.C.WithPos.clause in
                let s_pos = with_pos.C.WithPos.pos in
                match Lits.View.get_eqn (C.lits active) s_pos with
                | Some (s, t, true) ->
                    let info = SupInfo.({
                        s; t; active; active_pos=s_pos; scope_active=1; subst;
                        u_p; passive=clause; passive_lit; passive_pos; scope_passive=0;
                      }) in
                    do_superposition info acc
                | _ -> acc)
             acc)
      []
    in
    Util.exit_prof prof_infer_passive;
    new_clauses

  let infer_equality_resolution clause =
    Util.enter_prof prof_infer_equality_resolution;
    let eligible = C.Eligible.always in
    (* iterate on those literals *)
    let new_clauses =
      Lits.fold_eqn ~sign:false ~ord:(Ctx.ord ())
        ~both:false ~eligible (C.lits clause)
      |> Sequence.fold
        (fun acc (l, r, sign, l_pos) ->
           assert (not sign);
           let pos = Lits.Pos.idx l_pos in
           try
             let subst = Unif.FO.unification (l, 0) (r, 0) in
             if BV.get (C.eligible_res (clause, 0) subst) pos
             (* subst(lit) is maximal, we can do the inference *)
             then (
               Util.incr_stat stat_equality_resolution_call;
               let renaming = Ctx.renaming_clear () in
               let proof c = Proof.mk_c_inference
                   ~info:[S.to_string subst] ~rule:"eq_res" c [C.proof clause] in
               let new_lits = CCArray.except_idx (C.lits clause) pos in
               let new_lits = Lit.apply_subst_list ~renaming subst (new_lits,0) in
               let new_clause = C.create ~parents:[clause] new_lits proof in
               Util.debugf ~section 3 "@[<hv2>equality resolution on@ @[%a@]@ yields @[%a@]@]"
                 (fun k->k C.pp clause C.pp new_clause);
               new_clause::acc
             ) else
               acc
           with Unif.Fail ->
             acc  (* l and r not unifiable, try next *)
        ) []
    in
    Util.exit_prof prof_infer_equality_resolution;
    new_clauses

  module EqFactInfo = struct
    type t = {
      clause : C.t;
      active_idx : int;
      s : T.t;
      t : T.t;
      u : T.t;
      v : T.t;
      subst : S.t;
      scope : int;
    }
  end

  (* do the inference between given positions, if ordering conditions are respected *)
  let do_eq_factoring info acc =
    let open EqFactInfo in
    let ord = Ctx.ord () in
    let s = info.s and t = info.t and v = info.v in
    let subst = info.subst in
    (* check whether subst(lit) is maximal, and not (subst(s) < subst(t)) *)
    let renaming = S.Renaming.create () in
    if O.compare ord (S.FO.apply ~renaming subst (s, info.scope))
        (S.FO.apply ~renaming subst (t, info.scope)) <> Comp.Lt
       &&
       C.is_eligible_param (info.clause,info.scope) subst ~idx:info.active_idx
    then (
      Util.incr_stat stat_equality_factoring_call;
      let proof c = Proof.mk_c_inference
          ~info:[S.to_string subst] ~rule:"eq_fact" c [C.proof info.clause]
      (* new_lits: literals of the new clause. remove active literal
         and replace it by a t!=v one, and apply subst *)
      and new_lits = CCArray.except_idx (C.lits info.clause) info.active_idx in
      let new_lits = Lit.apply_subst_list ~renaming subst (new_lits,info.scope) in
      let lit' = Lit.mk_neq
          (S.FO.apply ~renaming subst (t, info.scope))
          (S.FO.apply ~renaming subst (v, info.scope))
      in
      let new_lits = lit' :: new_lits in
      let new_clause = C.create ~parents:[info.clause] new_lits proof in
      Util.debugf ~section 3 "@[<hv2>equality factoring on@ @[%a@]@ yields @[%a@]@]"
        (fun k->k C.pp info.clause C.pp new_clause);
      new_clause :: acc
    ) else
      acc

  let infer_equality_factoring clause =
    Util.enter_prof prof_infer_equality_factoring;
    let eligible = C.Eligible.(filter Lit.is_eq) in
    (* find root terms that are unifiable with s and are not in the
       literal at s_pos. Calls [k] with a position and substitution *)
    let find_unifiable_lits idx s _s_pos k =
      Array.iteri
        (fun i lit ->
           match lit with
           | _ when i = idx -> () (* same index *)
           | Lit.Prop (p, true) ->
               (* positive proposition *)
               begin try
                   let subst = Unif.FO.unification (s,0) (p,0) in
                   k (p, T.true_, subst)
                 with Unif.Fail -> ()
               end
           | Lit.Equation (u, v, true) ->
               (* positive equation *)
               begin try
                   let subst = Unif.FO.unification (s,0) (u,0) in
                   k (u, v, subst)
                 with Unif.Fail -> ()
               end;
               begin try
                   let subst = Unif.FO.unification (s,0) (v,0) in
                   k (v, u, subst)
                 with Unif.Fail -> ()
               end;
           | _ -> () (* ignore other literals *)
        ) (C.lits clause)
    in
    (* try to do inferences with each positive literal *)
    let new_clauses =
      Lits.fold_eqn ~sign:true ~ord:(Ctx.ord ())
        ~both:true ~eligible (C.lits clause)
      |> Sequence.fold
        (fun acc (s, t, _, s_pos) -> (* try with s=t *)
           let active_idx = Lits.Pos.idx s_pos in
           find_unifiable_lits active_idx s s_pos
           |> Sequence.fold
             (fun acc (u,v,subst) ->
                let info = EqFactInfo.({
                    clause; s; t; u; v; active_idx; subst; scope=0;
                  }) in
                do_eq_factoring info acc)
             acc)
        []
    in
    Util.exit_prof prof_infer_equality_factoring;
    new_clauses

  (* ----------------------------------------------------------------------
   * simplifications
   * ---------------------------------------------------------------------- *)

  exception RewriteInto of T.t * S.t

  (* TODO: put forward pointers in simpl_set, to make some rewriting steps
      faster? (invalidate when updated, also allows to reclaim memory) *)

  (* TODO: use a record with
     - head
     - args
     - subst
     so as not to rebuild intermediate terms, and also to avoid mixing
     the head normal form and the substitution for (evaluated) arguments.

     Might even convert rules into De Bruijn, because:
       - special restriction (vars rhs ⊆ vars lhs)
       - indexing on first symbol might be sufficient if matching is fast
       - must rewrite matching to work on the record anyway
    *)

  (** Compute normal form of term w.r.t active set. Clauses used to
      rewrite are added to the clauses hashset.
      restrict is an option for restricting demodulation in positive maximal terms *)
  let demod_nf ?(restrict=false) c clauses t =
    let ord = Ctx.ord () in
    (* compute normal form of subterm. If restrict is true, substitutions that
       are variable renamings are forbidden (since we are at root of a max term) *)
    let rec reduce_at_root ~restrict t =
      (* find equations l=r that match subterm *)
      try
        UnitIdx.retrieve ~sign:true (!_idx_simpl, 1) (t, 0)
        |> Sequence.iter
          (fun (l, r, (_,_,_,unit_clause), subst) ->
             (* r is the term subterm is going to be rewritten into *)
             assert (C.is_unit_clause unit_clause);
             if (not restrict || not (S.is_renaming subst))
             && C.trail_subsumes unit_clause c
             && (C.is_oriented_rule unit_clause ||
                 O.compare ord
                   (S.FO.apply_no_renaming subst (l,1))
                   (S.FO.apply_no_renaming subst (r,1)) = Comp.Gt)
             (* subst(l) > subst(r) and restriction does not apply, we can rewrite *)
             then (
               Util.debugf ~section 5
                "@[<hv2>demod:@ @[t=%a[0],@ l=%a[1],@ r=%a[1]@],@ subst=@[%a@]@]"
                 (fun k->k T.pp t T.pp l T.pp r S.pp subst);
               (* sanity check *)
               assert (
                 O.compare ord
                   (S.FO.apply_no_renaming subst (l,1))
                   (S.FO.apply_no_renaming subst (r,1)) = Comp.Gt);
               clauses := unit_clause :: !clauses;
               Util.incr_stat stat_demodulate_step;
               raise (RewriteInto (r, subst))
             ));
        t (* not found any match, normal form found *)
      with RewriteInto (t', subst) ->
        Util.debugf ~section 5 "@[<2>demod:@ rewrite @[%a@]@ into @[%a@]@]"
          (fun k->k T.pp t T.pp t');
        normal_form ~restrict subst t' 1 (* done one rewriting step, continue *)
    (* rewrite innermost-leftmost of [subst(t,scope)]. The initial scope is
       0, but then we normal_form terms in which variables are really the variables
       of the RHS of a previously applied rule (in context 1); all those
       variables are bound to terms in context 0 *)
    and normal_form ~restrict subst t scope =
      match T.view t with
      | T.App (hd, l) ->
          begin match T.view hd with
          | T.DB _
          | T.Var _ -> S.FO.apply_no_renaming subst (t, scope)
          | T.Const _ ->
              (* rewrite subterms in call by value *)
              let l' = List.map (fun t' -> normal_form ~restrict:false subst t' scope) l in
              let hd = Substs.FO.apply_no_renaming subst (hd, scope) in
              let t' = T.app hd l' in
              (* rewrite term at root *)
              reduce_at_root ~restrict t'
          | T.App _
          | T.AppBuiltin _ -> assert false
          end
      | _ -> S.FO.apply_no_renaming subst (t,scope)
    in
    normal_form ~restrict S.empty t 0

  (** Demodulate the clause, with restrictions on which terms to rewrite *)
  let demodulate c =
    Util.enter_prof prof_demodulate;
    Util.incr_stat stat_demodulate_call;
    let ord = Ctx.ord () in
    (* clauses used to rewrite *)
    let clauses = ref [] in
    (* literals that are eligible for resolution *)
    let eligible_res = lazy (C.eligible_res (c, 0) S.empty) in
    (* demodulate literals *)
    let demod_lit i lit =
      (* shall we restrict a subterm? only for max terms in positive
          equations that are eligible for resolution *)
      let restrict_term =
        if Lit.is_eq lit && BV.get (Lazy.force eligible_res) i
        then
          let max_terms = Lit.Comp.max_terms ~ord lit in
          fun t ->
            (* restrict max terms in literals eligible for resolution *)
            CCList.Set.mem ~eq:T.equal t max_terms
        else (fun _ -> false)
      in
      Lit.map
        (fun t -> demod_nf ~restrict:(restrict_term t) c clauses t)
        lit
    in
    (* demodulate every literal *)
    let lits = Array.mapi demod_lit (C.lits c) in
    if Lits.equal_com (C.lits c) lits
    then ( (* no rewriting performed *)
      Util.exit_prof prof_demodulate;
      c
    ) else (
      (* construct new clause *)
      clauses := CCList.Set.uniq ~eq:C.equal !clauses;
      let proof c' = Proof.mk_c_simp ~rule:"demod" c'
          (C.proof c :: List.map C.proof !clauses) in
      let parents = c :: C.parents c in
      let new_c = C.create_a ~parents lits proof in
      Util.debugf ~section 3 "@[<hv2>demodulate@ @[%a@]@ into @[%a@]@ using @[%a@]@]"
        (fun k->k C.pp c C.pp new_c (Util.pp_list C.pp) !clauses);
      (* return simplified clause *)
      Util.exit_prof prof_demodulate;
      new_c
    )

  (** Find clauses that [given] may demodulate, add them to set *)
  let backward_demodulate set given =
    Util.enter_prof prof_back_demodulate;
    let ord = Ctx.ord () in
    let renaming = Ctx.renaming_clear () in
    (* find clauses that might be rewritten by l -> r *)
    let recurse ~oriented set l r =
      I.retrieve_specializations (!_idx_back_demod,1) (l,0)
      |> Sequence.fold
        (fun set (_t',with_pos,subst) ->
           let c = with_pos.C.WithPos.clause in
           (* subst(l) matches t' and is > subst(r), very likely to rewrite! *)
           if ((oriented ||
                O.compare ord
                  (S.FO.apply ~renaming subst (l,0))
                  (S.FO.apply ~renaming subst (r,0)) = Comp.Gt
               ) && C.trail_subsumes c given
              )
           then  (* add the clause to the set, it may be rewritten by l -> r *)
             C.CSet.add set c
           else set)
        set
    in
    let set' = match C.lits given with
      | [|Lit.Equation (l,r,true) |] ->
          begin match Ordering.compare ord l r with
            | Comp.Gt -> recurse ~oriented:true set l r
            | Comp.Lt -> recurse ~oriented:true set r l
            | _ ->
                let set' = recurse ~oriented:false set l r in
                recurse ~oriented:false set' r l
                (* both sides can rewrite, but we need to check ordering *)
          end
      | _ -> set
    in
    Util.exit_prof prof_back_demodulate;
    set'

  let is_tautology c =
    let rec check lits i =
      if i = Array.length lits then false
      else
        let triv = match lits.(i) with
          | Lit.Prop (p, sign) ->
              CCArray.exists
                (function
                  | Lit.Prop (p', sign') when sign = not sign' ->
                      T.equal p p'  (* p  \/  ~p *)
                  | _ -> false)
                lits
          | Lit.Equation (l, r, true) when T.equal l r -> true
          | Lit.Equation (l, r, sign) ->
              CCArray.exists
                (function
                  | Lit.Equation (l', r', sign') when sign = not sign' ->
                      (T.equal l l' && T.equal r r') || (T.equal l r' && T.equal l' r)
                  | _ -> false)
                lits
          | lit -> Lit.is_trivial lit
        in
        triv || check lits (i+1)
    in
    let is_tauto = check (C.lits c) 0 || Trail.is_trivial (C.get_trail c) in
    if is_tauto then Util.debugf ~section 3 "@[@[%a@]@ is a tautology@]" (fun k->k C.pp c);
    is_tauto

  (** semantic tautology deletion, using a congruence closure algorithm
      to see if negative literals imply some positive literal *)
  let is_semantic_tautology c =
    Util.enter_prof prof_semantic_tautology;
    (* create the congruence closure of all negative equations of [c] *)
    let cc = Congruence.FO.create ~size:7 () in
    Array.iter
      (function
        | Lit.Equation (l, r, false) ->
            Congruence.FO.mk_eq cc l r
        | Lit.Prop (p, false) ->
            Congruence.FO.mk_eq cc p T.true_
        | _ -> ()
      ) (C.lits c);
    let res = CCArray.exists
        (function
          | Lit.Equation (l, r, true) ->
              (* if l=r is implied by the congruence, then the clause is redundant *)
              Congruence.FO.is_eq cc l r
          | Lit.Prop (p, true) ->
              Congruence.FO.is_eq cc p T.true_
          | _ -> false
        ) (C.lits c);
    in
    if res then (
      Util.incr_stat stat_semantic_tautology;
      Util.debugf ~section 2 "@[@[%a@]@ is a semantic tautology@]" (fun k->k C.pp c);
    );
    Util.exit_prof prof_semantic_tautology;
    res

  let flag_simplified = C.new_flag()

  let var_in_subst_ subst v sc =
    S.mem subst ((v:T.var:>InnerTerm.t HVar.t),sc)

  let basic_simplify c =
    if C.get_flag flag_simplified c
    then c
    else (
      Util.enter_prof prof_basic_simplify;
      Util.incr_stat stat_basic_simplify_calls;
      let lits = C.lits c in
      (* bv: literals to keep *)
      let bv = BV.create ~size:(Array.length lits) true in
      (* eliminate absurd lits *)
      Array.iteri (fun i lit -> if Lit.is_absurd lit then BV.reset bv i) lits;
      (* eliminate inequations x != t *)
      let subst = ref S.empty in
      let try_unif i t1 sc1 t2 sc2 =
        try
          let subst' = Unif.FO.unification ~subst:!subst (t1,sc1) (t2,sc2) in
          BV.reset bv i;
          subst := subst';
        with Unif.Fail -> ()
      in
      Array.iteri
        (fun i lit ->
           if BV.get bv i then match lit with
             | Lit.Equation (l, r, false) ->
                 begin match T.view l, T.view r with
                 | T.Var v, _ when not (var_in_subst_ !subst v 0) ->
                     (* eligible for destructive Equality Resolution, try to update
                         [subst]. Careful: in the case [X!=a | X!=b | C] we must
                         bind X only to [a] or [b], not unify [a] with [b]. *)
                     try_unif i l 0 r 0
                 | _, T.Var v when not (var_in_subst_ !subst v 0) ->
                     try_unif i r 0 l 0
                  | _ -> ()
                 end
             | _ -> ())
        lits;
      let new_lits = BV.select bv lits in
      let new_lits =
        if S.is_empty !subst then new_lits
        else
          let renaming = Ctx.renaming_clear () in
          Lit.apply_subst_list ~renaming !subst (new_lits,0)
      in
      let new_lits = CCList.Set.uniq ~eq:Lit.equal_com new_lits in
      if List.length new_lits = Array.length lits
      then (
        Util.exit_prof prof_basic_simplify;
        C.set_flag flag_simplified c true;
        c  (* no simplification *)
      ) else (
        let proof cc= Proof.mk_c_simp ~rule:"simplify" cc [C.proof c] in
        let new_clause = C.create ~parents:[c] new_lits proof in
        Util.debugf ~section 3
          "@[<2>@[%a@]@ basic_simplifies into @[%a@]@ with @[%a@]@]"
          (fun k->k C.pp c C.pp new_clause S.pp !subst);
        Util.incr_stat stat_basic_simplify;
        Util.exit_prof prof_basic_simplify;
        new_clause
      )
    )

  let is_distinct_ id =
    let s = ID.name id in
    String.length s > 2 && s.[0] = '"' && s.[String.length s-1] = '"'

  let handle_distinct_constants lit =
    match lit with
    | Lit.Equation (l, r, sign) when T.is_const l && T.is_const r ->
        let s1 = T.head_exn l and s2 = T.head_exn r in
        if is_distinct_ s1 && is_distinct_ s2
        then
          if sign = (ID.equal s1 s2)
          then Lit.mk_tauto  (* "a" = "a", or "a" != "b" *)
          else Lit.mk_absurd (* "a" = "b" or "a" != "a" *)
        else lit
    | _ -> lit

  exception FoundMatch of T.t * C.t * S.t

  let positive_simplify_reflect c =
    Util.enter_prof prof_pos_simplify_reflect;
    (* iterate through literals and try to resolve negative ones *)
    let rec iterate_lits acc lits clauses = match lits with
      | [] -> List.rev acc, clauses
      | (Lit.Equation (s, t, false) as lit)::lits' ->
          begin match equatable_terms clauses s t with
            | None -> (* keep literal *)
                iterate_lits (lit::acc) lits' clauses
            | Some new_clauses -> (* drop literal, remember clauses *)
                iterate_lits acc lits' new_clauses
          end
      | lit::lits' -> iterate_lits (lit::acc) lits' clauses
    (** try to make the terms equal using some positive unit clauses
        from active_set *)
    and equatable_terms clauses t1 t2 =
      match T.Classic.view t1, T.Classic.view t2 with
      | _ when T.equal t1 t2 -> Some clauses  (* trivial *)
      | T.Classic.App (f, ss), T.Classic.App (g, ts)
        when ID.equal f g && List.length ss = List.length ts ->
          (* try to make the terms equal directly *)
          begin match equate_root clauses t1 t2 with
            | None -> (* otherwise try to make subterms pairwise equal *)
                let ok, clauses = List.fold_left2
                    (fun (ok, clauses) t1' t2' ->
                       if ok
                       then match equatable_terms clauses t1' t2' with
                         | None -> false, []
                         | Some clauses -> true, clauses
                       else false, [])
                    (true, clauses) ss ts
                in
                if ok then Some clauses else None
            | Some clauses -> Some clauses
          end
      | _ -> equate_root clauses t1 t2 (* try to solve it with a unit equality *)
    (** try to equate terms with a positive unit clause that match them *)
    and equate_root clauses t1 t2 =
      try
        UnitIdx.retrieve ~sign:true (!_idx_simpl,1)(t1,0)
        |> Sequence.iter
            (fun (l,r,(_,_,_,c'),subst) ->
               assert (Unif.FO.equal ~subst (l,1)(t1,0));
               if Unif.FO.equal ~subst (r,1)(t2,0)
               && C.trail_subsumes c' c
               then begin
                 (* t1!=t2 is refuted by l\sigma = r\sigma *)
                 Util.debugf ~section 4
                   "@[<2>equate @[%a@]@ and @[%a@]@ using @[%a@]@]"
                   (fun k->k T.pp t1 T.pp t2 C.pp c');
                 raise (FoundMatch (r, c', subst)) (* success *)
               end
            );
        None (* no match *)
      with FoundMatch (_r, c', _subst) ->
        Some (C.proof c' :: clauses)  (* success *)
    in
    (* fold over literals *)
    let lits, premises = iterate_lits [] (C.lits c |> Array.to_list) [] in
    if List.length lits = Array.length (C.lits c)
    then (Util.exit_prof prof_pos_simplify_reflect; c) (* no literal removed, keep c *)
    else
      let proof c' = Proof.mk_c_simp
          ~rule:"simplify_reflect+" c' (C.proof c::premises) in
      let parents = c :: C.parents c in
      let new_c = C.create ~parents lits proof in
      Util.debugf ~section 3 "@[@[%a@]@ pos_simplify_reflect into @[%a@]@]"
        (fun k->k C.pp c C.pp new_c);
      Util.exit_prof prof_pos_simplify_reflect;
      new_c

  let negative_simplify_reflect c =
    Util.enter_prof prof_neg_simplify_reflect;
    (* iterate through literals and try to resolve positive ones *)
    let rec iterate_lits acc lits clauses = match lits with
      | [] -> List.rev acc, clauses
      | (Lit.Equation (s, t, true) as lit)::lits' ->
          begin match can_refute s t, can_refute t s with
            | None, None -> (* keep literal *)
                iterate_lits (lit::acc) lits' clauses
            | Some new_clause, _ | _, Some new_clause -> (* drop literal, remember clause *)
                iterate_lits acc lits' (new_clause :: clauses)
          end
      | lit::lits' -> iterate_lits (lit::acc) lits' clauses
    (** try to remove the literal using a negative unit clause *)
    and can_refute s t =
      try
        UnitIdx.retrieve ~sign:false (!_idx_simpl,1) (s,0)
        |> Sequence.iter
            (fun (l, r, (_,_,_,c'), subst) ->
               assert (Unif.FO.equal ~subst (l, 1) (s, 0));
               if Unif.FO.equal ~subst (r, 1) (t, 0)
               && C.trail_subsumes c' c
               then begin
                 (* TODO: useless? *)
                 let subst = Unif.FO.matching ~subst ~pattern:(r, 1) (t, 0) in
                 Util.debugf ~section 3 "@[neg_reflect eliminates@ @[%a=%a@]@ with @[%a@]@]"
                   (fun k->k T.pp s T.pp t C.pp c');
                 raise (FoundMatch (r, c', subst)) (* success *)
               end
            );
        None (* no match *)
      with FoundMatch (_r, c', _subst) ->
        Some (C.proof c') (* success *)
    in
    (* fold over literals *)
    let lits, premises = iterate_lits [] (C.lits c |> Array.to_list) [] in
    if List.length lits = Array.length (C.lits c)
    then (Util.exit_prof prof_neg_simplify_reflect; c) (* no literal removed *)
    else
      let proof c' = Proof.mk_c_simp ~rule:"simplify_reflect-"
          c' (C.proof c :: premises) in
      let parents = c :: C.parents c in
      let new_c = C.create ~parents lits proof in
      Util.debugf ~section 3 "@[@[%a@]@ neg_simplify_reflect into @[%a@]@]"
        (fun k->k C.pp c C.pp new_c);
      Util.exit_prof prof_neg_simplify_reflect;
      new_c

  (* ----------------------------------------------------------------------
   * subsumption
   * ---------------------------------------------------------------------- *)

  (** raised when a subsuming substitution is found *)
  exception SubsumptionFound of S.t

  (** check that every literal in a matches at least one literal in b *)
  let all_lits_match a sc_a b sc_b =
    CCArray.for_all
      (fun lita ->
         CCArray.exists
           (fun litb ->
              not (Sequence.is_empty (Lit.subsumes (lita, sc_a) (litb, sc_b))))
           b)
      a

  (** Compare literals by subsumption difficulty
      (see "towards efficient subsumption", Tammet).
      We sort by increasing order, so non-ground, deep, heavy literals are
      smaller (thus tested early) *)
  let compare_literals_subsumption lita litb =
    CCOrd.(
      (* ground literal is bigger *)
      bool_ (Lit.is_ground lita) (Lit.is_ground litb)
      (* deep literal is smaller *)
      <?> (map Lit.depth (opp int_), lita, litb)
      (* heavy literal is smaller *)
      <?> (map Lit.weight (opp int_), lita, litb)
    )

  (* replace the bitvector system by some backtracking scheme?
     XXX: maybe not a good idea. the algorithm is actually quite subtle
     and needs tight control over the traversal (lookahead of free
     variables in next literals, see [check_vars]...) *)

  (** Check whether [a] subsumes [b], and if it does, return the
      corresponding substitution *)
  let subsumes_with (a,sc_a) (b,sc_b) =
    Util.incr_stat stat_subsumption_call;
    (* a must not have more literals, and it must be possible to bind
        all its vars during subsumption *)
    if Array.length a > Array.length b
    || not (all_lits_match a sc_a b sc_b)
    then None
    else
      (* sort a copy of [a] by decreasing difficulty *)
      let a = Array.copy a in
      (* try to subsumes literals of b whose index are not in bv, with [subst] *)
      let rec try_permutations i subst bv =
        if i = Array.length a
        then raise (SubsumptionFound subst)
        else
          let lita = a.(i) in
          find_matched lita i subst bv 0
      (* find literals of b that are not bv and that are matched by lita *)
      and find_matched lita i subst bv j =
        if j = Array.length b then ()
        (* if litb is already matched, continue *)
        else if BV.get bv j then find_matched lita i subst bv (j+1)
        else (
          let litb = b.(j) in
          BV.set bv j;
          (* match lita and litb, then flag litb as used, and try with next literal of a *)
          let n_subst = ref 0 in
          Lit.subsumes ~subst (lita, sc_a) (litb, sc_b)
            (fun subst' -> incr n_subst; try_permutations (i+1) subst' bv);
          BV.reset bv j;
          (* some variable of lita occur in a[j+1...], try another literal of b *)
          if !n_subst > 0 && not (check_vars lita (i+1))
          then () (* no backtracking for litb *)
          else find_matched lita i subst bv (j+1)
        )
      (* does some literal in a[j...] contain a variable in l or r? *)
      and check_vars lit j =
        let vars = Lit.vars lit in
        if vars = []
        then false
        else
          try
            for k = j to Array.length a - 1 do
              if List.exists (fun v -> Lit.var_occurs v a.(k)) vars
              then raise Exit
            done;
            false
          with Exit -> true
      in
      try
        Array.sort compare_literals_subsumption a;
        let bv = BV.empty () in
        try_permutations 0 S.empty bv;
        None
      with (SubsumptionFound subst) ->
        Some subst

  let subsumes a b =
    Util.enter_prof prof_subsumption;
    let res = match subsumes_with (a,0) (b,1) with
      | None -> false
      | Some _ ->
          Util.debugf ~section 2 "@[<hv>@[%a@]@ subsumes @[%a@]@]"
            (fun k->k Lits.pp a Lits.pp b);
          true
    in
    Util.exit_prof prof_subsumption;
    res

  let eq_subsumes a b =
    (* counter for fresh scopes ([a] can be used several times with
       distinct scopes, e.g.  f(x)=x subsumes   g(f(a),f(b))=g(a,b) *)
    let a_scope = ref 1 in
    (* subsume a literal using a = b *)
    let rec equate_lit_with a b lit =
      match lit with
      | Lit.Equation (u, v, true) -> equate_terms a b u v
      | _ -> false
    (* make u and v equal using a = b (possibly several times) *)
    and equate_terms a b u v =
      match T.view u, T.view v with
      | _ when T.equal u v -> true
      | _ when equate_root a b u v -> true
      | T.App (f, ss), T.App (g, ts) when List.length ss = List.length ts ->
          equate_terms a b f g &&
          List.for_all2 (equate_terms a b) ss ts
      | _ -> false
    (* check whether a\sigma = u and b\sigma = v, for some sigma; or the commutation thereof *)
    and equate_root a b u v =
      let sc1 = !a_scope in
      incr a_scope;
      begin try
        let subst = Unif.FO.matching ~pattern:(a, sc1) (u, 0) in
        let _ = Unif.FO.matching ~subst ~pattern:(b, sc1) (v, 0) in
        true
       with Unif.Fail -> false
      end
      ||
      begin try
        let subst = Unif.FO.matching ~pattern:(b, sc1) (u, 0) in
        let _ = Unif.FO.matching ~subst ~pattern:(a, sc1) (v, 0) in
        true
      with Unif.Fail -> false
      end
    in
    (* check for each literal *)
    Util.enter_prof prof_eq_subsumption;
    Util.incr_stat stat_eq_subsumption_call;
    let res = match a with
      | [|Lit.Equation (s, t, true)|] ->
          let res = CCArray.exists (equate_lit_with s t) b in
          if res then Util.debugf ~section 3 "@[<2>@[%a@]@ eq-subsumes @[%a@]@]"
            (fun k->k Lits.pp a Lits.pp b);
          res
      | _ -> false  (* only a positive unit clause unit-subsumes a clause *)
    in
    Util.exit_prof prof_eq_subsumption;
    res

  let subsumed_by_active_set c =
    Util.enter_prof prof_subsumption_set;
    Util.incr_stat stat_subsumed_by_active_set_call;
    (* if there is an equation in c, try equality subsumption *)
    let try_eq_subsumption = CCArray.exists Lit.is_eqn (C.lits c) in
    (* use feature vector indexing *)
    let res =
      SubsumIdx.retrieve_subsuming_c !_idx_fv c
      |> Sequence.exists
        (fun c' ->
           C.trail_subsumes c' c
           &&
           ( (try_eq_subsumption && eq_subsumes (C.lits c') (C.lits c))
             ||
             subsumes (C.lits c') (C.lits c)
           ))
    in
    Util.exit_prof prof_subsumption_set;
    if res then (
      Util.debugf ~section 3 "@[<2>@[%a@]@ subsumed by active set@]" (fun k->k C.pp c);
      Util.incr_stat stat_clauses_subsumed;
    );
    res

  let subsumed_in_active_set c =
    Util.enter_prof prof_subsumption_in_set;
    Util.incr_stat stat_subsumed_in_active_set_call;
    (* if c is a single unit clause *)
    let try_eq_subsumption =
      C.is_unit_clause c && Lit.is_pos (C.lits c).(0)
    in
    (* use feature vector indexing *)
    let res =
      SubsumIdx.retrieve_subsumed_c !_idx_fv c
      |> Sequence.fold
        (fun res c' ->
           if C.trail_subsumes c c'
           then
             let redundant =
               (try_eq_subsumption && eq_subsumes (C.lits c) (C.lits c'))
               || subsumes (C.lits c) (C.lits c')
             in if redundant then (
               Util.incr_stat stat_clauses_subsumed;
               C.CSet.add res c'
             ) else res
           else res)
        C.CSet.empty
    in
    Util.exit_prof prof_subsumption_in_set;
    res

  (* Number of equational lits. Used as an estimation for the difficulty of the subsumption
     check for this clause. *)
  let num_equational lits =
    Array.fold_left
      (fun acc lit -> match lit with
         | Lit.Equation _ -> acc+1
         | _ -> acc
      ) 0 lits

  (* ----------------------------------------------------------------------
   * contextual literal cutting
   * ---------------------------------------------------------------------- *)

  exception RemoveLit of int * C.t

  (** Performs successive contextual literal cuttings *)
  let rec contextual_literal_cutting c =
    Util.enter_prof prof_clc;
    if Array.length (C.lits c) <= 1
    || num_equational (C.lits c) > 3
    || Array.length (C.lits c) > 8
    then (Util.exit_prof prof_clc; c) else
      (* do we need to try to use equality subsumption? *)
      let try_eq_subsumption = CCArray.exists Lit.is_eqn (C.lits c) in
      (* try to remove one literal from the literal array *)
      let remove_one_lit lits =
        try
          for i = 0 to Array.length lits - 1 do
            (* negate literal *)
            lits.(i) <- Lit.negate lits.(i);
            (* test for subsumption *)
            SubsumIdx.retrieve_subsuming !_idx_fv (Lits.Seq.to_form lits)
            |> Sequence.iter
              (fun c' ->
                 let redundant =
                   (try_eq_subsumption && eq_subsumes (C.lits c') lits)
                   || subsumes (C.lits c') lits
                 in
                 if redundant && C.trail_subsumes c' c
                 (* some clause subsumes the literals with i-th literal flipped *)
                 then (
                   lits.(i) <- Lit.negate lits.(i);
                   raise (RemoveLit (i, c'))
                 )
              );
            (* restore literal *)
            lits.(i) <- Lit.negate lits.(i);
          done;
          None (* no change *)
        with (RemoveLit (i, c')) ->
          (* remove the literal and recurse *)
          Some (CCArray.except_idx lits i, i, c')
      in
      match remove_one_lit (Array.copy (C.lits c)) with
      | None -> (Util.exit_prof prof_clc; c) (* no literal removed *)
      | Some (new_lits, i, c') ->
          (* hc' allowed us to cut a literal *)
          assert (List.length new_lits + 1 = Array.length (C.lits c));
          let info = [CCFormat.sprintf "cut lit %a" Lit.pp (C.lits c).(i)] in
          let proof c'' = Proof.mk_c_inference ~rule:"clc" ~info c'' [C.proof c; C.proof c'] in
          let parents = c :: C.parents c in
          let new_c = C.create ~parents new_lits proof in
          Util.debugf ~section 3
            "@[<2>contextual literal cutting@ in @[%a@]@ using @[%a@]@ gives @[%a@]@]"
            (fun k->k C.pp c C.pp c' C.pp new_c);
          Util.incr_stat stat_clc;
          (* try to cut another literal *)
          Util.exit_prof prof_clc;
          contextual_literal_cutting new_c

  (* ----------------------------------------------------------------------
   * contraction (condensation)
   * ---------------------------------------------------------------------- *)

  exception CondensedInto of Lit.t array * S.t

  (** performs condensation on the clause. It looks for two literals l1 and l2 of same
      sign such that l1\sigma = l2, and hc\sigma \ {l2} subsumes hc. Then
      hc is simplified into hc\sigma \ {l2}.
      If there are too many equational literals, the simplification is disabled to
      avoid pathologically expensive subsumption checks.
      TODO remove this limitation after an efficient subsumption check is implemented. *)
  let rec condensation c =
    Util.enter_prof prof_condensation;
    if Array.length (C.lits c) <= 1
    || num_equational (C.lits c) > 3
    || Array.length (C.lits c) > 8
    then (Util.exit_prof prof_condensation; c) else
      (* scope is used to rename literals for subsumption *)
      let lits = C.lits c in
      let n = Array.length lits in
      try
        for i = 0 to n - 1 do
          let lit = lits.(i) in
          for j = i+1 to n - 1 do
            let lit' = lits.(j) in
            (* see whether [lit |= lit'], and if removing [lit] gives a clause
               that subsumes c. Also try to swap [lit] and [lit']. *)
            let subst_remove_lit =
              Lit.subsumes (lit, 0) (lit', 0)
              |> Sequence.map (fun s -> s, i)
            and subst_remove_lit' =
              Lit.subsumes (lit', 0) (lit, 0)
              |> Sequence.map (fun s -> s, j)
            in
            (* potential condensing substitutions *)
            let substs = Sequence.append subst_remove_lit subst_remove_lit' in
            Sequence.iter
              (fun (subst,idx_to_remove) ->
                 let new_lits = Array.sub lits 0 (n - 1) in
                 if idx_to_remove <> n-1
                 then new_lits.(idx_to_remove) <- lits.(n-1);  (* remove lit *)
                 let renaming = Ctx.renaming_clear () in
                 let new_lits = Lits.apply_subst ~renaming subst (new_lits,0) in
                 (* check subsumption *)
                 if subsumes new_lits lits
                 then raise (CondensedInto (new_lits, subst)))
              substs
          done;
        done;
        Util.exit_prof prof_condensation;
        c
      with CondensedInto (new_lits, subst) ->
        (* clause is simplified *)
        let proof c' = Proof.mk_c_simp ~info:[S.to_string subst]
            ~rule:"condensation" c' [C.proof c] in
        let parents = c :: C.parents c in
        let new_c = C.create_a ~parents new_lits proof in
        Util.debugf ~section 3 "@[<2>condensation@ of @[%a@] (with @[%a@])@ gives @[%a@]@]"
          (fun k->k C.pp c S.pp subst C.pp new_c);
        (* try to condense further *)
        Util.exit_prof prof_condensation;
        Util.incr_stat stat_condensation;
        condensation new_c

  (** {2 Registration} *)

  (* print index into file *)
  let _print_idx file idx =
    CCIO.with_out file
      (fun oc ->
         let pp_leaf _ _ = () in
         let out = Format.formatter_of_out_channel oc in
         Format.fprintf out "@[%a@]@." (TermIndex.to_dot pp_leaf) idx;
         flush oc)

  let setup_dot_printers () =
    CCOpt.iter
      (fun f ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx f !_idx_sup_into)
      ) !_dot_sup_into;
    CCOpt.iter
      (fun f ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx f !_idx_sup_from)
      ) !_dot_sup_from;
    ()

  let register () =
    let rw_simplify c =
      let c = basic_simplify (demodulate c) in
      let c = positive_simplify_reflect c in
      let c = negative_simplify_reflect c in
      c
    and active_simplify c =
      (* condensation *)
      let c = condensation c in
      (* contextual literal cutting *)
      let c = contextual_literal_cutting c in
      c
    and backward_simplify c =
      let set = C.CSet.empty in
      backward_demodulate set c
    and redundant = subsumed_by_active_set
    and backward_redundant = subsumed_in_active_set
    and is_trivial = is_tautology in
    Env.add_binary_inf "superposition_passive" infer_passive;
    Env.add_binary_inf "superposition_active" infer_active;
    Env.add_unary_inf "equality_factoring" infer_equality_factoring;
    Env.add_unary_inf "equality_resolution" infer_equality_resolution;
    Env.add_rw_simplify rw_simplify;
    Env.add_simplify basic_simplify;
    Env.add_active_simplify active_simplify;
    Env.add_backward_simplify backward_simplify;
    Env.add_redundant redundant;
    Env.add_backward_redundant backward_redundant;
    if !_enable_semantic_tauto
    then Env.add_is_trivial is_semantic_tautology;
    Env.add_is_trivial is_trivial;
    Env.add_lit_rule "distinct_symbol" handle_distinct_constants;
    setup_dot_printers ();
    ()
end

let key = Mixtbl.access ()

let register ~sup =
  let module Sup = (val sup : S) in
  if not (Mixtbl.mem ~inj:key Sup.Env.mixtbl "superposition")
  then Mixtbl.set ~inj:key Sup.Env.mixtbl "superposition" sup

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module Sup = Make(E) in
    Sup.register();
    register ~sup:(module Sup : S)
  in
  { Extensions.default with
    Extensions.name="superposition";
    Extensions.actions = [Extensions.Do action];
  }

let () =
  Params.add_opts
    [ "--semantic-tauto"
      , Arg.Set _enable_semantic_tauto
      , " enable semantic tautology check"
    ; "--dot-sup-into"
      , Arg.String (fun s -> _dot_sup_into := Some s)
      , " print superposition-into index into file"
    ; "--dot-sup-from"
      , Arg.String (fun s -> _dot_sup_from := Some s)
      , " print superposition-from index into file"
    ; "--simultaneous-sup"
      , Arg.Bool (fun b -> _use_simultaneous_sup := b)
      , " enable/disable simultaneous superposition"
    ]

