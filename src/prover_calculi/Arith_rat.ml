
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Cancellative Inferences} *)

open Logtk
open Libzipperposition

module T = Term
module Lit = Literal
module Lits = Literals
module S = Subst
module M = Monome
module MF = Monome.Focus
module AL = Rat_lit
module ALF = AL.Focus
module Stmt = Statement
module US = Unif_subst

let stat_rat_sup = Util.mk_stat "rat.superposition"
let stat_rat_cancellation = Util.mk_stat "rat.rat_cancellation"
let stat_rat_eq_factoring = Util.mk_stat "rat.eq_factoring"
let stat_rat_ineq_chaining = Util.mk_stat "rat.ineq_chaining"
let stat_rat_semantic_tautology = Util.mk_stat "rat.semantic_tauto"
let stat_rat_ineq_factoring = Util.mk_stat "rat.ineq_factoring"
let stat_rat_demod = Util.mk_stat "rat.demod"
let stat_rat_backward_demod = Util.mk_stat "rat.backward_demod"
let stat_rat_trivial_ineq = Util.mk_stat "rat.redundant_by_ineq.calls"
let stat_rat_trivial_ineq_steps = Util.mk_stat "rat.redundant_by_ineq.steps"
let stat_rat_demod_ineq = Util.mk_stat "rat.demod_ineq.calls"
let stat_rat_demod_ineq_steps = Util.mk_stat "rat.demod_ineq.steps"
(*
let stat_rat_reflexivity_resolution = Util.mk_stat "rat.reflexivity_resolution"
*)

let prof_rat_sup = Util.mk_profiler "rat.superposition"
let prof_rat_cancellation = Util.mk_profiler "rat.rat_cancellation"
let prof_rat_eq_factoring = Util.mk_profiler "rat.eq_factoring"
let prof_rat_ineq_chaining = Util.mk_profiler "rat.ineq_chaining"
let prof_rat_demod = Util.mk_profiler "rat.demod"
let prof_rat_backward_demod = Util.mk_profiler "rat.backward_demod"
let prof_rat_semantic_tautology = Util.mk_profiler "rat.semantic_tauto"
let prof_rat_ineq_factoring = Util.mk_profiler "rat.ineq_factoring"
let prof_rat_trivial_ineq = Util.mk_profiler "rat.redundant_by_ineq"
let prof_rat_demod_ineq = Util.mk_profiler "rat.demod_ineq"
(*
let prof_rat_reflexivity_resolution = Util.mk_profiler "rat.reflexivity_resolution"
*)

let section = Util.Section.make ~parent:Const.section "rat-arith"

let enable_rat_ = ref true
let enable_ac_ = ref false
let enable_semantic_tauto_ = ref true
let dot_unit_ = ref None

let flag_tauto = SClause.new_flag ()
let flag_computed_tauto = SClause.new_flag ()

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  (** {2 Contributions to Env} *)

  val register : unit -> unit
end

let enable_trivial_ineq_ = ref true
let enable_demod_ineq_ = ref true

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module Ctx = Env.Ctx
  module C = Env.C
  module PS = Env.ProofState

  let _idx_eq = ref (PS.TermIndex.empty ())
  let _idx_ineq_left = ref (PS.TermIndex.empty ())
  let _idx_ineq_right = ref (PS.TermIndex.empty ())
  let _idx_all = ref (PS.TermIndex.empty ())

  (* unit clauses *)
  let _idx_unit_eq = ref (PS.TermIndex.empty ())
  let _idx_unit_ineq = ref (PS.TermIndex.empty ())

  (* apply [f] to some subterms of [c] *)
  let update f c =
    let ord = Ctx.ord () in
    _idx_eq :=
      Lits.fold_terms ~vars:false ~ty_args:false ~which:`Max ~ord ~subterms:false
        ~eligible:C.Eligible.(filter Lit.is_rat_eq ** max c)
        (C.lits c)
      |> Sequence.fold
        (fun acc (t,pos) ->
           let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
           f acc t with_pos)
        !_idx_eq;
    let left, right =
      Lits.fold_terms ~vars:false ~ty_args:false ~which:`Max ~ord ~subterms:false
        ~eligible:C.Eligible.(filter Lit.is_rat_less ** max c)
        (C.lits c)
      |> Sequence.fold
        (fun (left,right) (t,pos) ->
           let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
           match pos with
             | Position.Arg (_, Position.Left _) ->
               f left t with_pos, right
             | Position.Arg (_, Position.Right _) ->
               left, f right t with_pos
             | _ -> assert false)
        (!_idx_ineq_left, !_idx_ineq_right)
    in
    _idx_ineq_left := left;
    _idx_ineq_right := right;
    _idx_all :=
      Lits.fold_terms ~vars:false ~ty_args:false ~which:`Max ~ord ~subterms:false
        ~eligible:C.Eligible.(filter Lit.is_rat ** max c)
        (C.lits c)
      |> Sequence.fold
        (fun acc (t,pos) ->
           let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
           f acc t with_pos)
        !_idx_all;
    ()

  (* simplification set *)
  let update_simpl f c =
    let ord = Ctx.ord () in
    begin match C.lits c with
      | [| Lit.Rat ({AL.op=AL.Equal; _} as alit) |] ->
        let pos = Position.(arg 0 stop) in
        _idx_unit_eq :=
          AL.fold_terms ~subterms:false ~vars:false ~pos ~which:`Max ~ord alit
          |> Sequence.fold
            (fun acc (t,pos) ->
               assert (not (T.is_var t));
               let with_pos = C.WithPos.( {term=t; pos; clause=c;} ) in
               f acc t with_pos)
            !_idx_unit_eq
      | [| Lit.Rat ({AL.op=AL.Less; _} as alit) |] ->
        let pos = Position.(arg 0 stop) in
        _idx_unit_ineq :=
          if !enable_trivial_ineq_ || !enable_demod_ineq_
          then AL.fold_terms ~subterms:false ~vars:false ~pos ~which:`Max ~ord alit
               |> Sequence.fold
                 (fun acc (t,pos) ->
                    assert (not (T.is_var t));
                    let with_pos = C.WithPos.( {term=t; pos; clause=c;} ) in
                    f acc t with_pos)
                 !_idx_unit_ineq
          else !_idx_unit_ineq
      | _ -> ()
    end;
    ()

  let () =
    Signal.on PS.ActiveSet.on_add_clause
      (fun c ->
         if !enable_rat_ then update PS.TermIndex.add c;
         Signal.ContinueListening);
    Signal.on PS.SimplSet.on_add_clause
      (fun c ->
         if !enable_rat_ then update_simpl PS.TermIndex.add c;
         Signal.ContinueListening);
    Signal.on PS.ActiveSet.on_remove_clause
      (fun c ->
         if !enable_rat_ then update PS.TermIndex.remove c;
         Signal.ContinueListening);
    Signal.on PS.SimplSet.on_remove_clause
      (fun c ->
         if !enable_rat_ then update_simpl PS.TermIndex.remove c;
         Signal.ContinueListening);
    ()

  (** {2 Utils} *)


  (* data required for superposition *)
  module SupInfo = struct
    type t = {
      active : C.t;
      active_pos : Position.t;
      active_lit : AL.Focus.t;
      active_scope : int;
      passive : C.t;
      passive_pos : Position.t;
      passive_lit : AL.Focus.t;
      passive_scope : int;
      subst : Unif_subst.t;
    }
  end

  let rule_canc = Proof.Rule.mk "canc_sup"

  (* do cancellative superposition *)
  let _do_canc info acc =
    let open SupInfo in
    let ord = Ctx.ord () in
    let renaming = Subst.Renaming.create () in
    let us = info.subst in
    let idx_a, _ = Lits.Pos.cut info.active_pos in
    let idx_p, _ = Lits.Pos.cut info.passive_pos in
    let s_a = info.active_scope and s_p = info.passive_scope in
    let subst = Unif_subst.subst us in
    let lit_a = ALF.apply_subst renaming subst (info.active_lit,s_a) in
    let lit_p = ALF.apply_subst renaming subst (info.passive_lit,s_p) in
    Util.debugf ~section 5
      "@[<2>arith superposition@ between @[%a[%d]@]@ and @[%a[%d]@]@ (subst @[%a@])...@]"
      (fun k->k C.pp info.active s_a C.pp info.passive s_p Subst.pp subst);
    (* check ordering conditions *)
    if C.is_maxlit (info.active,s_a) subst ~idx:idx_a
    && C.is_maxlit (info.passive,s_p) subst ~idx:idx_p
    && ALF.is_max ~ord lit_a
    (* && ALF.is_max ~ord lit_p *)
    then (
      (* the active literals *)
      let lit_a, lit_p = ALF.scale lit_a lit_p in
      (* other literals *)
      let lits_a = CCArray.except_idx (C.lits info.active) idx_a in
      let lits_a = Lit.apply_subst_list renaming subst (lits_a,s_a) in
      let lits_p = CCArray.except_idx (C.lits info.passive) idx_p in
      let lits_p = Lit.apply_subst_list renaming subst (lits_p,s_p) in
      (* new literal: lit_a=[t+m1=m2], lit_p=[t'+m1' R m2'] for some
         relation R. Now let's replace t' by [m2-m1] in lit', ie,
         build m = [m1'-m2'+(m2-m1) R 0]. *)
      let mf_a, m_a = match lit_a with
        | ALF.Left (AL.Equal, mf, m)
        | ALF.Right (AL.Equal, m, mf) -> mf, m
        | _ -> assert false
      in
      let new_lit = match lit_p with
        | ALF.Left (op, mf_p, m_p) ->
          Lit.mk_rat_op op
            (M.sum (MF.rest mf_p) m_a)
            (M.sum m_p (MF.rest mf_a))
        | ALF.Right (op, m_p, mf_p) ->
          Lit.mk_rat_op op
            (M.sum m_p (MF.rest mf_a))
            (M.sum (MF.rest mf_p) m_a)
      in
      let c_guard = Literal.of_unif_subst renaming us in
      let all_lits = new_lit :: c_guard @ lits_a @ lits_p in
      (* build clause *)
      let proof =
        Proof.Step.inference ~tags:[Proof.Tag.T_lra]
          ~rule:rule_canc
          [C.proof_parent_subst renaming (info.active,s_a) subst;
           C.proof_parent_subst renaming (info.passive,s_p) subst] in
      let trail = C.trail_l [info.active;info.passive] in
      let penalty = C.penalty info.active + C.penalty info.passive in
      let new_c = C.create ~penalty ~trail all_lits proof in
      Util.debugf ~section 5 "@[<2>... gives@ @[%a@]@]" (fun k->k C.pp new_c);
      Util.incr_stat stat_rat_sup;
      new_c :: acc
    ) else (
      Util.debug ~section 5 "... has bad ordering conditions";
      acc
    )

  let canc_sup_active c =
    Util.enter_prof prof_rat_sup;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(pos ** max c ** filter Lit.is_rat_eq) in
    let sc_a = 0 and sc_p = 1 in
    let res =
      Lits.fold_rat_terms ~eligible ~which:`Max ~ord (C.lits c)
      |> Sequence.fold
        (fun acc (t,active_lit,active_pos) ->
           assert (ALF.op active_lit = AL.Equal);
           Util.debugf ~section 5 "@[<2>active canc. sup.@ with @[%a@]@ in @[%a@]@]"
             (fun k->k ALF.pp active_lit C.pp c);
           PS.TermIndex.retrieve_unifiables (!_idx_all,sc_p) (t,sc_a)
           |> Sequence.fold
             (fun acc (t',with_pos,subst) ->
                let passive = with_pos.C.WithPos.clause in
                let passive_pos = with_pos.C.WithPos.pos in
                let passive_lit = Lits.View.get_rat_exn (C.lits passive) passive_pos in
                Util.debugf ~section 5 "@[<4>  possible match:@ @[%a@]@ in @[%a@]@]"
                  (fun k->k ALF.pp passive_lit C.pp passive);
                (* now to unify active_lit and passive_lit further *)
                if T.is_var t || T.is_var t'
                then acc
                else
                  ALF.unify ~subst (active_lit,sc_a) (passive_lit,sc_p)
                  |> Sequence.fold
                    (fun acc (active_lit, passive_lit, subst) ->
                       let info = SupInfo.({
                           active=c; active_pos; active_lit; active_scope=sc_a;
                           passive; passive_pos; passive_lit; passive_scope=sc_p; subst;
                         }) in
                       _do_canc info acc)
                    acc)
             acc)
        []
    in
    Util.exit_prof prof_rat_sup;
    res

  let canc_sup_passive c =
    Util.enter_prof prof_rat_sup;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(max c ** arith) in
    let sc_a = 0 and sc_p = 1 in
    let res =
      Lits.fold_rat_terms ~eligible ~which:`All ~ord (C.lits c)
      |> Sequence.fold
        (fun acc (t,passive_lit,passive_pos) ->
           Util.debugf ~section 5 "@[<2>passive canc. sup.@ with @[%a@]@ in @[%a@]@]"
             (fun k->k ALF.pp passive_lit C.pp c);
           PS.TermIndex.retrieve_unifiables (!_idx_eq,sc_a) (t,sc_p)
           |> Sequence.fold
             (fun acc (t',with_pos,subst) ->
                let active = with_pos.C.WithPos.clause in
                let active_pos = with_pos.C.WithPos.pos in
                let active_lit = Lits.View.get_rat_exn (C.lits active) active_pos in
                (* must have an equation as active lit *)
                match ALF.op active_lit with
                  | AL.Equal when not (T.is_var t) && not (T.is_var t') ->
                    Util.debugf ~section 5 "@[<4>  possible match:@ @[%a@]@ in @[%a@]@]"
                      (fun k->k ALF.pp passive_lit C.pp c);
                    (* unify literals further *)
                    ALF.unify ~subst (active_lit,sc_a) (passive_lit,sc_p)
                    |> Sequence.fold
                      (fun acc (active_lit, passive_lit, subst) ->
                         let info = SupInfo.({
                             active; active_pos; active_lit; active_scope=sc_a;
                             passive=c; passive_pos; passive_lit; passive_scope=sc_p; subst;
                           }) in
                         _do_canc info acc
                      ) acc
                  | _ -> acc)
             acc)
        []
    in
    Util.exit_prof prof_rat_sup;
    res

  exception SimplifyInto of AL.t * C.t * S.t

  (* how to simplify the passive lit with the active lit, in one step *)
  let _try_demod_step ~subst passive_lit _s_p c pos active_lit s_a c' _pos' =
    let ord = Ctx.ord () in
    let i = Lits.Pos.idx pos in
    let renaming = S.Renaming.create () in
    let active_lit' = ALF.apply_subst renaming subst (active_lit,s_a) in
    (* restrictions:
       - the rewriting term must be bigger than other terms
        (in other words, the inference is strictly decreasing)
       - all variables of active clause must be bound by subst
       - must not rewrite itself (c != c')
       - trail(active) must subsume trail(passive) *)
    if ALF.is_strictly_max ~ord active_lit'
    && (C.Seq.vars c'
        |> Sequence.for_all
          (fun v -> S.mem subst ((v:Type.t HVar.t:>InnerTerm.t HVar.t),s_a)))
    && ( (C.lits c |> Array.length) > 1
         || not(Lit.equal (C.lits c).(i) (C.lits c').(0))
         || not(ALF.is_max ~ord passive_lit && C.is_maxlit (c,0) S.empty ~idx:i)
    )
    && (C.trail_subsumes c' c) then (
      (* we know all variables of [active_lit] are bound, no need
         for a renaming *)
      let active_lit = ALF.apply_subst Subst.Renaming.none subst (active_lit,s_a) in
      let active_lit, passive_lit = ALF.scale active_lit passive_lit in
      match active_lit, passive_lit with
        | ALF.Left (AL.Equal, mf1, m1), _
        | ALF.Right (AL.Equal, m1, mf1), _ ->
          let new_lit =
            ALF.replace passive_lit (M.difference m1 (MF.rest mf1))
          in
          raise (SimplifyInto (new_lit, c',subst))
        | _ -> ()
    ) else ()

  (* reduce an arithmetic literal to its current normal form *)
  let rec _demod_lit_nf ~add_lit ~add_premise ~i c a_lit =
    let ord = Ctx.ord () in
    let s_a = 1 and s_p = 0 in  (* scopes *)
    begin
      try
        AL.fold_terms ~pos:Position.stop ~vars:false ~which:`Max
          ~ord ~subterms:false a_lit
        |> Sequence.iter
          (fun (t,lit_pos) ->
             assert (not (T.is_var t));
             let passive_lit = ALF.get_exn a_lit lit_pos in
             (* search for generalizations of [t] *)
             PS.TermIndex.retrieve_generalizations (!_idx_unit_eq,s_a) (t,s_p)
             |> Sequence.iter
               (fun (_t',with_pos,subst) ->
                  let c' = with_pos.C.WithPos.clause in
                  let pos' = with_pos.C.WithPos.pos in
                  assert (C.is_unit_clause c');
                  assert (Lits.Pos.idx pos' = 0);
                  let active_lit = Lits.View.get_rat_exn (C.lits c') pos' in
                  let pos = Position.(arg i lit_pos) in
                  _try_demod_step ~subst passive_lit s_p c pos active_lit s_a c' pos'));
        (* could not simplify, keep the literal *)
        add_lit (Lit.mk_rat a_lit)
      with SimplifyInto (a_lit',c',subst) ->
        (* lit ----> lit' *)
        add_premise c' subst;
        (* recurse until the literal isn't reducible *)
        Util.debugf ~section 4
          "(@[<hv2>rewrite_rat_arith@ :lit `%a`@ :into `%a`@ \
           :using @[%a@]@ :subst @[%a@]@])"
          (fun k->k AL.pp a_lit AL.pp a_lit' C.pp c' S.pp subst);
        _demod_lit_nf ~add_premise ~add_lit ~i c a_lit'
    end

  let eq_c_subst (c1,s1)(c2,s2) = C.equal c1 c2 && Subst.equal s1 s2

  (* demodulation (simplification) *)
  let _demodulation c =
    Util.enter_prof prof_rat_demod;
    let did_simplify = ref false in
    let lits = ref [] in  (* simplified literals *)
    let add_lit l = lits := l :: !lits in
    let clauses = ref [] in  (* simplifying clauses *)
    (* add a rewriting clause *)
    let add_premise c' subst =
      did_simplify := true;
      clauses := (c',subst) :: !clauses
    in
    (* simplify each and every literal *)
    Lits.fold_lits ~eligible:C.Eligible.always (C.lits c)
    |> Sequence.iter
      (fun (lit,i) ->
         match lit with
           | Lit.Rat a_lit ->
             _demod_lit_nf ~add_lit ~add_premise ~i c a_lit
           | _ ->
             add_lit lit (* keep non-arith literals *)
      );
    (* build result clause (if it was simplified) *)
    let res =
      if !did_simplify then (
        clauses := CCList.uniq ~eq:eq_c_subst !clauses;
        let proof =
          Proof.Step.inference ~tags:[Proof.Tag.T_lra]
            ~rule:(Proof.Rule.mk "canc_demod")
            (C.proof_parent c ::
               List.rev_map
                 (fun (c,subst) -> C.proof_parent_subst Subst.Renaming.none (c,1) subst)
                 !clauses)
        in
        let trail = C.trail c in
        let new_c = C.create ~penalty:(C.penalty c) ~trail (List.rev !lits) proof in
        Util.incr_stat stat_rat_demod;
        Util.debugf ~section 5 "@[<2>arith demodulation@ of @[%a@]@ with [@[%a@]]@ gives @[%a@]@]"
          (fun k->
             let pp_c_s out (c,s) =
               Format.fprintf out "(@[%a@ :subst %a@])" C.pp c Subst.pp s
             in
             k C.pp c (Util.pp_list pp_c_s) !clauses C.pp new_c);
        SimplM.return_new new_c
      ) else
        SimplM.return_same c
    in
    Util.exit_prof prof_rat_demod;
    res

  let canc_demodulation c = _demodulation c

  (* find clauses in which some literal could be rewritten by [c], iff
     [c] is a positive unit arith clause *)
  let canc_backward_demodulation c =
    Util.enter_prof prof_rat_backward_demod;
    let ord = Ctx.ord () in
    let res = C.ClauseSet.empty in
    let res = match C.lits c with
      | [| Lit.Rat ({AL.op=AL.Equal; _} as alit) |] ->
        AL.fold_terms ~vars:false ~which:`Max ~subterms:false ~ord alit
        |> Sequence.fold
          (fun acc (t,pos) ->
             PS.TermIndex.retrieve_specializations (!_idx_all,0) (t,1)
             |> Sequence.fold
               (fun acc (_t',with_pos,subst) ->
                  let c' = with_pos.C.WithPos.clause in
                  (* check whether the term [t] is indeed maximal in
                     its literal (and clause) after substitution *)
                  let alit' = ALF.get_exn alit pos in
                  let alit' = ALF.apply_subst Subst.Renaming.none subst (alit',1) in
                  if C.trail_subsumes c' c && ALF.is_max ~ord alit'
                  then (
                    Util.incr_stat stat_rat_backward_demod;
                    C.ClauseSet.add c' acc
                  ) else acc)
               acc)
          C.ClauseSet.empty
      | _ -> res (* no demod *)
    in
    Util.exit_prof prof_rat_backward_demod;
    res

  let cancellation c =
    Util.enter_prof prof_rat_cancellation;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(max c ** arith) in
    let res =
      Lits.fold_rat ~eligible (C.lits c)
      |> Sequence.fold
        (fun acc (a_lit,pos) ->
           let idx = Lits.Pos.idx pos in
           (* cancellation depends on what the literal looks like *)
           let {AL.left=m1; right=m2; op} = a_lit in
           Util.debugf ~section 5 "@[<2>try cancellation@ in @[%a@]@]" (fun k->k AL.pp a_lit);
           (* try to unify terms in [m1] and [m2] *)
           MF.unify_mm (m1,0) (m2,0)
           |> Sequence.fold
             (fun acc (mf1, mf2, us) ->
                let renaming = Subst.Renaming.create () in
                let subst = US.subst us in
                let mf1' = MF.apply_subst renaming subst (mf1,0) in
                let mf2' = MF.apply_subst renaming subst (mf2,0) in
                let is_max_lit = C.is_maxlit (c,0) subst ~idx in
                Util.debugf ~section 5
                  "@[<4>... candidate:@ @[%a@] (max lit ? %B)@]"
                  (fun k->k S.pp subst is_max_lit);
                if is_max_lit && MF.is_max ~ord mf1' && MF.is_max ~ord mf2'
                then (
                  (* do the inference *)
                  let lits' = CCArray.except_idx (C.lits c) idx in
                  let lits' = Lit.apply_subst_list renaming subst (lits',0) in
                  let new_lit = Lit.mk_rat_op op (MF.rest mf1') (MF.rest mf2') in
                  let c_guard = Literal.of_unif_subst renaming us in
                  let all_lits = new_lit :: c_guard @ lits' in
                  let proof =
                    Proof.Step.inference ~tags:[Proof.Tag.T_lra]
                      ~rule:(Proof.Rule.mk "cancellation")
                      [C.proof_parent_subst renaming (c,0) subst] in
                  let trail = C.trail c in
                  let penalty = C.penalty c in
                  let new_c = C.create ~trail ~penalty all_lits proof in
                  Util.debugf ~section 3
                    "@[<2>cancellation@ of @[%a@]@ (with %a)@ into @[%a@]@]"
                    (fun k->k C.pp c Subst.pp subst C.pp new_c);
                  Util.incr_stat stat_rat_cancellation;
                  new_c :: acc
                ) else
                  acc
             ) acc)
        []
    in
    Util.exit_prof prof_rat_cancellation;
    res

  let rule_canc_eq_fact = Proof.Rule.mk "rat_eq_factoring"

  let canc_equality_factoring c =
    Util.enter_prof prof_rat_eq_factoring;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(max c ** filter Lit.is_rat_eq) in
    let res =
      Lits.fold_rat_terms ~which:`Max ~eligible ~ord (C.lits c)
      |> Sequence.fold
        (fun acc (t1,lit1,pos1) ->
           assert(ALF.op lit1 = AL.Equal);
           let idx1 = Lits.Pos.idx pos1 in
           (* lit1 is the factored literal *)
           Lits.fold_rat_terms ~which:`Max ~ord
             ~eligible:C.Eligible.(filter Lit.is_rat_eq) (C.lits c)
           |> Sequence.fold
             (fun acc (t2,lit2,pos2) ->
                assert(ALF.op lit2 = AL.Equal);
                let idx2 = Lits.Pos.idx pos2 in
                let mf1 = ALF.focused_monome lit1
                and mf2 = ALF.focused_monome lit2 in
                try
                  if idx1 = idx2 then raise Unif.Fail;  (* exit *)
                  let subst = Unif.FO.unify_full (t1,0) (t2,0) in
                  Util.debugf ~section 5
                    "@[<2>arith canc. eq. factoring:@ possible match in @[%a@]@ (at %d, %d)@]"
                    (fun k->k C.pp c idx1 idx2);
                  MF.unify_ff ~subst (mf1,0) (mf2,0)
                  |> Sequence.fold
                    (fun acc (_, _, us) ->
                       let renaming = Subst.Renaming.create () in
                       let subst = US.subst us in
                       let lit1' = ALF.apply_subst renaming subst (lit1,0) in
                       let lit2' = ALF.apply_subst renaming subst (lit2,0) in
                       if C.is_maxlit (c,0) subst ~idx:idx1
                       && ALF.is_max ~ord lit1'
                       && ALF.is_max ~ord lit2'
                       then (
                         (*  lit1 is   a.t + l1 = l2,
                             lit2 is   a'.t + l1' = l2',
                             so we scale them, and replace lit1 with
                             a'.l1 + a.l2' != a'.l2 + a.l1' *)
                         let lit1', lit2' = ALF.scale lit1' lit2' in
                         let m1 = ALF.opposite_monome lit1'
                         and mf1 = ALF.focused_monome lit1'
                         and m2 = ALF.opposite_monome lit2'
                         and mf2 = ALF.focused_monome lit2' in
                         (* m1 != m2, translated as two ineqs *)
                         let new_lits =
                           let x = M.sum m1 (MF.rest mf2) in
                           let y = M.sum m2 (MF.rest mf1) in
                           [Lit.mk_rat_less x y; Lit.mk_rat_less y x]
                         in
                         let other_lits = CCArray.except_idx (C.lits c) idx1 in
                         let other_lits =
                           Lit.apply_subst_list
                             renaming subst (other_lits,0) in
                         let c_guard = Literal.of_unif_subst renaming us in
                         (* apply subst and build clause *)
                         let all_lits = c_guard @ new_lits @ other_lits in
                         let proof =
                           Proof.Step.inference ~tags:[Proof.Tag.T_lra]
                             ~rule:rule_canc_eq_fact
                             [C.proof_parent_subst renaming (c,0) subst] in
                         let penalty = C.penalty c
                         and trail = C.trail c in
                         let new_c = C.create ~trail ~penalty all_lits proof in
                         Util.debugf ~section 5
                           "@[<2>arith_eq_factoring:@ @[%a@]@ gives @[%a@]@]"
                           (fun k->k C.pp c C.pp new_c);
                         Util.incr_stat stat_rat_eq_factoring;
                         new_c :: acc
                       ) else acc)
                    acc
                with Unif.Fail ->
                  acc)
             acc)
        []
    in Util.exit_prof prof_rat_eq_factoring;
    res

  (** Data necessary to fully describe a chaining inference.
      [left] is basically the clause/literal in which the chained
      term is on the left of <,
      [right] is the other one. *)
  module ChainingInfo = struct
    type t = {
      left : C.t;
      left_scope : int;
      left_pos : Position.t;
      left_lit : AL.Focus.t;
      right : C.t;
      right_scope : int;
      right_pos : Position.t;
      right_lit : AL.Focus.t;
      subst : US.t;
    }
  end

  (* range from low to low+len *)
  (* cancellative chaining *)
  let _do_chaining info acc =
    let open ChainingInfo in
    let ord = Ctx.ord () in
    let renaming = S.Renaming.create () in
    let us = info.subst in
    let idx_l, _ = Lits.Pos.cut info.left_pos in
    let idx_r, _ = Lits.Pos.cut info.right_pos in
    let s_l = info.left_scope and s_r = info.right_scope in
    let subst = Unif_subst.subst us in
    let lit_l = ALF.apply_subst renaming subst (info.left_lit,s_l) in
    let lit_r = ALF.apply_subst renaming subst (info.right_lit,s_r) in
    Util.debugf ~section 5
      "@[<2>arith chaining@ between @[%a[%d]@]@ and @[%a[%d]@]@ (subst @[%a@])...@]"
      (fun k->k C.pp info.left s_l C.pp info.right s_r Subst.pp subst);
    (* check ordering conditions *)
    if C.is_maxlit (info.left,s_l) subst ~idx:idx_l
    && C.is_maxlit (info.right,s_r) subst ~idx:idx_r
    && ALF.is_max ~ord lit_l
    && ALF.is_max ~ord lit_r
    then (
      (* scale literals *)
      let lit_l, lit_r = ALF.scale lit_l lit_r in
      match lit_l, lit_r with
        | ALF.Left (AL.Less, mf_1, m1), ALF.Right (AL.Less, m2, mf_2) ->
          (* m2 ≤ mf_2 and mf_1 ≤ m1, with mf_1 and mf_2 sharing the same
             focused term. We deduce m2 + mf_1 ≤ m1 + mf_2 and cancel the
             term out (after scaling) *)
          assert (Q.equal (MF.coeff mf_1) (MF.coeff mf_2));
          let new_lit = Lit.mk_rat_less
              (M.sum m2 (MF.rest mf_1))
              (M.sum m1 (MF.rest mf_2))
          in
          let lits_l = CCArray.except_idx (C.lits info.left) idx_l in
          let lits_l = Lit.apply_subst_list renaming subst (lits_l,s_l) in
          let lits_r = CCArray.except_idx (C.lits info.right) idx_r in
          let lits_r = Lit.apply_subst_list renaming subst (lits_r,s_r) in
          let c_guard = Literal.of_unif_subst renaming us in
          let all_lits = new_lit :: c_guard @ lits_l @ lits_r in
          let proof =
            Proof.Step.inference ~tags:[Proof.Tag.T_lra]
              ~rule:(Proof.Rule.mk "canc_ineq_chaining")
              [C.proof_parent_subst renaming (info.left,s_l) subst;
               C.proof_parent_subst renaming (info.right,s_r) subst] in
          let trail = C.trail_l [info.left; info.right] in
          (* penalty for some chaining *)
          let penalty =
            C.penalty info.left
            + C.penalty info.right
            + 3 (* nested chainings are dangerous *)
            + (if MF.term mf_1 |> T.is_var then 10 else 0)
            + (if MF.term mf_2 |> T.is_var then 10 else 0)
          in
          let new_c = C.create ~penalty ~trail all_lits proof in
          Util.debugf ~section 5 "@[<2>ineq chaining@ of @[%a@]@ and @[%a@]@ gives @[%a@]@]"
            (fun k->k C.pp info.left C.pp info.right C.pp new_c);
          Util.incr_stat stat_rat_ineq_chaining;
          new_c :: acc
        | _ -> assert false
    ) else
      acc

  let canc_ineq_chaining c =
    Util.enter_prof prof_rat_ineq_chaining;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(max c ** filter Lit.is_rat_less) in
    let sc_l = 0 and sc_r = 1 in
    let res =
      Lits.fold_rat_terms ~eligible ~ord ~which:`Max (C.lits c)
      |> Sequence.fold
        (fun acc (t,lit,pos) ->
           match lit with
             | _ when T.is_var t -> acc (* ignore variables *)
             | ALF.Left (AL.Less, mf_l, _) ->
               (* find a right-chaining literal in some other clause *)
               PS.TermIndex.retrieve_unifiables (!_idx_ineq_right,sc_r) (t,sc_l)
               |> Sequence.fold
                 (fun acc (_t',with_pos,subst) ->
                    let right = with_pos.C.WithPos.clause in
                    let right_pos = with_pos.C.WithPos.pos in
                    let lit_r = Lits.View.get_rat_exn (C.lits right) right_pos in
                    match lit_r with
                      | ALF.Right (AL.Less, _, mf_r) ->
                        MF.unify_ff ~subst (mf_l,sc_l) (mf_r,sc_r)
                        |> Sequence.fold
                          (fun acc (_, _, subst) ->
                             let info = ChainingInfo.({
                                 left=c; left_scope=sc_l; left_lit=lit; left_pos=pos;
                                 right; right_scope=sc_r; right_lit=lit_r; right_pos; subst;
                               })
                             in _do_chaining info acc)
                          acc
                      | _ -> acc)
                 acc
             | ALF.Right (AL.Less, _, mf_r) ->
               (* find a right-chaining literal in some other clause *)
               PS.TermIndex.retrieve_unifiables (!_idx_ineq_left,sc_l) (t,sc_r)
               |> Sequence.fold
                 (fun acc (_t',with_pos,subst) ->
                    let left = with_pos.C.WithPos.clause in
                    let left_pos = with_pos.C.WithPos.pos in
                    let lit_l = Lits.View.get_rat_exn (C.lits left) left_pos in
                    match lit_l with
                      | ALF.Left (AL.Less, mf_l, _) ->
                        MF.unify_ff ~subst (mf_l,sc_l) (mf_r,sc_r)
                        |> Sequence.fold
                          (fun acc (_, _, subst) ->
                             let info = ChainingInfo.({
                                 left; left_scope=sc_l; left_lit=lit_l; left_pos; subst;
                                 right=c; right_scope=sc_r; right_lit=lit; right_pos=pos;
                               })
                             in _do_chaining info acc)
                          acc
                      | _ -> acc)
                 acc
             | _ -> assert false)
        []
    in
    Util.exit_prof prof_rat_ineq_chaining;
    res

  (* TODO: update with equality case, check that signs correct *)
  let canc_ineq_factoring c =
    Util.enter_prof prof_rat_ineq_factoring;
    let ord = Ctx.ord () in
    let acc = ref [] in
    (* do the factoring if ordering conditions are ok *)
    let _do_factoring ~subst:us lit1 lit2 i j =
      let renaming = S.Renaming.create () in
      let subst = US.subst us in
      let lit1 = ALF.apply_subst renaming subst (lit1,0) in
      let lit2 = ALF.apply_subst renaming subst (lit2,0) in
      (* same coefficient for the focused term *)
      let lit1, lit2 = ALF.scale lit1 lit2 in
      match lit1, lit2 with
        | ALF.Left (AL.Less, mf1, m1), ALF.Left (AL.Less, mf2, m2)
        | ALF.Right (AL.Less, m1, mf1), ALF.Right (AL.Less, m2, mf2) ->
          (* mf1 ≤ m1  or  mf2 ≤ m2  (symmetry with > if needed)
             so we deduce that if  m1-mf1.rest ≤ m2 - mf2.rest
             then the first literal implies the second, so we only
             keep the second one *)
          if (C.is_maxlit (c,0) subst ~idx:i || C.is_maxlit (c,0) subst ~idx:j)
          && (ALF.is_max ~ord lit1 || ALF.is_max ~ord lit2)
          then (
            let left = match lit1 with ALF.Left _ -> true | _ -> false in
            (* remove lit1, add the guard *)
            let other_lits = CCArray.except_idx (C.lits c) i in
            let other_lits = Lit.apply_subst_list renaming subst (other_lits,0) in
            (* build new literal *)
            let new_lit =
              if left
              then
                Lit.mk_rat_less
                  (M.difference m1 (MF.rest mf1))
                  (M.difference m2 (MF.rest mf2))
              else
                Lit.mk_rat_less
                  (M.difference m2 (MF.rest mf2))
                  (M.difference m1 (MF.rest mf1))
            in
            (* negate the literal to obtain a guard *)
            let new_lit = Lit.negate new_lit in
            let c_guard = Literal.of_unif_subst renaming us in
            let lits = new_lit :: c_guard @ other_lits in
            (* build clauses *)
            let proof =
              Proof.Step.inference ~tags:[Proof.Tag.T_lra]
                ~rule:(Proof.Rule.mk "canc_ineq_factoring")
                [C.proof_parent_subst renaming (c,0) subst] in
            let trail = C.trail c
            and penalty = C.penalty c in
            let new_c = C.create ~trail ~penalty lits proof in
            Util.debugf ~section 5 "@[<2>ineq factoring@ of @[%a@]@ gives @[%a@]@"
              (fun k->k C.pp c C.pp new_c);
            Util.incr_stat stat_rat_ineq_factoring;
            acc := new_c :: !acc
          )
        | _ -> ()
    in
    (* traverse the clause to find matching pairs *)
    let eligible = C.Eligible.(max c ** filter Lit.is_rat_less) in
    Lits.fold_rat ~eligible (C.lits c)
    |> Sequence.iter
      (fun (lit1,pos1) ->
         let i = Lits.Pos.idx pos1 in
         let eligible' = C.Eligible.(filter Lit.is_rat_less) in
         Lits.fold_rat ~eligible:eligible' (C.lits c)
         |> Sequence.iter
           (fun (lit2,pos2) ->
              let j = Lits.Pos.idx pos2 in
              match lit1, lit2 with
                | _ when i=j -> ()  (* need distinct lits *)
                | {AL.op=AL.Less; left=l1; right=r1},
                  {AL.op=AL.Less; left=l2; right=r2} ->
                  (* see whether we have   l1 < a.x + mf1  and  l2 < a.x + mf2 *)
                  MF.unify_mm (r1,0) (r2,0)
                    (fun (mf1,mf2,subst) ->
                       let lit1 = ALF.mk_right AL.Less l1 mf1 in
                       let lit2 = ALF.mk_right AL.Less l2 mf2 in
                       _do_factoring ~subst lit1 lit2 i j
                    );
                  (* see whether we have   a.x + mf1 < r1 and a.x + mf2 < r2  *)
                  MF.unify_mm (l1,0) (l2,0)
                    (fun (mf1,mf2,subst) ->
                       let lit1 = ALF.mk_left AL.Less mf1 r1 in
                       let lit2 = ALF.mk_left AL.Less mf2 r2 in
                       _do_factoring ~subst lit1 lit2 i j
                    );
                  ()
                | _ -> assert false
           )
      );
    Util.exit_prof prof_rat_ineq_factoring;
    !acc

  (** One-shot literal/clause removal.
      We use unit clauses to try to prove a literal absurd/tautological, possibly
      using {b several} instances of unit clauses.

      For instance, 0 ≤ f(x)  makes  0 ≤ f(a) + f(b) redundant, but subsumption
      is not able to detect it. *)

  (* allow traces of depth at most 3 *)
  let max_ineq_trivial_steps = 3

  (* rewrite a literal [l] into a smaller literal [l'], such that [l'] and
     the current set of unit clauses imply [l]; then compute the
     transitive closure of this relation. If we obtain a trivial
     literal, then [l] is redundant (we keep a trace of literals used).
     We use continuations to deal with the multiple choices. *)
  let rec _ineq_find_sufficient ~ord ~trace c lit k = match lit with
    | _ when AL.is_trivial lit -> k (trace,lit)
    | _ when List.length trace >= max_ineq_trivial_steps ->
      () (* need another step, but it would exceed the limit *)
    | {AL.op=AL.Less; _} when Sequence.exists T.is_var (AL.Seq.terms lit) ->
      ()  (* no way we rewrite this into a tautology *)
    | {AL.op=AL.Less; _} ->
      Util.incr_stat stat_rat_trivial_ineq;
      Util.debugf ~section 5
        "(@[try_ineq_find_sufficient@ :lit `%a`@ :trace (@[%a@])@])"
        (fun k->k AL.pp lit (Util.pp_list C.pp) trace);
      AL.fold_terms ~vars:false ~which:`Max ~ord ~subterms:false lit
      |> Sequence.iter
        (fun (t,pos) ->
           let plit = ALF.get_exn lit pos in
           let is_left = match pos with
             | Position.Left _ -> true
             | Position.Right _ -> false
             | _ -> assert false
           in
           (* try to eliminate [t] in passive lit [plit]*)
           PS.TermIndex.retrieve_generalizations (!_idx_unit_ineq,1) (t,0)
           |> Sequence.iter
             (fun (_t',with_pos,subst) ->
                let active_clause = with_pos.C.WithPos.clause in
                let active_pos = with_pos.C.WithPos.pos in
                match Lits.View.get_rat (C.lits active_clause) active_pos with
                  | None -> assert false
                  | Some (ALF.Left (AL.Less, _, _) as alit') when is_left ->
                    let alit' = ALF.apply_subst Subst.Renaming.none subst (alit',1) in
                    if C.trail_subsumes active_clause c
                    && ALF.is_strictly_max ~ord alit'
                    then (
                      (* scale *)
                      let plit, _alit' = ALF.scale plit alit' in
                      let mf1', m2' =
                        match Lits.View.get_rat (C.lits active_clause) active_pos with
                          | Some (ALF.Left (_, mf1', m2')) -> mf1', m2'
                          | _ -> assert false
                      in
                      let mf1' = MF.apply_subst Subst.Renaming.none subst (mf1',1) in
                      let m2' = M.apply_subst Subst.Renaming.none subst (m2',1) in
                      (* from t+mf1 ≤ m2  and t+mf1' ≤ m2', we deduce
                         that if m2'-mf1' ≤ m2-mf1  then [lit] is redundant.
                         That is, the sufficient literal is
                         mf1 + m2' ≤ m2 + mf1'  (we replace [t] with [m2'-mf1']) *)
                      let new_plit =
                        ALF.replace plit (M.difference m2' (MF.rest mf1')) in
                      (* transitive closure *)
                      let trace = active_clause::trace in
                      _ineq_find_sufficient ~ord ~trace c new_plit k
                    )
                  | Some (ALF.Right (AL.Less, _, _) as alit') when not is_left ->
                    (* symmetric case *)
                    let alit' = ALF.apply_subst Subst.Renaming.none subst (alit',1) in
                    if C.trail_subsumes active_clause c
                    && ALF.is_strictly_max ~ord alit'
                    then (
                      (* scale *)
                      let plit, _alit' = ALF.scale plit alit' in
                      let m1', mf2' =
                        match Lits.View.get_rat (C.lits active_clause) active_pos with
                          | Some (ALF.Right (_, m1', mf2')) -> m1', mf2'
                          | _ -> assert false
                      in
                      let mf2' = MF.apply_subst Subst.Renaming.none subst (mf2',1) in
                      let m1' = M.apply_subst Subst.Renaming.none subst (m1',1) in
                      let new_plit =
                        ALF.replace plit (M.difference m1' (MF.rest mf2')) in
                      (* transitive closure *)
                      let trace = active_clause::trace in
                      _ineq_find_sufficient ~ord ~trace c new_plit k
                    )
                  | Some _ ->
                    ()   (* cannot make a sufficient literal *)
             )
        )
    | _ -> ()

  (* is a literal redundant w.r.t the current set of unit clauses *)
  let _ineq_is_redundant_by_unit c lit =
    match lit with
      | _ when Lit.is_trivial lit || Lit.is_absurd lit ->
        None  (* something more efficient will take care of it *)
      | Lit.Rat ({AL.op=AL.Less; _} as alit) ->
        let ord = Ctx.ord () in
        let traces =
          _ineq_find_sufficient ~ord ~trace:[] c alit
          |> Sequence.head  (* one is enough *)
        in
        begin match traces with
          | Some (trace, _lit') ->
            assert (AL.is_trivial _lit');
            let trace = CCList.uniq ~eq:C.equal trace in
            Some trace
          | None -> None
        end
      | _ -> None

  let is_redundant_by_ineq c =
    Util.enter_prof prof_rat_trivial_ineq;
    let res =
      CCArray.exists
        (fun lit -> match _ineq_is_redundant_by_unit c lit with
           | None -> false
           | Some trace ->
             Util.debugf ~section 3
               "@[<2>clause @[%a@]@ trivial by inequations @[%a@]@]"
               (fun k->k C.pp c (CCFormat.list C.pp) trace);
             Util.incr_stat stat_rat_trivial_ineq_steps;
             true)
        (C.lits c)
    in
    Util.exit_prof prof_rat_trivial_ineq;
    res

  (* allow traces of depth at most 3 *)
  let max_ineq_demod_steps = 3

  (* rewrite a literal [l] into a smaller literal [l'], such that [l] and
     the current set of unit clauses imply [l']; then compute the
     transitive closure of this relation. If we obtain an absurd
     literal, then [l] is absurd (we keep a trace of literals used).
     We use continuations to deal with the multiple choices.

     Each step looks like: from [l == (t <= u) && l' == (l <= t)]
     we deduce [l <= u]. If at some point we deduce [⊥], we win.  *)
  let rec ineq_find_necessary_ ~ord ~trace c lit k = match lit with
    | _ when AL.is_absurd lit -> k (trace,lit)
    | _ when List.length trace >= max_ineq_demod_steps ->
      () (* need another step, but it would exceed the limit *)
    | {AL.op=AL.Less; _} when Sequence.exists T.is_var (AL.Seq.terms lit) ->
      ()  (* too costly (will match too many things) *)
    | {AL.op=AL.Less; _} ->
      Util.incr_stat stat_rat_demod_ineq;
      Util.debugf ~section 5
        "(@[try_ineq_find_necessary@ :lit `%a`@ :trace (@[%a@])@])"
        (fun k->k AL.pp lit (Util.pp_list C.pp) trace);
      AL.fold_terms ~vars:false ~which:`Max ~ord ~subterms:false lit
      |> Sequence.iter
        (fun (t,pos) ->
           let plit = ALF.get_exn lit pos in
           let is_left = match pos with
             | Position.Left _ -> true
             | Position.Right _ -> false
             | _ -> assert false
           in
           (* try to eliminate [t] in passive lit [plit]*)
           PS.TermIndex.retrieve_generalizations (!_idx_unit_ineq,1) (t,0)
           |> Sequence.iter
             (fun (_t',with_pos,subst) ->
                let active_clause = with_pos.C.WithPos.clause in
                let active_pos = with_pos.C.WithPos.pos in
                match Lits.View.get_rat (C.lits active_clause) active_pos with
                  | None -> assert false
                  | Some (ALF.Left (AL.Less, _, _) as alit') when not is_left ->
                    let alit' = ALF.apply_subst Subst.Renaming.none subst (alit',1) in
                    if C.trail_subsumes active_clause c
                    && ALF.is_strictly_max ~ord alit'
                    then (
                      (* scale *)
                      let plit, _alit' = ALF.scale plit alit' in
                      let mf1', m2' =
                        match Lits.View.get_rat (C.lits active_clause) active_pos with
                          | Some (ALF.Left (_, mf1', m2')) -> mf1', m2'
                          | _ -> assert false
                      in
                      let mf1' = MF.apply_subst Subst.Renaming.none subst (mf1',1) in
                      let m2' = M.apply_subst Subst.Renaming.none subst (m2',1) in
                      (* from m1 ≤ t+mf2  and t+mf1' ≤ m2', we deduce
                         m1 + mf1' ≤ mf2 + m2'. If this literal is absurd
                         then so is [m1 ≤ t+mf2].
                         We replace [t] with [m2'-mf1'] *)
                      let new_plit =
                        ALF.replace plit (M.difference m2' (MF.rest mf1')) in
                      (* transitive closure *)
                      let trace = active_clause::trace in
                      ineq_find_necessary_ ~ord ~trace c new_plit k
                    )
                  | Some (ALF.Right (AL.Less, _, _) as alit') when is_left ->
                    (* symmetric case *)
                    let alit' = ALF.apply_subst Subst.Renaming.none subst (alit',1) in
                    if C.trail_subsumes active_clause c
                    && ALF.is_strictly_max ~ord alit'
                    then (
                      (* scale *)
                      let plit, _alit' = ALF.scale plit alit' in
                      let m1', mf2' =
                        match Lits.View.get_rat (C.lits active_clause) active_pos with
                          | Some (ALF.Right (_, m1', mf2')) -> m1', mf2'
                          | _ -> assert false
                      in
                      let mf2' = MF.apply_subst Subst.Renaming.none subst (mf2',1) in
                      let m1' = M.apply_subst Subst.Renaming.none subst (m1',1) in
                      let new_plit =
                        ALF.replace plit (M.difference m1' (MF.rest mf2')) in
                      (* transitive closure *)
                      let trace = active_clause::trace in
                      ineq_find_necessary_ ~ord ~trace c new_plit k
                    )
                  | Some _ ->
                    ()   (* cannot make a sufficient literal *)
             )
        )
    | _ -> ()

  (* is a literal absurd w.r.t the current set of unit clauses *)
  let _ineq_is_absurd_by_unit c lit =
    match lit with
      | _ when Lit.is_trivial lit || Lit.is_absurd lit ->
        None  (* something more efficient will take care of it *)
      | Lit.Rat ({AL.op=AL.Less; _} as alit) ->
        let ord = Ctx.ord () in
        let traces =
          ineq_find_necessary_ ~ord ~trace:[] c alit
          |> Sequence.head  (* one is enough *)
        in
        begin match traces with
          | Some (trace, _lit') ->
            assert (AL.is_absurd _lit');
            let trace = CCList.uniq ~eq:C.equal trace in
            Some trace
          | None -> None
        end
      | _ -> None

  (* demodulate using inequalities *)
  let demod_ineq c : C.t SimplM.t =
    Util.enter_prof prof_rat_demod_ineq;
    let res =
      CCArray.findi
        (fun i lit -> match _ineq_is_absurd_by_unit c lit with
           | None -> None
           | Some trace ->
             Util.debugf ~section 3
               "@[<2>clause @[%a@]@ rewritten by inequations @[%a@]@]"
               (fun k->k C.pp c (CCFormat.list C.pp) trace);
             Util.incr_stat stat_rat_demod_ineq_steps;
             Some (i,trace))
        (C.lits c)
    in
    let res = match res with
      | None -> SimplM.return_same c
      | Some (i,cs) ->
        let lits = CCArray.except_idx (C.lits c) i in
        let proof = Proof.Step.simp ~tags:[Proof.Tag.T_lra]
            ~rule:(Proof.Rule.mk "rat.demod_ineq")
            (C.proof_parent c :: List.map C.proof_parent cs)
        in
        let c' = C.create lits proof ~penalty:(C.penalty c) ~trail:(C.trail c) in
        SimplM.return_new c'
    in
    Util.exit_prof prof_rat_demod_ineq;
    res

  (** {3 Others} *)

  let _has_rat c = CCArray.exists Lit.is_rat (C.lits c)

  module Simp = Simplex.MakeHelp(T)

  (* tautology check: take the linear system that is the negation
     of all a≠b and a≤b, and check its satisfiability *)
  let _is_tautology c =
    Util.enter_prof prof_rat_semantic_tautology;
    (* convert a monome into a rational monome + Q constant *)
    let conv m = M.coeffs m, M.const m in
    (* create a list of constraints for some arith lits *)
    let constraints =
      Lits.fold_rat ~eligible:C.Eligible.arith (C.lits c)
      |> Sequence.fold
        (fun acc (lit,_) ->
           (* negate the literal and make a constraint out of it *)
           match lit with
             | {AL.op=AL.Less; left=m1; right=m2} ->
               (* m1 < m2 ----> m1-m2 > 0 ---> m1-m2 ≥ 0 by approx *)
               let m, c = conv (M.difference m1 m2) in
               (Simp.GreaterEq, m, Q.neg c) :: acc
             | _ -> acc)
        []
    in
    let simplex = Simp.add_constraints Simp.empty constraints in
    Util.exit_prof prof_rat_semantic_tautology;
    match Simp.ksolve simplex with
      | Simp.Unsatisfiable _ -> true (* negation unsatisfiable *)
      | Simp.Solution _ -> false

  (* cache the result because it's a bit expensive *)
  let is_tautology c =
    if C.get_flag flag_computed_tauto c
    then C.get_flag flag_tauto c
    else (
      (* compute whether [c] is an arith tautology *)
      let res = _has_rat c && _is_tautology c in
      C.set_flag flag_tauto c res;
      C.set_flag flag_computed_tauto c true;
      if res then
        Util.debugf ~section 4
          "@[<2>clause@ @[%a@]@ is an arith tautology@]" (fun k->k C.pp c);
      Util.incr_stat stat_rat_semantic_tautology;
      res
    )

  (* look for negated literals *)
  let convert_lit c: C.t SimplM.t =
    let type_ok t = Type.equal Type.rat (T.ty t) in
    let open CCOpt.Infix in
    let conv_lit i lit = match lit with
      | Lit.Equation (l, r, false) when type_ok l ->
        Monome.Rat.of_term l >>= fun m1 ->
        Monome.Rat.of_term r >|= fun m2 ->
        i, [Lit.mk_rat_less m1 m2; Lit.mk_rat_less m2 m1]
      | Lit.Prop (f,sign) ->
        begin match T.view f, sign with
          | T.AppBuiltin (Builtin.Less, [_; l; r]), false when type_ok l ->
            Monome.Rat.of_term l >>= fun m1 ->
            Monome.Rat.of_term r >|= fun m2 ->
            (* ¬(l<r) --> l=r ∨ r<l *)
            i, [Lit.mk_rat_eq m1 m2; Lit.mk_rat_less m2 m1]
          | T.AppBuiltin (Builtin.Less, [_; l; r]), true when type_ok l ->
            Monome.Rat.of_term l >>= fun m1 ->
            Monome.Rat.of_term r >|= fun m2 ->
            i, [Lit.mk_rat_less m1 m2]
          | T.AppBuiltin (Builtin.Lesseq, [_; l; r]), true when type_ok l ->
            Monome.Rat.of_term l >>= fun m1 ->
            Monome.Rat.of_term r >|= fun m2 ->
            (* l≤r --> l=r ∨ l<r *)
            i, [Lit.mk_rat_eq m1 m2; Lit.mk_rat_less m1 m2]
          | T.AppBuiltin (Builtin.Lesseq, [_; l; r]), false when type_ok l ->
            Monome.Rat.of_term l >>= fun m1 ->
            Monome.Rat.of_term r >|= fun m2 ->
            (* ¬(l≤r) --> r<l *)
            i, [Lit.mk_rat_less m2 m1]
          | T.AppBuiltin (Builtin.Eq, [_; l; r]), true when type_ok l ->
            Monome.Rat.of_term l >>= fun m1 ->
            Monome.Rat.of_term r >|= fun m2 ->
            i, [Lit.mk_rat_eq m1 m2]
          | T.AppBuiltin (Builtin.Eq, [_; l; r]), false when type_ok l ->
            Monome.Rat.of_term l >>= fun m1 ->
            Monome.Rat.of_term r >|= fun m2 ->
            (* ¬(l=r) --> l<r ∨ r<l *)
            i, [Lit.mk_rat_less m1 m2; Lit.mk_rat_less m2 m1]
          | _ -> None
        end
      | _ -> None
    in
    begin match CCArray.findi conv_lit (C.lits c) with
      | None -> SimplM.return_same c
      | Some (i, new_lits) ->
        let lits =
          new_lits @ CCArray.except_idx (C.lits c) i
        and proof =
          Proof.Step.simp ~tags:[Proof.Tag.T_lra] ~rule:(Proof.Rule.mk "convert_lit")
            [C.proof_parent c]
        in
        let c' =
          C.create ~trail:(C.trail c) ~penalty:(C.penalty c) lits proof
        in
        Util.debugf ~section 5 "(@[convert@ :from %a@ :to %a@])"
          (fun k->k C.pp c C.pp c');
        SimplM.return_new c'
    end

  (** {6 Variable Elimination Procedure} *)

  let unshielded_vars lits =
    Literals.unshielded_vars lits
      ~filter:(fun var -> Type.equal (HVar.ty var) Type.rat)

  let _negate_lits = List.map Lit.negate

  module Elim_var = struct
    type t = {
      var: T.var;
      lower: Q.t M.t list; (* monomes smaller than x *)
      upper: Q.t M.t list; (* monomes bigger than x *)
      eq: Q.t M.t list; (* monomes equal to x *)
      side_lits: Literal.t list; (* other literals *)
      initial_lits: Literals.t;
    }

    let pp out (e:t) =
      let pp_m = CCFormat.within "`" "`" M.pp in
      let pp_m_l out l =
        Format.fprintf out "(@[<hv>%a@])" (Util.pp_list ~sep:" " pp_m) l
      in
      Format.fprintf out
        "(@[<v>:var %a@ :lower %a@ :upper %a@ :eq %a@ :side (@[%a@])@])"
        T.pp_var e.var
        pp_m_l e.lower
        pp_m_l e.upper
        pp_m_l e.eq
        (Util.pp_list ~sep:" " Literal.pp) e.side_lits

    (* quick and dirty CNF *)
    module Form = struct
      type form =
        | Atom of Literal.t
        | And of form list
        | Or of form list
        | True
        | False

      let atom l = Atom l
      let or_ = function [] -> False | [t] -> t | l -> Or l
      let and_ = function [] -> True | [t] -> t | l -> And l

      let rec cnf (f:form): Lit.t list list = match f with
        | Atom lit -> [[lit]]
        | True -> [[Lit.mk_tauto]]
        | False -> [[Lit.mk_absurd]]
        | And [] | Or [] -> assert false
        | And [f] | Or [f] -> cnf f
        | And l -> CCList.flat_map cnf l
        | Or l -> Util.map_product ~f:cnf l

      let rec pp out (f:form): unit = match f with
        | True -> CCFormat.string out "⊤"
        | False -> CCFormat.string out "⊥"
        | Atom lit -> Lit.pp out lit
        | Or l -> Format.fprintf out "(@[<hv>or@ %a@])" (Util.pp_list ~sep:" " pp) l
        | And l -> Format.fprintf out "(@[<hv>and@ %a@])" (Util.pp_list ~sep:" " pp) l
    end

    (* variable elimination. First, build formula; then perform CNF. *)
    let to_clauses (e:t): Literal.t list list =
      (* let i range over e.eq
             j range over e.lower
             k range over e.upper.
         produce:
         [∨_j ∨_k (m_j < m_k ∨ ∨_i (m_i = m_j ∧ m_i = m_k))]
      *)
      let form_l =
        let open CCList.Infix in
        e.lower >>= fun m_j ->
        e.upper >>= fun m_k ->
        let f_diseq = Form.atom (Lit.mk_rat_less m_j m_k) in
        let f_eq =
          e.eq >|= fun m_i ->
          Form.and_
            [ Form.atom (Lit.mk_rat_eq m_i m_j);
              Form.atom (Lit.mk_rat_eq m_i m_k);
            ]
        in
        CCList.return (Form.or_ (f_diseq :: f_eq))
      in
      let form = Form.or_ form_l in
      Util.debugf ~section 5
        "(@[<2>elim_var_non_cnf :var %a@ :clause %a@ :state %a@ :form %a@])"
        (fun k->k T.pp_var e.var Lits.pp e.initial_lits pp e Form.pp form);
      begin
        Form.cnf form
        |> List.rev_map (fun lits -> List.rev_append e.side_lits lits)
      end

    exception Make_err

    (* builder *)
    let make_exn (lits:Literals.t) (x:T.var): t =
      assert (not (Literals.is_shielded x lits));
      (* gather literals *)
      let lower = ref [] in
      let upper = ref [] in
      let eq = ref [] in
      let side = ref [] in
      let push_l = CCList.Ref.push in
      let t_x = T.var x in
      Array.iter
        (fun lit -> match lit with
           | Lit.Rat {Rat_lit.op; left=m1; right=m2} ->
             begin match M.find m1 t_x, M.find m2 t_x with
               | None, None -> push_l side lit
               | Some _, Some _ -> raise Make_err (* cancellations are possible *)
               | Some c1, None ->
                 let m = M.Rat.divide (M.difference m2 (M.remove m1 t_x)) c1 in
                 begin match op with
                   | Rat_lit.Equal -> push_l eq m
                   | Rat_lit.Less -> push_l upper m
                 end
               | None, Some c2 ->
                 let m = M.Rat.divide (M.difference m1 (M.remove m2 t_x)) c2 in
                 begin match op with
                   | Rat_lit.Equal -> push_l eq m
                   | Rat_lit.Less -> push_l lower m
                 end
             end
           | Lit.Equation (t, u, true) when T.equal t t_x ->
             push_l eq (M.Rat.singleton Q.one u)
           | Lit.Equation (t, u, true) when T.equal u t_x ->
             push_l eq (M.Rat.singleton Q.one t)
           | Lit.Equation (t, _, false) when T.equal t t_x -> raise Make_err
           | Lit.Equation (_, u, false) when T.equal u t_x -> raise Make_err
           | _ ->
             assert (not (Lit.var_occurs x lit)); (* shielding *)
             push_l side lit)
        lits;
      { var=x; lower= !lower; upper= !upper; eq= !eq;
        side_lits= !side; initial_lits=lits; }

    let make lits x : t option =
      try Some (make_exn lits x)
      with Make_err -> None
  end

  let eliminate_unshielded (c:C.t): C.t list option =
    let module E = Elim_var in
    let nvars = unshielded_vars (C.lits c) in
    begin match nvars with
      | [] -> None
      | x :: _ ->
        begin match E.make (C.lits c) x with
          | None -> None
          | Some e ->
            let clauses = E.to_clauses e in
            let proof =
              Proof.Step.simp [C.proof_parent c]
                ~tags:[Proof.Tag.T_lra]
                ~rule:(Proof.Rule.mkf "elim_var(%a)" T.pp_var x)
            in
            let new_c =
              List.map
                (fun lits ->
                   C.create lits proof
                     ~penalty:(C.penalty c) ~trail:(C.trail c))
                clauses
            in
            Util.debugf ~section 4
              "(@[<2>elim_var :var %a@ :clause %a@ :yields (@[<hv>%a@])@]@)"
              (fun k->k T.pp_var x C.pp c (Util.pp_list C.pp) new_c);
            Some new_c
        end
    end

  (** {2 Setup} *)

  (* print index into file *)
  let _print_idx file idx =
    CCIO.with_out file
      (fun oc ->
         let pp_leaf _ _ = () in
         let out = Format.formatter_of_out_channel oc in
         Format.fprintf out "@[<2>%a@]@." (PS.TermIndex.to_dot pp_leaf) idx;
         flush oc)

  let setup_dot_printers () =
    CCOpt.iter
      (fun f ->
         Signal.once Signals.on_dot_output
           (fun () -> _print_idx f !_idx_unit_eq))
      !dot_unit_;
    ()

  let register () =
    Util.debug ~section 2 "rat-arith: setup env";
    (* add inference rules *)
    Env.add_binary_inf "rat_sup_active" canc_sup_active;
    Env.add_binary_inf "rat_sup_passive" canc_sup_passive;
    Env.add_unary_inf "rat_cancellation" cancellation;
    Env.add_unary_inf "rat_eq_factoring" canc_equality_factoring;
    Env.add_binary_inf "rat_ineq_chaining" canc_ineq_chaining;
    Env.add_unary_inf "rat_ineq_factoring" canc_ineq_factoring;
    Env.add_multi_simpl_rule eliminate_unshielded;
    Env.add_unary_simplify canc_demodulation;
    Env.add_backward_simplify canc_backward_demodulation;
    Env.add_is_trivial is_tautology;
    Env.add_unary_simplify convert_lit;
    if !enable_trivial_ineq_ then (
      Env.add_redundant is_redundant_by_ineq;
    );
    if !enable_demod_ineq_ then (
      Env.add_active_simplify demod_ineq;
    );
    Env.add_multi_simpl_rule eliminate_unshielded;
    (* completeness? I don't think so *)
    Ctx.lost_completeness ();
    (* enable AC-property of sum *)
    (* FIXME: currently AC doesn't handle builtins
       if !_enable_ac then begin
       let sum = ID.Arith.sum in
       let ty = Signature.find_exn Signature.TPTP.Arith.full sum in
       let module A = Env.flex_get AC.key_ac in
       A.add sum ty;
       end;
    *)
    setup_dot_printers ();
    ()
end

let k_should_register = Flex_state.create_key ()
let k_has_rat = Flex_state.create_key ()

let extension =
  let env_action env =
    let module E = (val env : Env.S) in
    if E.flex_get k_should_register then (
      let module I = Make(E) in
      I.register ()
    ) else if E.flex_get k_has_rat then (
      (* arith not enabled, so we cannot solve the problem, do not answer "sat" *)
      E.Ctx.lost_completeness ();
    )
  and post_typing_action stmts state =
    let module PT = TypedSTerm in
    let has_rat =
      CCVector.to_seq stmts
      |> Sequence.flat_map Stmt.Seq.to_seq
      |> Sequence.flat_map
        (function
          | `ID _ -> Sequence.empty
          | `Ty ty -> Sequence.return ty
          | `Form t
          | `Term t -> PT.Seq.subterms t |> Sequence.filter_map PT.ty)
      |> Sequence.exists (PT.Ty.equal PT.Ty.rat)
    in
    let should_reg = !enable_rat_ && has_rat in
    Util.debugf ~section 2 "decision to register rat-arith: %B" (fun k->k should_reg);
    state
    |> Flex_state.add k_should_register should_reg
    |> Flex_state.add k_has_rat has_rat
  in
  { Extensions.default with
      Extensions.
    name="arith_rat";
    post_typing_actions=[post_typing_action];
    env_actions=[env_action];
  }

let () =
  Params.add_opts
    [ "--rat-no-semantic-tauto"
    , Arg.Clear enable_semantic_tauto_
    , " disable rational arithmetic semantic tautology check"
    ; "--rat-arith"
    , Arg.Set enable_rat_
    , " enable axiomatic rational arithmetic"
    ; "--no-arith"
    , Arg.Clear enable_rat_
    , " disable axiomatic rational arithmetic"
    ; "--rat-ac"
    , Arg.Set enable_ac_
    , " enable AC axioms for rational arithmetic (sum)"
    ; "--dot-rat-unit"
    , Arg.String (fun s -> dot_unit_ := Some s)
    , " print arith-rat-unit index into file"
    ];
  ()

