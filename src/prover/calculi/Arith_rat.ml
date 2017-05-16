
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Cancellative Inferences} *)

open Logtk

module T = Term
module Lit = Literal
module Lits = Literals
module S = Subst
module M = Monome
module MF = Monome.Focus
module AL = Rat_lit
module ALF = AL.Focus
module Stmt = Statement

let stat_rat_sup = Util.mk_stat "rat.superposition"
let stat_rat_cancellation = Util.mk_stat "rat.rat_cancellation"
let stat_rat_eq_factoring = Util.mk_stat "rat.eq_factoring"
let stat_rat_ineq_chaining = Util.mk_stat "rat.ineq_chaining"
let stat_rat_purify = Util.mk_stat "rat.purify"
let stat_rat_semantic_tautology = Util.mk_stat "rat.semantic_tauto"
let stat_rat_ineq_factoring = Util.mk_stat "rat.ineq_factoring"
let stat_rat_demod = Util.mk_stat "rat.demod"
let stat_rat_backward_demod = Util.mk_stat "rat.backward_demod"
(*
let stat_rat_reflexivity_resolution = Util.mk_stat "rat.reflexivity_resolution"
*)

let prof_rat_sup = Util.mk_profiler "rat.superposition"
let prof_rat_cancellation = Util.mk_profiler "rat.rat_cancellation"
let prof_rat_eq_factoring = Util.mk_profiler "rat.eq_factoring"
let prof_rat_ineq_chaining = Util.mk_profiler "rat.ineq_chaining"
let prof_rat_purify = Util.mk_profiler "rat.purify"
let prof_rat_demod = Util.mk_profiler "rat.demod"
let prof_rat_backward_demod = Util.mk_profiler "rat.backward_demod"
let prof_rat_semantic_tautology = Util.mk_profiler "rat.semantic_tauto"
let prof_rat_ineq_factoring = Util.mk_profiler "rat.ineq_factoring"
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

(* TODO: global purification framework *)
(* flag to be used to know when a clause cannot be purified *)
let flag_no_purify = SClause.new_flag ()

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  (** {2 Contributions to Env} *)

  val register : unit -> unit
end

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
      subst : Subst.t;
    }
  end

  let rule_canc = Proof.Rule.mk "canc_sup"

  (* do cancellative superposition *)
  let _do_canc info acc =
    let open SupInfo in
    let ord = Ctx.ord () in
    let renaming = Ctx.renaming_clear () in
    let subst = info.subst in
    let idx_a, _ = Lits.Pos.cut info.active_pos in
    let idx_p, _ = Lits.Pos.cut info.passive_pos in
    let s_a = info.active_scope and s_p = info.passive_scope in
    let lit_a = ALF.apply_subst ~renaming subst (info.active_lit,s_a) in
    let lit_p = ALF.apply_subst ~renaming subst (info.passive_lit,s_p) in
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
      let lits_a = Lit.apply_subst_list ~renaming subst (lits_a,s_a) in
      let lits_p = CCArray.except_idx (C.lits info.passive) idx_p in
      let lits_p = Lit.apply_subst_list ~renaming subst (lits_p,s_p) in
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
      let all_lits = new_lit :: lits_a @ lits_p in
      (* build clause *)
      let proof =
        Proof.Step.inference
          ~rule:rule_canc
          ~comment:(CCFormat.sprintf "lhs(%a)" MF.pp mf_a)
          [C.proof_parent_subst (info.active,s_a) subst;
           C.proof_parent_subst (info.passive,s_p) subst] in
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
    let active_lit' = ALF.apply_subst ~renaming subst (active_lit,s_a) in
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
    && (C.trail_subsumes c' c)
    then (
      (* we know all variables of [active_lit] are bound, no need
         for a renaming *)
      let active_lit = ALF.apply_subst_no_renaming subst (active_lit,s_a) in
      let active_lit, passive_lit = ALF.scale active_lit passive_lit in
      match active_lit, passive_lit with
        | ALF.Left (AL.Equal, mf1, m1), _
        | ALF.Right (AL.Equal, m1, mf1), _ ->
          let new_lit = ALF.replace passive_lit
              (M.difference m1 (MF.rest mf1)) in
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
        add_premise c';
        (* recurse until the literal isn't reducible *)
        Util.debugf ~section 4
          "@[<2>rewrite arith lit (@[%a@])@ into (@[%a@])@ using clause @[%a@]@ and subst @[%a@]@]"
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
          Proof.Step.inference
            ~rule:(Proof.Rule.mk "canc_demod")
            (C.proof_parent c ::
               List.map (fun (c,subst) -> C.proof_parent_subst (c,1) subst) !clauses)
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
                  let alit' = ALF.apply_subst_no_renaming subst (alit',1) in
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
           let {AL.op; left=m1; right=m2} = a_lit in
           Util.debugf ~section 5 "@[<2>try cancellation@ in @[%a@]@]" (fun k->k AL.pp a_lit);
           (* try to unify terms in [m1] and [m2] *)
           MF.unify_mm (m1,0) (m2,0)
           |> Sequence.fold
             (fun acc (mf1, mf2, subst) ->
                let renaming = Ctx.renaming_clear () in
                let mf1' = MF.apply_subst ~renaming subst (mf1,0) in
                let mf2' = MF.apply_subst ~renaming subst (mf2,0) in
                let is_max_lit = C.is_maxlit (c,0) subst ~idx in
                Util.debugf ~section 5
                  "@[<4>... candidate:@ @[%a@] (max lit ? %B)@]"
                  (fun k->k S.pp subst is_max_lit);
                if is_max_lit && MF.is_max ~ord mf1' && MF.is_max ~ord mf2'
                then (
                  (* do the inference *)
                  let lits' = CCArray.except_idx (C.lits c) idx in
                  let lits' = Lit.apply_subst_list ~renaming subst (lits',0) in
                  let new_lit = Lit.mk_rat_op op (MF.rest mf1') (MF.rest mf2') in
                  let all_lits = new_lit :: lits' in
                  let proof =
                    Proof.Step.inference
                      ~rule:(Proof.Rule.mk "cancellation")
                      [C.proof_parent_subst (c,0) subst] in
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
                  let subst = Unif.FO.unification (t1,0) (t2,0) in
                  Util.debugf ~section 5
                    "@[<2>arith canc. eq. factoring:@ possible match in @[%a@]@ (at %d, %d)@]"
                    (fun k->k C.pp c idx1 idx2);
                  MF.unify_ff ~subst (mf1,0) (mf2,0)
                  |> Sequence.fold
                    (fun acc (_, _, subst) ->
                       let renaming = Ctx.renaming_clear () in
                       let lit1' = ALF.apply_subst ~renaming subst (lit1,0) in
                       let lit2' = ALF.apply_subst ~renaming subst (lit2,0) in
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
                             ~renaming subst (other_lits,0) in
                         (* apply subst and build clause *)
                         let all_lits = new_lits @ other_lits in
                         let proof =
                           Proof.Step.inference
                             ~rule:rule_canc_eq_fact
                             ~comment:(CCFormat.sprintf "idx(%d,%d)" idx1 idx2)
                             [C.proof_parent_subst (c,0) subst] in
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
      subst : Subst.t;
    }
  end

  (* range from low to low+len *)
  let _range low len =
    let rec make acc i len =
      if Z.sign len < 0 then acc
      else make (i::acc) (Z.succ i) (Z.pred len)
    in make [] low len

  (* cancellative chaining *)
  let _do_chaining info acc =
    let open ChainingInfo in
    let ord = Ctx.ord () in
    let renaming = S.Renaming.create () in
    let subst = info.subst in
    let idx_l, _ = Lits.Pos.cut info.left_pos in
    let idx_r, _ = Lits.Pos.cut info.right_pos in
    let s_l = info.left_scope and s_r = info.right_scope in
    let lit_l = ALF.apply_subst ~renaming subst (info.left_lit,s_l) in
    let lit_r = ALF.apply_subst ~renaming subst (info.right_lit,s_r) in
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
          let lits_l = Lit.apply_subst_list ~renaming subst (lits_l,s_l) in
          let lits_r = CCArray.except_idx (C.lits info.right) idx_r in
          let lits_r = Lit.apply_subst_list ~renaming subst (lits_r,s_r) in
          let all_lits = new_lit :: lits_l @ lits_r in
          let proof =
            Proof.Step.inference
              ~rule:(Proof.Rule.mk "canc_ineq_chaining")
              ~comment:(CCFormat.sprintf "(@[idx(%d,%d)@ left(%a)@ right(%a)@])"
                  idx_l idx_r T.pp (MF.term mf_2) T.pp (MF.term mf_1))
              [C.proof_parent_subst (info.left,s_l) subst;
               C.proof_parent_subst (info.right,s_r) subst] in
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
    let _do_factoring ~subst lit1 lit2 i j =
      let renaming = S.Renaming.create () in
      let lit1 = ALF.apply_subst ~renaming subst (lit1,0) in
      let lit2 = ALF.apply_subst ~renaming subst (lit2,0) in
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
            let other_lits = Lit.apply_subst_list ~renaming subst (other_lits,0) in
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
            let lits = new_lit :: other_lits in
            (* build clauses *)
            let proof =
              Proof.Step.inference
                ~rule:(Proof.Rule.mk "canc_ineq_factoring")
                [C.proof_parent_subst (c,0) subst] in
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

  (** {3 Others} *)

  let _has_rat c = CCArray.exists Lit.is_rat (C.lits c)

  module Simp = Simplex.MakeHelp(T)

  (* tautology check: take the linear system that is the negation
     of all a≠b and a≤b, and check its (rational) satisfiability. If
     it's unsat in Q, it's unsat in Z, and its negation (a subset of c)
     is tautological *)
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
          Proof.Step.simp ~rule:(Proof.Rule.mk "convert_lit") [C.proof_parent c]
        in
        let c' =
          C.create ~trail:(C.trail c) ~penalty:(C.penalty c) lits proof
        in
        Util.debugf ~section 5 "(@[convert@ :from %a@ :to %a@])"
          (fun k->k C.pp c C.pp c');
        SimplM.return_new c'
    end

  (* replace arith subterms with fresh variable + constraint *)
  let purify c = SimplM.return c
  (* FIXME
    let module TC = T.Classic in
    Util.enter_prof prof_rat_purify;
    (* set of new literals *)
    let new_lits = ref [] in
    let cache = T.Tbl.create 16 in  (* cache for term->var *)
    let _add_lit lit = new_lits := lit :: !new_lits in
    (* index of the next fresh variable *)
    let varidx =
      ref ((Lits.Seq.terms (C.lits c)
            |> Sequence.flat_map T.Seq.vars
            |> T.Seq.max_var) + 1) in
    (* replace term by a fresh var + constraint *)
    let replace t ~by =
      (* [v]: fresh var that will replace [t] *)
      let v =
        try T.Tbl.find cache t
        with Not_found ->
          let ty = T.ty t in
          let v = T.var_of_int ~ty !varidx in
          T.Tbl.replace cache t v;
          incr varidx;
          v
      in
      let lit = Lit.mk_rat_neq (M.Int.singleton Z.one v) by in
      _add_lit lit;
      v (* return variable instead of literal *)
    (* should we purify the term t with head symbol [s] ?
       yes if it's an arith expression or if it a int-sorted
       function/constant under some other function *)
    and should_purify ~root s t =
      Builtin.is_rat s
      ||
      ( not root && Type.equal (T.ty t) Type.TPTP.int)
    in
    let purify_sub t =
      Util.debugf ~section 5 "@[<2>need to purify term@ @[%a@]@]" (fun k->k T.pp t);
      (* purify the term and add a constraint *)
      begin match M.Int.of_term t with
        | None ->
          Util.debugf ~section 5
            "@[<2>could not purify@ @[%a@]@ (non linear; cache)@]" (fun k->k T.pp t);
          C.set_flag flag_no_purify c true;
          t  (* non linear, abort. *)
        | Some m -> replace t ~by:m
      end
    in
    (* purify a term (adding constraints to the list).  *)
    let rec purify_term ~root t = match TC.view t with
      | TC.AppBuiltin (b, []) when Builtin.is_numeric b -> t   (* keep constants *)
      | TC.NonFO
      | TC.DB _
      | TC.Var _ -> t
      | TC.AppBuiltin (Builtin.Int _, []) -> t  (* don't purify numbers *)
      | TC.AppBuiltin (b, _l) when should_purify ~root b t -> purify_sub t
      | TC.App _ when (not root && Type.equal (T.ty t) Type.TPTP.int) ->
        purify_sub t (* uninterpreted int subterm *)
      | TC.AppBuiltin _
      | TC.App _ ->
        (* not an arith term. *)
        begin match T.view t with
          | T.App (hd, l) ->
            T.app hd (List.map (purify_term ~root:false) l)
          | T.AppBuiltin (b, l) ->
            T.app_builtin ~ty:(T.ty t) b (List.map (purify_term ~root:false) l)
          | T.DB _
          | T.Var _ -> assert false
          | T.Const _ -> t
        end
    in
    (* replace! *)
    let res =
      if C.get_flag flag_no_purify c
      then SimplM.return_same c
      else
        let lits' = Lits.map (purify_term ~root:true) (C.lits c) in
        match !new_lits with
          | [] -> SimplM.return_same c (* no change *)
          | _::_ ->
            let all_lits = !new_lits @ (Array.to_list lits') in
            let proof =
              ProofStep.mk_inference ~rule:(ProofStep.mk_rule "purify") [C.proof c] in
            let new_c =
              C.create ~trail:(C.trail c) ~penalty:(C.penalty c) all_lits proof
            in
            Util.debugf ~section 5 "@[<2>purify@ @[%a@]@ into @[%a@]@]"
              (fun k->k C.pp c C.pp new_c);
            Util.incr_stat stat_rat_purify;
            SimplM.return_new new_c
    in
    Util.exit_prof prof_rat_purify;
    res
  *)

  (** {6 Variable Elimination Procedure} *)

  let is_shielded lits ~var =
    let rec shielded_by_term ~root t = match T.view t with
      | T.Var v' when HVar.equal Type.equal v' var -> not root
      | T.Var _
      | T.DB _
      | T.Const _ -> false
      | T.AppBuiltin (_, l)
      | T.App (_, l) ->
        (* ignore arith terms, they are shielding as well. We should
           only eliminate variables that occur directly under proper arith
           literals *)
        List.exists (shielded_by_term ~root:false) l
    in
    (* is there a term, directly under a literal, that shields the variable? *)
    lits
    |> Lits.Seq.terms
    |> Sequence.exists (shielded_by_term ~root:true)

  let naked_vars lits =
    Lits.vars lits
    |> List.filter (fun var -> Type.equal (HVar.ty var) Type.int)
    |> List.filter (fun var -> not (is_shielded lits ~var))

  let _negate_lits = List.map Lit.negate

  let eliminate_unshielded _ = None
    (* FIXME: new elimination
    let module NVE = NakedVarElim in
    let nvars = naked_vars (C.lits c) in
    match nvars with
      | [] -> None
      | x::_ ->
        (* eliminate v *)
        Util.debugf ~section 3
          "@[<2>eliminate naked variable %a@ from @[%a@]@]" (fun k->k HVar.pp x C.pp c);
        (* split C into C' (not containing v) and 6 kinds of literals *)
        let view = NVE.of_lits (C.lits c) x in
        let delta, view = NVE.scale view in
        if not (Z.fits_int delta) then None
        else begin
          let delta = Z.to_int delta in
          let a_set = NVE.a_set view
          and b_set = NVE.b_set view in
          (* prepare to build clauses *)
          let acc = ref [] in
          let add_clause ~by ~which lits =
            let rule_name =
              CCFormat.sprintf "var_elim(%s×%a → %a)" (Z.to_string view.NVE.lcm)
                HVar.pp x M.pp by
            in
            let rule = ProofStep.mk_rule rule_name ~comment: [ which] in
            let proof = ProofStep.mk_inference ~rule [C.proof c] in
            let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) lits proof in
            Util.debugf ~section 5
              "@[<2>elimination of %s×%a@ by %a (which:%s)@ in @[%a@]:@ gives @[%a@]@]"
              (fun k->k (Z.to_string view.NVE.lcm)
                  HVar.pp x M.pp by which C.pp c C.pp new_c);
            acc := new_c :: !acc
          in
          (* choose which form to use *)
          if List.length a_set > List.length b_set
          then begin
            (* use B *)
            Util.debug ~section 5 "use the B elimination algorithm";
            (* first, the -infty part *)
            for i = 1 to delta do
              let x' = M.Int.const (Z.of_int i) in
              let lits = view.NVE.rest @
                  _negate_lits (NVE.eval_minus_infty view x') in
              add_clause ~by:x' ~which:"-∝" lits
            done;
            (* then the enumeration *)
            for i = 1 to delta do
              List.iter
                (fun x' ->
                   (* evaluate at x'+i *)
                   let x'' = M.add_const x' Z.(of_int i) in
                   let lits = view.NVE.rest @ _negate_lits (NVE.eval_at view x'') in
                   add_clause ~by:x'' ~which:"middle" lits
                ) b_set
            done;
          end else begin
            (* use A *)
            Util.debug ~section 5 "use the A elimination algorithm";
            (* first, the +infty part *)
            for i = 1 to delta do
              let x' = M.Int.const Z.(neg (of_int i)) in
              let lits = view.NVE.rest @
                  _negate_lits (NVE.eval_plus_infty view x') in
              add_clause ~by:x' ~which:"+∝" lits
            done;
            (* then the enumeration *)
            for i = 1 to delta do
              List.iter
                (fun x' ->
                   (* evaluate at x'-i *)
                   let x'' = M.add_const x' Z.(neg (of_int i)) in
                   let lits = view.NVE.rest @ _negate_lits (NVE.eval_at view x'') in
                   add_clause ~by:x'' ~which:"middle" lits
                ) a_set
            done;
          end;
          Some !acc
        end
    *)

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
    Env.add_simplify canc_demodulation;
    Env.add_backward_simplify canc_backward_demodulation;
    Env.add_is_trivial is_tautology;
    Env.add_simplify purify;
    Env.add_simplify convert_lit;
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
    ; "--rat"
    , Arg.Set enable_rat_
    , " enable axiomatic rational arithmetic"
    ; "--no-arith"
    , Arg.Clear enable_rat_
    , " disable axiomatic rational arithmetic"
    ; "--arith-ac"
    , Arg.Set enable_ac_
    , " enable AC axioms for rational arithmetic (sum)"
    ; "--dot-arith-unit"
    , Arg.String (fun s -> dot_unit_ := Some s)
    , " print arith-unit index into file"
    ];
  ()

