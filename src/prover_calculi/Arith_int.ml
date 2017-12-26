
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
module AL = Int_lit
module ALF = AL.Focus
module Stmt = Statement
module US = Unif_subst

let stat_arith_sup = Util.mk_stat "int.superposition"
let stat_arith_cancellation = Util.mk_stat "int.arith_cancellation"
let stat_arith_eq_factoring = Util.mk_stat "int.eq_factoring"
let stat_arith_ineq_chaining = Util.mk_stat "int.ineq_chaining"
let stat_arith_case_switch = Util.mk_stat "int.case_switch"
let stat_arith_semantic_tautology = Util.mk_stat "int.semantic_tauto"
let stat_arith_semantic_tautology_steps = Util.mk_stat "int.semantic_tauto.steps"
let stat_arith_ineq_factoring = Util.mk_stat "int.ineq_factoring"
let stat_arith_div_chaining = Util.mk_stat "int.div_chaining"
let stat_arith_divisibility = Util.mk_stat "int.divisibility"
let stat_arith_demod = Util.mk_stat "int.demod"
let stat_arith_backward_demod = Util.mk_stat "int.backward_demod"
let stat_arith_trivial_ineq = Util.mk_stat "int.redundant_by_ineq.calls"
let stat_arith_trivial_ineq_steps = Util.mk_stat "int.redundant_by_ineq.steps"
let stat_arith_demod_ineq = Util.mk_stat "int.demod_ineq.calls"
let stat_arith_demod_ineq_steps = Util.mk_stat "int.demod_ineq.steps"
(*
let stat_arith_reflexivity_resolution = Util.mk_stat "int.reflexivity_resolution"
*)

let prof_arith_sup = Util.mk_profiler "int.superposition"
let prof_arith_cancellation = Util.mk_profiler "int.arith_cancellation"
let prof_arith_eq_factoring = Util.mk_profiler "int.eq_factoring"
let prof_arith_ineq_chaining = Util.mk_profiler "int.ineq_chaining"
let prof_arith_demod = Util.mk_profiler "int.demod"
let prof_arith_backward_demod = Util.mk_profiler "int.backward_demod"
let prof_arith_semantic_tautology = Util.mk_profiler "int.semantic_tauto"
let prof_arith_ineq_factoring = Util.mk_profiler "int.ineq_factoring"
let prof_arith_div_chaining = Util.mk_profiler "int.div_chaining"
let prof_arith_divisibility = Util.mk_profiler "int.divisibility"
let prof_arith_trivial_ineq = Util.mk_profiler "int.redundant_by_ineq"
let prof_arith_demod_ineq = Util.mk_profiler "int.demod_ineq"
(*
let prof_arith_reflexivity_resolution = Util.mk_profiler "int.reflexivity_resolution"
*)

let section = Util.Section.make ~parent:Const.section "int-arith"

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  (** {3 Equations and Inequations} *)

  val canc_sup_active: Env.binary_inf_rule
  (** cancellative superposition where given clause is active *)

  val canc_sup_passive: Env.binary_inf_rule
  (** cancellative superposition where given clause is passive *)

  val cancellation: Env.unary_inf_rule
  (** cancellation (unifies some terms on both sides of a
      comparison operator) *)

  val canc_equality_factoring: Env.unary_inf_rule
  (** cancellative equality factoring *)

  val canc_ineq_chaining : Env.binary_inf_rule
  (** cancellative inequality chaining.

      Also does case switch if conditions are present:
          C1 or a < b     C2 or b < c
      -------------------------------------
          C1 or C2 or or_{i=a+1....c-1} (b = i)
      if a and c are integer linear expressions whose difference is
      a constant. If a > c, then the range a...c is empty and the literal
      is just removed. *)

  val canc_ineq_factoring : Env.unary_inf_rule
  (** Factoring between two inequation literals *)

  val canc_less_to_lesseq : Env.lit_rewrite_rule
  (** Simplification:  a <= b  ----> a < b+1 *)

  (** {3 Divisibility} *)

  val canc_div_chaining : Env.binary_inf_rule
  (** Chain together two divisibility literals, assuming they share the
      same prime *)

  val canc_div_case_switch : Env.unary_inf_rule
  (** Eliminate negative divisibility literals within a power-of-prime
      quotient of Z:
      not (d^i | m) -----> *)

  val canc_div_prime_decomposition : Env.multi_simpl_rule
  (** Eliminate divisibility literals with a non-power-of-prime
      quotient of Z (for instance  [6 | a ---> { 2 | a, 3 | a }]) *)

  val canc_divisibility : Env.unary_inf_rule
  (** Infer divisibility constraints from integer equations,
      for instace   C or  2a=b ---->  C or 2 | b    if a is maximal *)

  (** {3 Other} *)

  val is_tautology : C.t -> bool
  (** is the clause a tautology w.r.t linear expressions? *)

  val eliminate_unshielded : Env.multi_simpl_rule
  (** Eliminate unshielded variables using an adaptation of
      Cooper's algorithm *)

  (** {2 Contributions to Env} *)

  val register : unit -> unit
end

let enable_arith_ = ref true
let enable_ac_ = ref false
let enable_semantic_tauto_ = ref true
let enable_trivial_ineq_ = ref true
let enable_demod_ineq_ = ref true
let dot_unit_ = ref None
let diff_to_lesseq_ = ref `Simplify

let case_switch_limit = ref 30
let div_case_switch_limit = ref 100

let flag_tauto = SClause.new_flag ()
let flag_computed_tauto = SClause.new_flag ()

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module Ctx = Env.Ctx
  module C = Env.C
  module PS = Env.ProofState

  let _idx_eq = ref (PS.TermIndex.empty ())
  let _idx_ineq_left = ref (PS.TermIndex.empty ())
  let _idx_ineq_right = ref (PS.TermIndex.empty ())
  let _idx_div = ref (PS.TermIndex.empty ())
  let _idx_all = ref (PS.TermIndex.empty ())

  (* unit clauses *)
  let _idx_unit_eq = ref (PS.TermIndex.empty ())
  let _idx_unit_div = ref (PS.TermIndex.empty ())
  let _idx_unit_ineq = ref (PS.TermIndex.empty ())

  (* apply [f] to some subterms of [c] *)
  let update f c =
    let ord = Ctx.ord () in
    _idx_eq :=
      Lits.fold_terms ~vars:false ~ty_args:false ~which:`Max ~ord ~subterms:false
        ~eligible:C.Eligible.(filter Lit.is_arith_eqn ** max c)
        (C.lits c)
      |> Sequence.fold
        (fun acc (t,pos) ->
           let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
           f acc t with_pos)
        !_idx_eq;
    let left, right =
      Lits.fold_terms ~vars:false ~ty_args:false ~which:`Max ~ord ~subterms:false
        ~eligible:C.Eligible.(filter Lit.is_arith_ineq** max c)
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
    _idx_div :=
      Lits.fold_terms ~vars:false ~ty_args:false ~which:`Max ~ord ~subterms:false
        ~eligible:C.Eligible.(filter Lit.is_arith_divides ** max c)
        (C.lits c)
      |> Sequence.fold
        (fun acc (t,pos) ->
           let with_pos = C.WithPos.( {term=t; pos; clause=c} ) in
           f acc t with_pos)
        !_idx_div;
    _idx_all :=
      Lits.fold_terms ~vars:false ~ty_args:false ~which:`Max ~ord ~subterms:false
        ~eligible:C.Eligible.(filter Lit.is_arith ** max c)
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
      | [| Lit.Int ((AL.Binary (AL.Equal, _, _)) as alit) |] ->
        let pos = Position.(arg 0 stop) in
        _idx_unit_eq :=
          AL.fold_terms ~subterms:false ~vars:false ~pos ~which:`Max ~ord alit
          |> Sequence.fold
            (fun acc (t,pos) ->
               assert (not (T.is_var t));
               let with_pos = C.WithPos.( {term=t; pos; clause=c;} ) in
               f acc t with_pos)
            !_idx_unit_eq
      | [| Lit.Int ((AL.Binary (AL.Lesseq, _, _)) as alit) |] ->
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
      | [| Lit.Int (AL.Divides d as alit) |] when d.AL.sign ->
        let pos = Position.(arg 0 stop) in
        _idx_unit_div :=
          AL.fold_terms ~subterms:false ~vars:false ~pos ~which:`Max ~ord alit
          |> Sequence.fold
            (fun acc (t,pos) ->
               assert (not (T.is_var t));
               let with_pos = C.WithPos.( {term=t; pos; clause=c;} ) in
               f acc t with_pos)
            !_idx_unit_div
      | _ -> ()
    end;
    ()

  let () =
    Signal.on PS.ActiveSet.on_add_clause
      (fun c ->
         if !enable_arith_ then update PS.TermIndex.add c;
         Signal.ContinueListening);
    Signal.on PS.SimplSet.on_add_clause
      (fun c ->
         if !enable_arith_ then update_simpl PS.TermIndex.add c;
         Signal.ContinueListening);
    Signal.on PS.ActiveSet.on_remove_clause
      (fun c ->
         if !enable_arith_ then update PS.TermIndex.remove c;
         Signal.ContinueListening);
    Signal.on PS.SimplSet.on_remove_clause
      (fun c ->
         if !enable_arith_ then update_simpl PS.TermIndex.remove c;
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
      subst : US.t;
    }
  end

  let rule_canc = Proof.Rule.mk "canc_sup"

  (* do cancellative superposition *)
  let _do_canc info acc =
    let open SupInfo in
    let ord = Ctx.ord () in
    let renaming = Subst.Renaming.create () in
    let us = info.subst in
    let subst = US.subst us in
    let idx_a, _ = Lits.Pos.cut info.active_pos in
    let idx_p, _ = Lits.Pos.cut info.passive_pos in
    let s_a = info.active_scope and s_p = info.passive_scope in
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
      let c_guard = Lit.of_unif_subst renaming us in
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
          Lit.mk_arith_op op
            (M.sum (MF.rest mf_p) m_a)
            (M.sum m_p (MF.rest mf_a))
        | ALF.Right (op, m_p, mf_p) ->
          Lit.mk_arith_op op
            (M.sum m_p (MF.rest mf_a))
            (M.sum (MF.rest mf_p) m_a)
        | ALF.Div _ ->
          Lit.mk_arith (ALF.replace lit_p (M.difference m_a (MF.rest mf_a)))
      in
      let all_lits = new_lit :: c_guard @ lits_a @ lits_p in
      (* build clause *)
      let proof =
        Proof.Step.inference ~tags:[Proof.Tag.T_lia]
          ~rule:rule_canc
          [C.proof_parent_subst renaming (info.active,s_a) subst;
           C.proof_parent_subst renaming (info.passive,s_p) subst] in
      let trail = C.trail_l [info.active;info.passive] in
      let penalty = C.penalty info.active + C.penalty info.passive in
      let new_c = C.create ~penalty ~trail all_lits proof in
      Util.debugf ~section 5 "@[<2>... gives@ @[%a@]@]" (fun k->k C.pp new_c);
      Util.incr_stat stat_arith_sup;
      new_c :: acc
    ) else (
      Util.debug ~section 5 "... has bad ordering conditions";
      acc
    )

  let canc_sup_active c =
    Util.enter_prof prof_arith_sup;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(pos ** max c ** filter Lit.is_arith_eq) in
    let sc_a = 0 and sc_p = 1 in
    let res =
      Lits.fold_arith_terms ~eligible ~which:`Max ~ord (C.lits c)
      |> Sequence.fold
        (fun acc (t,active_lit,active_pos) ->
           assert (ALF.op active_lit = `Binary AL.Equal);
           Util.debugf ~section 5 "@[<2>active canc. sup.@ with @[%a@]@ in @[%a@]@]"
             (fun k->k ALF.pp active_lit C.pp c);
           PS.TermIndex.retrieve_unifiables (!_idx_all,sc_p) (t,sc_a)
           |> Sequence.fold
             (fun acc (t',with_pos,subst) ->
                let passive = with_pos.C.WithPos.clause in
                let passive_pos = with_pos.C.WithPos.pos in
                let passive_lit = Lits.View.get_arith_exn (C.lits passive) passive_pos in
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
    Util.exit_prof prof_arith_sup;
    res

  let canc_sup_passive c =
    Util.enter_prof prof_arith_sup;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(max c ** arith) in
    let sc_a = 0 and sc_p = 1 in
    let res =
      Lits.fold_arith_terms ~eligible ~which:`All ~ord (C.lits c)
      |> Sequence.fold
        (fun acc (t,passive_lit,passive_pos) ->
           Util.debugf ~section 5 "@[<2>passive canc. sup.@ with @[%a@]@ in @[%a@]@]"
             (fun k->k ALF.pp passive_lit C.pp c);
           PS.TermIndex.retrieve_unifiables (!_idx_eq,sc_a) (t,sc_p)
           |> Sequence.fold
             (fun acc (t',with_pos,subst) ->
                let active = with_pos.C.WithPos.clause in
                let active_pos = with_pos.C.WithPos.pos in
                let active_lit = Lits.View.get_arith_exn (C.lits active) active_pos in
                (* must have an equation as active lit *)
                match ALF.op active_lit with
                  | `Binary AL.Equal when not (T.is_var t) && not (T.is_var t') ->
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
    Util.exit_prof prof_arith_sup;
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
    && (C.trail_subsumes c' c)
    then (
      (* we know all variables of [active_lit] are bound, no need
         for a renaming *)
      let active_lit = ALF.apply_subst Subst.Renaming.none subst (active_lit,s_a) in
      let active_lit, passive_lit = ALF.scale active_lit passive_lit in
      match active_lit, passive_lit with
        | ALF.Left (AL.Equal, mf1, m1), _
        | ALF.Right (AL.Equal, m1, mf1), _ ->
          let new_lit = ALF.replace passive_lit
              (M.difference m1 (MF.rest mf1)) in
          raise (SimplifyInto (new_lit, c',subst))
        | ALF.Div d1, ALF.Div d2 when d1.AL.sign ->
          let n1 = Z.pow d1.AL.num d1.AL.power and n2 = Z.pow d2.AL.num d2.AL.power in
          let gcd = Z.gcd (MF.coeff d1.AL.monome) (MF.coeff d2.AL.monome) in
          (* simplification: we only do the rewriting if both
             literals have exactly the same num and power...
             TODO: generalize *)
          if Z.equal n1 n2
          && Z.lt gcd Z.(pow d2.AL.num d2.AL.power)
          then
            let new_lit = ALF.replace passive_lit
                (M.uminus (MF.rest d1.AL.monome)) in
            raise (SimplifyInto (new_lit, c',subst))
        | _ -> ()
    ) else ()

  (* reduce an arithmetic literal to its current normal form *)
  let rec _demod_lit_nf ~add_lit ~add_premise ~i c a_lit =
    let ord = Ctx.ord () in
    let s_a = 1 and s_p = 0 in  (* scopes *)
    (* which term indexes can be used *)
    let indexes = match a_lit with
      | AL.Divides _ -> [!_idx_unit_div; !_idx_unit_eq]
      | AL.Binary _ -> [!_idx_unit_eq]
    in
    begin try
        AL.fold_terms ~pos:Position.stop ~vars:false ~which:`Max
          ~ord ~subterms:false a_lit
        |> Sequence.iter
          (fun (t,lit_pos) ->
             assert (not (T.is_var t));
             let passive_lit = ALF.get_exn a_lit lit_pos in
             (* search for generalizations of [t] *)
             List.iter
               (fun index ->
                  PS.TermIndex.retrieve_generalizations (index,s_a) (t,s_p)
                  |> Sequence.iter
                    (fun (_t',with_pos,subst) ->
                       let c' = with_pos.C.WithPos.clause in
                       let pos' = with_pos.C.WithPos.pos in
                       assert (C.is_unit_clause c');
                       assert (Lits.Pos.idx pos' = 0);
                       let active_lit = Lits.View.get_arith_exn (C.lits c') pos' in
                       let pos = Position.(arg i lit_pos) in
                       _try_demod_step ~subst passive_lit s_p c pos active_lit s_a c' pos'))
               indexes
          );
        (* could not simplify, keep the literal *)
        add_lit (Lit.mk_arith a_lit)
      with SimplifyInto (a_lit',c',subst) ->
        (* lit ----> lit' *)
        add_premise c' subst;
        (* recurse until the literal isn't reducible *)
        Util.debugf ~section 4
          "@[<2>rewrite arith lit (@[%a@])@ into (@[%a@])@ using clause @[%a@]@ and subst @[%a@]@]"
          (fun k->k AL.pp a_lit AL.pp a_lit' C.pp c' S.pp subst);
        _demod_lit_nf ~add_premise ~add_lit ~i c a_lit'
    end

  let eq_c_subst (c1,s1)(c2,s2) = C.equal c1 c2 && Subst.equal s1 s2

  (* demodulation (simplification) *)
  let _demodulation c =
    Util.enter_prof prof_arith_demod;
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
           | Lit.Int a_lit ->
             _demod_lit_nf ~add_lit ~add_premise ~i c a_lit
           | _ ->
             add_lit lit (* keep non-arith literals *)
      );
    (* build result clause (if it was simplified) *)
    let res =
      if !did_simplify then (
        clauses := CCList.uniq ~eq:eq_c_subst !clauses;
        let proof =
          Proof.Step.inference ~tags:[Proof.Tag.T_lia]
            ~rule:(Proof.Rule.mk "canc_demod")
            (C.proof_parent c ::
               List.rev_map
                 (fun (c,subst) -> C.proof_parent_subst Subst.Renaming.none (c,1) subst)
                 !clauses)
        in
        let trail = C.trail c in
        let new_c = C.create ~penalty:(C.penalty c) ~trail (List.rev !lits) proof in
        Util.incr_stat stat_arith_demod;
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
    Util.exit_prof prof_arith_demod;
    res

  let canc_demodulation c = _demodulation c

  (* find clauses in which some literal could be rewritten by [c], iff
     [c] is a positive unit arith clause *)
  let canc_backward_demodulation c =
    Util.enter_prof prof_arith_backward_demod;
    let ord = Ctx.ord () in
    let res = C.ClauseSet.empty in
    let res = match C.lits c with
      | [| Lit.Int (AL.Binary (AL.Equal, _, _) as alit) |] ->
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
                    Util.incr_stat stat_arith_backward_demod;
                    C.ClauseSet.add c' acc
                  ) else acc)
               acc)
          C.ClauseSet.empty
      | [| Lit.Int (AL.Binary (AL.Lesseq, _m1, _m2)) |] ->
        res (* TODO *)
      | [| Lit.Int (AL.Divides d) |] when d.AL.sign ->
        res (* TODO *)
      | _ -> res (* no demod *)
    in
    Util.exit_prof prof_arith_backward_demod;
    res

  let cancellation c =
    Util.enter_prof prof_arith_cancellation;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(max c ** arith) in
    let res =
      Lits.fold_arith ~eligible (C.lits c)
      |> Sequence.fold
        (fun acc (a_lit,pos) ->
           let idx = Lits.Pos.idx pos in
           (* cancellation depends on what the literal looks like *)
           match a_lit with
             | AL.Binary (op, m1, m2) ->
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
                      "@[<4>... candidate:@ @[%a@] (max lit ? %B)@ :mf1 %a@ :mf2 %a@]"
                      (fun k->k S.pp subst is_max_lit MF.pp mf1' MF.pp mf2');
                    if is_max_lit && MF.is_max ~ord mf1' && MF.is_max ~ord mf2'
                    then (
                      (* do the inference *)
                      let lits' = CCArray.except_idx (C.lits c) idx in
                      let lits' = Lit.apply_subst_list renaming subst (lits',0) in
                      let new_lit = Lit.mk_arith_op op (MF.rest mf1') (MF.rest mf2') in
                      let c_guard = Literal.of_unif_subst renaming us in
                      let all_lits = new_lit :: c_guard @ lits' in
                      let proof =
                        Proof.Step.inference ~tags:[Proof.Tag.T_lia]
                          ~rule:(Proof.Rule.mk "cancellation")
                          [C.proof_parent_subst renaming (c,0) subst] in
                      let trail = C.trail c in
                      let penalty = C.penalty c in
                      let new_c = C.create ~trail ~penalty all_lits proof in
                      Util.debugf ~section 3
                        "@[<2>cancellation@ of @[%a@]@ (with %a)@ into @[%a@]@]"
                        (fun k->k C.pp c Subst.pp subst C.pp new_c);
                      Util.incr_stat stat_arith_cancellation;
                      new_c :: acc
                    ) else
                      acc
                 ) acc
             | AL.Divides d ->
               Util.debugf ~section 5 "@[try cancellation@ in @[%a@]@]" (fun k->k AL.pp a_lit);
               MF.unify_self_monome (d.AL.monome,0)
               |> Sequence.fold
                 (fun acc (mf, us) ->
                    let renaming = Subst.Renaming.create () in
                    let subst = US.subst us in
                    let mf' = MF.apply_subst renaming subst (mf,0) in
                    if C.is_maxlit (c,0) subst ~idx
                    && MF.is_max ~ord mf
                    then (
                      let lits' = CCArray.except_idx (C.lits c) idx in
                      let lits' = Lit.apply_subst_list renaming subst (lits',0) in
                      let new_lit =
                        Lit.mk_divides
                          ~sign:d.AL.sign d.AL.num ~power:d.AL.power (MF.to_monome mf')
                      in
                      let c_guard = Literal.of_unif_subst renaming us in
                      let all_lits = new_lit :: c_guard @ lits' in
                      let proof =
                        Proof.Step.inference ~tags:[Proof.Tag.T_lia]
                          ~rule:(Proof.Rule.mk "cancellation")
                          [C.proof_parent_subst renaming (c,0) subst] in
                      let trail = C.trail c
                      and penalty = C.penalty c in
                      let new_c = C.create ~trail ~penalty all_lits proof in
                      Util.debugf ~section 3
                        "@[<2>cancellation@ of @[%a@]@ (with %a)@ into @[%a@]@]"
                        (fun k->k C.pp c Subst.pp subst C.pp new_c);
                      Util.incr_stat stat_arith_cancellation;
                      new_c :: acc
                    ) else acc)
                 acc)
        []
    in
    Util.exit_prof prof_arith_cancellation;
    res

  let rule_canc_eq_fact = Proof.Rule.mk "arith_eq_factoring"

  let canc_equality_factoring c =
    Util.enter_prof prof_arith_eq_factoring;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(max c ** filter Lit.is_arith_eq) in
    let res =
      Lits.fold_arith_terms ~which:`Max ~eligible ~ord (C.lits c)
      |> Sequence.fold
        (fun acc (t1,lit1,pos1) ->
           assert(ALF.op lit1 = `Binary AL.Equal);
           let idx1 = Lits.Pos.idx pos1 in
           (* lit1 is the factored literal *)
           Lits.fold_arith_terms ~which:`Max ~ord
             ~eligible:C.Eligible.(filter Lit.is_arith_eq) (C.lits c)
           |> Sequence.fold
             (fun acc (t2,lit2,pos2) ->
                assert(ALF.op lit2 = `Binary AL.Equal);
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
                         let m1 = ALF.opposite_monome_exn lit1'
                         and mf1 = ALF.focused_monome lit1'
                         and m2 = ALF.opposite_monome_exn lit2'
                         and mf2 = ALF.focused_monome lit2' in
                         let new_lit = Lit.mk_arith_neq
                             (M.sum m1 (MF.rest mf2))
                             (M.sum m2 (MF.rest mf1))
                         in
                         let other_lits = CCArray.except_idx (C.lits c) idx1 in
                         let other_lits =
                           Lit.apply_subst_list
                             renaming subst (other_lits,0) in
                         let c_guard = Literal.of_unif_subst renaming us in
                         (* apply subst and build clause *)
                         let all_lits = new_lit :: c_guard @ other_lits in
                         let proof =
                           Proof.Step.inference ~tags:[Proof.Tag.T_lia]
                             ~rule:rule_canc_eq_fact
                             [C.proof_parent_subst renaming (c,0) subst] in
                         let penalty = C.penalty c
                         and trail = C.trail c in
                         let new_c = C.create ~trail ~penalty all_lits proof in
                         Util.debugf ~section 5
                           "@[<2>arith_eq_factoring:@ @[%a@]@ gives @[%a@]@]"
                           (fun k->k C.pp c C.pp new_c);
                         Util.incr_stat stat_arith_eq_factoring;
                         new_c :: acc
                       ) else acc)
                    acc
                with Unif.Fail ->
                  acc)
             acc)
        []
    in Util.exit_prof prof_arith_eq_factoring;
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
    let us = info.subst in
    let subst = US.subst us in
    let idx_l, _ = Lits.Pos.cut info.left_pos in
    let idx_r, _ = Lits.Pos.cut info.right_pos in
    let s_l = info.left_scope and s_r = info.right_scope in
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
        | ALF.Left (AL.Lesseq, mf_1, m1), ALF.Right (AL.Lesseq, m2, mf_2) ->
          (* m2 ≤ mf_2 and mf_1 ≤ m1, with mf_1 and mf_2 sharing the same
             focused term. We deduce m2 + mf_1 ≤ m1 + mf_2 and cancel the
             term out (after scaling) *)
          assert (Z.equal (MF.coeff mf_1) (MF.coeff mf_2));
          let new_lit = Lit.mk_arith_lesseq
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
            Proof.Step.inference ~tags:[Proof.Tag.T_lia]
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
          Util.incr_stat stat_arith_ineq_chaining;
          let acc = new_c :: acc in

          (* now, maybe we can also perform case switch! We can if
             mf_1 - m1 = k + (m2 - mf_2). In this case necessarily
             Or_{i=0...k} mf_2 = m2 + i *)
          let diff = M.difference
              (M.sum m1 (MF.rest mf_2))
              (M.sum m2 (MF.rest mf_1)) in
          if M.is_const diff && Z.leq (M.const diff) Z.(of_int !case_switch_limit)
          then (
            (* re-use lits_l and lits_r, but build an enumeration *)
            let new_lits =
              List.map
                (fun i ->
                   (* mf_2 = m2 + i *)
                   Lit.mk_arith_eq (MF.to_monome mf_2) (M.add_const m2 i))
                (_range Z.zero (M.const diff))
            in
            let all_lits = CCList.flatten [new_lits; c_guard; lits_l; lits_r] in
            let proof =
              Proof.Step.inference ~tags:[Proof.Tag.T_lia]
                ~rule:(Proof.Rule.mk "canc_case_switch")
                [C.proof_parent_subst renaming (info.left,s_l) subst;
                 C.proof_parent_subst renaming (info.right,s_r) subst] in
            let trail = C.trail_l [info.left; info.right] in
            (* small penalty for case switch *)
            let penalty = C.penalty info.left + C.penalty info.right + 3 in
            let new_c = C.create ~trail ~penalty all_lits proof in
            Util.debugf ~section 5 "@[<2>case switch@ of @[%a@]@ and @[%a@]@ gives @[%a@]@]"
              (fun k->k C.pp info.left C.pp info.right C.pp new_c);
            Util.incr_stat stat_arith_case_switch;
            new_c :: acc
          ) else acc
        | _ -> assert false
    ) else
      acc

  let canc_ineq_chaining c =
    Util.enter_prof prof_arith_ineq_chaining;
    let ord = Ctx.ord () in
    let eligible = C.Eligible.(max c ** filter Lit.is_arith_lesseq) in
    let sc_l = 0 and sc_r = 1 in
    let res =
      Lits.fold_arith_terms ~eligible ~ord ~which:`Max (C.lits c)
      |> Sequence.fold
        (fun acc (t,lit,pos) ->
           match lit with
             | _ when T.is_var t -> acc (* ignore variables *)
             | ALF.Left (AL.Lesseq, mf_l, _) ->
               (* find a right-chaining literal in some other clause *)
               PS.TermIndex.retrieve_unifiables (!_idx_ineq_right,sc_r) (t,sc_l)
               |> Sequence.fold
                 (fun acc (_t',with_pos,subst) ->
                    let right = with_pos.C.WithPos.clause in
                    let right_pos = with_pos.C.WithPos.pos in
                    let lit_r = Lits.View.get_arith_exn (C.lits right) right_pos in
                    match lit_r with
                      | ALF.Right (AL.Lesseq, _, mf_r) ->
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
             | ALF.Right (AL.Lesseq, _, mf_r) ->
               (* find a right-chaining literal in some other clause *)
               PS.TermIndex.retrieve_unifiables (!_idx_ineq_left,sc_l) (t,sc_r)
               |> Sequence.fold
                 (fun acc (_t',with_pos,subst) ->
                    let left = with_pos.C.WithPos.clause in
                    let left_pos = with_pos.C.WithPos.pos in
                    let lit_l = Lits.View.get_arith_exn (C.lits left) left_pos in
                    match lit_l with
                      | ALF.Left (AL.Lesseq, mf_l, _) ->
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
    Util.exit_prof prof_arith_ineq_chaining;
    res

  (* TODO: update with equality case, check that signs correct *)
  let canc_ineq_factoring c =
    Util.enter_prof prof_arith_ineq_factoring;
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
        | ALF.Left (AL.Lesseq, mf1, m1), ALF.Left (AL.Lesseq, mf2, m2)
        | ALF.Right (AL.Lesseq, m1, mf1), ALF.Right (AL.Lesseq, m2, mf2) ->
          (* mf1 ≤ m1  or  mf2 ≤ m2  (symmetry with > if needed)
             so we deduce that if  m1-mf1.rest ≤ m2 - mf2.rest
             then the first literal implies the second, so we only
             keep the second one *)
          assert (Z.equal (MF.coeff mf1) (MF.coeff mf2));
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
                Lit.mk_arith_lesseq
                  (M.difference m1 (MF.rest mf1))
                  (M.difference m2 (MF.rest mf2))
              else
                Lit.mk_arith_lesseq
                  (M.difference m2 (MF.rest mf2))
                  (M.difference m1 (MF.rest mf1))
            in
            (* negate the literal to obtain a guard *)
            let new_lit = Lit.negate new_lit in
            let c_guard = Literal.of_unif_subst renaming us in
            let lits = new_lit :: c_guard @ other_lits in
            (* build clauses *)
            let proof =
              Proof.Step.inference ~tags:[Proof.Tag.T_lia]
                ~rule:(Proof.Rule.mk "canc_ineq_factoring")
                [C.proof_parent_subst renaming (c,0) subst] in
            let trail = C.trail c
            and penalty = C.penalty c in
            let new_c = C.create ~trail ~penalty lits proof in
            Util.debugf ~section 5 "@[<2>ineq factoring@ of @[%a@]@ gives @[%a@]@"
              (fun k->k C.pp c C.pp new_c);
            Util.incr_stat stat_arith_ineq_factoring;
            acc := new_c :: !acc
          )
        | _ -> ()
    in
    (* traverse the clause to find matching pairs *)
    let eligible = C.Eligible.(max c ** filter Lit.is_arith_lesseq) in
    Lits.fold_arith ~eligible (C.lits c)
    |> Sequence.iter
      (fun (lit1,pos1) ->
         let i = Lits.Pos.idx pos1 in
         let eligible' = C.Eligible.(filter Lit.is_arith_lesseq) in
         Lits.fold_arith ~eligible:eligible' (C.lits c)
         |> Sequence.iter
           (fun (lit2,pos2) ->
              let j = Lits.Pos.idx pos2 in
              match lit1, lit2 with
                | _ when i=j -> ()  (* need distinct lits *)
                | AL.Binary (AL.Lesseq, l1, r1), AL.Binary (AL.Lesseq, l2, r2) ->
                  (* see whether we have   l1 < a.x + mf1  and  l2 < a.x + mf2 *)
                  MF.unify_mm (r1,0) (r2,0)
                    (fun (mf1,mf2,subst) ->
                       let lit1 = ALF.mk_right AL.Lesseq l1 mf1 in
                       let lit2 = ALF.mk_right AL.Lesseq l2 mf2 in
                       _do_factoring ~subst lit1 lit2 i j
                    );
                  (* see whether we have   a.x + mf1 < r1 and a.x + mf2 < r2  *)
                  MF.unify_mm (l1,0) (l2,0)
                    (fun (mf1,mf2,subst) ->
                       let lit1 = ALF.mk_left AL.Lesseq mf1 r1 in
                       let lit2 = ALF.mk_left AL.Lesseq mf2 r2 in
                       _do_factoring ~subst lit1 lit2 i j
                    );
                  ()
                | _ -> assert false
           )
      );
    Util.exit_prof prof_arith_ineq_factoring;
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
    | AL.Binary _ when Sequence.exists T.is_var (AL.Seq.terms lit) ->
      ()  (* no way we rewrite this into a tautology *)
    | AL.Binary (AL.Lesseq, _, _) ->
      Util.incr_stat stat_arith_trivial_ineq;
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
                match Lits.View.get_arith (C.lits active_clause) active_pos with
                  | None -> assert false
                  | Some (ALF.Left (AL.Lesseq, _, _) as alit') when is_left ->
                    let alit' = ALF.apply_subst Subst.Renaming.none subst (alit',1) in
                    if C.trail_subsumes active_clause c
                    && ALF.is_strictly_max ~ord alit'
                    then (
                      (* scale *)
                      let plit, _alit' = ALF.scale plit alit' in
                      let mf1', m2' =
                        match Lits.View.get_arith (C.lits active_clause) active_pos with
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
                  | Some (ALF.Right (AL.Lesseq, _, _) as alit') when not is_left ->
                    (* symmetric case *)
                    let alit' = ALF.apply_subst Subst.Renaming.none subst (alit',1) in
                    if C.trail_subsumes active_clause c
                    && ALF.is_strictly_max ~ord alit'
                    then (
                      (* scale *)
                      let plit, _alit' = ALF.scale plit alit' in
                      let m1', mf2' =
                        match Lits.View.get_arith (C.lits active_clause) active_pos with
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
      | Lit.Int (AL.Binary (AL.Lesseq, _m1, _m2) as alit) ->
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
    Util.enter_prof prof_arith_trivial_ineq;
    let res =
      CCArray.exists
        (fun lit -> match _ineq_is_redundant_by_unit c lit with
           | None -> false
           | Some trace ->
             Util.debugf ~section 3
               "@[<2>clause @[%a@]@ trivial by inequations @[%a@]@]"
               (fun k->k C.pp c (CCFormat.list C.pp) trace);
             Util.incr_stat stat_arith_trivial_ineq_steps;
             true)
        (C.lits c)
    in
    Util.exit_prof prof_arith_trivial_ineq;
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
    | AL.Binary _ when Sequence.exists T.is_var (AL.Seq.terms lit) ->
      ()  (* too costly (will match too many things) *)
    | AL.Binary (AL.Lesseq, _, _) ->
      Util.incr_stat stat_arith_demod_ineq;
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
                match Lits.View.get_arith (C.lits active_clause) active_pos with
                  | None -> assert false
                  | Some (ALF.Left (AL.Lesseq, _, _) as alit') when not is_left ->
                    let alit' = ALF.apply_subst Subst.Renaming.none subst (alit',1) in
                    if C.trail_subsumes active_clause c
                    && ALF.is_strictly_max ~ord alit'
                    then (
                      (* scale *)
                      let plit, _alit' = ALF.scale plit alit' in
                      let mf1', m2' =
                        match Lits.View.get_arith (C.lits active_clause) active_pos with
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
                  | Some (ALF.Right (AL.Lesseq, _, _) as alit') when is_left ->
                    (* symmetric case *)
                    let alit' = ALF.apply_subst Subst.Renaming.none subst (alit',1) in
                    if C.trail_subsumes active_clause c
                    && ALF.is_strictly_max ~ord alit'
                    then (
                      (* scale *)
                      let plit, _alit' = ALF.scale plit alit' in
                      let m1', mf2' =
                        match Lits.View.get_arith (C.lits active_clause) active_pos with
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
      | Lit.Int (AL.Binary (AL.Lesseq, _m1, _m2) as alit) ->
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
    Util.enter_prof prof_arith_demod_ineq;
    let res =
      CCArray.findi
        (fun i lit -> match _ineq_is_absurd_by_unit c lit with
           | None -> None
           | Some trace ->
             Util.debugf ~section 3
               "@[<2>clause @[%a@]@ rewritten by inequations @[%a@]@]"
               (fun k->k C.pp c (CCFormat.list C.pp) trace);
             Util.incr_stat stat_arith_demod_ineq_steps;
             Some (i,trace))
        (C.lits c)
    in
    let res = match res with
      | None -> SimplM.return_same c
      | Some (i,cs) ->
        let lits = CCArray.except_idx (C.lits c) i in
        let proof = Proof.Step.simp ~tags:[Proof.Tag.T_lia]
            ~rule:(Proof.Rule.mk "int.demod_ineq")
            (C.proof_parent c :: List.map C.proof_parent cs)
        in
        let c' = C.create lits proof ~penalty:(C.penalty c) ~trail:(C.trail c) in
        SimplM.return_new c'
    in
    Util.exit_prof prof_arith_demod_ineq;
    res

  (** {3 Divisibility} *)

  let canc_div_chaining c =
    let ord = Ctx.ord () in
    Util.enter_prof prof_arith_div_chaining;
    let eligible = C.Eligible.(max c ** filter Lit.is_arith_divides) in
    let sc1 = 0 and sc2 = 1 in
    (* do the inference (if ordering conditions are ok) *)
    let _do_chaining ~sign n power c1 lit1 pos1 c2 lit2 pos2 us acc =
      let renaming = Subst.Renaming.create () in
      let subst = US.subst us in
      let idx1 = Lits.Pos.idx pos1 and idx2 = Lits.Pos.idx pos2 in
      let lit1' = ALF.apply_subst renaming subst (lit1,sc1) in
      let lit2' = ALF.apply_subst renaming subst (lit2,sc2) in
      let lit1', lit2' = ALF.scale lit1' lit2' in
      let mf1' = ALF.focused_monome lit1'
      and mf2' = ALF.focused_monome lit2' in
      (* now we have two literals with the same power and coeff *)
      let gcd = Z.gcd (MF.coeff mf1') (MF.coeff mf2') in
      (* check that we didn't "overflow", and that ordering conditions
         are good *)
      Util.debugf ~section 5
        "@[<2>div. chaining@ with @[%a@]@ between @[%a@] (at %a)@ and@ @[%a@] (at %a)@]"
        (fun k->k Subst.pp subst C.pp c1 Position.pp pos1 C.pp c2 Position.pp pos2);
      if Z.lt gcd Z.(pow n power)
      && C.is_maxlit (c1,sc1) subst ~idx:idx1
      && C.is_maxlit (c2,sc2) subst ~idx:idx2
      && ALF.is_max ~ord lit1'
      && ALF.is_max ~ord lit2'
      then (
        let new_lit = Lit.mk_divides ~sign n ~power
            (M.difference (MF.rest mf1') (MF.rest mf2'))
        in
        let lits1 = CCArray.except_idx (C.lits c1) idx1
        and lits2 = CCArray.except_idx (C.lits c2) idx2 in
        let lits1 = Lit.apply_subst_list renaming subst (lits1,sc1)
        and lits2 = Lit.apply_subst_list renaming subst (lits2,sc2) in
        let c_guard = Literal.of_unif_subst renaming us in
        let all_lits = new_lit :: c_guard @ lits1 @ lits2 in
        let proof =
          Proof.Step.inference ~tags:[Proof.Tag.T_lia] ~rule:(Proof.Rule.mk "div_chaining")
            [C.proof_parent_subst renaming (c1,sc1) subst;
             C.proof_parent_subst renaming (c2,sc2) subst] in
        let trail = C.trail_l [c1; c2] in
        (* penalize chaining into variables *)
        let penalty =
          C.penalty c1
          + C.penalty c2
          + (if MF.term mf1' |> T.is_var then 10 else 0)
          + (if MF.term mf2' |> T.is_var then 10 else 0)
        in
        let new_c = C.create ~trail ~penalty all_lits proof in
        Util.debugf ~section 5 "@[<4>... gives@ @[%a@]@]" (fun k->k C.pp new_c);
        Util.incr_stat stat_arith_div_chaining;
        new_c :: acc
      ) else (
        Util.debug ~section 5 "... has bad ordering conditions";
        acc
      )
    in
    let res =
      Lits.fold_arith_terms ~eligible ~which:`Max ~ord (C.lits c)
      |> Sequence.fold
        (fun acc (t,lit1,pos1) ->
           match lit1 with
             | ALF.Div d1 when AL.Util.is_prime d1.AL.num ->
               (* inferences only possible when lit1 is a power-of-prime *)
               let n = d1.AL.num in
               PS.TermIndex.retrieve_unifiables (!_idx_div,sc2) (t,sc1)
               |> Sequence.fold
                 (fun acc (_t',with_pos,subst) ->
                    (* [subst t = subst t'], see whether they belong to the same group *)
                    let c2 = with_pos.C.WithPos.clause in
                    let pos2 = with_pos.C.WithPos.pos in
                    let lit2 = Lits.View.get_arith_exn (C.lits c2) pos2 in
                    match lit2 with
                      | ALF.Div d2 when (d1.AL.sign || d2.AL.sign) && Z.equal n d2.AL.num ->
                        (* inference seems possible (at least one lit is positive).
                           start with scaling the literals to the same power *)
                        let sign = d1.AL.sign && d2.AL.sign in
                        let power = max d1.AL.power d2.AL.power in
                        let lit1 = ALF.scale_power lit1 power
                        and lit2 = ALF.scale_power lit2 power in
                        let mf1 = ALF.focused_monome lit1
                        and mf2 = ALF.focused_monome lit2 in
                        (* unify mf1 and mf2 as possible *)
                        MF.unify_ff ~subst (mf1,sc1) (mf2,sc2)
                        |> Sequence.fold
                          (fun acc (_, _, subst) ->
                             _do_chaining ~sign n power c lit1 pos1 c2 lit2 pos2 subst acc)
                          acc
                      | _ -> acc)
                 acc
             | _ -> acc)
        []
    in
    Util.exit_prof prof_arith_div_chaining;
    res

  exception ReplaceLitByLitsInSameClause of int * Lit.t list
  exception ReplaceLitByLitsInManyClauses of int * Lit.t list

  let canc_div_case_switch c =
    let eligible = C.Eligible.(max c ** neg ** filter Lit.is_arith_divides) in
    try
      Lits.fold_arith ~eligible (C.lits c)
      |> Sequence.iter
        (fun (lit,pos) -> match lit with
           | AL.Divides d ->
             assert (not (d.AL.sign));
             let n = d.AL.num and power = d.AL.power in
             (* check that [n] is a not-too-big prime *)
             if Z.gt n Z.one && AL.Util.is_prime n then
               if Z.leq n (Z.of_int !div_case_switch_limit)
               then (
                 let idx = Lits.Pos.idx pos in
                 (* build the list of alternatives *)
                 let lits = ref [] in
                 for e = 0 to power-1 do
                   for i=1 to Z.to_int n - 1 do
                     (* new lit:   n^{e+1} | m + i·n^e *)
                     let m' = M.add_const d.AL.monome
                         Z.((n ** e) * of_int i)
                     in
                     let new_lit = Lit.mk_divides
                         n ~power:(e+1) m'
                     in
                     lits := new_lit :: !lits
                   done
                 done;
                 raise (ReplaceLitByLitsInSameClause (idx, !lits))
               ) else Ctx.lost_completeness ()
             else ()
           | _ -> assert false
        );
      []
    with ReplaceLitByLitsInSameClause (i, lits) ->
      (* replace lit number [i] with [lits] *)
      let lits' = CCArray.except_idx (C.lits c) i in
      let all_lits = List.rev_append lits lits' in
      let proof =
        Proof.Step.inference ~tags:[Proof.Tag.T_lia] ~rule:(Proof.Rule.mk "div_case_switch")
          [C.proof_parent c] in
      let new_c =
        C.create ~trail:(C.trail c) ~penalty:(C.penalty c) all_lits proof
      in
      Util.debugf ~section 5 "@[<2>div_case_switch@ of @[%a@]@ into @[%a@]@]"
        (fun k->k C.pp c C.pp new_c);
      [new_c]

  let _pp_z out z = Z.pp_print out z
  let _pp_div out d =
    Format.fprintf out "%a^%d" _pp_z d.AL.Util.prime d.AL.Util.power

  let canc_div_prime_decomposition c =
    let eligible = C.Eligible.(max c ** filter Lit.is_arith_divides) in
    try
      Lits.fold_arith ~eligible (C.lits c)
      |> Sequence.iter
        (fun (lit,pos) -> match lit with
           | AL.Divides d when d.AL.sign ->
             (* positive "divides" predicate *)
             let n = d.AL.num in
             (* check that [n] is a composite number *)
             if Z.gt n Z.one && not (AL.Util.is_prime n) then (
               let n' = Z.pow n d.AL.power in
               let idx = Lits.Pos.idx pos in
               let divisors = AL.Util.prime_decomposition n' in
               Util.debugf ~section 5 "@[<2>composite num:@ @[%a = %a@]@]"
                 (fun k->k _pp_z n' (Util.pp_list _pp_div) divisors);
               let lits = List.map
                   (fun div -> Lit.mk_divides ~sign:true
                       div.AL.Util.prime ~power:div.AL.Util.power d.AL.monome)
                   divisors
               in
               raise (ReplaceLitByLitsInManyClauses (idx, lits))
             )
           | AL.Divides d ->
             (* negative "divides" predicate *)
             let n = d.AL.num in
             (* check that [n] is a composite number *)
             if Z.gt n Z.one && not (AL.Util.is_prime n) then (
               let n' = Z.pow n d.AL.power in
               let idx = Lits.Pos.idx pos in
               let divisors = AL.Util.prime_decomposition n' in
               Util.debugf ~section 5 "@[<2>composite num:@ @[%a = %a@]@]"
                 (fun k->k _pp_z n' (Util.pp_list _pp_div) divisors);
               assert (List.length divisors >= 2);
               let lits = List.map
                   (fun div -> Lit.mk_divides ~sign:false
                       div.AL.Util.prime ~power:div.AL.Util.power d.AL.monome)
                   divisors
               in
               raise (ReplaceLitByLitsInSameClause (idx, lits))
             )
           | _ -> assert false
        );
      None
    with
      | ReplaceLitByLitsInSameClause (i, lits) ->
        (* replace lit number [i] with [lits] *)
        let lits' = CCArray.except_idx (C.lits c) i in
        let all_lits = List.rev_append lits lits' in
        let proof =
          Proof.Step.inference ~tags:[Proof.Tag.T_lia]
            ~rule:(Proof.Rule.mk "div_prime_decomposition")
            [C.proof_parent c] in
        let new_c =
          C.create ~trail:(C.trail c) ~penalty:(C.penalty c) all_lits proof
        in
        Util.debugf ~section 5
          "@[<2>prime_decomposition- of@ @[%a@]@ into @[%a@]@]"
          (fun k->k C.pp c C.pp new_c);
        Some [new_c]
      | ReplaceLitByLitsInManyClauses (i, lits) ->
        let clauses = List.map
            (fun lit ->
               let all_lits = Array.copy (C.lits c) in
               all_lits.(i) <- lit;
               let proof =
                 Proof.Step.inference ~tags:[Proof.Tag.T_lia]
                   [C.proof_parent c]
                   ~rule:(Proof.Rule.mk "div_prime_decomposition")  in
               let new_c =
                 C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) all_lits proof
               in
               new_c)
            lits
        in
        Util.debugf ~section 5
          "@[<2>prime_decomposition+@ of @[%a@]@ into set {@[%a@]}@]"
          (fun k->k C.pp c (Util.pp_list C.pp) clauses);
        Some clauses

  let canc_divisibility c =
    Util.enter_prof prof_arith_divisibility;
    (* inference on 1/ positive eq  2/ positive divisibility *)
    let eligible = C.Eligible.(max c **
          (filter Lit.is_arith_eq ++ (pos ** filter Lit.is_arith_divides))) in
    let ord = Ctx.ord () in
    let res =
      Lits.fold_arith_terms ~eligible ~which:`Max ~ord (C.lits c)
      |> Sequence.fold
        (fun acc (_t,lit,pos) ->
           let mf = ALF.focused_monome lit in
           let idx = Lits.Pos.idx pos in
           MF.unify_self (mf,0)
           |> Sequence.fold
             (fun acc (_, us) ->
                let renaming = Subst.Renaming.create () in
                let subst = US.subst us in
                let lit' = ALF.apply_subst renaming subst (lit,0) in
                let mf' = ALF.focused_monome lit' in
                (* does the maximal term have a coeff bigger-than-one? *)
                let n = MF.coeff mf' in
                if Z.gt n Z.one
                && C.is_maxlit (c,0) subst ~idx
                && ALF.is_max ~ord lit'
                &&
                (* in case we have a divisibility, only infer if the coefficient
                   of [t] divides [d^k]. In particular it means [n] is a
                   power-of-prime *)
                begin match lit' with
                  | ALF.Div d -> Z.sign (Z.rem (Z.pow d.AL.num d.AL.power) n) = 0
                  | _ -> true
                end
                then (
                  (* do the inference *)
                  Util.debugf ~section 5
                    "@[<2>divisibility@ on @[%a@]@ at @[%a@]@ with @[%a@]...@]"
                    (fun k->k C.pp c Position.pp pos Subst.pp subst);
                  let new_lit = match lit' with
                    | ALF.Left (AL.Equal, mf, m)
                    | ALF.Right (AL.Equal, m, mf) ->
                      (* remove the max term from [mf], and inject into the Z/nZ group *)
                      Lit.mk_divides n ~power:1 (M.difference m (MF.rest mf))
                    | ALF.Div d ->
                      assert d.AL.sign;
                      let n', power' = match AL.Util.prime_decomposition n with
                        | [{AL.Util.prime=n'; AL.Util.power=p}] ->
                          assert (Z.equal n' d.AL.num);
                          assert (p <= d.AL.power);
                          n', p
                        | _ -> assert false
                      in
                      Lit.mk_divides n' ~power:power' (MF.rest d.AL.monome)
                    | _ -> assert false
                  in
                  let lits' = CCArray.except_idx (C.lits c) idx in
                  let lits' = Lit.apply_subst_list renaming subst (lits',0) in
                  let c_guard = Literal.of_unif_subst renaming us in
                  let all_lits = new_lit :: c_guard @ lits' in
                  let proof =
                    Proof.Step.inference ~tags:[Proof.Tag.T_lia]
                      ~rule:(Proof.Rule.mk "divisibility")
                      [C.proof_parent_subst renaming (c,0) subst] in
                  let new_c =
                    C.create ~trail:(C.trail c) ~penalty:(C.penalty c) all_lits proof
                  in
                  Util.debugf ~section 5 "@[<4>... gives@ @[%a@]@]" (fun k->k C.pp new_c);
                  Util.incr_stat stat_arith_divisibility;
                  new_c :: acc
                ) else acc)
             acc)
        []
    in
    Util.exit_prof prof_arith_divisibility;
    res

  (* regular literal ----> arith literal, sometimes *)
  let canc_lit_of_lit lit =
    match lit with
      | Lit.Equation (l, r, sign) when Type.equal Type.int (T.ty l) ->
        begin match T.view l, T.view r with
          | T.AppBuiltin (Builtin.Remainder_e, [l'; r']), opp
          | opp, T.AppBuiltin (Builtin.Remainder_e, [l'; r']) ->
            begin match Monome.Int.of_term l', T.view r', opp with
              | Some m,
                T.AppBuiltin (Builtin.Int n,[]),
                T.AppBuiltin (Builtin.Int opp', [])
                when Z.sign opp' >= 0 && Z.compare opp' n < 0 ->
                (* remainder(l1, n) = opp ----> n | l1-opp,
                   assuming 0<=opp<n *)
                let m = M.add_const m Z.(rem (~- opp') n) in
                let lit = Lit.mk_divides ~sign n ~power:1 m in
                Some (lit,[],[Proof.Tag.T_lia])
              | Some _,
                T.AppBuiltin (Builtin.Int n,[]),
                T.AppBuiltin (Builtin.Int opp', [])
                when Z.sign opp' < 0 || Z.compare opp' n >= 0 ->
                (* remainder(l1, n) = opp --> false
                   assuming opp ∉ [0.. n-1] *)
                let lit = if sign then Lit.mk_absurd else Lit.mk_tauto in
                Some (lit,[],[Proof.Tag.T_lia])
              | _ -> None
            end
          | _ ->
            begin match Monome.Int.of_term l, Monome.Int.of_term r with
              | Some m1, Some m2 ->
                if sign
                then Some (Lit.mk_arith_eq m1 m2,[],[Proof.Tag.T_lia])
                else Some (Lit.mk_arith_neq m1 m2,[],[Proof.Tag.T_lia])
              | _, None
              | None, _-> None
            end
        end
      | _ -> None

  (** {3 Others} *)

  let _has_arith c =
    CCArray.exists Lit.is_arith (C.lits c)

  module Simp = Simplex.MakeHelp(T)

  (* tautology check: take the linear system that is the negation
     of all a≠b and a≤b, and check its (rational) satisfiability. If
     it's unsat in Q, it's unsat in Z, and its negation (a subset of c)
     is tautological *)
  let _is_tautology c =
    Util.enter_prof prof_arith_semantic_tautology;
    (* convert a monome into a rational monome + Q constant *)
    let to_rat m =
      let const = Q.of_bigint (M.const m) in
      List.map (fun (c,t) -> Q.of_bigint c, t) (M.coeffs m), const
    in
    (* create a list of constraints for some arith lits *)
    let constraints =
      Lits.fold_arith ~eligible:C.Eligible.arith (C.lits c)
      |> Sequence.fold
        (fun acc (lit,_) ->
           (* negate the literal and make a constraint out of it *)
           match lit with
             | AL.Binary (AL.Lesseq, m1, m2) ->
               (* m1 ≤ m2 ----> m1-m2 > 0 ---> m1-m2 ≥ 1 *)
               let m, c = to_rat (M.difference m1 m2) in
               (Simp.GreaterEq, m, Q.add (Q.neg c) Q.one) :: acc
             | AL.Binary (AL.Different, m1, m2) ->
               (* m1 != m2  -----> (m1-m2) = 0 *)
               let m, c = to_rat (M.difference m1 m2) in
               (Simp.Eq, m, Q.neg c) :: acc
             | _ -> acc)
        []
    in
    let simplex = Simp.add_constraints Simp.empty constraints in
    Util.exit_prof prof_arith_semantic_tautology;
    match Simp.ksolve simplex with
      | Simp.Unsatisfiable _ -> true (* negation unsatisfiable *)
      | Simp.Solution _ -> false

  (* cache the result because it's a bit expensive *)
  let is_tautology c =
    if C.get_flag flag_computed_tauto c
    then C.get_flag flag_tauto c
    else (
      (* compute whether [c] is an arith tautology *)
      let res = _has_arith c && _is_tautology c in
      C.set_flag flag_tauto c res;
      C.set_flag flag_computed_tauto c true;
      if res then (
        Util.incr_stat stat_arith_semantic_tautology_steps;
        Util.debugf ~section 4
          "@[<2>clause@ @[%a@]@ is an arith tautology@]" (fun k->k C.pp c);
      );
      Util.incr_stat stat_arith_semantic_tautology;
      res
    )

  (* Simplification:  a < b  ----> a+1 ≤ b *)
  let canc_less_to_lesseq = function
    | Lit.Int (AL.Binary (AL.Less, m1, m2)) ->
      Some (Lit.mk_arith_lesseq (M.succ m1) m2, [], [Proof.Tag.T_lia])
    | _ -> None

  exception VarElim of int * S.t

  (* X != Y or C -----> C[X/Y] *)
  let canc_eq_resolution c =
    (* check whether [m] is only one variable with coeff 1 *)
    let is_unary_var m =
      match M.coeffs m with
        | [c, t] ->
          begin match T.view t with
            | T.Var v when Z.(equal c one) && Z.(equal (M.const m) zero) ->
              Some v
            | _ -> None
          end
        | _ -> None
    in
    try
      Lits.fold_arith ~eligible:C.Eligible.(filter Lit.is_arith_neq) (C.lits c)
      |> Sequence.iter
        (fun (lit,pos) ->
           match lit with
             | AL.Binary (AL.Different, m1, m2) ->
               begin match is_unary_var m1, is_unary_var m2 with
                 | Some v1, Some v2 ->
                   let subst =
                     S.FO.bind S.empty
                       ((v1:Type.t HVar.t :> InnerTerm.t HVar.t),0)
                       (T.var v2,0) in
                   let i = Lits.Pos.idx pos in
                   raise (VarElim (i, subst))
                 | _ -> ()
               end
             | _ -> ()
        );
      SimplM.return_same c (* could not simplify *)
    with VarElim (i, subst) ->
      let lits' = CCArray.except_idx (C.lits c) i in
      let renaming = Subst.Renaming.create () in
      let lits' = Lit.apply_subst_list renaming subst (lits',0) in
      let proof =
        Proof.Step.inference ~tags:[Proof.Tag.T_lia]
          ~rule:(Proof.Rule.mk "canc_eq_res")
          [C.proof_parent_subst renaming (c,0) subst] in
      let c' = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) lits' proof in
      Util.debugf ~section 4
        "@[<2>arith_eq_res:@ simplify @[%a@]@ into @[%a@]@]"
        (fun k->k C.pp c C.pp c');
      SimplM.return_new c'

  exception DiffToLesseq of C.t

  let is_singleton_unshielded_var lits (m:_ M.t) : bool =
    Z.sign (M.const m) = 0 &&
    begin match M.coeffs m with
      | [c,t] ->
        Z.equal Z.one c &&
        T.is_var t &&
        (not @@ Literals.is_shielded (T.as_var_exn t) lits)
      | _ -> false
    end

  (* a != b ------> a+1 ≤ b | a ≥ b+1 *)
  let canc_diff_to_lesseq c =
    let eligible = C.Eligible.(filter Lit.is_arith_neq ** max c) in
    try
      Lits.fold_lits ~eligible (C.lits c)
      |> Sequence.iter
        (fun (lit,i) ->
           match lit with
             | Lit.Int (AL.Binary (AL.Different, m1, m2))
               when not (is_singleton_unshielded_var (C.lits c) m1) &&
                    not (is_singleton_unshielded_var (C.lits c) m2) ->
               (* translate [m1 ≠ m2] into [m1 < m2 ∨ m1 > m2],
                  do not do it on a variable that is going to be eliminated. *)
               assert (eligible i lit);
               (*Format.printf
                 "@[<2>lit @[%a [%d]@]@ in @[%a@]@ :is-max %B@ :max_lits %a@]@."
                 Lit.pp lit i C.pp c (Lits.is_max ~ord (C.lits c) i)
                 CCBV.print (Lits.maxlits ~ord @@ C.lits c);*)
               (* FIXME: find why this sometimes fails
                  assert (Lits.is_max ~ord (C.lits c) i); *)
               let lits = CCArray.except_idx (C.lits c) i in
               let new_lits =
                 [ Lit.mk_arith_lesseq (M.succ m1) m2
                 ; Lit.mk_arith_lesseq (M.succ m2) m1
                 ]
               in
               let proof =
                 Proof.Step.inference [C.proof_parent c] ~tags:[Proof.Tag.T_lia]
                   ~rule:(Proof.Rule.mk "arith_diff_to_lesseq") in
               let c' =
                 C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
                   (new_lits @ lits) proof
               in
               Util.debugf ~section 5 "@[<2>diff2less:@ @[%a@]@ into @[%a@]@]"
                 (fun k->k C.pp c C.pp c');
               raise (DiffToLesseq c')
             | Lit.Int (AL.Binary (AL.Different, _, _)) -> ()
             | _ -> assert false
        );
      SimplM.return_same c
    with DiffToLesseq c ->
      SimplM.return_new c

  (* inference rule corresponding to {!canc_diff_to_lesseq} *)
  let canc_diff_imply_lesseq c =
    let c, st = canc_diff_to_lesseq c in
    match st with
      | `New -> [c]
      | `Same -> []

  (** {6 Variable Elimination Procedure} *)

  let naked_vars lits =
    Literals.unshielded_vars lits
      ~filter:(fun var -> Type.equal (HVar.ty var) Type.int)

  (** Description of a clause, focused around the elimination
      of some variable x *)
  module NakedVarElim = struct
    type t = {
      rest : Literal.t list;  (* x doesn't occur in rest *)
      x : Type.t HVar.t; (* variable to eliminate *)
      a_lit : ALF.t list;  (* c.x + m1 = m2 *)
      b_lit : ALF.t list;  (* c.x + m1 != m2 *)
      c_lit : ALF.t list;  (* n | m_c.x + m  *)
      d_lit : ALF.t list;  (* n not| m_c.x + m *)
      e_lit : ALF.t list;  (* c.x + m1 ≤ m2 *)
      f_lit : ALF.t list;  (* m1 ≤ c.x + m2 *)
      lcm : Z.t; (* scaling coefficient (divisibility guard) *)
      delta : Z.t; (* lcm of all divisibility constraints *)
    }

    let _empty x = {
      rest = []; x; a_lit = []; b_lit = []; c_lit = [];
      d_lit = []; e_lit = []; f_lit = []; lcm=Z.one; delta=Z.one;
    }

    let _lits c k =
      List.iter k c.a_lit;
      List.iter k c.b_lit;
      List.iter k c.c_lit;
      List.iter k c.d_lit;
      List.iter k c.e_lit;
      List.iter k c.f_lit;
      ()

    let map f c =
      {c with
         a_lit = List.map f c.a_lit;
         b_lit = List.map f c.b_lit;
         c_lit = List.map f c.c_lit;
         d_lit = List.map f c.d_lit;
         e_lit = List.map f c.e_lit;
         f_lit = List.map f c.f_lit;
      }

    (* map literals to 'a option, and return a list of 'a *)
    let map_lits f c =
      List.concat
        [ List.map f c.a_lit
        ; List.map f c.b_lit
        ; List.map f c.c_lit
        ; List.map f c.d_lit
        ; List.map f c.e_lit
        ; List.map f c.f_lit
        ]

    (* reduce all occurrences of [x] to the same coefficient (their LCM).
       then, pretend we replace LCM.x with x, so all the coefficients
       become 1 (but the monomes keep their multiplicative factors!) *)
    let scale c =
      let lcm, delta = _lits c
                       |> Sequence.fold
                         (fun (lcm,delta) lit ->
                            let lcm = Z.lcm lcm (ALF.focused_monome lit |> MF.coeff) in
                            let delta = match lit with
                              | ALF.Div d -> Z.lcm delta (Z.pow d.AL.num d.AL.power)
                              | _ -> delta
                            in lcm, Z.lcm lcm delta
                         ) (Z.one,Z.one)
      in
      assert (Z.geq delta lcm);
      if Z.equal lcm Z.one then delta, {c with delta; }
      else
        let c' = map
            (fun lit ->
               let n = Z.divexact lcm (ALF.focused_monome lit |> MF.coeff) in
               (* scale monome by n *)
               ALF.product lit n
            )
            c
        in
        let c' = {c' with lcm; delta; } in
        delta, c'

    (* make from clause *)
    let of_lits lits x =
      Array.fold_left
        (fun acc lit -> match lit with
           | Lit.Int o when Lit.var_occurs x lit ->
             (* one of the literals [x] occurs in! classify it, but
                remember that we need to {b negate} it first. *)
             begin match AL.Focus.focus_term (AL.negate o) (T.var x) with
               | None -> assert false
               | Some (ALF.Left (AL.Equal, _, _) as lit)
               | Some (ALF.Right (AL.Equal, _, _) as lit) ->
                 { acc with a_lit = lit::acc.a_lit; }
               | Some (ALF.Left (AL.Different, _, _) as lit)
               | Some (ALF.Right (AL.Different, _, _) as lit) ->
                 { acc with b_lit = lit::acc.b_lit; }
               | Some (ALF.Div d as lit) when d.AL.sign ->
                 { acc with c_lit = lit::acc.c_lit; }
               | Some (ALF.Div d as lit) ->
                 assert (not(d.AL.sign));
                 { acc with d_lit = lit::acc.d_lit; }
               | Some (ALF.Left (AL.Lesseq, _, _) as lit) ->
                 { acc with e_lit = lit::acc.e_lit; }
               | Some (ALF.Left (AL.Less, mf1, m2)) ->
                 (* mf1 < m2 ------> mf1 ≤ m2-1 *)
                 let lit = ALF.Left (AL.Lesseq, mf1, M.pred m2) in
                 { acc with e_lit = lit::acc.e_lit; }
               | Some (ALF.Right (AL.Lesseq, _, _) as lit) ->
                 { acc with f_lit = lit::acc.f_lit; }
               | Some (ALF.Right (AL.Less, m1, mf2)) ->
                 (* m1 < mf2 -----> m1+1 ≤ mf2 *)
                 let lit = ALF.Right (AL.Lesseq, M.succ m1, mf2) in
                 { acc with f_lit = lit::acc.f_lit; }
             end
           | _ ->
             { acc with rest=lit::acc.rest; })
        (_empty x) lits

    (* higher bounds *)
    let a_set c =
      List.concat
        [ List.map (function
              | ALF.Left (AL.Equal, mf, m)
              | ALF.Right (AL.Equal, m, mf) -> M.difference (M.succ m) (MF.rest mf)
              | _ -> assert false)
              c.a_lit
        ; List.map (function
              | ALF.Left (AL.Different, mf, m)
              | ALF.Right (AL.Different, m, mf) -> M.difference m (MF.rest mf)
              | _ -> assert false)
              c.b_lit
        ; List.map (function
              | ALF.Left (AL.Lesseq, mf, m) -> M.difference (M.succ m) (MF.rest mf)
              | _ -> assert false)
              c.e_lit
        ]

    let b_set c =
      List.concat
        [ List.map (function
              | ALF.Left (AL.Equal, mf, m)
              | ALF.Right (AL.Equal, m, mf) -> M.difference (M.pred m) (MF.rest mf)
              | _ -> assert false)
              c.a_lit
        ; List.map (function
              | ALF.Left (AL.Different, mf, m)
              | ALF.Right (AL.Different, m, mf) -> M.difference m (MF.rest mf)
              | _ -> assert false)
              c.b_lit
        ; List.map (function
              | ALF.Right (AL.Lesseq, m, mf) -> M.difference (M.pred m) (MF.rest mf)
              | _ -> assert false)
              c.f_lit
        ]

    (* evaluate when the variable is equal to x *)
    let eval_at c x =
      Lit.mk_divides ~power:1 c.lcm x ::
        map_lits
          (function
            | ALF.Left (op, mf, m) ->
              Lit.mk_arith_op op (M.sum (MF.rest mf) x) m
            | ALF.Right (op, m, mf) ->
              Lit.mk_arith_op op m (M.sum (MF.rest mf) x)
            | ALF.Div d ->
              Lit.mk_divides ~sign:d.AL.sign d.AL.num
                ~power:d.AL.power (M.sum (MF.rest d.AL.monome) x)
          ) c

    (* evaluate when the variable is equal to x, but as small as needed.
        Many literals will become true or false *)
    let eval_minus_infty c x =
      Lit.mk_divides ~power:1 c.lcm x ::
        map_lits
          (function
            | ALF.Left (AL.Different, _, _)
            | ALF.Right (AL.Different, _, _)
            | ALF.Left (AL.Lesseq, _, _) -> Lit.mk_tauto
            | ALF.Left (AL.Equal, _, _)
            | ALF.Right (AL.Equal, _, _)
            | ALF.Right (AL.Lesseq, _, _) -> Lit.mk_absurd
            | ALF.Div d ->
              Lit.mk_divides ~sign:d.AL.sign d.AL.num
                ~power:d.AL.power (M.sum (MF.rest d.AL.monome) x)
            | ALF.Left (AL.Less, _, _) | ALF.Right (AL.Less, _, _) -> assert false
          ) c

    (* evaluate when the variable is equal to x, but as big as needed.
        Many literals will become true or false *)
    let eval_plus_infty c x =
      Lit.mk_divides ~power:1 c.lcm x ::
        map_lits
          (function
            | ALF.Left (AL.Different, _, _)
            | ALF.Right (AL.Different, _, _)
            | ALF.Right (AL.Lesseq, _, _) -> Lit.mk_tauto
            | ALF.Left (AL.Equal, _, _)
            | ALF.Right (AL.Equal, _, _)
            | ALF.Left (AL.Lesseq, _, _) -> Lit.mk_absurd
            | ALF.Div d ->
              Lit.mk_divides ~sign:d.AL.sign d.AL.num
                ~power:d.AL.power (M.sum (MF.rest d.AL.monome) x)
            | ALF.Left (AL.Less, _, _) | ALF.Right (AL.Less, _, _) -> assert false
          ) c
  end

  let _negate_lits = List.map Lit.negate

  let eliminate_unshielded c =
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
            let infos = UntypedAST.A.([
                app "var_elim"
                  [quoted (Z.to_string view.NVE.lcm);
                   quoted (HVar.to_string_tstp x);
                   quoted (CCFormat.to_string M.pp by);
                   quoted which]
              ])
            in
            (* TODO: use substitution (for ∞ cases just take sth high enough) *)
            let rule = Proof.Rule.mkf "var_elim(%a)" T.pp_var x in
            let proof = Proof.Step.inference ~tags:[Proof.Tag.T_lia] ~infos ~rule
                [C.proof_parent c] in
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
    Util.debug ~section 2 "arith: setup env";
    (* add inference rules *)
    Env.add_binary_inf "canc_sup_active" canc_sup_active;
    Env.add_binary_inf "canc_sup_passive" canc_sup_passive;
    Env.add_unary_inf "cancellation" cancellation;
    Env.add_unary_inf "canc_eq_factoring" canc_equality_factoring;
    Env.add_binary_inf "canc_ineq_chaining" canc_ineq_chaining;
    Env.add_unary_inf "canc_ineq_factoring" canc_ineq_factoring;
    Env.add_binary_inf "div_chaining" canc_div_chaining;
    Env.add_unary_inf "divisibility" canc_divisibility;
    Env.add_unary_inf "div_case_switch" canc_div_case_switch;
    Env.add_multi_simpl_rule canc_div_prime_decomposition;
    Env.add_multi_simpl_rule eliminate_unshielded;
    Env.add_lit_rule "canc_lit_of_lit" canc_lit_of_lit;
    Env.add_lit_rule "less_to_lesseq" canc_less_to_lesseq;
    (* transformation ≠ to ≤ *)
    begin match !diff_to_lesseq_ with
      | `Simplify -> Env.add_unary_simplify canc_diff_to_lesseq
      | `Inf -> Env.add_unary_inf "canc_diff_imply_lesseq" canc_diff_imply_lesseq
    end;
    Env.add_basic_simplify canc_eq_resolution;
    Env.add_unary_simplify canc_demodulation;
    Env.add_backward_simplify canc_backward_demodulation;
    Env.add_is_trivial is_tautology;
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
let k_has_arith = Flex_state.create_key ()

let extension =
  let env_action env =
    let module E = (val env : Env.S) in
    if E.flex_get k_should_register then (
      let module I = Make(E) in
      I.register ()
    ) else if E.flex_get k_has_arith then (
      (* arith not enabled, so we cannot solve the problem, do not answer "sat" *)
      E.Ctx.lost_completeness ();
    )
  and post_typing_action stmts state =
    let module PT = TypedSTerm in
    let has_int =
      CCVector.to_seq stmts
      |> Sequence.flat_map Stmt.Seq.to_seq
      |> Sequence.flat_map
        (function
          | `ID _ -> Sequence.empty
          | `Ty ty -> Sequence.return ty
          | `Form t
          | `Term t -> PT.Seq.subterms t |> Sequence.filter_map PT.ty)
      |> Sequence.exists (PT.Ty.equal PT.Ty.int)
    in
    let should_reg = !enable_arith_ && has_int in
    Util.debugf ~section 2 "decision to register arith: %B" (fun k->k should_reg);
    state
    |> Flex_state.add k_should_register should_reg
    |> Flex_state.add k_has_arith has_int
  in
  { Extensions.default with Extensions.
                         name="arith_int";
                         post_typing_actions=[post_typing_action];
                         env_actions=[env_action];
  }

let () =
  Params.add_opts
    [ "--no-int-semantic-tauto"
    , Arg.Clear enable_semantic_tauto_
    , " disable integer arithmetic semantic tautology check"
    ; "--int-trivial-ineq"
    , Arg.Set enable_trivial_ineq_
    , " enable integer inequality triviality checking by rewriting"
    ; "--no-int-trivial-ineq"
    , Arg.Clear enable_trivial_ineq_
    , " disable integer inequality triviality checking by rewriting"
    ; "--int-demod-ineq"
    , Arg.Set enable_demod_ineq_
    , " enable integer demodulation of inequalities"
    ; "--no-int-demod-ineq"
    , Arg.Clear enable_demod_ineq_
    , " disable integer demodulation of inequalities"
    ; "--int-arith"
    , Arg.Set enable_arith_
    , " enable axiomatic integer arithmetic"
    ; "--no-int-arith"
    , Arg.Clear enable_arith_
    , " disable axiomatic integer arithmetic"
    ; "--int-ac"
    , Arg.Set enable_ac_
    , " enable AC axioms for integer arithmetic (sum)"
    ; "--dot-int-unit"
    , Arg.String (fun s -> dot_unit_ := Some s)
    , " print arith-int-unit index into file"
    ; "--int-inf-diff-to-lesseq"
    , Arg.Unit (fun () -> diff_to_lesseq_ := `Inf)
    , " ≠ → ≤ as inference"
    ; "--int-simp-diff-to-lesseq"
    , Arg.Unit (fun () -> diff_to_lesseq_ := `Simplify)
    , " ≠ → ≤ as simplification"
    ];
  ()
