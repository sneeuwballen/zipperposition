
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Induction Through QBF} *)

module Sym = Logtk.Symbol
module T = Logtk.FOTerm
module Ty = Logtk.Type
module Su = Logtk.Substs
module Util = Logtk.Util
module Lits = Literals

module type S = sig
  module Env : Env.S
  module Ctx : module type of Env.Ctx

  val scan : Env.C.t Sequence.t -> unit
  val register : unit -> unit
end

let ind_types_ = ref []
let cover_set_depth_ = ref 1

let prof_encode_qbf = Util.mk_profiler "ind.encode_qbf"

let section = Util.Section.make ~parent:Const.section "ind"
let section_qbf = Util.Section.make
  ~parent:section ~inheriting:[BoolSolver.section; BBox.section] "qbf"

(* is [s] a constructor symbol for some inductive type? *)
let is_constructor_ s = match s with
  | Sym.Cst info ->
      let name = info.Sym.cs_name in
      List.exists (fun (_, cstors) -> List.mem name cstors) !ind_types_
  | _ -> false

module Make(Sup : Superposition.S)
           (Solver : BoolSolver.QBF) =
struct
  module Env = Sup.Env
  module Ctx = Env.Ctx

  module C = Env.C
  module CI = Ctx.Induction
  module CCtx = ClauseContext
  module BoolLit = Ctx.BoolLit
  module Avatar = Avatar.Make(Env)(Solver)  (* will use some inferences *)

  (** Map that is subsumption-aware *)
  module FVMap(X : sig type t end) = struct
    module FV = Logtk.FeatureVector.Make(struct
      type t = Lits.t * X.t
      let cmp (l1,_)(l2,_) = Lits.compare l1 l2
      let to_lits (l,_) = Lits.Seq.abstract l
    end)

    type t = FV.t

    let empty () = FV.empty ()

    let add fv lits x = FV.add fv (lits,x)

    (*
    let remove fv lits = FV.remove fv lits
    *)

    let find fv lits =
      FV.retrieve_alpha_equiv fv (Lits.Seq.abstract lits) ()
        |> Sequence.map2 (fun _ x -> x)
        |> Sequence.filter_map
          (fun (lits', x) ->
            if Lits.are_variant lits lits'
              then Some x else None
          )
        |> Sequence.head

    (* find clauses in [fv] that are subsumed by [lits] *)
    let find_subsumed_by fv lits =
      FV.retrieve_subsumed fv (Lits.Seq.abstract lits) ()
        |> Sequence.map2 (fun _ x -> x)
        |> Sequence.filter
          (fun (lits', x) -> Sup.subsumes lits lits')

    let to_seq = FV.iter

    (*
    (* find clauses in [fv] that subsume [lits] *)
    let find_subsuming fv lits =
      FV.retrieve_subsuming fv (Lits.Seq.abstract lits) ()
        |> Sequence.map2 (fun _ x -> x)
        |> Sequence.filter
          (fun (lits', x) -> Sup.subsumes lits' lits)
    *)
  end

  let level0_ = Solver.level0
  let level1_ = Solver.push Qbf.Forall []
  let level2_ = Solver.push Qbf.Exists []

  let pp_bclause buf c =
    CCList.pp ~start:"[" ~stop:"]" ~sep:" ⊔ " BoolLit.pp buf c

  (* TODO: when discover new clause with inductive cst, add "input" to its trail *)

  (* a candidate clause context *)
  type candidate_context = {
    cand_ctx : CCtx.t; (* the ctx itself *)
    cand_cst : CI.cst;    (* the inductive constant *)
    cand_lits : Lits.t; (* literals of [ctx[t]] *)
    mutable cand_explanations : Proof.t list
      [@compare (fun _ _ -> 0)]; (* justification(s) of why the context exists *)
  } [@@deriving ord]

  module CandidateCtxSet = Sequence.Set.Make(struct
    type t = candidate_context
    let compare = compare_candidate_context
  end)

  (* if true, ignore clause when it comes to extracting contexts *)
  let flag_no_ctx = C.new_flag ()

  (* if true, means the clause represents the possibility of another
    clause being the witness of the minimality of the current model *)
  let flag_expresses_minimality = C.new_flag ()

  (** {6 Split on Inductive Constants} *)

  (* true if [t = c] where [c] is some inductive constructor such as "cons" or "node" *)
  let is_a_constructor_ t = match T.Classic.view t with
    | T.Classic.App (s, _, _) ->
        Sequence.exists (Sym.eq s) CI.Seq.constructors
    | _ -> false

  (* find inductive constants in clauses of [seq] *)
  let find_inductive_cst (seq:C.t Sequence.t) : T.t Sequence.t =
    seq
    |> Sequence.flat_map (fun c -> Lits.Seq.terms (Env.C.lits c))
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.filter
      (fun t ->
        T.is_ground t
        && T.is_const t
        && not (CI.is_blocked t)
        && CI.is_inductive_type (T.ty t)
        && not (is_a_constructor_ t)   (* 0 and nil: not inductive const *)
          )

  (* scan clauses for ground terms of an inductive type, and declare those terms *)
  let scan (seq:C.t Sequence.t) =
    Sequence.iter CI.declare (find_inductive_cst seq)

  (* boolean xor *)
  let mk_xor_ l =
    let at_least_one = l in
    let at_most_one =
      CCList.diagonal l
        |> List.map (fun (l1,l2) -> [BoolLit.neg l1; BoolLit.neg l2])
    in
    at_least_one :: at_most_one

  let new_cover_sets : (CI.cst * CI.cover_set) list ref = ref []

  let () =
    Signal.on CI.on_new_cover_set
      (fun (cst,set) ->
        new_cover_sets := (cst,set) :: !new_cover_sets;
        Signal.ContinueListening
      );
    ()

  (* detect ground terms of an inductive type, and perform a special
      case split with Xor on them. *)
  let case_split_ind c =
    let res = ref [] in
    (* first scan for new inductive consts *)
    scan (Sequence.singleton c);
    Lits.Seq.terms (Env.C.lits c)
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.filter_map CI.is_inductive
      |> Sequence.iter
        (fun t ->
          match CI.cover_set ~depth:!cover_set_depth_ t with
          | _, `Old -> ()
          | set, `New ->
              (* Make a case split on the cover set (one clause per literal) *)
              Util.debugf ~section 2 "make a case split on inductive %a" CI.pp_cst t;
              let clauses_and_lits = List.map
                (fun t' ->
                  assert (T.is_ground t');
                  let lits = [| Literal.mk_eq (t:CI.cst:>T.t) t' |] in
                  let bool_lit = BoolLit.inject_lits lits in
                  Solver.quantify_lits level2_ [bool_lit];
                  let proof cc = Proof.mk_c_trivial
                    ~theories:["induction"] ~info:["boolean split"] cc in
                  let trail = C.Trail.of_list [bool_lit] in
                  let clause = C.create_a ~trail lits proof in
                  C.set_flag flag_no_ctx clause true; (* no context from split *)
                  clause, bool_lit
                ) set.CI.cases
              in
              let clauses, bool_lits = List.split clauses_and_lits in
              (* add a boolean constraint: Xor of boolean lits *)
              Solver.add_clauses (mk_xor_ bool_lits);
              (* return clauses *)
              Util.debugf ~section 4 "split inference for %a: %a"
                CI.pp_cst t (CCList.print C.fmt) clauses;
              res := List.rev_append clauses !res
        );
    !res

  (* TODO: add presence of "input" and "C[] in Sloop" as redundant *)
  (* checks whether the trail of [c] contains two literals [i = t1]
      and [i = t2] with [t1], [t2] distinct cover set members, or two
      literals [loop(i) minimal by a] and [loop(i) minimal by b]. *)
  let has_trivial_trail c =
    let trail = C.get_trail c |> C.Trail.to_seq in
    (* all i=t where i is inductive *)
    let relevant_cases = trail
      |> Sequence.filter_map
        (fun blit ->
          let sign = BoolLit.sign blit in
          match BoolLit.extract (BoolLit.abs blit) with
          | None -> None
          | Some (BoolLit.Clause_component [| Literal.Equation (l, r, true) |]) ->
              begin match CI.is_inductive l, CI.is_inductive r with
              | Some l, _ when sign -> Some (`Case (l, r))
              | None, Some r when sign -> Some (`Case (r, l))
              | _ -> None
              end
          | Some (BoolLit.Ctx (ctx, n, BoolLit.ExpressesMinimality t) as lit) ->
              Some (`Minimal (ctx, n, lit, t))
          | Some _ -> None
        )
    in
    (* is there i such that   i=t1 and i=t2 can be found in the trail? *)
    Sequence.product relevant_cases relevant_cases
      |> Sequence.exists
        (function
          | (`Case (i1, t1), `Case (i2, t2)) ->
              let res = CI.equal_cst i1 i2 && not (T.eq t1 t2) in
              if res
              then Util.debugf ~section 4
                "clause %a redundant because of %a={%a,%a} in trail"
                C.fmt c CI.pp_cst i1 T.fmt t1 T.fmt t2;
              res
          | (`Minimal (ctx1, i1, lit1, t1), `Minimal (ctx2, i2, lit2, t2)) ->
              let res = CI.equal_cst i1 i2
              && not (ClauseContext.equal ctx1 ctx2)
              && T.eq t1 t2
              in
              if res
              then Util.debug ~section 4
                "clause %a redundant because %a and %a both in trail"
                C.pp c BoolLit.pp_injected lit1 BoolLit.pp_injected lit2;
              res
          | _ -> false
        )

  (* transform a lit a=b where a,b have different inductive constructors to false *)
  let injectivity_simple lit = match lit with
    | Literal.Equation (l, r, true) ->
        begin match T.head l, T.head r with
          | Some s1, Some s2
            when CI.is_constructor_sym s1 && CI.is_constructor_sym s2 ->
            (* two inductive constructors! *)
            if Sym.eq s1 s2
              then lit (* keep it *)
              else Literal.mk_absurd
          | _ -> lit
        end
    | Literal.Equation (_, _, false)
    | Literal.True  | Literal.False  | Literal.Prop (_,_)
    | Literal.Ineq _ | Literal.Arith _ -> lit

  exception FoundInductiveLit of int * (T.t * T.t) list

  (* if c is  f(t1,...,tn) != f(t1',...,tn') or d, with f inductive symbol, then
      replace c with    t1 != t1' or ... or tn != tn' or d *)
  let injectivity_destruct c =
    try
      let eligible = C.Eligible.(filter Literal.is_neq) in
      Lits.fold_lits ~eligible (C.lits c) ()
        (fun () lit i -> match lit with
          | Literal.Equation (l, r, false) ->
              begin match T.Classic.view l, T.Classic.view r with
              | T.Classic.App (s1, _, l1), T.Classic.App (s2, _, l2)
                when Sym.eq s1 s2
                && CI.is_constructor_sym s1
                ->
                  (* destruct *)
                  assert (List.length l1 = List.length l2);
                  let pairs = List.combine l1 l2 in
                  raise (FoundInductiveLit (i, pairs))
              | _ -> ()
              end
          | _ -> ()
        );
      c (* nothing happened *)
    with FoundInductiveLit (idx, pairs) ->
      let lits = CCArray.except_idx (C.lits c) idx in
      let new_lits = List.map (fun (t1,t2) -> Literal.mk_neq t1 t2) pairs in
      let proof cc = Proof.mk_c_inference ~theories:["induction"]
        ~rule:"injectivity_destruct" cc [C.proof c]
      in
      let c' = C.create ~trail:(C.get_trail c) ~parents:[c] (new_lits @ lits) proof in
      Util.debug ~section 3 "injectivity: simplify %a into %a" C.pp c C.pp c';
      c'

  (** {6 Clause Contexts}

  at any point, we have a set of clause contexts "of interest", that is,
  that might be used for induction. For any context [c], inductive [i]
  with subcases [t_1,...,t_n], we watch proofs of [c[i]], [c[t_1]], ... [c[t_n]].
  If [c[i]] has a proof, then [c] will be candidate for induction in
  the QBF formula.
  *)

  module FV_cand = FVMap(struct
    type t = CandidateCtxSet.t ref
  end)
  module ClauseContextMap = Sequence.Map.Make(ClauseContext)
  module CstTbl = CCHashtbl.Make(struct
    type t = CI.cst
    let equal = CI.equal_cst
    let hash = CI.hash_cst
  end)

  (* maps each inductive constant to
      set(clause contexts that are candidate for induction on this constant) *)
  let candidates_by_icst_
    : candidate_context ClauseContextMap.t ref CstTbl.t
    = CstTbl.create 16
  let candidates_by_idx_ = ref (FV_cand.empty ())

  (* candidates for a term *)
  let find_candidates_for_cst_ (t:CI.cst) =
    try CstTbl.find candidates_by_icst_ t
    with Not_found ->
      let set = ref ClauseContextMap.empty in
      CstTbl.add candidates_by_icst_ t set;
      set

  (* candidates for some [lits] *)
  let find_candidates_for_lits_ lits =
    match FV_cand.find !candidates_by_idx_ lits with
    | Some set -> set
    | None ->
        let set = ref CandidateCtxSet.empty in
        candidates_by_idx_ := FV_cand.add !candidates_by_idx_ lits set;
        set

  let new_contexts : candidate_context list ref = ref []

  let on_new_context =
    let s = Signal.create () in
    Signal.on s (fun ctx ->
      (* new context:
        - assert [not l] for all [t] a sub-constant of [i] and [l]
            literal of [ctx[t]], to express the minimality *)
      Util.debugf ~section 3 "watch new context %a@ (for inductive %a)"
        Lits.fmt ctx.cand_lits CI.pp_cst ctx.cand_cst;
      new_contexts := ctx :: !new_contexts;
      Signal.ContinueListening
    );
    s

  (* add a candidate context *)
  let add_ctx ctx =
    let set = find_candidates_for_lits_ ctx.cand_lits in
    set := CandidateCtxSet.add ctx !set;
    let map = find_candidates_for_cst_ ctx.cand_cst in
    map := ClauseContextMap.add ctx.cand_ctx ctx !map;
    Signal.send on_new_context ctx;
    ()

  (* [t] is an inductive const; this returns [true] iff some subconstant of [t]
      occurs in [c] *)
  let contains_any_sub_constant_of c (t:CI.cst) =
    Lits.Seq.terms (Env.C.lits c)
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.exists
      (fun t' ->
        T.is_ground (t:>T.t)
        && CI.is_sub_constant t'
        && CI.is_sub_constant_of t' t
      )

  (* [t] is a sub-constant; neither its parent nor its siblings must be
      present in [c] *)
  let contains_only_sub_constant_ c t =
    assert (CI.is_sub_constant t);
    let cst, _ = CI.inductive_cst_of_sub_cst t in
    let others =
      Lits.Seq.terms (Env.C.lits c)
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.exists
        (fun t' ->
          T.is_ground t
          && (T.eq t' (cst:>T.t)
            ||
            (CI.is_sub_constant t'
            && not (T.eq t' t)
            && CI.is_sub_constant_of t' cst
            )
          )
        )
    in
    not others

  (* set of subterms of [lits] that could be extruded to form a context.
     XXX restrictions:
       - a context can be extracted only from an inductive constant
         such that no sub-constant of it occurs in the clause; otherwise
         it would be meaningless to express the minimality of the clause
      - a context can also be extracted from a sub-constant if its parent
        nor its siblings are present in the clause
  *)
  let subterms_candidates_for_context_ c =
    Lits.Seq.terms (C.lits c)
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.filter
        (fun t ->
          T.is_ground t
          && not (C.get_flag flag_expresses_minimality c)
          &&
          (
            match CI.is_inductive t with
            | Some t -> not (contains_any_sub_constant_of c t)
            | None ->
                CI.is_sub_constant t
                && contains_only_sub_constant_ c t
          )
        )
      |> T.Seq.add_set T.Set.empty

  (* see whether (ctx,t) is of interest. Returns a new clause if it is, the
    clause being an induction hypothesis for the given context *)
  let process_ctx_ c ctx t =
    (* if [t] is an inductive constant, ctx is enabled! *)
    let is_new = match CI.is_inductive t with
    | Some t ->
      let map = find_candidates_for_cst_ t in
      begin try
        let cand = ClauseContextMap.find ctx !map in
        let expl = C.proof c in
        cand.cand_explanations <- expl :: cand.cand_explanations;
        `Old
      with Not_found ->
        let cand = {
          cand_ctx=ctx;
          cand_cst=t;
          cand_lits=C.lits c;
          cand_explanations=[C.proof c];
        } in
        add_ctx cand;
        `New t
      end
    | None ->
      (* [t] is a subterm of the case [t'] of an inductive [cst] *)
      let cst, t' = CI.inductive_cst_of_sub_cst t in
      let map = find_candidates_for_cst_ cst in
      let lits' = CCtx.apply ctx (cst:>T.t) in
      try
        let _ = ClauseContextMap.find ctx !map in
        `Old (* no new context *)
      with Not_found ->
        let cand_ctx = {
          cand_ctx=ctx;
          cand_cst=cst;
          cand_lits=lits'; (* ctx[cst] *)
          cand_explanations=[C.proof c];
        } in
        (* need to watch ctx[cst] until it is proved *)
        add_ctx cand_ctx;
        `New cst
    in
    match is_new with
    | `New icst ->
        (* new clause for induction hypothesis (on inductive constant [icst]) *)
        let proof = Proof.mk_c_trivial ~info:["ind.hyp."] ~theories:["ind"] in
        let trail = C.Trail.of_list
          [ BoolLit.inject_ctx ctx icst BoolLit.InLoop
          ; BoolLit.inject_ctx ctx icst BoolLit.InitOk ]
        in
        let c' = C.create_a ~trail (C.lits c) proof in
        Util.debug ~section 2 "new induction hyp. %a" C.pp c';
        (* TODO: move clause creation in event handler? *)
        (* TODO: put minimality witness here too? *)
        Some c'
    | `Old -> None

  (* search whether given clause [c] contains some "interesting" occurrences
     of an inductive  term. This is pretty much the main heuristic. *)
  let scan_given_for_context c =
    if C.get_flag flag_no_ctx c then [] else (
    let terms = subterms_candidates_for_context_ c in
    T.Set.fold
      (fun t acc ->
        (* extract a context [c = ctx[t]] *)
        let lits = C.lits c in
        let ctx = CCtx.extract_exn lits t in
        match process_ctx_ c ctx t with
          | None -> acc
          | Some c -> c :: acc
      ) terms []
    )

  (* [c] subsumes [lits'] which is watched with set of contexts [set].
    - add "[init(ctx,i)]" <- c.trail for each (ctx,i) in set
      (remove "input" from c.trail first)
  *)
  let _process_clause_match_watched c lits' set =
    (* check whether that makes some cand_ctx initialized *)
    CandidateCtxSet.iter
      (fun cand ->
        (* the "guard": assume c.trail, but after "input" is removed *)
        let trail_guard = C.get_trail c
          |> C.Trail.remove BoolLit.inject_input
          |> C.Trail.to_list
          |> List.map BoolLit.neg
        in
        let bclause = (
          BoolLit.inject_ctx cand.cand_ctx cand.cand_cst BoolLit.InitOk
        ) :: trail_guard in
        Solver.add_clause bclause;
        Util.debug ~section 3 "add bool constraint %a" pp_bclause bclause;
      ) set

  (* search whether [c] subsumes some watched clause
     if [C[i]] subsumed, where [i] is an inductive constant:
      - "infer" the clause [C[i]]
      - monitor for [C[i']] for every [i'] sub-case of [i]
        - if such [C[i']] is detected, add its proof relation to QBF. Will
          be checked by avatar.check_satisfiability. Watch for proofs
          of [C[i']], they could evolve!! *)
  let scan_given_for_proof c =
    let lits = C.lits c in
    if Array.length lits = 0
    then [] (* proofs from [false] aren't interesting *)
    else (
      FV_cand.find_subsumed_by !candidates_by_idx_ lits
      |> Sequence.iter
        (fun (lits', set) ->
          _process_clause_match_watched c lits' !set
        );
      []
    )

  (* scan the set of active clauses, to see whether it proves some
      of the candidate contexts' initial form in [new_contexts] *)
  let scan_backward () =
    let l = !new_contexts in
    new_contexts := [];
    List.iter
      (fun ctx ->
        let idx = Sup.idx_fv () in
        let lits = Lits.Seq.abstract ctx.cand_lits in
        (* find whether some active clause subsumes [ctx] *)
        Sup.PS.SubsumptionIndex.retrieve_subsuming idx lits ()
          (fun () c ->
            if not (C.is_empty c) && Sup.subsumes (C.lits c) ctx.cand_lits
              then
                _process_clause_match_watched
                  c ctx.cand_lits (CandidateCtxSet.singleton ctx)
          )
      ) l;
    [] (* no new clause *)

  (** {6 Encoding to QBF} *)

  let candidates_for_ cst : candidate_context Sequence.t =
    !(find_candidates_for_cst_ cst)
    |> ClauseContextMap.values

  (* sub-constants of a term *)
  let sub_constants_ cst : T.t Sequence.t = assert false (* TODO *)

  let neg_ = BoolLit.neg
  let valid_ x = BoolLit.inject_name' "valid(%s)" (CI.show_cst x)
  let empty_ x = BoolLit.inject_name' "empty(%s)" (CI.show_cst x)
  let minimal_ x y = BoolLit.inject_name' "minimal(%s,%a)" (CI.show_cst x) T.pp y

  let expresses_minimality_ ctx cst t =
    BoolLit.inject_ctx ctx cst (BoolLit.ExpressesMinimality t)

  let is_true_ lits = BoolLit.inject_lits lits
  let is_eq_ a b = is_true_ [| Literal.mk_eq (a:CI.cst:>T.t) b |]
  let trail_ok_ lits = BoolLit.inject_lits_pred lits BoolLit.TrailOk
  let in_loop_ ctx i = BoolLit.inject_ctx ctx i BoolLit.InLoop
  let init_ok_ ctx i = BoolLit.inject_ctx ctx i BoolLit.InitOk

  (* encode proof relation into QBF, starting from the set of "roots"
      that are (applications of) clause contexts + false.

      Plan:
        incremental:
          bigAnd i:inductive_cst:
            (path_constraint(i) => valid(i))
            valid(i) = ([empty(i)] | minimal_and_eq(t, i))
        backtrackable:
          bigAnd i:inductive_cst:
            minimal_and_eq(t,i) => t=i & minimal(t, i)
            minimal(t, i) => bigAnd_{t' < t} bigOr_{ctx} (inloop(C,i) & minimal(C,i,t'))
            empty(i) => def of empty(i)
        *)

  (* current save level *)
  let save_level_ = ref Solver.root_save_level

  module QF = Qbf.Formula

  (* add
    - pathconditions(i) => [valid(i)]
    - valid(i) =>
        bigand_{set in coversets}
        bigor_{t in set}
        ([i = t] & minimal(i, t))
  *)
  let qbf_encode_valid_ cst =
    let pc = CI.pc cst in (* path conditions *)
    Solver.quantify_lits level2_
      [valid_ cst; empty_ cst];
    Solver.add_clause
      (valid_ cst :: List.map (fun pc -> neg_ pc.CI.pc_lit) pc); (* pc -> valid *)
    Solver.add_form
      (QF.imply
        (QF.atom (valid_ cst))
        (QF.and_map (CI.cover_sets cst)
          ~f:(fun set -> QF.or_map (set.CI.cases |> CCList.to_seq)
            ~f:(fun t -> QF.or_l
                [ QF.atom (is_eq_ cst t)
                ; QF.atom (minimal_ cst t)
                ]
            )
          )
        )
      );
    ()

  (* definition of empty(cst):
    empty(loop(cst)) => bigor_{C in candidates(cst)} [C in loop(cst)] *)
  let qbf_encode_empty_ cst =
    let guard = neg_ (empty_ cst) in
    let clauses = candidates_for_ cst
      |> Sequence.map
        (fun ctx ->
          let inloop = in_loop_ ctx.cand_ctx cst in
          Solver.quantify_lits level1_ [inloop];
          [guard; neg_ inloop]
        )
    in
    Solver.add_clause_seq clauses;
    ()

  (* encode [minimal(cst,t)]:
    minimal(cst,t) =>
      bigand_{t' sub-constant of t}
      bigor{C[_] in candidates(cst)}
        (
          [C[_] in S_loop(cst)] and
          expresses_minimality(C[_],cst,t')
        )
  *)
  let qbf_encode_minimal_ cst =
    CI.cover_sets cst
    |> Sequence.flat_map
      (fun set ->
        set.CI.cases
          |> List.map (fun x -> set, x)
          |> Sequence.of_list
      )
    |> Sequence.iter
      (fun (set, t) ->
        (* now to define minimal(cst,t) *)
        Solver.add_form
          (QF.imply
            (QF.atom (minimal_ cst t))
            (QF.and_map (assert false) (assert false)) (* TODO iterate on sub-constants of t *)
          )
      )

  (* TODO: from here *)

  (* the whole process of:
      - adding non-backtracking constraints
      - save state
      - adding backtrackable constraints *)
  let qbf_encode_enter_ () =
    Util.enter_prof prof_encode_qbf;
    (* normal constraints should be added already *)
    Util.debug ~section:section_qbf 4 "save QBF solver...";
    save_level_ := Solver.save ();
    CI.Seq.cst |> Sequence.iter qbf_encode_minimal_;
    CI.Seq.cst |> Sequence.iter qbf_encode_empty_;
    Util.exit_prof prof_encode_qbf;
    ()

  (* restoring state *)
  let qbf_encode_exit_ () =
    Util.debug ~section:section_qbf 4 "...restore QBF solver";
    Solver.restore !save_level_;
    ()

  (* add/remove constraints before/after satisfiability checking *)
  let () =
    Signal.on Avatar.before_check_sat
      (fun () -> qbf_encode_enter_ (); Signal.ContinueListening);
    Signal.on Avatar.after_check_sat
      (fun () -> qbf_encode_exit_ (); Signal.ContinueListening);
    Signal.on CI.on_new_inductive
      (fun cst ->
        qbf_encode_valid_ cst;
        Signal.ContinueListening
      );
    ()

  (** {6 Expressing Minimality} *)

  (* TODO: move all this into CI? *)
  (* build a skolem term to replace [v] *)
  let skolem_term v =
    let sym = Logtk.Skolem.fresh_sym_with ~ctx:Ctx.skolem ~ty:(T.ty v) "#min" in
    Ctx.declare sym (T.ty v);
    let t = T.const ~ty:(T.ty v) sym in
    CI.set_blocked t; (* no induction on this witness! *)
    t

  let subst_of_seq l =
    Sequence.fold (fun s (v,t) -> Su.FO.bind s v 1 t 0) Su.empty l

  let split_for_minimality cand set cst =
    T.Set.to_seq set.CI.sub_constants
    |> Sequence.flat_map
      (fun t' ->
        let lits = ClauseContext.apply cand.cand_ctx t' in
        let cst, t = CI.inductive_cst_of_sub_cst t' in
        (* ground every variable of the clause, because of the negation
            in front of "forall" *)
        let subst = Lits.Seq.terms lits
          |> Sequence.flat_map T.Seq.vars
          |> Sequence.sort_uniq ~cmp:T.cmp
          |> Sequence.map (fun v -> v, skolem_term v)
          |> subst_of_seq
        and renaming = Su.Renaming.create () in
        (* not sigma(l) <- [expresses_minimality ctx cst], [cst = t]
          for each l in cand[t'], where sigma is a grounding substitution
          with fresh skolems *)
        CCArray.to_seq lits
        |> Sequence.map
          (fun lit ->
            let lit = Literal.apply_subst ~renaming subst lit 1 in
            let trail = C.Trail.of_list
              [ expresses_minimality_ cand.cand_ctx cst t
              ; is_eq_ cst t
              ]
            in
            let proof cc = Proof.mk_c_inference
              ~info:[Util.sprintf "minimality of [%a]" ClauseContext.pp cand.cand_ctx]
              ~rule:"minimality"
              ~theories:["ind"] cc cand.cand_explanations
            in
            let c = C.create ~trail [Literal.negate lit] proof in
            C.set_flag flag_expresses_minimality c true;
            Util.debug ~section 2 "minimality of %a gives %a"
              Lits.pp cand.cand_lits C.pp c;
            c
          )
      )
    |> Sequence.to_rev_list

  (* for every new context [ctx[_]], express that [ctx[t']] can be the
      witness of [not S_loop[t']] if [ctx[_] in S_loop[cst]] for
      every coverset;
      do the same for every new coverset and already existing context *)
  let express_minimality_ctx () =
    let clauses = List.fold_left
      (fun acc cand ->
        CI.cover_sets cand.cand_cst
        |> Sequence.fold
          (fun acc set ->
            let clauses = split_for_minimality cand set cand.cand_cst in
            List.rev_append clauses acc
          ) acc
      ) [] !new_contexts
    in
    new_contexts := [];
    let clauses = List.fold_left
      (fun acc (cst,set) ->
        candidates_for_ cst
        |> Sequence.fold
          (fun acc cand ->
            let clauses = split_for_minimality cand set cst in
            List.rev_append clauses acc
          ) acc
      ) clauses !new_cover_sets
    in
    new_cover_sets := [];
    clauses

  (** {6 Summary}

  How to print a summary of which contexts are available, which proofs can
  they use, etc. *)

  let print_summary () =
    (* print a coverset *)
    let print_coverset cst fmt set =
      Format.fprintf fmt "@[<h>cover set {%a}@]"
        (Sequence.pp_seq T.fmt) (set.CI.cases |> CCList.to_seq)
    (* print a clause context *)
    and print_cand cst fmt cand =
      Format.fprintf fmt "@[<h>%a@]" ClauseContext.print cand.cand_ctx
    in
    (* print the loop for cst *)
    let print_cst fmt cst =
      Format.fprintf fmt "@[<hv2>for %a:@ %a@,%a@]" CI.pp_cst cst
        (Sequence.pp_seq ~sep:"" (print_coverset cst))
        (CI.cover_sets cst)
        (Sequence.pp_seq ~sep:"" (print_cand cst))
        (candidates_for_ cst)
    in
    Format.printf "@[<v2>inductive constants:@ %a@]@."
      (Sequence.pp_seq ~sep:"" print_cst)
      (CstTbl.keys candidates_by_icst_);
    ()

  (** {6 Registration} *)

  (* declare a list of inductive types *)
  let declare_types_ l =
    List.iter
      (fun (ty,cstors) ->
        (* TODO: support polymorphic types? *)
        let pattern = Ty.const (Sym.of_string ty) in
        let constructors = List.map
          (fun str ->
            let s = Sym.of_string str in
            match Ctx.find_signature s with
              | None ->
                  let msg = Util.sprintf
                    "cannot find the type of inductive constructor %s" str
                  in failwith msg
              | Some ty ->
                  s, ty
          ) cstors
        in
        (* declare type. *)
        ignore (CI.declare_ty pattern constructors);
        Util.debug ~section 1 "declare inductive type %a" Ty.pp pattern;
        ()
      ) l

  (* ensure s1 > s2 if s1 is an inductive constant and s2 is a sub-case of s1 *)
  let constr_sub_cst_ s1 s2 =
    let module C = Logtk.Comparison in
    let res =
      if CI.is_inductive_symbol s1 && CI.dominates s1 s2
        then C.Gt
      else if CI.is_inductive_symbol s2 && CI.dominates s2 s1
        then C.Lt
      else C.Incomparable
    in res

  let register() =
    Util.debug ~section 1 "register induction calculus";
    declare_types_ !ind_types_;
    Solver.set_printer BoolLit.print;
    Ctx.add_constr 20 constr_sub_cst_;  (* enforce new constraint *)
    (* avatar rules *)
    Env.add_multi_simpl_rule Avatar.split;
    Env.add_unary_inf "avatar.check_empty" Avatar.check_empty;
    Env.add_generate "avatar.check_sat" Avatar.check_satisfiability;
    (* induction rules *)
    Env.add_generate "induction.scan_backward" scan_backward;
    Env.add_unary_inf "induction.scan_extrude" scan_given_for_context;
    Env.add_unary_inf "induction.scan_proof" scan_given_for_proof;
    Env.add_is_trivial has_trivial_trail;
    Env.add_lit_rule "induction.injectivity1" injectivity_simple;
    Env.add_simplify injectivity_destruct;
    Env.add_generate "induction.express_minimality" express_minimality_ctx;
    (* XXX: ugly, but we must do case_split before scan_extrude/proof.
      Currently we depend on Env.generate_unary applying inferences in
      the reverse order of their addition *)
    Env.add_unary_inf "induction.case_split" case_split_ind;
    ()

  (* print info before exit, if asked to *)
  let summary_on_exit () =
    Signal.once Signals.on_exit
      (fun _ -> print_summary ());
    ()
end

let summary_ = ref false

let extension =
  let action env =
    let module E = (val env : Env.S) in
    E.Ctx.lost_completeness ();
    let sup = Mixtbl.find ~inj:Superposition.key E.mixtbl "superposition" in
    let module Sup = (val sup : Superposition.S) in
    let module Solver = (val BoolSolver.get_qbf() : BoolSolver.QBF) in
    Util.debug ~section:section_qbf 2 "created QBF solver \"%s\"" Solver.name;
    let module A = Make(Sup)(Solver) in
    A.register();
    if !summary_ then A.summary_on_exit()
  (* add an ordering constraint: ensure that constructors are smaller
    than other terms *)
  and add_constr penv =
    let module C = Logtk.Comparison in
    let constr_cstor s1 s2 = match is_constructor_ s1, is_constructor_ s2 with
      | true, true
      | false, false -> if Sym.eq s1 s2 then C.Eq else C.Incomparable
      | true, false -> C.Lt
      | false, true -> C.Gt
    in
    PEnv.add_constr ~penv 15 constr_cstor
  in
  Extensions.({default with
    name="induction";
    actions=[Do action];
    penv_actions=[Penv_do add_constr];
  })

let enabled_ = ref false
let enable_ () =
  if not !enabled_ then (
    enabled_ := true;
    Util.debug ~section 1 "Induction: requires ord=rpo6; select=NoSelection";
    Params.ord := "rpo6";   (* new default! RPO is necessary*)
    Params.dot_all_roots := true;  (* print proofs more clearly *)
    Params.select := "NoSelection";
    Extensions.register extension
  )

let declare_ ty cstors =
  (* remember to declare this type as inductive *)
  Util.debug ~section 1 "user declares inductive type %s = %a"
    ty (CCList.pp CCString.pp) cstors;
  ind_types_ := (ty, cstors) :: !ind_types_;
  enable_();
  ()

(* [str] describes an inductive type, under the form "foo:c1|c2|c3" where
    "foo" is the type name and "c1", "c2", "c3" are the type constructors. *)
let add_ind_type_ str =
  let _fail() =
    failwith "expected \"type:c1|c2|c3\" where c1,... are constructors"
  in
  match Util.str_split ~by:":" str with
  | [ty; cstors] ->
      let cstors = Util.str_split ~by:"|" cstors in
      if List.length cstors < 2 then _fail();
      declare_ ty cstors
  | _ -> _fail()

module A = Logtk_parsers.Ast_tptp

let init_from_decls pairs =
  let get_str = function
    | A.GNode (s, []) | A.GString s -> s
    | _ -> raise Exit
  in
  (* search for "inductive(c1, c2, ...)" *)
  let rec scan_for_constructors = function
    | A.GNode ("inductive", l) :: tail when List.length l >= 2 ->
        begin try
          let constructors = List.map get_str l in
          Some constructors
        with Exit ->
          scan_for_constructors tail
        end
    | _ :: tail -> scan_for_constructors tail
    | []  -> None
  in
  Sequence.iter
    (fun (ty, info) -> match scan_for_constructors info with
      | None -> ()
      | Some l -> declare_ ty l
    ) pairs

let () =
  Params.add_opts
    [ "-induction", Arg.String add_ind_type_,
      " enable Induction on the given type"
    ; "-induction-depth", Arg.Set_int cover_set_depth_,
      " set default induction depth"
    ; "-induction-summary", Arg.Set summary_,
      " show summary of induction before exit"
    ]
