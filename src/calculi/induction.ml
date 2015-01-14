
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

module Make(E : Env.S)(Sup : Superposition.S)(Solver : BoolSolver.QBF) = struct
  module Env = E
  module Ctx = Env.Ctx

  module C = Env.C
  module CI = Ctx.Induction
  module CCtx = ClauseContext
  module BoolLit = Ctx.BoolLit
  module Avatar = Avatar.Make(E)(Solver)  (* will use some inferences *)

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

  let level1_ = Solver.push Qbf.Forall []
  let level2_ = Solver.push Qbf.Exists []

  (* a candidate clause context *)
  type candidate_context = {
    cand_ctx : CCtx.t; (* the ctx itself *)
    cand_cst : T.t;    (* the inductive constant *)
    cand_lits : Lits.t; (* literals of [ctx[t]] *)
    mutable cand_initialized : bool; (* is [ctx[i]] proved yet? *)
    mutable cand_explanations : Proof.t list; (* justification(s) of why the context exists *)
  }

  (* if true, ignore clause when it comes to extracting contexts *)
  let flag_no_ctx = C.new_flag ()

  (* if true, means the clause represents the possibility of another
    clause being the witness of the minimality of the current model *)
  let flag_expresses_minimality = C.new_flag ()

  (** {6 Proof Relation} *)

  (* TODO: remove the whole proof relation stuff? In particular, we only
      need to check whether a S_loop() is initialized; that could be done
      by finding subsuming clauses (trails)
      for each C[i] where C[_] in S_loop(i) *)

  module CompactClauseSet = Sequence.Set.Make(CompactClause)

  (* one way to prove a clause: set of parent clauses
  type proof = CompactClauseSet.t
  *)

  (* given a proof (a set of parent clauses) extract the corresponding trail *)
  let trail_of_proof set =
    CompactClauseSet.fold
      (fun cc acc ->
        let trail = CompactClause.trail cc in
        List.fold_left
          (fun acc lit -> match lit with
            | (sign, `Box_clause lits) ->
                BoolLit.set_sign sign (BoolLit.inject_lits lits) :: acc
            | (sign, `Qbf_artifact (i, repr)) ->
                BoolLit.set_sign sign i :: acc
          ) acc trail
      ) set []

  module ProofSet = Sequence.Set.Make(CompactClauseSet)

  (* a set of proofs *)
  type proof_set = {
    mutable proofs : ProofSet.t;
  }

  module FV_proofs = FVMap(struct
    type t = proof_set  (* lits -> set of proofs of those literals *)
  end)

  module FV_watch = FVMap(struct
    type t = [ `Inductive_cst of candidate_context | `Sub_cst ] list ref
    (* watched literals (as [lits]) ->
        set of either:
          - `Inductive ctx, a candidate_ctx
              such that [ctx.cand_lits[ctx.cand_cst] = lits]
          - `Sub_cst, we watch for proofs but have nothing else to do *)
  end)

  (* maps clauses to a list of their known proofs *)
  let proofs_ = ref (FV_proofs.empty ())

  (* find all known proofs of the given lits *)
  let find_proofs_ lits = FV_proofs.find !proofs_ lits

  (* clauses that we check for subsumption. If they are subsumed by
    some clause, we add an explicit proof *)
  let to_watch_ = ref (FV_watch.empty())

  (* clauses that were just added to {!to_watch_} *)
  let to_watch_new_ = ref (FV_watch.empty())

  (* signal to trigger whenever a new clause should be watched *)
  let on_new_to_watch = Signal.create ()

  (* from now on, we are interested in proofs of this clause *)
  let watch_proofs_of_clause lits elt =
    match FV_watch.find !to_watch_ lits with
    | Some l -> CCList.Ref.push l elt
    | None ->
        to_watch_new_ := FV_watch.add !to_watch_new_ lits (ref [elt]);
        Signal.send on_new_to_watch lits

  exception NonClausalProof
  (* raised when a proof doesn't use only clauses (also formulas...) *)

  (* add a proof to the set of proofs for [lits] *)
  let add_proof_ lits p =
    try
      let parents = Array.to_list p.Proof.parents
      |> List.map
        (fun p -> match p.Proof.result with
          | Proof.Form _ -> raise NonClausalProof
          | Proof.Clause lits -> lits
        )
      in
      let parents = CompactClauseSet.of_list parents in
      (* all proofs of [lits] *)
      let set = match find_proofs_ lits with
        | None ->
            let set = {proofs=ProofSet.empty} in
            proofs_ := FV_proofs.add !proofs_ lits set;
            set
        | Some set -> set
      in
      set.proofs <- ProofSet.add parents set.proofs;
      Util.debug ~section 5 "add proof: %a" Proof.pp_notrec p;
    with NonClausalProof ->
      Util.debug ~section 5 "ignore non-clausal proof: %a" Proof.pp_notrec p;
      ()  (* ignore the proof *)

  let () =
    Signal.on C.on_proof
      (fun (lits, p) ->
        add_proof_ lits p;
        Signal.ContinueListening
      );
    ()

  (** {6 Split on Inductive Constants} *)

  (* true if [t = c] where [c] is some inductive constructor such as "cons" or "node" *)
  let is_a_constructor_ t = match T.Classic.view t with
    | T.Classic.App (s, _, _) ->
        Sequence.exists (Sym.eq s) CI.Seq.constructors
    | _ -> false

  (* scan clauses for ground terms of an inductive type, and declare those terms *)
  let scan seq =
    Sequence.iter
      (fun c ->
        Lits.Seq.terms (Env.C.lits c)
        |> Sequence.flat_map T.Seq.subterms
        |> Sequence.filter
          (fun t ->
            T.is_ground t
            && T.is_const t
            && not (CI.is_blocked t)
            && CI.is_inductive_type (T.ty t)
            && not (is_a_constructor_ t)   (* 0 and nil: not inductive const *)
          )
        |> Sequence.iter (fun t -> CI.declare t)
      ) seq

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
      |> Sequence.filter CI.is_inductive
      |> Sequence.iter
        (fun t ->
          match CI.cover_set ~depth:!cover_set_depth_ t with
          | _, `Old -> ()
          | set, `New ->
              (* Make a case split on the cover set (one clause per literal) *)
              Util.debug ~section 2 "make a case split on inductive %a" T.pp t;
              let clauses_and_lits = List.map
                (fun t' ->
                  assert (T.is_ground t');
                  let lits = [| Literal.mk_eq t t' |] in
                  let bool_lit = BoolLit.inject_lits lits in
                  Solver.quantify_lits Solver.level0 [bool_lit];
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
              Util.debug ~section 4 "split inference for %a: %a"
                T.pp t (CCList.pp C.pp) clauses;
              res := List.rev_append clauses !res
        );
    !res

  (* checks whether the trail of [c] contains two literals [i = t1]
      and [i = t2] with [t1], [t2] distinct cover set members, or two
      literals [loop(i) minimal by a] and [loop(i) minimal by b]. *)
  let has_trivial_trail c =
    let trail = C.get_trail c |> C.Trail.to_seq in
    (* all i=t where i is inductive *)
    let relevant_cases = trail
      |> Sequence.filter_map
        (fun blit ->
          let sign = blit >= 0 in
          match BoolLit.extract (abs blit) with
          | None -> None
          | Some (BoolLit.Clause_component [| Literal.Equation (l, r, true) |]) ->
              if sign && CI.is_inductive l then Some (`Case (l, r))
              else if sign && CI.is_inductive r then Some (`Case (r, l))
              else None
          | Some (BoolLit.Ctx (ctx, n, BoolLit.ExpressesMinimality) as lit) ->
              Some (`Minimal (ctx, n, lit))
          | Some _ -> None
        )
    in
    (* is there i such that   i=t1 and i=t2 can be found in the trail? *)
    Sequence.product relevant_cases relevant_cases
      |> Sequence.exists
        (function
          | (`Case (i1, t1), `Case (i2, t2)) ->
              let res = T.eq i1 i2 && not (T.eq t1 t2) in
              if res
              then Util.debug ~section 4
                "clause %a redundant because of %a={%a,%a} in trail"
                C.pp c T.pp i1 T.pp t1 T.pp t2;
              res
          | (`Minimal (ctx1, i1, lit1), `Minimal (ctx2, i2, lit2)) ->
              let res = T.eq i1 i2 && not (ClauseContext.equal ctx1 ctx2) in
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
                && CI.is_constructor_sym s2 ->
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
      let lits = Util.array_except_idx (C.lits c) idx in
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
    type t = candidate_context
  end)

  type candidate_context_set = FV_cand.t ref

  (* maps each inductive constant to
      set(clause contexts that are candidate for induction on this constant) *)
  let candidates_ : candidate_context_set T.Tbl.t = T.Tbl.create 255

  (* candidates for a term *)
  let find_candidates_ t =
    try T.Tbl.find candidates_ t
    with Not_found ->
      let set = ref (FV_cand.empty ()) in
      T.Tbl.add candidates_ t set;
      set

  let new_contexts : candidate_context list ref = ref []

  let on_new_context =
    let s = Signal.create () in
    Signal.on s (fun ctx ->
      (* new context:
        - watch proofs of ctx[i] and ctx[t] for all [t] that is
          a sub-constant of [i]
        - assert [not l] for all [t] a sub-constant of [i] and [l]
            literal of [ctx[t]], to express the minimality *)
      watch_proofs_of_clause ctx.cand_lits (`Inductive_cst ctx);
      Util.debug ~section 2 "watch %a (initialization, %a)"
        Lits.pp ctx.cand_lits T.pp ctx.cand_cst;
      CI.cover_sets ctx.cand_cst
        |> Sequence.flat_map (fun set -> T.Set.to_seq set.CI.sub_constants)
        |> Sequence.iter
          (fun t ->
            let c = CCtx.apply ctx.cand_ctx t in
            Util.debug ~section 2 "watch %a (sub-cst %a)" Lits.pp c T.pp t;
            watch_proofs_of_clause c `Sub_cst
          );
      new_contexts := ctx :: !new_contexts;
      Signal.ContinueListening
    );
    s

  (* [t] is an inductive const; this returns [true] iff some subconstant of [t]
      occurs in [c] *)
  let contains_any_sub_constant_of c t =
    Lits.Seq.terms (Env.C.lits c)
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.exists
      (fun t' ->
        T.is_ground t
        && CI.is_sub_constant t'
        && T.eq t (fst (CI.inductive_cst_of_sub_cst t'))
      )

  (* set of subterms of [lits] that could be extruded to form a context.
     restrictions:
       - a context can be extracted only from an inductive constant
         such that no sub-constant of it occurs in the clause; otherwise
         it would be meaningless to express the minimality of the clause
  *)
  let subterms_candidates_for_context_ c =
    Lits.Seq.terms (C.lits c)
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.filter
        (fun t ->
          T.is_ground t
          && CI.is_inductive t
          && not (contains_any_sub_constant_of c t)
        )
      |> T.Seq.add_set T.Set.empty

  (* ctx [c] is now initialized. Return [true] if it wasn't initialized before *)
  let cand_ctx_initialized_ c explanation =
    let is_new = not c.cand_initialized in
    if is_new then (
      c.cand_initialized <- true;
      c.cand_explanations <- explanation :: c.cand_explanations;
      Util.debug ~section 2 "clause context %a[%a] now initialized"
        CCtx.pp c.cand_ctx T.pp c.cand_cst;
    );
    is_new

  (* see whether (ctx,t) is of interest. *)
  let process_ctx_ c ctx t =
    (* if [t] is an inductive constant, ctx is enabled! *)
    if CI.is_inductive t
      then (
        let set = find_candidates_ t in
        match FV_cand.find !set (C.lits c) with
          | None ->
              let ctx = {
                cand_initialized=true;
                cand_ctx=ctx;
                cand_cst=t;
                cand_lits=C.lits c;
                cand_explanations=[C.proof c];
              } in
              Util.debug ~section 2 "new (initialized) context for %a: %a"
                T.pp t CCtx.pp ctx.cand_ctx;
              set := FV_cand.add !set (C.lits c) ctx;
              Signal.send on_new_context ctx;
              None
          | Some ctx ->
              let expl = C.proof c in
              let is_new = cand_ctx_initialized_ ctx expl in (* we just proved ctx[t] *)
              if is_new
                then Some c (* context initialized; we can assume it *)
                else None
              (* TODO: is this necessary? we should just watch for proofs anyway *)
      ) else
        (* [t] is a subterm of the case [t'] of an inductive [cst] *)
        let cst, t' = CI.inductive_cst_of_sub_cst t in
        let set = find_candidates_ cst in
        let lits' = CCtx.apply ctx cst in
        match FV_cand.find !set lits' with
          | None ->
              let cand_ctx = {
                cand_initialized=false;
                cand_ctx=ctx;
                cand_cst=cst;
                cand_lits=lits'; (* ctx[cst] *)
                cand_explanations=[C.proof c];
              } in
              (* need to watch ctx[cst] until it is proved *)
              Util.debug ~section 2 "new context %a" CCtx.pp ctx;
              Signal.send on_new_context cand_ctx;
              set := FV_cand.add !set lits' cand_ctx;
              None
          | Some _ ->
              None  (* no new context *)

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

  (* [c] (same as [lits]) subsumes [lits'] which is watched with list
    of contexts [l] *)
  let _process_clause_match_watched acc c lits lits' l =
    (* remember proof *)
    let proof cc = Proof.mk_c_inference ~rule:"subsumes" cc [C.proof c] in
    let proof' = proof (CompactClause.make lits' (C.get_trail c |> C.compact_trail)) in
    add_proof_ lits' proof';
    Util.debug ~section 4 "add proof of watched %a because of %a" Lits.pp lits' C.pp c;
    (* check whether that makes some cand_ctx initialized *)
    List.fold_left
      (fun acc elt -> match elt with
        | `Sub_cst -> acc  (* sub-constant, no initialization *)
        | `Inductive_cst cand_ctx ->
            (* [c] proves the initialization of [cand_ctx], i.e. the
              clause context applied to the corresponding
              inductive constant (rather than a sub-case) *)
            assert (CI.is_inductive cand_ctx.cand_cst);
            let is_new = cand_ctx_initialized_ cand_ctx (C.proof c) in
            if is_new then (
              (* ctx[t] is now proved, deduce ctx[t] and add it to set of clauses *)
              let clause = C.create_a ~parents:[c]
                ~trail:(C.get_trail c) lits proof in
              (* disable subsumption for [clause] *)
              C.set_flag C.flag_persistent clause true;
              Util.debug ~section 4 "initialized %a by subsumption from %a"
                C.pp clause C.pp c;
              clause :: acc
            ) else acc
      ) acc !l

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
    else FV_watch.find_subsumed_by !to_watch_ lits
      |> Sequence.fold
        (fun acc (lits', l) ->
          _process_clause_match_watched acc c lits lits' l
        ) []

  (* scan the set of active clauses, to see whether it proves some
      of the lits in [to_watch_new_] *)
  let scan_backward () =
    let w = !to_watch_new_ in
    to_watch_new_ := FV_watch.empty ();
    Env.ProofState.ActiveSet.clauses ()
      |> C.CSet.to_seq
      |> Sequence.filter (fun c -> Array.length (C.lits c) > 0)
      |> Sequence.fold
        (fun acc c ->
          let lits = C.lits c in
          FV_watch.find_subsumed_by w lits
            |> Sequence.fold
              (fun acc (lits',l) ->
                _process_clause_match_watched acc c lits lits' l
              ) acc
        ) []

  (** {6 Encoding to QBF} *)

  let neg_ = BoolLit.neg
  let valid_ x = BoolLit.inject_name' "valid(%a)" T.pp x
  let cases_ x = BoolLit.inject_name' "cases(%a)" T.pp x
  let empty_ x = BoolLit.inject_name' "empty(%a)" T.pp x
  let minimal_ x = BoolLit.inject_name' "minimal(%a)" T.pp x
  let init_ x = BoolLit.inject_name' "init(%a)" T.pp x

  let expresses_minimality_ ctx cst =
    BoolLit.inject_ctx ctx cst BoolLit.ExpressesMinimality

  let expresses_minimality_aux_ ctx cst =
    BoolLit.inject_ctx ctx cst BoolLit.ExpressesMinimalityAux

  let is_true_ lits = BoolLit.inject_lits lits
  let trail_ok_ lits = BoolLit.inject_lits_pred lits BoolLit.TrailOk
  let in_loop_ ctx i = BoolLit.inject_ctx ctx i BoolLit.InLoop
  let init_ok_ ctx i = BoolLit.inject_ctx ctx i BoolLit.InitOk

  (* encode proof relation into QBF, starting from the set of "roots"
      that are (applications of) clause contexts + false.

      Plan:
        incremental:
          case splits
          bigAnd i:inductive_cst:
            def of [cases(i)]
            (path_constraint(i) => valid(i))
            valid(i) = [cases(i)] & ([empty(i)] | not [init(i)] | [minimal(i)])
        backtrackable:
          (bigAnd_coversets(i) (bigXOr_{t in coverset(i)} i=t)) => [valid(i)]
          bigAnd i:inductive_cst:
            minimal(i) => def of minimal(i)
            empty(i) => def of empty(i)
        *)

  (* current save level *)
  let save_level_ = ref Solver.root_save_level

  (* add
    - pathconditions(i) => [valid(i)]
    - valid(i) => [cases(i)] &
    - (not [proofgraph] | [empty(i)] | not [loop(i)]) *)
  let qbf_encode_valid_ cst =
    let pc = CI.pc cst in
    Solver.quantify_lits level2_
      [valid_ cst; empty_ cst; init_ cst; minimal_ cst];
    Solver.add_clauses
      [ valid_ cst :: List.map (fun pc -> neg_ pc.CI.pc_lit) pc (* pc -> valid *)
      ; [ neg_ (valid_ cst); cases_ cst ]
      ; [ neg_ (valid_ cst); neg_ (init_ cst); empty_ cst; minimal_ cst ]
      ];
    ()

  (* - for each candidate ctx C,
        for each proof p in proofs(C[cst]), add:
        "trail(p) => trail_ok_(C[cst])" and
        "trail_ok(C,cst) => init_ok(C[cst])"
        "not (C in loop(cst)) => init_ok(C[cst])"
     - add "(bigand_{candidate ctx C} init_ok(C[cst])) => init(cst)"
  *)
  let qbf_encode_init_ cst =
    let candidates = !(find_candidates_ cst)
      |> FV_cand.to_seq |> Sequence.map snd in
    let init_ok_clauses =
      candidates
      |> Sequence.fold
        (fun acc ctx ->
          (* proof of [ctx[cst]].
           TODO: find those proofs through regular subsumption w.r.t active
           set so we can stop storing all proofs *)
          match find_proofs_ ctx.cand_lits with
            | None -> acc
            | Some {proofs} ->
                ProofSet.to_seq proofs
                |> Sequence.map trail_of_proof
                |> Sequence.fold
                  (fun acc lits ->
                    let c1 =
                      trail_ok_ ctx.cand_lits ::
                      List.map neg_ lits
                    and c2 =
                      [ neg_ (trail_ok_ ctx.cand_lits)
                      ; init_ok_ ctx.cand_ctx cst ]
                    and c3 =
                      [ in_loop_ ctx.cand_ctx cst
                      ; init_ok_ ctx.cand_ctx cst ]
                    in c1 :: c2 :: c3 :: acc
                  ) acc
        ) []
    and init_clause =
      let guard = Sequence.map
        (fun ctx ->
          Solver.quantify_lits level2_
            [ init_ok_ ctx.cand_ctx cst
            ; trail_ok_ ctx.cand_lits
            ];
          neg_ (init_ok_ ctx.cand_ctx cst)
        ) candidates
      in
      init_ cst :: Sequence.to_rev_list guard
    in
    Solver.add_clauses (init_clause :: init_ok_clauses)

  (* encode [minimal(cst)]:
    minimal(cst) =>
      bigor{C[_] in candidates(cst)}
       ( [C[_] in S_loop(cst)] and expresses_minimality(C[_],cst)

    becomes:

    minimal(cst) & [cst=t] => bigor_{C in cand(cst)} expresses_minimality_aux(C, t)
    expresses_minimality_aux(C[_], t) =>
      [C in_loop(cst)] & expresses_minimality(C[t])
  *)
  let qbf_encode_minimal_ cst =
    let candidates = !(find_candidates_ cst) |> FV_cand.to_seq |> Sequence.map snd in
    (* Big_or, to choose which context is going to be the one
      that expresses minimality *)
    Solver.add_clause
      ( neg_ (minimal_ cst)
      :: (
        candidates
        |> Sequence.map
          (fun cand -> expresses_minimality_aux_ cand.cand_ctx cst)
        |> Sequence.to_rev_list
      )
    );
    (* for each candidate, express what it means to be the one to
        express minimality *)
    candidates
      |> Sequence.iter
        (fun cand ->
          let elit_aux = expresses_minimality_aux_ cand.cand_ctx cst
          and elit = expresses_minimality_ cand.cand_ctx cst in
          Solver.quantify_lits level2_ [elit_aux; elit];
          Solver.add_clauses
            [ [ neg_ elit_aux; in_loop_ cand.cand_ctx cst ]
            ; [ neg_ elit_aux; elit ]
            ]
        );
    ()

  (* definition of empty(cst):
    empty(loop(cst)) => bigor_{C in candidates(cst)} [C in loop(cst)] *)
  let qbf_encode_empty_ cst =
    let guard = neg_ (empty_ cst) in
    let clauses = !(find_candidates_ cst)
      |> FV_cand.to_seq
      |> Sequence.map snd
      |> Sequence.map
        (fun ctx ->
          let inloop = in_loop_ ctx.cand_ctx cst in
          Solver.quantify_lits level1_ [inloop];
          [guard; neg_ inloop]
        )
    in
    Solver.add_clause_seq clauses;
    ()

  (* encode: cases(i) => bigAnd_{s in coversets(i) (xor_{t in s} i=t} *)
  let qbf_encode_cover_set cst set =
    let big_xor =
      mk_xor_
        (List.map
          (fun t -> is_true_ [|Literal.mk_eq t cst|])
          set.CI.cases
        )
    in
    (* guard every clause with "cases(i) => clause" *)
    Solver.quantify_lits level2_ [cases_ cst];
    let clauses = List.map (fun c -> neg_ (cases_ cst) :: c) big_xor in
    Solver.add_clauses clauses;
    ()

  (* the whole process of:
      - adding non-backtracking constraints
      - save state
      - adding backtrackable constraints *)
  let qbf_encode_enter_ () =
    Util.enter_prof prof_encode_qbf;
    (* normal constraints should be added already *)
    Util.debug ~section:section_qbf 4 "save QBF solver...";
    save_level_ := Solver.save ();
    CI.Seq.cst |> Sequence.iter qbf_encode_init_;
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
    Signal.on CI.on_new_cover_set
      (fun (cst, set) ->
        qbf_encode_cover_set cst set;
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
              [ expresses_minimality_ cand.cand_ctx cst
              ; is_true_ [| Literal.mk_eq t cst |]
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
        !(find_candidates_ cst)
          |> FV_cand.to_seq
          |> Sequence.map snd
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
    (* print a clause context; print for which terms it's provable *)
    and print_cand cst fmt cand =
      let has_proof lits =
        match find_proofs_ lits with
        | None -> false
        | Some _ -> true
      in
      let lits = CI.cover_sets cst
          |> Sequence.flat_map (fun set -> T.Set.to_seq set.CI.sub_constants)
          |> Sequence.map
            (fun t -> t, ClauseContext.apply cand.cand_ctx t)
          |> Sequence.(cons (cand.cand_cst, cand.cand_lits))
      in
      let proofs =
        Sequence.filter_map
          (fun (t, lits) -> if has_proof lits then Some t else None)
          lits
      in
      Format.fprintf fmt "@[<h>(%s) %a   (proofs: %a)@]"
        (if cand.cand_initialized then "*" else " ")
        ClauseContext.print cand.cand_ctx
        (Sequence.pp_seq ~sep:", " T.fmt) proofs
    in
    (* print the loop for cst *)
    let print_cst fmt cst =
      Format.fprintf fmt "@[<hv2>for %a:@ %a@,%a@]" T.fmt cst
        (Sequence.pp_seq ~sep:"" (print_coverset cst))
        (CI.cover_sets cst)
        (Sequence.pp_seq ~sep:"" (print_cand cst))
        (!(find_candidates_ cst) |> FV_cand.to_seq |> Sequence.map snd)
    in
    Format.printf "@[<v2>inductive constants:@ %a@]@."
      (Sequence.pp_seq ~sep:"" print_cst)
      (T.Tbl.to_seq candidates_ |> Sequence.map fst);
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
    let module A = Make(E)(Sup)(Solver) in
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
    [ "-induction", Arg.String add_ind_type_, " enable Induction on the given type"
    ; "-induction-depth", Arg.Set_int cover_set_depth_, " set default induction depth"
    ; "-induction-summary", Arg.Set summary_, " show summary of induction before exit"
    ]
