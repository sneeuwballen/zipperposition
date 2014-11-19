
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

  (* a candidate clause context *)
  type candidate_context = {
    cand_ctx : CCtx.t; (* the ctx itself *)
    cand_cst : T.t;    (* the inductive constant *)
    cand_lits : Lits.t; (* literals of [ctx[t]] *)
    mutable cand_initialized : bool; (* is [ctx[i]] proved yet? *)
  }

  (* if true, ignore clause when it comes to extracting contexts *)
  let flag_no_ctx = C.new_flag ()

  (** {6 Proof Relation} *)

  (* one way to prove a clause: parent clauses and the union of their trails *)
  type proof = CompactClause.t list

  (* a set of proofs *)
  type proof_set = {
    mutable proofs : proof list;
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
      let parents = Array.to_list p.Proof.parents in
      let parents = List.map
        (fun p -> match p.Proof.result with
          | Proof.Form _ -> raise NonClausalProof
          | Proof.Clause lits -> lits
        ) parents
      in
      (* all proofs of [lits] *)
      let set = match find_proofs_ lits with
        | None ->
            let set = {proofs=[]} in
            proofs_ := FV_proofs.add !proofs_ lits set;
            set
        | Some set -> set
      in
      set.proofs <- parents :: set.proofs;
      Util.debug 4 "add proof: %a" Proof.pp_notrec p;
    with NonClausalProof ->
      ()  (* ignore the proof *)

  let () =
    Signal.on C.on_proof
      (fun (lits, p) ->
        add_proof_ lits p;
        Signal.ContinueListening
      );
    ()

  (* TODO: encode proof relation into QBF, starting from the set of "roots"
      that are (applications of) clause contexts + false *)

  (* encode bigand_{i inductive} (path_constraint(i) => valid(i))
    where valid(i) = cases(i) & (not proofgraph | empty(i) | not loop(i)) *)
  let qbf_encode_top_ () =
    assert false (* TODO *)

  (* current save level *)
  let save_level_ = ref Solver.root_save_level

  (* the whole process of:
      - adding non-backtracking constraints
      - save state
      - adding backtrackable constraints *)
  let qbf_encode_enter_ () =
    (* TODO add normal constraints *)
    Util.debug 4 "ind: save QBF solver";
    save_level_ := Solver.save ();
    (* TODO add backtrackable constraints *)
    ()

  (* restoring state *)
  let qbf_encode_exit_ () =
    Util.debug 4 "ind: restore QBF solver";
    Solver.restore !save_level_;
    ()

  (* add/remove constraints before/after satisfiability checking *)
  let () =
    Signal.on Avatar.before_check_sat
      (fun () -> qbf_encode_enter_ (); Signal.ContinueListening);
    Signal.on Avatar.after_check_sat
      (fun () -> qbf_encode_exit_ (); Signal.ContinueListening);
    ()

  (** {6 Split on Inductive Constants} *)

  (* true if [t = c] where [c] is some inductive constructor *)
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
            && T.is_const t  (* TODO: terms such as allow nil(alpha) *)
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

  (* TODO: observe new cover sets, to split clauses again *)

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
              Util.debug 2 "make a case split on inductive %a" T.pp t;
              let clauses_and_lits = List.map
                (fun t' ->
                  assert (T.is_ground t');
                  let lits = [| Literal.mk_eq t t' |] in
                  let bool_lit = BoolLit.inject_lits lits in
                  let proof cc = Proof.mk_c_trivial ~theories:["induction"] cc in
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
              Util.debug 4 "split inference for %a: %a"
                T.pp t (CCList.pp C.pp) clauses;
              res := List.rev_append clauses !res
        );
    !res

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

  let on_new_context =
    let s = Signal.create () in
    Signal.on s (fun ctx ->
      (* new context: watch proofs of ctx[i]
        and ctx[t] for all [t] that is a sub-constant of [i] *)
      watch_proofs_of_clause ctx.cand_lits (`Inductive_cst ctx);
      Util.debug 2 "ind: watch %a (initialization, %a)"
        Lits.pp ctx.cand_lits T.pp ctx.cand_cst;
      CI.cover_sets ctx.cand_cst
        |> Sequence.flat_map (fun set -> T.Set.to_seq set.CI.sub_constants)
        |> Sequence.iter
          (fun t ->
            let c = CCtx.apply ctx.cand_ctx t in
            Util.debug 2 "ind: watch %a (sub-cst %a)" Lits.pp c T.pp t;
            watch_proofs_of_clause c `Sub_cst
          );
      Signal.ContinueListening
    );
    s

  (* set of subterms of [lits] that could be extruded to form a context.
   TODO: stronger restrictions? for instance:
     - if clause contains several distinct terms of same inductive type, ignore
     - if [t] is a sub_cst and its constant also occurs in [lits] (similar),
        should we extrude context?
        e.g.  in [n = s(n')] no need to extract n' nor n *)
  let subterms_candidates_for_context_ lits =
    Lits.Seq.terms lits
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.filter
        (fun t -> CI.is_inductive t || CI.is_sub_constant t)
      |> T.Seq.add_set T.Set.empty

  (* ctx [c] is now initialized. Return [true] if it wasn't initialized before *)
  let cand_ctx_initialized_ c =
    let is_new = not c.cand_initialized in
    if is_new then (
      c.cand_initialized <- true;
      Util.debug 2 "ind: clause context %a[%a] now initialized"
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
              } in
              Util.debug 2 "ind: new (initialized) context for %a: %a"
                T.pp t CCtx.pp ctx.cand_ctx;
              set := FV_cand.add !set (C.lits c) ctx;
              Signal.send on_new_context ctx;
              None
          | Some ctx ->
              let is_new = cand_ctx_initialized_ ctx in (* we just proved ctx[t] *)
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
              } in
              (* need to watch ctx[cst] until it is proved *)
              Util.debug 2 "ind: new context %a" CCtx.pp ctx;
              Signal.send on_new_context cand_ctx;
              set := FV_cand.add !set lits' cand_ctx;
              None
          | Some _ ->
              None  (* no new context *)

  (* search whether given clause [c] contains some "interesting" occurrences
     of an inductive  term. This is pretty much the main heuristic. *)
  let scan_given_for_context c =
    if C.get_flag flag_no_ctx c then [] else (
    let terms = subterms_candidates_for_context_ (C.lits c) in
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

  (* [c] (same as [lits] subsumes [lits'] which is watched with list
    of contexts [l] *)
  let _process_clause_match_watched acc c lits lits' l =
    (* remember proof *)
    let proof cc = Proof.mk_c_inference ~rule:"subsumes" cc [C.proof c] in
    let proof' = proof (CompactClause.make lits' (C.get_trail c |> C.compact_trail)) in
    add_proof_ lits' proof';
    Util.debug 2 "add proof of watched %a because of %a" Lits.pp lits' C.pp c;
    (* check whether that makes some cand_ctx initialized *)
    List.fold_left
      (fun acc elt -> match elt with
        | `Sub_cst -> acc  (* sub-constant, no initialization *)
        | `Inductive_cst cand_ctx ->
            (* [c] proves the initialization of [cand_ctx], i.e. the
              clause context applied to the corresponding
              inductive constant (rather than a sub-case) *)
            assert (CI.is_inductive cand_ctx.cand_cst);
            let is_new = cand_ctx_initialized_ cand_ctx in
            if is_new then (
              (* ctx[t] is now proved, deduce ctx[t] and add it to set of clauses *)
              let clause = C.create_a ~parents:[c]
                ~trail:(C.get_trail c) lits proof in
              (* disable subsumption for [clause] *)
              C.set_flag C.flag_persistent clause true;
              Util.debug 2 "initialized %a by subsumption from %a"
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
        Util.debug 1 "ind: declare inductive type %a" Ty.pp pattern;
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
    Util.debug 1 "register induction calculus";
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
    (* XXX: ugly, but we must do case_split before scan_extrude/proof.
      Currently we depend on Env.generate_unary applying inferences in
      the reverse order of their addition *)
    Env.add_unary_inf "induction.case_split" case_split_ind;
    ()
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let sup = Mixtbl.find ~inj:Superposition.key E.mixtbl "superposition" in
    let module Sup = (val sup : Superposition.S) in
    let module Solver = (val BoolSolver.get_qbf() : BoolSolver.QBF) in
    Util.debug 2 "created QBF solver \"%s\"" Solver.name;
    let module A = Make(E)(Sup)(Solver) in
    A.register()
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
    Util.debug 1 "Induction: requires ord=rpo6; select=NoSelection";
    Params.ord := "rpo6";   (* new default! RPO is necessary*)
    Params.select := "NoSelection";
    Extensions.register extension
  )

(* [str] describes an inductive type, under the form "foo:c1|c2|c3" where
    "foo" is the type name and "c1", "c2", "c3" are the type constructors. *)
let add_ind_type_ str =
  enable_();
  let _fail() =
    failwith "expected \"type:c1|c2|c3\" where c1,... are constructors"
  in
  match Util.str_split ~by:":" str with
  | [ty; cstors] ->
      let cstors = Util.str_split ~by:"|" cstors in
      if List.length cstors < 2 then _fail();
      (* remember to declare this type as inductive *)
      Util.debug 2 "user declares inductive type %s = %a"
        ty (CCList.pp CCString.pp) cstors;
      ind_types_ := (ty, cstors) :: !ind_types_
  | _ -> _fail()

let () =
  Params.add_opts
    [ "-induction", Arg.String add_ind_type_, "enable Induction on the given type"
    ; "-induction-depth", Arg.Set_int cover_set_depth_, "set default induction depth"
    ]
