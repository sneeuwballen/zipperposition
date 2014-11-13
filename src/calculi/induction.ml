
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

let _ind_types = ref []
let _depth = ref 1

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

    let remove fv lits = FV.remove fv lits

    let find fv lits =
      FV.retrieve_alpha_equiv fv (Lits.Seq.abstract lits) ()
        |> Sequence.map2 (fun _ x -> x)
        |> Sequence.filter_map
          (fun (lits', x) ->
            if Lits.are_variant lits lits'
              then Some x else None
          )
        |> Sequence.head

    (* find clauses in [fv] that are subsumes by [lits] *)
    let find_subsumed_by fv lits =
      FV.retrieve_subsumed fv (Lits.Seq.abstract lits) ()
        |> Sequence.map2 (fun _ x -> x)
        |> Sequence.filter_map
          (fun (lits', x) ->
            if Sup.subsumes lits lits'
              then Some x else None
          )

    (* find clauses in [fv] that subsume [lits] *)
    let find_subsuming fv lits =
      FV.retrieve_subsuming fv (Lits.Seq.abstract lits) ()
        |> Sequence.map2 (fun _ x -> x)
        |> Sequence.filter_map
          (fun (lits', x) ->
            if Sup.subsumes lits' lits
              then Some x else None
          )
  end

  let level0 = Solver.level0
  let level1 = Solver.push Qbf.Forall []

  (** {6 Proof Relation} *)

  (* one way to prove a clause: parent clauses and the union of their trails *)
  type proof = CompactClause.t list

  (* a set of proofs *)
  type proof_set = {
    mutable proofs : proof list;
  }

  module FV_proofs = FVMap(struct
    type t = proof_set
  end)

  (* maps clauses to a list of their known proofs *)
  let proofs_ = ref (FV_proofs.empty ())

  (* find all known proofs of the given lits *)
  let find_proofs_ lits = FV_proofs.find !proofs_ lits

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
      set.proofs <- parents :: set.proofs
    with NonClausalProof ->
      ()  (* ignore the proof *)

  let () =
    Signal.on C.on_proof
      (fun (lits, p) ->
        add_proof_ lits p;
        Signal.ContinueListening
      );
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
            && T.is_const t  (* TODO: allow nil(alpha) *)
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
          match CI.cover_set ~depth:!_depth t with
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

  (** {6 Clause Contexts} *)

  (* a candidate clause context *)
  type candidate_context = {
    mutable cand_initialized : bool; (* is ctx[I] proved yet? *)
    cand_ctx : CCtx.t; (* the ctx itself *)
    cand_cst : T.t;  (* the inductive constant *)
    mutable cand_sub_cst : T.t list;  (* inductive sub-cases on which ctx is provable *)
  }

  module FV_cand = FVMap(struct
    type t = T.t * candidate_context
  end)

  type candidate_context_set = FV_cand.t ref

  (* maps each inductive constant to
      set(clause contexts that are candidate for induction) *)
  let candidates_ : candidate_context_set T.Tbl.t = T.Tbl.create 255

  (* set of contexts we watch for *)
  let watched_ : FV_cand.t ref = ref (FV_cand.empty())

  (* candidates for a term *)
  let find_candidates_ t =
    try T.Tbl.find candidates_ t
    with Not_found ->
      let set = ref (FV_cand.empty ()) in
      T.Tbl.add candidates_ t set;
      set

  (* set of subterms of [lits] that could be extruded to form a context.
    TODO: if [t] is a sub_cst and its constant also occurs in [lits],
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
    assert (not c.cand_initialized);
    Util.debug 2 "ind: clause context %a[%a] now initialized"
      CCtx.pp c.cand_ctx T.pp c.cand_cst;
    let is_new = c.cand_initialized in
    if is_new then c.cand_initialized <- true;
    is_new

  let cand_ctx_add_sub_cst_ c t =
    (* remember that ctx[t] is proved, can be useful later *)
    if not (CCList.Set.mem ~eq:T.eq t c.cand_sub_cst)
      then (
        Util.debug 2 "ind: clause context {%a} now proved for subcase %a"
          CCtx.pp c.cand_ctx T.pp t;
        c.cand_sub_cst <- t :: c.cand_sub_cst;
      )

  (* see whether (ctx,t) is of interest. *)
  let on_context_ c ctx t =
    let set = find_candidates_ t in
    (* if [t] is an inductive constant, ctx is enabled! *)
    if CI.is_inductive t
      then (
        match FV_cand.find !set (C.lits c) with
          | None ->
              let ctx = {
                cand_initialized=true;
                cand_ctx=ctx;
                cand_cst=t;
                cand_sub_cst=[]
              } in
              Util.debug 2 "ind: new (initialized) context for %a: %a"
                T.pp t CCtx.pp ctx.cand_ctx;
              set := FV_cand.add !set (C.lits c) (t,ctx);
              None
          | Some (t',ctx) ->
              assert (T.eq t t');
              let is_new = cand_ctx_initialized_ ctx in (* we just proved ctx[t] *)
              if is_new then Some c else None
      ) else
        (* [t] is a subterm of the case [t'] of an inductive [cst] *)
        let cst, t' = CI.inductive_cst_of_sub_cst t in
        match FV_cand.find !set (C.lits c) with
          | None ->
              let ctx = {
                cand_initialized=false;
                cand_ctx=ctx;
                cand_cst=t;
                cand_sub_cst=[]
              } in
              (* need to watch ctx[cst] until it is proved *)
              let lits' = CCtx.apply ctx.cand_ctx cst in
              Util.debug 2 "ind: now watching clause %a" Lits.pp lits';
              watched_ := FV_cand.add !watched_ lits' (cst,ctx);
              set := FV_cand.add !set (C.lits c) (t,ctx);
              None
          | Some (t_bis,c) ->
              assert (T.eq t t_bis);
              cand_ctx_add_sub_cst_ c t;
              None

  (* search whether given clause [c] contains some "interesting" occurrences
     of an inductive  term. This is pretty much the main heuristic *)
  let scan_given_for_context c =
    let terms = subterms_candidates_for_context_ (C.lits c) in
    T.Set.fold
      (fun t acc ->
        let lits = C.lits c in
        let ctx = CCtx.extract_exn lits t in
        match on_context_ c ctx t with
          | None -> acc
          | Some c -> c :: acc
      ) terms []

  (* search whether [c] subsumes some watched clause
     if [C[i]] subsumed, where [i] is an inductive constant:
      - "infer" the clause [C[i]]
      - monitor for [C[i']] for every [i'] sub-case of [i]
        - if such [C[i']] is detected, add its proof relation to QBF. Will
          be checked by avatar.check_satisfiability. Watch for proofs
          of [C[i']], they could evolve!! *)
  let scan_given_for_proof c =
    FV_cand.find_subsumed_by !watched_ (C.lits c)
      |> Sequence.fold
        (fun acc (t, cand_ctx) ->
           if CI.is_inductive t
           then
             let is_new = cand_ctx_initialized_ cand_ctx in
             if is_new then (
               (* ctx[t] is now proved, deduce ctx[t] and add it to set of clauses *)
               let lits = CCtx.apply cand_ctx.cand_ctx t in
               let proof cc = Proof.mk_c_inference ~rule:"subsumes" cc [C.proof c] in
               let clause = C.create_a ~parents:[c] ~trail:(C.get_trail c) lits proof in
               (* disable subsumption for [clause] *)
               C.set_flag C.flag_persistent clause true;
               clause :: acc
             ) else acc
          else (
             assert (CI.is_sub_constant t);
             cand_ctx_add_sub_cst_ cand_ctx t;
             acc
           )
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

  (* TODO: ensure inductive_cst > sub_cst > ind constructors
    in the term ordering *)

  let register() =
    Util.debug 1 "register induction calculus";
    declare_types_ !_ind_types;
    Solver.set_printer BoolLit.print;
    (* avatar rules *)
    Env.add_multi_simpl_rule Avatar.split;
    Env.add_unary_inf "avatar.check_empty" Avatar.check_empty;
    Env.add_generate "avatar.check_sat" Avatar.check_satisfiability;
    (* induction rules *)
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
  in
  Extensions.({default with name="induction"; actions=[Do action]})

let enabled_ = ref false
let enable_ () =
  if not !enabled_ then (
    enabled_ := true;
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
      _ind_types := (ty, cstors) :: !_ind_types
  | _ -> _fail()

let () =
  Params.add_opts
    [ "-induction", Arg.String add_ind_type_, "enable Induction on the given type"
    ; "-induction-depth", Arg.Set_int _depth, "set default induction depth"
    ]
