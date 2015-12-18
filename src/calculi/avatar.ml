
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic Splitting à la Avatar} *)

open Logtk

module T = Logtk.FOTerm
module Lit = Literal
module Util = Logtk.Util

type 'a printer = Format.formatter -> 'a -> unit

let section = Logtk.Util.Section.make ~parent:Const.section "avatar"
(** {2 Avatar} *)

let prof_splits = Util.mk_profiler "avatar.split"
let prof_check = Util.mk_profiler "avatar.check"

let stat_splits = Util.mk_stat "avatar.splits"

module type S = sig
  module E : Env.S
  module Solver : Sat_solver.S

  val split : E.multi_simpl_rule
  (** Split a clause into components *)

  val check_empty : E.unary_inf_rule
  (** Forbid empty clauses with trails, i.e. adds the negation of their
      trails to the SAT-solver *)

  val before_check_sat : unit Signal.t
  val after_check_sat : unit Signal.t

  val filter_absurd_trails : (Trail.t -> bool) -> unit
  (** [filter_trails f] calls [f] on every trail associated with the empty
      clause. If [f] returns [false], the trail is ignored, otherwise
      it's negated and sent to the SAT solver *)

  val check_satisfiability : E.generate_rule
  (** Checks  that the SAT context is still valid *)

  val save_clause : tag:int -> E.C.t -> unit
  (** Map the tag to the clause *)

  val get_clause : tag:int -> E.C.t option
  (** Recover clause from the tag, if any *)

  val introduce_cut :
    Literals.t ->
    (CompactClause.t -> Proof.t) ->
    E.C.t list * E.Ctx.BoolLit.t
  (** Introduce a cut on the given formula *)

  val register : unit -> unit
  (** Register inference rules to the environment *)
end

module Make(E : Env.S)(Sat : Sat_solver.S) = struct
  module E = E
  module Ctx = E.Ctx
  module C = E.C
  module BoolLit = Ctx.BoolLit
  module Solver = Sat

  let _pp_bclause out lits =
    Format.fprintf out "%a" (Util.pp_list ~sep:" ⊔ " BoolLit.pp) lits

  (* map ID -> clause *)
  let id_to_clause_ = Hashtbl.create 24

  let save_clause ~tag c = Hashtbl.replace id_to_clause_ tag c
  let get_clause ~tag = CCHashtbl.get id_to_clause_ tag

  (* union-find that maps vars to list of literals, used for splitting *)
  module UF = UnionFind.Make(struct
      type key = T.var
      type value = Lit.t list
      let equal = HVar.equal
      let hash = HVar.hash
      let zero = []
      let merge = List.rev_append
    end)

  module LitSet = Sequence.Set.Make(Lit)

  let _infer_split c =
    let lits = C.lits c in
    (* maps each variable to a list of literals. Sets can be merged whenever
       two variables occur in the same literal.  *)
    let uf_vars =
      C.Seq.vars c
      |> T.VarSet.of_seq
      |> T.VarSet.to_list
      |> UF.create
    (* set of ground literals (each one is its own component) *)
    and cluster_ground = ref LitSet.empty in

    (* literals belong to either their own ground component, or to every
        sets in [uf_vars] associated to their variables *)
    Array.iter
      (fun lit ->
         let v_opt = Lit.Seq.vars lit |> Sequence.head in
         match v_opt with
         | None -> (* ground, lit has its own component *)
             cluster_ground := LitSet.add lit !cluster_ground
         | Some v ->
             (* merge other variables of the literal with [v] *)
             Lit.Seq.vars lit
             |> Sequence.iter
               (fun v' ->
                  UF.add uf_vars v' [lit];  (* lit is in the equiv class of [v'] *)
                  UF.union uf_vars v v'
               );
      ) lits;

    (* now gather all the components as a literal list list *)
    let components = ref [] in
    LitSet.iter (fun lit -> components := [lit] :: !components) !cluster_ground;
    UF.iter uf_vars (fun _ comp -> components := comp :: !components);

    match !components with
    | [] -> assert (Array.length lits=0); None
    | [_] -> None
    | _::_ ->
        (* do a simplification! *)
        Util.incr_stat stat_splits;
        let clauses_and_names = List.map
            (fun lits ->
               let proof cc = Proof.mk_c_esa ~rule:"split" cc [C.proof c] in
               let lits = Array.of_list lits in
               let bool_name = BoolLit.inject_lits lits in
               let trail =
                 C.get_trail c
                 |> Trail.add bool_name
               in
               let c = C.create_a ~parents:[c] ~trail lits proof in
               C.set_bool_name c bool_name;
               c, bool_name
            ) !components
        in
        let clauses, bool_clause = List.split clauses_and_names in
        Util.debugf ~section 4 "@[split of @[%a@]@ yields @[%a@]@]"
          (fun k->k C.pp c (Util.pp_list C.pp) clauses);
        (* add boolean constraint: trail(c) => bigor_{name in clauses} name *)
        let bool_guard =
          C.get_trail c
          |> Trail.to_list
          |> List.map Bool_lit.neg in
        let bool_clause = List.append bool_clause bool_guard in
        save_clause ~tag:(C.id c) c;
        Sat.add_clause ~tag:(C.id c) bool_clause;
        Util.debugf ~section 4 "@[constraint clause is @[%a@]@]"
          (fun k->k _pp_bclause bool_clause);
        (* return the clauses *)
        Some clauses

  (* Hyper-splitting *)
  let split c =
    Util.enter_prof prof_splits;
    let res = if Array.length (C.lits c) <= 1
      then None
      else _infer_split c
    in
    Util.exit_prof prof_splits;
    res

  let filter_absurd_trails_ = ref (fun _ -> true)
  let filter_absurd_trails f = filter_absurd_trails_ := f

  (* if c.lits = [], negate c.trail *)
  let check_empty c =
    if Array.length (C.lits c) = 0 && !filter_absurd_trails_ (C.get_trail c)
    then (
      assert (not (Trail.is_empty (C.get_trail c)));
      let b_clause =
        C.get_trail c
        |> Trail.to_list
        |> List.map Bool_lit.neg
      in
      Util.debugf ~section 4 "@[negate trail of @[%a@] (id %d)@ with @[%a@]@]"
        (fun k->k C.pp c (C.id c) _pp_bclause b_clause);
      save_clause ~tag:(C.id c) c;
      Sat.add_clause ~tag:(C.id c) b_clause;
    );
    [] (* never infers anything! *)

  (* generic mechanism for adding a clause
      and make a lemma out of it, including Skolemization, etc. *)
  let introduce_cut lits proof : C.t list * BoolLit.t =
    let box = BoolLit.inject_lits lits in
    (* positive clause *)
    let c_pos =
      C.create_a ~trail:(Trail.singleton box) lits proof
    in
    let c_neg = assert false
    (* FIXME:
      - gather variables
      - skolemize them with fresh (inductive?) constants
      - map each [lit] to [negate subst(lit) <- [not box]]

      Array.map
        (fun lit ->
      PFormula.create (F.Base.not_ f) (Proof.mk_f_trivial f)
      |> PFormula.Set.singleton
      |> E.cnf
      |> C.CSet.to_list
      |> List.map
        (fun c ->
           let trail = C.Trail.singleton (BoolLit.neg box) in
           C.create_a ~trail (C.lits c) proof
        )
    *)
    in
    c_pos :: c_neg, box

  (* introduce a cut for each lemma proposed by the meta-prover *)
  let introduce_meta_lemmas (q:MetaProverState.lemma Queue.t) _given =
    (* translate all new lemmas into cuts *)
    let clauses =
      Sequence.of_queue q
      |> Sequence.flat_map
        (fun (cc, proof) ->
           assert (CompactClause.trail cc = []);
           let proof = Proof.adapt_c proof in
           let new_clauses, _box = introduce_cut (CompactClause.lits cc) proof in
           Util.debugf ~section 2 "@[<hv2>introduce cut from meta lemma:@,%a@]"
             (fun k->k (CCList.print C.pp) new_clauses);
           Sequence.of_list new_clauses
        )
      |> Sequence.to_rev_list
    in
    Queue.clear q;
    clauses

  let before_check_sat = Signal.create()
  let after_check_sat = Signal.create()

  (* Just check the solver *)
  let check_satisfiability () =
    Util.enter_prof prof_check;
    Signal.send before_check_sat ();
    let res = match Sat.check ()  with
      | Sat_solver.Sat ->
          Util.debug ~section 3 "SAT-solver reports \"SAT\"";
          []
      | Sat_solver.Unsat ->
          Util.debug ~section 1 "SAT-solver reports \"UNSAT\"";
          let premises =
            let l = Sequence.to_rev_list Sat.unsat_core in
            Util.debugf ~section 3 "unsat core:@ @[%a@]" (fun k->k (CCList.print CCInt.print) l);
            l
            |> CCList.filter_map (fun tag -> get_clause ~tag)
            |> List.map C.proof
          in
          let proof cc = Proof.mk_c_inference
              ~rule:"sat" ~theories:["sat"] cc premises in
          let c = C.create [] proof in
          [c]
    in
    Signal.send after_check_sat ();
    Util.exit_prof prof_check;
    res

  let register () =
    Util.debug ~section:Const.section 2 "register extension Avatar";
    Sat.set_printer BoolLit.pp;
    E.add_multi_simpl_rule split;
    E.add_unary_inf "avatar_check_empty" check_empty;
    E.add_generate "avatar_check_sat" check_satisfiability;
    (* meta lemmas *)
    begin try
        let meta = MetaProverState.get_global () in
        Util.debug ~section 1 "found meta-prover, watch for lemmas";
        let q = Queue.create () in
        Signal.on (MetaProverState.on_lemma meta)
          (fun lemma ->
             Util.debugf ~section 2 "@[obtained lemma @[%a@]@ from meta-prover@]"
               (fun k->k CompactClause.pp (fst lemma));
             Queue.push lemma q;
             Signal.ContinueListening
          );
        E.add_unary_inf "avatar_meta_lemmas" (introduce_meta_lemmas q);
      with Not_found ->
        Util.debug ~section 1 "could not find meta-prover";
        ()
    end;
    ()
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    Util.debug 1 "create new SAT solver";
    let module Sat = Sat_solver.Make(struct end) in
    let module A = Make(E)(Sat) in
    A.register()
  in
  Extensions.({default with name="avatar"; actions=[Do action]})

let _enabled = ref false
let _enable_avatar () =
  if not !_enabled then (
    _enabled := true;
    Extensions.register extension
  )

let () =
  Params.add_opts
    [ "--avatar",
        Arg.Unit _enable_avatar,
        " enable Avatar-like splitting (based on SAT)"
    ]
