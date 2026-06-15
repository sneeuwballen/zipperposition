(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Bridge to [MSat] prover} *)

open Logtk
module SI = Msat.Solver_intf

let section = Util.Section.make ~parent:Const.section "msat"
let stat_num_clauses = Util.mk_stat "msat.num_clauses"
let stat_num_calls = Util.mk_stat "msat.num_calls"

module Lit = BBox.Lit

type clause = Lit.t list
type proof_step = Sat_solver_intf.proof_step
type proof = Sat_solver_intf.proof

type result = Sat_solver_intf.result =
  | Sat
  | Unsat of proof

exception WrongState of string

let wrong_state_ msg = raise (WrongState msg)
let errorf msg = Util.errorf ~where:"sat_solver" msg
let sat_dump_file_ = ref ""
let sat_log_file = ref ""
let sat_compact_ = ref true
let sat_pp_model_ = ref false

module Sat_solver_intf = Sat_solver_intf

(* Instantiate solver *)
module Solver = Msat.Make_pure_sat (struct
  module Formula = struct
    include BBox.Lit

    let norm (l : t) : t * _ =
      let l', b = norm l in
      ( l',
        if b then
          SI.Negated
        else
          SI.Same_sign )
  end

  type proof = Sat_solver_intf.proof_step
end)

module ClauseTbl = CCHashtbl.Make (struct
  type t = Lit.t list

  let equal = CCList.equal Lit.equal
  let hash = Hash.list Lit.hash
end)

type t = {
  solver: Solver.t ref;
  queue_: (Lit.t list list * proof_step) Queue.t;
  must_check: bool ref;
  dump_to: out_channel option ref;
  result_: result ref;
  eval_: (Lit.t -> bool) ref;
  eval_level_: (Lit.t -> bool * int) ref;
  proof_: proof option ref;
  proved_lits_: Lit.Set.t lazy_t ref;
  pp_: Lit.t CCFormat.printer ref;
  clause_tbl_: unit ClauseTbl.t;
  lit_tbl_: unit Lit.Tbl.t;
}

let eval_fail_ _ = wrong_state_ "eval"

let pp_clause t out c =
  Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list ~sep:" ⊔ " !(t.pp_)) c

let pp_form_simpl t out l = Util.pp_list ~sep:"" (pp_clause t) out l

let pp_form t fmt f : unit =
  Format.fprintf fmt "[@[<hv>%a@]]" (pp_form_simpl t) f

(* print list of clauses on [dump_to], if it's defined *)
let dump_l t l =
  match !(t.dump_to) with
  | None -> ()
  | Some out ->
    let pp_lit out l = output_string out (string_of_int (Lit.to_int l)) in
    let pp_c out c =
      List.iter
        (fun l ->
          output_char out ' ';
          pp_lit out l)
        c;
      output_string out " 0\n"
    in
    List.iter (pp_c out) l;
    flush out

let init_from_log () =
  if !sat_log_file <> "" then (
    let oc = open_out !sat_log_file in
    let fmt = Format.formatter_of_out_channel oc in
    Msat.Log.set_debug_out fmt;
    Msat.Log.set_debug 9999;
    at_exit (fun () ->
        Format.pp_print_flush fmt ();
        close_out_noerr oc)
  )

let () = init_from_log ()

(* add clause, if not added already *)
let add_clause_ t ~proof c =
  let open Msat in
  if not (ClauseTbl.mem t.clause_tbl_ c) then (
    Util.incr_stat stat_num_clauses;
    (* add new clause -> check again *)
    t.must_check := true;
    ClauseTbl.add t.clause_tbl_ c ();
    List.iter (fun lit -> Lit.Tbl.replace t.lit_tbl_ (Lit.abs lit) ()) c;
    Queue.push ([ c ], proof) t.queue_
  )

let add_clause t ~proof (c : Lit.t list) =
  let c = CCList.sort_uniq ~cmp:Lit.compare c in
  dump_l t [ c ];
  add_clause_ t ~proof c

let add_clauses t ~proof l =
  dump_l t l;
  List.iter (add_clause_ t ~proof) l

let add_clause_seq t ~proof (seq : Lit.t list Iter.t) =
  add_clauses t ~proof (Iter.to_rev_list seq)

let last_result t = !(t.result_)
let valuation t l = !(t.eval_) l
let valuation_level t l = !(t.eval_level_) l
let all_proved t = Lazy.force !(t.proved_lits_)

let get_proof t =
  match !(t.proof_) with
  | None -> assert false
  | Some p -> p

let get_proof_opt t = !(t.proof_)

exception UndecidedLit = Solver.UndecidedLit

type sat_clause = Lit.t list

let bool_clause_of_sat (c : Solver.Clause.t) : sat_clause =
  Solver.Clause.atoms_l c |> List.map Solver.Atom.formula

let proof_of_leaf c step : proof =
  let c = bool_clause_of_sat c in
  Proof.S.mk step (Bool_clause.mk_proof_res c)

(* convert a SAT proof into a tree of ProofStep *)
let conv_proof_atomic_ p : proof =
  let rec aux p =
    let module P = Solver.Proof in
    match P.expand p with
    | { P.step = P.Lemma _; _ } -> errorf "SAT proof involves a lemma"
    | { P.step = P.Assumption; _ } -> errorf "SAT proof involves an assumption"
    | { P.step = P.Duplicate (c', _); _ } -> aux c'
    | { P.conclusion = c; step = P.Hyper_res { P.hr_init; hr_steps } } ->
      let c = bool_clause_of_sat c in
      (* atomic resolution step *)
      let q1 = aux hr_init in
      let q2 = List.map (fun (_, p) -> aux p) hr_steps in
      let parents = Proof.Parent.from q1 :: List.map Proof.Parent.from q2 in
      let step =
        Proof.Step.inference parents ~rule:(Proof.Rule.mk "sat_resolution")
      in
      Proof.S.mk step (Bool_clause.mk_proof_res c)
    | { P.conclusion = c; step = P.Hypothesis step; _ } -> proof_of_leaf c step
  in
  Solver.Proof.check p;
  aux p

let conv_proof_compact_ p : proof =
  let module P = Solver.Proof in
  let leaves =
    P.fold
      (fun acc pnode ->
        match pnode with
        | { P.step = P.Lemma _; _ } -> errorf "SAT proof involves a lemma"
        | { P.step = P.Assumption; _ } ->
          errorf "SAT proof involves an assumption"
        | { P.step = P.Hyper_res _ | P.Duplicate _; _ } ->
          acc (* ignore, intermediate node *)
        | { P.conclusion = c; step = P.Hypothesis step; _ } ->
          Proof.Parent.from (proof_of_leaf c step) :: acc)
      [] p
  in
  let { P.conclusion = c; _ } = P.expand p in
  let c = bool_clause_of_sat c in
  let step =
    Proof.Step.inference leaves ~rule:(Proof.Rule.mk "sat_resolution*")
  in
  Proof.S.mk step (Bool_clause.mk_proof_res c)

let conv_proof_ p =
  if !sat_compact_ then
    conv_proof_compact_ p
  else
    conv_proof_atomic_ p

let rec get_proved_lits_ t : Lit.Set.t =
  Lit.Tbl.to_iter t.lit_tbl_
  |> Iter.filter_map (fun (lit, _) ->
         match proved_at_0 t lit with
         | Some true -> Some lit
         | Some false -> Some (Lit.neg lit)
         | None -> None)
  |> Lit.Set.of_iter

(* call [S.solve()] in any case, and enforce invariant about eval/unsat_core *)
and check_unconditional_ t =
  (* reset functions, so they will fail if called in the wrong state *)
  t.proof_ := None;
  t.eval_ := eval_fail_;
  t.eval_level_ := eval_fail_;
  Util.incr_stat stat_num_calls;
  (* add pending clauses *)
  while not (Queue.is_empty t.queue_) do
    let c, proof = Queue.pop t.queue_ in
    Util.debugf ~section 4 "@[<hv2>assume@ @[%a@]@ proof: %a@]" (fun k ->
        k (pp_form t) c Proof.Step.pp proof);
    Solver.assume !(t.solver) c proof
  done;
  (* solve *)
  Util.debug ~section 4 "solve...";
  let res = Solver.solve !(t.solver) in
  Util.debug ~section 4 "solve done.";
  (match res with
  | Solver.Sat s ->
    t.eval_ := s.SI.eval;
    t.eval_level_ := s.SI.eval_level;
    t.proved_lits_ := lazy (get_proved_lits_ t);
    t.result_ := Sat
  | Solver.Unsat us ->
    let p = us.SI.get_proof () |> conv_proof_ in
    t.result_ := Unsat p;
    t.proof_ := Some p);
  !(t.result_)

(* proved_at_0 depends on check_unconditional_ for its solver ref *)
and proved_at_0 t lit =
  let a = Solver.make_atom !(t.solver) lit in
  if Solver.true_at_level0 !(t.solver) a then
    Some true
  else if Solver.true_at_level0 !(t.solver) (Solver.Atom.neg a) then
    Some false
  else
    None

let get_proof_of_lit t lit =
  let module P = Solver.Proof in
  let b, l = valuation_level t lit in
  if (not b) || l <> 0 then invalid_arg "get_proof_of_lit";
  let a = Solver.make_atom !(t.solver) lit in
  match P.prove_atom a with
  | Some p -> conv_proof_ p
  | None -> assert false

let check t ~full () =
  if full || !(t.must_check) then (
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "msat.check" in
    assert (full || not (Queue.is_empty t.queue_));
    Util.debug ~section 5 "check_real";
    t.must_check := false;
    check_unconditional_ t
  ) else
    !(t.result_)

let set_printer t pp = t.pp_ := pp

let pp_model_ t =
  match last_result t with
  | Sat ->
    let m =
      Lit.Tbl.keys t.lit_tbl_
      |> Iter.map (fun l -> l, valuation t l)
      |> Iter.to_rev_list
    in
    let pp_pair out (l, b) = Format.fprintf out "(@[%B %a@])" b BBox.pp l in
    Format.printf "(@[<hv2>bool_model@ %a@])@."
      (Util.pp_list ~sep:" " pp_pair)
      m
  | Unsat _ -> ()

let setup t =
  if !sat_dump_file_ <> "" then (
    Util.debugf ~section 1 "dump SAT clauses to `%s`" (fun k ->
        k !sat_dump_file_);
    try
      let oc = open_out !sat_dump_file_ in
      t.dump_to := Some oc;
      at_exit (fun () -> close_out_noerr oc)
    with e ->
      Util.warnf "@[<2>could not open `%s`:@ %s@]" !sat_dump_file_
        (Printexc.to_string e)
  );
  if !sat_pp_model_ then at_exit (fun () -> pp_model_ t);
  ()

let clear t ?(size = `Big) () =
  Queue.clear t.queue_;
  t.must_check := true;
  ClauseTbl.clear t.clause_tbl_;
  Lit.Tbl.clear t.lit_tbl_;
  t.solver := Solver.create ~size ()

let create () =
  let solver = ref (Solver.create ~size:`Big ()) in
  let result =
    {
      solver;
      queue_ = Queue.create ();
      must_check = ref false;
      dump_to = ref None;
      result_ = ref Sat;
      eval_ = ref (fun _ -> assert false);
      eval_level_ = ref (fun _ -> assert false);
      proof_ = ref None;
      proved_lits_ = ref (lazy Lit.Set.empty);
      pp_ = ref Lit.pp;
      clause_tbl_ = ClauseTbl.create 32;
      lit_tbl_ = Lit.Tbl.create 32;
    }
  in
  (* initialize: check once to set up eval_/eval_level_ *)
  let _ = check_unconditional_ result in
  result

let set_compact b = sat_compact_ := b

(* Capture outer functions for Make() backward compat functor *)
let _add_clause = add_clause
let _add_clauses = add_clauses
let _add_clause_seq = add_clause_seq
let _check = check
let _last_result = last_result
let _valuation = valuation
let _valuation_level = valuation_level
let _proved_at_0 = proved_at_0
let _all_proved = all_proved
let _set_printer = set_printer
let _get_proof = get_proof
let _get_proof_opt = get_proof_opt
let _get_proof_of_lit = get_proof_of_lit
let _setup = setup
let _clear = clear

(* Backward-compatible generative functor for avatar/qle/pred_elim *)
module Make () = struct
  module Lit = BBox.Lit

  exception UndecidedLit = Solver.UndecidedLit

  type clause = Lit.t list

  type nonrec result = Sat_solver_intf.result =
    | Sat
    | Unsat of proof

  let _st = create ()
  let add_clause = _add_clause _st
  let add_clauses = _add_clauses _st
  let add_clause_seq = _add_clause_seq _st
  let check = _check _st
  let last_result () = _last_result _st
  let valuation = _valuation _st
  let valuation_level = _valuation_level _st
  let proved_at_0 = _proved_at_0 _st
  let all_proved () = _all_proved _st
  let set_printer = _set_printer _st
  let get_proof () = _get_proof _st
  let get_proof_opt () = _get_proof_opt _st
  let get_proof_of_lit = _get_proof_of_lit _st
  let setup () = _setup _st
  let clear = _clear _st
end

let () =
  Params.add_opts
    [
      ( "--sat-dump",
        Arg.Set_string sat_dump_file_,
        " output SAT problem(s) into <file>" );
      "--sat-log", Arg.Set_string sat_log_file, " output SAT logs into <file>";
      "--compact-sat", Arg.Set sat_compact_, " compact SAT proofs";
      "--no-compact-sat", Arg.Clear sat_compact_, " do not compact SAT proofs";
      "--pp-sat-model", Arg.Set sat_pp_model_, " print SAT model on exit";
    ]
