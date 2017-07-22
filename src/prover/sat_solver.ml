
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Bridge to [MSat] prover} *)

open Logtk

module FI = Msat.Formula_intf
module SI = Msat.Solver_intf

let section = Util.Section.make ~parent:Const.section "msat"
let prof_call_msat = Util.mk_profiler "msat.call"
let stat_num_clauses = Util.mk_stat "msat.num_clauses"
let stat_num_calls = Util.mk_stat "msat.num_calls"

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
let sat_compact_ = ref false
let sat_pp_model_ = ref false

module type S = Sat_solver_intf.S

module Make(Dummy : sig end)
  : Sat_solver_intf.S
= struct
  module Lit = BBox.Lit

  type clause = Lit.t list

  (* queue of clauses waiting for being pushed into the solver *)
  let queue_ = Queue.create()

  (* flag to indicate whether it's time to re-check the model *)
  let must_check = ref false

  (* channel on which to print boolean clauses *)
  let dump_to : out_channel option ref = ref None

  (* print list of clauses on [dump_to], if it's defined *)
  let dump_l l = match !dump_to with
    | None -> ()
    | Some out ->
      let pp_lit out l = output_string out (string_of_int (Lit.to_int l)) in
      let pp_c out c =
        List.iter (fun l -> output_char out ' '; pp_lit out l) c;
        output_string out " 0\n";
      in
      List.iter (pp_c out) l;
      flush out

  let fresh_tag_ =
    let n = ref 0 in
    fun () ->
      let x = !n in
      incr n;
      x

  module ClauseTbl = CCHashtbl.Make(struct
      type t = Lit.t list
      let equal = CCList.equal Lit.equal
      let hash = (Hash.list Lit.hash)
    end)

  let clause_tbl_ : (int * proof_step) ClauseTbl.t = ClauseTbl.create 32
  let tag_to_proof_ : (int, proof_step) Hashtbl.t = Hashtbl.create 32
  let lit_tbl_ : unit Lit.Tbl.t = Lit.Tbl.create 32

  (* add clause, if not added already *)
  let add_clause_ ~proof c =
    if not (ClauseTbl.mem clause_tbl_ c) then (
      Util.incr_stat stat_num_clauses;
      (* add new clause -> check again *)
      must_check := true;
      let tag = fresh_tag_() in
      ClauseTbl.add clause_tbl_ c (tag,proof);
      Hashtbl.add tag_to_proof_ tag proof;
      List.iter (fun lit -> Lit.Tbl.replace lit_tbl_ (Lit.abs lit) ()) c;
      Queue.push ([c], proof, tag) queue_
    )

  let add_clause ~proof (c:clause) =
    let c = CCList.sort_uniq ~cmp:Lit.compare c in (* no duplicates *)
    dump_l [c];
    add_clause_ ~proof c

  let add_clauses ~proof l =
    dump_l l;
    List.iter (add_clause_ ~proof) l

  let add_clause_seq ~proof (seq:clause Sequence.t) =
    add_clauses ~proof (Sequence.to_rev_list seq)

  let result_ = ref Sat

  let res_is_unsat_ () = match !result_ with Sat -> false | Unsat _ -> true

  (* invariant:
     when result_ = Sat, only eval/eval_level are defined
     when result_ = Unsat, only unsat_core_ is defined
  *)

  let eval_fail_ _ = assert (res_is_unsat_ ()); wrong_state_ "eval"

  let eval_ = ref eval_fail_
  let eval_level_ = ref eval_fail_
  let proof_ : proof option ref = ref None
  let proved_lits_ : Lit.Set.t lazy_t ref = ref (lazy Lit.Set.empty)

  let pp_ = ref Lit.pp

  let pp_clause out c =
    Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list ~sep:" ⊔ " !pp_) c

  let pp_form_simpl out l = Util.pp_list ~sep:"" pp_clause out l

  let pp_form fmt (f,tag) =
    Format.fprintf fmt "[@[<hv>%a@]]/%d" pp_form_simpl f tag

  let last_result () = !result_
  let valuation l = !eval_ l
  let valuation_level l = !eval_level_ l
  let all_proved () = Lazy.force !proved_lits_

  let get_proof () = match !proof_ with
    | None -> assert false
    | Some p -> p

  let get_proof_opt () = !proof_

  module SatForm = struct
    include Lit
    let norm l =
      let l', b = norm l in
      l', if b then FI.Negated else FI.Same_sign
    type proof = Proof.Step.t
    let print = Lit.pp
  end

  (* Instantiate solver *)
  module S =
    Msat.Solver.Make
      (SatForm)
      (Msat.Solver.DummyTheory(SatForm))
      (struct end)

  let () =
    if !sat_log_file <> "" then (
      let oc = open_out !sat_log_file in
      let fmt = Format.formatter_of_out_channel oc in
      Msat.Log.set_debug_out fmt;
      Msat.Log.set_debug 9999;
      at_exit (fun () -> Format.pp_print_flush fmt (); close_out_noerr oc);
    )

  exception UndecidedLit = S.UndecidedLit

  type sat_clause = Lit.t list

  let bool_clause_of_sat c : sat_clause =
    S.Proof.to_list c |> List.map (fun a -> a.S.St.lit)

  (* (clause * proof * proof) -> 'a *)
  module ResTbl = CCHashtbl.Make(struct
      type t = sat_clause * Proof.t * Proof.t
      let equal (c,a1,a2)(c',b1,b2) =
        CCList.equal Lit.equal c c' &&
        Proof.S.equal a1 b1 && Proof.S.equal a2 b2
      let hash (c,a,b) =
        Hashtbl.hash
          [List.length c; Proof.S.hash a; Proof.S.hash b]
    end)

  let tbl_res = ResTbl.create 16

  let proof_of_leaf c =
    (* leaf of the proof *)
    let tag = match S.get_tag c with
      | None ->
        errorf "no tag in leaf of SAT proof (clause %a)" S.St.pp_clause c
      | Some id -> id
    in
    begin match CCHashtbl.get tag_to_proof_ tag with
      | Some step ->
        let c = bool_clause_of_sat c in
        Proof.S.mk step (Bool_clause.mk_proof_res c)
      | None -> errorf "no proof for tag %d" tag
    end

  (* convert a SAT proof into a tree of ProofStep *)
  let conv_proof_atomic_ p : proof =
    let rec aux p =
      let open S.Proof in
      match S.Proof.expand p with
        | { step = S.Proof.Lemma _; _ } ->
          errorf "SAT proof involves a lemma"
        | { conclusion=c; step = S.Proof.Resolution (p1,p2,_) } ->
          let c = bool_clause_of_sat c in
          (* atomic resolution step *)
          let q1 = aux p1 in
          let q2 = aux p2 in
          begin match ResTbl.get tbl_res (c,q1,q2) with
            | Some s -> s
            | None ->
              let parents = [Proof.Parent.from q1; Proof.Parent.from q2] in
              let step =
                Proof.Step.inference parents
                  ~rule:(Proof.Rule.mk "sat_resolution") in
              let s = Proof.S.mk step (Bool_clause.mk_proof_res c) in
              ResTbl.add tbl_res (c,q1,q2) s;
              ResTbl.add tbl_res (c,q2,q1) s;
              s
          end
        | { conclusion=c; step = _ } -> proof_of_leaf c
    in
    S.Proof.check p;
    aux p

  let conv_proof_compact_ p : proof =
    let open S.Proof in
    let leaves =
      S.Proof.fold
        (fun acc pnode -> match pnode with
           | { step = S.Proof.Lemma _; _ } ->
             errorf "SAT proof involves a lemma"
           | { step = S.Proof.Resolution (_,_,_); _ } ->
             acc (* ignore, intermediate node *)
           | { conclusion=c; step = _ } ->
             Proof.Parent.from (proof_of_leaf c) :: acc)
        [] p
    in
    let {conclusion=c;_} = S.Proof.expand p in
    let c = bool_clause_of_sat c in
    let step =
      Proof.Step.inference leaves
        ~rule:(Proof.Rule.mk "sat_resolution*")  in
    Proof.S.mk step (Bool_clause.mk_proof_res c)

  let conv_proof_ p =
    if !sat_compact_
    then conv_proof_compact_ p
    else conv_proof_atomic_ p

  let get_proof_of_lit lit =
    let b, l = valuation_level lit in
    if not b || l <> 0 then invalid_arg "get_proof_of_lit";
    let a = S.St.add_atom lit in
    match S.Proof.prove_atom a with
      | Some p -> conv_proof_ p
      | None -> assert false

  let proved_at_0 lit =
    if S.true_at_level0 lit then Some true
    else if S.true_at_level0 (Lit.neg lit) then Some false
    else None

  let get_proved_lits (): Lit.Set.t =
    Lit.Tbl.to_seq lit_tbl_
    |> Sequence.filter_map
      (fun (lit,_) -> match proved_at_0 lit with
         | Some true -> Some lit
         | Some false -> Some (Lit.neg lit)
         | None -> None)
    |> Lit.Set.of_seq

  let pp_model_ (): unit = match last_result() with
    | Sat ->
      let m =
        Lit.Tbl.keys lit_tbl_
        |> Sequence.map (fun l -> l, valuation l)
        |> Sequence.to_rev_list
      in
      let pp_pair out (l,b) = Format.fprintf out "(@[%B %a@])" b BBox.pp l in
      Format.printf "(@[<hv2>bool_model@ %a@])@." (Util.pp_list ~sep:" " pp_pair) m
    | Unsat _ -> ()

  (* call [S.solve()] in any case, and enforce invariant about eval/unsat_core *)
  let check_unconditional_ () =
    (* reset functions, so they will fail if called in the wrong state *)
    proof_ := None;
    eval_ := eval_fail_;
    eval_level_ := eval_fail_;
    Util.incr_stat stat_num_calls;
    (* add pending clauses *)
    while not (Queue.is_empty queue_) do
      let c, proof, tag = Queue.pop queue_ in
      Util.debugf ~section 4 "@[<hv2>assume@ @[%a@]@ proof: %a@]"
        (fun k->k pp_form (c,tag) Proof.Step.pp proof);
      S.assume ~tag c
    done;
    (* solve *)
    begin match S.solve () with
      | S.Sat s ->
        eval_ := s.SI.eval;
        eval_level_ := s.SI.eval_level;
        proved_lits_ := lazy (get_proved_lits ());
        result_ := Sat;
      | S.Unsat us ->
        let p = us.SI.get_proof ()  |> conv_proof_ in
        result_ := Unsat p;
        proof_ := Some p;
    end;
    !result_

  let check_ full =
    if full || !must_check
    then (
      assert (full || not (Queue.is_empty queue_));
      Util.debug ~section 5 "check_real";
      must_check := false;
      check_unconditional_ ()
    )
    else !result_

  (* initialize eval/eval_level to enforce invariant *)
  let () =
    let res = check_unconditional_ () in
    assert (res = Sat)

  let check ~full () = Util.with_prof prof_call_msat check_ full

  let set_printer pp = pp_ := pp

  let setup () =
    if !sat_dump_file_ <> "" then (
      Util.debugf ~section 1 "dump SAT clauses to `%s`" (fun k->k !sat_dump_file_);
      try
        let oc = open_out !sat_dump_file_ in
        dump_to := Some oc;
        at_exit (fun () -> close_out_noerr oc);
      with e ->
        Util.warnf "@[<2>could not open `%s`:@ %s@]"
          !sat_dump_file_ (Printexc.to_string e);
    );
    if !sat_pp_model_ then at_exit pp_model_;
    ()
end

let set_compact b = sat_compact_ := b

let () =
  Params.add_opts
    [ "--sat-dump", Arg.Set_string sat_dump_file_, " output SAT problem(s) into <file>"
    ; "--sat-log", Arg.Set_string sat_log_file, " output SAT logs into <file>"
    ; "--compact-sat", Arg.Set sat_compact_, " compact SAT proofs"
    ; "--no-compact-sat", Arg.Clear sat_compact_, " do not compact SAT proofs"
    ; "--pp-sat-model", Arg.Set sat_pp_model_, " print SAT model on exit"
    ]
