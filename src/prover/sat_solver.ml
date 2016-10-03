
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Bridge to [MSat] prover} *)

open Libzipperposition

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

  let add_clause_ ~proof c =
    Util.incr_stat stat_num_clauses;
    (* if the clause has only negative lits: check again *)
    if List.for_all (fun lit -> not (Lit.sign lit)) c
    then must_check := true;
    Queue.push ([c], proof, fresh_tag_ ()) queue_

  let add_clause ~proof (c:clause) =
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

  let pp_ = ref Lit.pp

  let pp_clause out c =
    Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list ~sep:" ⊔ " !pp_) c

  let pp_form_simpl out l = Util.pp_list ~sep:"" pp_clause out l

  let pp_form fmt (f,tag) =
    Format.fprintf fmt "[@[<hv>%a@]]/%d" pp_form_simpl f tag

  let last_result () = !result_
  let valuation l = !eval_ l
  let valuation_level l = !eval_level_ l

  let get_proof () = match !proof_ with
    | None -> assert false
    | Some p -> p


  (* map tags to the associated proof *)
  let tag_to_proof_ : (int, proof_step) Hashtbl.t = Hashtbl.create 32

  module SatForm = struct
    include Lit
    let norm l =
      let l', b = norm l in
      l', if b then FI.Negated else FI.Same_sign
    type proof = ProofStep.t
    let fresh () = Lit.make (Lit.payload Lit.dummy)
    let print = Lit.pp
  end

  (* Instantiate solver *)
  module S =
    Msat.Solver.Make
      (SatForm)
      (Msat.Solver.DummyTheory(SatForm))
      (struct end)

  exception UndecidedLit = S.UndecidedLit

  type sat_clause = Lit.t list

  let bool_clause_of_sat c : sat_clause =
    S.Proof.to_list c |> List.map (fun a -> a.S.St.lit)

  (* (clause * proof * proof) -> 'a *)
  module ResTbl = CCHashtbl.Make(struct
      type t = sat_clause * ProofStep.of_ * ProofStep.of_
      let equal (c,a1,a2)(c',b1,b2) =
        CCList.equal Lit.equal c c' &&
        ProofStep.equal_proof a1 b1 && ProofStep.equal_proof a2 b2
      let hash (c,a,b) =
        Hashtbl.hash
          [List.length c; ProofStep.hash_proof a; ProofStep.hash_proof b]
    end)

  let tbl0 : (int,proof) Hashtbl.t = Hashtbl.create 16
  let tbl_res = ResTbl.create 16

  (* convert a SAT proof into a tree of ProofStep *)
  let conv_proof_ p : proof =
    let rec aux p =
      let open S.Proof in
      match S.Proof.expand p with
      | { step = S.Proof.Lemma _; _ } ->
        errorf "SAT proof involves a lemma"
      | { conclusion=c; step = S.Proof.Resolution (p1,p2,_) } ->
        let c = bool_clause_of_sat c in
        let q1 = aux p1 in
        let q2 = aux p2 in
        begin match ResTbl.get tbl_res (c,q1,q2) with
          | Some s -> s
          | None ->
            let parents = [q1; q2] in
            let step =
              ProofStep.mk_inference parents
                ~rule:(ProofStep.mk_rule "sat_resolution")  in
            let s = ProofStep.mk_bc step c in
            ResTbl.add tbl_res (c,q1,q2) s;
            ResTbl.add tbl_res (c,q2,q1) s;
            s
        end
      | { conclusion=c; step = _ } ->
        let tag = match S.get_tag c with
          | None ->
            errorf "no tag in leaf of SAT proof (clause %a)" S.St.pp_clause c
          | Some id -> id
        in
        begin match CCHashtbl.get tbl0 tag with
        | Some s -> s
        | None ->
          begin match CCHashtbl.get tag_to_proof_ tag with
            | Some step ->
              let c = bool_clause_of_sat c in
              let s = ProofStep.mk_bc step c in
              Hashtbl.add tbl0 tag s;
              s
            | None -> errorf "no proof for tag %d" tag
          end
        end
    in
    S.Proof.check p;
    aux p

  let get_proof_of_lit lit =
    let b, l = valuation_level lit in
    if not b || l <> 0 then invalid_arg "get_proof_of_lit";
    let a = S.St.add_atom lit in
    match S.Proof.prove_atom a with
      | Some p -> conv_proof_ p
      | None -> assert false

  let proved_at_0 lit =
    let b,l = valuation_level lit in
    if l=0 then Some b else None

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
      Util.debugf ~section 4 "@[<hv2>assume@ @[%a@]@]"
        (fun k->k pp_form (c,tag));
      (* remember tag->proof *)
      assert (not (Hashtbl.mem tag_to_proof_ tag));
      Hashtbl.replace tag_to_proof_ tag proof;
      S.assume ~tag c
    done;
    (* solve *)
    begin match S.solve () with
    | S.Sat s ->
      eval_ := s.SI.eval;
      eval_level_ := s.SI.eval_level;
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
    if !sat_dump_file_ <> ""
    then (
      Util.debugf ~section 1 "dump SAT clauses to `%s`" (fun k->k !sat_dump_file_);
      try
        let oc = open_out !sat_dump_file_ in
        dump_to := Some oc;
        at_exit (fun () -> close_out_noerr oc);
      with e ->
        Util.warnf "@[<2>could not open `%s`:@ %s@]"
          !sat_dump_file_ (Printexc.to_string e);
    );
    ()
end

let () =
  Params.add_opts
    [ "--sat-dump", Arg.Set_string sat_dump_file_, " output SAT problem(s) into <file>"
    ]
