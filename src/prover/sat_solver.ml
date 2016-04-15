
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Bridge to [MSat] prover} *)

open Libzipperposition

let section = Util.Section.make ~parent:Const.section "msat"
let prof_call_msat = Util.mk_profiler "msat.call"

type result = Sat_solver_intf.result =
  | Sat
  | Unsat

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
  type proof_step = ProofStep.t
  type proof = ProofStep.of_

  (* queue of clauses waiting for being pushed into the solver *)
  let queue_ = Queue.create()

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

  let add_clause ~proof (c:clause) =
    dump_l [c];
    Queue.push ([c], proof, fresh_tag_ ()) queue_

  let add_clauses ~proof l =
    dump_l l;
    Queue.push (l, proof, fresh_tag_ ()) queue_

  let add_clause_seq ~proof (seq:clause Sequence.t) =
    add_clauses ~proof (Sequence.to_rev_list seq)

  let result_ = ref Sat

  (* invariant:
    when result_ = Sat, only eval/eval_level are defined
    when result_ = Unsat, only unsat_core_ is defined
  *)

  let eval_fail_ _ = assert (!result_ = Unsat); wrong_state_ "eval"

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
  let tag_to_proof_ = Hashtbl.create 32

  module SatForm = struct
    include Lit
    type proof = ProofStep.t
    let fresh () = Lit.make (Lit.payload Lit.dummy)
    let label _ = assert false
    let add_label _ _ = assert false
    let print = Lit.pp
  end

  (* Instantiate solver *)
  module S =
    Msat.Solver.Make
      (SatForm)
      (Msat.Solver.DummyTheory(SatForm))
      (struct end)

  exception UndecidedLit = S.UndecidedLit

  let bool_clause_of_sat c =
    S.Proof.to_list c |> List.map (fun a -> a.S.St.lit)

  (* convert a SAT proof into a tree of ProofStep *)
  let conv_proof_ p : proof =
    let rec aux p =
      let open S.Proof in
      match S.Proof.expand p with
      | { conclusion=c; step = S.Proof.Hypothesis } ->
        let tag = match S.get_tag c with
          | None ->
            errorf "no tag in leaf of SAT proof (clause %a)" S.St.pp_clause c
          | Some id -> id
        in
        begin
          try
            let step = Hashtbl.find tag_to_proof_ tag in
            let c = bool_clause_of_sat c in
            ProofStep.mk_bc step c
          with Not_found -> errorf "no proof for tag %d" tag
        end
      | { step = S.Proof.Lemma _; _ } ->
        errorf "SAT proof involves a lemma"
      | { conclusion=c; step = S.Proof.Resolution (p1,p2,_) } ->
        let c = bool_clause_of_sat c in
        let parents = [aux p1; aux p2] in
        let step =
          ProofStep.mk_inference parents
            ~rule:(ProofStep.mk_rule "sat_resolution")  in
        ProofStep.mk_bc step c
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

  (* call [S.solve()] in any case, and enforce invariant about eval/unsat_core *)
  let check_unconditional_ () =
    (* reset functions, so they will fail if called in the wrong state *)
    proof_ := None;
    eval_ := eval_fail_;
    eval_level_ := eval_fail_;
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
    | S.Sat ->
      eval_ := S.eval;
      eval_level_ := S.eval_level;
      result_ := Sat;
    | S.Unsat ->
      result_ := Unsat;
      let p = S.get_proof () in
      proof_ := Some (conv_proof_ p);
    end;
    !result_

  let check_ () =
    if Queue.is_empty queue_ then !result_
    else check_unconditional_ ()

  (* initialize eval/eval_level to enforce invariant *)
  let () =
    let res = check_unconditional_ () in
    assert (res = Sat)

  let check () = Util.with_prof prof_call_msat check_ ()

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

  type save_level = int

  let root_save_level = 0

  let save () = assert false

  let restore _ = assert false
end

let () =
  Params.add_opts
    [ "--sat-dump", Arg.Set_string sat_dump_file_, " output SAT problem(s) into <file>"
    ]
