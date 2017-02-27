
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {6 Simple Proof checker for TSTP} *)

open Logtk
open Logtk_parsers

module A = Ast_tptp
module T = STerm
module TT = Trace_tstp
module StepTbl = TT.StepTbl
module E = CCResult

(* result of checking the step: success or failure, with name of prover.
   The step may be unchecked. *)
type check_result =
  | Unchecked
  | Succeeded of string
  | Failed of string

let is_success = function
  | Unchecked
  | Succeeded _ -> true
  | Failed _ -> false

let is_failure res = not (is_success res)

let pp_res out res = match res with
  | Unchecked -> CCFormat.string out "unchecked"
  | Succeeded p -> Format.fprintf out "prover %s succeeded" p
  | Failed p -> Format.fprintf out "prover %s failed" p

module CheckedTrace = struct
  type t = {
    trace : Trace_tstp.t; (* trace to check *)
    steps : check_result list StepTbl.t;  (* result of checks for every step *)
    mutable failures : (Trace_tstp.t * check_result) list;
  } (** Maps every step of the given trace to a list of results
        obtained by trying to verify it using provers *)

  let create trace = {
    trace;
    steps = StepTbl.create 15;
    failures = [];
  }

  let trace ~checked = checked.trace
  let failures ~checked = checked.failures

  (* result for this step *)
  let get ~checked step =
    try
      StepTbl.find checked.steps step
    with Not_found -> []

  (* add a result to the checked_trace *)
  let add ~checked step res =
    let l = get ~checked step in
    StepTbl.replace checked.steps step (res :: l);
    match res with
      | Failed _ -> checked.failures <- (step, res) :: checked.failures
      | _ -> ()
end

(* can we avoid checking this proof step? *)
let _do_not_check proof = match proof with
  | TT.Axiom _
  | TT.Theory _ -> true
  | TT.InferClause (_, lazy step)
  | TT.InferForm (_, lazy step) ->
    step.TT.esa ||
    match step.TT.parents with
      | [| a |] when TT.is_axiom a -> true  (* axiom *)
      | _ -> false

let slit_to_form = function
  | SLiteral.Atom (t, true) -> t
  | SLiteral.Atom (t, false) -> T.not_ t
  | SLiteral.Eq (a,b) -> T.eq a b
  | SLiteral.Neq (a,b) -> T.neq a b
  | SLiteral.True -> T.true_
  | SLiteral.False -> T.false_

(* make a proof obligation (TSTP declarations list) out of a proof step.
   May return none in case the proof obligation is trivial *)
let mk_proof_obligation proof =
  if _do_not_check proof
  then None  (* no need to check *)
  else
    try
      let goal, step = match proof with
        | TT.InferForm (f, lazy step) ->
          let f = T.close_all Binder.Forall f in
          A.FOF(step.TT.id, A.R_conjecture, f, []), step
        | TT.InferClause (c, lazy step) ->
          let c = List.map slit_to_form c in
          let c = T.close_all Binder.Forall (T.or_ c) in
          A.FOF(step.TT.id, A.R_conjecture, c, []), step
        | _ -> assert false
      in
      let premises = CCList.filter_map
          (fun parent -> match parent with
             | TT.InferClause (c, lazy step') ->
               let c = List.map slit_to_form c in
               let c = T.close_all Binder.Forall (T.or_ c) in
               Some (A.FOF(step'.TT.id, A.R_axiom, c, []))
             | TT.InferForm(f, lazy step') ->
               let f = T.close_all Binder.Forall f in
               Some (A.FOF(step'.TT.id, A.R_axiom, f, []))
             | TT.Axiom _
             | TT.Theory _ -> None)
          (Array.to_list step.TT.parents)
      in
      Some (goal :: premises)
    with Exit ->
      None

(* check a proof step using a prover *)
let check_step ~timeout ~prover step =
  (* input to feed to the prover *)
  let obligation = mk_proof_obligation step in
  match obligation with
    | None -> E.return Unchecked  (* nothing to check, no obligation! *)
    | Some decls ->
      E.(
        CallProver.call ~timeout ~prover decls
        >>= fun res ->
        let p_name = CallProver.name prover in
        (* interpret result of the subprover *)
        let res = match res with
          | CallProver.Unsat -> Succeeded p_name
          | CallProver.Error e ->
            Util.debugf 1 "error trying to check %a: %s" (fun k->k TT.pp1 step e);
            Failed p_name
          | CallProver.Unknown
          | CallProver.Sat -> Failed p_name
        in
        Util.debugf 1 "step %a: %a" (fun k->k A.pp_name (TT.get_id step) pp_res res);
        E.return res
      )

(* print progress in proof checking *)
let pp_progress num total =
  Printf.printf "\r checking step %-5d on %-5d" num total;
  flush stdout;
  ()

(* check every step of [trace] using [provers], and put the result
    in [checked] *)
let check_all ~progress ~provers ~timeout ~checked =
  let n = ref 0 in
  let trace = CheckedTrace.trace ~checked in
  let len = TT.size trace in
  try
    TT.traverse trace
      (fun step ->
         (* print progress *)
         incr n;
         if progress then pp_progress !n len;
         List.iter
           (fun prover ->
              (* check step with prover *)
              match  check_step ~timeout ~prover step with
                | E.Ok res ->
                  CheckedTrace.add ~checked step res
                | E.Error msg ->
                  failwith msg)
           provers);
    (* clean line of progress *)
    if progress then Printf.printf "                                      \n";
    E.return ()
  with Failure msg ->
    E.fail msg

(* check that all paths from root, through proof nodes that satisfy [valid],
    reach axioms *)
let all_paths_correct ~valid ~checked =
  (* steps currently being explored *)
  let current = StepTbl.create 10 in
  (* steps totally explored *)
  let closed = StepTbl.create 10 in
  (* recursive DFS traversal *)
  let rec check_proof proof =
    if StepTbl.mem closed proof
    then ()  (* ok *)
    else if StepTbl.mem current proof
    then
      let _ = Util.debugf 1 "step %a not valid, in a cycle" (fun k->k TT.pp proof) in
      raise Exit  (* we followed a back link! *)
    else if not (valid proof)
    then
      let _ = Util.debugf 1 "step %a not validated" (fun k->k TT.pp proof) in
      raise Exit  (* path leads to invalid step *)
    else begin
      StepTbl.add current proof ();
      begin match proof with
        | TT.InferClause (_, lazy step)
        | TT.InferForm (_, lazy step) ->
          Array.iter check_proof step.TT.parents
        | TT.Axiom _
        | TT.Theory _ -> ()
      end;
      (* proof is now totally explored *)
      StepTbl.remove current proof;
      StepTbl.replace closed proof ();
    end
  in
  try
    let trace = CheckedTrace.trace ~checked in
    check_proof trace;  (* check from root *)
    true
  with Exit ->
    false  (* loop detected *)

(* check the structure. [minimum] is the number of check success that is
    required for a proof step to be valid. *)
let check_structure ?(minimum=1) ~checked =
  let valid step =
    let l = CheckedTrace.get ~checked step in
    List.length (List.filter is_success l) >= minimum
  in
  all_paths_correct ~valid ~checked

(** {2 Main} *)

let file = ref "stdin"
let print_problem = ref false
let timeout = ref 10
let minimum = ref 1
let provers = CallProver.Prover.default
let progress = ref false

(* TODO option to choose provers *)

let options = Arg.align (
    [ "--pp", Arg.Set print_problem, " print problem after parsing"
    ; "--timeout", Arg.Set_int timeout, " timeout for subprovers (in seconds)"
    ; "--minimum", Arg.Set_int minimum, " minimum number of checks to validate a step (1)"
    ; "--progress", Arg.Set progress, " print progress"
    ] @ Options.make ()
  )

(** parse_args returns parameters *)
let parse_args () =
  Arg.parse options (fun f -> file := f) "check the given TSTP derivation"

let main file =
  E.(
    TT.parse ~recursive:true file
    >>= fun trace ->
    if !print_problem
    then Format.printf "@[<2>derivation:@,@[%a@]@]@." TT.pp_tstp trace;
    Util.debugf 1 "trace of %d steps" (fun k->k (TT.size trace));
    (* check that the steps form a DAG *)
    if not (TT.is_dag trace) then (
      Util.debug 0 "derivation is not a DAG, failure.";
      exit 1
    ) else Util.debug 0 "derivation is a DAG";
    (* validate steps one by one *)
    let checked = CheckedTrace.create trace in
    check_all ~progress:!progress ~provers ~checked ~timeout:!timeout
    >>= fun () ->
    (* print failures *)
    List.iter
      (fun (proof, res) -> match res with
         | Failed prover ->
           Util.debugf 1 "trying to prove %a with %s failed" (fun k->k TT.pp1 proof prover)
         | _ -> assert false)
      (CheckedTrace.failures ~checked);
    (* check the global structure of the validated steps *)
    if check_structure ~minimum:!minimum ~checked
    then Util.debug 0 "validated steps form a correct proof. success."
    else Util.debug 0 "proof structure incorrect. failure.";
    E.return ()
  )

(** main entry point *)
let () =
  CCFormat.set_color_default true;
  parse_args ();
  match main !file with
    | E.Ok () -> ()
    | E.Error msg ->
      print_endline msg;
      exit 1
