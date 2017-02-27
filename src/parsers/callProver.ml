
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Call external provers with TSTP} *)

open Logtk

module A = Ast_tptp
module TT = Trace_tstp
module Err = CCResult
module ST = STerm

type 'a or_error = ('a, string) CCResult.t
type untyped = STerm.t

(** {2 Description of provers} *)

module Prover = struct
  type t = {
    name : string;                (** name of the prover *)
    command : string;             (** command to call prover*)
    unsat : string list;           (** prover returned unsat *)
    sat : string list;             (** prover returned sat *)
  } (** data useful to invoke a prover. The prover must read from
        stdin. The command is interpolated using {! Buffer.add_substitude}, with
        the given patterns:

        - "timeout" is the timeout in seconds *)

  let __table : (string,t) Hashtbl.t = Hashtbl.create 5

  let lookup name = Hashtbl.find __table name

  let list_provers () =
    Hashtbl.fold
      (fun n _ acc -> n :: acc)
      __table []

  let register name prover =
    if Hashtbl.mem __table name
    then invalid_arg ("prover already registered: "^ name)
    else Hashtbl.add __table name prover

  let p_E = {
    name = "E";
    command = "eprover --cpu-limit=${timeout} --auto -l0 --tstp-in --tstp-out";
    unsat = ["SZS status Unsat"; "SZS status Theorem"];
    sat = ["SZS status Satisfiable"; "SZS status CounterTheorem"];
  }

  let p_Eproof =
    { p_E with
        name = "Eproof";
        command = "eproof_ram --cpu-limit=${timeout} -tAuto -xAuto -l0 --tstp-in --tstp-out";
    }

  let p_SPASS = {
    name = "SPASS";
    command = "SPASS -TPTP -TimeLimit=${timeout} -Stdin";
    unsat = ["Proof found"];
    sat = ["Completion found"];
  }

  let p_Zenon = {
    name = "Zenon";
    command = "zenon -itptp -p0 -max-time ${timeout}s -";
    unsat = ["PROOF-FOUND"];
    sat = ["NO-PROOF"];
  }

  let default = [p_E; p_SPASS]
end

let name p = p.Prover.name

(** {2 Run provers} *)

type result =
  | Unsat
  | Sat
  | Unknown
  | Error of string

(* among the strings in [patterns], find if one is a substring of [s] *)
let _find_mem patterns s =
  List.exists
    (fun p -> CCString.find ~sub:p s >= 0)
    patterns

let call_with_out ?(timeout=30) ?(args=[]) ~prover decls =
  (* compute input to give to the prover *)
  let input =
    CCFormat.sprintf "@[<v>%a@]"
      (Util.pp_list ~sep:"" (A.pp ST.pp)) decls in
  (* build command (add arguments to the end) *)
  let buf = Buffer.create 15 in
  Buffer.add_substitute buf
    (function
      | "timeout" -> string_of_int timeout
      | s -> s)
    prover.Prover.command;
  List.iter (fun arg -> Buffer.add_char buf ' '; Buffer.add_string buf arg) args;
  let cmd = Buffer.contents buf in
  Util.debugf 2 "run prover %s" (fun k->k prover.Prover.name);
  Util.debugf 4 "command is: \"%s\"" (fun k->k cmd);
  Util.debugf 4 "obligation is: \"%s\"" (fun k->k input);
  Err.(
    (* run the prover *)
    Util.popen ~cmd ~input
    >>= fun output ->
    Util.debugf 2 "prover %s done" (fun k->k prover.Prover.name);
    Util.debugf 4 "output: \"%s\"" (fun k->k output);
    (* parse output *)
    let result =
      if _find_mem prover.Prover.unsat output
      then Unsat
      else if _find_mem prover.Prover.sat output
      then Sat
      else Unknown
    in
    Err.return (result, output)
  )

let call ?timeout ?args ~prover decls =
  Err.(
    call_with_out ?timeout ?args ~prover decls
    >>= fun (res, _) ->
    return res
  )

let decls_of_string ~source str =
  let lexbuf = Lexing.from_string str in
  ParseLocation.set_file lexbuf source;
  Util_tptp.parse_lexbuf lexbuf

(* try to parse a proof. Returns a proof option *)
let proof_of_decls decls =
  let res = Trace_tstp.of_decls decls in
  match res with
    | Err.Error _ -> None
    | Err.Ok proof -> Some proof

let call_proof ?timeout ?args ~prover decls =
  Err.(
    call_with_out ?timeout ?args ~prover decls
    >>= fun (res, output) ->
    decls_of_string ~source:("output of prover "^ prover.Prover.name) output
    >>= Trace_tstp.of_decls
    >>= fun proof ->
    return (res, proof)
  )

module Eprover = struct
  type result = {
    answer : szs_answer;
    output : string;
    decls : untyped Ast_tptp.t Sequence.t option;
    proof : Trace_tstp.t option;
  }
  and szs_answer =
    | Theorem
    | CounterSatisfiable
    | Unknown

  let string_of_answer = function
    | Theorem -> "Theorem"
    | CounterSatisfiable -> "CounterSatisfiable"
    | Unknown -> "Unknown"

  (* parse SZS answer *)
  let parse_answer output =
    if CCString.mem ~sub:"SZS status Theorem" output
    then Theorem
    else if CCString.mem ~sub:"SZS status CounterSatisfiable" output
    then CounterSatisfiable
    else Unknown

  (* run eproof_ram on the given input. returns a result *)
  let _run_either ?(opts=[]) ?(level=1) ~prover ~steps ~input () =
    let level' = Printf.sprintf "-l%d" level in
    let command =
      [ prover; "--tstp-in"; "--tstp-out"; level'; "-C"
      ; string_of_int steps; "-xAuto"; "-tAuto" ] @ opts
    in
    let cmd = String.concat " " command in
    Err.(
      Util.popen ~cmd ~input
      >>= fun output ->
      (* parse answer *)
      let answer = parse_answer output in
      (* read its output *)
      let decls, proof =
        match decls_of_string ~source:"E" output with
          | Err.Error _ -> None, None
          | Err.Ok s ->
            (* try to parse proof, if it's a theorem *)
            let proof =
              if answer = Theorem
              then proof_of_decls s
              else None
            in
            Some s, proof
      in
      Err.return { answer; output; decls; proof }
    )

  (* run eproof_ram on the given input. returns a result *)
  let run_eproof ~steps ~input =
    _run_either ~prover:"eproof_ram" ~steps ~input ()

  (* run eprover on the given input. returns a result Lwt *)
  let run_eprover ?opts ?level ~steps ~input () =
    _run_either ~prover:"eprover" ?opts ?level ~steps ~input ()

  (* explore the surrounding of this list of formulas, returning a
     list of terms (clausal form) *)
  let discover ?(opts=[]) ~steps decls =
    let command = [ "eprover"; "--tstp-in"; "--tstp-out";
                    "-S"; "--restrict-literal-comparisons";
                    "-C"; string_of_int steps ] @ opts in
    let cmd = String.concat " " command in
    (* build stdin *)
    let input =
      CCFormat.sprintf "@[%a@]"
        (Util.pp_seq ~sep:"" (A.pp ST.pp)) decls
    in
    Err.(
      (* call E *)
      Util.popen ~cmd ~input
      >>= fun output ->
      (* read its output *)
      decls_of_string ~source:"E" output
    )

  let cnf ?(opts=[]) decls =
    let command = [ "eprover"; "--tstp-in"; "--tstp-out"; "--cnf" ] @ opts in
    let cmd = String.concat " " command in
    (* build stdin *)
    let input =
      CCFormat.sprintf "@[%a@]"
        (Util.pp_seq ~sep:"" (A.pp ST.pp)) decls
    in
    Err.(
      (* call E *)
      Util.popen ~cmd ~input
      >>= fun output ->
      (* read its output *)
      decls_of_string ~source:"E" output
    )
end
