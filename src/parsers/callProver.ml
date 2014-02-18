
(*
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

(** {6 Call external provers with TSTP} *)

module A = Ast_tptp
module TT = Trace_tstp

(** {2 Description of provers} *)

module Prover = struct
  type t = {
    name : string;                (** name of the prover *)
    command : string;             (** command to call prover*)
    unsat : Str.regexp;           (** prover returned unsat *)
    sat : Str.regexp;             (** prover returned sat *)
  } (** data useful to invoke a prover. The prover must read from
        stdin. The command is interpolated using {! Buffer.add_substitude}, with
        the given patterns:

        - "timeout" is the timeout in seconds *)

  let __table = Hashtbl.create 5

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
    command = "eprover --cpu-limit=${timeout} -tAuto -xAuto -l0 --tstp-in";
    unsat = Str.regexp_case_fold "SZS status \\(Theorem\\|Unsat\\)";
    sat = Str.regexp_case_fold "SZS status \\(Satisfiable\\|CounterTheorem\\)";
  }

  let p_Eproof =
    { p_E with
      name = "Eproof";
      command = "eproof_ram --cpu-limit=${timeout} -tAuto -xAuto -l0 --tstp-in";
    }

  let p_SPASS = {
    name = "SPASS";
    command = "SPASS -TPTP -TimeLimit=${timeout} -Stdin";
    unsat = Str.regexp_case_fold "Proof found";
    sat = Str.regexp_case_fold "Completion found";
  }

  let p_Zenon = {
    name = "Zenon";
    command = "zenon -itptp -p0 -max-time ${timeout}s -";
    unsat = Str.regexp_case_fold "PROOF-FOUND";
    sat = Str.regexp_case_fold "NO-PROOF";
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

let call_with_out ?(timeout=30) ~prover decls =
  try
    (* compute input to give to the prover *)
    let input = Util.on_buffer (Util.pp_list ~sep:"\n" A.pp_declaration) decls in
    (* build command *)
    let buf = Buffer.create 15 in
    Buffer.add_substitute buf
      (function
        | "timeout" -> string_of_int timeout
        | s -> s)
      prover.Prover.command;
    let cmd = Buffer.contents buf in
    Util.debug 2 "run prover %s" prover.Prover.name;
    Util.debug 4 "command is: \"%s\"" cmd;
    Util.debug 4 "obligation is: \"%s\"" input;
    (* run the prover *)
    let output = Util.popen ~cmd ~input in
    Util.debug 2 "prover %s done" prover.Prover.name;
    Util.debug 4 "output: \"%s\"" output;
    (* parse output *)
    let result =
      try
        ignore (Str.search_forward prover.Prover.unsat output 0);
        Unsat
      with Not_found ->
        try
          ignore (Str.search_forward prover.Prover.sat output 0);
          Sat
        with Not_found ->
          Unknown
    in
    result, output
  with e ->
    let str = Printexc.to_string e in
    Error str, ""

let call ?timeout ~prover decls =
  let res, _ = call_with_out ?timeout ~prover decls in
  res

let call_proof ?timeout ~prover decls =
  let res, output = call_with_out ?timeout ~prover decls in
  let proof =
    try
      let lexbuf = Lexing.from_string output in
      let decls = Parse_tptp.parse_declarations Lex_tptp.token lexbuf in
      let proof = Trace_tstp.of_decls (Sequence.of_list decls) in
      proof
    with _ -> None
  in
  res, proof
