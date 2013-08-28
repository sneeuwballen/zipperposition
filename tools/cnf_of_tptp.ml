
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

(** {1 Reduction to CNF of TPTP file} *)

open Logtk

module T = Term
module A = Ast_tptp

let declare_types = ref false

let options =
  [ "-debug", Arg.Int Util.set_debug, "debug level"
  ; "-declare", Arg.Set declare_types, "declare types of symbols"
  ]

(* conversion to CNF of declarations *)
let to_cnf decls =
  let ctx = Skolem.create () in
  let seq = Sequence.flatMap
    (function
      | A.FOF(n,role,f,info)
      | A.TFF(n,role,f,info) ->
        let clauses = Cnf.cnf_of ~ctx f in
        Sequence.map
          (fun c -> A.CNF(n,role,c,info))
          (Sequence.of_list clauses)
      | A.CNF _ as d -> Sequence.singleton d
      | _ -> Sequence.empty)
    decls
  in
  (* iterating again would change skolems, etc, which is bad *)
  Sequence.persistent seq

(* process the given file, converting it to CNF *)
let process file =
  Util.debug 1 "process file %s" file;
  try
    (* parse *)
    let decls = Util_tptp.parse_file ~recursive:true file in
    (* to CNF *)
    let decls = to_cnf decls in
    let decls =
      if !declare_types
        then
          let signature = Util_tptp.signature decls in
          Sequence.append (Util_tptp.declare_symbols signature) decls
        else decls
    in
    (* print *)
    Sequence.iter
      (fun d -> Util.printf "%a\n" A.pp_declaration d)
      decls
  with
  | Util_tptp.ParseError _ as e ->
    (* syntax error *)
    Printf.printf "%s\n" (Util_tptp.string_of_error e);
    exit 1

let main () =
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse options add_file "cnf_of_tptp [options] [file1|stdin] file2...";
  (if !files = [] then files := ["stdin"]);
  files := List.rev !files;
  List.iter process !files;
  Util.debug 1 "success!";
  ()

let _ =
  main ()
