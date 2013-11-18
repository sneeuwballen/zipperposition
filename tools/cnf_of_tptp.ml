
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

module BF = Basic.Form
module F = FOFormula
module A = Ast_tptp

let declare_types = ref false
let print_sig = ref false

let options =
  [ "-declare", Arg.Set declare_types, "declare types of symbols"
  ; "-signature", Arg.Set print_sig, "print signature"
  ] @ Options.global_opts

(* conversion to CNF of declarations *)
let to_cnf decls =
  let signature = Util_tptp.type_declarations decls in
  let tyctx = TypeInference.Ctx.of_signature signature in
  let ctx = Skolem.create ~base:signature () in
  let seq = Sequence.flatMap
    (function
      | A.FOF(n,role,f,info)
      | A.TFF(n,role,f,info) ->
        begin match role with
        | A.R_conjecture ->
          (* type conjecture *)
          let f = TypeInference.FO.convert_form ~ctx:tyctx f in
          (* negate conjecture *)
          let clauses = Cnf.cnf_of ~ctx (F.mk_not f) in
          Sequence.map
            (fun c ->
              let c = TypeErasure.Form.erase (F.close_forall (F.mk_or c)) in
              A.TFF(n,A.R_negated_conjecture,c,info))
            (Sequence.of_list clauses)
        | _ ->
          (* type conjecture *)
          let f = TypeInference.FO.convert_form ~ctx:tyctx f in
          (* translate, keeping the same role *)
          let clauses = Cnf.cnf_of ~ctx f in
          Sequence.map
            (fun c ->
              let c = TypeErasure.Form.erase (F.close_forall (F.mk_or c)) in
              A.TFF(n,role,c,info))
            (Sequence.of_list clauses)
        end
      | A.CNF _ as d -> Sequence.singleton d
      | _ -> Sequence.empty)
    decls
  in
  (* iterating again would change skolems, etc, which is bad *)
  Sequence.persistent seq, Skolem.to_signature ctx

(* process the given file, converting it to CNF *)
let process file =
  Util.debug 1 "process file %s" file;
  try
    (* parse *)
    let decls = Util_tptp.parse_file ~recursive:true file in
    (* to CNF *)
    let decls, signature = to_cnf decls in
    let decls = if !declare_types
      then Sequence.append (Util_tptp.declare_symbols signature) decls
      else decls
    in
    if !print_sig
      then Util.printf "signature: %a\n" Signature.pp signature;
    (* print *)
    Sequence.iter
      (fun d -> Util.printf "%a\n" A.pp_declaration d)
      decls
  with
  | Ast_tptp.ParseError loc ->
    (* syntax error *)
    Util.eprintf "parse error at %a\n" Location.pp loc;
    exit 1
  | TypeInference.Error msg
  | Type.Error msg ->
    Util.eprintf "type error: %s\n" msg;
    Printexc.print_backtrace stderr;
    exit 1
  | TypeUnif.Error e ->
    Util.eprintf "%a\n" TypeUnif.pp_error e;
    Printexc.print_backtrace stderr;
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
