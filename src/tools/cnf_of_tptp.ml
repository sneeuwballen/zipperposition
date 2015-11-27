
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
open Logtk_parsers

module F = Formula.FO
module A = Ast_tptp
module AU = Ast_tptp.Untyped
module AT = Ast_tptp.Typed
module Err = CCError

let declare_types = ref false
let print_sig = ref false
let flag_distribute_exists = ref false
let flag_disable_renaming = ref false

let options = Arg.align (
  [ "-declare", Arg.Set declare_types, " declare types of symbols"
  ; "-signature", Arg.Set print_sig, " print signature"
  ; "-distribute-exist", Arg.Set flag_distribute_exists,
    " distribute existential quantifiers during miniscoping"
  ; "-disable-def", Arg.Set flag_disable_renaming, " disable definitional CNF"
  ; "-time-limit", Arg.Int Util.set_time_limit, " hard time limit (in s)"
  ] @ Options.mk_global_opts ()
  )

(* process the given file, converting it to CNF *)
let process file =
  Util.debugf 1 "process file %s" (fun k->k file);
  let res = Err.(
    (* parse *)
    Util_tptp.parse_file ~recursive:true file
    >>= fun decls ->
    (* to CNF *)
    Util_tptp.infer_types (`sign Signature.TPTP.base) decls
    >>= fun (signature, decls) ->
    let opts =
      (if !flag_distribute_exists then [Cnf.DistributeExists] else []) @
      (if !flag_disable_renaming then [Cnf.DisableRenaming] else []) @
      []
    in
    let signature, decls = Util_tptp.to_cnf ~opts signature decls in
    let decls = if !declare_types
      then Sequence.append (Util_tptp.Typed.declare_symbols signature) decls
      else decls
    in
    if !print_sig
      then Format.printf "@[<2>signature: @[%a@]@]@." Signature.pp signature;
    (* print *)
    Sequence.iter
      (fun d -> Format.printf "%a@." AT.pp d)
      decls;
    Err.return ()
  )
  in match res with
    | `Ok () -> ()
    | `Error msg ->
        print_endline msg;
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
