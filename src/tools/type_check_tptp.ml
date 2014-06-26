
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

(** {1 TPTP Syntax and types checking} *)

open Logtk
open Logtk_parsers

module PT = PrologTerm
module Err = CCError

let print_line () =
  Printf.printf "%s\n" (Util.str_repeat "=" 60);
  ()

let cat_input = ref false  (* print input declarations? *)
let stats = ref false
let pp_base = ref false

let options =
  [ "-cat", Arg.Set cat_input, "print (annotated) declarations"
  ; "-base", Arg.Set pp_base, "print signature of base symbols"
  ] @ Options.global_opts

(* check the given file *)
let check file =
  Err.(
    print_line ();
    Printf.printf "checking file %s...\n" file;
    Util_tptp.parse_file ~recursive:true file
    >>= fun decls ->
    Util_tptp.infer_types (`sign Signature.empty) decls
    >>= fun (signature, decls') ->
    let decls' = Util_tptp.erase_types decls' in
    Printf.printf "signature:\n";
    Signature.iter signature
      (fun s ty ->
        Util.printf "  %a : %a\n" Symbol.pp s Type.pp ty);
    (* print formulas *)
    if !cat_input then begin
      Printf.printf "formulas:\n";
      Sequence.iter
        (fun decl -> Util.printf "  %a\n" Ast_tptp.Untyped.pp decl)
        decls';
      end;
    if !stats then begin
      Util.printf "number of symbols: %d\n" (Signature.cardinal signature);
      Util.printf "number of input declarations: %d\n" (Sequence.length decls);
      end;
    Err.return ()
  )

let main () =
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse options add_file "check_tptp [options] [file1|stdin] file2...";
  (if !files = [] then files := ["stdin"]);
  files := List.rev !files;
  let res = Err.fold_l
    (fun () file -> check file)
    () !files;
  in
  match res with
  | `Ok () ->
    print_line ();
    Printf.printf "success!\n"
  | `Error msg ->
    print_endline msg

let _ =
  main ()
