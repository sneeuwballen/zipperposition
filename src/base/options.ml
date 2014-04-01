
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

(** {1 Global CLI options}

Those options can be used by any program that parses command
line arguments using the standard module {!Arg}. It may modify some
global parameters, and return a parameter type for other options.
*)


type t = {
  stats : bool;  (** statistics *) 
  print_format : string; (** Printing format ("tstp", "debug"...) *)
}

let default = {
  stats = false;
  print_format = "debug";
}

let _print_types () =
  HOTerm.print_all_types := true;
  FOTerm.print_all_types := true;
  ()

let make opts =
  let mod_opt f = opts := f !opts in
  [ "-debug", Arg.Int Util.set_debug, "logtk: debug level"
  ; "-profile", Arg.Set Util.enable_profiling, "logtk: enable profiling"
  ; "-stats", Arg.Unit (fun () -> mod_opt (fun o -> {o with stats=true;})),
      "enable statistics"
  ; "-print", Arg.String (fun s -> mod_opt (fun o -> {o with print_format=s;} )),
      "choose printing format for terms and formulas (default \"debug\""
  ; "-print-types", Arg.Unit _print_types , "print type annotations everywhere"
  ; "-bt", Arg.Bool Printexc.record_backtrace, "enable backtraces"
  ; "-print-types", Arg.Set FOTerm.print_all_types, "print all types"
  ]

let global = ref default

let global_opts = make global
