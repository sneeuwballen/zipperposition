
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
  print_format : string; (** Printing format ("tstp", "debugf"...) *)
} (** Options that can be set by the user *)

val default : t
  (** Default options *)

val make : t ref -> (string * Arg.spec * string) list
  (** Produce of list of options suitable for {!Arg.parse}, that may
      modify global parameters and the given option reference.
      After parsing, the reference content will reflect CLI options *)

val global : t ref
  (** Global parameters, can be used as a mutable default *)

val global_opts : (string * Arg.spec * string) list
  (** Options that modify {!global}.
      Caution, this might miss some options from modules that aren't registered yet.
      @deprecated since 0.6.1 , use {!mk_global_opts} instead*)

val mk_global_opts : unit -> (string * Arg.spec * string) list
