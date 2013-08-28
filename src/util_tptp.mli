
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

(** {1 Utils related to TPTP} *)

(** {2 Printing/Parsing} *)

val find_file : string -> string -> string
  (** [find_file name dir] looks for a file with the given [name],
      recursively, in [dir], or in its parent dir recursively.
      It also looks in the "TPTP" environment variable.
      @raise Failure if no such file can be found. *)

exception ParseError of string * int * int * int * int
  (** Error at the given file, start(line, column) stop(line,column) *)

val string_of_error : exn -> string
  (** message from ParseError *)

val parse_file : recursive:bool -> string -> Ast_tptp.declaration Sequence.t
  (** Parsing a TPTP file is here presented with a [recursive] option
      that, if true, will make "include" directives to be recursively
      parsed. It uses {!find_file} for included files.
      @raise ParseError in case a syntax error is met. *)

(** Printing is simpler, because it does not involve includes. *)

val print_into : out_channel -> Ast_tptp.declaration Sequence.t -> unit
val print_into_file : string -> Ast_tptp.declaration Sequence.t -> unit
val print_into_buf : Buffer.t -> Ast_tptp.declaration Sequence.t -> unit

val has_includes : Ast_tptp.declaration Sequence.t -> bool
  (** Check whether some include declaration can be found in the sequence *)

(** {2 Type inference} *)

val infer_type : TypeInference.Ctx.t -> Ast_tptp.declaration Sequence.t -> unit

val signature : ?signature:Signature.t ->
                Ast_tptp.declaration Sequence.t ->
                Signature.t

val formulas : ?negate:(Ast_tptp.role -> bool) ->
               Ast_tptp.declaration Sequence.t ->
               Term.t Sequence.t
  (** Extract only the formulas from some declarations. Formulas with
      a role that satisfies [negate] are negated.
      [negate] is true, by default, only for {!Ast_tptp.R_conjecture})*)

