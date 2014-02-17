
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

val parse_file : recursive:bool -> string -> Ast_tptp.Untyped.t Sequence.t
  (** Parsing a TPTP file is here presented with a [recursive] option
      that, if true, will make "include" directives to be recursively
      parsed. It uses {!find_file} for included files.
      @raise Ast_tptp.ParseError in case a syntax error is met. *)

(* TODO: a function that takes a TPTP file, and returns the list of
        files that it depends on (recursive includes) *)

(** Printing is simpler, because it does not involve includes. *)

module type S = sig
  module A : Ast_tptp.S

  val print_into : out_channel -> A.t Sequence.t -> unit
  val print_into_file : string -> A.t Sequence.t -> unit
  val print_into_buf : Buffer.t -> A.t Sequence.t -> unit

  val has_includes : A.t Sequence.t -> bool
    (** Check whether some include declaration can be found in the sequence *)

  val type_declarations : A.t Sequence.t -> Signature.t
    (** Initial signature obtained by only considering the type declarations.
        In contrast to {!signature}, this doesn't perform type inference. *)

  val declare_symbols : ?name:(int -> string -> Ast_tptp.name) ->
                        Signature.t -> A.t Sequence.t
    (** Declare the symbols of the signature. A custom function
        to name the [i]-th symbol declaration can be provided. *)

  val formulas : ?negate:(Ast_tptp.role -> bool) ->
                 A.t Sequence.t -> A.form Sequence.t
    (** Extract only the formulas from some declarations. Formulas with
        a role that satisfies [negate] are negated.
        [negate] is true, by default, only for {!Ast_tptp.R_conjecture})*)

  val sourced_formulas : ?negate:(Ast_tptp.role -> bool) ->
                         ?file:string ->
                         A.t Sequence.t ->
                         (A.form * string * string) Sequence.t
    (** Same as {!formulas}, but keeps a source attached to formulas.
        A [file] name has to be provided for the source to be accurate,
        the default is "unknown_file". *)
end

module Untyped : S with module A = Ast_tptp.Untyped
module Typed : S with module A = Ast_tptp.Typed

(** {2 Type inference and erasure} *)

val infer_types : TypeInference.Ctx.t ->
                  Ast_tptp.Untyped.t Sequence.t ->
                  Signature.t * Ast_tptp.Typed.t Sequence.t
  (** Infer types from type declarations and formulas, returning a sequence
      of well-typed ASTs, and the inferred signature.
      @raise Type.Error if there is a type error. *)

val erase_types : Ast_tptp.Typed.t Sequence.t ->
                  Ast_tptp.Untyped.t Sequence.t
  (** Reverse operation of {!infer_types}, that erases types and converts
      formulas and terms back to {!PrologTerm.t}. *)
