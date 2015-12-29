
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Utils related to TPTP} *)

open Libzipperposition

module A = Ast_tptp

type 'a or_error = [`Error of string | `Ok of 'a]
type untyped = STerm.t
type typed = TypedSTerm.t

(** {2 Printing/Parsing} *)

val find_file : string -> string -> string option
(** [find_file name dir] looks for a file with the given [name],
    recursively, in [dir], or in its parent dir recursively.
    It also looks in the "TPTP" environment variable. *)

val parse_lexbuf :
  ?names:A.name list ->
  Lexing.lexbuf ->
  untyped A.t Sequence.t or_error
(** Given a lexbuf, try to parse its content into a sequence of untyped
    declarations *)

val parse_file :
  recursive:bool -> string ->
  untyped A.t Sequence.t or_error
(** Parsing a TPTP file is here presented with a [recursive] option
    that, if true, will make "include" directives to be recursively
    parsed. It uses {!find_file} for included files.
    @return an error-wrapped sequence of declarations. *)

(* TODO: a function that takes a TPTP file, and returns the list of
        files that it depends on (recursive includes) *)

(** Printing is simpler, because it does not involve includes. *)

val print_into : 't CCFormat.printer -> 't A.t Sequence.t CCFormat.printer
val print_into_file : 't CCFormat.printer -> string -> 't A.t Sequence.t -> unit

val has_includes : _ A.t Sequence.t -> bool
(** Check whether some include declaration can be found in the sequence *)

val type_declarations : typed A.t Sequence.t -> typed ID.Map.t
(** Initial signature obtained by only considering the type declarations. *)

val declare_symbols :
  ?name:(ID.t -> A.name) ->
  typed ID.Map.t ->
  typed A.t Sequence.t
(** Declare the symbols of the signature. A custom function
    to name the [i]-th symbol declaration can be provided. *)

val formulas :
  ?negate:(A.role -> bool) ->
  typed A.t Sequence.t -> typed Sequence.t
(** Extract only the formulas from some declarations. Formulas with
    a role that satisfies [negate] are negated.
    [negate] is true, by default, only for {!A.R_conjecture})*)

(** {2 Type inference and erasure}
    The following functions can raise Type.Error if the declarations
    are not consistent. *)

val infer_types :
  ?ctx:TypeInference.Ctx.t ->
  untyped A.t Sequence.t ->
  typed A.t Sequence.t or_error
(** Infer types from type declarations and formulas, returning a sequence
    of well-typed ASTs, and the inferred signature.
    @return `Error if some formula is ill-typed, `Ok otherwise *)

val infer_types_exn :
  ?ctx:TypeInference.Ctx.t ->
  untyped A.t Sequence.t ->
  typed A.t Sequence.t
(** Unsafe version of {!infer_types} *)

val erase_types :
  typed A.t Sequence.t ->
  untyped A.t Sequence.t
(** Reverse operation of {!infer_types}, that erases types and converts
    formulas and terms back to {!STerm.t}. *)

val to_cnf :
  ?opts:Cnf.options list ->
  typed A.t Sequence.t ->
  ((A.role * string) Cnf.statement, CCVector.ro) CCVector.t
(** [to_cnf decls] reduces declarations to CNF, and returns the new
    declarations (including type declarations for Skolem symbols)
    in the form of {!Cnf.statement}.

    Each statement is labelled with the role and name of the declaration
    is comes from.
    @param opts options for CNF *)

