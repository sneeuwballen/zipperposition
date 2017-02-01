
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parse Rewrite Rules} *)

open Libzipperposition

type 'a or_error = ('a, string) CCResult.t

type statement =
  | Rule of STerm.t * STerm.t
  | Type of string * STerm.t

type rule = TypedSTerm.t * TypedSTerm.t
(** Rewrite rules on terms *)

val parse_file : string -> in_channel -> statement list or_error
(** Parse the given file, containing rules, into statements *)

val rules_of_pairs :
  statement list ->
  rule list or_error
(** Infer types and signature from a list of statements.
   @return typed rules and the new signature *)

val signature : rule Sequence.t -> TypedSTerm.t ID.Map.t
(** Compute signature *)

val print_rules : rule list CCFormat.printer
