
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Implementation of Phases}

    See {!Phases} for the list of steps to execute *)

val parse_cli :
  (Phases.filename list * Params.t, [`Init], [`Parse_cli]) Phases.t
(** Parses the file list and parameters, also puts the parameters in
  the state *)

val load_extensions : (Extensions.t list, [`Parse_cli], [`LoadExtensions]) Phases.t

val setup_gc : (unit, [`Init], [`Init]) Phases.t

val setup_signal : (unit, [`Init], [`Init]) Phases.t

val process_file :
  Phases.filename ->
  (Phases.env_with_result, [`LoadExtensions], [`Saturate]) Phases.t
(** [process_file f] parses [f], does the preprocessing phases, including
    type inference, choice of precedence, ordering, etc. , saturates
    the set of clauses, and return the result *)

val print :
  Phases.filename ->
  'c Env.packed ->
  'c Saturate.szs_status ->
  (unit, [`Saturate], [`Print_dot]) Phases.t
(** Printing of results *)

val process_files_and_print :
  Phases.filename list ->
  (unit, [`LoadExtensions], [`Print_dot]) Phases.t
(** Process each file in the list successively, printing the results. *)

