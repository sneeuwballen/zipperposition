
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Implementation of Phases}

    See {!Phases} for the list of steps to execute *)

open Libzipperposition

val parse_cli :
  (Phases.filename list * Params.t, [`Init], [`Parse_cli]) Phases.t
(** Parses the file list and parameters, also puts the parameters in
    the state *)

val load_extensions : (Extensions.t list, [`Parse_cli], [`LoadExtensions]) Phases.t

val setup_gc : (unit, [`Init], [`Init]) Phases.t

val setup_signal : (unit, [`Init], [`Init]) Phases.t

val process_file :
  ?prelude:Phases.prelude ->
  Phases.filename ->
  (Phases.env_with_result, [`Parse_prelude], [`Saturate]) Phases.t
(** [process_file f] parses [f], does the preprocessing phases, including
    type inference, choice of precedence, ordering, etc. , saturates
    the set of clauses, and return the result *)

val print :
  Phases.filename ->
  'c Env.packed ->
  Saturate.szs_status ->
  (unit, [`Saturate], [`Print_dot]) Phases.t
(** Printing of results *)

val check :
  Saturate.szs_status ->
  (Phases.errcode, [`Print_dot], [`Check_proof]) Phases.t

val process_files_and_print :
  ?params:Params.t ->
  Phases.filename list ->
  (Phases.errcode, [`LoadExtensions], [`Print_stats]) Phases.t
(** Process each file in the list successively, printing the results. *)

val print_stats : unit -> (unit, [`Check_proof], [`Print_stats]) Phases.t

val main_cli :
  ?setup_gc:bool ->
  unit ->
  (Phases.errcode, [`Init], [`Exit]) Phases.t
(** Main for the command-line prover *)

val main :
  ?setup_gc:bool ->
  ?params:Params.t ->
  string -> (** file *)
  (Phases.errcode, [`Init], [`Exit]) Phases.t
(** Main to use from a library *)

(* TODO: finer-grained APIs *)
