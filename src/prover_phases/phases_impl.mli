(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Implementation of Phases}

    See {!Phases} for the list of steps to execute *)

open Libzipperposition

val parse_cli : (Phases.filename list * Params.t) Phases.t
(** Parses the file list and parameters, also puts the parameters in the state
*)

val load_extensions : Extensions.t list Phases.t
val setup_gc : unit Phases.t

val process_file :
  ?prelude:Phases.prelude -> Phases.filename -> Phases.env_with_result Phases.t

val print : Phases.filename -> Env.t -> Saturate.szs_status -> unit Phases.t
val check : Saturate.szs_status -> Phases.errcode Phases.t

val process_files_and_print :
  ?params:Params.t -> Phases.filename list -> Phases.errcode Phases.t
(** Process each file in the list successively, printing the results. *)

val print_stats : unit Phases.t

val main_cli : ?setup_gc:bool -> unit -> Phases.errcode Phases.t
(** Main for the command-line prover *)

val main :
  ?setup_gc:bool ->
  ?params:Params.t ->
  string ->
  (* file *)
  Phases.errcode Phases.t
(** Main to use from a library *)

(* TODO: finer-grained APIs *)
