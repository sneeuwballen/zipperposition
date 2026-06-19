(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition
(** {1 Rewriting}

    Deal with definitions as rewrite rules *)

val setup :
  Env.t -> ?ctx_narrow:bool -> narrowing:bool -> has_rw:bool -> unit -> unit

val unfold_def_before_cnf :
  ( ( Logtk.TypedSTerm.t,
      Logtk.TypedSTerm.t,
      Logtk.TypedSTerm.t )
    Logtk.Statement.t,
    'c )
  CCVector.t ->
  ( ( Logtk.TypedSTerm.t,
      Logtk.TypedSTerm.t,
      Logtk.TypedSTerm.t )
    Logtk.Statement.t,
    'c )
  CCVector.t

val normalize_simpl : Env.t -> unit
val extension : Extensions.t
