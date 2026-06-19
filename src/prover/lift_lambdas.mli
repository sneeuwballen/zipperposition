(* This file is free software, part of Zipperposition. See file "license" for more details. *)

val setup : Env.t -> unit
(** Register rules in the environment *)

val lift_lambdas : Clause.t -> Clause.t list
val extension : Extensions.t
