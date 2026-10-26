(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Interfacing with E} *)

(** {5 Registration} *)

val set_e_bin : string -> unit
val try_e : Env.t -> Clause.t Iter.t -> Clause.t Iter.t -> Clause.t option

val setup : Env.t -> unit
(** Register rules in the environment *)
