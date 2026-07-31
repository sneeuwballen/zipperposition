(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition
(** {1 Booleans} *)

open Logtk

val k_enabled : bool Flex_state.key
val k_pa_renaming : bool Flex_state.key
val setup : Env.t -> unit

val update_form_counter :
  action:[< `Decrease | `Increase ] -> Env.t -> Clause.t -> unit

val extension : Extensions.t
