(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

type term = Term.t

val setup : unit -> unit

val rw_bool_lits : Env.multi_simpl_rule
(** Register rules in the environment *)

val extension : Extensions.t
