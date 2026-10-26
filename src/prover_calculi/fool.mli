(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

type term = Term.t

(** Deal with boolean subterms as if prop was a specific case of datatype. The
    rules are:

    C[s] ---------------------- C[true] or s=false where s boolean, not a
    variable, proper subterm *)

(** {5 Registration} *)

val setup : Env.t -> unit
(** Register rules in the environment *)

val rw_bool_lits : Env.multi_simpl_rule

(** {2 As Extension} *)

val extension : Extensions.t
