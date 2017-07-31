
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Clauses} *)

(** The clauses are hashconsed within a context (an application of {!Make}).
    Now clauses also have a {b trail}, that is, a set of boolean literals
    that it depends upon as a conditional guard. *)

open Logtk

val stat_clause_create : Util.stat

module type S = Clause_intf.S

(** Bundle of clause sets *)
type 'c sets = {
  c_set: 'c CCVector.ro_vector; (** main set of clauses *)
  c_sos: 'c CCVector.ro_vector; (** set of support *)
}

(** {2 Clauses that depend on a Context} *)

module Make(Ctx : Ctx.S) : S with module Ctx = Ctx
