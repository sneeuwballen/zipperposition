
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Clauses} *)

(** The clauses are hashconsed within a context (an application of {!Make}).
    Now clauses also have a {b trail}, that is, a set of boolean literals
    that it depends upon as a conditional guard. *)

open Libzipperposition

val stat_fresh : Util.stat
val stat_clause_create : Util.stat

module type S = Clause_intf.S

(** {2 Clauses that depend on a Context} *)

module Make(Ctx : Ctx.S) : S with module Ctx = Ctx
