
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Meta Prover for zipperposition} *)

type 'a or_error = [`Ok of 'a | `Error of string]

module type S = MetaProverState_intf.S

module Make(E : Env.S) : S with module E = E

(** {2 Interface to {!Env}} *)

val key : (module S) CCMixtbl.injection

val get_env : (module Env.S) -> (module S)
(** [get_env (module Env)] returns the meta-prover saved in [Env],
    assuming the extension has been loaded in [Env].
    @raise Not_found if the extension was not loaded already. *)

val extension : Extensions.t
(** Prover extension *)
