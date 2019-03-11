(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module T = Term
module US = Unif_subst

type subst = US.t

module S : sig

  val apply : subst -> T.t Scoped.t -> T.t
  val pp : subst CCFormat.printer

end

val enable_conservative_elim : unit -> unit
val set_imit_first : unit -> unit


(* Unify terms of the same scope. Assumes that terms are in eta-long form. *)
val unify : depth:int ->
            scope:Scoped.scope ->
            fresh_var_:int ref ->
            subst:US.t -> (T.t * T.t * bool) CCList.t -> US.t option OSeq.t

val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> subst option OSeq.t