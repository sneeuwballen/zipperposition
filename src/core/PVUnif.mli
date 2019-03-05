(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module T = Term
module US = Unif_subst

type subst = US.t

module S : sig

  val apply : subst -> T.t Scoped.t -> T.t

end


(* Unify terms of the same scope. Assumes that terms are in eta-long form. *)
val unify : scope:Scoped.scope -> fresh_var_:int ref -> subst:US.t -> (T.t * T.t) CCList.t -> US.t OSeq.t

val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> subst option OSeq.t