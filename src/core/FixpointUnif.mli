module T = Term
module US = Unif_subst

val unify_scoped : ?subst:US.t -> ?counter:int ref -> T.t Scoped.t -> T.t Scoped.t -> US.t