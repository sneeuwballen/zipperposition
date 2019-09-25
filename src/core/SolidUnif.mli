module T = Term
module US = Unif_subst

val solidify : ?limit:bool -> ?exception_on_error:bool -> T.t -> T.t
val unify_scoped : ?subst:US.t -> ?counter:int ref -> T.t Scoped.t -> T.t Scoped.t -> US.t list
