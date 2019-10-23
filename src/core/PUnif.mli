module IntSet : CCSet.S with type elt = CCInt.t


val unify_scoped : Term.t Scoped.t -> Term.t Scoped.t -> Unif_subst.t option OSeq.t
val proj_hs : counter:int ref -> scope:Scoped.scope -> flex:Term.t -> Term.t -> Subst.FO.t CCList.t
val elim_subsets_rule : ?max_elims:int option -> elim_vars:IntSet.t ref -> counter:IntSet.elt ref ->
                        scope:Scoped.scope -> Term.t -> Term.t -> int -> (Subst.FO.t * int) OSeq.t