module Make
    (S: sig val st: Flex_state.t end) : sig
  val unify_scoped : Term.t Scoped.t -> Term.t Scoped.t -> Unif_subst.t option OSeq.t
end