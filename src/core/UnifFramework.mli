module S = Subst
module LL = OSeq
module T = Term

module type PARAMETERS = sig
  exception NotInFragment
  exception NotUnifiable
  type flag_type
  val init_flag : flag_type
  val identify_scope : T.t Scoped.t -> T.t Scoped.t -> T.t * T.t * Scoped.scope * S.t
  val frag_algs : (T.t Scoped.t -> T.t Scoped.t -> S.t -> S.t) list
  val pb_oracle : (T.t Scoped.t -> T.t Scoped.t ->  flag_type -> Scoped.scope -> (S.t * flag_type) option LL.t  )
end

module Make(X:PARAMETERS) : sig 
  val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> S.FO.t option OSeq.t
end