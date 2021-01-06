module S = Subst
module US = Unif_subst
module LL = OSeq
module T = Term

module type PARAMETERS = sig
  exception NotInFragment
  exception NotUnifiable
  type flag_type
  val init_flag : flag_type
  val flex_state : Flex_state.t
  val identify_scope : T.t Scoped.t -> T.t Scoped.t -> T.t * T.t * Scoped.scope * S.t
  val identify_scope_l : T.t list Scoped.t -> T.t list Scoped.t -> T.t list * T.t list * Scoped.scope * S.t
  val frag_algs : unit -> (T.t Scoped.t -> T.t Scoped.t -> S.t -> S.t list) list
  val pb_oracle : (T.t Scoped.t -> T.t Scoped.t -> flag_type -> S.t -> Scoped.scope -> (S.t * flag_type) option LL.t)
end

module type S = sig 
  val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> S.FO.t option OSeq.t
  val unify_scoped_l : T.t list Scoped.t -> T.t list Scoped.t -> S.FO.t option OSeq.t
end

module type US = sig 
  val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> US.t option OSeq.t
  val unify_scoped_l : T.t list Scoped.t -> T.t list Scoped.t -> US.t option OSeq.t
end

module Make(X:PARAMETERS) : S
