module T = Term
module US = Unif_subst

module Make(S : sig val st : Flex_state.t end): 
sig
  val solidify : ?limit:bool -> ?exception_on_error:bool -> T.t -> T.t
  val unify_scoped : ?subst:US.t -> ?counter:int ref -> 
    T.t Scoped.t -> T.t Scoped.t -> US.t list
end