(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Pragmatic variant of JP algorithm} *)

(** Provides plug-in module for UnifFramework.ml that implements
    pragmatic restriction of the unification algorithm described 
    in the paper Efficient Full Higher-Order Unification
    \url{http://matryoshka.gforge.inria.fr/pubs/hounif_paper.pdf} *)

module IntSet : CCSet.S with type elt = CCInt.t

val elim_subsets_rule : ?max_elims:int option -> elim_vars:IntSet.t ref -> counter:IntSet.elt ref ->
  scope:Scoped.scope -> Term.t -> Term.t -> int -> (Subst.FO.t * int) OSeq.t
val proj_hs : counter:int ref -> scope:Scoped.scope -> flex:Term.t -> Term.t -> Subst.FO.t CCList.t

module Make (S : sig val st : Flex_state.t end) : sig
  val unify_scoped : Term.t Scoped.t -> Term.t Scoped.t -> Unif_subst.t option OSeq.t
end 