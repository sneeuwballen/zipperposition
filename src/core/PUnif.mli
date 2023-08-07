(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Pragmatic variant of JP algorithm} *)

(** Provides plug-in module for UnifFramework.ml that implements
    pragmatic restriction of the unification algorithm described 
    in the paper Efficient Full Higher-Order Unification
    {{:http://matryoshka.gforge.inria.fr/pubs/hounif_paper.pdf} (paper)} *)

module IntSet : CCSet.S with type elt = CCInt.t

val elim_subsets_rule : ?max_elims:int option -> elim_vars:IntSet.t ref -> counter:IntSet.elt ref ->
  scope:Scoped.scope -> Term.t -> Term.t -> int -> (Subst.FO.t * int) OSeq.t
val proj_hs : counter:int ref -> scope:Scoped.scope -> flex:Term.t -> Term.t -> Subst.FO.t CCList.t

module Make (S : sig val st : Flex_state.t end) : UnifFramework.US
