(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module T = Term

type subst = Subst.t

module S : sig

  val apply : subst -> T.t -> T.t

end

val project_onesided : T.t -> subst OSeq.t

val imitate : T.t -> T.t -> (T.var * int) list -> subst OSeq.t

val identify : T.t -> T.t -> (T.var * int) list -> subst OSeq.t

val eliminate : T.t -> T.t -> (Type.t HVar.t * int) list -> subst OSeq.t

(** Find disagreeing subterms. 
    This function also returns a list of variables occurring above the
    disagreement pair, along with the index of the argument that the disagreement
    pair occurs in. *)
val find_disagreement : T.t -> T.t -> ((T.t * T.t) * (T.var * int) CCList.t) option

val unify : T.t -> T.t -> subst option OSeq.t

val unify_nonterminating : T.t -> T.t -> subst OSeq.t

val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> Unif_subst.t option OSeq.t