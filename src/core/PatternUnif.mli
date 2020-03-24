(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Pattern unification algorithm implementation} *)

(** This module implements pattern unification oracle described in
    \url{http://matryoshka.gforge.inria.fr/pubs/hounif_paper.pdf}.
    It can be applied to terms out of the pattern fragment in which
    case it raises NotInFragment exception.
*)


module US = Unif_subst

type subst = US.t

module S : sig

  val apply : subst -> Term.t Scoped.t -> Term.t
  val pp : subst CCFormat.printer

end

exception NotUnifiable
exception NotInFragment


val eta_expand_otf : subst:subst -> scope:Scoped.scope -> Type.t list -> Type.t list -> Term.t -> Term.t -> Term.t * Term.t * Type.t list
val norm_deref :  Unif_subst.t -> Term.t Scoped.t -> Term.t



(* Does unification on types (or other simple constructs) and catches
   exception in case of non-unifiability *)
val unif_simple : ?subst:Subst.t -> scope:int -> Term.t -> Term.t -> US.t option

val unify_scoped : ?subst:subst -> ?counter:int ref -> Term.t Scoped.t -> Term.t Scoped.t -> subst