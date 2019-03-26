(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module T = Term
module US = Unif_subst

type subst = US.t

module S : sig

  val apply : subst -> T.t Scoped.t -> T.t
  val pp : subst CCFormat.printer

end

exception NotUnifiable
exception NotInFragment



(* Given two terms and their lambda prefixes, expands one of the terms
   ,if necessary, to have the same size of the lambda prefix as the other
   term *)
val eta_expand_otf : Type.t list -> Type.t list -> T.t -> T.t -> T.t * T.t * Type.t list

(* Does unification on types (or other simple constructs) and catches
   exception in case of non-unifiability *)
val unif_simple : ?subst:Subst.t -> scope:int -> T.t -> T.t -> US.t option

val unify_scoped : ?subst:subst -> ?counter:int ref -> T.t Scoped.t -> T.t Scoped.t -> subst