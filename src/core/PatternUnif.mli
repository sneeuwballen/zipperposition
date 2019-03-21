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

val unify_scoped : ?subst:subst -> T.t Scoped.t -> T.t Scoped.t -> subst