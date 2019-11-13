(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module T = Term
module US = Unif_subst

type subst = US.t

type unif_state =
{
  num_identifications : int;
  num_var_imitations  : int;
  num_app_projections : int;
  num_elims           : int;
  depth               : int;
  monomorphic         : bool;
}

module S : sig

  val apply : subst -> T.t Scoped.t -> T.t
  val pp : subst CCFormat.printer

end

val unify : state:unif_state ->
            scope:Scoped.scope ->
            counter:int ref ->
            subst:US.t -> (T.t * T.t * bool) CCList.t -> US.t option OSeq.t

val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> subst option OSeq.t

val single_unif : Term.t Scoped.t -> Term.t Scoped.t -> US.t
