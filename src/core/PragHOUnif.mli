(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module T = Term
module US = Unif_subst

type subst = US.t

type unif_state =
{
  norm_deref          : US.t -> T.t Scoped.t -> T.t;
  num_identifications : int;
  num_var_imitations  : int;
  num_app_projections : int;
  num_elims           : int;
  depth               : int
}

module S : sig

  val apply : subst -> T.t Scoped.t -> T.t
  val pp : subst CCFormat.printer

end


val max_app_projections : int ref
val max_var_imitations  : int ref
val max_identifications : int ref
val max_elims           : int ref
val max_depth           : int ref

(* Disable getting only the first solution for unifying arguments
   after performing identification *)
val disable_conservative_elim : unit -> unit
(* Apply imitation and projection rules for flex-flex pairs *)
val enable_imit_first : unit -> unit
(* Solve pairs that have exactly one unifier directly using 
   an extension of pattern unification algorithm. *)
val enable_solve_var : unit -> unit


val unify : state:unif_state ->
            scope:Scoped.scope ->
            counter:int ref ->
            subst:US.t -> (T.t * T.t * bool) CCList.t -> US.t option OSeq.t

val unify_scoped : T.t Scoped.t -> T.t Scoped.t -> subst option OSeq.t