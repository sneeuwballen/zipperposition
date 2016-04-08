
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Deal with Inductive Types} *)

(*

  - disjointness (simplification):
    * an equation [c1(...) = c2(...)] becomes false;
    * a disequation [c1(...) != c2(...)] becomes true

  - injectivity (simplification):
    * an equation [c(t1...tn) = c(u1...un)] simplifies into
      [t1 = u1 & ... & tn = un]
    * a disequation [c(t1...tn) != c(u1...un)] simplifies into
      [t1 != u1 || ... || tn != un]
      (XXX is it really needed? probably, if we keep literal selection)

  - acyclicity (simplification):
    * an equation [t = c1(...,(...,ck(...,t, ...), ...),...))] where every [c]
      is an inductive constructor becomes [false]
    * an inequation [t != c1(....(...,ck(...,t,...), ...),...)]
      becomes [true]
*)
module Make(E : Env_intf.S) : sig

  val setup : unit -> unit
end

val extension : Extensions.t
