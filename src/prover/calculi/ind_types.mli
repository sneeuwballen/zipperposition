
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Deal with Inductive Types} *)

(* 
  - exhaustivity (inference):
    if some term [t : tau] is maximal in a clause, [tau] is inductive,
    and [t] was never split on, then introduce
    [t = c1(...) or t = c2(...) or ... or t = ck(...)] where the [ci] are
    constructors of [tau], and [...] are new Skolems of [t];
    if [t] is ground then Avatar splitting (with xor) should apply directly
      instead, as an optimization, with [k] unary clauses and 1 bool clause

  - disjointness (simplification):
    * an equation [c1(...) = c2(...)] becomes false;
    * a disequation [c1(...) != c2(...)] becomes true

  - injectivity (simplification):
    * an equation [c(t1...tn) = c(u1...un)] simplifies into
      [t1 = u1 & ... & tn = un]
    * a disequation [c(t1...tn) != c(u1...un)] simplifies into
      [t1 != u1 || ... || tn != un] (XXX is it really needed?)
*)

