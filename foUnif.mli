(*
    ||M||  This file is part of HELM, an Hypertextual, Electronic
    ||A||  Library of Mathematics, developed at the Computer Science
    ||T||  Department, University of Bologna, Italy.
    ||I||
    ||T||  HELM is free software; you can redistribute it and/or
    ||A||  modify it under the terms of the GNU General Public License
    \   /  version 2 or (at your option) any later version.
     \ /   This software is distributed as is, NO WARRANTY.
      V_______________________________________________________________ *)

(* $Id: nCic.ml 9058 2008-10-13 17:42:30Z tassi $ *)

exception UnificationFailure of string Lazy.t

(* unify terms, returns a substitution of raise an exception *)
val unification: Terms.foterm -> Terms.foterm -> Terms.substitution

(* match_term [a] [b] returns sigma such that sigma(a) = b *)
val match_term: Terms.foterm -> Terms.foterm -> Terms.substitution

(* alpha_eq [a] [b] returns sigma with sigma(a) = b, and sigma
 * a variable renaming, or raise UnificationFailure *)
val alpha_eq: Terms.foterm -> Terms.foterm -> Terms.substitution

