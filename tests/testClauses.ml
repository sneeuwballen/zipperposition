(** test clauses *)

open Types

module C = Clauses
module T = Terms
module H = Helpers
module TT = TestTerms
module Utils = FoUtils

(** random literal *)
let random_lit ~ord () =
  let sign = H.R.bool () in
  (* a random predicate *)
  let random_pred () =
    let p = TT.random_pred () in
    C.mk_lit ~ord p T.true_term sign
  (* a random equation *)
  and random_eq () =
    let l = TT.random_term ()
    and r = TT.random_term ()
    in C.mk_lit ~ord l r sign
  in if H.R.bool ()  (* choice between equation and predicate *)
    then random_pred ()
    else random_eq ()

let random_clause ~ord () =
  let size = H.random_in 1 4 in
  let lits = List.map
    (fun _ -> random_lit ~ord ())
    (Utils.list_range 0 size)
  in C.mk_clause ~ord lits (lazy (Axiom ("", "random")))

let run () =
  Format.printf "run clauses tests@." 

