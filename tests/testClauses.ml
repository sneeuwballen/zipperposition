(** test clauses *)

open Types

module C = Clauses
module O = Orderings
module T = Terms
module H = Helpers
module TT = TestTerms
module Utils = FoUtils

let print_clause = C.pp_clause ~sort:false
let print_clause_pair formatter (c1, c2) = 
  Format.fprintf formatter "(%a, %a)" print_clause c1 print_clause c2

(** random literal *)
let random_lit () =
  let sign = H.R.bool () in
  (* a random predicate *)
  let random_pred () =
    let p = TT.random_pred () in
    let ord = O.dummy_ordering in
    C.mk_lit ~ord p T.true_term sign
  (* a random equation *)
  and random_eq () =
    let l = TT.random_term ()
    and r = TT.random_term ()
    and ord = O.dummy_ordering in
    C.mk_lit ~ord l r sign
  in if H.R.bool ()  (* choice between equation and predicate *)
    then random_pred ()
    else random_eq ()

(** random clause *)
let random_clause () =
  if H.random_in 0 200 >= 198 then
    (* empty clause, from time to time *)
    let ord = O.dummy_ordering in
    C.mk_clause ~ord [] (lazy (Axiom ("", "empty!")))
  else
    (* build an actual clause *)
    let size = H.random_in 1 4 in
    let lits = Utils.times size (fun _ -> random_lit ()) in
    let ord = O.dummy_ordering in
    C.mk_clause ~ord lits (lazy (Axiom ("", "random")))

(** check that renaming works *)
let check_fresh c =
  let maxvar = T.max_var c.cvars in
  let ord = O.default_ordering () in
  let c', _ = C.fresh_clause ~ord (maxvar+1) c in
  if Utils.list_inter T.eq_foterm c.cvars c'.cvars = []
    then H.TestOk
    else H.TestFail (c, c')

let run () =
  Format.printf "run clauses tests@.";
  H.check_and_print ~name:"check_fresh" check_fresh random_clause print_clause_pair 5000

