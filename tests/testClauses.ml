(** test clauses *)

open Types

module C = Clauses
module O = Orderings
module T = Terms
module H = Helpers
module S = FoSubst
module TT = TestTerms
module Utils = FoUtils
module Sup = Superposition

let print_clause formatter c = !C.pp_clause#pp formatter c
let print_clause_pair formatter (c1, c2) = 
  Format.fprintf formatter "(%a, %a)" !C.pp_clause#pp c1 !C.pp_clause#pp c2

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
let random_clause ?(size=4) () =
  let ord = O.default_ordering () in
  if H.random_in 0 200 >= 198 then
    (* empty clause, from time to time *)
    C.mk_clause ~ord [] ~selected:(lazy []) (lazy (Axiom ("", "empty!"))) (lazy [])
  else
    (* build an actual clause *)
    let size = H.random_in 1 size in
    let lits = Utils.times size (fun _ -> random_lit ()) in
    C.mk_clause ~ord lits ~selected:(lazy [])(lazy (Axiom ("", "random"))) (lazy [])

(* pair of random clauses *)
let random_clause_pair () = (random_clause ~size:2 (), random_clause ~size:5 ())

(** check that renaming works *)
let check_fresh c =
  let maxvar = T.max_var c.cvars in
  let ord = O.default_ordering () in
  let c', _ = C.fresh_clause ~ord (maxvar+1) c in
  if Utils.list_inter T.eq_term c.cvars c'.cvars = []
    then H.TestOk
    else H.TestFail (c, c')

(** check subsumption property *)
let check_subsumption (c1, c2) =
  (* check whether a clause is a subset of another clause *)
  let rec clause_subset c1 c2 = lits_subset c1.clits c2.clits
  and lits_subset lits1 lits2 = match lits1 with
  | [] -> true
  | x::lits1' -> if List.exists (C.eq_literal_com x) lits2
    then let lits2' = remove_once x lits2 in
      assert (List.length lits2' = (List.length lits2) - 1); lits_subset lits1' lits2'
    else false
  (* remove one occurrence of x in l *)
  and remove_once x l = match l with
  | [] -> assert false
  | y::l' -> if C.eq_literal_com x y then l' else y :: (remove_once x l')
  in
  let ord = O.default_ordering () in
  let c1 = C.relocate_clause ~ord c2.cvars c1 in
  (* see whether c1 subsumes c2 *)
  match Sup.subsumes_with c1 c2 with
  | None -> H.TestPreconditionFalse
  | Some subst ->
    let c1' = C.apply_subst_cl ~ord subst c1 in
    if clause_subset c1' c2
      then H.TestOk
      else H.TestFail (c1, c2, c1', c2, subst)

let print_subsuming_failure formatter (c1,c2,c1',c2',subst) =
  Format.fprintf formatter "@[<hv 3>(%a,@; %a,@; %a,@; %a,@; %a)@]"
    print_clause c1 print_clause c2 print_clause c1' print_clause c2'
    S.pp_substitution subst

let run () =
  Utils.set_debug 1;
  Format.printf "run clauses tests@.";
  H.check_and_print ~name:"check_fresh" check_fresh
    (random_clause ~size:4) print_clause_pair 2000;
  H.check_and_print ~name:"check_subsumption" check_subsumption
    random_clause_pair print_subsuming_failure 1000

