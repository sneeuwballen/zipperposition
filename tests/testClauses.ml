(** test clauses *)

open Types
open Symbols

module C = Clauses
module O = Orderings
module T = Terms
module H = Helpers
module S = FoSubst
module TT = TestTerms
module Utils = FoUtils
module Sup = Superposition

let print_clause_pair formatter (c1, c2) = 
  Format.fprintf formatter "(%a, %a)" !C.pp_clause#pp_h c1 !C.pp_clause#pp c2

let ord = O.default_ordering ()

(** random literal *)
let random_lit () =
  let sign = H.R.bool () in
  (* a random predicate *)
  let random_pred () =
    let p = TT.random_pred () in
    ord#refresh ();
    C.mk_lit ~ord p T.true_term sign
  (* a random equation *)
  and random_eq () =
    let l = TT.random_term ()
    and r = TT.random_term () in
    ord#refresh ();
    C.mk_lit ~ord l r sign
  in if H.R.bool ()  (* choice between equation and predicate *)
    then random_pred ()
    else random_eq ()

(** random clause *)
let random_clause ?(size=4) () =
  if H.random_in 0 200 >= 198 then
    (* empty clause, from time to time *)
    C.mk_hclause ~ord [] (lazy (Axiom ("", "empty!"))) []
  else
    (* build an actual clause *)
    let size = H.random_in 1 size in
    let lits = Utils.times size (fun _ -> random_lit ()) in
    C.mk_hclause ~ord lits (lazy (Axiom ("", "random"))) []

(* pair of random clauses *)
let random_clause_pair () = (random_clause ~size:2 (), random_clause ~size:5 ())

(** check that renaming works *)
let check_fresh c =
  let maxvar = T.max_var c.hcvars in
  let ord = O.default_ordering () in
  let c' = C.fresh_clause ~ord (maxvar+1) c in
  if Utils.list_inter T.eq_term c.hcvars (Lazy.force c'.cvars) = []
    then H.TestOk
    else H.TestFail (c, c')

(** check subsumption property *)
let check_subsumption (c1, c2) =
  (* check whether a clause is a subset of another clause *)
  let rec clause_subset ~ord c1 c2 subst =
    lits_subset
      (List.map (C.apply_subst_lit ~ord subst) (Array.to_list c1.clits))
      (Array.to_list c2.hclits)
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
  let maxvar = T.max_var c2.hcvars in
  let c1' = C.fresh_clause ~ord maxvar c1 in
  (* see whether c1 subsumes c2 *)
  match Sup.subsumes_with c1'.clits c2.hclits with
  | None -> H.TestPreconditionFalse
  | Some subst ->
    if clause_subset ~ord c1' c2 subst
      then H.TestOk
      else H.TestFail (c1, c2, subst)

let print_subsuming_failure formatter (c1,c2,subst) =
  Format.fprintf formatter "@[<hv 3>%a@; %a@; with %ag]"
    !C.pp_clause#pp_h c1 !C.pp_clause#pp_h c2 S.pp_substitution subst

let run () =
  Utils.set_debug 1;
  Format.printf "run clauses tests@.";
  H.check_and_print ~name:"check_fresh" check_fresh
    (random_clause ~size:4) print_clause_pair 2000;
  H.check_and_print ~name:"check_subsumption" check_subsumption
    random_clause_pair print_subsuming_failure 1000

