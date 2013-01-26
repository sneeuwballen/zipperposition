(** test clauses *)

open Types
open Symbols

module C = Clauses
module O = Orderings
module T = Terms
module H = Helpers
module S = FoSubst
module Lits = Literals
module TT = TestTerms
module Utils = FoUtils
module Sup = Superposition

let print_clause_pair formatter (c1, c2) = 
  Format.fprintf formatter "(%a, %a)" !C.pp_clause#pp_h c1 !C.pp_clause#pp c2

let ord = TT.ord
let ctx = {ctx_ord=ord; ctx_select=no_select;}

(** random literal *)
let random_lit () =
  let sign = H.R.bool () in
  (* a random predicate *)
  let random_pred () =
    let p = TT.random_pred () in
    Lits.mk_lit ~ord p T.true_term sign
  (* a random equation *)
  and random_eq () =
    let l = TT.random_term ()
    and r = TT.random_term () in
    Lits.mk_lit ~ord l r sign
  in if H.R.bool ()  (* choice between equation and predicate *)
    then random_pred ()
    else random_eq ()

(** random clause *)
let random_clause ?(size=4) () =
  if H.random_in 0 200 >= 198 then
    (* empty clause, from time to time *)
    C.mk_hclause ~ctx [] (Axiom ("", "empty!"))
  else
    (* build an actual clause *)
    let size = H.random_in 1 size in
    let lits = Utils.times size (fun _ -> random_lit ()) in
    C.mk_hclause ~ctx lits (Axiom ("", "random"))

(* pair of random clauses *)
let random_clause_pair () =
  let c1 = random_clause ~size:2 ()
  and c2 = random_clause ~size:5 () in
  ((c1, 0), (c2, T.max_var c1.hcvars + 1))

(** check subsumption property *)
let check_subsumption ((c1,o1), (c2,o2)) =
  (* check whether a clause is a subset of another clause *)
  let rec clause_subset ~ord (c1,o1) (c2,o2) subst =
    lits_subset
      (Lits.apply_subst_list ~ord subst (Array.to_list c1.hclits, o1))
      (Lits.apply_subst_list ~ord subst (Array.to_list c2.hclits, o2))
  and lits_subset lits1 lits2 = match lits1 with
  | [] -> true
  | x::lits1' -> if List.exists (Lits.eq_com x) lits2
    then let lits2' = remove_once x lits2 in
      assert (List.length lits2' = (List.length lits2) - 1);
              lits_subset lits1' lits2'
    else false
  (* remove one occurrence of x in l *)
  and remove_once x l = match l with
  | [] -> assert false
  | y::l' -> if Lits.eq_com x y then l' else y :: (remove_once x l')
  in
  let offset = T.max_var c1.hcvars + 1 in
  (* see whether c1 subsumes c2 *)
  match Sup.subsumes_with (c1.hclits,0) (c2.hclits,offset) with
  | None -> H.TestPreconditionFalse
  | Some subst ->
    if clause_subset ~ord (c1,0) (c2,offset) subst
      then H.TestOk
      else H.TestFail ((c1,0), (c2,offset), subst)

let print_subsuming_failure formatter ((c1,o1),(c2,o2),subst) =
  Format.fprintf formatter "@[<hv 3>%a[%d]@; %a[%d]@; with %a@]"
    !C.pp_clause#pp_h c1 o1 !C.pp_clause#pp_h c2 o2 S.pp_substitution subst

let run () =
  Utils.set_debug 1;
  Format.printf "run clauses tests@.";
  H.check_and_print ~name:"check_subsumption" check_subsumption
    random_clause_pair print_subsuming_failure 1000

