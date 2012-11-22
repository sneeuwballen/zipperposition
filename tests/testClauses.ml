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
    C.mk_eqn p T.true_term sign
  (* a random equation *)
  and random_eq () =
    let l = TT.random_term ()
    and r = TT.random_term () in
    C.mk_eqn l r sign in
  let lit = if H.R.bool ()  (* choice between equation and predicate *)
    then random_pred ()
    else random_eq () in
  incr T.sig_version;
  lit

(** random clause *)
let random_clause ?(size=4) () =
  if H.random_in 0 200 >= 198 then
    (* empty clause, from time to time *)
    let cs = C.mk_state ~ord:(O.default_ordering ()) ~select:no_select in
    C.mk_clause ~cs [||] (lazy (Axiom ("", "empty!"))) []
  else
    (* build an actual clause *)
    let size = H.random_in 1 size in
    let lits = Array.of_list (Utils.times size (fun _ -> random_lit ())) in
    let cs = C.mk_state ~ord:(O.default_ordering ()) ~select:no_select in
    C.mk_clause ~cs lits (lazy (Axiom ("", "random"))) []

(* pair of random clauses *)
let random_clause_pair () = (random_clause ~size:2 (), random_clause ~size:5 ())

(** check subsumption property *)
let check_subsumption (c1, c2) =
  (* check whether a clause is a subset of another clause *)
  let rec clause_subset c1 c2 = lits_subset c1.clits c2.clits
  and lits_subset lits1 lits2 =
    Array.fold_left
      (fun acc lit -> acc && Array.fold_left (fun acc' lit' -> acc' || C.eq_eqn lit.lit_eqn lit'.lit_eqn) false lits2)
      true lits1
  in
  let cs = C.mk_state ~ord:(O.default_ordering ()) ~select:no_select in
  let c1 = C.fresh_clause ~cs (T.max_var c2.cvars + 1) c1 in
  (* see whether c1 subsumes c2 *)
  match Sup.subsumes_with c1 c2 with
  | None -> H.TestPreconditionFalse
  | Some subst ->
    let c1' = C.apply_subst_cl ~cs subst c1 in
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
  H.check_and_print ~name:"check_subsumption" check_subsumption
    random_clause_pair print_subsuming_failure 1000

