(** test terms *)

open Types

module T = Terms
module TT = TestTerms
module H = Helpers
module S = FoSubst
module C = Clauses
module O = Orderings
module Utils = FoUtils

let instantiate t1 t2 =
  (* find a subst for t1 and t2 *)
  let vars = T.merge_varlist t1.vars t2.vars in
  let subst = List.map (fun v -> (v, TT.random_term ~ground:true ())) vars in
  S.apply_subst subst t1, S.apply_subst subst t2

(** generate all pairs (a, b, compare a b) for a, b in list *)
let all_orders ~ord l =
  let all = ref [] in
  let rec recurse l' =
    match l' with
    | [] -> ()
    | t::l'' -> 
      List.iter (fun t' -> all := (t, t', ord#compare t t') :: !all) l';
      recurse l''
  in
  recurse l;
  !all

(** check whether cmp1 is more specific (butcompatible) than cmp2 *)
let more_specific cmp1 cmp2 =
  match cmp1, cmp2 with
  | Eq, Eq -> true
  | _, Incomparable -> true
  | Lt, Lt | Gt, Gt -> true
  | _ -> false

let check_more_specific ~ord a b cmp a' b' =
  let cmp' = ord#compare a' b' in
  if not (more_specific cmp' cmp)
    then begin
      Format.printf "  more_specific failed on @[<h>%a %s %a (%a %s %a)@]"
        !T.pp_term#pp a' (C.string_of_comparison cmp') !T.pp_term#pp b' 
        !T.pp_term#pp a (C.string_of_comparison cmp) !T.pp_term#pp b;
      assert false
    end

(** check invariants of reduction orderings *)
let check_properties ~ord (a, b, cmp) =
  (* invariant by substitution *)
  (if not (T.is_ground_term a) || not (T.is_ground_term b)
    then begin
      let a', b' = instantiate a b in
      check_more_specific ~ord a b cmp a' b';
    end);
  (* subterm property *)
  (if a == b then assert (cmp = Eq));
  (if T.member_term a b && a != b
    then assert (cmp = Lt));
  (if T.member_term b a && a != b
    then assert (cmp = Gt));
  (* monotonicity *)
  let ga = T.mk_node "g" univ_sort [a]
  and gb = T.mk_node "g" univ_sort [b] in
  assert (cmp = ord#compare ga gb)

(** check invariants on the list of terms *)
let check ord_name ~ord terms =
  Format.printf "  check %s (%a)@." ord_name T.pp_signature ord#symbol_ordering#signature;
  let pairs = all_orders ~ord terms in
  List.iter (check_properties ~ord) pairs

(** check similar results for RPO and RPO6 *)
let check_same ~so terms =
  let pairs = all_orders ~ord:(new O.rpo so) terms
  and pairs6 = all_orders ~ord:(new O.rpo6 so) terms in
  List.iter2
    (fun (t1, t2, cmp12) (t1', t2', cmp12') ->
      assert (t1 == t1' && t2 == t2');
      if not (cmp12 = cmp12')
        then Format.printf "@[<h>on %a %a, RPO gave %s and RPO6 gave %s@]@."
          !T.pp_term#pp t1 !T.pp_term#pp t2 (C.string_of_comparison cmp12)
          (C.string_of_comparison cmp12'))
    pairs pairs6

let n = 500

let run () =
  Format.printf "run orderings test (%d terms)@." n;
  Utils.set_debug 2;
  (* generate terms *)
  let terms = Utils.times n (TT.random_term ~ground:false) in
  let so = O.default_symbol_ordering () in
  check "KBO" ~ord:(new O.kbo so) terms;
  check "RPO" ~ord:(new O.rpo so) terms;
  check "RPO6" ~ord:(new O.rpo6 so) terms;
  check_same ~so terms
