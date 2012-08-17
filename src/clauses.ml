(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(* literals and clauses *)

open Hashcons
open Types

module T = Terms
module S = FoSubst
module Utils = FoUtils

(* some pretty printers are useful now *)
open Format

(* ----------------------------------------------------------------------
 * literals
 * ---------------------------------------------------------------------- *)

let left_pos = 1

let right_pos = 2

let string_of_pos s = match s with
  | _ when s == left_pos -> "left"
  | _ when s == right_pos -> "right"
  | _ -> assert false

let string_of_direction = function
    | Left2Right -> "Left to right"
    | Right2Left -> "Right to left"
    | Nodir -> "No direction"

let string_of_comparison = function
  | Lt -> "=<="
  | Gt -> "=>="
  | Eq -> "==="
  | Incomparable -> "=?="
  | Invertible -> "=<->="

let pp_literal ?(sort=false) formatter lit =
  let pp_foterm = T.pp_foterm_sort ~sort in
  match lit with
  | Equation (left, right, false, _) when right = T.true_term ->
    fprintf formatter "~%a" pp_foterm left
  | Equation (left, right, true, _) when right = T.true_term ->
    pp_foterm formatter left
  | Equation (left, right, true, _) when left = T.true_term ->
    pp_foterm formatter right
  | Equation (left, right, false, _) when left = T.true_term ->
    fprintf formatter "~%a" pp_foterm right
  | Equation (left, right, sign, ord) ->
    if sign
    then fprintf formatter "@[%a@ %a@ %a@]"
        pp_foterm left pp_foterm T.eq_term pp_foterm right
    else fprintf formatter "@[<hv 2>%a !%a@ %a@]"
        pp_foterm left pp_foterm T.eq_term pp_foterm right

let opposite_pos p = match p with
  | _ when p = left_pos -> right_pos
  | _ when p = right_pos -> left_pos
  | _ -> assert false

let eq_literal l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
      o1 = o2 && T.eq_foterm l1 l2 && T.eq_foterm r1 r2 && sign1 = sign2

let compare_literal l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
      let c = Pervasives.compare o1 o2 in
      if c <> 0 then c else
        let c = T.compare_foterm l1 l2 in
        if c <> 0 then c else
          let c = T.compare_foterm r1 r2 in
          if c <> 0 then c else
            Pervasives.compare sign1 sign2

let hash_literal lit = match lit with
  | Equation (l, r, sign, _) ->
    if sign
      then Utils.murmur_hash ((Utils.murmur_hash l.hkey) lxor r.hkey)
      else Utils.murmur_hash ((Utils.murmur_hash r.hkey) lxor l.hkey)

let check_type a b = if a.node.sort <> b.node.sort
  then raise (SortError "sides of equations of different sorts") else ()

let mk_eq ~ord a b =
  check_type a b;
  Equation (a, b, true, ord#compare a b)

let mk_neq ~ord a b = 
  check_type a b;
  Equation (a, b, false, ord#compare a b)

let mk_lit ~ord a b sign =
  check_type a b;
  Equation (a, b, sign, ord#compare a b)

let apply_subst_lit ?(recursive=true) ~ord subst lit =
  if subst = S.id_subst then lit
  else match lit with
  | Equation (l,r,sign,_) ->
    assert (l.node.sort = r.node.sort);
    let new_l = S.apply_subst ~recursive subst l
    and new_r = S.apply_subst ~recursive subst r
    in
    Utils.debug 3 (lazy (Utils.sprintf "apply %a to %a gives %a"
        (S.pp_substitution ~sort:true) subst
        (T.pp_foterm_sort ~sort:true) l (T.pp_foterm_sort ~sort:true) new_l));
    Utils.debug 3 (lazy (Utils.sprintf "apply %a to %a gives %a"
        (S.pp_substitution ~sort:true) subst
        (T.pp_foterm_sort ~sort:true) r (T.pp_foterm_sort ~sort:true) new_r));
    mk_lit ~ord new_l new_r sign

let reord_lit ~ord (Equation (l,r,sign,_)) = Equation (l,r,sign, ord#compare l r)

let negate_lit (Equation (l,r,sign,ord)) = Equation (l,r,not sign,ord)

let fmap_lit ~ord f = function
  | Equation (left, right, sign, _) ->
    let new_left = f left
    and new_right = f right in
    Equation (new_left, new_right, sign, ord#compare new_left new_right)

let vars_of_lit = function
  | Equation (left, right, _, _) ->
    T.merge_varlist (T.vars_of_term left) (T.vars_of_term right)

let lit_to_multiset lit = match lit with
  | Equation (l, r, true, _) -> [[l; r]]
  | Equation (l, r, false, _) -> [[l]; [r]]

(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)

let eq_clause c1 c2 =
  try
    List.for_all2 eq_literal c1.clits c2.clits
  with
    Invalid_argument _ -> false

let pp_clause ?(sort=false) formatter {clits=lits} =
  fprintf formatter "@[<h>[%a]@]" (Utils.pp_list ~sep:" | " (pp_literal ~sort)) lits

let compare_clause c1 c2 = FoUtils.lexicograph compare_literal c1.clits c2.clits

module H = Hashcons.Make(struct
  type t = clause
  let equal c1 c2 = eq_clause c1 c2
  let hash c =
    let rec aux h = function
    | [] -> h
    | lit::tail -> aux (Utils.murmur_hash (h lxor hash_literal lit)) tail
    in aux 113 c.clits
end)

let clauses = H.create 251  (* the hashtable for hclauses *)

let hashcons_clause c = H.hashcons clauses c

let eq_hclause hc1 hc2 = hc1 == hc2

let compare_hclause hc1 hc2 = Pervasives.compare hc1.tag hc2.tag

let mk_clause lits proof =
    let all_vars =
      List.fold_left T.merge_varlist [] (List.map vars_of_lit lits) in
    {clits=lits; cvars=all_vars; cproof=proof}

let reord_clause ~ord c = mk_clause (List.map (reord_lit ~ord) c.clits) c.cproof

let apply_subst_cl ?(recursive=true) ~ord subst c =
  if subst = S.id_subst then c
  else
    let new_lits = List.map (apply_subst_lit ~recursive ~ord subst) c.clits in
    mk_clause new_lits c.cproof
  (*  TODO modify proof lazily
  let proof =
    match proof with
    | T.Exact t -> T.Exact (Subst.reloc_subst subst t)
    | T.Step (rule,c1,c2,dir,pos,s) ->
        T.Step(rule,c1,c2,dir,pos,Subst.concat subst s)
  in
  *)

let get_lit clause idx = Utils.list_get clause.clits idx

let get_pos clause pos =
  match pos with
  | idx::side::tpos ->
      let lit = get_lit clause idx in
      let rec find_subterm pos t = match (pos, t.node.term) with
      | [], _ -> t
      | i::pos', Node l when List.length l > i ->
          find_subterm pos' (Utils.list_get l i)
      | _ -> invalid_arg "position does not match term"
      in
      (match lit with
      | Equation (l, _, _, _) when side = left_pos ->
          find_subterm tpos l
      | Equation (_, r, _, _) when side = right_pos ->
          find_subterm tpos r
      | _ -> invalid_arg "wrong side in literal"
      )
  | _ -> invalid_arg "wrong position for clause"

let fresh_clause ~ord maxvar c =
  (* prerr_endline 
    ("varlist = " ^ (String.concat "," (List.map string_of_int varlist)));*)
  let maxvar, _, subst = S.relocate ~recursive:false maxvar c.cvars S.id_subst in
  Utils.debug 3 (lazy (Utils.sprintf "  @[<h>relocate %a using %a@]"
                      (pp_clause ~sort:true) c (S.pp_substitution ~sort:true) subst));
  (apply_subst_cl ~recursive:false ~ord subst c), maxvar

let relocate_clause ~ord varlist c =
  let idx = T.max_var c.cvars in
  let _, newvars, subst = S.relocate ~recursive:false idx c.cvars S.id_subst in
  apply_subst_cl ~recursive:false ~ord subst c

let normalize_clause ~ord c =
  (* sort literals and vars before renaming *)
  let c = {c with cvars=List.stable_sort T.compare_foterm c.cvars;
                  clits=List.stable_sort compare_literal c.clits;}
  in
  fst (fresh_clause ~ord 0 c)

(* ----------------------------------------------------------------------
 * bag of clauses
 * ---------------------------------------------------------------------- *)

module M = Ptmap

type bag = {
  bag_maxvar : int;           (* index of maximum variable *)
  bag_clauses : hclause M.t;  (* clause ID -> clause *)
}

let add_hc_to_bag {bag_maxvar=maxvar_b; bag_clauses=clauses_b} hc =
  let maxvar_hc = T.max_var hc.node.cvars in
  {bag_maxvar=(max maxvar_hc maxvar_b);
   bag_clauses=M.add hc.tag hc clauses_b}

let add_to_bag bag c =
  let hc = hashcons_clause c in
  add_hc_to_bag bag hc, hc

let remove_from_bag ({bag_clauses=clauses_b} as bag) id =
  let new_clauses = M.remove id clauses_b in
  {bag with bag_clauses=new_clauses}

let get_from_bag bag id =
  M.find id bag.bag_clauses

let is_in_bag bag id = M.mem id bag.bag_clauses

let empty_bag = {bag_maxvar=0; bag_clauses=M.empty}

let partition_bag bag pred =
  let bag_yes = ref empty_bag
  and bag_no = ref empty_bag in
  M.iter
    (fun _ hc -> if pred hc
      then bag_yes := add_hc_to_bag !bag_yes hc
      else bag_no := add_hc_to_bag !bag_no hc)
    bag.bag_clauses;
  !bag_yes, !bag_no

let size_bag bag =
  let count = ref 0 in
  M.iter (fun _ _ -> count := !count+1) bag.bag_clauses;
  !count

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

let pp_pos formatter pos =
  fprintf formatter "@[<h>%a@]" (Utils.pp_list ~sep:"." pp_print_int) pos

let pp_clause_pos formatter (c, pos) =
  fprintf formatter "@[<h>[%a at @[<h>%a@]]@]"
    (pp_clause ~sort:false) c pp_pos pos

let pp_hclause formatter c =
  fprintf formatter "@[<h>[%a]_%d@]" (pp_clause ~sort:false) c.node c.tag

let pp_hclause_pos formatter (c, pos, _) =
  fprintf formatter "@[<h>[%a at @[<h>%a@]]@]" pp_hclause c pp_pos pos

let pp_bag formatter bag =
  fprintf formatter "@[<v>";
  M.iter
    (fun _ hc -> fprintf formatter "%a@;" (pp_clause ~sort:false) hc.node)
    bag.bag_clauses;
  fprintf formatter "@]"

let pp_clause_pos_subst formatter (c, pos, subst) =
  fprintf formatter "@[<h>[%a at @[<h>%a@] with %a]@]"
    (pp_clause ~sort:false) c pp_pos pos
    (S.pp_substitution ~sort:false) subst

let pp_proof ~subst formatter p =
  match p with
  | Axiom (f, s) -> fprintf formatter "axiom %s in %s" s f
  | Proof (rule, premisses) ->
    if subst
    then
      fprintf formatter "%s with@ %a" rule
        (Utils.pp_list ~sep:", " pp_clause_pos_subst)
        premisses
    else
      fprintf formatter "%s with@ %a" rule
        (Utils.pp_list ~sep:", " pp_clause_pos)
        (List.map (fun (c, pos, subst) -> (c, pos)) premisses)

let pp_clause_proof formatter clause =
  fprintf formatter "@[<hov 2>%a  <--- @[<hv>%a@]@]@;"
    (pp_clause ~sort:false) clause (pp_proof ~subst:true) (Lazy.force clause.cproof)

let rec pp_proof_rec formatter clause =
  pp_clause_proof formatter clause;
  match Lazy.force clause.cproof with
  | Axiom _ -> ()
  | Proof (_, premisses) ->
      (* print premisses recursively *)
      List.iter
        (fun (c, pos, subst) ->
            pp_proof_rec formatter c)
        premisses

let pp_tstp_clause formatter clause =
  match clause.clits with
  | [] -> pp_print_string formatter "$false"
  | _ ->
    fprintf formatter "@[<h>(%a)@]"
      (Utils.pp_list ~sep:" | " (pp_literal ~sort:false)) clause.clits

let rec pp_tstp_proof formatter clause =
  let hc = hashcons_clause clause in
  match Lazy.force clause.cproof with
  | Axiom (f, ax_name) ->
    fprintf formatter "@[<h>cnf(%d, axiom, %a,@ @[<h>file('%s', %s)@]).@]@;"
      hc.tag pp_tstp_clause clause f ax_name
  | Proof (name, premisses) ->
    let premisses_idx = List.map (fun (c,_,_) -> (hashcons_clause c).tag) premisses in
    (* print the inference *)
    fprintf formatter ("@[<h>cnf(%d, derived, %a,@ " ^^
                       "@[<h>inference(%s, [status(thm)], @[<h>[%a]@])@]).@]@;")
      hc.tag pp_tstp_clause clause name (Utils.pp_list ~sep:"," pp_print_int) premisses_idx;
    (* print every premisse *)
    List.iter (fun (c,_,_) -> pp_tstp_proof formatter c) premisses
  

(*
(* may be moved inside the bag *)
let mk_unit_clause maxvar ty proofterm =
  let varlist =
    let rec aux acc = function
      | T.Leaf _ -> acc
      | T.Var i -> if List.mem i acc then acc else i::acc
      | T.Node l -> List.fold_left aux acc l 
    in
     aux (aux [] ty) proofterm
  in
  let lit = 
    match B.is_eq ty with
    | Some(ty,l,r) ->
         let o = Order.compare_terms l r in
         T.Equation (l, r, ty, o)
    | None -> T.Predicate ty
  in
  let proof = T.Exact proofterm in
  fresh_unit_clause maxvar (0, lit, varlist, proof)


let mk_passive_clause cl =
  (Order.compute_unit_clause_weight cl, cl)


let mk_passive_goal g =
  (Order.compute_unit_clause_weight g, g)
*)

(*
let compare_passive_clauses_weight (w1,(id1,_,_,_)) (w2,(id2,_,_,_)) =
  if w1 = w2 then id1 - id2
  else w1 - w2

let compare_passive_clauses_age (_,(id1,_,_,_)) (_,(id2,_,_,_)) =
  id1 - id2
*)
