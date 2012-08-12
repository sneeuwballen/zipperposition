(* literals and clauses *)

open Hashcons
open Types

module T = Terms
module S = FoSubst
module Utils = FoUtils

(* ----------------------------------------------------------------------
 * literals
 * ---------------------------------------------------------------------- *)

let left_pos = 1

let right_pos = 2

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

let check_type a b = if a.node.sort <> b.node.sort
  then raise (SortError "sides of equations of different sorts") else ()

let mk_eq ~ord a b =
  check_type a b;
  Equation (a, b, true, ord#compare_terms a b)

let mk_neq ~ord a b = 
  check_type a b;
  Equation (a, b, false, ord#compare_terms a b)

let mk_lit ~ord a b sign =
  check_type a b;
  Equation (a, b, sign, ord#compare_terms a b)

let negate_lit (Equation (l,r,sign,ord)) = Equation (l,r,not sign,ord)

let fmap_lit ~ord f = function
  | Equation (left, right, sign, _) ->
    let new_left = f left
    and new_right = f right in
    Equation (new_left, new_right, sign, ord#compare_terms new_left new_right)

let vars_of_lit = function
  | Equation (left, right, _, _) ->
    T.merge_varlist (T.vars_of_term left) (T.vars_of_term right)


(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)

let eq_clause c1 c2 =
  try
    List.for_all2 eq_literal c1.clits c2.clits
  with
    Invalid_argument _ -> false

let compare_clause c1 c2 = FoUtils.lexicograph compare_literal c1.clits c2.clits

module HC = Hashcons.Make(struct
  type t = clause
  let equal x y = eq_clause x y
  let hash x = Hashtbl.hash x.clits
end)

let clauses = HC.create 251  (* the hashtable for hclauses *)

let hashcons_clause c = HC.hashcons clauses c

let eq_hclause hc1 hc2 = hc1 == hc2

let compare_hclause hc1 hc2 = Pervasives.compare hc1.tag hc2.tag

let mk_clause lits proof =
    let all_vars =
      List.fold_left T.merge_varlist [] (List.map vars_of_lit lits) in
    {clits=lits; cvars=all_vars; cproof=proof}

let apply_subst_lit ?(recursive=true) ~ord subst =
  function
  | Equation (l,r,sign,_) ->
    mk_lit ~ord
      (S.apply_subst ~recursive subst l)
      (S.apply_subst ~recursive subst r)
      sign

let apply_subst_cl ?(recursive=true) ~ord subst c =
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
  let maxvar, _, subst = S.relocate maxvar c.cvars S.id_subst in
  (apply_subst_cl ~recursive:false ~ord subst c), maxvar

let relocate_clause ~ord varlist c =
  let idx = T.max_var c.cvars in
  let _, newvars, subst = S.relocate idx c.cvars S.id_subst in
  apply_subst_cl ~recursive:false ~ord subst c

let normalize_clause ~ord c = fst (fresh_clause ~ord 0 c)

(* ----------------------------------------------------------------------
 * bag of clauses
 * ---------------------------------------------------------------------- *)

module M : Map.S with type key = int
  = Map.Make(
     struct
       type t = int
       let compare = Pervasives.compare
     end)

type bag = {
  bag_maxvar : int;           (* index of maximum variable *)
  bag_clauses : hclause M.t;  (* clause ID -> clause *)
}

let add_to_bag {bag_maxvar=maxvar_b; bag_clauses=clauses_b} c =
  let hc = hashcons_clause c
  and maxvar_c = T.max_var c.cvars in
  {bag_maxvar=(max maxvar_c maxvar_b);
   bag_clauses=M.add hc.tag hc clauses_b}, hc

let remove_from_bag ({bag_clauses=clauses_b} as bag) id =
  let new_clauses = M.remove id clauses_b in
  {bag with bag_clauses=new_clauses}

let get_from_bag bag id =
  M.find id bag.bag_clauses

let is_in_bag bag id = M.mem id bag.bag_clauses

let empty_bag = {bag_maxvar=0; bag_clauses=M.empty}

let size_bag bag = M.cardinal bag.bag_clauses

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
