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

open Types
open Symbols

module T = Terms
module S = FoSubst
module Utils = FoUtils

(* some pretty printers are useful now *)
open Format

let stat_fresh = mk_stat "fresh_clause"
let stat_mk_hclause = mk_stat "mk_hclause"
let stat_new_clause = mk_stat "new_clause"
let prof_check_max_lit = HExtlib.profile ~enable:true "check_max_lit"

(* ----------------------------------------------------------------------
 * literals
 * ---------------------------------------------------------------------- *)

let left_pos = 0
let right_pos = 1

let opposite_pos p = match p with
  | _ when p = left_pos -> right_pos
  | _ when p = right_pos -> left_pos
  | _ -> assert false

let eq_literal l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,ord1), Equation (l2,r2,sign2,ord2) ->
      sign1 = sign2 && T.eq_term l1 l2 && T.eq_term r1 r2 && ord1 = ord2

let eq_literal_com l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
      sign1 = sign2 &&
      ((T.eq_term l1 l2 && T.eq_term r1 r2 && o1 = o2) ||
       (T.eq_term l1 r2 && T.eq_term r1 l2 && o1 = (Utils.not_partial o2)))

let compare_literal l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
      let c = Pervasives.compare o1 o2 in
      if c <> 0 then c else
        let c = T.compare_term l1 l2 in
        if c <> 0 then c else
          let c = T.compare_term r1 r2 in
          if c <> 0 then c else
            Pervasives.compare sign1 sign2

let lit_to_multiset lit = match lit with
  | Equation (l, r, true, _) -> [l; r]
  | Equation (l, r, false, _) -> [l; l; r; r]

let compare_lits_partial ~ord l1 l2 =
  (* Utils.multiset_partial ord#compare (lit_to_multiset l1) (lit_to_multiset l2) *)
  match l1, l2 with
  | Equation (s, t, sign_st, _), Equation (u, v, sign_uv, _) ->
    let s_u = ord#compare s u
    and s_v = ord#compare s v
    and t_u = ord#compare t u
    and t_v = ord#compare t v in
    match s_u, s_v, t_u, t_v, sign_st, sign_uv with
    | Eq, _, _, Eq, _, _
    | _, Eq, Eq, _, _, _ ->
      if sign_st = sign_uv then Eq
      else if sign_st then Lt
      else (assert sign_uv; Gt)
    | Gt, Gt, _, _, _, _        (* s dominates *)
    | _, _, Gt, Gt, _, _ -> Gt  (* t dominates *)
    | Gt, Eq, _, _, false, true (* s = v & s > u *)
    | Eq, Gt, _, _, false, true (* s = u & s > v *)
    | _, _, Gt, Eq, false, true (* t = v & t > u *)
    | _, _, Eq, Gt, false, true -> Gt (* t = u & t > v *)
    | Lt, _, Lt, _, _, _        (* u dominates *)
    | _, Lt, _, Lt, _, _ -> Lt  (* v dominates *)
    | Eq, _, Lt, _, true, false (* s = u, t < u *)
    | Lt, _, Eq, _, true, false (* t = u, s < u *)
    | _, Eq, _, Lt, true, false (* s = v, t < v *)
    | _, Lt, _, Eq, true, false -> Lt (* t = v, s < v *)
    | Eq, _, _, Gt, _, _        (* s = u, t > v *)
    | Gt, _, _, Eq, _, _        (* s > u, t = v *)
    | _, Eq, Gt, _, _, _        (* s = v, t > u *)
    | _, Gt, Eq, _, _, _        (* s > v, t = u *)
      when sign_uv = sign_st -> Gt
    | Eq, _, _, Lt, _, _        (* s = u, t < v *)
    | Lt, _, _, Eq, _, _        (* s < u, t = v *)
    | _, Eq, Lt, _, _, _        (* s = v, t < u *)
    | _, Lt, Eq, _, _, _        (* s < v, t = u *)
      when sign_uv = sign_st -> Lt
    | Eq, Eq, _, _, false, true (* s = u, s = v *)
    | _, _, Eq, Eq, false, true -> Gt (* t = u, t = v *)
    | _, Eq, _, Eq, true, false (* s = v, t = v *)
    | Eq, _, Eq, _, true, false -> Lt (* s = u, t = u *)
    | _ -> Incomparable

let hash_literal lit = match lit with
  | Equation (l, r, sign, o) ->
    if sign
      then Hashtbl.hash o lxor ((Utils.murmur_hash l.hkey) lxor r.hkey)
      else Hashtbl.hash o lxor ((Utils.murmur_hash r.hkey) lxor l.hkey)

let weight_literal = function
  | Equation (l, r, _ ,_) -> l.tsize + r.tsize

let pos_lit lit = match lit with
  | Equation (_,_,sign,_) -> sign

let neg_lit lit = match lit with
  | Equation (_,_,sign,_) -> not sign

let equational_lit = function
  | Equation (l, r, _,_) -> l != T.true_term && r != T.true_term

let orientation_lit = function
  | Equation (_, _, _, ord) -> ord

let check_type a b = if a.sort <> b.sort
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
  match lit with
  | Equation (l,r,sign,_) ->
    if subst = S.id_subst
    then mk_lit ~ord l r sign
    else
      let new_l = S.apply_subst ~recursive subst l
      and new_r = S.apply_subst ~recursive subst r in
      mk_lit ~ord new_l new_r sign

let reord_lit ~ord (Equation (l,r,sign,_)) = mk_lit ~ord l r sign

let rec lit_of_fof ~ord ((Equation (l,r,sign,_)) as lit) =
  match l.term, r.term with
  (* deal with trivial literals *)
  | _ when T.eq_term l T.true_term && T.eq_term r T.false_term ->
    mk_lit ~ord T.true_term T.true_term (not sign)
  | _ when T.eq_term r T.true_term && T.eq_term l T.false_term ->
    mk_lit ~ord T.true_term T.true_term (not sign)
  | _ when T.eq_term l r ->
    mk_lit ~ord T.true_term T.true_term sign
  (* deal with false/true *)
  | _ when T.eq_term l T.false_term ->
    assert (r.sort = bool_sort);
    lit_of_fof ~ord (mk_lit ~ord r T.true_term (not sign))
  | _ when T.eq_term r T.false_term ->
    assert (l.sort = bool_sort);
    lit_of_fof ~ord (mk_lit ~ord l T.true_term (not sign))
  (* deal with negation *)
  | Node (s, [t]), _ when s = not_symbol && T.eq_term r T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord t T.true_term (not sign))
  | _, Node (s, [t]) when s = not_symbol && T.eq_term l T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord t T.true_term (not sign))
  (* deal with equality symbol *)
  | Node (s, [a; b]), _ when s = eq_symbol && T.eq_term r T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord a b sign)
  | _, Node (s, [a; b]) when s = eq_symbol && T.eq_term l T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord a b sign)
  (* default is just reordering *)
  | _ -> reord_lit ~ord lit

let term_of_lit lit =
  match lit with
  | Equation (left, right, false, _) when T.eq_term right T.true_term ->
    T.mk_not left
  | Equation (left, right, true, _) when T.eq_term right T.true_term ->
    left
  | Equation (left, right, true, _) when T.eq_term left T.true_term ->
    right
  | Equation (left, right, false, _) when T.eq_term left T.true_term ->
    T.mk_not right
  | Equation (left, right, sign, ord) ->
    if sign then T.mk_eq left right else T.mk_not (T.mk_eq left right)

let negate_lit (Equation (l,r,sign,ord)) = Equation (l,r,not sign,ord)

let fmap_lit ~ord f = function
  | Equation (left, right, sign, _) ->
    let new_left = f left
    and new_right = f right in
    Equation (new_left, new_right, sign, ord#compare new_left new_right)

let vars_of_lit = function
  | Equation (left, right, _, _) ->
    T.merge_varlist left.vars right.vars

(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)

let eq_clits lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then true else
    eq_literal lits1.(i) lits2.(i) && check (i+1)
  in
  if Array.length lits1 <> Array.length lits2
    then false
    else check 0

let eq_clause c1 c2 = eq_clits c1.clits c2.clits

let compare_clause c1 c2 = 
  let rec check i =
    if i = Array.length c1.clits then 0 else
      let cmp = compare_literal c1.clits.(i) c2.clits.(i) in
      if cmp = 0 then check (i+1) else cmp
  in
  if Array.length c1.clits <> Array.length c2.clits
    then Array.length c1.clits - Array.length c2.clits
    else check 0

let hash_lits lits =
  let rec aux h i =
    if i = Array.length lits then h else
    aux (Utils.murmur_hash (h lxor hash_literal lits.(i))) (i+1)
  in aux 113 0

let hash_clause c = hash_lits c.clits

(* ----------------------------------------------------------------------
 * hashconsing of clauses, and data structures
 * ---------------------------------------------------------------------- *)

module H = Hashcons.Make(struct
  type t = hclause
  let equal hc1 hc2 = eq_clits hc1.hclits hc2.hclits
  let hash hc = hash_lits hc.hclits
  let tag i hc = (hc.hctag <- i; hc)
end)

let stats () = H.stats ()

let eq_hclause hc1 hc2 = hc1 == hc2

let compare_hclause hc1 hc2 = hc1.hctag - hc2.hctag

let hash_hclause hc = hash_lits hc.hclits

module CHashtbl = Hashtbl.Make(
  struct
    type t = hclause
    let hash hc = hash_lits hc.hclits
    let equal = eq_hclause
  end)

module CHashSet =
  struct
    type t = unit CHashtbl.t
    let create () = CHashtbl.create 13
    let is_empty t = CHashtbl.length t = 0
    let member t hc = CHashtbl.mem t hc
    let iter t f = CHashtbl.iter (fun hc _ -> f hc) t
    let add t hc = CHashtbl.replace t hc ()
    let to_list t =
      let l = ref [] in
      iter t (fun hc -> l := hc :: !l);
      !l
  end

(* ----------------------------------------------------------------------
 * useful functions to build and examine clauses
 * ---------------------------------------------------------------------- *)

(** make a bitvector of size n with all bits set *)
let bv_make n =
  assert (n <= 31);
  let rec shift bv n = if n = 0 then bv else shift ((bv lsl 1) lor 1) (n-1)
  in shift 0 n

(** bitvector n-th element is true? *)
let bv_get bv n = (bv land (1 lsl n)) <> 0 

(** set n-th element of bitvector *)
let bv_set bv n = bv lor (1 lsl n)

(** reset n-th element of bitvector *)
let bv_clear bv n = bv land (lnot (1 lsl n))

(** is bitvector empty? *)
let bv_empty bv = bv = 0

(** Find the maximal literals among lits, returns a bitvector *)
let find_max_lits ~ord lits =
  let n = Array.length lits in
  let bv = ref (bv_make n) in
  for i = 0 to n-1 do
    (* i-th lit is already known not to be max? *)
    if not (bv_get !bv i) then () else
    for j = i+1 to n-1 do
      if not (bv_get !bv j) then () else
      match compare_lits_partial ~ord lits.(i) lits.(j) with
      | Incomparable | Eq -> ()     (* no further information about i-th and j-th *)
      | Gt -> bv := bv_clear !bv j  (* j-th cannot be max *)
      | Lt -> bv := bv_clear !bv i  (* i-th cannot be max *)
    done;
  done;
  !bv

(** comparison of variables by index *)
let compare_vars a b =
  match a.term, b.term with
  | Var i, Var j -> i - j
  | _ -> assert false

(** check whether variables are from 0 to n *)
let check_normal vars =
  let rec check prev = function
  | [] -> true
  | {term=Var n}::l -> n = prev+1 && check n l
  | _ -> assert false
  in check (-1) vars

(** compute the set of variables of literals *)
let rec merge_lit_vars acc lits i = 
  if i = Array.length lits then acc else
  match lits.(i) with
  | Equation (l, r, _, _) ->
    let acc = T.merge_varlist (T.merge_varlist acc l.vars) r.vars in
    merge_lit_vars acc lits (i+1)

(** the tautological empty clause *)
let true_clause =
  H.hashcons
    { hclits = [| Equation (T.true_term, T.true_term, true, Eq) |];
      hctag = -1; hcweight=2; hcmaxlits=lazy 0x1; hcselected=0; hcselected_done=true;
      hcvars=[]; hcproof=lazy (Proof ("trivial", []));
      hcparents=[]; hcdescendants=Ptset.empty; }

(** Build a new hclause from the given literals. If there are more than 31 literals,
    the prover becomes incomplete by returning [true] instead. *)
let mk_hclause_a ~ord lits proof parents =
  incr_stat stat_mk_hclause;
  if Array.length lits > 31
  then (Utils.debug 1 (lazy (Utils.sprintf "%% incompleteness: clause of %d lits -> $true"
      (Array.length lits))); true_clause)
  else begin
  let all_vars = merge_lit_vars [] lits 0 in
  let all_vars = List.sort compare_vars all_vars in
  (* normalize literals *)
  let all_vars =
    let subst = if check_normal all_vars then S.id_subst else S.relocate 0 all_vars in
    Array.iteri
      (fun i lit -> lits.(i) <- apply_subst_lit ~ord ~recursive:false subst lit)
      lits;
    List.map (S.apply_subst ~recursive:false subst) all_vars
  in
  (* create the structure *)
  let hc = {
    hclits = lits;
    hctag = -1;
    hcweight = 0;
    hcmaxlits = lazy (find_max_lits ~ord lits);
    hcselected_done = false;
    hcselected = 0;
    hcvars = [];
    hcproof = proof;
    hcparents = parents;
    hcdescendants = Ptset.empty;
  } in
  (* hashcons the clause, compute additional data if fresh *)
  let hc' = H.hashcons hc in
  (if hc == hc' then begin
    incr_stat stat_new_clause;
    hc.hcvars <- all_vars;
    hc.hcweight <- Array.fold_left (fun acc lit -> acc + weight_literal lit) 0 lits;
    (* update the parent clauses' sets of descendants *)
    List.iter (fun parent -> parent.hcdescendants <- Ptset.add hc.hctag parent.hcdescendants) parents;
    end);
  (* return hashconsed clause *)
  hc'
  end

(** build clause from a list *)
let mk_hclause ~ord lits proof parents =
  mk_hclause_a ~ord (Array.of_list lits) proof parents

(** simplify literals *)
let clause_of_fof ~ord hc =
  let lits = Array.map (lit_of_fof ~ord) hc.hclits in
  mk_hclause_a ~ord lits hc.hcproof hc.hcparents

(** change the ordering of literals *)
let reord_hclause ~ord hc =
  let lits = Array.map (reord_lit ~ord) hc.hclits in
  mk_hclause_a ~ord lits hc.hcproof hc.hcparents

(** check the ordering relation of lits (always true after reord_clause ~ord) *)
let check_ord_hclause ~ord hc =
  assert (
  Utils.array_forall
    (function (Equation (l,r,_,o)) ->
      let ok = o = ord#compare l r in
      (if not ok then Format.printf "@[<h>Ord problem: literal %a %a" !T.pp_term#pp l !T.pp_term#pp r);
      ok)
    hc.hclits)

(** Compute selected literals. Note that selection on a unit clause is a no-op. *)
let select_clause ~select hc =
  (match hc.hclits with
   | _ when hc.hcselected_done -> ()  (* already selected *)
   | [|_|] -> hc.hcselected_done <- true  (* selection useless in unit clauses *)
   | _ ->
   begin
      let bv = List.fold_left bv_set 0 (select hc) in
      (hc.hcselected <- bv;
       hc.hcselected_done <- true);
    end);
  hc

(** are there selected literals in the clause? *)
let has_selected_lits hc = not (bv_empty hc.hcselected)

(** descendants of the clause *)
let descendants hc = hc.hcdescendants

(** Check whether the literal is maximal *)
let is_maxlit hc i =
  let bv = Lazy.force hc.hcmaxlits in
  bv_get bv i  (* just check i-th bit *)

(** Check whether the literal is maximal after applying subst *)
let check_maximal_lit_ ~ord c pos subst =
  let bv = Lazy.force c.cref.hcmaxlits in
  let n = Array.length c.clits in
  (* check if lit already not maximal, before subst *)
  if not (bv_get bv pos) then false else 
  let lit = c.clits.(pos) in
  let slit = apply_subst_lit ~ord subst lit in
  (* check that slit is not < subst(lit') for any lit' maximal in c *)
  let rec check i = 
    if i = n then true
    else if not (bv_get bv i) then check (i+1)
    else
      let slit' = apply_subst_lit ~ord subst c.clits.(i) in
      (compare_lits_partial ~ord slit slit' <> Lt) && check (i+1)
  in
  check 0

let check_maximal_lit ~ord clause pos subst =
  prof_check_max_lit.HExtlib.profile (check_maximal_lit_ ~ord clause pos) subst

(** Get an indexed list of maximum literals *)
let maxlits c =
  let bv = Lazy.force c.cref.hcmaxlits in
  Utils.array_foldi
    (fun acc i lit -> if bv_get bv i then (lit, i)::acc else acc)
    [] c.clits

(** Apply substitution to the clause *)
let rec apply_subst_cl ?(recursive=true) ~ord subst hc =
  if S.is_empty subst then hc
  else
    let lits = Array.map (apply_subst_lit ~recursive ~ord subst) hc.hclits in
    mk_hclause_a ~ord lits hc.hcproof hc.hcparents

(** get literal by index *)
let get_lit c i = c.clits.(i)

(** get term by position *)
let get_pos c pos =
  match pos with
  | idx::side::tpos ->
    let lit = c.clits.(idx) in
    (match lit with
    | Equation (l, _, _, _) when side = left_pos -> T.at_pos l tpos
    | Equation (_, r, _, _) when side = right_pos -> T.at_pos r tpos
    | _ -> invalid_arg "wrong side in literal")
  | _ -> invalid_arg "wrong position for clause"

(** Get a variant of the clause, in which variables are all > offset *)
let fresh_clause ~ord offset hc =
  incr_stat stat_fresh;
  let subst = S.relocate (offset + 1) hc.hcvars in
  let clits = Array.map (apply_subst_lit ~recursive:false ~ord subst) hc.hclits in
  let cvars = lazy (List.map (S.apply_subst ~recursive:false subst) hc.hcvars) in
  { cref=hc; clits; cvars; }

(** create a clause from a hclause, without renaming *)
let base_clause hc =
  incr_stat stat_fresh;
  { clits = hc.hclits; cref = hc; cvars = Lazy.lazy_from_val hc.hcvars}

(** Check whether the literal is selected *)
let is_selected hc i =
  let bv = hc.hcselected in
  bv_get bv i

(** Indexed list of selected literals *)
let selected_lits c =
  let bv = c.cref.hcselected in
  Utils.array_foldi
    (fun acc i lit -> if bv_get bv i then (lit,i)::acc else acc)
    [] c.clits

(** check whether a literal is eligible for resolution *)
let eligible_res ~ord c idx subst =
  (* bitvectors of selected and maximal literals *)
  let selected = c.cref.hcselected in
  let maximal = Lazy.force c.cref.hcmaxlits in
  let lits = c.clits in
  let n = Array.length lits in
  (* check maximality among selected literals with given sign *)
  let rec check_among_selected slit i sign =
    if i = n then true
    else if i <> idx && bv_get maximal i && bv_get selected i && pos_lit lits.(i) = pos_lit slit
      then let slit' = apply_subst_lit ~ord subst lits.(i) in
      compare_lits_partial ~ord slit slit' <> Lt && check_among_selected slit (i+1) sign
      else check_among_selected slit (i+1) sign
  in
  (* if no lit is selected, max lits are eligible *)
  if not (has_selected_lits c.cref)
    then check_maximal_lit ~ord c idx subst (* just check maximality *)
    else if not (is_selected c.cref idx) then false (* some are selected, not lit *)
    else (* check that slit max among selected literals of same sign *)
      let slit = apply_subst_lit ~ord subst c.clits.(idx) in
      let sign = pos_lit slit in
      check_among_selected slit 0 sign

(** check whether a literal is eligible for paramodulation *)
let eligible_param ~ord c idx subst =
  if has_selected_lits c.cref then false
  else if neg_lit c.clits.(idx) then false (* only positive lits *)
  else check_maximal_lit ~ord c idx subst

let is_unit_clause hc = match hc.hclits with
  | [|_|] -> true
  | _ -> false

let rec from_simple ~ord (f,source) =
  let rec convert f = match f with
  | Simple.Not (Simple.Atom f) -> [mk_neq ~ord (T.from_simple f) T.true_term]
  | Simple.Atom f -> [mk_eq ~ord (T.from_simple f) T.true_term]
  | Simple.Not (Simple.Eq (t1,t2)) -> [mk_neq ~ord (T.from_simple t1) (T.from_simple t2)]
  | Simple.Eq (t1, t2) -> [mk_eq ~ord (T.from_simple t1) (T.from_simple t2)]
  | Simple.Not (Simple.Equiv (f1,f2)) -> [mk_neq ~ord (T.from_simple_formula f1) (T.from_simple_formula f2)]
  | Simple.Equiv (f1, f2) -> [mk_eq ~ord (T.from_simple_formula f1) (T.from_simple_formula f2)]
  | Simple.Or l -> List.concat (List.map convert l)
  | _ -> [mk_eq ~ord (T.from_simple_formula f) T.true_term]
  in
  let proof = match source with
  | Simple.Axiom (a,b) -> Lazy.lazy_from_val (Axiom (a,b))
  | Simple.Derived (name, fs) -> failwith "unable to convert non-axiom simple clause to hclause"
  in
  mk_hclause ~ord (convert f) proof []

let to_simple hc = failwith "not implemented"

(* ----------------------------------------------------------------------
 * set of hashconsed clauses
 * ---------------------------------------------------------------------- *)

module CSet =
  struct

    (** Set of hashconsed clauses. 'a is the fantom type for hclauses.
        It also contains a payload that is updated on every addition/
        removal of clauses. The additional payload is also updated upon
        addition/deletion. *)
    type t = {
      maxvar : int;                 (** index of maximum variable *)
      clauses : hclause Ptmap.t;    (** clause ID -> clause *)
    }

    let empty = { maxvar=0; clauses = Ptmap.empty; }

    let is_empty set = Ptmap.is_empty set.clauses

    let size set = Ptmap.fold (fun _ _ b -> b + 1) set.clauses 0

    let add set hc =
      let maxvar = max (T.max_var hc.hcvars) set.maxvar in
      { maxvar; clauses = Ptmap.add hc.hctag hc set.clauses; }

    let add_list set hcs =
      let maxvar, clauses =
        List.fold_left
          (fun (m,c) hc -> max m (T.max_var hc.hcvars), Ptmap.add hc.hctag hc c)
          (set.maxvar, set.clauses) hcs in
      {maxvar; clauses;}

    let add_clause set c = add set c.cref

    let union s1 s2 =
      (* merge small into big *)
      let merge_into small big =
        let maxvar, clauses =
          Ptmap.fold
            (fun _ hc (m,c) -> max m (T.max_var hc.hcvars), Ptmap.add hc.hctag hc c)
            small.clauses (big.maxvar, big.clauses) in
        {maxvar;clauses;}
      in
      if size s1 < size s2 then merge_into s1 s2 else merge_into s2 s1

    let remove_id set i =
      { set with clauses = Ptmap.remove i set.clauses }

    let remove set hc = remove_id set hc.hctag

    let remove_list set hcs =
      let clauses =
        List.fold_left
          (fun c hc -> Ptmap.remove hc.hctag c)
          set.clauses hcs in
      {set with clauses;}

    let remove_ids set ids =
      let clauses =
        Ptset.fold
          (fun i set -> Ptmap.remove i set)
          ids set.clauses in
      {set with clauses;}

    let get set i = Ptmap.find i set.clauses

    let mem set hc = Ptmap.mem hc.hctag set.clauses

    let mem_id set i = Ptmap.mem i set.clauses

    let iter set k = Ptmap.iter (fun _ hc -> k hc) set.clauses

    let iteri set k = Ptmap.iter k set.clauses

    let fold f acc set =
      let acc = ref acc in
      iteri set (fun i hc -> acc := f !acc i hc);
      !acc

    let partition set pred =
      Ptmap.fold
        (fun _ hc (yes,no) ->
          if pred hc then add yes hc, no else yes, add no hc)
        set.clauses (empty, empty)

    let to_list set =
      Ptmap.fold (fun _ hc acc -> hc :: acc) set.clauses []

    let of_list l =
      add_list empty l
  end

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

let string_of_pos s = match s with
  | _ when s == left_pos -> "left"
  | _ when s == right_pos -> "right"
  | _ -> assert false

let string_of_comparison = function
  | Lt -> "=<="
  | Gt -> "=>="
  | Eq -> "==="
  | Incomparable -> "=?="

(** pretty printer for literals *)
class type pprinter_literal =
  object
    method pp : Format.formatter -> literal -> unit     (** print literal *)
  end

let pp_literal_gen pp_term formatter lit =
  match lit with
  | Equation (l, r, sign, _) when T.eq_term r T.true_term ->
    if sign
      then pp_term#pp formatter l
      else Format.fprintf formatter "¬%a" pp_term#pp l
  | Equation (l, r, sign, _) when T.eq_term l T.true_term ->
    if sign
      then pp_term#pp formatter r
      else Format.fprintf formatter "¬%a" pp_term#pp r
  | Equation (l, r, sign, _) when l.sort == bool_sort ->
    if sign
      then Format.fprintf formatter "%a <=> %a" pp_term#pp l pp_term#pp r
      else Format.fprintf formatter "%a <~> %a" pp_term#pp l pp_term#pp r
  | Equation (l, r, sign, _) ->
    if sign
      then Format.fprintf formatter "%a = %a" pp_term#pp l pp_term#pp r
      else Format.fprintf formatter "%a != %a" pp_term#pp l pp_term#pp r

let pp_literal_debug =
  let print_ord = ref false in
  object
    method pp formatter ((Equation (_,_,_,ord)) as lit) =
      pp_literal_gen T.pp_term_debug formatter lit;
      if !print_ord
        then Format.fprintf formatter "(%s)" (string_of_comparison ord)
        else ()

    method ord b = print_ord := b
  end

let pp_literal_tstp =
  object
    method pp formatter lit = pp_literal_gen T.pp_term_tstp formatter lit
  end

let pp_literal =
  object
    method pp formatter lit = pp_literal_gen !T.pp_term formatter lit
  end

let pp_pos formatter pos =
  if pos = []
    then Format.pp_print_string formatter "ε"
    else fprintf formatter "@[<h>%a@]" (Utils.pp_list ~sep:"." pp_print_int) pos

(** pretty printer for clauses *)
class type pprinter_clause =
  object
    method pp_lits : Format.formatter -> literal array -> hclause -> unit
    method pp : Format.formatter -> clause -> unit      (** print clause *)
    method pp_h : Format.formatter -> hclause -> unit   (** print hclause *)
    method pp_pos : Format.formatter -> (clause * position) -> unit
    method pp_h_pos : Format.formatter -> (hclause * position * term) -> unit
    method pp_pos_subst : Format.formatter -> (clause * position * substitution) -> unit
    method horizontal : bool -> unit                    (** print in horizontal box? *)
  end

(** factor some code for classes *)
class virtual common_pp_clause =
  object (self)
    method virtual pp_lits : Format.formatter -> literal array -> hclause -> unit
    method pp formatter c = self#pp_lits formatter c.clits c.cref
    method pp_h formatter hc = self#pp_lits formatter hc.hclits hc
    method pp_pos formatter (c, pos) =
      Format.fprintf formatter "@[<h>[%a at %a]@]" self#pp c pp_pos pos
    method pp_h_pos formatter (hc, pos, t) =
      Format.fprintf formatter "@[<h>[%a at %a with %a]@]"
        self#pp_h hc pp_pos pos !T.pp_term#pp t
    method pp_pos_subst formatter (c, pos, subst) =
      Format.fprintf formatter "@[<h>[%a at %a with %a]@]"
        self#pp c pp_pos pos S.pp_substitution subst
  end

let pp_clause_debug =
  let _horizontal = ref true in
  let pp_annot hc i =
    ""^(if is_selected hc i then "+" else "")
      ^(if is_maxlit hc i then "*" else "") in
  object (self)
    (* print literals with a '*' for maximal, and '+' for selected *)
    method pp_lits formatter lits hc =
      (* how to print the list of literals *)
      let lits_printer formatter lits =
        Utils.pp_arrayi ~sep:" | "
          (fun formatter i lit ->
            let annot = pp_annot hc i in
            fprintf formatter "%a%s" pp_literal_debug#pp lit annot)
          formatter lits
      in
      (* print in an horizontal box, or not *)
      if !_horizontal
        then fprintf formatter "@[<h>[%a]@]" lits_printer lits
        else fprintf formatter "[%a]" lits_printer lits
    (* regular printing is printing with no literal selected *)
    inherit common_pp_clause
    method horizontal s = _horizontal := s
  end

let pp_clause_tstp =
  let _horizontal = ref true in
  object (self)
    method pp_lits formatter lits hc =
      (* how to print the list of literals *)
      let lits_printer formatter lits =
        (* convert into a big term *)
        let t =
          match lits with
          | [||] -> T.false_term
          | _ -> Array.fold_left
            (fun t lit -> T.mk_or t (term_of_lit lit))
            (term_of_lit lits.(0)) (Array.sub lits 1 (Array.length lits - 1))
        in
        (* quantify all free variables *)
        let vars = t.vars in
        let t = List.fold_left
          (fun t var -> T.mk_node forall_symbol bool_sort [var; t])
          t vars
        in
        T.pp_term_tstp#pp formatter t
      in
      (* print in an horizontal box, or not *)
      if !_horizontal
        then fprintf formatter "@[<h>%a@]" lits_printer lits
        else fprintf formatter "%a" lits_printer lits
    inherit common_pp_clause
    method horizontal s = _horizontal := s
  end

let pp_clause = ref pp_clause_debug

let pp_lits formatter lits = 
  Utils.pp_arrayi ~sep:" | "
    (fun formatter i lit -> fprintf formatter "%a" pp_literal_debug#pp lit)
    formatter lits

(** pretty printer for proofs *)
class type pprinter_proof =
  object
    method pp : Format.formatter -> hclause -> unit      (** pretty print proof from clause *)
  end

let pp_proof_debug =
  object (self)
    method pp formatter hc =
      assert (hc.hclits = [||]);
      (* already_printed is a set of clauses already printed. *)
      let already_printed = ref Ptset.empty
      and to_print = Queue.create () in
      (* initialize queue *)
      Queue.add hc to_print; 
      (* print every clause in the queue, if not already printed *)
      while not (Queue.is_empty to_print) do
        let hc = Queue.take to_print in
        if Ptset.mem hc.hctag !already_printed then ()
        else begin
          already_printed := Ptset.add hc.hctag !already_printed;
          match Lazy.force hc.hcproof with
          | Axiom (f, s) ->
              fprintf formatter "@[<hov 4>@[<h>%a@]@ <--- @[<h>axiom %s in %s@]@]@;"
                !pp_clause#pp_h hc s f
          | Proof (rule, premises) ->
              (* print the proof step *)
              fprintf formatter "@[<hov 4>@[<h>%a@]@ <--- @[<h>%s with @[<hv>%a@]@]@]@;"
                !pp_clause#pp_h hc rule
                (Utils.pp_list ~sep:", " !pp_clause#pp_pos_subst) premises;
              (* print premises recursively *)
              List.iter (fun (c, _, _) -> Queue.add c.cref to_print) premises
        end
      done
  end

let pp_proof_tstp =
  object (self)
    method pp formatter hc =
      assert (hc.hclits = [||]);
      (* already_printed is a set of clauses already printed. *)
      let already_printed = ref Ptset.empty
      and clause_num = ref Ptmap.empty
      and counter = ref 1
      and to_print = Queue.create () in
      (* c -> hashconsed c, unique number for c *)
      let get_num hc = 
        try hc, Ptmap.find hc.hctag !clause_num
        with Not_found ->
          clause_num := Ptmap.add hc.hctag !counter !clause_num;
          incr counter;
          hc, Ptmap.find hc.hctag !clause_num
      in
      (* initialize queue *)
      let hc, num = get_num hc in
      Queue.add (hc, num) to_print; 
      (* print every clause in the queue, if not already printed *)
      while not (Queue.is_empty to_print) do
        let hc, num = Queue.take to_print in
        if Ptset.mem hc.hctag !already_printed then ()
        else begin
          already_printed := Ptset.add hc.hctag !already_printed;
          match Lazy.force hc.hcproof with
          | Axiom (f, ax_name) ->
            fprintf formatter "@[<h>fof(%d, axiom, %a,@ @[<h>file('%s', %s)@]).@]@;"
              num pp_clause_tstp#pp_h hc f ax_name
          | Proof (name, premises) ->
            let premises = List.map (fun (c,_,_) -> get_num c.cref) premises in
            let status = if name = "elim" || name = "to_cnf" then "esa" else "thm" in
            (* print the inference *)
            fprintf formatter ("@[<h>fof(%d, plain, %a,@ " ^^
                               "@[<h>inference('%s', [status(%s)], @[<h>[%a, theory(equality)]@])@]).@]@;")
              num pp_clause_tstp#pp_h hc name status
              (Utils.pp_list ~sep:"," pp_print_int) (List.map snd premises);
            (* print every premise *)
            List.iter (fun (hc,num) -> Queue.add (hc, num) to_print) premises
        end
      done;
  end

let pp_proof = ref pp_proof_debug

(** print the content of a clause set *)
let pp_set formatter set =
  let clauses = CSet.to_list set in
  (* print as a list of clauses *)
  fprintf formatter "@[<v>%a@]" (Utils.pp_list ~sep:"" !pp_clause#pp_h) clauses
