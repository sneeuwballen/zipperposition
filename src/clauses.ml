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

let prof_check_max_lit = HExtlib.profile ~enable:true "check_max_lit"

(* ----------------------------------------------------------------------
 * equations
 * ---------------------------------------------------------------------- *)

let left_pos = 1

let right_pos = 2

let opposite_pos p = match p with
  | _ when p = left_pos -> right_pos
  | _ when p = right_pos -> left_pos
  | _ -> assert false

let eq_eqn l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
    T.eq_term l1 l2 && T.eq_term r1 r2 && sign1 = sign2

(* by construction, equations are sorted by term hash *)
let eq_eqn_com l1 l2 = eq_eqn l1 l2

let compare_eqn l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1), Equation (l2,r2,sign2) ->
    let c = T.compare_term l1 l2 in
    if c <> 0 then c else
      let c = T.compare_term r1 r2 in
      if c <> 0 then c else
        Pervasives.compare sign1 sign2

let eqn_to_multiset = function
  | Equation (l, r, true) -> [l; r]
  | Equation (l, r, false) -> [l; l; r; r]

let compare_eqn_partial ~ord l1 l2 =
  (* Utils.multiset_partial ord#compare (lit_to_multiset l1) (lit_to_multiset l2) *)
  match l1, l2 with
  | Equation (s, t, sign_st), Equation (u, v, sign_uv) ->
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

let hash_eqn = function
  | Equation (l, r, sign) ->
    if sign
      then (Utils.murmur_hash l.hkey) lxor (33 + r.hkey)
      else (Utils.murmur_hash r.hkey) lxor (42 + l.hkey)

let pos_eqn = function
  | Equation (_,_,sign) -> sign

let neg_eqn = function
  | Equation (_,_,sign) -> not sign

let check_type a b = if a.sort <> b.sort
  then raise (SortError "sides of equations of different sorts") else ()

let mk_eqn a b sign =
  check_type a b;
  if a.tag < b.tag
    then Equation (a, b, sign)
    else Equation (b, a, sign)

let mk_eq a b = mk_eqn a b true

let mk_neq a b = mk_eqn a b false

let apply_subst_eqn ?(recursive=true) subst eqn =
  if subst = S.id_subst then eqn
  else match eqn with
  | Equation (l,r,sign) ->
    let new_l = S.apply_subst ~recursive subst l
    and new_r = S.apply_subst ~recursive subst r
    in
    Utils.debug 4 (lazy (Utils.sprintf "apply %a to %a gives %a"
        S.pp_substitution subst !T.pp_term#pp l !T.pp_term#pp new_l));
    Utils.debug 4 (lazy (Utils.sprintf "apply %a to %a gives %a"
        S.pp_substitution subst !T.pp_term#pp r !T.pp_term#pp new_r));
    mk_eqn new_l new_r sign

let rec eqn_of_fof ((Equation (l,r,sign)) as eqn) =
  match l.term, r.term with
  (* deal with trivial literals *)
  | _ when T.eq_term l T.true_term && T.eq_term r T.false_term ->
    mk_eqn T.true_term T.true_term (not sign)
  | _ when T.eq_term r T.true_term && T.eq_term l T.false_term ->
    mk_eqn T.true_term T.true_term (not sign)
  | _ when T.eq_term l r ->
    mk_eqn T.true_term T.true_term sign
  (* deal with false/true *)
  | _ when T.eq_term l T.false_term ->
    assert (r.sort = bool_sort);
    eqn_of_fof (mk_eqn r T.true_term (not sign))
  | _ when T.eq_term r T.false_term ->
    assert (l.sort = bool_sort);
    eqn_of_fof (mk_eqn l T.true_term (not sign))
  (* deal with negation *)
  | Node (s, [t]), _ when s = not_symbol && T.eq_term r T.true_term ->
    eqn_of_fof (mk_eqn t T.true_term (not sign))
  | _, Node (s, [t]) when s = not_symbol && T.eq_term l T.true_term ->
    eqn_of_fof (mk_eqn t T.true_term (not sign))
  (* deal with equality symbol *)
  | Node (s, [a; b]), _ when s = eq_symbol && T.eq_term r T.true_term ->
    eqn_of_fof (mk_eqn a b sign)
  | _, Node (s, [a; b]) when s = eq_symbol && T.eq_term l T.true_term ->
    eqn_of_fof (mk_eqn a b sign)
  (* default is just reordering *)
  | _ -> eqn

let term_of_eqn = function
  | Equation (left, right, false) when T.eq_term right T.true_term ->
    T.mk_not left
  | Equation (left, right, true) when T.eq_term right T.true_term ->
    left
  | Equation (left, right, true) when T.eq_term left T.true_term ->
    right
  | Equation (left, right, false) when T.eq_term left T.true_term ->
    T.mk_not right
  | Equation (left, right, sign) ->
    if sign then T.mk_eq left right else T.mk_not (T.mk_eq left right)

let negate_eqn (Equation (l,r,sign)) = Equation (l,r,not sign)

let fmap_eqn f = function
  | Equation (left, right, sign) ->
    let new_left = f left
    and new_right = f right in
    Equation (new_left, new_right, sign)

let vars_of_eqn = function
  | Equation (left, right, _) ->
    let v = Vector.from_array left.vars in
    Vector.append_array v right.vars;
    Vector.uniq_sort ~cmp:T.compare_term v

let eqn_depth (Equation (l, r, _)) = max (T.depth l) (T.depth r)

(* ----------------------------------------------------------------------
 * literals
 * ---------------------------------------------------------------------- *)

(** compare literals, including the selected status *)
let compare_lit l1 l2 =
  let cmp = compare_eqn l1.lit_eqn l2.lit_eqn in
  if cmp <> 0
    then cmp
    else if l1.lit_selected && not l2.lit_selected then 1
    else if l2.lit_selected && not l1.lit_selected then -1
    else 0

let eq_lit l1 l2 = compare_lit l1 l2 = 0

let mk_lit eqn = {
  lit_eqn = eqn;
  lit_selected = false;
  lit_maximal = false;
  lit_hash = hash_eqn eqn;
  lit_depth = eqn_depth eqn;
}

let copy_lit lit = {lit with lit_selected=false; lit_maximal=false}

(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)

(* the comparison is sensitive to the selected literals *)
let compare_clause c1 c2 =
  if Array.length c1.clits < Array.length c2.clits then -1
  else if Array.length c1.clits > Array.length c2.clits then 1
  else (* lexicographic comparison *)
    let ans = ref 0 in
    try
      for i = 0 to Array.length c1.clits do
        let cmp = compare_lit c1.clits.(i) c2.clits.(i) in
        (* this literal differs from c1 to c2 *)
        if cmp <> 0 then (ans := cmp; raise Exit)
      done;
      0
    with Exit -> !ans

let eq_clause c1 c2 = compare_clause c1 c2 = 0

let hash_clause c = c.chkey

module HashedClause =
  struct
    type t = clause
    let equal c1 c2 = eq_clause c1 c2
    let hash c = hash_clause c
    let tag i c = (c.ctag <- i; c)
  end

module H = Hashcons.Make(HashedClause)

let hashcons_clause c = H.hashcons c

let stats () = H.stats ()

let eq_hclause hc1 hc2 = hc1 == hc2

let compare_hclause hc1 hc2 = hc1.ctag - hc2.ctag

module CHashtbl = Hashtbl.Make(HashedClause)

module CHashSet =
  struct
    type t = unit CHashtbl.t
    let create () = CHashtbl.create 13
    let is_empty t = CHashtbl.length t = 0
    let member t c = CHashtbl.mem t c
    let iter t f = CHashtbl.iter (fun c _ -> f c) t
    let add t c = CHashtbl.replace t c ()
    let to_list t =
      let l = ref [] in
      iter t (fun c -> l := c :: !l);
      !l
  end

(** container used to store the state necessary to build clauses *)
type clause_state =
  < ord : ordering;
    select : selection_fun >

let mk_state ?(select=no_select) ~ord =
  object
    method ord = ord
    method select = select
  end

(** put the flag 'maximal' on literal of the array
    that are maximal *)
let compute_maxlits ~ord lits =
  for i = 0 to Array.length lits - 1 do
    let lit = lits.(i) in
    let rec check_max j =
      if j >= Array.length lits then true
      else 
        let lit' = lits.(j) in
        match () with
        | _ when j < i ->
          (* compare with max literals only *)
          (not lit'.lit_maximal
            || compare_eqn_partial ~ord lit.lit_eqn lit'.lit_eqn <> Lt)
          && check_max (j+1)
        | _ when j > i ->
          (* compare in any case *)
          compare_eqn_partial ~ord lit.lit_eqn lit'.lit_eqn <> Lt && check_max (j+1)
        | _ -> check_max (j+1)
    in lit.lit_maximal <- check_max 0
  done

(** compute which literals are selected *)
let compute_selected ~select lits = select lits

(** compute hash from the list of literals *)
let compute_hash_clause lits =
  let h = ref 113 in
  for i = 0 to Array.length lits - 1 do
    h := Utils.murmur_hash (!h lxor hash_eqn lits.(i).lit_eqn);
  done;
  !h

let mk_clause ~cs eqns cproof cparents =
  (* merge sets of variables *)
  let v = Vector.create 10 in
  for i = 0 to Array.length eqns - 1 do
    Vector.append v (vars_of_eqn eqns.(i));
  done;
  let v' = Vector.uniq_sort ~cmp:T.compare_term v in
  let cvars = Vector.to_array v' in
  (* sort literals by hash, after copying them *)
  let clits = Array.map mk_lit eqns in
  Array.sort (fun lit1 lit2 -> lit1.lit_hash - lit2.lit_hash) clits;
  let chkey = compute_hash_clause clits in
  (* select literals and find the maximal ones. Also count selected lits *)
  compute_maxlits ~ord:cs#ord clits;
  compute_selected ~select:cs#select clits;
  let cselected = Array.fold_left
    (fun cnt lit -> if lit.lit_selected then cnt+1 else cnt) 0 clits in
  {clits; cvars; cproof; cselected; cparents; ctag= -1; chkey}

let mk_clause_vec ~cs eqns cproof cparents =
  mk_clause ~cs (Vector.to_array eqns) cproof cparents

let parents clause = clause.cparents

let clause_of_fof ~cs c =
  let eqns = Array.map (fun lit -> eqn_of_fof lit.lit_eqn) c.clits in
  let same =
    try Array.iteri
      (fun i lit -> if not (eq_eqn lit.lit_eqn eqns.(i)) then raise Exit) c.clits;
      true
    with Exit -> false
  in
  (* rebuild a clause only if some equation was changed *)
  if same then c else mk_clause ~cs eqns c.cproof c.cparents

let rec apply_subst_cl ?(recursive=true) ~cs subst c =
  if subst = S.id_subst then c
  else
    (* recompute lits *)
    let eqns = Array.map
      (fun lit -> apply_subst_eqn ~recursive subst lit.lit_eqn)
      c.clits in
    (* do not update parents nor proof *)
    mk_clause ~cs eqns c.cproof c.cparents

(** check that the literal is maximal in the clause, after
    substitution. We only need to compare it to instantiated
    maximal literals, by stability under substitution of > *)
let check_maximal_lit ~ord clause pos subst =
  let lits = clause.clits in
  let eqn = apply_subst_eqn subst lits.(pos).lit_eqn in
  (* iterate through literals *)
  let rec check i =
    if i >= Array.length lits
      then true
      else
        let lit' = lits.(i) in
        match () with
        | _ when i = pos -> check (i+1)
        | _ when not lit'.lit_selected -> check (i+1)  (* lit is not maximal *)
        | _ -> (* compare with substituted lit *)
          let eqn' = apply_subst_eqn subst lit'.lit_eqn in
          compare_eqn_partial ~ord eqn eqn' <> Lt && check (i+1)
  in
  prof_check_max_lit.HExtlib.profile check 0

let get_lit clause idx = clause.clits.(idx)

let get_pos clause pos =
  match pos with
  | idx::side::tpos ->
    let lit = get_lit clause idx in
    (* find the subterm in the good side of the literal *)
    (match lit.lit_eqn with
    | Equation (l, _, _) when side = left_pos -> T.at_pos l tpos
    | Equation (_, r, _) when side = right_pos -> T.at_pos r tpos
    | _ -> invalid_arg "wrong side in literal")
  | _ -> invalid_arg "wrong position for clause"

let iter_maxlits c k =
  for i = 0 to Array.length c.clits - 1 do
    let lit = c.clits.(i) in
    if lit.lit_maximal then k i lit
  done

let iter_selected c k =
  for i = 0 to Array.length c.clits - 1 do
    let lit = c.clits.(i) in
    if lit.lit_selected then k i lit
  done

let fold_lits ~pos ~neg ~selected ~max c acc k =
  let acc = ref acc in
  for i = 0 to Array.length c.clits - 1 do
    let lit = c.clits.(i) in
    if (   (not pos || pos_eqn lit.lit_eqn)
        && (not neg || neg_eqn lit.lit_eqn)
        && (not selected || lit.lit_selected)
        && (not max || lit.lit_maximal))
      then acc := k !acc i lit
  done;
  !acc

let fresh_clause ~cs maxvar c =
  let subst = S.relocate_array maxvar c.cvars in
  apply_subst_cl ~recursive:false ~cs subst c

let normalize_clause ~cs c = fresh_clause ~cs 0 c

(** check whether a literal is selected *)
let selected_lit c idx = c.clits.(idx).lit_selected

(** get the list of selected literals, with their index *)
let selected_lits c =
  let l = ref [] in
  for i = 0 to Array.length c.clits - 1 do
    let lit = c.clits.(i) in
    if lit.lit_selected then l := (lit, i) :: !l;
  done;
  !l

(** check whether a literal is eligible for resolution *)
let eligible_res ~cs c idx subst =
  let lit = c.clits.(idx) in
  if c.cselected = 0
    then (* if no lit is selected, max lits are eligible *)
      check_maximal_lit ~ord:cs#ord c idx subst
    else if not c.clits.(idx).lit_selected
      then false (* literal not selected but others are *)
    else
      let eqn = c.clits.(idx).lit_eqn in
      let eqn = apply_subst_eqn subst eqn in
      (* check maximality among selected literals of same sign *)
      let rec check_selected i =
        let lit' = c.clits.(i) in
        if i >= Array.length c.clits
          then true
        else if i = idx || not lit'.lit_selected || pos_eqn lit.lit_eqn <> pos_eqn lit'.lit_eqn
          then check_selected (i+1)  (* no need to compare *)
          else
            let eqn' = apply_subst_eqn subst c.clits.(i).lit_eqn in
            if compare_eqn_partial ~ord:cs#ord eqn eqn' = Lt
              then false  (* some other selected lit *)
              else check_selected (i+1)
      in
      check_selected 0

(** check whether a literal is eligible for paramodulation *)
let eligible_param ~cs c idx subst =
  if c.cselected > 0 || neg_eqn c.clits.(idx).lit_eqn
    then false  (* negative lit, or there are some selected lits *)
    else check_maximal_lit ~ord:cs#ord c idx subst

(* ----------------------------------------------------------------------
 * bag of clauses
 * ---------------------------------------------------------------------- *)

module M = Ptmap

type bag = {
  bag_maxvar : int;           (* index of maximum variable *)
  bag_clauses : hclause M.t;  (* clause ID -> clause *)
}

let add_hc_to_bag {bag_maxvar=maxvar_b; bag_clauses=clauses_b} hc =
  let maxvar_hc = T.max_var hc.cvars in
  {bag_maxvar=(max maxvar_hc maxvar_b);
   bag_clauses=M.add hc.ctag hc clauses_b}

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

let iter_bag bag f = M.iter f bag.bag_clauses

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
  M.iter (fun _ _ -> incr count) bag.bag_clauses;
  !count

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
  match lit.lit_eqn with
  | Equation (l, r, sign) when T.eq_term r T.true_term ->
    if sign
      then pp_term#pp formatter l
      else Format.fprintf formatter "¬%a" pp_term#pp l
  | Equation (l, r, sign) when T.eq_term l T.true_term ->
    if sign
      then pp_term#pp formatter r
      else Format.fprintf formatter "¬%a" pp_term#pp r
  | Equation (l, r, sign) ->
    if sign
      then Format.fprintf formatter "%a = %a" pp_term#pp l pp_term#pp r
      else Format.fprintf formatter "%a != %a" pp_term#pp l pp_term#pp r

let pp_literal_debug =
  object
    method pp formatter lit = pp_literal_gen T.pp_term_debug formatter lit
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
    method virtual pp : Format.formatter -> clause -> unit
    method pp_h formatter hc = self#pp formatter hc
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
  let pp_annot c idx lit =
    ""^(if lit.lit_selected then "+" else "")
      ^(if lit.lit_maximal then "*" else "") in
  object (self)
    (* print literals with a '*' for maximal, and '+' for selected *)
    method pp formatter c =
      (* how to print the list of literals *)
      let lits_printer formatter lits =
        Utils.pp_array ~sep:" | "
          (fun formatter idx lit ->
            let annot = pp_annot c idx lit in
            fprintf formatter "%a%s" pp_literal_debug#pp lit annot)
          formatter lits
      in
      (* print in an horizontal box, or not *)
      if !_horizontal
        then fprintf formatter "@[<h>[%a]@]" lits_printer c.clits
        else fprintf formatter "[%a]" lits_printer c.clits
    (* regular printing is printing with no literal selected *)
    inherit common_pp_clause
    method horizontal s = _horizontal := s
  end

let pp_clause_tstp =
  let _horizontal = ref true in
  object (self)
    method pp formatter c =
      (* how to print the list of literals *)
      let lits_printer formatter lits =
        (* convert into a big term *)
        let t = match Array.to_list lits with
          | [] -> T.false_term
          | hd::tl -> List.fold_left
            (fun t lit -> T.mk_or t (term_of_eqn lit.lit_eqn)) (term_of_eqn hd.lit_eqn) tl
        in
        (* quantify all free variables *)
        let t = Array.fold_left
          (fun t var -> T.mk_node forall_symbol bool_sort [var; t])
          t t.vars
        in
        T.pp_term_tstp#pp formatter t
      in
      (* print in an horizontal box, or not *)
      if !_horizontal
        then fprintf formatter "@[<h>%a@]" lits_printer c.clits
        else fprintf formatter "%a" lits_printer c.clits
    inherit common_pp_clause
    method horizontal s = _horizontal := s
  end

let pp_clause = ref pp_clause_debug

(** pretty printer for proofs *)
class type pprinter_proof =
  object
    method pp : Format.formatter -> clause -> unit      (** pretty print proof from clause *)
  end

let pp_proof_debug =
  object (self)
    method pp formatter clause =
      assert (clause.clits = [||]);
      (* already_printed is a set of clauses already printed. *)
      let already_printed = ref Ptset.empty
      and to_print = Queue.create () in
      (* initialize queue *)
      let hc = hashcons_clause clause in
      Queue.add hc to_print; 
      (* print every clause in the queue, if not already printed *)
      while not (Queue.is_empty to_print) do
        let hc = Queue.take to_print in
        if Ptset.mem hc.ctag !already_printed then ()
        else begin
          already_printed := Ptset.add hc.ctag !already_printed;
          match Lazy.force hc.cproof with
          | Axiom (f, s) ->
              fprintf formatter "@[<hov 4>@[<h>%a@]@ <--- @[<h>axiom %s in %s@]@]@;"
                !pp_clause#pp hc s f
          | Proof (rule, premises) ->
              (* print the proof step *)
              fprintf formatter "@[<hov 4>@[<h>%a@]@ <--- @[<h>%s with @[<hv>%a@]@]@]@;"
                !pp_clause#pp hc rule
                (Utils.pp_list ~sep:", " !pp_clause#pp_pos_subst) premises;
              (* print premises recursively *)
              List.iter
                (fun (c, _, _) -> Queue.add (hashcons_clause c) to_print)
                premises
        end
      done
  end

let pp_proof_tstp =
  object (self)
    method pp formatter clause =
      assert (clause.clits = [||]);
      (* already_printed is a set of clauses already printed. *)
      let already_printed = ref Ptset.empty
      and clause_num = ref Ptmap.empty
      and counter = ref 1
      and to_print = Queue.create () in
      (* c -> hashconsed c, unique number for c *)
      let get_num clause = 
        let hc = hashcons_clause clause in
        try hc, Ptmap.find hc.ctag !clause_num
        with Not_found ->
          clause_num := Ptmap.add hc.ctag !counter !clause_num;
          incr counter;
          hc, Ptmap.find hc.ctag !clause_num
      in
      (* initialize queue *)
      let hc, num = get_num clause in
      Queue.add (hc, num) to_print; 
      (* print every clause in the queue, if not already printed *)
      while not (Queue.is_empty to_print) do
        let hc, num = Queue.take to_print in
        if Ptset.mem hc.ctag !already_printed then ()
        else begin
          already_printed := Ptset.add hc.ctag !already_printed;
          match Lazy.force hc.cproof with
          | Axiom (f, ax_name) ->
            fprintf formatter "@[<h>fof(%d, axiom, %a,@ @[<h>file('%s', %s)@]).@]@;"
              num pp_clause_tstp#pp hc f ax_name
          | Proof (name, premises) ->
            let premises = List.map (fun (c,_,_) -> get_num c) premises in
            (* print the inference *)
            fprintf formatter ("@[<h>fof(%d, derived, %a,@ " ^^
                               "@[<h>inference(%s, [status(thm)], @[<h>[%a]@])@]).@]@;")
              num pp_clause_tstp#pp hc name
              (Utils.pp_list ~sep:"," pp_print_int) (List.map snd premises);
            (* print every premise *)
            List.iter (fun (hc,num) -> Queue.add (hc, num) to_print) premises
        end
      done
  end

let pp_proof = ref pp_proof_debug

(** print the content of a bag *)
let pp_bag formatter bag =
  let clauses = ref [] in
  (* collect clauses in the list by iterating on the map *)
  M.iter (fun _ hc -> clauses := hc :: !clauses) bag.bag_clauses;
  (* print as a list of clauses *)
  fprintf formatter "@[<v>%a@]" (Utils.pp_list ~sep:"" !pp_clause#pp_h) !clauses
