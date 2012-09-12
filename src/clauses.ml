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

let eq_literal_com l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
      o1 = o2 && sign1 = sign2 &&
      ((T.eq_foterm l1 l2 && T.eq_foterm r1 r2) ||
       (T.eq_foterm l1 r2 && T.eq_foterm r1 l2))

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
  | Equation (l, r, sign, _) ->
    if sign
      then Utils.murmur_hash ((Utils.murmur_hash l.hkey) lxor r.hkey)
      else Utils.murmur_hash ((Utils.murmur_hash r.hkey) lxor l.hkey)

let pos_lit lit = match lit with
  | Equation (_,_,sign,_) -> sign

let neg_lit lit = match lit with
  | Equation (_,_,sign,_) -> not sign

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
  if subst = S.id_subst then lit
  else match lit with
  | Equation (l,r,sign,_) ->
    assert (l.sort = r.sort);
    let new_l = S.apply_subst ~recursive subst l
    and new_r = S.apply_subst ~recursive subst r
    in
    Utils.debug 4 (lazy (Utils.sprintf "apply %a to %a gives %a"
        S.pp_substitution subst !T.pp_term#pp l !T.pp_term#pp new_l));
    Utils.debug 4 (lazy (Utils.sprintf "apply %a to %a gives %a"
        S.pp_substitution subst !T.pp_term#pp r !T.pp_term#pp new_r));
    mk_lit ~ord new_l new_r sign

let reord_lit ~ord (Equation (l,r,sign,_)) = Equation (l,r,sign, ord#compare l r)

let rec lit_of_fof ~ord ((Equation (l,r,sign,_)) as lit) =
  match l.term, r.term with
  (* deal with trivial literals *)
  | _ when T.eq_foterm l T.true_term && T.eq_foterm r T.false_term ->
    mk_lit ~ord T.true_term T.true_term (not sign)
  | _ when T.eq_foterm r T.true_term && T.eq_foterm l T.false_term ->
    mk_lit ~ord T.true_term T.true_term (not sign)
  | _ when T.eq_foterm l r ->
    mk_lit ~ord T.true_term T.true_term sign
  (* deal with false/true *)
  | _ when T.eq_foterm l T.false_term ->
    assert (r.sort = bool_sort);
    lit_of_fof ~ord (mk_lit ~ord r T.true_term (not sign))
  | _ when T.eq_foterm r T.false_term ->
    assert (l.sort = bool_sort);
    lit_of_fof ~ord (mk_lit ~ord l T.true_term (not sign))
  (* deal with negation *)
  | Node [{term=Leaf s}; t], _ when s = not_symbol && T.eq_foterm r T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord t T.true_term (not sign))
  | _, Node [{term=Leaf s}; t] when s = not_symbol && T.eq_foterm l T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord t T.true_term (not sign))
  (* deal with equality symbol *)
  | Node [{term=Leaf s}; a; b], _ when s = eq_symbol && T.eq_foterm r T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord a b sign)
  | _, Node [{term=Leaf s}; a; b] when s = eq_symbol && T.eq_foterm l T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord a b sign)
  (* default is just reordering *)
  | _ -> reord_lit ~ord lit

let term_of_lit lit =
  match lit with
  | Equation (left, right, false, _) when T.eq_foterm right T.true_term ->
    T.mk_not left
  | Equation (left, right, true, _) when T.eq_foterm right T.true_term ->
    left
  | Equation (left, right, true, _) when T.eq_foterm left T.true_term ->
    right
  | Equation (left, right, false, _) when T.eq_foterm left T.true_term ->
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

module H = Hashcons.Make(struct
  type t = clause
  let equal c1 c2 = eq_clause c1 c2 && (Lazy.force c1.cselected = Lazy.force c2.cselected)
  let hash c =
    let rec aux h = function
    | [] -> h
    | lit::tail -> aux (Utils.murmur_hash (h lxor hash_literal lit)) tail
    in aux 113 c.clits
  let tag i c = {c with ctag = i}
end)

let hashcons_clause c = H.hashcons c

let hashcons_clause_noselect c = H.hashcons {c with cselected=lazy []}

let stats () = H.stats ()

let eq_hclause hc1 hc2 = hc1 == hc2

let compare_hclause hc1 hc2 = Pervasives.compare hc1.ctag hc2.ctag

let check_maximal_lit_ ~ord clause pos subst =
  let lits_pos = Utils.list_pos clause.clits in
  let lit = Utils.list_get clause.clits pos in
  let slit = apply_subst_lit ~ord subst lit in
  List.for_all
    (fun (lit', idx) ->
      if idx = pos
        then (assert (eq_literal lit lit'); true)
        else
          let slit' = apply_subst_lit ~ord subst lit' in
          match compare_lits_partial ~ord slit slit' with
          | Eq | Gt | Invertible | Incomparable -> true
          | Lt -> begin
            false  (* slit is not maximal *)
          end
    )
    lits_pos

let check_maximal_lit ~ord clause pos subst =
  prof_check_max_lit.HExtlib.profile (check_maximal_lit_ ~ord clause pos) subst

(** find the maximal literals among lits *)
let find_max_lits ~ord lits_pos =
  List.filter
    (fun (lit, idx) ->
      List.for_all
        (fun (lit', idx') ->
          if idx' = idx then true
          else compare_lits_partial ~ord lit lit' <> Lt
        )
        lits_pos
    )
    lits_pos

(** is literal maximal among given literals? *)
let max_among ~ord lit lits =
  List.for_all 
    (fun lit' ->
      if eq_literal_com lit lit' then true
      else compare_lits_partial ~ord lit lit' <> Lt)
    lits

let mk_clause ~ord lits ~selected proof parents =
  (* merge sets of variables *)
  let rec merge_vars acc vars1 = match vars1 with
  | [] -> acc
  | v::vars1' when List.mem v acc -> merge_vars acc vars1'
  | v::vars1' -> merge_vars (v::acc) vars1'
  in
  let all_vars = List.fold_left merge_vars [] (List.map vars_of_lit lits) in
  let all_vars = List.stable_sort T.compare_foterm all_vars
  and maxlits = lazy (find_max_lits ~ord (Utils.list_pos lits)) in
  {clits=lits; cvars=all_vars; cproof=proof; cselected=selected; cparents=parents;
   cmaxlits=maxlits; ctag= -1}

let maxlits clause = Lazy.force clause.cmaxlits

let selected clause = Lazy.force clause.cselected

let parents clause = Lazy.force clause.cparents

let clause_of_fof ~ord c =
  mk_clause ~ord (List.map (lit_of_fof ~ord) c.clits)
    ~selected:c.cselected c.cproof c.cparents

let reord_clause ~ord c =
  mk_clause ~ord (List.map (reord_lit ~ord) c.clits)
    ~selected:c.cselected c.cproof c.cparents

let select_clause ~select c =
  {c with cselected = lazy (select c)}

let rec apply_subst_cl ?(recursive=true) ~ord subst c =
  if subst = S.id_subst then c
  else
    let new_lits = List.map (apply_subst_lit ~recursive ~ord subst) c.clits
    and new_parents = lazy (List.map (apply_subst_cl ~recursive ~ord subst) (parents c)) in
    mk_clause ~ord new_lits ~selected:c.cselected c.cproof new_parents
  (* TODO modify proof lazily *)

let get_lit clause idx = Utils.list_get clause.clits idx

let get_pos clause pos =
  match pos with
  | idx::side::tpos ->
      let lit = get_lit clause idx in
      let rec find_subterm pos t = match (pos, t.term) with
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
  (apply_subst_cl ~recursive:false ~ord subst c), maxvar

let relocate_clause ~ord varlist c =
  let idx = T.max_var c.cvars in
  let _, newvars, subst = S.relocate ~recursive:false idx c.cvars S.id_subst in
  apply_subst_cl ~recursive:false ~ord subst c

let normalize_clause ~ord c = fst (fresh_clause ~ord 0 c)

(** check whether a literal is selected *)
let selected_lit c idx =
  let rec check l = match l with
  | [] -> false
  | i::l' -> if i = idx then true else check l'
  in
  check (selected c)

(** get the list of selected literals *)
let selected_lits c =
  List.map (fun idx -> get_lit c idx, idx) (selected c)

(** check whether a literal is eligible for resolution *)
let eligible_res ~ord c idx subst =
  (* find selected maximal literals with given sign *)
  let rec gather_slits acc lits_pos sign = match lits_pos with
  | [] -> acc
  | (Equation (_, _, sign', _) as lit, idx)::lits_pos' when sign=sign' ->
    let acc' = if selected_lit c idx
      then (apply_subst_lit ~ord subst lit) :: acc (* same sign, maximal, selected *)
      else acc in
    gather_slits acc' lits_pos' sign
  | _::lits_pos' ->  gather_slits acc lits_pos' sign  (* goto next *)
  in 
  (* if no lit is selected, max lits are eligible *)
  if selected c = []
    then check_maximal_lit ~ord c idx subst
    else begin
      (* check maximality among selected literals of same sign *)
      let (Equation (_,_,sign,_) as lit) = get_lit c idx in
      let slit = apply_subst_lit ~ord subst lit in
      let set = gather_slits [] (maxlits c) sign in
      max_among ~ord slit set
    end

(** check whether a literal is eligible for paramodulation *)
let eligible_param ~ord c idx subst =
  if selected c <> [] then false
  else if neg_lit (get_lit c idx) then false (* only positive lits *)
  else check_maximal_lit ~ord c idx subst

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
  M.iter (fun _ _ -> count := !count+1) bag.bag_clauses;
  !count

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

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

(** pretty printer for literals *)
class type pprinter_literal =
  object
    method pp : Format.formatter -> literal -> unit     (** print literal *)
  end

let pp_literal_gen pp_term formatter lit =
  match lit with
  | Equation (l, r, sign, _) when T.eq_foterm r T.true_term ->
    if sign
      then pp_term#pp formatter l
      else Format.fprintf formatter "¬%a" pp_term#pp l
  | Equation (l, r, sign, _) when T.eq_foterm l T.true_term ->
    if sign
      then pp_term#pp formatter r
      else Format.fprintf formatter "¬%a" pp_term#pp r
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
  fprintf formatter "@[<h>%a@]" (Utils.pp_list ~sep:"." pp_print_int) pos

(** pretty printer for clauses *)
class type pprinter_clause =
  object
    method pp : Format.formatter -> clause -> unit      (** print clause *)
    method pp_h : Format.formatter -> hclause -> unit   (** print hclause *)
    method pp_pos : Format.formatter -> (clause * position) -> unit
    method pp_h_pos : Format.formatter -> (hclause * position * foterm) -> unit
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
  let is_max_lit c lit = List.exists (fun (lit',_) -> eq_literal lit lit') (maxlits c) in
  let pp_annot c (lit, idx) =
    ""^(if selected_lit c idx then "+" else "")
      ^(if is_max_lit c lit then "*" else "") in
  object (self)
    (* print literals with a '*' for maximal, and '+' for selected *)
    method pp formatter c =
      (* how to print the list of literals *)
      let lits_printer formatter lits =
        Utils.pp_list ~sep:" | "
          (fun formatter (lit, idx) ->
            let annot = pp_annot c (lit, idx) in
            fprintf formatter "%a%s" pp_literal_debug#pp lit annot)
          formatter lits
      in
      (* print in an horizontal box, or not *)
      if !_horizontal
        then fprintf formatter "@[<h>[%a]@]" lits_printer (Utils.list_pos c.clits)
        else fprintf formatter "[%a]" lits_printer (Utils.list_pos c.clits)
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
        let t = match lits with
          | [] -> T.false_term
          | hd::tl -> List.fold_left
            (fun t lit -> T.mk_or t (term_of_lit lit)) (term_of_lit hd) tl
        in
        (* quantify all free variables *)
        let vars = T.vars_of_term t in
        let t = List.fold_left
          (fun t var -> T.mk_node [T.mk_leaf forall_symbol bool_sort; var; t])
          t vars
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
      assert (clause.clits = []);
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
              fprintf formatter "@[<h>%a <--- @[<h>axiom %s in %s@]@]@;"
                !pp_clause#pp hc s f
          | Proof (rule, premises) ->
              (* print the proof step *)
              fprintf formatter "@[<h>%a <--- @[<h>%s with @[<hv>%a@]@]@]@;"
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
      assert (clause.clits = []);
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
