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

open Types
open Hashcons

module T = Terms
module C = Clauses
module Utils = FoUtils

(* ----------------------------------------------------------------------
 symbol ordering
 ---------------------------------------------------------------------- *)

(** compute the current signature: existing symbols,
    with their aritys and sorts *)
let current_signature () =
  let sorts = Hashtbl.create 23
  and arities  = Hashtbl.create 23
  and symbols = ref [] in
  T.iter_terms
    (fun t -> match t.node.term with
     | Var _ -> ()
     | Leaf s ->
        begin
          (* update the arity only if not already found *)
          if not (Hashtbl.mem arities s) then Hashtbl.replace arities s 0;
          Hashtbl.replace sorts s t.node.sort;
          if not (List.mem s !symbols) then symbols := (s::!symbols) else ()
        end
     | Node (({node={term=Leaf s}})::tail) ->
        Hashtbl.replace arities s (List.length tail)
     | _ -> assert false);
  sorts, arities, !symbols

let cluster_constraint clusters =
  let table = Hashtbl.create 17
  and cluster_num = ref 0 in
  (* for each cluster, assign it a (incremented) number, and
     remember symbol->number for every symbol of the cluster *)
  List.iter
    (fun cluster ->
      let num = !cluster_num in
      incr cluster_num;
      List.iter (fun symb -> Hashtbl.add table symb num) cluster)
    clusters;
  (* compare symbols by their number, if they have. Smaller numbers are bigger symbols *)
  let compare s1 s2 =
    try
      let s1_num = Hashtbl.find table s1
      and s2_num = Hashtbl.find table s2 in
      s2_num - s1_num
    with Not_found -> 0 (* at least one is not in the table, we do not order *)
  in compare

let list_constraint l =
  let num = ref  0
  and table = Hashtbl.create 13 in
  (* give a number to every symbol *)
  List.iter
    (fun symb ->
      let symb_num = !num in
      incr num;
      Hashtbl.add table symb symb_num)
    l;
  (* compare symbols by number. Smaller symbols have bigger number *)
  let compare s1 s2 =
    try
      let s1_num = Hashtbl.find table s1
      and s2_num = Hashtbl.find table s2 in
      s2_num - s1_num
    with Not_found -> 0 (* at least one is not in the table, we do not order *)
  in compare

let ordering_to_constraint so =
  list_constraint so#signature 

let arity_constraint arities =
  let compare s1 s2 =
    try
      let s1_arity = Hashtbl.find arities s1
      and s2_arity = Hashtbl.find arities s2 in
      s1_arity - s2_arity  (* bigger arity is bigger *)
    with Not_found -> 0
  in compare

let max_constraint symbols =
  let table = Hashtbl.create 11
  and num = ref 0 in
  (* give number to symbols *)
  List.iter
    (fun symb -> let n = !num in
      incr num; Hashtbl.add table symb n)
    symbols;
  let compare a b =
    (* not found implies the symbol is smaller than maximal symbols *)
    let a_n = try Hashtbl.find table a with Not_found -> !num
    and b_n = try Hashtbl.find table b with Not_found -> !num in
    b_n - a_n  (* if a > b then a_n < b_n *)
  in compare
  
let min_constraint symbols =
  let table = Hashtbl.create 11
  and num = ref 0 in
  (* give number to symbols *)
  List.iter
    (fun symb -> let n = !num in
      incr num; Hashtbl.add table symb n)
    symbols;
  let compare a b =
    (* not found implies the symbol is bigger than minimal symbols *)
    let a_n = try Hashtbl.find table a with Not_found -> -1
    and b_n = try Hashtbl.find table b with Not_found -> -1 in
    b_n - a_n  (* if a > b then a_n < b_n *)
  in compare

let compose_constraints c1 c2 =
  (* first we compare using c2, then using c1 if needed, because
     c2 is prioritary *)
  let compare a b =
    let c2_ab = c2 a b in
    if c2_ab <> 0 then c2_ab  (* c2 has decided *)
    else c1 a b               (* let c1 decide *)
  in compare

(** constraint that makes the three symbols the smaller ones *)
let consts_constraint = min_constraint [T.false_symbol; T.true_symbol]

let rec apply_constraint so constr =
  let symbols = so#signature in
  (* stable_sort the signature in decreasing order using the constraint *)
  let ordered_symbols = List.stable_sort (fun x y -> - (constr x y)) symbols in
  (* comparison function is given by the place in the ordered signature *)
  let new_compare = list_constraint ordered_symbols in
  object (self)
    method refresh () = apply_constraint (so#refresh ()) constr
    method signature = ordered_symbols
    method compare = new_compare
    method weight s = so#weight s       (* delegate to so *)
    method var_weight = so#var_weight   (* delegate to so *)
  end

let check_constraint so constr =
  (* check whether a list is sorted in decreasing order w.r.t constraint *)
  let rec is_sorted l = match l with
  | [] | [_] -> true
  | x::y::l' ->
    let cmp_xy = constr x y in
    if cmp_xy >= 0 then is_sorted l'
    else false
  in
  is_sorted so#signature

let dummy_symbol_ordering =
  (* recompute signature *)
  let rec produce () =
    let _, _, signature = current_signature () in
    let signature = List.sort Pervasives.compare signature in
    object
      method refresh () = produce ()
      method signature = signature
      method compare a b = Pervasives.compare a b
      method weight _ = 2
      method var_weight = 1
    end
  in produce ()

let make_ordering constr =
  apply_constraint (dummy_symbol_ordering#refresh ()) constr

let rec default_symbol_ordering () =
  let _, arities, _ = current_signature () in
  let constr = compose_constraints
    (arity_constraint arities) consts_constraint in
  (* apply the constraints to the dummy symbol ordering *)
  make_ordering constr

(* ----------------------------------------------------------------------
 module interface for orderings, internal (used to create the different classes)
 ---------------------------------------------------------------------- *)

type aux_comparison = XEQ | XLE | XGE | XLT | XGT | XINCOMPARABLE | XINVERTIBLE

module type S =
  sig
    (* This order relation should be:
     * - stable for instantiation
     * - total on ground terms
     *)
    val compare_terms : so:symbol_ordering -> foterm -> foterm -> comparison

    val name : string
  end

(** simple weight for terms *)
let rec weight_of_term ~so term = match term.node.term with
  | Var _ -> so#var_weight
  | Leaf s -> so#weight s
  | Node l -> List.fold_left
      (fun sum subterm -> sum + weight_of_term ~so subterm) 0 l

(** simple weight for clauses *)
let compute_clause_weight ~so {clits=lits} =
  let weight_of_lit l = match l with
  | Equation (l,r,_,ord) ->
      let wl = weight_of_term ~so l in
      let wr = weight_of_term ~so r in
      wl + wr
  in
  (* sum of squares of weights of literals *)
  List.fold_left
    (fun sum lit ->
      let wlit = weight_of_lit lit in
      sum + wlit*wlit)
    0 lits 

(** extended weight for KBO, with multiset of variables *)
type kbo_weight = int * (int * int) list

let string_of_weight (cw, mw) =
  let s =
    String.concat ", "
      (List.map (function (m, w) -> Printf.sprintf "(%d,%d)" m w) mw)
  in
  Printf.sprintf "[%d; %s]" cw s

let kbo_weight_of_term ~so term =
  let vars_dict = Hashtbl.create 5 in
  let rec aux x = match x.node.term with
    | Var i ->
        (try
           let oldw = Hashtbl.find vars_dict i in
           Hashtbl.replace vars_dict i (oldw+1)
         with Not_found ->
           Hashtbl.add vars_dict i 1);
        0
    | Leaf symb -> so#weight symb
    | Node l -> List.fold_left (+) 0 (List.map aux l)
  in
  let w = aux term in
  let l =
    Hashtbl.fold (fun meta metaw resw -> (meta, metaw)::resw) vars_dict []
  in
  let compare w1 w2 =
    match w1, w2 with
    | (m1, _), (m2, _) -> m1 - m2
  in
  (w, List.sort compare l) (* from the smallest meta to the bigest *)

let kbo_compute_clause_weight ~so {clits=lits} =
  let rec weight_of_polynomial w m =
    let factor = 2 in
    w + factor * List.fold_left (fun acc (_,occ) -> acc+occ) 0 m
  and weight_of_lit l = match l with
  | Equation (l,r,_,ord) ->  (* TODO use order? *)
      let wl, ml = kbo_weight_of_term ~so l in
      let wr, mr = kbo_weight_of_term ~so r in
      weight_of_polynomial (wl+wr) (ml@mr) in
  List.fold_left (+) 0 (List.map weight_of_lit lits)

(* Riazanov: 3.1.5 pag 38 *)
(* Compare weights normalized in a new way :
 * Variables should be sorted from the lowest index to the highest
 * Variables which do not occur in the term should not be present
 * in the normalized polynomial
 *)
let compare_kbo_weights (h1, w1) (h2, w2) =
  let rec aux hdiff (lt, gt) diffs w1 w2 =
    match w1, w2 with
      | ((var1, w1)::tl1) as l1, (((var2, w2)::tl2) as l2) ->
          if var1 = var2 then
            let diffs = (w1 - w2) + diffs in
            let r = Pervasives.compare w1 w2 in
            let lt = lt or (r < 0) in
            let gt = gt or (r > 0) in
              if lt && gt then XINCOMPARABLE else
                aux hdiff (lt, gt) diffs tl1 tl2
          else if var1 < var2 then
            if lt then XINCOMPARABLE else
              aux hdiff (false,true) (diffs+w1) tl1 l2
          else
            if gt then XINCOMPARABLE else
              aux hdiff (true,false) (diffs-w2) l1 tl2
      | [], (_,w2)::tl2 ->
          if gt then XINCOMPARABLE else
            aux hdiff (true,false) (diffs-w2) [] tl2
      | (_,w1)::tl1, [] ->
          if lt then XINCOMPARABLE else
            aux hdiff (false,true) (diffs+w1) tl1 []
      | [], [] ->
          if lt then
            if hdiff <= 0 then XLT
            else if (- diffs) >= hdiff then XLE else XINCOMPARABLE
          else if gt then
            if hdiff >= 0 then XGT
            else if diffs >= (- hdiff) then XGE else XINCOMPARABLE
          else
            if hdiff < 0 then XLT
            else if hdiff > 0 then XGT
            else XEQ
  in
    aux (h1-h2) (false,false) 0 w1 w2


(* Riazanov: p. 40, relation >>>
 * if head_only=true then it is not >>> but helps case 2 of 3.14 p 39 *)
let rec aux_ordering b_compare ?(head_only=false) t1 t2 =
  match t1.node.term, t2.node.term with
  (* We want to discard any identity equality. *
   * If we give back XEQ, no inference rule    *
   * will be applied on this equality          *)
  | Var i, Var j when i = j ->
      XEQ
  (* 1. *)
  | Var _, _
  | _, Var _ -> XINCOMPARABLE
  (* 2.a *)
  | Leaf a1, Leaf a2 ->
      let cmp = b_compare a1 a2 in
      if cmp = 0 then XEQ else if cmp < 0 then XLT else XGT
  | Leaf _, Node _ -> XLT
  | Node _, Leaf _ -> XGT
  (* 2.b *)
  | Node l1, Node l2 ->
      let rec cmp t1 t2 =
        match t1, t2 with
        | [], [] -> XEQ
        | _, [] -> (* XGT *) assert false (* hd symbols were eq *)
        | [], _ -> (* XLT *) assert false (* hd symbols were eq *)
        | hd1::tl1, hd2::tl2 ->
            let o = aux_ordering b_compare ~head_only hd1 hd2 in
            if o = XEQ && not head_only then cmp tl1 tl2 else o
      in
      cmp l1 l2


(* compare terms using the given auxiliary ordering, and
   convert the result to .Comparison *)
let compare_terms o x y =
    match o x y with
      | XINCOMPARABLE -> Incomparable
      | XGT -> Gt
      | XLT -> Lt
      | XEQ -> Eq
      | XINVERTIBLE -> Invertible
      | _ -> assert false

module KBO = struct
  let name = "kbo"

  let eq_foterm = T.eq_foterm

  let kbo ~so t1 t2 = XINCOMPARABLE (* TODO *)

  let compare_terms ~so = compare_terms (kbo ~so)

  let profiler = HExtlib.profile ~enable:true "compare_terms(kbo)"
  let compare_terms ~so x y =
    profiler.HExtlib.profile (compare_terms ~so x) y
end

(* TODO extend into RPO, for symbols with multiset status *)
module LPO = struct
  let name = "lpo"

  let eq_foterm = T.eq_foterm

  let rec lpo ~so s t =
    match s.node.term, t.node.term with
      | _, _ when eq_foterm s t ->
          XEQ
      | Var _, Var _ ->
          XINCOMPARABLE
      | _, Var i ->
          if (List.mem t (T.vars_of_term s)) then XGT
          else XINCOMPARABLE
      | Var i,_ ->
          if (List.mem s (T.vars_of_term t)) then XLT
          else XINCOMPARABLE
      | Node (hd1::tl1), Node (hd2::tl2) ->
          let rec ge_subterm t ol = function
            | [] -> (false, ol)
            | x::tl ->
                let res = lpo ~so x t in
                match res with
                  | XGT | XEQ -> (true,res::ol)
                  | o -> ge_subterm t (o::ol) tl
          in
          let (res, l_ol) = ge_subterm t [] tl1 in
            if res then XGT
            else let (res, r_ol) = ge_subterm s [] tl2 in
              if res then XLT
              else begin
                let rec check_subterms t = function
                  | _,[] -> true
                  | o::ol,_::tl ->
                      if o = XLT then check_subterms t (ol,tl)
                      else false
                  | [], x::tl ->
                      if lpo ~so x t = XLT then check_subterms t ([],tl)
                      else false
                in
                match aux_ordering so#compare hd1 hd2 with
                  | XGT -> if check_subterms s (r_ol,tl2) then XGT
                    else XINCOMPARABLE
                  | XLT -> if check_subterms t (l_ol,tl1) then XLT
                    else XINCOMPARABLE
                  | XEQ ->
                     (try
                      let lex = List.fold_left2
                        (fun acc si ti -> if acc = XEQ then lpo ~so si ti else acc)
                        XEQ tl1 tl2
                      in
                 (match lex with
                    | XGT ->
                        if List.for_all (fun x -> lpo ~so s x = XGT) tl2 then XGT
                      else XINCOMPARABLE
                    | XLT ->
                        if List.for_all (fun x -> lpo ~so x t = XLT) tl1 then XLT
                      else XINCOMPARABLE
                    | o -> o)
                      with Invalid_argument _ -> (* assert false *)
                              XINCOMPARABLE)
              | XINCOMPARABLE -> XINCOMPARABLE
              | _ -> assert false
          end
      | _,_ -> aux_ordering so#compare s t

  let compare_terms ~so = compare_terms (lpo ~so)

  let profiler = HExtlib.profile ~enable:true "compare_terms(lpo)"
  let compare_terms ~so x y =
    profiler.HExtlib.profile (compare_terms ~so x) y
end

(* ----------------------------------------------------------------------
 class interface
 ---------------------------------------------------------------------- *)

(** cache for pairs of terms *)
module OrdCache = Cache.Make(
  struct
    type t = (foterm * foterm)
    let equal (x1,y1) (x2,y2) = T.eq_foterm x1 x2 && T.eq_foterm y1 y2
    let hash (x,y) = (Utils.murmur_hash x.hkey) lxor y.hkey
    let should_cache (x,y) = match x.node.term, y.node.term with
    | Node _, Node _ -> true  (* cache for complex terms *)
    | _ -> false
  end)

class kbo (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 29
    val so = so
    method refresh () = ({< so = so#refresh () >} :> ordering)
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.with_cache cache
      (fun (a, b) -> KBO.compare_terms ~so a b) (a, b)
    method compute_term_weight t = weight_of_term ~so t
    method compute_clause_weight c = compute_clause_weight ~so c
    method name = KBO.name
  end

class lpo (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 29
    val so = so
    method refresh () = ({< so = so#refresh () >} :> ordering)
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.with_cache cache
      (fun (a, b) -> LPO.compare_terms ~so a b) (a, b)
    method compute_term_weight t = weight_of_term ~so t
    method compute_clause_weight c = compute_clause_weight ~so c
    method name = LPO.name
  end

let default_ordering () = new lpo (default_symbol_ordering ())

let dummy_ordering =
  object
    method refresh () = ({< >} :> ordering)
    method clear_cache () = ()
    method symbol_ordering = dummy_symbol_ordering
    method compare a b = Incomparable
    method compute_term_weight t = weight_of_term ~so:dummy_symbol_ordering t
    method compute_clause_weight c =
      compute_clause_weight ~so:dummy_symbol_ordering c
    method name = "dummy"
  end
