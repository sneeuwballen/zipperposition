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

(** build a map symbol -> int that gives an ordering on symbols, from the arities *)
let build_ordering arities symbols =
  let cmp t1 t2 = match t1, t2 with  (* caution, reverse order! *)
  | _, _ when t1 = t2 -> 0
  | _, _ when t1 = T.true_symbol -> 1
  | _, _ when t2 = T.true_symbol -> -1
  | _, _ when t1 = T.eq_symbol -> 1
  | _, _ when t2 = T.eq_symbol -> -1
  | _, _ -> Hashtbl.find arities t2 - Hashtbl.find arities t1 in
  (* thanks to the reverse order, sorted_symbols is stable-sorted by decreasing order *)
  let sorted_symbols = List.stable_sort cmp symbols in
  let cur_idx = ref 1
  and ordering = Hashtbl.create 23 in
  List.iter
    (fun s -> Hashtbl.replace ordering s !cur_idx; cur_idx := !cur_idx+1)
    sorted_symbols;
  ordering, sorted_symbols

let rec arity_ordering () : symbol_ordering =
  (* compute the current signature *)
  let sorts, arities, symbols = current_signature () in
  let ord, sorted_symbols = build_ordering arities symbols in
  (* the object itself *)
  object
    method refresh () = arity_ordering ()
    method signature = sorted_symbols
    method compare s1 s2 = if s1 = s2 then 0
      else (Hashtbl.find ord s2) - (Hashtbl.find ord s1)
    method weight s = Hashtbl.find arities s  (* weight is arity *)
  end

let default_symbol_ordering () = arity_ordering ()

let dummy_symbol_ordering =
  let rec produce () =
    object
      method refresh () = produce ()
      method signature = []
      method compare a b = Pervasives.compare a b
      method weight _ = 1
    end
  in produce ()

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

type weight = int * (int * int) list

let string_of_weight (cw, mw) =
  let s =
    String.concat ", "
      (List.map (function (m, w) -> Printf.sprintf "(%d,%d)" m w) mw)
  in
  Printf.sprintf "[%d; %s]" cw s

let weight_of_term ~so term =
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

let compute_clause_weight ~so {clits=lits} =
    let rec weight_of_polynomial w m =
      let factor = 2 in
      w + factor * List.fold_left (fun acc (_,occ) -> acc+occ) 0 m
    and weight_of_lit l = match l with
    | Equation (l,r,_,ord) ->  (* TODO use order? *)
        let wl, ml = weight_of_term ~so l in
        let wr, mr = weight_of_term ~so r in
        weight_of_polynomial (wl+wr) (ml@mr) in
    List.fold_left (+) 0 (List.map weight_of_lit lits)

(* Riazanov: 3.1.5 pag 38 *)
(* Compare weights normalized in a new way :
 * Variables should be sorted from the lowest index to the highest
 * Variables which do not occur in the term should not be present
 * in the normalized polynomial
 *)
let compare_weights (h1, w1) (h2, w2) =
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


module NRKBO = struct
  let name = "nrkbo"

  let eq_foterm x y = T.eq_foterm x y

  exception UnificationFailure of string Lazy.t

  let are_invertible l r = false   (* FIXME ignore this case *)
    (*
    let varlist = (T.vars_of_term l)@(T.vars_of_term r) in
    let maxvar = List.fold_left max 0 varlist in
    let _,_,subst = FoUtils.relocate maxvar varlist FoSubst.id_subst in
    let newl = FoSubst.apply_subst subst l in
    let newr = FoSubst.apply_subst subst r in
      try (let subst = FoUnif.alpha_eq l newr in
           eq_foterm newl (FoSubst.apply_subst subst r))
      with
	  UnificationFailure _ -> false
    *)

  (* Riazanov: p. 40, relation >_n *)
  let nonrec_kbo ~so t1 t2 =
    let w1 = weight_of_term ~so t1 in
    let w2 = weight_of_term ~so t2 in
    match compare_weights w1 w2 with
    | XLE ->  (* this is .> *)
        if aux_ordering so#compare t1 t2 = XLT then XLT else XINCOMPARABLE
    | XGE ->
        if aux_ordering so#compare t1 t2 = XGT then XGT else XINCOMPARABLE
    | XEQ -> let res = aux_ordering so#compare t1 t2 in
	if res = XINCOMPARABLE && are_invertible t1 t2 then XINVERTIBLE
	else res
    | res -> res

  let compare_terms ~so = compare_terms (nonrec_kbo ~so)

  let profiler = HExtlib.profile ~enable:true "compare_terms(nrkbo)"
  let compare_terms ~so x y =
    profiler.HExtlib.profile (compare_terms ~so x) y
end

module KBO = struct
  let name = "kbo"

  let eq_foterm = T.eq_foterm

  (* Riazanov: p. 38, relation > *)
  let rec kbo ~so t1 t2 =
    let aux = aux_ordering so#compare ~head_only:true in
    let rec cmp t1 t2 =
      match t1, t2 with
      | [], [] -> XEQ
      | _, [] -> XGT
      | [], _ -> XLT
      | hd1::tl1, hd2::tl2 ->
          let o = kbo ~so hd1 hd2 in
          if o = XEQ then cmp tl1 tl2
          else o
    in
    let w1 = weight_of_term ~so t1 in
    let w2 = weight_of_term ~so t2 in
    let comparison = compare_weights w1 w2 in
    match comparison with
    | XLE ->
        let r = aux t1 t2 in
        if r = XLT then XLT
        else if r = XEQ then (
          match t1.node.term, t2.node.term with
          | Node (_::tl1), Node (_::tl2) ->
              if cmp tl1 tl2 = XLT then XLT else XINCOMPARABLE
          | _, _ -> assert false
        ) else XINCOMPARABLE
    | XGE ->
        let r = aux t1 t2 in
        if r = XGT then XGT
        else if r = XEQ then (
          match t1.node.term, t2.node.term with
          | Node (_::tl1), Node (_::tl2) ->
              if cmp tl1 tl2 = XGT then XGT else XINCOMPARABLE
          | _, _ ->  assert false
        ) else XINCOMPARABLE
    | XEQ ->
        let r = aux t1 t2 in
        if r = XEQ then (
          match t1.node.term, t2.node.term with
	  | Var i, Var j when i=j -> XEQ
          | Node (_::tl1), Node (_::tl2) -> cmp tl1 tl2
          | _, _ ->  XINCOMPARABLE
        ) else r
    | res -> res

  let compare_terms ~so = compare_terms (kbo ~so)

  let profiler = HExtlib.profile ~enable:true "compare_terms(kbo)"
  let compare_terms ~so x y =
    profiler.HExtlib.profile (compare_terms ~so x) y
end

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

class nrkbo (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 29
    val so = so
    method refresh () = ({< so = so#refresh () >} :> ordering)
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.with_cache cache
      (fun (a, b) -> NRKBO.compare_terms ~so a b) (a, b)
    method compute_clause_weight c = compute_clause_weight ~so c
    method name = NRKBO.name
  end

class kbo (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 29
    val so = so
    method refresh () = ({< so = so#refresh () >} :> ordering)
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.with_cache cache
      (fun (a, b) -> KBO.compare_terms ~so a b) (a, b)
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
    method compute_clause_weight c =
      compute_clause_weight ~so:dummy_symbol_ordering c
    method name = "dummy"
  end
