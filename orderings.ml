(*
    ||M||  This file is part of HELM, an Hypertextual, Electronic        
    ||A||  Library of Mathematics, developed at the Computer Science     
    ||T||  Department, University of Bologna, Italy.                     
    ||I||                                                                
    ||T||  HELM is free software; you can redistribute it and/or         
    ||A||  modify it under the terms of the GNU General Public License   
    \   /  version 2 or (at your option) any later version.      
     \ /   This software is distributed as is, NO WARRANTY.     
      V_______________________________________________________________ *)

(* $Id: orderings.ml 10997 2010-10-17 09:12:29Z tassi $ *)

type aux_comparison = XEQ | XLE | XGE | XLT | XGT | XINCOMPARABLE | XINVERTIBLE

module type S =
  sig 
    type foterm = Terms.foterm

    (* This order relation should be:
     * - stable for instantiation
     * - total on ground terms
     *
     *)
    val compare_terms : Terms.foterm -> Terms.foterm -> Terms.comparison

    (* these could be outside the module, but to ease experimentation
     * we allow them to be tied with the ordering *)
    val compute_clause_weight : Terms.clause -> int

    val name : string
  end

module T = Terms
open Hashcons
open T
  
type weight = int * (int * int) list
  
let string_of_weight (cw, mw) =
  let s =
    String.concat ", "
      (List.map (function (m, w) -> Printf.sprintf "(%d,%d)" m w) mw)
  in
  Printf.sprintf "[%d; %s]" cw s
  
let weight_of_term term =
    let vars_dict = Hashtbl.create 5 in
    let rec aux x = match x.node.term with
      | T.Var i -> 
          (try
             let oldw = Hashtbl.find vars_dict i in
             Hashtbl.replace vars_dict i (oldw+1)
           with Not_found ->
             Hashtbl.add vars_dict i 1);
          0
      | T.Leaf _ -> 1
      | T.Node l -> List.fold_left (+) 0 (List.map aux l)
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

let compute_clause_weight (_,lits, _, _) = 
    let rec weight_of_polynomial w m =
      let factor = 2 in      
      w + factor * List.fold_left (fun acc (_,occ) -> acc+occ) 0 m
    and weight_of_lit l = match l with
    | T.Equation (l,r,_,ord) ->  (* TODO use order? *)
        let wl, ml = weight_of_term l in 
        let wr, mr = weight_of_term r in 
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
  | T.Var i, T.Var j when i = j ->
      XEQ
  (* 1. *)
  | T.Var _, _
  | _, T.Var _ -> XINCOMPARABLE
  (* 2.a *)
  | T.Leaf a1, T.Leaf a2 -> 
      let cmp = b_compare a1 a2 in
      if cmp = 0 then XEQ else if cmp < 0 then XLT else XGT
  | T.Leaf _, T.Node _ -> XLT
  | T.Node _, T.Leaf _ -> XGT
  (* 2.b *)
  | T.Node l1, T.Node l2 ->
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
   convert the result to Terms.Comparison *)
let compare_terms o x y = 
    match o x y with
      | XINCOMPARABLE -> T.Incomparable
      | XGT -> T.Gt
      | XLT -> T.Lt
      | XEQ -> T.Eq
      | XINVERTIBLE -> T.Invertible
      | _ -> assert false


module NRKBO = struct
  let name = "nrkbo"

  type foterm = Terms.foterm

  let eq_foterm x y = x == y

  exception UnificationFailure of string Lazy.t

  let are_invertible l r = false   (* FIXME ignore this case *)
    (*
    let relocate maxvar varlist subst =
      List.fold_right
        (fun i (maxvar, varlist, s) -> 
           maxvar+1, maxvar::varlist, FoSubst.build_subst i (T.Var maxvar) s)
        varlist (maxvar+1, [], subst) in
    let varlist = (T.vars_of_term l)@(T.vars_of_term r) in
    let maxvar = List.fold_left max 0 varlist in
    let _,_,subst = relocate maxvar varlist FoSubst.id_subst in
    let newl = FoSubst.apply_subst subst l in
    let newr = FoSubst.apply_subst subst r in
      try (let subst = FoUnif.alpha_eq l newr in
           eq_foterm newl (FoSubst.apply_subst subst r))
      with
	  UnificationFailure _ -> false
    *)

  let compute_clause_weight = compute_clause_weight
  
  (* Riazanov: p. 40, relation >_n *)
  let nonrec_kbo t1 t2 =
    let w1 = weight_of_term t1 in
    let w2 = weight_of_term t2 in
    match compare_weights w1 w2 with
    | XLE ->  (* this is .> *)
        if aux_ordering Signature.compare t1 t2 = XLT then XLT else XINCOMPARABLE
    | XGE -> 
        if aux_ordering Signature.compare t1 t2 = XGT then XGT else XINCOMPARABLE
    | XEQ -> let res = aux_ordering Signature.compare t1 t2 in
	if res = XINCOMPARABLE && are_invertible t1 t2 then XINVERTIBLE
	else res
    | res -> res
  
  let compare_terms = compare_terms nonrec_kbo

  let profiler = HExtlib.profile ~enable:true "compare_terms(nrkbo)"
  let compare_terms x y =
    profiler.HExtlib.profile (compare_terms x) y
end
  
module KBO = struct
  let name = "kbo"

  type foterm = Terms.foterm

  let eq_foterm x y = x == y

  let compute_clause_weight = compute_clause_weight

  (* Riazanov: p. 38, relation > *)
  let rec kbo t1 t2 =
    let aux = aux_ordering Signature.compare ~head_only:true in
    let rec cmp t1 t2 =
      match t1, t2 with
      | [], [] -> XEQ
      | _, [] -> XGT
      | [], _ -> XLT
      | hd1::tl1, hd2::tl2 ->
          let o = kbo hd1 hd2 in
          if o = XEQ then cmp tl1 tl2
          else o
    in
    let w1 = weight_of_term t1 in
    let w2 = weight_of_term t2 in
    let comparison = compare_weights w1 w2 in
    match comparison with
    | XLE ->
        let r = aux t1 t2 in
        if r = XLT then XLT
        else if r = XEQ then (
          match t1.node.term, t2.node.term with
          | T.Node (_::tl1), T.Node (_::tl2) ->
              if cmp tl1 tl2 = XLT then XLT else XINCOMPARABLE
          | _, _ -> assert false
        ) else XINCOMPARABLE
    | XGE ->
        let r = aux t1 t2 in
        if r = XGT then XGT
        else if r = XEQ then (
          match t1.node.term, t2.node.term with
          | T.Node (_::tl1), T.Node (_::tl2) ->
              if cmp tl1 tl2 = XGT then XGT else XINCOMPARABLE
          | _, _ ->  assert false
        ) else XINCOMPARABLE
    | XEQ ->
        let r = aux t1 t2 in
        if r = XEQ then (
          match t1.node.term, t2.node.term with
	  | T.Var i, T.Var j when i=j -> XEQ
          | T.Node (_::tl1), T.Node (_::tl2) -> cmp tl1 tl2
          | _, _ ->  XINCOMPARABLE
        ) else r 
    | res -> res
  

  let compare_terms = compare_terms kbo

  let profiler = HExtlib.profile ~enable:true "compare_terms(kbo)"
  let compare_terms x y =
    profiler.HExtlib.profile (compare_terms x) y
end

module LPO = struct
  let name = "lpo"

  type foterm = Terms.foterm

  let eq_foterm x y = x == y

  let compute_clause_weight = compute_clause_weight

  let rec lpo s t =
    match s.node.term, t.node.term with
      | _, _ when eq_foterm s t ->
          XEQ
      | T.Var _, T.Var _ ->
          XINCOMPARABLE
      | _, T.Var i ->
          if (List.mem t (T.vars_of_term s)) then XGT
          else XINCOMPARABLE
      | T.Var i,_ ->
          if (List.mem s (T.vars_of_term t)) then XLT
          else XINCOMPARABLE
      | T.Node (hd1::tl1), T.Node (hd2::tl2) ->
          let rec ge_subterm t ol = function
            | [] -> (false, ol)
            | x::tl ->
                let res = lpo x t in
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
                      if lpo x t = XLT then check_subterms t ([],tl)
                      else false
                in
                match aux_ordering Signature.compare hd1 hd2 with
                  | XGT -> if check_subterms s (r_ol,tl2) then XGT
                    else XINCOMPARABLE
                  | XLT -> if check_subterms t (l_ol,tl1) then XLT
                    else XINCOMPARABLE
                  | XEQ -> 
                     (try
                      let lex = List.fold_left2
                        (fun acc si ti -> if acc = XEQ then lpo si ti else acc)
                        XEQ tl1 tl2
                      in
                 (match lex with
                    | XGT ->
                        if List.for_all (fun x -> lpo s x = XGT) tl2 then XGT
                      else XINCOMPARABLE
                    | XLT ->
                        if List.for_all (fun x -> lpo x t = XLT) tl1 then XLT
                      else XINCOMPARABLE
                    | o -> o)   
                      with Invalid_argument _ -> (* assert false *)
                              XINCOMPARABLE)
              | XINCOMPARABLE -> XINCOMPARABLE
              | _ -> assert false
          end
      | _,_ -> aux_ordering Signature.compare s t
            
  

  let compare_terms = compare_terms lpo

  let profiler = HExtlib.profile ~enable:true "compare_terms(lpo)"
  let compare_terms x y =
    profiler.HExtlib.profile (compare_terms x) y
end

(* default ordering (LPO) *)
module Default = LPO
