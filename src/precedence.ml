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
open Symbols

module T = Terms
module Utils = FoUtils

(** Precedence on symbols *)

(** compute the current signature: existing symbols,
    with their arities and sorts *)
let compute_signature () =
  let sorts, arities, symbols = base_signature () in
  let symbols = ref symbols in
  T.iter_terms
    (fun t -> match t.term with
     | Var _ -> ()
     | Node (s, l) ->
       begin
         (* update the arity only if not already found *)
         if not (SHashtbl.mem arities s) then SHashtbl.replace arities s (List.length l);
         (* update sort only if it is not already bool *)
         (if (try SHashtbl.find sorts s == bool_sort with Not_found -> false)
           then ()
           else SHashtbl.replace sorts s t.sort);
         if not (List.mem s !symbols) then symbols := s::!symbols
       end);
  sorts, arities, !symbols

let sig_version = ref 0  (* version of signature that is computed *)

let current_signature =
  (* store the signature, to avoid recomputing it all the time *)
  let cached_signature = ref (compute_signature ()) in
  fun () ->
    assert (!sig_version <= !Symbols.sig_version);
    (if !sig_version < !Symbols.sig_version
      then(* recompute signature, it did change *)
        cached_signature := compute_signature ());
    !cached_signature

(* ----------------------------------------------------------------------
 * hard constraints on the ordering
 * ---------------------------------------------------------------------- *)

let cluster_constraint clusters =
  let table = SHashtbl.create 17
  and cluster_num = ref 0 in
  (* for each cluster, assign it a (incremented) number, and
     remember symbol->number for every symbol of the cluster *)
  List.iter
    (fun cluster ->
      let num = !cluster_num in
      incr cluster_num;
      List.iter (fun symb -> SHashtbl.add table symb num) cluster)
    clusters;
  (* compare symbols by their number, if they have. Smaller numbers are bigger symbols *)
  let compare s1 s2 =
    try
      let s1_num = SHashtbl.find table s1
      and s2_num = SHashtbl.find table s2 in
      s2_num - s1_num
    with Not_found -> 0 (* at least one is not in the table, we do not order *)
  in compare

let list_constraint l =
  let num = ref  0
  and table = SHashtbl.create 13 in
  (* give a number to every symbol *)
  List.iter
    (fun symb ->
      let symb_num = !num in
      assert (symb == mk_symbol (name_symbol symb));
      incr num;
      SHashtbl.add table symb symb_num)
    l;
  (* compare symbols by number. Smaller symbols have bigger number *)
  let compare s1 s2 =
    try
      let s1_num = SHashtbl.find table s1
      and s2_num = SHashtbl.find table s2 in
      s2_num - s1_num
    with Not_found -> 0 (* at least one is not in the table, we do not order *)
  in compare

let ordering_to_constraint so =
  list_constraint so#signature 

let arity_constraint arities =
  let compare s1 s2 =
    try
      let s1_arity = SHashtbl.find arities s1
      and s2_arity = SHashtbl.find arities s2 in
      s1_arity - s2_arity  (* bigger arity is bigger *)
    with Not_found -> 0
  in compare

let max_constraint symbols =
  let table = SHashtbl.create 11
  and num = ref 0 in
  (* give number to symbols *)
  List.iter
    (fun symb -> let n = !num in
      incr num; SHashtbl.add table symb n)
    symbols;
  let compare a b =
    (* not found implies the symbol is smaller than maximal symbols *)
    let a_n = try SHashtbl.find table a with Not_found -> !num
    and b_n = try SHashtbl.find table b with Not_found -> !num in
    b_n - a_n  (* if a > b then a_n < b_n *)
  in compare
  
let min_constraint symbols =
  let table = SHashtbl.create 11
  and num = ref 0 in
  (* give number to symbols *)
  List.iter
    (fun symb -> let n = !num in
      incr num; SHashtbl.add table symb n)
    symbols;
  let compare a b =
    (* not found implies the symbol is bigger than minimal symbols *)
    let a_n = try SHashtbl.find table a with Not_found -> -1
    and b_n = try SHashtbl.find table b with Not_found -> -1 in
    b_n - a_n  (* if a > b then a_n < b_n *)
  in compare

(* regular string ordering *)
let alpha_constraint a b = Pervasives.compare a b

let compose_constraints c1 c2 =
  (* first we compare using c2, then using c1 if needed, because
     c2 is prioritary *)
  let compare a b =
    let c2_ab = c2 a b in
    if c2_ab <> 0 then c2_ab  (* c2 has decided *)
    else c1 a b               (* let c1 decide *)
  in compare

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


(* ----------------------------------------------------------------------
 * Heuristic creation of precedences
 * ---------------------------------------------------------------------- *)

(** special heuristic: an ordering constraint that makes symbols
    occurring in negative equations bigger than symbols in
    positive equations in the given list of clauses *)
let heuristic_constraint clauses : ordering_constraint =
  let _, _, signature = current_signature () in
  let table = Hashtbl.create 23 in (* symbol -> (neg occurrences - pos occurences) *)
  (* update counts with term *)
  let rec update_with_term sign t = match t.term with
    | Var _ -> ()
    | Node (s, l) ->
        let count = try Hashtbl.find table s with Not_found -> 0 in
        Hashtbl.replace table s (if sign then count-1 else count+1);
        List.iter (update_with_term sign) l
  (* update counts with clause *)
  and update_with_clause clause =
    List.iter
      (fun (Equation (l, r, sign, _)) ->
        update_with_term sign l;
        update_with_term sign r)
    clause.clits
  in 
  List.iter update_with_clause clauses;
  (* sort symbols by decreasing (neg occurences - pos occurences) *)
  let ordered_symbols = List.sort
    (fun a b ->
      let count_a = try Hashtbl.find table a with Not_found -> 0
      and count_b = try Hashtbl.find table b with Not_found -> 0 in
      count_b - count_a)
    signature
  in
  (* make a constraint out of the ordered signature *)
  list_constraint ordered_symbols

(* ----------------------------------------------------------------------
 * Creation of a precedence (symbol_ordering) from constraints
 * ---------------------------------------------------------------------- *)

(* build an ordering from a constraint *)
let make_ordering constr =
  (* references that hold current state *)
  let cur_signature = ref []
  and cmp = ref (fun x y -> 0)
  and multiset_pred = ref (fun s -> s = eq_symbol) in
  (* the object itself *)
  let obj = object (self)
    (* refresh computes a new ordering based on the current signature *)
    method refresh () =
      (* the constraint is: keep the current signature in the same order *)
      let keep_constr = compose_constraints constr !cmp in
      (* the new signature, possibly with more symbols*)
      let _, _, symbols = current_signature () in
      (* sort according to the constraint *)
        cur_signature := List.stable_sort (fun x y -> -(keep_constr x y)) symbols;
        (* comparison function is given by the place in the ordered signature *)
        cmp := list_constraint !cur_signature;
        Utils.debug 3 (lazy (Utils.sprintf "%% new signature %a" T.pp_signature self#signature));
        (* assert (check_constraint self (list_constraint old_signature)) *)
      method signature = !cur_signature
      method compare a b = !cmp a b
      method multiset_status s = !multiset_pred s
      method set_multiset f = multiset_pred := f
    end
    in
    (* do the initial computation and return the object *)
    obj#refresh ();
    obj

let rec default_symbol_ordering () =
  let _, arities, _ = current_signature () in
  (* two constraints: false, true at end of precedence, and arity constraint *)
  let constr = compose_constraints
    (arity_constraint arities) (min_constraint [false_symbol; true_symbol]) in
  (* apply the constraints to the dummy symbol ordering *)
  make_ordering constr
