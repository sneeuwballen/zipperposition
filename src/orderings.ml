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
    (fun t -> match t.term with
     | Var _ -> ()
     | Leaf s ->
        begin
          (* update the arity only if not already found *)
          if not (Hashtbl.mem arities s) then Hashtbl.replace arities s 0;
          (* update sort only if it is not already bool *)
          (if (try Hashtbl.find sorts s = bool_sort with Not_found -> false)
            then ()
            else Hashtbl.replace sorts s t.sort);
          if not (List.mem s !symbols) then symbols := (s::!symbols) else ()
        end
     | Node (({term=Leaf s})::tail) ->
        Hashtbl.replace arities s (List.length tail)
     | _ -> failwith (Utils.sprintf "bad term %a" !T.pp_term#pp t));
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

(** constraint that makes the three symbols the smaller ones *)
let consts_constraint = min_constraint [false_symbol; true_symbol]

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

(* build an ordering from a constraint *)
let make_ordering constr =
  (* references that hold current state *)
  let cur_signature = ref []
  and cmp = ref (fun x y -> 0)
  and multiset_pred = ref (fun s -> s = eq_symbol) in
  (* the object itself *)
  let obj = object
    (* refresh computes a new ordering based on the current signature *)
    method refresh () =
      let _, _, symbols = current_signature () in
      (* sort according to the constraint *)
      cur_signature := List.stable_sort (fun x y -> - (constr x y)) symbols;
      (* comparison function is given by the place in the ordered signature *)
      cmp := list_constraint !cur_signature
    method signature = !cur_signature
    method compare a b = !cmp a b
    method weight s = 2
    method var_weight = 1
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
    (arity_constraint arities) consts_constraint in
  (* apply the constraints to the dummy symbol ordering *)
  make_ordering constr

(* ----------------------------------------------------------------------
 module interface for orderings, internal (used to create the different classes)
 ---------------------------------------------------------------------- *)

module type S =
  sig
    (* This order relation should be:
     * - stable for instantiation
     * - total on ground terms
     *)
    val compare_terms : so:symbol_ordering -> term -> term -> comparison

    val name : string
  end

(** simple weight for terms *)
let rec weight_of_term ~so term = match term.term with
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

(** helper to decompose a non-var term into (symbol, list of subterms) *)
let decompose t =
  let rec get_head t = match t.term with
  | Leaf a -> a
  | Node (hd::_) -> get_head hd
  | Var _ -> assert false
  | Node [] -> assert false
  in
  match t.term with
  | Leaf a -> a, []
  | Node ({term=Leaf a}::l) -> a, l
  | Node (hd::l) -> get_head hd, l
  | _ -> assert false

(* Riazanov: p. 40, relation >>>
 * if head_only=true then it is not >>> but helps case 2 of 3.14 p 39 *)
let rec aux_ordering b_compare ?(head_only=false) t1 t2 =
  match t1.term, t2.term with
  (* We want to discard any identity equality. *
   * If we give back Eq, no inference rule    *
   * will be applied on this equality          *)
  | Var i, Var j when i = j ->
      Eq
  (* 1. *)
  | Var _, _
  | _, Var _ -> Incomparable
  (* 2.a *)
  | Leaf a1, Leaf a2 ->
      let cmp = b_compare a1 a2 in
      if cmp = 0 then Eq else if cmp < 0 then Lt else Gt
  | Leaf _, Node _ -> Lt
  | Node _, Leaf _ -> Gt
  (* 2.b *)
  | Node l1, Node l2 ->
    let rec cmp t1 t2 =
      match t1, t2 with
      | [], [] -> Eq
      | _, [] -> (* Gt *) assert false (* hd symbols were eq *)
      | [], _ -> (* Lt *) assert false (* hd symbols were eq *)
      | hd1::tl1, hd2::tl2 ->
          let o = aux_ordering b_compare ~head_only hd1 hd2 in
          if o = Eq && not head_only then cmp tl1 tl2 else o
    in
    cmp l1 l2

module KBO = struct
  let name = "kbo"

  let eq_term = T.eq_term

  (** used to keep track of the balance of variables *)
  type var_balance = {
    offset : int;
    mutable pos_counter : int;
    mutable neg_counter : int;
    balance : int array;
  }

  (** create a balance for the two terms *)
  let mk_balance t1 t2 =
    if T.is_ground_term t1 && T.is_ground_term t2
      then
        { offset = 0; pos_counter = 0; neg_counter = 0; balance = Obj.magic None }
      else begin
        let minvar = min (T.min_var t1.vars) (T.min_var t2.vars)
        and maxvar = max (T.max_var t1.vars) (T.max_var t2.vars) in
        assert (minvar <= maxvar);
        let width = maxvar - minvar + 1 in  (* width between min var and max var *)
        let vb = {
          offset = minvar; (* offset of variables to 0 *)
          pos_counter = 0;
          neg_counter = 0;
          balance = Array.make width 0;
        } in
        Obj.set_tag (Obj.repr vb.balance) Obj.no_scan_tag;  (* no GC scan *)
        vb
      end

  (** add a positive variable *)
  let add_pos_var balance idx =
    let idx = idx - balance.offset in
    let n = balance.balance.(idx) in
    (if n = 0
      then balance.pos_counter <- balance.pos_counter + 1
      else if n = -1 then balance.neg_counter <- balance.neg_counter - 1);
    balance.balance.(idx) <- n + 1

  (** add a negative variable *)
  let add_neg_var balance idx =
    let idx = idx - balance.offset in
    let n = balance.balance.(idx) in
    (if n = 0
      then balance.neg_counter <- balance.neg_counter + 1
      else if n = 1 then balance.pos_counter <- balance.pos_counter - 1);
    balance.balance.(idx) <- n - 1

  (** the KBO ordering itself. The implementation is borrowed from
      the kbo_5 version of "things to know when implementing KBO".
      It should be linear time. *)
  let rec kbo ~so t1 t2 =
    let balance = mk_balance t1 t2 in
    (** variable balance, weight balance, t contains variable y. pos
        stands for positive (is t the left term) *)
    let rec balance_weight wb t y pos =
      match t.term with
      | Var x ->
        if pos
          then (add_pos_var balance x; (wb + so#var_weight, x = y))
          else (add_neg_var balance x; (wb - so#var_weight, x = y))
      | Leaf s ->
        if pos then (wb + so#weight s, false) else (wb - so#weight s, false)
      | Node l ->
        balance_weight_rec wb l y pos false
    (** list version of the previous one, threaded with the check result *)
    and balance_weight_rec wb terms y pos res = match terms with
      | [] -> (wb, res)
      | t::terms' ->
        let (wb', res') = balance_weight wb t y pos in
        balance_weight_rec wb' terms' y pos (res || res')
    (** lexicographic comparison *)
    and tckbolex wb terms1 terms2 =
      match terms1, terms2 with
      | [], [] -> wb, Eq
      | t1::terms1', t2::terms2' ->
        (match tckbo wb t1 t2 with
        | (wb', Eq) -> tckbolex wb' terms1' terms2'
        | (wb', res) -> (* just compute the weights and return result *)
          let wb'', _ = balance_weight_rec wb' terms1' 0 true false in
          let wb''', _ = balance_weight_rec wb'' terms2' 0 false false in
          wb''', res)
      | [], _ | _, [] -> failwith "different arities in lexicographic comparison"
    (** commutative comparison. Not linear, must call kbo to
        avoid breaking the weight computing invariants *)
    and tckbocommute wb ss ts =
      (* multiset comparison *)
      let res = Utils.multiset_partial (kbo ~so) ss ts in
      (* also compute weights of subterms *)
      let wb', _ = balance_weight_rec wb ss 0 true false in
      let wb'', _ = balance_weight_rec wb' ts 0 false false in
      wb'', res
    (** tupled version of kbo (kbo_5 of the paper) *)
    and tckbo wb t1 t2 =
      match t1.term, t2.term with
      | _ when T.eq_term t1 t2 -> (wb, Eq) (* do not update weight or var balance *)
      | Var x, Var y ->
        add_pos_var balance x;
        add_neg_var balance y;
        (wb, Incomparable)
      | Var x,  _ ->
        add_pos_var balance x;
        let wb', contains = balance_weight wb t2 x false in
        (wb' + so#var_weight, if contains then Lt else Incomparable)
      |  _, Var y -> 
        add_neg_var balance y;
        let wb', contains = balance_weight wb t1 y true in
        (wb' - so#var_weight, if contains then Gt else Incomparable)
      | _ ->
        let f, ss = decompose t1
        and g, ts = decompose t2 in
        (* do the recursive computation of kbo *)
        let wb', recursive = tckbo_rec wb f g ss ts in
        let wb'' = wb' + (so#weight f) - (so#weight g) in
        (* check variable condition *)
        let g_or_n = if balance.neg_counter = 0 then Gt else Incomparable
        and l_or_n = if balance.pos_counter = 0 then Lt else Incomparable in
        (* lexicographic product of weight and precedence *)
        if wb'' > 0 then wb'', g_or_n
        else if wb'' < 0 then wb'', l_or_n
        else (match so#compare f g with
          | n when n > 0 -> wb'', g_or_n
          | n when n < 0 ->  wb'', l_or_n
          | _ ->
            assert (List.length ss = List.length ts);
            if recursive = Eq then wb'', Eq
            else if recursive = Lt then wb'', l_or_n
            else if recursive = Gt then wb'', g_or_n
            else wb'', Incomparable)
    (** recursive comparison *)
    and tckbo_rec wb f g ss ts =
      if f = g
        then if so#multiset_status f
          (* use multiset or lexicographic comparison *)
          then tckbocommute wb ss ts
          else tckbolex wb ss ts
        else
          (* just compute variable and weight balances *)
          let wb', _ = balance_weight_rec wb ss 0 true false in
          let wb'', _ = balance_weight_rec wb' ts 0 false false in
          wb'', Incomparable
    in
    let _, res = tckbo 0 t1 t2 in res  (* ignore the weight *)

  let profiler = HExtlib.profile ~enable:true "compare_terms(kbo)"
  let compare_terms ~so x y =
    profiler.HExtlib.profile (kbo ~so x) y
end

module RPO = struct
  let name = "rpo"

  let rec rpo ~so s t =
    match s.term, t.term with
    | _, _ when T.eq_term s t -> Eq
    | Var _, Var _ -> Incomparable
    | _, Var i -> if T.var_occurs t s then Gt else Incomparable
    | Var i,_ -> if T.var_occurs s t then Lt else Incomparable
    | Node (hd1::tl1), Node (hd2::tl2) ->
      (* check whether an elemnt of the list is >= t, and
         also returns the list of comparison results *)
      let rec ge_subterm t ol = function
        | [] -> (false, ol)
        | x::tl ->
            let res = rpo ~so x t in
            match res with
              | Gt | Eq -> (true, res::ol)
              | o -> ge_subterm t (o::ol) tl
      in
      (* try the subterm property (when s in t or t in s) *)
      let (res, l_ol) = ge_subterm t [] tl1 in
        if res then Gt
        else let (res, r_ol) = ge_subterm s [] tl2 in
          if res then Lt
          else begin
            (* check whether all terms of the list are smaller than t *)
            let rec check_subterms t = function
              | _, [] -> true
              | o::ol, _::tl ->
                  if o = Lt then check_subterms t (ol,tl) else false
              | [], x::tl ->
                  if rpo ~so x t = Lt then check_subterms t ([],tl) else false
            in
            (* non recursive comparison of function symbols *)
            match aux_ordering so#compare hd1 hd2 with
            | Gt -> if check_subterms s (r_ol,tl2) then Gt else Incomparable
            | Lt -> if check_subterms t (l_ol,tl1) then Lt else Incomparable
            | Eq -> rpo_rec ~so hd1 tl1 tl2 s t
            | Incomparable -> Incomparable
          end
    | _,_ -> aux_ordering so#compare s t
  (* recursive comparison of lists of terms (head symbol is hd) *)
  and rpo_rec ~so hd l1 l2 s t =
    match hd.term with
    | Var _ | Node _ -> assert false
    | Leaf f ->
    if so#multiset_status f
      then Utils.multiset_partial (rpo ~so) l1 l2
      else match Utils.lexicograph_partial (rpo ~so) l1 l2 with
        | Gt ->
          if List.for_all (fun x -> rpo ~so s x = Gt) l2 then Gt else Incomparable
        | Lt ->
          if List.for_all (fun x -> rpo ~so x t = Lt) l1 then Lt else Incomparable
        | o -> o

  let profiler = HExtlib.profile ~enable:true "compare_terms(rpo)"
  let compare_terms ~so x y =
    profiler.HExtlib.profile (rpo ~so x) y
end

(** hopefully more efficient (polynomial) implementation of LPO,
    following the paper "things to know when implementing LPO" by Löchner.
    We adapt here the implementation clpo6 with some multiset symbols (=) *)
module RPO6 = struct
  let name = "rpo6"

  (** recursive path ordering *)
  let rec rpo6 ~so s t =
    if T.eq_term s t then Eq else  (* equality test is cheap *)
    match s.term, t.term with
    | Var _, Var _ -> Incomparable
    | _, Var _ -> if T.var_occurs t s then Gt else Incomparable
    | Var _, _ -> if T.var_occurs s t then Lt else Incomparable
    | _ -> begin
      match decompose s, decompose t with
       | (f, []), (g, []) ->
          (match so#compare f g with
           | n when n < 0 -> Lt
           | n when n > 0 -> Gt
           | _ -> Eq)
       | (f, ss), (g, ts) ->
          (match so#compare f g with
          | 0 when so#multiset_status f ->
            cMultiset ~so ss ts (* multiset subterm comparison *)
          | 0 ->
            cLMA ~so s t ss ts  (* lexicographic subterm comparison *)
          | n when n > 0 -> cMA ~so s ts
          | n when n < 0 -> Utils.not_partial (cMA ~so t ss)
          | _ -> assert false   (* match exhaustively *)
          )
      end
  (** try to dominate all the terms in ts by s; but by subterm property
      if some t' in ts is >= s then s < t=g(ts) *)
  and cMA ~so s ts = match ts with
    |[] -> Gt
    | t::ts' ->
      (match rpo6 ~so s t with
      | Gt -> cMA ~so s ts'
      | Eq | Lt -> Lt
      | Incomparable -> Utils.not_partial (alpha ~so ts' s))
  (** lexicographic comparison of s=f(ss), and t=f(ts) *)
  and cLMA ~so s t ss ts = match ss, ts with
    | si::ss', ti::ts' ->
      (match rpo6 ~so si ti with
        | Eq -> cLMA ~so s t ss' ts'
        | Gt -> cMA ~so s ts' (* just need s to dominate the remaining elements *)
        | Lt -> Utils.not_partial (cMA ~so t ss')
        | Incomparable -> cAA ~so s t ss' ts'
      )
    | [], [] -> assert false (* should be equal?! *)
    | _ -> assert false (* different length... *)
  (** multiset comparison of subterms (not optimized) *)
  and cMultiset ~so ss ts = Utils.multiset_partial (rpo6 ~so) ss ts
  (** bidirectional comparison by subterm property (bidirectional alpha) *)
  and cAA ~so s t ss ts =
    match alpha ~so ss t with
    | Gt -> Gt
    | Incomparable -> Utils.not_partial (alpha ~so ts s)
    | _ -> assert false
  (** if some s in ss is >= t, then s > t by subterm property and transitivity *)
  and alpha ~so ss t = match ss with
    | [] -> Incomparable
    | s::ss' ->
      (match rpo6 ~so s t with
       | Eq | Gt -> Gt
       | Incomparable | Lt -> alpha ~so ss' t)

  let profiler = HExtlib.profile ~enable:true "compare_terms(rpo6)"
  let compare_terms ~so x y =
    profiler.HExtlib.profile (rpo6 ~so x) y
end

(* ----------------------------------------------------------------------
 class interface
 ---------------------------------------------------------------------- *)

(** cache for pairs of terms *)
module OrdCache = Cache.Make(
  struct
    type t = (term * term)
    let equal (x1,y1) (x2,y2) = T.eq_term x1 x2 && T.eq_term y1 y2
    let hash (x,y) = (Utils.murmur_hash x.hkey) lxor y.hkey
    let should_cache (x,y) = match x.term, y.term with
    | Node _, Node _ -> true  (* cache for complex terms *)
    | _ -> false
  end)

(* TODO use cache in some recursive calls in LPO? *)

class kbo (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 29
    val so = so
    method refresh () = (OrdCache.clear cache; so#refresh ())
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.with_cache cache
      (fun (a, b) -> KBO.compare_terms ~so a b) (a, b)
    method compute_term_weight t = weight_of_term ~so t
    method compute_clause_weight c = compute_clause_weight ~so c
    method name = KBO.name
  end

class rpo (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 29
    val so = so
    method refresh () = OrdCache.clear cache; so#refresh ()
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.with_cache cache
      (fun (a, b) -> RPO.compare_terms ~so a b) (a, b)
    method compute_term_weight t = weight_of_term ~so t
    method compute_clause_weight c = compute_clause_weight ~so c
    method name = RPO.name
  end

class rpo6 (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 29
    val so = so
    method refresh () = OrdCache.clear cache; so#refresh ()
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.with_cache cache
      (fun (a, b) -> RPO6.compare_terms ~so a b) (a, b)
    method compute_term_weight t = weight_of_term ~so t
    method compute_clause_weight c = compute_clause_weight ~so c
    method name = RPO6.name
  end

let default_ordering () = new rpo6 (default_symbol_ordering ())

let dummy_ordering =
  let so = ref (default_symbol_ordering ()) in
  object
    method refresh () = !so#refresh ()
    method clear_cache () = ()
    method symbol_ordering = !so
    method compare a b = Incomparable
    method compute_term_weight t = weight_of_term ~so:!so t
    method compute_clause_weight c =
      compute_clause_weight ~so:!so c
    method name = "dummy"
  end
