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
module C = Clauses
module Utils = FoUtils


(* ----------------------------------------------------------------------
 * module interface for orderings, internal (used to create the different classes)
 * ---------------------------------------------------------------------- *)

module type S =
  sig
    (* This order relation should be:
     * - stable for instantiation
     * - monotonic
     * - total on ground terms *)
    val compare_terms : so:symbol_ordering -> term -> term -> comparison

    val name : string
  end

(* Riazanov: p. 40, relation >>>
 * if head_only=true then it is not >>> but helps case 2 of 3.14 p 39 *)
let rec aux_ordering b_compare ?(head_only=false) t1 t2 =
  match t1.term, t2.term with
  (* We want to discard any identity equality. *
   * If we give back Eq, no inference rule    *
   * will be applied on this equality          *)
  | Var i, Var j when i = j -> Eq
  (* 1. *)
  | Var _, _
  | _, Var _ -> Incomparable
  (* 2 *)
  | Node (a1, l1), Node (a2, l2) ->
    let cmp = b_compare a1 a2 in
    if cmp < 0 then Lt else if cmp > 0 then Gt else
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

  let fun_weight = 2  (* var weight = 1 *)

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
          then (add_pos_var balance x; (wb + 1, x = y))
          else (add_neg_var balance x; (wb - 1, x = y))
      | Node (s, l) ->
        let wb' = if pos then wb + fun_weight else wb - fun_weight in
        balance_weight_rec wb' l y pos false
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
        (wb' + 1, if contains then Lt else Incomparable)
      |  _, Var y -> 
        add_neg_var balance y;
        let wb', contains = balance_weight wb t1 y true in
        (wb' - 1, if contains then Gt else Incomparable)
      | Node (f, ss), Node (g, ts) ->
        (* do the recursive computation of kbo *)
        let wb', recursive = tckbo_rec wb f g ss ts in
        let wb'' = wb' in
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
    | Node (hd1, tl1), Node (hd2, tl2) ->
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
            match so#compare hd1 hd2 with
            | n when n > 0 -> if check_subterms s (r_ol,tl2) then Gt else Incomparable
            | n when n < 0 -> if check_subterms t (l_ol,tl1) then Lt else Incomparable
            | _ -> rpo_rec ~so hd1 tl1 tl2 s t
          end
  (* recursive comparison of lists of terms (head symbol is hd) *)
  and rpo_rec ~so hd l1 l2 s t =
    (if so#multiset_status hd
    then Utils.multiset_partial (rpo ~so) l1 l2
    else match Utils.lexicograph_partial (rpo ~so) l1 l2 with
      | Gt ->
        if List.for_all (fun x -> rpo ~so s x = Gt) l2 then Gt else Incomparable
      | Lt ->
        if List.for_all (fun x -> rpo ~so x t = Lt) l1 then Lt else Incomparable
      | o -> o)

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
    | Node (f, []), Node (g, []) ->
      (match so#compare f g with
       | n when n < 0 -> Lt
       | n when n > 0 -> Gt
       | _ -> Eq)
    | Node (f, ss), Node (g, ts) ->
      (match so#compare f g with
      | 0 when so#multiset_status f ->
        cMultiset ~so ss ts (* multiset subterm comparison *)
      | 0 ->
        cLMA ~so s t ss ts  (* lexicographic subterm comparison *)
      | n when n > 0 -> cMA ~so s ts
      | n when n < 0 -> Utils.not_partial (cMA ~so t ss)
      | _ -> assert false)  (* match exhaustively *)
  (** try to dominate all the terms in ts by s; but by subterm property
      if some t' in ts is >= s then s < t=g(ts) *)
  and cMA ~so s ts = match ts with
    | [] -> Gt
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
    | [], [] -> Eq
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
 * class interface
 * ---------------------------------------------------------------------- *)

module OrdCache = Cache.Make(
  struct
    type t = term
    let hash t = t.hkey
      (* non commutative to avoid collision between (t1, t2) and (t2, t1) *)
    let equal t1 t2 = T.eq_term t1 t2
  end)

class kbo (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 4096 (KBO.compare_terms ~so)
    val so = so
    method refresh () = (OrdCache.clear cache; so#refresh ())
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.lookup cache a b
    method name = KBO.name
  end

class rpo (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 4096 (RPO.compare_terms ~so)
    val so = so
    method refresh () = (OrdCache.clear cache; so#refresh ())
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.lookup cache a b
    method name = RPO.name
  end

class rpo6 (so : symbol_ordering) : ordering =
  object
    val cache = OrdCache.create 4096 (RPO6.compare_terms ~so)
    val so = so
    method refresh () = (OrdCache.clear cache; so#refresh ())
    method clear_cache () = OrdCache.clear cache
    method symbol_ordering = so
    method compare a b = OrdCache.lookup cache a b
    method name = RPO6.name
  end

let default_ordering () = new rpo6 (Precedence.default_symbol_ordering ())
