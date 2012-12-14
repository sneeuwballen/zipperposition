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
module PO = PartialOrder

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

let current_signature =
  (* version of signature that is computed *)
  let sig_version = ref 0 in
  (* store the signature, to avoid recomputing it all the time *)
  let cached_signature = ref (compute_signature ()) in
  fun () ->
    assert (!sig_version <= !Symbols.sig_version);
    (if !sig_version < !Symbols.sig_version
      then (sig_version := !Symbols.sig_version; (* recompute signature, it did change *)
            cached_signature := compute_signature ()));
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

let arity_constraint s1 s2 =
  let _, arities, _ = current_signature () in
  let s1_arity = SHashtbl.find arities s1
  and s2_arity = SHashtbl.find arities s2 in
  s1_arity - s2_arity  (* bigger arity means bigger symbol *)

let invfreq_constraint clauses =
  let freq_table = SHashtbl.create 5 in
  (* frequency of symbols in clause *)
  let rec clause_freq hc = Array.iter lit_freq hc.hclits
  and lit_freq = function | Equation (l,r,_,_) -> term_freq l; term_freq r
  and term_freq t = match t.term with
    | Var _ -> ()
    | Node (s,l) ->
      (let count = try SHashtbl.find freq_table s with Not_found -> 0 in
      SHashtbl.replace freq_table s (count+1);
      List.iter term_freq l)
  in
  List.iter clause_freq clauses;
  (* compare by inverse frequency (higher frequency => smaller) *)
  fun s1 s2 ->
    let freq1 = try SHashtbl.find freq_table s1 with Not_found -> 0
    and freq2 = try SHashtbl.find freq_table s2 with Not_found -> 0 in
    freq2 - freq1

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
  fun a b ->
    (* not found implies the symbol is bigger than minimal symbols *)
    let a_n = try SHashtbl.find table a with Not_found -> -1
    and b_n = try SHashtbl.find table b with Not_found -> -1 in
    b_n - a_n  (* if a > b then a_n < b_n *)

(* regular string ordering *)
let alpha_constraint a b = compare_symbols a b

(* ----------------------------------------------------------------------
 * Weight function
 * ---------------------------------------------------------------------- *)

(** weight of f = arity of f + 4 *)
let weight_modarity () =
  let _, arities, _ = current_signature () in
  fun a -> (SHashtbl.find arities a) + 4

(* ----------------------------------------------------------------------
 * Creation of a precedence (symbol_ordering) from constraints
 * ---------------------------------------------------------------------- *)

(** build history by patching old signature to get the new one *)
let compute_history old_sig new_sig history =
  let rec compare ?prev old_sig new_sig history =
    match old_sig, new_sig with
    | [], [] -> history
    | [], [s] -> (* s is the new bottom *)
      let patch = Between (prev, s, None) in patch::history
    | [], s::s'::new_sig' ->  (* s between prev and s' *)
      let patch = Between (prev, s, Some s') in
      compare ~prev:s old_sig (s'::new_sig') (patch::history)
    | s::old_sig', s'::new_sig' when s == s' ->
      compare ~prev:s old_sig' new_sig' history
    | s::old_sig', s'::s''::new_sig' ->
      (* trickiest case: first deal with the remaining new symbols,
         then insert s' *)
      let history' = compare ~prev:s old_sig (s''::new_sig') history in
      (Between (prev, s', Some s'')) :: history'
    | _::_, [_] | _::_, [] -> assert false  (* not monotonic increase? *)
  in compare old_sig new_sig history

(* build an ordering from a list of constraints *)
let make_ordering constrs symbols =
  (* add default symbols *)
  let _,_,base_symbols = base_signature () in
  let symbols = List.rev_append base_symbols symbols in
  (* create a partial order, and complete it *)
  let po = PartialOrder.mk_partial_order symbols in
  let weight = ref (weight_modarity ()) in
  let apply_constrs () = List.iter (PartialOrder.complete po) constrs in
  let multiset_pred = ref (fun s -> s == eq_symbol) in
  (* the object itself *)
  let obj = object (self)
    val mutable m_version = 0
    val mutable m_history = []
    method version = m_version
    method history = m_history
    (* refresh computes a new ordering based on the current signature *)
    method refresh () =
      let old_signature = self#signature in

      (* the new signature, possibly with more symbols*)
      let _, _, symbols = current_signature () in
      let num_added = PartialOrder.extend po symbols in
      if num_added > 0 then begin
        Utils.debug 3 (lazy (Utils.sprintf "%% old signature %a" T.pp_signature self#signature));
        (* complete the ordering with successive constraints *)
        apply_constrs ();
        assert (PartialOrder.is_total po);
        (* new version of the precedence *)
        m_version <- m_version + 1;
        m_history <- compute_history old_signature self#signature m_history;
        (* refresh weight function *)
        weight := weight_modarity ();
        Utils.debug 3 (lazy (Utils.sprintf "%% new signature %a" T.pp_signature self#signature));
      end
    method weight s = !weight s
    method signature = PartialOrder.signature po
    method compare a b = PartialOrder.compare po a b
    method multiset_status s = !multiset_pred s
    method set_multiset f = multiset_pred := f
  end
  in
  (* do the initial computation and return the object *)
  apply_constrs ();
  obj#refresh ();
  (obj :> symbol_ordering)

let rec default_symbol_ordering symbols =
  (* two constraints: false, true at end of precedence, and arity constraint *)
  let constrs =
    [min_constraint [false_symbol; true_symbol];
     arity_constraint;
     alpha_constraint] in
  (* apply the constraints to the dummy symbol ordering *)
  make_ordering constrs symbols

(* ----------------------------------------------------------------------
 * Heuristic constraints to try to reduce search space
 * ---------------------------------------------------------------------- *)

(** a weighted constraint is a weight (cost),
    a list of symbols to order,
    and a function to check if it's satisfied *)
type weighted_constr = int * symbol list * (ordering -> bool)

(** find all symbols of the term *)
let rec term_symbols acc t =
  match t.term with
  | Var _ -> acc
  | Node (f, ts) ->
    let acc' = if List.exists ((==) f) acc then acc else f::acc in
    List.fold_left term_symbols acc' ts

(** create a constraint that a > b holds in the given ordering *)
let check_gt ~weight a b =
  (weight, term_symbols (term_symbols [] a) b, fun ord -> ord#compare a b = Gt)

let weight_def = 5      (** weight of definitions *)
let weight_rewrite = 2  (** weight of rewriting rule *)

(** Creates a weighted constraint if the clause is a symbol definition,
    ie an equation/equivalence f(x1,...,xn)=b where f does not occur in b *)
let check_definition clause =
  match Theories.is_definition clause with
  | Some (l,r) -> (* definition of l by r *)
    Utils.debug 0 (lazy (Utils.sprintf "%% @[<h>definition: %a == %a@]" !T.pp_term#pp l !T.pp_term#pp r));
    [check_gt ~weight:weight_def l r]
  | None -> []

let check_rules clause =
  (* otherwise, try to interpret the clause as a rewrite rule *)
  let rules = Theories.is_rewrite_rule clause in
  match rules with
  | [l, r] ->
    Utils.debug 0 (lazy (Utils.sprintf "%% @[<h>rewrite rule: %a --> %a@]" !T.pp_term#pp l !T.pp_term#pp r));
    [check_gt ~weight:weight_rewrite l r]
  | _ -> []  (* not unambiguously a rewrite rule *)

(** Create the constraints for a single clause *)
let create_constraints clause = check_definition clause @ check_rules clause

(* ----------------------------------------------------------------------
 * Heuristic creation of precedences (satisfying maximal number of constraints)
 * ---------------------------------------------------------------------- *)

(** Check whether the two precedences are equal *)
let eq_precedence c1 c2 =
  assert (List.length c1#signature = List.length c2#signature);
  List.for_all2 (==) c1#signature c2#signature

(** Compute a precedence from the signature and the strong constraint *)
let compute_precedence signature weak_constrs strong_constrs symbols : symbol_ordering =
  let sig_constraint = list_constraint symbols in
  let constrs = strong_constrs @ [sig_constraint] @ weak_constrs in
  make_ordering constrs signature

(** Compute the cost for the precedence, given a list of constraints
    and a way to build a term ordering *)
let compute_cost ord_factory constraints precedence : int =
  let ord = ord_factory precedence in
  (* sum up weights of unsatisfied constraints *)
  let cost = List.fold_left
    (fun cost (w, _, constr) -> if constr ord then cost else cost + w)
    0 constraints
  in
  cost

(** Shuffle the symbols by reversing some symbols (uses random).
    It returns at most [num] modifications of the list, that differ from
    it by swapping pairs of elements) *)
let perturbate ?(num=10) symbols =
  let new_symbols = ref [] in
  (* swap indexes i and j in the list *)
  let rec swap i j a = 
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp;
  (* perform n swaps on the array *)
  and swap_n n a =
    if n = 0 then a else begin
      let i = (Random.int (Array.length a - 2)) + 1 in
      let j = Random.int i in
      swap i j a;
      swap_n (n-1) a;
    end
  in
  (* generate the [num] perturbations *)
  for i = 0 to num-1 do
    let symbols' = Array.to_list (swap_n 3 (Array.of_list symbols)) in
    new_symbols := symbols' :: !new_symbols
  done;
  !new_symbols

(** Hill climbing, on the given list of constraints, for at most the
    given number of steps. It shuffles the signature to try to find
    one that satisfies more constraints.
    See http://en.wikipedia.org/wiki/Hill_climbing *)
let hill_climb ~steps mk_precedence mk_cost symbols =
  (* main loop to follow gradient. Current state is precedence, with cost cost *)
  let rec follow_gradient ~steps precedence cost =
    if steps = 0 || cost = 0 then precedence, cost else begin (* done *)
    Utils.debug 2 (lazy (Utils.sprintf "> on the hill with cost %d" cost));
    (* perturbate current precedence *)
    let new_symbols_list = perturbate symbols in
    (* find which new precedence has minimal cost *)
    let min_cost, min_symbols =
      List.fold_left
        (fun (min_cost, min_symbols) symbols' ->
          let precedence' = mk_precedence symbols' in
          (* only compare with new precedence if it is not the same *)
          if eq_precedence precedence precedence' then (min_cost, min_symbols)
          else
            let cost' = mk_cost precedence' in
            if cost' < min_cost
              then cost', symbols'  (* the new precedence is better *)
              else min_cost, min_symbols)
        (cost, symbols) new_symbols_list
    in
    (* follow gradient, unless we are at a (local) minimum *)
    if min_cost < cost
      then follow_gradient ~steps:(steps-1) (mk_precedence min_symbols) min_cost
      else precedence, cost  (* local optimum, stop there *)
    end
  in
  let precedence = mk_precedence symbols in
  let cost = mk_cost precedence in
  follow_gradient ~steps precedence cost

(** define a constraint on symbols that is believed to improve
    the search by enabling as many simplifications as possible. It takes
    an ordering as a parameter, to be able to decide the orientation of
    terms in a given precedence, and ordering constraints that are
    respectively weaker/stronger than the optimization (the first one, weaker,
    is applied to break ties, the second one, stronger, is always enforced first) *)
let heuristic_precedence ord_factory weak_constrs strong_constrs clauses =
  let _, _, signature = current_signature () in
  (* the constraints *)
  let constraints = Utils.list_flatmap create_constraints clauses in
  let max_cost = List.fold_left (fun acc (w,_,_) -> acc+w) 0 constraints in
  (* the list of symbols to heuristically order *)
  let symbols = List.fold_left (fun acc (_,symbols,_) -> symbols @ acc) [] constraints in
  let symbols = Utils.list_uniq (==) symbols in
  (* helper functions *)
  let mk_precedence = compute_precedence signature weak_constrs strong_constrs
  and mk_cost = compute_cost ord_factory constraints in
  (* Randomized hill climbing on the ordering of symbols. The result is
     the precedence that has the lowest cost. *)
  let rec climb_hills ~num symbols precedence cost =
    if num = 5 || cost = 0
      then begin
        Utils.debug 0 (lazy (Utils.sprintf "%% found precedence after %d attempts, cost %d / %d"
                       num cost max_cost));
        precedence  (* done enough restarts *)
      end else begin
        let symbols' = Utils.list_shuffle symbols in
        Utils.debug 1 (lazy ">>> restart hill climbing");
        let precedence', cost' = hill_climb ~steps:8 mk_precedence mk_cost symbols' in
        if cost' < cost
          then climb_hills ~num:(num+1) symbols' precedence' cost' (* choose new precedence *)
          else climb_hills ~num:(num+1) symbols precedence cost    (* continue with same (better) precedence *)
      end
  in
  let precedence = mk_precedence symbols in
  let precedence = climb_hills ~num:0 symbols precedence (mk_cost precedence) in
  (* yield the precedence *)
  precedence
