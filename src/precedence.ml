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

open Basic
open Symbols

module T = Terms
module C = Clauses
module Utils = FoUtils
module PO = PartialOrder

(** Precedence on symbols *)

(* ----------------------------------------------------------------------
 * hard constraints on the ordering
 * ---------------------------------------------------------------------- *)

let cluster_constraint clusters =
  let table = SHashtbl.create 5
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

(** build a hashtable from the given ordering *)
let mk_table symbols =
  let table = SHashtbl.create 5 in
  let _ = List.fold_left (fun i s -> SHashtbl.add table s i; i+1) 0 symbols
  in table

let list_constraint l =
  let table = mk_table l in
  (* compare symbols by number. Smaller symbols have bigger number *)
  fun a b ->
    try let na = SHashtbl.find table a
        and nb = SHashtbl.find table b in
        nb - na
    with Not_found -> 0

let arity_constraint signature s1 s2 =
  try
    let s1sort = SMap.find s1 signature
    and s2sort = SMap.find s2 signature in
    (arity s1sort) - (arity s2sort)  (* bigger arity means bigger symbol *)
  with Not_found -> 0

let invfreq_constraint clauses =
  let freq_table = SHashtbl.create 5 in
  (* frequency of symbols in clause *)
  let rec clause_freq hc = Array.iter lit_freq hc.hclits
  and lit_freq = function | Equation (l,r,_,_) -> term_freq l; term_freq r
  and term_freq t = match t.term with
    | Var _ | BoundVar _ -> ()
    | Bind (_, _, t') ->
      term_freq t'  (* do not bother with (special) binder symbols anyway *)
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
  let table = SHashtbl.create 5
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
let weight_modarity signature a =
  try fst (SMap.find a signature) + 4
  with Not_found -> 4

(** constant weight *)
let weight_constant a = 4

(* ----------------------------------------------------------------------
 * Creation of a precedence (symbol_ordering) from constraints
 * ---------------------------------------------------------------------- *)

(** Add the special symbols to the list *)
let complete_symbols symbols = 
  Utils.list_union (==) symbols (symbols_of_signature base_signature)

(** Order the list of symbols using the constraints *)
let order_symbols constrs symbols =
  let symbols = List.fold_right
    (fun constr symbols -> List.stable_sort constr symbols) constrs symbols
  in List.rev symbols  (* decreasing order *)

(** build a precedence on the [symbols] from a list of constraints *)
let mk_precedence ?(complete=true) constrs symbols =
  let symbols = if complete then complete_symbols symbols else symbols in
  let symbols = order_symbols constrs symbols in
  let symbols = ref symbols in
  let table = ref (mk_table !symbols) in
  let weight = ref weight_constant in
  (* the precedence *)
  let obj = object (self)
    method snapshot = !symbols

    (** Add the given symbols to the precedence. Returns how many of them
        are new and have effectively been added *)
    method add_symbols new_symbols =
      let old_len = List.length !symbols in
      let all_symbols = Utils.list_union (==) new_symbols !symbols in
      let new_len = List.length all_symbols in
      if new_len > old_len then begin
        (* some symbols have been added *)
        Utils.debug 3 "%% add @[<h>%a@] to the precedence"
                      (Utils.pp_list ~sep:", " pp_symbol) new_symbols;
        Utils.debug 3 "%% old precedence %a"
                       pp_precedence !symbols;

        (* build a partial order that respects the current ordering *)
        let po = PartialOrder.mk_partial_order all_symbols in
        PartialOrder.complete po (list_constraint !symbols);
        (* complete it with the constraints *)
        List.iter (fun constr -> PartialOrder.complete po constr) constrs;
        assert (PartialOrder.is_total po);
        (* get the new precedence from the completed partial order *)
        let all_symbols = PartialOrder.symbols po in
        symbols := all_symbols;
        table := mk_table !symbols;

        Utils.debug 3 "%% new precedence %a" pp_precedence !symbols;
        (* return number of new symbols *)
        new_len - old_len
      end else 0

    (** To compare symbols, compare their index in the decreasing precedence. Symbols that
        are split symbols are compared to other symbols like "split_symbol". *)
    method compare a b =
      (* some symbols are not explicitely in the signature. Instead, they
         are represented by 'generic' symbols *)
      let transform_symbol s = match s with
        | _ when has_attr attr_split s -> split_symbol
        | _ when has_attr attr_fresh_const s -> const_symbol
        | _ -> s
      in
      let a' = transform_symbol a
      and b' = transform_symbol b in
      if a' == b' && a != b
        then (* both are in the same symbol family (e.g. split symbols). Any
                arbitrary but total ordering on them is ok, as long as it's stable. *)
          Symbols.compare_symbols a b
        else SHashtbl.find !table b' - SHashtbl.find !table a'

    method weight s = !weight s

    method set_weight f = weight := f
  end
  in
  (obj :> precedence)

let rec default_precedence signature =
  (* two constraints: false, true at end of precedence, and arity constraint *)
  let constrs =
    [min_constraint [false_symbol; true_symbol];
     arity_constraint signature;
     alpha_constraint] in
  mk_precedence constrs (symbols_of_signature signature)

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
  | Var _ | BoundVar _ -> acc
  | Bind (f, _, t') -> 
    let acc' = if List.exists ((==) f) acc then acc else f::acc in
    term_symbols acc' t'
  | Node (f, ts) ->
    let acc' = if List.exists ((==) f) acc then acc else f::acc in
    List.fold_left term_symbols acc' ts

(** create a constraint that a > b holds in the given ordering *)
let check_gt ~weight a b =
  (weight, term_symbols (term_symbols [] a) b, fun ord -> ord#compare a b = Gt)

let weight_def = 5        (** weight of definitions *)
let weight_rewrite = 2    (** weight of rewriting rule *)
let weight_const_def = 1  (** weight of definitions of constants *)

(** Creates a weighted constraint if the clause is a symbol definition,
    ie an equation/equivalence f(x1,...,xn)=b where f does not occur in b *)
let check_definition clause =
  match C.is_definition clause with
  | Some (l,r) -> (* definition of l by r *)
    Utils.debug 0 "%% @[<h>definition: %a == %a@]" !T.pp_term#pp l !T.pp_term#pp r;
    [check_gt ~weight:weight_def l r]
  | None -> []

let check_rules clause =
  (* otherwise, try to interpret the clause as a rewrite rule. It only provides
     a constraint if the rule is not trivially oriented by the subterm property. *)
  let rules = C.is_rewrite_rule clause in
  match rules with
  | [l, r] when not (T.member_term r l) ->
    Utils.debug 0 "%% @[<h>rewrite rule: %a --> %a@]" !T.pp_term#pp l !T.pp_term#pp r;
    [check_gt ~weight:weight_rewrite l r]
  | _ -> []  (* not unambiguously a rewrite rule *)

let check_const_def clause =
  match C.is_const_definition clause with
  | None -> []
  | Some (const, definition) ->
    Utils.debug 0 "%% @[<h>definition of constant: %a --> %a@]"
                !T.pp_term#pp const !T.pp_term#pp definition;
    [check_gt ~weight:weight_const_def const definition]

(** Create the constraints for a single clause *)
let create_constraints clause = check_definition clause @ check_rules clause

(* ----------------------------------------------------------------------
 * Heuristic creation of precedences (satisfying maximal number of constraints)
 * ---------------------------------------------------------------------- *)

(** Check whether the two precedences are equal *)
let eq_precedence c1 c2 =
  assert (List.length c1#snapshot = List.length c2#snapshot);
  List.for_all2 (==) c1#snapshot c2#snapshot

(** Compute a precedence from the signature and the strong constraint *)
let compute_precedence signature weak_constrs strong_constrs symbols : precedence =
  let sig_constraint = list_constraint symbols in
  let constrs = strong_constrs @ [sig_constraint] @ weak_constrs in
  mk_precedence constrs signature

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
  Utils.debug 4 "perturbate @[<h>[%a]@]" (Utils.pp_list pp_symbol) symbols;
  let new_symbols = ref [] in
  (* swap indexes i and j in the list *)
  let rec swap i j a =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp;
  (* perform n swaps on the array *)
  and swap_n n a =
    if n = 0 || Array.length a <= 1 then a else begin
      let i = max 1 (Random.int (Array.length a - 1)) in
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
    Utils.debug 2 "> on the hill with cost %d" cost;
    (* perturbate current precedence *)
    let new_symbols_list = perturbate symbols in
    (* find which new precedence has minimal cost *)
    let min_cost, min_symbols =
      List.fold_left
        (fun (min_cost, min_symbols) symbols' ->
          let precedence' = mk_precedence symbols' in
          Utils.debug 3 "try precedence @[<h>%a@]" pp_precedence precedence'#snapshot;
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
  let signature = C.signature clauses in
  (* the constraints *)
  let constraints = Utils.list_flatmap create_constraints clauses in
  let max_cost = List.fold_left (fun acc (w,_,_) -> acc+w) 0 constraints in
  (* the list of symbols to heuristically order *)
  let symbols = List.fold_left (fun acc (_,symbols,_) -> symbols @ acc) [] constraints in
  let symbols = Utils.list_uniq (==) symbols in
  (* helper functions *)
  let mk_precedence = compute_precedence (symbols_of_signature signature)
    weak_constrs strong_constrs
  and mk_cost = compute_cost ord_factory constraints in
  (* Randomized hill climbing on the ordering of symbols. The result is
     the precedence that has the lowest cost. *)
  let rec climb_hills ~num symbols precedence cost =
    if num = 5 || cost = 0
      then begin
        Utils.debug 0 "%% found precedence after %d attempts, cost %d / %d"
                       num cost max_cost;
        precedence  (* done enough restarts *)
      end else begin
        let symbols' = Utils.list_shuffle symbols in
        Utils.debug 1 "%% >>> restart hill climbing";
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
