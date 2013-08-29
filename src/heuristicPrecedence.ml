(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Heuristic precedence} *)

open Logtk
open Comparison.Infix
open Sequence.Infix

module T = Term
module C = Clause
module O = Ordering
module Lit = Literal

(** a weighted constraint is a weight (cost),
    a list of symbols to order,
    and a function to check if it's satisfied *)
type weighted_constr = int * Symbol.t list * (Ordering.t -> bool)

(** find all symbols of the term *)
let rec term_symbols acc t =
  match t.T.term with
  | T.Var _ | T.BoundVar _ -> acc
  | T.Bind (f, t') -> 
    let acc' = if List.exists ((==) f) acc then acc else f::acc in
    term_symbols acc' t'
  | T.Node (f, ts) ->
    let acc' = if List.exists ((==) f) acc then acc else f::acc in
    List.fold_left term_symbols acc' ts
  | T.At (t1, t2) ->
    term_symbols (term_symbols acc t1) t2

(** create a constraint that a > b holds in the given ordering *)
let check_gt ~weight a b =
  (weight, term_symbols (term_symbols [] a) b,
    fun ord -> O.compare ord a b = Gt)

let weight_def = 5        (** weight of definitions *)
let weight_rewrite = 2    (** weight of rewriting rule *)
let weight_const_def = 1  (** weight of definitions of constants *)

(** Creates a weighted constraint if the clause is a symbol definition,
    ie an equation/equivalence f(x1,...,xn)=b where f does not occur in b *)
let check_definition clause =
  match Lit.is_definition clause.C.hclits with
  | Some (l,r) -> (* definition of l by r *)
    Util.debug 0 "definition: %a == %a" T.pp l T.pp r;
    [check_gt ~weight:weight_def l r]
  | None -> []

let check_rules clause =
  (* otherwise, try to interpret the clause as a rewrite rule. It only provides
     a constraint if the rule is not trivially oriented by the subterm property. *)
  let rules = Lit.is_rewrite_rule clause.C.hclits in
  match rules with
  | [l, r] when not (T.subterm ~sub:r l) ->
    Util.debug 0 "rewrite rule: %a --> %a" T.pp l T.pp r;
    [check_gt ~weight:weight_rewrite l r]
  | _ -> []  (* not unambiguously a rewrite rule *)

let check_const_def clause =
  match Lit.is_const_definition clause.C.hclits with
  | None -> []
  | Some (const, definition) ->
    Util.debug 0 "definition of constant: %a --> %a" T.pp const T.pp definition;
    [check_gt ~weight:weight_const_def const definition]

(** Create the constraints for a single clause *)
let create_constraints clause = check_definition clause @ check_rules clause

(* Compute a precedence from the signature and the strong constraint *)
let compute_precedence signature weak_constrs strong_constrs symbols =
  let sig_constraint = Precedence.list_constraint symbols in
  let constrs = strong_constrs @ [sig_constraint] @ weak_constrs in
  Precedence.create constrs signature

(* Compute the cost for the precedence, given a list of constraints
    and a way to build a term ordering *)
let compute_cost ord_factory constraints precedence : int =
  let ord = ord_factory precedence in
  (* sum up weights of unsatisfied constraints *)
  let cost = List.fold_left
    (fun cost (w, _, constr) -> if constr ord then cost else cost + w)
    0 constraints
  in
  cost

(* Shuffle the symbols by reversing some symbols (uses random).
    It returns at most [num] modifications of the list, that differ from
    it by swapping pairs of elements) *)
let perturbate ?(num=10) symbols =
  Util.debug 4 "perturbate [%a]" (Util.pp_list Symbol.pp) symbols;
  let new_symbols = ref [] in
  (* generate the [num] perturbations *)
  for i = 0 to num-1 do
    let symbols' = Util.list_shuffle symbols in
    new_symbols := symbols' :: !new_symbols
  done;
  !new_symbols

(* Hill climbing, on the given list of constraints, for at most the
    given number of steps. It shuffles the signature to try to find
    one that satisfies more constraints.
    See http://en.wikipedia.org/wiki/Hill_climbing *)
let hill_climb ~steps mk_precedence mk_cost symbols =
  (* main loop to follow gradient. Current state is precedence, with cost cost *)
  let rec follow_gradient ~steps precedence cost =
    if steps = 0 || cost = 0 then precedence, cost else begin (* done *)
    Util.debug 2 "> on the hill with cost %d" cost;
    (* perturbate current precedence *)
    let new_symbols_list = perturbate symbols in
    (* find which new precedence has minimal cost *)
    let min_cost, min_symbols =
      List.fold_left
        (fun (min_cost, min_symbols) symbols' ->
          let precedence' = mk_precedence symbols' in
          Util.debug 3 "try precedence %a" Precedence.pp precedence';
          (* only compare with new precedence if it is not the same *)
          if Precedence.eq precedence precedence' then (min_cost, min_symbols)
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

let compute ?(signature=Signature.empty)
ord_factory weak_constrs strong_constrs clauses =
  let signature = Signature.merge signature (C.signature clauses) in
  (* the constraints *)
  let constraints =
    Sequence.(map create_constraints clauses
      |> map of_list
      |> flatten
      |> to_list)
  in
  let max_cost = List.fold_left (fun acc (w,_,_) -> acc+w) 0 constraints in
  (* the list of symbols to heuristically order *)
  let symbols = List.fold_left (fun acc (_,symbols,_) -> symbols @ acc) [] constraints in
  let symbols = Util.list_uniq (==) symbols in
  (* helper functions *)
  let mk_precedence = compute_precedence (Signature.to_symbols signature)
    weak_constrs strong_constrs
  and mk_cost = compute_cost ord_factory constraints in
  (* Randomized hill climbing on the ordering of symbols. The result is
     the precedence that has the lowest cost. *)
  let rec climb_hills ~num symbols precedence cost =
    if num = 5 || cost = 0
      then begin
        Util.debug 0 "found precedence after %d attempts, cost %d / %d"
                       num cost max_cost;
        precedence  (* done enough restarts *)
      end else begin
        let symbols' = Util.list_shuffle symbols in
        Util.debug 1 ">>> restart hill climbing";
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
