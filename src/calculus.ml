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

(** Some common things for superposition calculi *)

open Types
open Symbols

module T = Terms
module C = Clauses
module S = FoSubst
module Utils = FoUtils
module PS = ProofState

(** binary inferences. An inference returns a list of conclusions *)
type binary_inf_rule = ProofState.active_set -> clause -> hclause list

(** unary infererences *)
type unary_inf_rule = ord:ordering -> hclause -> hclause list

(** The type of a calculus for first order reasoning with equality *) 
class type calculus =
  object
    method binary_rules : (string * binary_inf_rule) list
      (** the binary inference rules *)

    method unary_rules : (string * unary_inf_rule) list
      (** the unary inference rules *)

    method basic_simplify : ord:ordering -> hclause -> hclause
      (** how to simplify a clause *)

    method rw_simplify : select:selection_fun -> ProofState.simpl_set -> hclause -> hclause
      (** how to simplify a clause w.r.t a set of unit clauses *)

    method active_simplify : select:selection_fun -> ProofState.active_set -> hclause -> hclause
      (** how to simplify a clause w.r.t an active set of clauses *)

    method backward_simplify : ProofState.active_set -> hclause -> Clauses.CSet.t
      (** backward simplification by a unit clause. It returns a set of
          active clauses that can potentially be simplified by the given clause *)

    method redundant : ProofState.active_set -> hclause -> bool
      (** check whether the clause is redundant w.r.t the set *)

    method backward_redundant : ProofState.active_set -> hclause -> hclause list
      (** find redundant clauses in set w.r.t the clause *)

    method list_simplify : ord:ordering -> select:selection_fun -> hclause -> hclause list
      (** how to simplify a clause into a (possibly empty) list
          of clauses. This subsumes the notion of trivial clauses (that
          are simplified into the empty list of clauses) *)

    method is_trivial : hclause -> bool
      (** single test to detect trivial clauses *)

    method axioms : hclause list
      (** a list of axioms to add to the problem *)

    method constr : hclause list -> ordering_constraint list
      (** some constraints on the precedence *)

    method preprocess : ord:ordering -> select:selection_fun -> hclause list -> hclause list
      (** how to preprocess the initial list of clauses *)
  end

(** do binary inferences that involve the given clause *)
let do_binary_inferences active_set rules hc =
  (* relocate clause *)
  let c = active_set#relocate hc in
  Utils.debug 3 (lazy (Utils.sprintf "do binary inferences with current active set: %a"
                       C.pp_set active_set#clauses));
  (* apply every inference rule *)
  List.fold_left
    (fun acc (name, rule) ->
      Utils.debug 3 (lazy ("#  apply binary rule " ^ name));
      let new_clauses = rule active_set c in
      List.rev_append new_clauses acc)
    [] rules

(** do unary inferences for the given clause *)
let do_unary_inferences ~ord rules hc =
  Utils.debug 3 (lazy "do unary inferences");
  (* apply every inference rule *)
  List.fold_left
    (fun acc (name, rule) ->
      Utils.debug 3 (lazy ("#  apply unary rule " ^ name));
      let new_clauses = rule ~ord hc in
      List.rev_append new_clauses acc)
    [] rules

(** fold f over all literals sides, with their positions.
    f is given (acc, left side, right side, sign, position of left side)
    if both=true, then both sides of a non-oriented equation
      will be visited

    ?pos:bool -> ?neg:bool -> ?both:bool
    -> ('a -> Types.term -> Types.term -> bool -> int list -> 'a)
    -> 'a -> (Types.literal * int) list
    -> 'a *)
let fold_lits ?(both=true) eligible f acc lits =
  let rec fold acc i =
    if i = Array.length lits then acc
    else if not (eligible i lits.(i)) then fold acc (i+1)
    else
      let acc = match lits.(i) with
      | Equation (l,r,sign,Gt) ->
        f acc l r sign [i; C.left_pos]
      | Equation (l,r,sign,Lt) ->
        f acc r l sign [i; C.right_pos]
      | Equation (l,r,sign,_) ->
        if both
        then (* visit both sides of the equation *)
          let acc = f acc r l sign [i; C.right_pos] in
          f acc l r sign [i; C.left_pos]
        else (* only visit one side (arbitrary) *)
          f acc l r sign [i; C.left_pos]
      in fold acc (i+1)
  in fold acc 0

(** decompose the literal at given position *)
let get_equations_sides c pos = match pos with
  | idx::eq_side::[] ->
    (match c.clits.(idx) with
    | Equation (l,r,sign,_) when eq_side = C.left_pos -> (l, r, sign)
    | Equation (l,r,sign,_) when eq_side = C.right_pos -> (r, l, sign)
    | _ -> invalid_arg "wrong side")
  | _ -> invalid_arg "wrong kind of position (list of >= 2 elements)"

(** Perform backward simplification with a given clause (belonging to simpl_set).
    It uses the simpl_set to simplify the candidate clauses
    that match a maximal side of the given clause (simpler, though not optimal) *)
let backward_simplify ~select ~calculus active_set simpl_set given =
  (* set of candidate clauses, that may be unit-simplifiable *)
  let candidates = calculus#backward_simplify active_set given in
  (* try to simplify the candidates. Before is the set of clauses that
     are simplified, after is the list of those clauses after simplification *)
  let before, after =
    C.CSet.fold
      (fun (before, after) _ hc ->
        let hc' = calculus#rw_simplify ~select simpl_set hc in
        if not (C.eq_hclause hc hc')
          (* the active clause has been simplified! *)
          then begin
            Utils.debug 2 (lazy (Utils.sprintf
                           "@[<hov 4>active clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                           !C.pp_clause#pp_h hc !C.pp_clause#pp_h hc'));
            C.CSet.add before hc, hc' :: after
          end else before, after)
    (C.CSet.empty, []) candidates
  in
  before, after

