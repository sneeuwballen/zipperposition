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

module C = Clauses
module Utils = FoUtils
module PS = ProofState

(** binary inferences *)
type binary_inf_rule = PS.active_set -> clause -> clause list

(** unary infererences *)
type unary_inf_rule = clause -> clause list

(** The type of a calculus for first order reasoning with equality *) 
class type calculus =
  object
    (** the binary inference rules *)
    method binary_rules : (string * binary_inf_rule) list
    (** the unary inference rules *)
    method unary_rules : (string * unary_inf_rule) list
    (** how to simplify a clause *)
    method basic_simplify : clause -> clause
    (** how to simplify a clause w.r.t a set of clauses. Returns
        the (renamed) clause and the simplified clause. *)
    method simplify : ProofState.active_set -> clause -> clause * clause
    (** check whether the clause is redundant w.r.t the set *)
    method redundant : ProofState.active_set -> clause -> bool
    (** check whether the clause is trivial *)
    method trivial : clause -> bool
    (** a list of axioms to add to the Set of Support *)
    method axioms : clause list
    (** some constraints on the precedence *)
    method constr : ordering_constraint
    (** how to preprocess the initial list of clauses *)
    method preprocess : clause list -> clause list
  end

(** do binary inferences that involve the given clause *)
let do_binary_inferences active_set rules c =
  (* rename clause to avoid collisions *)
  let c = PS.relocate_active active_set c in
  Utils.debug 3 (lazy (Utils.sprintf "do binary inferences with current active: %a" C.pp_bag
                       active_set.PS.active_clauses));
  (* apply every inference rule *)
  List.fold_left
    (fun acc (name, rule) ->
      Utils.debug 3 (lazy ("#  apply binary rule " ^ name));
      let new_clauses = rule active_set c in
      List.rev_append new_clauses acc)
    [] rules

(** do unary inferences for the given clause *)
let do_unary_inferences rules c =
  Utils.debug 3 (lazy "do unary inferences");
  (* apply every inference rule *)
  List.fold_left
    (fun acc (name, rule) ->
      Utils.debug 3 (lazy ("#  apply unary rule " ^ name));
      let new_clauses = rule c in
      List.rev_append new_clauses acc)
    [] rules

(** fold f over all literals sides, with their positions.
    f is given (acc, left side, right side, sign, position of left side)
    if pos, then positive literals will be visited.
    if neg, then negative literals will be visited.
    if both, then both sides of a non-oriented equation
      will be visited

    ?pos:bool -> ?neg:bool -> ?both:bool
    -> ('a -> Types.foterm -> Types.foterm -> bool -> int list -> 'a)
    -> 'a -> (Types.literal * int) list
    -> 'a *)
let rec fold_lits ?(pos=true) ?(neg=true) ?(both=true) f acc lits =
  if (not pos) && (not neg) then acc else
  (* is the sign ok, given the parameters? *)
  let sign_ok sign = if sign then pos else neg in
  List.fold_left
    (fun acc (lit, idx) ->
      match lit with
      | Equation (l,r,sign,Gt) when sign_ok sign ->
        f acc l r sign [idx; C.left_pos]
      | Equation (l,r,sign,Lt) when sign_ok sign ->
        f acc r l sign [idx; C.right_pos]
      | Equation (l,r,sign,_) when sign_ok sign ->
        if both
        then (* visit both sides of the equation *)
          let acc = f acc r l sign [idx; C.right_pos] in
          f acc l r sign [idx; C.left_pos]
        else (* only visit one side (arbitrary) *)
          f acc l r sign [idx; C.left_pos]
      | _ -> acc)
    acc lits

(** Visit all non-minimal sides of positive equations *)
let rec fold_positive ?(both=true) f acc lits =
  fold_lits ~pos:true ~neg:false ~both f acc lits

(** Visit all non-minimal sides of negative equations *)
let rec fold_negative ?(both=true) f acc lits =
  fold_lits ~pos:false ~neg:true ~both f acc lits

(** decompose the literal at given position *)
let get_equations_sides clause pos = match pos with
  | idx::eq_side::[] ->
    (match Utils.list_get clause.clits idx with
    | Equation (l,r,sign,_) when eq_side = C.left_pos -> (l, r, sign)
    | Equation (l,r,sign,_) when eq_side = C.right_pos -> (r, l, sign)
    | _ -> invalid_arg "wrong side")
  | _ -> invalid_arg "wrong kind of position (expected binary list)"
