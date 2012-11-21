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

module T = Terms
module C = Clauses
module S = FoSubst
module Utils = FoUtils
module PS = ProofState

(** binary inferences. An inference returns a list of conclusions *)
type binary_inf_rule = ProofState.active_set -> clause -> clause list

(** unary infererences *)
type unary_inf_rule = cs:Clauses.clause_state -> clause -> clause list

(** The type of a calculus for first order reasoning with equality *) 
class type calculus =
  object
    (** the binary inference rules *)
    method binary_rules : (string * binary_inf_rule) list
    (** the unary inference rules *)
    method unary_rules : (string * unary_inf_rule) list
    (** how to simplify a clause *)
    method basic_simplify : cs:Clauses.clause_state -> clause -> clause
    (** how to simplify a clause w.r.t a set of clauses *)
    method simplify : ProofState.active_set -> clause -> clause
    (** check whether the clause is redundant w.r.t the set *)
    method redundant : ProofState.active_set -> clause -> bool
    (** find redundant clauses in set w.r.t the clause *)
    method redundant_set : ProofState.active_set -> clause -> hclause list
    (** how to simplify a clause into a (possibly empty) list
        of clauses. This subsumes the notion of trivial clauses (that
        are simplified into the empty list of clauses) *)
    method list_simplify : cs:Clauses.clause_state -> clause -> clause Vector.t option
    (** a list of axioms to add to the Set of Support *)
    method axioms : clause Vector.t
    (** some constraints on the precedence *)
    method constr : clause Vector.t -> ordering_constraint
    (** how to preprocess the initial list of clauses *)
    method preprocess : cs:Clauses.clause_state -> clause Vector.t -> clause Vector.t
  end

(** do binary inferences that involve the given clause *)
let do_binary_inferences active_set rules c =
  (* rename clause to avoid collisions *)
  let c = PS.relocate_active active_set c in
  Utils.debug 3 (lazy (Utils.sprintf "do binary inferences with current active: %a"
                       C.pp_bag active_set.PS.active_clauses));
  (* apply every inference rule *)
  List.fold_left
    (fun acc (name, rule) ->
      Utils.debug 3 (lazy ("#  apply binary rule " ^ name));
      let new_clauses = rule active_set c in
      List.rev_append new_clauses acc)
    [] rules

(** do unary inferences for the given clause *)
let do_unary_inferences ~cs rules c =
  Utils.debug 3 (lazy "do unary inferences");
  (* apply every inference rule *)
  List.fold_left
    (fun acc (name, rule) ->
      Utils.debug 3 (lazy ("#  apply unary rule " ^ name));
      let new_clauses = rule ~cs c in
      List.rev_append new_clauses acc)
    [] rules

(** fold f over all literals sides, with their positions.
    f is given (acc, left side, right side, sign, position of left side)
    if pos, then positive literals will be visited.
    if neg, then negative literals will be visited.
    if both, then both sides of a non-oriented equation
      will be visited

    ?pos:bool -> ?neg:bool -> ?both:bool -> ord:ordering ->
    -> ('a -> Types.term -> Types.term -> bool -> int list -> 'a)
    -> 'a -> (Types.literal * int) list
    -> 'a *)
let rec fold_lits ?(pos=true) ?(neg=true) ?(both=true) ~ord f acc lits =
  if (not pos) && (not neg) then acc else
  (* is the sign ok, given the parameters? *)
  let sign_ok sign = if sign then pos else neg in
  let acc = ref acc in
  Array.iteri
    (fun idx ({lit_eqn=Equation (l,r,sign)} as lit) ->
      if sign_ok sign then
      match ord#compare l r with
      | Gt -> acc := f !acc lit l r sign [idx; C.left_pos]
      | Lt -> acc := f !acc lit r l sign [idx; C.right_pos]
      | _ ->
        if both
        then (* visit both sides of the equation *)
          acc := f !acc lit r l sign [idx; C.right_pos];
          acc := f !acc lit l r sign [idx; C.left_pos]
        else (* only visit one side (arbitrary) *)
          acc := f !acc lit l r sign [idx; C.left_pos])
    lits;
  !acc

(** Visit all non-minimal sides of positive equations *)
let rec fold_positive ?(both=true) ~ord f acc lits =
  fold_lits ~pos:true ~neg:false ~both ~ord f acc lits

(** Visit all non-minimal sides of negative equations *)
let rec fold_negative ?(both=true) ~ord f acc lits =
  fold_lits ~pos:false ~neg:true ~both ~ord f acc lits

(** decompose the literal at given position *)
let get_equations_sides clause pos =
  match pos with
  | idx::eq_side::[] ->
    (match clause.clits.(idx).lit_eqn with
    | Equation (l,r,sign) when eq_side = C.left_pos -> (l, r, sign)
    | Equation (l,r,sign) when eq_side = C.right_pos -> (r, l, sign)
    | _ -> invalid_arg "wrong side")
  | _ -> invalid_arg "wrong kind of position (expected binary list)"

(** Skolemize the given term at root (assumes it occurs just under an
    existential quantifier, whose De Bruijn variable is replaced
    by a fresh symbol applied to free variables). This also
    caches symbols, so that the same term is always skolemized
    the same way.

    It also refreshes the ordering (the signature has changed) *)
let skolem =
  let cache = T.THashtbl.create 13 (* global cache for skolemized terms *)
  and count = ref 0 in  (* current symbol counter *)
  fun ~ord t sort ->
    Utils.debug 4 (lazy (Utils.sprintf "skolem %a@." !T.pp_term#pp t));
    let normalized_t, subst_to_t = S.normalize_term t in
    let vars = normalized_t.vars in
    (* find the skolemized normalized term *)
    let normalized_t'= try
      T.THashtbl.find cache normalized_t
    with Not_found ->
      (* actual skolemization of normalized_t *)
      let new_symbol = "sk" ^ (string_of_int !count) in
      Utils.debug 4 (lazy (Utils.sprintf "new symbol %s@." new_symbol));
      incr T.sig_version;  (* update version of signature *)
      incr count;
      let skolem_term =
        if vars = [||] then T.mk_const new_symbol sort
        else T.mk_node new_symbol sort (Array.to_list vars)
      in
      (* update the ordering *)
      ord#refresh ();
      (* build the skolemized term *)
      T.db_unlift (T.db_replace normalized_t skolem_term)
    in
    T.THashtbl.replace cache normalized_t normalized_t';
    (* get back to the variables of the given term *)
    let new_t = S.apply_subst ~recursive:false subst_to_t normalized_t' in
    Utils.debug 4 (lazy (Utils.sprintf "skolem %a gives %a@."
                         !T.pp_term#pp t !T.pp_term#pp new_t));
    new_t
