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
    (** the binary inference rules *)
    method binary_rules : (string * binary_inf_rule) list
    (** the unary inference rules *)
    method unary_rules : (string * unary_inf_rule) list
    (** how to simplify a clause *)
    method basic_simplify : ord:ordering -> hclause -> hclause
    (** how to simplify a clause w.r.t a set of unit clauses *)
    method simplify : select:selection_fun -> ProofState.active_set -> ProofState.simpl_set -> hclause -> hclause
    (** check whether the clause is redundant w.r.t the set *)
    method redundant : ProofState.active_set -> hclause -> bool
    (** find redundant clauses in set w.r.t the clause *)
    method redundant_set : ProofState.active_set -> hclause -> hclause list
    (** how to simplify a clause into a (possibly empty) list
        of clauses. This subsumes the notion of trivial clauses (that
        are simplified into the empty list of clauses) *)
    method list_simplify : ord:ordering -> select:selection_fun -> hclause -> hclause list option
    (** a list of axioms to add to the problem *)
    method axioms : hclause list
    (** some constraints on the precedence *)
    method constr : hclause list -> ordering_constraint
    (** how to preprocess the initial list of clauses *)
    method preprocess : ord:ordering -> select:selection_fun -> hclause list -> hclause list
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

(** Skolemize the given term at root (assumes it occurs just under an
    existential quantifier, whose De Bruijn variable is replaced
    by a fresh symbol applied to free variables). This also
    caches symbols, so that the same term is always skolemized
    the same way.

    It also refreshes the ordering (the signature has changed) *)
let classic_skolem =
  let cache = T.THashtbl.create 13 (* global cache for skolemized terms *)
  and count = ref 0 in  (* current symbol counter *)
  (* find an unused skolem symbol *)
  let rec find_skolem () = 
    let skolem = "sk" ^ (string_of_int !count) in
    incr count;
    if Symbols.is_used skolem then find_skolem () else skolem
  in
  fun ~ord t sort ->
    Utils.debug 4 (lazy (Utils.sprintf "skolem %a@." !T.pp_term#pp t));
    let normalized_t, subst_to_t = S.normalize_term t in
    let vars = normalized_t.vars in
    (* find the skolemized normalized term *)
    let normalized_t'= try
      T.THashtbl.find cache normalized_t
    with Not_found ->
      (* actual skolemization of normalized_t *)
      let new_symbol = find_skolem () in
      Utils.debug 1 (lazy (Utils.sprintf "new symbol %s@." new_symbol));
      let new_symbol = mk_symbol ~attrs:Symbols.attr_skolem new_symbol in  (* build symbol *)
      let skolem_term = T.mk_node new_symbol sort vars in
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

(** Skolemization with a special non-first order symbol. The purpose is
    not to introduce too many terms. A proposition p is skolemized
    into $$skolem(p), which makes naturally for inner skolemization.

    The advantage is that it does not modify the signature, and also that
    rewriting can be performed inside the skolem terms. *)
let unamed_skolem ~ord t sort =
  Utils.debug 4 (lazy (Utils.sprintf "@[<h>magic skolem %a@]@." !T.pp_term#pp t));
  let symb = mk_symbol ~attrs:Symbols.attr_skolem "$$sk" in
  (* the existential witness, parametrized by the 'quoted' formula. The
     lambda is used to keep the formula closed. *)
  let args = [T.mk_node lambda_symbol t.sort [t]] in
  let skolem_term = T.mk_node symb sort args in
  ord#refresh ();  (* skolem symbol may be new *)
  (* build the skolemized term by replacing first DB index with skolem symbol *)
  T.db_unlift (T.db_replace t skolem_term)

let skolem = ref classic_skolem
