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

(** Decision procedures for theories *)

open Types
open Symbols

module T = Terms
module C = Clauses
module Utils = FoUtils

(** {2 General interface} *)

type dp = {
  dp_name : string;                 (** Theory this procedures decides *)
  dp_descr : string;                (** Description of the procedure *)
  dp_equal : term -> term -> bool;  (** Check whether two terms are equal *)
  dp_sig : SSet.t;                  (** Symbols of the theory *)
  dp_clauses : hclause list;        (** Clauses to add to the problem *)
  dp_canonize : term -> term;       (** Get a canonical form of the term *)
  dp_solve : (term -> term -> substitution) option;
  (* TODO dp_solve : ((term*term) list -> substitution list) option
    for simplex-like solvers? *)
}

(** Simple syntaxic criterion to decide whether two decision procedures
    are compatibles: check whether they have no symbol in common.

    TODO: more elaborate checks, for instance with ground-joinability of all
    critical pairs *)
let dp_compatible dp1 dp2 =
  SSet.is_empty (SSet.union dp1.dp_sig dp2.dp_sig)

(** Combine two decision procedures into a new one, that decides
    the combination of their theories, assuming they are compatible. *)
let dp_combine dp1 dp2 =
  assert (dp_compatible dp1 dp2);
  (* compute normal form using both systems *)
  let rec nf t =
    let t' = dp1.dp_canonize t in
    let t' = dp2.dp_canonize t' in
    if t == t' then t' else nf t'
  in
  {
    dp_name = Utils.sprintf "%s_U_%s" dp1.dp_name dp2.dp_name;
    dp_descr =
      Utils.sprintf "@[<hov2>union of@ %s and@ %s@]" dp1.dp_descr dp2.dp_descr;
    dp_equal = (fun t1 t2 -> nf t1 == nf t2);
    dp_sig = SSet.union dp1.dp_sig dp2.dp_sig;
    dp_clauses = List.rev_append dp1.dp_clauses dp2.dp_clauses;
    dp_canonize = nf;
    dp_solve = None;
  }

(** [dp_more_specific dp1 dp2] returns true if [dp1] decides a theory
    whose symbols are included in the theory of [dp2]. Heuristically, that
    means that we can ignore [dp1] and focus on [dp2] *)
let dp_more_specific dp1 dp2 =
  SSet.subset dp1.dp_sig dp2.dp_sig &&
  not (SSet.equal dp1.dp_sig dp2.dp_sig)

(** Get the normal form of the term *)
let dp_canonize dp t = dp.dp_canonize t

let dp_equal dp t1 t2 = dp.dp_equal t1 t2

let dp_sig dp = dp.dp_sig

(** Decide whether this clause is redundant *)
let dp_is_redundant dp hc =
  if C.get_flag C.flag_persistent hc then false else
  let ans = Utils.array_exists
    (fun lit -> match lit with
      | Equation (l, r, true, _) -> dp.dp_equal l r
      | _ -> false)
    hc.hclits
  in
  (if ans then
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a redundant with %s@]"
                   !C.pp_clause#pp_h hc dp.dp_name)));
  ans

(** Simplify the clause *)
let dp_simplify ~ctx dp hc =
  let lits = Array.to_list hc.hclits in
  let lits = List.filter
    (fun lit -> match lit with
      | Equation (l, r, false, _) when dp.dp_equal l r -> false
      | _ -> true)
    lits in
  if List.length lits = Array.length hc.hclits
    then hc  (* no simplification *)
    else begin
      let rule = "dp_" ^ dp.dp_name in
      let proof c' = Proof (c', rule, [hc.hcproof]) in
      let parents = hc :: hc.hcparents in
      let new_hc = C.mk_hclause ~parents ~ctx lits proof in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>theory-simplified %a into %a with %s@]"
                     !C.pp_clause#pp hc !C.pp_clause#pp_h new_hc dp.dp_name));
      (* return simplified clause *)
      new_hc
    end

(** Get a list of clauses this DP needs to be present in the
    superposition prover for it to be complete *)
let dp_clauses dp = 
  let clauses = dp.dp_clauses in
  List.iter (fun hc -> C.set_flag C.flag_persistent hc true) clauses;
  clauses

(** {2 Ground joinable sets of equations} *)

(** We use ground convergent sets of equations to decide some equational
    theories. See
    "On using ground joinable equations in equational theorem proving", by
    Avenhaus, Hillenbrand, Lochner *)

type gnd_convergent = {
  gc_ord : string;                    (** name of the ordering *)
  gc_prec : symbol list;              (** Precedence *)
  gc_sig : SSet.t;                    (** Symbols of the theory *)
  gc_equations : literal array list;  (** Equations of the system *)
} (** A set of ground convergent equations, for some order+precedence *)

let mk_gc ~ord clauses = failwith "nope"

(** From a set of ground convergent equations, make a decision
    procedure that can be used by the prover *)
let gc_to_dp gc = failwith "nope"

(** Pretty-print the system of ground convergent equations *)
let pp_gc formatter gc = failwith "nope"

(** {3 JSON encoding} *)

let gc_to_json gc = failwith "nope"

let gc_of_json ~ctx json = failwith "nope"

(** {2 Some builtin theories} *)

(** Theory of Associative-Commutative symbols, for the given symbol *)
let ac f = 
  (* function that computes the AC(f)-normal form of the term *)
  let nf t = failwith "AC not implemented"
  in
  let dp = {
    dp_name = Utils.sprintf "AC_%s" (name_symbol f);
    dp_descr = Utils.sprintf "AC for symbol %s" (name_symbol f);
    dp_equal = (fun t1 t2 -> nf t1 == nf t2);
    dp_sig = SSet.singleton f;
    dp_clauses = []; (* TODO *)
    dp_canonize = nf;
    dp_solve = None;
  } in
  dp
