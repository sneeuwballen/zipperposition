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

type expert = {
  expert_name : string;                 (** Theory the expert works on *)
  expert_descr : string;                (** Description of the expert *)
  expert_equal : term -> term -> bool;  (** Check whether two terms are equal *)
  expert_sig : SSet.t;                  (** Symbols of the theory *)
  expert_clauses : hclause list;        (** Additional axioms *)
  expert_canonize : term -> term;       (** Get a canonical form of the term *)
  expert_solve : ((term*term) list -> substitution list) option;
    (** The expert may be able to solve systems of equations, returning
        a list of substitutions. Example: the simplex. *)
} (** An expert for some theory *)

(** Simple syntaxic criterion to decide whether two decision procedures
    are compatibles: check whether they have no symbol in common.

    TODO: more elaborate checks, for instance with ground-joinability of all
    critical pairs *)
let expert_compatible e1 e2 =
  SSet.is_empty (SSet.union e1.expert_sig e2.expert_sig)

(** Combine two decision procedures into a new one, that decides
    the combination of their theories, assuming they are compatible. *)
let expert_combine e1 e2 =
  assert (expert_compatible e1 e2);
  (* compute normal form using both systems *)
  let rec nf t =
    let t' = e1.expert_canonize t in
    let t' = e2.expert_canonize t' in
    if t == t' then t' else nf t'
  in
  {
    expert_name = Utils.sprintf "%s_U_%s" e1.expert_name e2.expert_name;
    expert_descr =
      Utils.sprintf "@[<hov2>union of@ %s and@ %s@]" e1.expert_descr e2.expert_descr;
    expert_equal = (fun t1 t2 -> nf t1 == nf t2);
    expert_sig = SSet.union e1.expert_sig e2.expert_sig;
    expert_clauses = List.rev_append e1.expert_clauses e2.expert_clauses;
    expert_canonize = nf;
    expert_solve = None;
  }

(** [expert_more_specific e1 e2] returns true if [e1] decides a theory
    whose symbols are included in the theory of [e2]. Heuristically, that
    means that we can ignore [e1] and focus on [e2] *)
let expert_more_specific e1 e2 =
  SSet.subset e1.expert_sig e2.expert_sig &&
  not (SSet.equal e1.expert_sig e2.expert_sig)

(** Get the normal form of the term *)
let expert_canonize expert t = expert.expert_canonize t

let expert_equal expert t1 t2 = expert.expert_equal t1 t2

let expert_sig expert = expert.expert_sig

(** Decide whether this clause is redundant *)
let expert_is_redundant expert hc =
  if C.get_flag C.flag_persistent hc then false else
  let ans = Utils.array_exists
    (fun lit -> match lit with
      | Equation (l, r, true, _) -> expert.expert_equal l r
      | _ -> false)
    hc.hclits
  in
  (if ans then
    Utils.debug 3 "@[<h>%a redundant with %s@]" !C.pp_clause#pp_h hc expert.expert_name);
  ans

(** Simplify the clause *)
let expert_simplify ~ctx expert hc =
  let lits = Array.to_list hc.hclits in
  let lits = List.filter
    (fun lit -> match lit with
      | Equation (l, r, false, _) when expert.expert_equal l r -> false
      | _ -> true)
    lits in
  if List.length lits = Array.length hc.hclits
    then hc  (* no simplification *)
    else begin
      let rule = "expert_" ^ expert.expert_name in
      let proof c' = Proof (c', rule, [hc.hcproof]) in
      let parents = hc :: hc.hcparents in
      let new_hc = C.mk_hclause ~parents ~ctx lits proof in
      Utils.debug 3 "@[<h>theory-simplified %a into %a with %s@]"
                     !C.pp_clause#pp hc !C.pp_clause#pp_h new_hc expert.expert_name;
      (* return simplified clause *)
      new_hc
    end

(** Get a list of clauses this DP needs to be present in the
    superposition prover for it to be complete *)
let expert_clauses expert = 
  let clauses = expert.expert_clauses in
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
  gc_equations : literal list;        (** Equations of the system *)
} (** A set of ground convergent equations, for some order+precedence *)

let mk_gc name prec lits =
  let set = T.symbols
    (Sequence.concat
      (Sequence.map 
        (fun (Equation (l,r,_,_)) -> Sequence.of_list [l;r])
        (Sequence.of_list lits)))
  in
  { gc_ord = name;
    gc_prec = prec;
    gc_sig = set;
    gc_equations = lits;
  }

(** From a set of ground convergent equations, create an expert for
    the associated theory. *)
let gc_expert gc = failwith "todo: Experts.gc_expert"

(** Pretty-print the system of ground convergent equations *)
let pp_gc formatter gc = failwith "todo: Experts.pp_gc"

(** {3 JSON encoding} *)

let gc_to_json gc = failwith "nope"

let gc_of_json ~ctx json = failwith "nope"

(** {2 Some builtin theories} *)

(** Theory of Associative-Commutative symbols, for the given symbol *)
let ac f = 
  (* function that computes the AC(f)-normal form of the term *)
  let nf t = failwith "AC not implemented"
  in
  let expert = {
    expert_name = Utils.sprintf "AC_%s" (name_symbol f);
    expert_descr = Utils.sprintf "AC for symbol %s" (name_symbol f);
    expert_equal = (fun t1 t2 -> nf t1 == nf t2);
    expert_sig = SSet.singleton f;
    expert_clauses = []; (* TODO *)
    expert_canonize = nf;
    expert_solve = None;
  } in
  expert
