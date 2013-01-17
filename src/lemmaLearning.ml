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

(** Extracting lemmas from proofs *)

open Types
open Symbols

module T = Terms
module C = Clauses
module S = FoSubst
module Utils = FoUtils

(* ----------------------------------------------------------------------
 * (heuristic) search of "interesting" lemma in a proof.
 * ---------------------------------------------------------------------- *)

(** Heuristic "simplicity and elegance" measure for pclauses. The smaller,
    the better. *)
let rate_pclause pclause =
  let rate = ref 1. in
  (* many symbols is not simple *)
  let symbols = pclause.Patterns.pc_vars in
  let num_symbols = List.length symbols in
  rate := !rate +. (float_of_int (num_symbols - 1));
  (* many literals is not simple *)
  let length = List.length pclause.Patterns.pc_lits in
  rate := !rate +. (2. *. float_of_int (length - 1));
  (* result *)
  Utils.debug 3 (lazy (Utils.sprintf
                "%% simplicity of @[<h>%a@] is %.2f (%d symbols, %d lits)"
                Patterns.pp_pclause pclause !rate num_symbols length));
  !rate 

(** 'cost', or handicap, of a symbol *)
let cost_symbol ~is_theory_symbol signature s =
  if is_theory_symbol s then 0.2
  else
    let arity = try fst (SMap.find s signature) with Not_found -> 3 in
    if arity = 0 then 1. else 3. *. (float_of_int arity)

(** Heuristic "simplicity and elegance" measure for clauses in a proof. Theory
    symbols are less 'costly' than other symbols, as are constants.
    The smaller the result, the better. *)
let rate_clause ~is_theory_symbol hc = 
  let rate = ref 1. in
  (* many symbols is not simple *)
  let signature = C.signature [hc] in
  let symbols = symbols_of_signature signature in
  let symbols = List.filter (fun s -> not (SSet.mem s base_symbols)) symbols in
  List.iter
    (fun s -> rate := !rate +. cost_symbol ~is_theory_symbol signature s)
    symbols;
  (* weight of clause, as a measure of weight of terms *)
  rate := !rate *. (float_of_int hc.hcweight) /. 2.;
  (* many literals is not simple, multiply by length of clause *)
  let length = Array.length hc.hclits in
  rate := !rate *. (float_of_int (length - 1));
  (* result *)
  Utils.debug 3 (lazy (Utils.sprintf
                "%% simplicity of @[<h>%a@] is %.2f" !C.pp_clause#pp_h hc !rate));
  !rate 

(** Depth of a proof, ie max distance between the empty clause (root) and an axiom *)
let proof_depth hc =
  let explored = ref C.CSet.empty in
  let depth = ref 0 in
  let q = Queue.create () in
  Queue.push (hc, 0) q;
  while not (Queue.is_empty q) do
    let (hc, d) = Queue.pop q in
    if C.CSet.mem !explored hc then () else begin
      explored := C.CSet.add !explored hc;
      match hc.hcproof with
      | Axiom _ -> depth := max d !depth
      | Proof (_, l) -> (* explore parents *)
        List.iter (fun (c,_,_) -> Queue.push (c.cref, d+1) q) l
    end
  done;
  !depth

(** Maximum number of lemmas that can be learnt from one proof *)
let max_lemmas = ref 3

(** Maximum lemma rate, above which the lemma is discarded *)
let max_rate = ref 50.

(** A possible lemma, i.e. a subgraph *)
type candidate_lemma = {
  cl_conclusion : hclause;
  cl_premises : hclause list;
  cl_rate : float;
}

exception GotchaLittlePclause of Theories.named_formula * Patterns.mapping

(** Find a named formula [nf] associated with this hclause. If none is
    available, create a new named formula.
    This returns both the named_formula and a mapping from [hc] to the nf.nf_pclause. *)
let get_nf kb hc =
  let open Theories in
  try
    Patterns.Map.retrieve kb.kb_patterns hc ()
      (fun () pc mapping nf ->
        raise (GotchaLittlePclause (nf, mapping)));
    (* failed to find a matching named_formula *)
    let name = next_name ~prefix:"formula" kb in
    (* create a new named_formula for this clause *)
    let pc = Patterns.pclause_of_clause hc in
    let atom = name, pc.Patterns.pc_vars in
    let nf = { nf_atom = atom; nf_pclause = pc; } in
    (* add the named formula to the KB *)
    add_named kb [nf];
    (* find a match *)
    match Patterns.match_pclause pc hc with
    | [] -> assert false
    | mapping::_ -> nf, mapping
  with GotchaLittlePclause (nf, mapping) ->
    nf, mapping  (* matched against already existing formula *)

(** Convert a candidate_lemma to a proper lemma. It may have the side-effect
    to add some named formulas to the KB. *)
let candidate_to_lemma kb cl =
  let open Theories in
  (* map: a list of (symbol, variable number) *)
  let map = ref [] in
  let var_idx = ref (-1) in
  (* get an atom for the named formula with the given mapping *)
  let atom_of_nf nf mapping =
    let name, args = nf.nf_atom in
    (* create an atom, binding concrete symbols to variables *)
    let args = List.map
      (fun i -> if i < Patterns.symbol_offset
        then i (* special constant *)
        else
          (* lookup symbol for this pattern symbol, then variable for the symbol *)
          let symbol = Ptmap.find i mapping.Patterns.m_symbol in
          try List.assq symbol !map
          with Not_found ->
            (* associate a new variable with this symbol *)
            let n = !var_idx in
            decr var_idx;
            map := (symbol, n) :: !map;
            n)
      args in
    name, args
  in
  (* atom for conclusion *)
  let conclusion_nf, conclusion_mapping = get_nf kb cl.cl_conclusion in
  let conclusion_atom = atom_of_nf conclusion_nf conclusion_mapping in
  (* atoms for premises *)
  let premises_atoms = List.map
    (fun premise ->
      let nf, mapping = get_nf kb premise in atom_of_nf nf mapping)
    cl.cl_premises in
  (* return the lemma *)
  { lemma_conclusion = conclusion_atom;
    lemma_premises = premises_atoms; }

module CostMap = Map.Make(
  struct
    type t = hclause * int
    let compare (hc1,d1) (hc2,d2) = if d1 <> d2 then d1 - d2 else hc1.hctag - hc2.hctag
  end)

(** Minimum threshold for the max distance between the conclusion of a lemma,
    and a premise of the lemma. *)
let min_distance_threshold = 3

(** Explore parents of the clause, looking for clauses that are simple
    or for clauses that belong to a theory.
    [distance] is the distance between [hc] and the root of the proof.
    [cost_map]: for [c] a hclause, stores the best (rate, list of clauses) where
       the list of clauses is a proof for [c] (best means with lowest rate
       but max distance from the clause)
    It returns a tuple (rate, list of premises, max distance). *)
let explore_parents meta cost_map distance hc =
  let open Theories in
  let is_theory_symbol s = SSet.mem s meta.meta_theory_symbols in
  let is_theory_clause hc = Ptmap.mem hc.hctag meta.meta_theory_clauses in
  (* map (hclause, distance) -> 'a *)
  (* compute the best list of premises for hc. It may be [hc] itself.
     distance is the distance from the initial (empty) clause *)
  let rec compute_best distance hc =
    (* try to find if it's already been computed *)
    try CostMap.find (hc, distance) !cost_map
    with Not_found ->
      (* cost of returning [hc] *)
      let cost_hc =
        if is_theory_clause hc
          then 0.1 (* theory clauses are cheap *)
          else rate_clause ~is_theory_symbol hc /. (0.1 +. (float_of_int distance))
      in
      let best_cost, best_premises, max_dist = match hc.hcproof with
      | Axiom _ -> cost_hc /. 2., C.ClauseSet.singleton hc, distance  (* axioms are cheaper *)
      | Proof (_, l) ->
        (* cost of recursing into parents *)
        let cost_parents, premises_parents, max_dist = List.fold_left
          (fun (cost,premises,m) (parent,_,_) ->
            let cost', premises',m' = compute_best (distance+1) parent.cref in
            cost +. cost', C.ClauseSet.union premises premises', max m m')
          (0., C.ClauseSet.empty, 0) l
        in
        (* make the choice that gives the lowest cost *)
        if cost_hc < cost_parents && distance >= min_distance_threshold
          then (cost_hc, C.ClauseSet.singleton hc, max_dist)
          else (cost_parents, premises_parents, max_dist)
      in
      (* memoize and return *)
      let triple = best_cost, best_premises, max_dist in
      cost_map := CostMap.add (hc, distance) triple !cost_map;
      triple
  in
  (* return the best choice of premises for this (clause, distance) *)
  let cost, premises, max_dist = compute_best distance hc in
  let premises = C.ClauseSet.elements premises in
  cost, premises, max_dist

(** Given an empty clause (and its proof), look in the proof for lemmas. *)
let search_lemmas meta hc =
  assert (hc.hclits = [||]);
  let open Theories in
  let is_theory_symbol s = SSet.mem s meta.meta_theory_symbols in
  (* depth of the proof *)
  let depth = proof_depth hc in
  Utils.debug 1 (lazy (Utils.sprintf "%% analyse proof of depth %d" depth));
  (* candidate lemmas *)
  let cost_map = ref CostMap.empty in
  let candidates = ref [] in
  let explored = ref C.CSet.empty in
  let q = Queue.create () in
  (* push parents of the empty clause in the queue *)
  (match hc.hcproof with
  | Axiom _ -> ()
  | Proof (_, l) ->
    List.iter (fun (c,_,_) -> Queue.push (c.cref, 1) q) l);
  (* breadth-first exploration of the proof. The depth is kept with the clause *)
  while not (Queue.is_empty q) do
    let hc, depth = Queue.pop q in
    if C.CSet.mem !explored hc then () else begin
      (* clause not already explored *)
      explored := C.CSet.add !explored hc;
      (* absolute rate of the clause itself *)
      let hc_rate = rate_clause ~is_theory_symbol hc in
      (* choice of premises, with associate cost *)
      let premises_rate, premises, max_dist = explore_parents meta cost_map 0 hc in
      begin if max_dist >= min_distance_threshold then
        (* build the candidate *)
        let cl = {
          cl_conclusion = hc;
          cl_premises = premises;
          cl_rate = hc_rate +. premises_rate;
        } in
        candidates := cl :: !candidates
      end;
      (* explore parent clauses *)
      match hc.hcproof with
      | Axiom _ -> ()
      | Proof (_, l) ->
        List.iter (fun (c,_,_) -> Queue.push (c.cref, depth+1) q) l
    end
  done;
  (* sort candidate by increasing rate (bad candidates at the end), and take
     only a given amount of them. *)
  let candidates = List.sort
    (fun cl1 cl2 -> int_of_float (cl1.cl_rate -. cl2.cl_rate))
    !candidates in
  let candidates = Utils.list_take !max_lemmas candidates in
  (* convert the candidate lemma to a lemma. Also keep all the
     involved named formulas. *)
  let lemmas = Utils.list_flatmap
    (fun cl ->
      if cl.cl_rate > !max_rate
        then begin
          Utils.debug 0 (lazy (Utils.sprintf "%% drop candidate with rate %.2f" cl.cl_rate));
          []
        end else
          let lemma = candidate_to_lemma meta.meta_kb cl in
          [lemma, cl.cl_rate])
    candidates
  in
  (* only keep lemmas that give safe rules *)
  let lemmas = List.filter
    (fun (lemma, _) ->
      let rule = rule_of_lemma lemma in
      Datalog.Logic.check_safe rule)
    lemmas
  in
  lemmas

(** Update the KB of this meta-prover by learning from
    the given (empty) clause's proof. The KB is modified
    in place. *)
let learn_and_update meta hc =
  let open Theories in
  let kb = meta.meta_kb in
  let h = hash_proof hc in
  if ProofHashSet.mem h kb.kb_proofs
  then Utils.debug 0 (lazy (Utils.sprintf "%% proof %Ld already processed" h))
  else begin
    (* this proof is not known yet, learn from it *)
    kb.kb_proofs <- ProofHashSet.add h kb.kb_proofs;
    let lemmas = search_lemmas meta hc in
    let lemmas = List.map
      (fun (lemma, rate) ->
        Format.printf "%%   learn @[<h>%a@], rated %.2f@." pp_lemma lemma rate;
        lemma)
      lemmas
    in
    (* store new lemmas *)
    add_lemmas kb lemmas
  end
