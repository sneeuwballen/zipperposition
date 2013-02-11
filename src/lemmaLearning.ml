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

(** {1 Extracting lemmas from proofs} *)

open Types
open Symbols

module T = Terms
module C = Clauses
module S = FoSubst
module Lits = Literals
module Utils = FoUtils

(** {2 Parameters} *)

let subgraph_min_diameter = ref 4     (** Min distance conclusion <-> axioms *)
let simplicity_threshold = ref 100.   (** Max handicap for conclusion *)
let max_lemmas = ref 2                (** Max number of lemmas *)

(** {2 Utils to build lemmas} *)

(** A possible lemma, i.e. a cut of the graph *)
type candidate_lemma = {
  cl_conclusion : literal array;
  cl_premises : literal array list;
}

exception GotchaLittlePclause of Theories.named_formula * Patterns.mapping

(** Find a named formula [nf] associated with those literals. If none is
    available, create a new named formula.
    This returns both the named_formula and a mapping from [lits]
    to the nf.nf_pclause. *)
let get_nf kb lits =
  let open Theories in
  try
    Patterns.Map.retrieve kb.kb_patterns lits ()
      (fun () pc mapping nf ->
        raise (GotchaLittlePclause (nf, mapping)));
    (* failed to find a matching named_formula *)
    let name = next_name kb in
    (* create a new named_formula for this clause *)
    let pc = Patterns.pclause_of_lits lits in
    let nf = { Patterns.np_name = name; Patterns.np_pattern = pc; } in
    (* add the named formula to the KB *)
    add_named kb [nf];
    (* find a match *)
    match Patterns.match_pclause pc lits with
    | [] -> assert false
    | mapping::_ -> nf, mapping
  with GotchaLittlePclause (nf, mapping) ->
    nf, mapping  (* matched against already existing formula *)

(** Convert a candidate_lemma to a proper lemma. It may have the side-effect
    to add some named formulas to the KB. *)
let candidate_to_lemma kb cl =
  let module Th = Theories in
  let module Pat = Patterns in
  (* map: a list of (symbol, variable number) *)
  let map = ref [] in
  let var_idx = ref (-1) in
  (* get an atom for the named formula with the given mapping *)
  let atom_of_nf nf mapping =
    let name, args = nf.Pat.np_name, nf.Pat.np_pattern.Pat.pc_vars in
    (* create an atom, binding concrete symbols to variables *)
    let args = List.map
      (fun i -> if i < Patterns.symbol_offset
        then `Symbol (Patterns.special_symbols.(i)) (* special constant *)
        else
          (* lookup symbol for this pattern symbol, then variable for the symbol *)
          let symbol = Ptmap.find i mapping.Pat.m_symbol in
          try `Var (List.assq symbol !map)
          with Not_found ->
            (* associate a new variable with this symbol *)
            let n = !var_idx in
            decr var_idx;
            map := (symbol, n) :: !map;
            `Var n)
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
  let premises_atoms = List.sort Th.compare_atom premises_atoms in
  (* return the lemma *)
  { Th.lemma_conclusion = conclusion_atom;
    Th.lemma_premises = premises_atoms; }

(** 'simplicity' heuristic for a list of literals. The lower, the better. *)
let simplicity lits =
  let w = Lits.weight_lits lits
  and d = Lits.depth_lits lits
  and n = Array.length lits in
  (* first estimation of heuristic *)
  let h = float_of_int (n * d + w) in
  if Cnf.is_cnf lits then h  (* good *)
  else if Cnf.is_quasi_cnf lits then h *. 2. (* so-so *)
  else max_float /. 10.  (* very bad *)

(** {2 Cut extraction} *)

(** The idea here is, given a clause [c] in a proof graph, to find
    a cut [P] of the subgraph composed of ancestors of [c], such
    that any path from an axiom [a] to [c] contains at least one
    clause of [P].
    That means that from the conjunction of clauses in [P], [c] is provable. *)

(** Combine two float heuristics (both of them beeing low for
    interesting cases) *)
let combine_heuristics simplicity closeness =
  (simplicity ** 1.2) *. closeness

(** In a path of given [length], a clause at given [depth], counting
    from the beginning of the path (last of the path is the empty clause
    or conclusion [c] of lemma), compute a 'closeness' between clause and [c] *)
let closeness depth length =
  let alpha = 5. in
  (* exp( 1 + alpha depth/length), so that it increases exponentially
     when we are close to the conclusion *)
  exp (1. +. alpha *. (float_of_int depth /. (float_of_int length +. 0.1)))

(** Find a cut for the given proof, from its ancestors, or
    raise Not_found if no cut that covers a big enough portion
    of the proof can be found. *)
let cut graph proof =
  let module G = Proof.ProofGraph in
  (* graph of reversed inference edges: [c] -> [c'] if [c'] is a premise
     in the inference that proves [c] *)
  let graph = G.rev graph in
  assert (G.is_dag graph);
  (* set of selected nodes (clauses) of the graph, to eventually form a cut *)
  let cut = ref G.S.empty in
  let ignore v = G.S.mem v !cut in
  let goal v _ = Proof.is_axiom v in
  (* search for the shortest open path from [proof] to an axiom, then
     cut the path and loop *)
  let rec next_path () =
    let progress = try
      let _, _, path = G.min_path_full graph ~ignore ~goal proof in
      cut_path path;
      true
    with Not_found ->
      false  (* success, all paths are closed *)
    in if progress then next_path () else ()
  (* select an element of the path, and add it to [cut] *)
  and cut_path path =
    let length = List.length path in
    Utils.debug 1 (lazy (Utils.sprintf "%% cut path of length %d" length));
    (* heuristic cost of the proof, at given closeness from [proof] *)
    let heuristic p depth =
      combine_heuristics
        (simplicity (Proof.proof_lits p))
        (closeness depth length)
    in
    match path with
    | [] -> assert false
    | (p,_,_)::path' ->
      (* by default, choose the first clause *)
      let best = ref (p, heuristic p 1) in
      let _ = List.fold_left
        (fun depth (p,_,_) ->
          assert (not (G.S.mem p !cut));
          (* [p] is a proof in the path *)
          let h = heuristic p depth in
          let best_proof, best_h = !best in
          (if h < best_h then
            best := (p, h));
          depth+1)
        2 path'
      in
      cut := G.S.add (fst !best) !cut
  in
  next_path ();
  (* convert the cut to a list *)
  G.S.elements !cut

(** Pretty print the graph, including the cut, in given file *)
let pp_cut_dot ~name filename (graph, cut, conclusion) = 
  let module G = Proof.ProofGraph in
  (* DOT printer of this [cut] of the graph *)
  let cut_dot_printer =
    (* convert to set *)
    let cut = Sequence.to_set
      (module G.S : Set.S with type elt = compact_clause proof 
                           and type t = G.S.t)
      cut in
    let print_vertex proof =
      let lits = Proof.proof_lits proof in
      let label = `Label (Utils.sprintf "@[<h>%a (%.2F)@]"
        Lits.pp_lits lits (simplicity lits)) in
      let attributes = [`Shape "box"; `Style "filled"] in
      let attributes =
        if Proof.proof_lits proof = [||] then `Color "red" :: `Label "[]" :: attributes
        else if Proof.proof_id conclusion = Proof.proof_id proof
          then `Color "cyan" :: label :: attributes
        else if G.S.mem proof cut then `Color "green" :: label :: attributes
        else if Proof.is_axiom proof then label :: `Color "yellow" :: attributes
        else label :: attributes in
      attributes
    and print_edge v1 e v2 =
      [`Label e]
    in
    G.mk_dot_printer ~print_vertex ~print_edge
  in
  (* print graph on file *)
  let out = open_out filename in
  try
    (* write on the opened out channel *)
    let formatter = Format.formatter_of_out_channel out in
    Format.printf "%% print cut-graph to %s@." filename;
    G.pp cut_dot_printer ~name formatter
      (Sequence.map G.rev_edge (G.to_seq graph));
    Format.fprintf formatter "@.";
    close_out out
  with _ -> close_out out

(** {2 Lemma learning} *)

(** From the given proof of the empty clause, find a cut [P] of
    its premises, and learn p_1 & p_2 & ... & p_n => empty *)
let learn_empty ~meta proof =
  let kb = meta.Theories.meta_kb in
  try
    (* find a good cut of the proof of $false *)
    let graph = Proof.to_graph proof in
    let proofs = cut graph proof in
    pp_cut_dot ~name:"cut" "learn_empty.dot" (graph, Sequence.of_list proofs,proof);
    match proofs with
    | [] -> failwith "empty proof cut?"
    | _ ->
      (* premises: all clauses in cut *)
      let premises = List.map Proof.proof_lits proofs in
      (* conclusion: empty clause *)
      let conclusion = [||] in
      (* build lemma *)
      let candidate = { cl_premises=premises; cl_conclusion=conclusion; } in
      let lemma = candidate_to_lemma kb candidate in
      Some lemma
  with Not_found ->
    None  (* no good cut *)

let learn_subproof_idx = ref 0
let cut_max_cost_factor = 50.

let cut_too_costly proofs =
  (* Sum of 'simplicity' of the cut *)
  let cut_cost proofs =
    List.fold_left
      (fun sum proof -> sum +. simplicity (Proof.proof_lits proof))
      0. proofs
  in
  let len = float_of_int (List.length proofs) in
  cut_cost proofs > cut_max_cost_factor *. len

(** From the given proof [c], find a cut [P] of its premises,
    and learn the lemma p_1 & p_2 & ... & p_n => c *)
let learn_subproof ~meta proof =
  Utils.debug 1 (lazy (Utils.sprintf "%% try to learn lemma with conclusion @[<h>%a@]"
                Lits.pp_lits (Proof.proof_lits proof)));
  let kb = meta.Theories.meta_kb in
  try
    (* find a good cut of [proof] *)
    let graph = Proof.to_graph proof in
    let proofs = cut graph proof in
    if cut_too_costly proofs
    then None  (* some premise is too expensive *)
    else begin
      let filename = Utils.sprintf "learn_subproof%d.dot" !learn_subproof_idx in
      incr learn_subproof_idx;
      pp_cut_dot ~name:"cut" filename (graph, Sequence.of_list proofs,proof);
      (* premises: clauses of the cut *)
      let premises = List.map Proof.proof_lits proofs in
      (* conclusion: negation of chosen clause *)
      let conclusion = Proof.proof_lits proof in
      let candidate = { cl_premises=premises; cl_conclusion=conclusion; } in
      let lemma = candidate_to_lemma kb candidate in
      Some lemma
    end
  with Not_found ->
    None  (* no good cut *)

(** {2 Search for salient clauses *)

(** Find a list of {b salient} clauses in the given proof. Salient clauses
    are small clauses that have many descendants in the proof, and are
    close to the conclusion. Those clauses should be good candidates
    for [learn_subproof].
    Implementation should rely on PageRank on the reverse graph. *)
let salient_clauses proof =
  let module G = Proof.ProofGraph in
  let graph = Proof.to_graph proof in
  let graph = G.rev graph in
  (* check whether the subgraph is big enough *)
  let check_big_subgraph proof =
    let diameter = G.diameter graph proof in
    diameter >= !subgraph_min_diameter
  in
  (* explore clauses, starting from the empty clause but ignoring it *)
  let candidates = Sequence.drop 1 (G.bfs_seq graph proof) in
  (* only retain simple clauses, far away from axioms, and keep a few of them *)
  let candidates = Sequence.filter
    (fun proof -> simplicity (Proof.proof_lits proof) < !simplicity_threshold
               && check_big_subgraph proof)
    candidates in
  Sequence.to_list candidates

(** {2 Batteries-included lemma learning} *)

(** Given an empty clause (and its proof), look in the proof for lemmas. *)
let search_lemmas meta proof =
  assert (Proof.proof_lits proof = [||]);
  (* trivial proof? *)
  if Proof.is_axiom proof then [] else
  let open Theories in
  (* learn subproofs *)
  let salient = Utils.list_take !max_lemmas (salient_clauses proof) in
  let lemmas = List.map (learn_subproof ~meta) salient in
  (* learn from empty clause (full proof) *)
  let lemmas = learn_empty ~meta proof :: lemmas in
  (* flatten ('a option list -> 'a list) *)
  let lemmas = Utils.list_flatmap (function None -> [] | Some x -> [x]) lemmas in
  (* only keep lemmas that give safe clauses *)
  let lemmas = List.filter
    (fun lemma ->
      let clause = clause_of_lemma lemma in
      Logic.check_safe clause)
    lemmas
  in
  lemmas

(** Update the KB of this meta-prover by learning from
    the given (empty) clause's proof. The KB is modified
    in place. *)
let learn_and_update meta hc =
  let open Theories in
  let kb = meta.meta_kb in
  (* learn from this proof *)
  let lemmas = search_lemmas meta hc.hcproof in
  let lemmas = List.map
    (fun lemma ->
      Format.printf "%%@[<h>   learn %a@]@." pp_lemma lemma;
      lemma)
    lemmas
  in
  (* store new lemmas *)
  add_lemmas kb lemmas
