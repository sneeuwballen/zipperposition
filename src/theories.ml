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

(** Recognition of theories *)

open Types
open Symbols

module T = Terms
module C = Clauses
module S = FoSubst
module Utils = FoUtils

(* ----------------------------------------------------------------------
 * recognition of proof
 * ---------------------------------------------------------------------- *)

type proof_hash = Int64.t

(** Get a (probably) unique hash for this proof *)
let hash_proof hc =
  assert (hc.hclits = [||]);
  let explored = ref C.CSet.empty in
  let h = ref 23029L in
  let counter = ref 17L in   (* a counter of traversal in the DAG, with offset *)
  (* recurse in the DAG *)
  let rec explore hc =
    if C.CSet.mem !explored hc
    then ()
    else begin
      explored := C.CSet.add !explored hc;
      (* combine hash with the clause's hash *)
      let h_clause = Int64.of_int (C.hash_hclause hc) in
      h := Int64.add !h (Int64.mul !counter  h_clause);
      counter := Int64.succ !counter;
      match hc.hcproof with
      | Axiom _ -> ()
      | Proof (kind, l) ->
        (* explore parents of the clause; first hash the inference kind *)
        let h_kind = Int64.of_int (Hashtbl.hash kind) in
        h := Int64.add !h (Int64.mul 22571L h_kind);
        List.iter (fun (c, _, _) -> explore c.cref) l
    end
  in
  explore hc;
  !h

module ProofHashSet = Set.Make(struct type t = proof_hash let compare = Int64.compare end) 
  (** Set of proof hashes *)

(* ----------------------------------------------------------------------
 * generic representation of theories and lemmas (persistent)
 * ---------------------------------------------------------------------- *)

type atom_name = string
  (** The name of a formula. If a formula is part of a known axiomatisation
      it can have a specific name, otherwise just "lemma_X" with X a number
      (e.g. f(X,Y)=f(Y,X) is named "commutativity") *)

type atom = atom_name * int list
  (** An atom in the meta level of reasoning. This represents a fact about
      the current proof search (presence of a theory, of a clause, of a lemma... *)

type named_formula = {
  nf_atom : atom;                       (* meta-atom for an instance of the pclause *)
  nf_pclause : Patterns.pclause;        (* the pattern of the formula itself *)
} (** A named formula is a pattern clause, plus a name (used for the datalog
      representation of instances of this formula *)

type theory = {
  th_atom : atom;                           (* meta-atom for the theory *)
  th_definition : atom list;                (* definition (set of axioms) *)
} (** A theory is a named set of formulas (axioms) *)

type lemma = {
  lemma_conclusion : atom;                  (* conclusion of the lemma *)
  lemma_premises : atom list;               (* hypotheses of the lemma *)
} (** A lemma is a named formula that can be deduced from a list
      of other named formulas. It will be translated as a datalog rule. *)

type kb = {
  mutable kb_name_idx : int;
  mutable kb_potential_lemmas : lemma list;           (** potential lemma, to explore *)
  mutable kb_patterns : named_formula Patterns.Map.t; (** named formulas, indexed by pattern *)
  kb_formulas : (atom_name, named_formula) Hashtbl.t; (** formulas, by name *)
  kb_theories : (atom_name, theory) Hashtbl.t;        (** theories, by name *)
  mutable kb_lemmas : lemma list;                     (** list of lemmas *)
  mutable kb_proofs : ProofHashSet.t;                 (** proofs already met *)
} (** a Knowledge Base for lemma and theories *)
  
(** Create an empty Knowledge Base *)
let empty_kb () = {
  kb_name_idx = 0;
  kb_potential_lemmas = [];
  kb_patterns = Patterns.Map.create ();
  kb_formulas = Hashtbl.create 5;
  kb_theories = Hashtbl.create 3;
  kb_lemmas = [];
  kb_proofs = ProofHashSet.empty;
}

(** Add a potential lemma to the KB. The lemma must be checked before
    it is used. *)
let add_potential_lemmas kb pot_lemmas =
  let kb_potential_lemmas =
    List.fold_left (fun kb_potential_lemmas lemma ->
      if List.mem lemma kb_potential_lemmas then kb_potential_lemmas
        else lemma :: kb_potential_lemmas)
    kb.kb_potential_lemmas pot_lemmas in
  kb.kb_potential_lemmas <- kb_potential_lemmas

let pp_atom formatter (name, args) =
  let pp_arg formatter = function
    | i when i < 0 -> Format.fprintf formatter "X%d" (-i)
    | i ->
      let symbol = mk_symbol (Datalog.Symbols.get_symbol i) in
      !T.pp_symbol#pp formatter symbol
  in
  Format.fprintf formatter "@[<h>%s(%a)@]"
    name (Utils.pp_list pp_arg) args

let pp_named_formula formatter nf =
  Format.fprintf formatter "@[<h>%s == %a@]"
    (fst nf.nf_atom) Patterns.pp_pclause nf.nf_pclause

let pp_theory formatter theory =
  Format.fprintf formatter "theory %a: %a"
    pp_atom theory.th_atom (Utils.pp_list pp_atom) theory.th_definition

let pp_lemma formatter lemma =
  Format.fprintf formatter "lemma:@ %a :-@ %a"
    pp_atom lemma.lemma_conclusion
    (Utils.pp_list pp_atom) lemma.lemma_premises

(** Pretty print content of KB *)
let pp_kb formatter kb =
  Format.fprintf formatter "@[<v>kb:@;";
  (* print theories *)
  Hashtbl.iter 
    (fun _ th -> Format.fprintf formatter "  @[<h>%a@]@;" pp_theory th)
    kb.kb_theories;
  (* print lemmas *)
  List.iter
    (fun lemma -> Format.fprintf formatter "  @[<hv 2>%a@]@;" pp_lemma lemma)
    kb.kb_lemmas;
  (* print formulas definitions *)
  Hashtbl.iter
    (fun _ nf -> Format.fprintf formatter "  %a@;" pp_named_formula nf)
    kb.kb_formulas;
  Format.fprintf formatter "@]"


(* ----------------------------------------------------------------------
 * reasoning over a problem using Datalog
 * ---------------------------------------------------------------------- *)

type meta_prover = {
  meta_db : Datalog.Logic.db;
  meta_kb : kb;
  mutable meta_theory_symbols : SSet.t;
  mutable meta_theory_clauses : Datalog.Logic.term list Ptmap.t; (* clause -> list of theory terms *)
  mutable meta_ord : ordering;
  mutable meta_lemmas : hclause list;
} (** The main type used to reason over the current proof, detecting axioms
      and theories, inferring lemma... *)

let get_kb_formula ~kb name =
  try Hashtbl.find kb.kb_formulas name
  with Not_found -> failwith ("no such formula: " ^ name)

let get_kb_theory ~kb name =
  try Hashtbl.find kb.kb_theories name
  with Not_found -> failwith ("no such theory: " ^ name)

(** Translate back a datalog (ground) term into a hclause.
    First, the corresponding named formula has to be retrieved
    from kb, then it is 'matched' against the term.
    A proof and a list of parent clauses have to be provided. *)
let term_to_hclause ~ord ~kb term proof parents =
  let term_name = Datalog.Symbols.get_symbol term.(0)
  and term_args = Array.to_list (Array.sub term 1 (Array.length term - 1)) in
  (* get the named formula corresponding to this term *)
  let nf = get_kb_formula kb term_name in
  let nf_name, nf_args = nf.nf_atom in
  (* mapping from free symbols to concrete symbols *)
  let mapping = List.fold_left2
    (fun mapping nf_arg term_arg ->
      assert (nf_arg >= 0);  (* ground atom *)
      let symbol = mk_symbol (Datalog.Symbols.get_symbol term_arg) in
      Patterns.bind_symbol mapping nf_arg symbol)
    Patterns.empty_mapping nf_args term_args
  in
  Patterns.instantiate_pclause ~ord ~map:mapping nf.nf_pclause proof parents

(** This handler is triggered whenever a named formula is discovered
    to be true for the current problem. *)
let handle_formula meta term =
  let ord = meta.meta_ord in
  let kb = meta.meta_kb in
  (* find parents (other formulas) *)
  let explanation = Datalog.Logic.db_explain meta.meta_db term in
  let parents = List.map
    (fun t -> term_to_hclause ~ord ~kb t (Axiom ("kb", "kb")) [])
    explanation
  in
  (* is the clause deduced or merely an axiom? *)
  if explanation = [term]
  then ()
  else begin
    (* proof and parents of conclusion *)
    let premises = List.map (fun hc -> (C.base_clause hc, [], S.id_subst)) parents in
    let proof = Proof ("lemma", premises) in
    (* build conclusion *)
    let conclusion = term_to_hclause ~ord ~kb term proof parents in
    (* yield lemma *)
    Utils.debug 0 (lazy (Utils.sprintf "%% meta-prover: deduced @[<h>%a@]"
                  !C.pp_clause#pp_h conclusion));
    meta.meta_lemmas <- conclusion :: meta.meta_lemmas
  end

(** Handler triggered when a theory is discovered in the current problem *)
let handle_theory meta term =
  let ord = meta.meta_ord in
  let kb = meta.meta_kb in
  Utils.debug 0 (lazy (Utils.sprintf "%% meta-prover: theory @[<h>%a@]"
                 (Datalog.Logic.pp_term ?to_s:None) term));
  (* the clauses that belong to this theory *)
  let premises = Datalog.Logic.db_premises meta.meta_db term in
  let premise_clauses = List.map
    (fun term -> term_to_hclause ~ord ~kb term (Axiom ("kb","kb")) [])
    premises in
  (* add the premises of the clause to the set of theory clauses. Each of those
     clauses keeps the list of theories it belongs to. *)
  List.iter
    (fun hc ->
      let l = try Ptmap.find hc.hctag meta.meta_theory_clauses
              with Not_found -> [] in
      meta.meta_theory_clauses <- Ptmap.add hc.hctag (term::l) meta.meta_theory_clauses)
    premise_clauses;
  (* add the symbols in those clauses to the set of theory symbols *)
  let signature = C.signature premise_clauses in
  let symbols = symbols_of_signature signature in
  meta.meta_theory_symbols <-
    List.fold_left
      (fun set s ->
        (* add the symbol to the theory symbols if it's not a base symbol *)
        if not (SSet.mem s base_symbols) then SSet.add s set else set)
      meta.meta_theory_symbols symbols;
  ()

(** Convert an atom to a Datalog term *)
let atom_to_term (head, args) =
  let head = Datalog.Symbols.mk_symbol head in
  Array.of_list (head :: args)

(** Add a lemma to the Datalog engine *)
let db_add_lemma db lemma =
  (* add conclusion(args) :- premise1(args), ..., premise_n(args), for
     further propagations. *)
  let head = atom_to_term lemma.lemma_conclusion in
  let body = List.map (fun premise -> atom_to_term premise) lemma.lemma_premises in
  let rule = Datalog.Logic.mk_rule head body in
  Utils.debug 2 (lazy (Utils.sprintf "%% add rule @[<h>%a@] to meta-prover"
                 (Datalog.Logic.pp_rule ?to_s:None) rule));
  Datalog.Logic.db_add db rule
  
(** Add the definition of a theory to the Datalog engine *)
let db_add_theory db theory =
  let head = atom_to_term theory.th_atom in
  let body = List.map atom_to_term theory.th_definition in
  let rule = Datalog.Logic.mk_rule head body in
  Utils.debug 2 (lazy (Utils.sprintf "%% add rule @[<h>%a@] to meta-prover"
                 (Datalog.Logic.pp_rule ?to_s:None) rule));
  Datalog.Logic.db_add db rule

(** Create a meta_prover, using a Knowledge Base *)
let create_meta ~ord kb =
  let meta = {
    meta_db = Datalog.Logic.db_create ();
    meta_kb = kb;
    meta_theory_symbols = SSet.empty;
    meta_theory_clauses = Ptmap.empty;
    meta_ord = ord;
    meta_lemmas = [];
  } in
  (* handler for new formulas and theories *)
  let formula_handler = handle_formula meta in
  let theory_handler = handle_theory meta in
  (* add definitions of lemma *) 
  List.iter
    (fun lemma ->
      (* the lemma is f(X,...) :- g(Y...), ...; we need the index of f *)
      let s = Datalog.Symbols.mk_symbol (fst lemma.lemma_conclusion) in
      Datalog.Logic.db_subscribe meta.meta_db s formula_handler;
      (* also add the lemma as a rule *)
      db_add_lemma meta.meta_db lemma)
    meta.meta_kb.kb_lemmas;
  (* add definitions of theories *)
  Hashtbl.iter
    (fun _ theory ->
      (* detect theories *)
      let s = Datalog.Symbols.mk_symbol (fst theory.th_atom) in
      Datalog.Logic.db_subscribe meta.meta_db s theory_handler;
      (* also add the theory as a rule *)
      db_add_theory meta.meta_db theory)
    meta.meta_kb.kb_theories; 
  (* return the meta-prover *)
  meta

(** Update the ordering used by the meta-prover *)
let meta_update_ord ~ord meta = meta.meta_ord <- ord

(** Scan the given clause to recognize if it matches axioms from the KB;
    if it does, return the lemma that are newly discovered by the Datalog engine.

    It returns lemma that have been discovered by adding the clause. Those
    lemma can be safely added to the problem.
    *)
let scan_clause meta hc =
  meta.meta_lemmas <- [];
  (* retrieve patterns that match this clause *)
  Patterns.Map.retrieve meta.meta_kb.kb_patterns hc ()
    (fun () pclause mapping nf ->
      (* a named formula is detected, assert the corresponding datalog
         predicate *)
      let head, args = nf.nf_atom in
      let open Patterns in
      let args = List.map 
        (function
          | i when i < 0 -> i
          | i -> (* translate from symbol to datalog symbol *)
            let symbol = Ptmap.find i mapping.m_symbol in
            Datalog.Symbols.mk_symbol (name_symbol symbol))
        args
      in
      let term = atom_to_term (head, args) in
      let rule = Datalog.Logic.mk_rule term [] in
      if not (Datalog.Logic.db_mem meta.meta_db rule) then begin
        (* add fact if not already present *)
        Utils.debug 0 (lazy (Utils.sprintf "%% meta-prover: property @[<h>%a@]"
                       (Datalog.Logic.pp_rule ?to_s:None) rule));
        Datalog.Logic.db_add meta.meta_db rule
      end);
  (* get lemmas, and clear the list for next use *)
  let lemmas = meta.meta_lemmas in
  meta.meta_lemmas <- [];
  lemmas

(* ----------------------------------------------------------------------
 * Some builtin theories, axioms and lemma
 * ---------------------------------------------------------------------- *)

(** Add a list of named formulas to the KB *)
let add_named kb named =
  List.iter
    (fun nf ->
      let name, _ = nf.nf_atom in
      Hashtbl.replace kb.kb_formulas name nf;
      Patterns.Map.add kb.kb_patterns nf.nf_pclause nf)
    named

(** Add a list of lemmas to the KB *)
let add_lemmas kb lemmas =
  kb.kb_lemmas <- List.rev_append lemmas kb.kb_lemmas

(*Add a list of theories to the KB *)
let add_theories kb theories =
  List.iter
    (fun th ->
      let th_name = fst th.th_atom in
      Hashtbl.replace kb.kb_theories th_name th)
    theories

(** Add builtin lemma, axioms, theories to the KB *)
let add_builtin ~ord kb =
  (* From a string, extract a pclause *)
  let from_str ~ord name s =
    let simple = Parser_tptp.parse_clause Lexer_tptp.token (Lexing.from_string s) in
    let ord = Orderings.default_ordering (Simple.signature [simple]) in
    let hc = C.from_simple ~ord (simple, Simple.Axiom ("builtin", name)) in
    let pc = Patterns.pclause_of_clause hc in
    let atom = name, pc.Patterns.pc_vars in
    Utils.debug 2 (lazy (Utils.sprintf "%% axiom %s has pattern @[<h>%a@]"
                   name Patterns.pp_pclause pc));
    { nf_atom = atom; nf_pclause = pc; }
  in
  let assoc = from_str ~ord "associative" "$$f(X,$$f(Y,Z)) = $$f($$f(X,Y),Z)"
  and commut = from_str ~ord "commutative" "$$f(X,Y) = $$f(Y,X)"
  and functional = from_str ~ord "functional3" "~$$p(X,Y,Z) | ~$$p(X,Y,Z2) | Z=Z2"
  and total = from_str ~ord "total3" "$$p(X,Y,$$f(X,Y))"
  and functional_total = from_str ~ord "total_function3" "$$p(X,Y,Z) <=> ($$f(X,Y)=Z)"
  in
  (* add named formulas *)
  add_named kb [assoc; commut; functional; total; functional_total];
  (* add the functional total lemma *)
  let mk_var i = -1-i in
  let lemma = {
    lemma_conclusion = "total_function3", [mk_var 0; mk_var 1;];
    lemma_premises = ["functional3", [mk_var 0]; "total3", [mk_var 1; mk_var 0]];
  } in
  add_lemmas kb [lemma];
  (* add the AC theory *)
  let th = {
    th_atom = "ac", [mk_var 0];
    th_definition = ["associative", [mk_var 0]; "commutative", [mk_var 0]];
  } in
  add_theories kb [th];
  ()

(** Add theories and named formulas from file to the KB *)
let parse_theory_file filename kb =
  ()  (* TODO *)

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
  Utils.debug 2 (lazy (Utils.sprintf
                "%% simplicity of @[<h>%a@] is %.2f (%d symbols, %d lits)"
                Patterns.pp_pclause pclause !rate num_symbols length));
  !rate 

(** 'cost', or handicap, of a symbol *)
let cost_symbol ~is_theory_symbol signature s =
  if is_theory_symbol s then 0.2
  else
    let arity = try fst (SMap.find s signature) with Not_found -> 0 in
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
  (* many literals is not simple *)
  let length = Array.length hc.hclits in
  rate := !rate +. (2. *. float_of_int (length - 1));
  (* result *)
  Utils.debug 2 (lazy (Utils.sprintf
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

(** A possible lemma, i.e. a subgraph *)
type candidate_lemma = {
  cl_conclusion : hclause;
  cl_premises : hclause list;
  cl_theories : term list;
  cl_rate : float;
}

module CostMap = Map.Make(
  struct
    type t = hclause * int
    let compare (hc1,d1) (hc2,d2) = if d1 <> d2 then d1 - d2 else hc1.hctag - hc2.hctag
  end)

(** Explore parents of the clause, looking for clauses that are simple
    or for clauses that belong to a theory.
    [distance] is the distance between [hc] and the root of the proof.
    [cost_map]: for [c] a hclause, stores the best (rate, list of clauses) where
       the list of clauses is a proof for [c] (best means with lowest rate
       but max distance from the clause)
    It returns a pair (rate, list of premises). *)
let explore_parents meta cost_map distance hc =
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
      let best_cost, best_premises = match hc.hcproof with
      | Axiom _ -> cost_hc /. 2., [hc]  (* axioms are cheaper *)
      | Proof (_, l) ->
        (* cost of recursing into parents *)
        let cost_parents, premises_parents = List.fold_left
          (fun (cost,premises) (parent,_,_) ->
            let cost', premises' = compute_best (distance+1) parent.cref in
            cost +. cost', premises @ premises')
          (0., []) l
        in
        (* make the choice that gives the lowest cost *)
        if cost_parents < cost_hc then (cost_parents, premises_parents) else (cost_hc, [hc])
      in
      (* memoize and return *)
      cost_map := CostMap.add (hc, distance) (best_cost, best_premises) !cost_map;
      (best_cost, best_premises)
  in
  (* return the best choice of premises for this (clause, distance) *)
  compute_best distance hc

(** Convert a candidate_lemma to a proper lemma *)
let candidate_to_lemma cl = failwith "not implemented"  (* TODO *)

(** given an empty clause (and its proof), look in the proof for lemmas. *)
let search_lemmas meta hc =
  assert (hc.hclits = [||]);
  let is_theory_symbol s = SSet.mem s meta.meta_theory_symbols in
  (* depth of the proof *)
  let depth = proof_depth hc in
  Utils.debug 1 (lazy (Utils.sprintf "%% analyse proof of depth %d" depth));
  (* candidate lemmas *)
  let cost_map = ref CostMap.empty in
  let candidates = ref [] in
  let explored = ref C.CSet.empty in
  let q = Queue.create () in
  (* breadth-first exploration of the proof. The depth is kept with the clause *)
  Queue.push (hc, 0) q;
  while not (Queue.is_empty q) do
    let hc, depth = Queue.pop q in
    if C.CSet.mem !explored hc then () else begin
      (* clause not already explored *)
      explored := C.CSet.add !explored hc;
      (* absolute rate of the clause itself *)
      let hc_rate = rate_clause ~is_theory_symbol hc in
      (* choice of premises, with associate cost *)
      let premises_rate, premises = explore_parents meta cost_map 0 hc in
      (* build the candidate *)
      let cl = {
        cl_conclusion = hc;
        cl_premises = premises;
        cl_theories = [];  (* TODO convert some premises to theories *)
        cl_rate = hc_rate +. premises_rate;
      } in
      candidates := cl :: !candidates;
    end
  done;
  (* sort candidate by increasing rate (bad candidates at the end), and take
     only a given amount of them. *)
  let candidates = List.sort
    (fun cl1 cl2 -> int_of_float (cl1.cl_rate -. cl2.cl_rate))
    !candidates in
  let candidates = FoUtils.list_take !max_lemmas candidates in
  (* convert the candidate lemma to a lemma *)
  let lemmas = List.map candidate_to_lemma candidates in
  lemmas

(** Update the KB of this meta-prover by learning from
    the given (empty) clause's proof. The KB is modified
    in place. *)
let learn_and_update meta hc =
  let kb = meta.meta_kb in
  let h = hash_proof hc in
  if ProofHashSet.mem h kb.kb_proofs
  then Utils.debug 0 (lazy (Utils.sprintf "%% proof %Ld already processed" h))
  else begin
    (* this proof is not known yet, learn from it *)
    kb.kb_proofs <- ProofHashSet.add h kb.kb_proofs;
    let lemmas = search_lemmas meta hc in
    List.iter
      (fun lemma -> Format.printf "%%  learn @[<h>%a@]@." pp_lemma lemma)
      lemmas;
    (* store new lemmas *)
    add_lemmas kb lemmas
  end

(* ----------------------------------------------------------------------
 * serialization/deserialization for abstract logic structures
 * ---------------------------------------------------------------------- *)

exception ReadKB of kb

(* read KB without locking (may crash if wrong format) *)
let read_kb_nolock filename =
  try
    let file = Unix.openfile filename [Unix.O_RDONLY] 0o644 in
    let file = Unix.in_channel_of_descr file in
    let kb = (Marshal.from_channel file : kb) in
    close_in file;
    kb
  with
  | Unix.Unix_error _ -> empty_kb ()
  | Failure e -> Format.printf "%% [error while reading %s: %s]" filename e; empty_kb ()

let read_kb ~lock ~file =
  Utils.with_lock_file lock
    (fun () -> read_kb_nolock file)

let save_kb ~lock ~file kb =
  Utils.with_lock_file lock
    (fun () ->
    let out = Unix.openfile file [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
    let out = Unix.out_channel_of_descr out in
    Marshal.to_channel out kb [];
    flush out;
    close_out out)

let update_kb ~lock ~file f =
  Format.printf "%% update knowledge base...@.";
  Utils.with_lock_file lock
    (fun () ->
    let kb = read_kb_nolock file in
    (* tranform kb with function *)
    let kb = f kb in
    (* write modified kb to file *)
    let out = Unix.openfile file [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
    let out = Unix.out_channel_of_descr out in
    Marshal.to_channel out kb [];
    flush out;
    close_out out);
  Format.printf "%% ... done@."
