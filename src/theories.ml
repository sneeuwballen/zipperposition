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

(* TODO analyse lemmas to replace a subset of their premises by corresponding theory *)
(* TODO associate induction schema to theories *)

let prof_scan_clause = Utils.mk_profiler "Theories.scan_clause"

let stat_lemma_deduced = mk_stat "lemmas deduced"
let stat_theory_detected = mk_stat "theory detected"
let stat_formula_detected = mk_stat "formulas detected"

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

(** Convert an atom to a Datalog term *)
let atom_to_term (head, args) =
  let head = Datalog.Symbols.mk_symbol head in
  Array.of_list (head :: args)

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

let rule_of_lemma lemma =
  let head = atom_to_term lemma.lemma_conclusion in
  let body = List.map (fun premise -> atom_to_term premise) lemma.lemma_premises in
  let rule = Datalog.Logic.mk_rule head body in
  rule

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

(** Find a new name for a formula *)
let next_name ~prefix kb =
  let n = kb.kb_name_idx in
  kb.kb_name_idx <- n + 1;
  Utils.sprintf "%s_%d" prefix n

let pp_atom formatter (name, args) =
  let pp_arg formatter = function
    | i when i < 0 -> Format.fprintf formatter "X%d" (-i)
    | i -> Patterns.pp_symb formatter i
  in
  Format.fprintf formatter "@[<h>%s(%a)@]"
    name (Utils.pp_list pp_arg) args

let pp_named_formula formatter nf =
  Format.fprintf formatter "@[<h>%a is %a.@]"
    pp_atom nf.nf_atom Patterns.pp_pclause nf.nf_pclause

let pp_theory formatter theory =
  Format.fprintf formatter "theory %a is %a."
    pp_atom theory.th_atom (Utils.pp_list ~sep:" and " pp_atom) theory.th_definition

let pp_lemma formatter lemma =
  Format.fprintf formatter "lemma %a if @;%a."
    pp_atom lemma.lemma_conclusion
    (Utils.pp_list ~sep:" and " pp_atom) lemma.lemma_premises

(** Pretty print content of KB *)
let pp_kb formatter kb =
  Format.fprintf formatter "@[<v2>%% kb:@;";
  (* print formulas definitions *)
  Format.fprintf formatter "@[<v2>%% named formulas:@;";
  let formulas = ref [] in
  Hashtbl.iter (fun _ nf -> formulas := nf :: !formulas) kb.kb_formulas;
  List.iter
    (fun nf -> Format.fprintf formatter "%a@;" pp_named_formula nf)
    (List.sort (fun nf1 nf2 -> compare nf1.nf_atom nf2.nf_atom) !formulas);
  Format.fprintf formatter "@]@;";
  (* print theories *)
  Format.fprintf formatter "@[<v2>%% theories:@;";
  let theories = ref [] in
  Hashtbl.iter (fun _ x -> theories := x :: !theories) kb.kb_theories;
  List.iter
    (fun th -> Format.fprintf formatter "@[<h>%a@]@;" pp_theory th)
    (List.sort (fun th1 th2 -> compare th1.th_atom th2.th_atom) !theories);
  Format.fprintf formatter "@]@;";
  (* print lemmas *)
  Format.fprintf formatter "@[<v2>%% lemmas:@;";
  List.iter
    (fun lemma -> Format.fprintf formatter "@[<hv 2>%a@]@;" pp_lemma lemma)
    kb.kb_lemmas;
  Format.fprintf formatter "@]@;";
  Format.fprintf formatter "@]"

(** Print statistics about KB *)
let pp_kb_stats formatter kb =
  Format.fprintf formatter "@[<h>KB stats: %d formulas, %d lemmas, %d theories@]"
    (Hashtbl.length kb.kb_formulas)
    (List.length kb.kb_lemmas)
    (Hashtbl.length kb.kb_theories)

(** Add a potential lemma to the KB. The lemma must be checked before
    it is used. *)
let add_potential_lemmas kb pot_lemmas =
  let kb_potential_lemmas =
    List.fold_left (fun kb_potential_lemmas lemma ->
      if List.mem lemma kb_potential_lemmas then kb_potential_lemmas
        else lemma :: kb_potential_lemmas)
    kb.kb_potential_lemmas pot_lemmas in
  kb.kb_potential_lemmas <- kb_potential_lemmas

(** Add a list of named formulas to the KB *)
let add_named kb named =
  List.iter
    (fun nf ->
      let name, _ = nf.nf_atom in
      if Hashtbl.mem kb.kb_formulas name then () else begin
        (* no formula with this name is already present *)
        Utils.debug 1 (lazy (Utils.sprintf "%%   add new formula %a" pp_named_formula nf));
        Hashtbl.replace kb.kb_formulas name nf;
        Patterns.Map.add kb.kb_patterns nf.nf_pclause nf
      end)
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

(* ----------------------------------------------------------------------
 * reasoning over a problem using Datalog
 * ---------------------------------------------------------------------- *)

module TermMap = Map.Make(
  struct type t = Datalog.Logic.term let compare = compare end)

type meta_prover = {
  meta_db : Datalog.Logic.db;
  meta_kb : kb;
  mutable meta_clauses : hclause TermMap.t; (* map terms to hclauses *)
  mutable meta_theories : Datalog.Logic.term list;  (* detected theories *)
  mutable meta_theory_symbols : SSet.t;
  mutable meta_theory_clauses : Datalog.Logic.term list Ptmap.t; (* clause -> list of theory terms *)
  mutable meta_ord : ordering;
  mutable meta_lemmas : hclause list; (* temp buffer of deduced lemmas *)
} (** The main type used to reason over the current proof, detecting axioms
      and theories, inferring lemma... *)

let get_kb_formula ~kb name = Hashtbl.find kb.kb_formulas name

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
    (fun t ->  (* find clause that triggered hypotheses *)
      try TermMap.find t meta.meta_clauses
      with Not_found ->
        failwith (Utils.sprintf "no clause backs term %a"
                  (Datalog.Logic.pp_term ?to_s:None) t))
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
    incr_stat stat_lemma_deduced;
    meta.meta_lemmas <- conclusion :: meta.meta_lemmas;
    (* remember that the term maps to this clause *)
    meta.meta_clauses <- TermMap.add term conclusion meta.meta_clauses
  end

(** Handler triggered when a theory is discovered in the current problem *)
let handle_theory meta term =
  let ord = meta.meta_ord in
  let kb = meta.meta_kb in
  Utils.debug 0 (lazy (Utils.sprintf "%% meta-prover: theory @[<h>%a@]"
                 (Datalog.Logic.pp_term ?to_s:None) term));
  incr_stat stat_theory_detected;
  (* the clauses that belong to this theory *)
  let premises = Datalog.Logic.db_premises meta.meta_db term in
  let premise_clauses = Utils.list_flatmap
    (fun term -> try [term_to_hclause ~ord ~kb term (Axiom ("kb","kb")) []]
                 with Not_found -> [])
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
  (* add the theory to the set of detect theories *)
  meta.meta_theories <- term :: meta.meta_theories;
  ()

(** Add a lemma to the Datalog engine *)
let db_add_lemma db lemma =
  (* add conclusion(args) :- premise1(args), ..., premise_n(args), for
     further propagations. *)
  let rule = rule_of_lemma lemma in
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
    meta_clauses = TermMap.empty;
    meta_theories = [];
    meta_theory_symbols = SSet.empty;
    meta_theory_clauses = Ptmap.empty;
    meta_ord = ord;
    meta_lemmas = [];
  } in
  Utils.debug 1 (lazy (Utils.sprintf
                 "%% meta-prover: kb contains %d lemmas, %d theories, %d named formulas"
                 (List.length kb.kb_lemmas) (Hashtbl.length kb.kb_theories)
                 (Hashtbl.length kb.kb_formulas)));
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
  Utils.enter_prof prof_scan_clause;
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
        Utils.debug 0 (lazy (Utils.sprintf "%% meta-prover: property @[<h>%a where %a@]"
                       (Datalog.Logic.pp_rule ?to_s:None) rule pp_named_formula nf));
        incr_stat stat_formula_detected;
        (* remember the clause that made us add the fact to datalog *)
        meta.meta_clauses <- TermMap.add term hc meta.meta_clauses;
        (* add the rule to datalog *)
        Datalog.Logic.db_add meta.meta_db rule;
      end);
  (* get lemmas, and clear the list for next use *)
  let lemmas = meta.meta_lemmas in
  meta.meta_lemmas <- [];
  Utils.exit_prof prof_scan_clause;
  lemmas

(* ----------------------------------------------------------------------
 * Some builtin theories, axioms and lemma
 * ---------------------------------------------------------------------- *)

type disjunction = Lemma of lemma | Theory of theory | Named of named_formula

(** Add theories and named formulas from file to the KB *)
let load_theory kb disjunctions =
  List.iter
    (function
     | Lemma lemma -> add_lemmas kb [lemma]
     | Theory th -> add_theories kb [th]
     | Named n -> add_named kb [n])
    disjunctions

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
  let kb = Utils.with_lock_file lock
    (fun () ->
    let kb = read_kb_nolock file in
    (* tranform kb with function *)
    let kb = f kb in
    (* write modified kb to file *)
    let out = Unix.openfile file [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
    let out = Unix.out_channel_of_descr out in
    Marshal.to_channel out kb [];
    flush out;
    close_out out;
    kb)
  in
  Format.printf "%% ... done@.";
  kb

let clear_kb ~lock ~file =
  Utils.with_lock_file lock
    (fun () ->
      Format.printf "%% clear Knowledge Base stored in %s@." file;
      try Unix.unlink file
      with Unix.Unix_error (e,_,_) ->
        Format.printf "%% error: %s@." (Unix.error_message e))
