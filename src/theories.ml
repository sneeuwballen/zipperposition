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
let prof_datalog = Utils.mk_profiler "datalog_add"

let stat_lemma_deduced = mk_stat "lemmas deduced"
let stat_theory_detected = mk_stat "theory detected"
let stat_formula_detected = mk_stat "formulas detected"

(* ----------------------------------------------------------------------
 * generic representation of theories and lemmas (persistent)
 * ---------------------------------------------------------------------- *)

(** Datalog engine using same symbols as the rest of the prover *)
module Logic = Datalog.Logic.Make(
  struct
    type t = symbol
    let equal s1 s2 = s1 == s2
    let hash s = hash_symbol s
    let to_string s = name_symbol s
    let of_string s = mk_symbol s
  end)

type atom_name = symbol
  (** The name of a formula. If a formula is part of a known axiomatisation
      it can have a specific name, otherwise just "lemma_X" with X a number
      (e.g. f(X,Y)=f(Y,X) is named "commutativity") *)

module SHashtbl = Symbols.SHashtbl

type atom = atom_name * [`Var of int | `Symbol of atom_name] list
  (** An atom in the meta level of reasoning. This represents a fact about
      the current proof search (presence of a theory, of a clause, of a lemma... *)

let compare_atom (n1, args1) (n2, args2) =
  let compare_args a1 a2 = match a1, a2 with
  | `Var i, `Var j -> i - j
  | `Symbol s1, `Symbol s2 -> Symbols.compare_symbols s1 s2
  | `Var _, `Symbol _ -> -1
  | `Symbol _, `Var _ -> 1
  in
  if n1 != n2 then Symbols.compare_symbols n1 n2
  else Utils.lexicograph compare_args args1 args2

(** Convert an atom to a Datalog literal *)
let atom_to_literal (head, args) = Logic.mk_literal head args

type named_formula = Patterns.named_pattern
  (** A named formula is a pattern clause, plus a name (used for the datalog
      representation of instances of this formula *)

type theory = {
  th_atom : atom;                           (* meta-atom for the theory *)
  th_definition : atom list;                (* definition (set of axioms) *)
} (** A theory is a named set of formulas (axioms) *)

type lemma = {
  lemma_conclusion : atom;                  (* conclusion of the lemma *)
  lemma_premises : atom list;               (* hypotheses of the lemma *)
} (** A lemma is a named formula that can be deduced from a list
      of other named formulas. It will be translated as a datalog clause. *)

(** Arbitrary lexicographic comparison of lemmas *)
let compare_lemma l1 l2 =
  let cmp = compare_atom l1.lemma_conclusion l2.lemma_conclusion in
  if cmp <> 0 then cmp
  else Utils.lexicograph compare_atom l1.lemma_premises l2.lemma_premises

let clause_of_lemma lemma =
  let head = atom_to_literal lemma.lemma_conclusion in
  let body = List.map (fun premise -> atom_to_literal premise) lemma.lemma_premises in
  let clause = Logic.mk_clause head body in
  clause

(** Set of lemmas *)
module LemmaSet = Set.Make(struct type t = lemma let compare = compare_lemma end)

type kb = {
  mutable kb_name_idx : int;
  mutable kb_patterns : named_formula Patterns.Map.t; (** named formulas, indexed by pattern *)
  kb_formulas : named_formula SHashtbl.t;             (** formulas, by name *)
  kb_theories : theory SHashtbl.t;                    (** theories, by name *)
  mutable kb_lemmas : LemmaSet.t;                     (** set of lemmas *)
} (** a Knowledge Base for lemma and theories *)
  
(** Create an empty Knowledge Base *)
let empty_kb () = {
  kb_name_idx = 0;
  kb_patterns = Patterns.Map.create ();
  kb_formulas = SHashtbl.create 5;
  kb_theories = SHashtbl.create 3;
  kb_lemmas = LemmaSet.empty;
}

(** Prefix for formulas that do not have a name added by a user *)
let prefix = "anon_f_"

(** Update the name-idx, if some named formula has a bigger number *)
let update_name_idx kb named =
  try Scanf.sscanf (name_symbol named.Patterns.np_name) "anon_f_%d"
    (fun i -> kb.kb_name_idx <- max kb.kb_name_idx (i+1))
  with Scanf.Scan_failure _ -> ()

(** Find a new name for a formula *)
let next_name kb =
  let n = kb.kb_name_idx in
  kb.kb_name_idx <- n + 1;
  mk_symbol (Utils.sprintf "%s%d" prefix n)

(** Add a list of named formulas to the KB *)
let add_named kb named =
  List.iter
    (fun nf ->
      let name = nf.Patterns.np_name in
      (* if formula is anonymous, update fresh name index *)
      update_name_idx kb nf;
      if SHashtbl.mem kb.kb_formulas name then () else begin
        (* no formula with this name is already present *)
        SHashtbl.replace kb.kb_formulas name nf;
        Patterns.Map.add kb.kb_patterns nf.Patterns.np_pattern nf
      end)
    named

(** Add a list of lemmas to the KB *)
let add_lemmas kb lemmas =
  kb.kb_lemmas <- List.fold_left
    (fun set lemma -> LemmaSet.add lemma set)
    kb.kb_lemmas lemmas

(*Add a list of theories to the KB *)
let add_theories kb theories =
  List.iter
    (fun th ->
      let th_name = fst th.th_atom in
      SHashtbl.replace kb.kb_theories th_name th)
    theories

type disjunction =
  | Lemma of lemma
  | Theory of theory
  | Named of named_formula
  (** Type of an entry in a Knowledge Base file *)

(** Add parsed content to the KB *)
let load_kb kb disjunctions =
  Sequence.iter
    (function
     | Lemma lemma -> add_lemmas kb [lemma]
     | Theory th -> add_theories kb [th]
     | Named n -> add_named kb [n])
    disjunctions

(** Dump content of the KB as a sequence of disjunctions *)
let dump_kb kb =
  let iter k =
    SHashtbl.iter (fun _ nf -> k (Named nf)) kb.kb_formulas;
    SHashtbl.iter (fun _ th -> k (Theory th)) kb.kb_theories;
    LemmaSet.iter (fun lemma -> k (Lemma lemma)) kb.kb_lemmas; 
    ()
  in
  Sequence.from_iter iter

let pp_atom ?(var_prefix="X") formatter (name, args) =
  let pp_arg formatter = function
    | `Var i -> Format.fprintf formatter "%s%d" var_prefix (-i)
    | `Symbol s -> !T.pp_symbol#pp formatter s
  in
  match args with
  | [] -> Format.fprintf formatter "@[<h>%a@]" !T.pp_symbol#pp name
  | args -> Format.fprintf formatter "@[<h>%a(%a)@]"
    !T.pp_symbol#pp name (Utils.pp_list pp_arg) args

let pp_named_formula formatter nf =
  Format.fprintf formatter "@[<hv>%a@]." Patterns.pp_named_pattern nf

let pp_theory formatter theory =
  let var_prefix = "f" in
  Format.fprintf formatter "theory @[<h>%a@] is@ @[<hv2>%a@]."
    (pp_atom ~var_prefix) theory.th_atom
    (Utils.pp_list ~sep:" and " (pp_atom ~var_prefix)) theory.th_definition

let pp_lemma formatter lemma =
  let var_prefix = "f" in
  Format.fprintf formatter "lemma @[<h>%a@] if@ @[<hv2>%a@]."
    (pp_atom ~var_prefix) lemma.lemma_conclusion
    (Utils.pp_list ~sep:" and " (pp_atom ~var_prefix)) lemma.lemma_premises

(** Print the disjunction in a human readable form *)
let pp_disjunction formatter = function
  | Lemma l -> pp_lemma formatter l
  | Theory th -> pp_theory formatter th
  | Named n -> pp_named_formula formatter n

let pp_disjunctions formatter seq =
  Format.fprintf formatter "@[<v>%% vim:syntax=ocaml@;";
  Sequence.pp_seq ~sep:"" pp_disjunction formatter seq;
  Format.fprintf formatter "@]@."

(** Pretty print content of KB *)
let pp_kb formatter kb =
  Format.fprintf formatter "@[<v>%% kb:@;";
  pp_disjunctions formatter (dump_kb kb);
  Format.fprintf formatter "@]"

(** Print statistics about KB *)
let pp_kb_stats formatter kb =
  Format.fprintf formatter "@[<h>KB stats: %d formulas, %d lemmas, %d theories@]"
    (SHashtbl.length kb.kb_formulas)
    (LemmaSet.cardinal kb.kb_lemmas)
    (SHashtbl.length kb.kb_theories)

(* ----------------------------------------------------------------------
 * reasoning over a problem using Datalog
 * ---------------------------------------------------------------------- *)

module TermMap = Map.Make(struct type t = Logic.literal let compare = Logic.compare_literal end)

type meta_prover = {
  meta_db : Logic.db;
  meta_kb : kb;
  mutable meta_clauses : hclause TermMap.t; (* map terms to hclauses *)
  mutable meta_theories : Logic.literal list;  (* detected theories *)
  mutable meta_theory_symbols : SSet.t;
  mutable meta_theory_clauses : Logic.literal list Ptmap.t; (* clause -> list of theory terms *)
  mutable meta_ctx : context;
  mutable meta_lemmas : hclause list; (* temp buffer of deduced lemmas *)
} (** The main type used to reason over the current proof, detecting axioms
      and theories, inferring lemma... *)

let get_kb_formula ~kb name = SHashtbl.find kb.kb_formulas name

let get_kb_theory ~kb name =
  try SHashtbl.find kb.kb_theories name
  with Not_found -> failwith ("no such theory: " ^ (name_symbol name))

(** Translate back a datalog (ground) literal into a hclause.
    First, the corresponding named formula has to be retrieved
    from kb, then it is 'matched' against the literal.
    A proof and a list of parent clauses have to be provided. *)
let lit_to_hclause ~ctx ~kb literal proof =
  let lit_name, lit_args = Logic.open_literal literal in
  let lit_args = List.map
    (function
     | `Var _ -> assert false
     | `Symbol s -> `Symbol s)
    lit_args
  in
  (* get the named formula corresponding to this literal *)
  let nf = get_kb_formula kb lit_name in
  (* instantiate the named formula *)
  let hc = Patterns.instantiate_np ~ctx nf (lit_name, lit_args) proof in
  hc

(** This handler is triggered whenever a named formula is discovered
    to be true for the current problem. *)
let handle_formula meta lit =
  let ctx = meta.meta_ctx in
  let kb = meta.meta_kb in
  (* find parents (other formulas) *)
  let explanation = Logic.db_explain meta.meta_db lit in
  let parents = List.map
    (fun t ->  (* find clause that triggered hypotheses *)
      try TermMap.find t meta.meta_clauses
      with Not_found ->
        failwith (Utils.sprintf "no clause justifies literal %a" Logic.pp_literal t))
    explanation
  in
  (* is the clause deduced or merely an axiom? *)
  if explanation = [lit]
  then ()
  else begin
    (* proof and parents of conclusion *)
    let premises = List.map (fun hc -> hc.hcproof) parents in
    (* build conclusion *)
    let proof c = Proof.mk_proof c "lemma" premises in
    let conclusion = lit_to_hclause ~ctx ~kb lit proof in
    C.set_flag C.flag_lemma conclusion true;
    (* yield lemma *)
    Utils.debug 0 "%% meta-prover: deduced @[<h>%a@]" !C.pp_clause#pp_h conclusion;
    incr_stat stat_lemma_deduced;
    meta.meta_lemmas <- conclusion :: meta.meta_lemmas;
    (* remember that the term maps to this clause *)
    meta.meta_clauses <- TermMap.add lit conclusion meta.meta_clauses
  end

(** Handler triggered when a theory is discovered in the current problem *)
let handle_theory meta lit =
  let ctx = meta.meta_ctx in
  let kb = meta.meta_kb in
  Utils.debug 0 "%% meta-prover: theory @[<h>%a@]" Logic.pp_literal lit;
  incr_stat stat_theory_detected;
  (* the clauses that belong to this theory *)
  let premises = Logic.db_explain meta.meta_db lit in
  let proof c = Proof.mk_axiom c "kb" "kb" in
  let premise_clauses = Utils.list_flatmap
    (fun lit -> try [lit_to_hclause ~ctx ~kb lit proof]
                 with Not_found -> [])
    premises in
  (* add the premises of the clause to the set of theory clauses. Each of those
     clauses keeps the list of theories it belongs to. *)
  List.iter
    (fun hc ->
      let l = try Ptmap.find hc.hctag meta.meta_theory_clauses
              with Not_found -> [] in
      meta.meta_theory_clauses <- Ptmap.add hc.hctag (lit::l) meta.meta_theory_clauses)
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
  meta.meta_theories <- lit :: meta.meta_theories;
  ()

(** Add a lemma to the Datalog engine *)
let db_add_lemma db lemma =
  (* add conclusion(args) :- premise1(args), ..., premise_n(args), for
     further propagations. *)
  let clause = clause_of_lemma lemma in
  Utils.debug 2 "%% add clause @[<h>%a@] to meta-prover"
                 Logic.pp_clause clause;
  Logic.db_add db clause
  
(** Add the definition of a theory to the Datalog engine *)
let db_add_theory db theory =
  let head = atom_to_literal theory.th_atom in
  let body = List.map atom_to_literal theory.th_definition in
  let clause = Logic.mk_clause head body in
  Utils.debug 2 "%% add clause @[<h>%a@] to meta-prover" Logic.pp_clause clause;
  Logic.db_add db clause 

(** Create a meta_prover, using a Knowledge Base *)
let create_meta ~ctx kb =
  let meta = {
    meta_db = Logic.db_create ();
    meta_kb = kb;
    meta_clauses = TermMap.empty;
    meta_theories = [];
    meta_theory_symbols = SSet.empty;
    meta_theory_clauses = Ptmap.empty;
    meta_ctx = ctx;
    meta_lemmas = [];
  } in
  Utils.debug 1 "%% meta-prover: kb contains %d lemmas, %d theories, %d named formulas"
                 (LemmaSet.cardinal kb.kb_lemmas) (SHashtbl.length kb.kb_theories)
                 (SHashtbl.length kb.kb_formulas);
  (* handler for new formulas and theories *)
  let formula_handler = handle_formula meta in
  let theory_handler = handle_theory meta in
  (* add definitions of lemma *) 
  LemmaSet.iter
    (fun lemma ->
      (* the lemma is f(X,...) :- g(Y...), ...; we need the index of f *)
      let s = fst lemma.lemma_conclusion in
      Logic.db_subscribe meta.meta_db s formula_handler;
      (* also add the lemma as a clause *)
      db_add_lemma meta.meta_db lemma)
    meta.meta_kb.kb_lemmas;
  (* add definitions of theories *)
  SHashtbl.iter
    (fun _ theory ->
      (* detect theories *)
      let s = fst theory.th_atom in
      Logic.db_subscribe meta.meta_db s theory_handler;
      (* also add the theory as a rule *)
      db_add_theory meta.meta_db theory)
    meta.meta_kb.kb_theories; 
  (* return the meta-prover *)
  meta

(** Update the ordering used by the meta-prover *)
let meta_update_ctx ~ctx meta = meta.meta_ctx <- ctx

(** Scan the given clause to recognize if it matches axioms from the KB;
    if it does, return the lemma that are newly discovered by the Datalog engine.

    It returns lemma that have been discovered by adding the clause. Those
    lemma can be safely added to the problem. *)
let scan_clause meta hc =
  Utils.enter_prof prof_scan_clause;
  meta.meta_lemmas <- [];
  (* retrieve patterns that match this clause *)
  Patterns.Map.retrieve meta.meta_kb.kb_patterns hc.hclits ()
    (fun () pclause mapping nf ->
      (* abstract the named pattern into a Datalog term *)
      let (head, args) = Patterns.abstract_np ~map:mapping nf in
      let args = List.map
        (fun (`Symbol s) -> `Symbol s)
        args in
      let term = atom_to_literal (head, args) in
      (* assert the fact in the Datalog engine *)
      let clause = Logic.mk_clause term [] in
      if not (Logic.db_mem meta.meta_db clause) then begin
        (* add fact if not already present *)
        Utils.debug 0 "%% meta-prover: property @[<h>%a where %a@]"
                       Logic.pp_clause clause pp_named_formula nf;
        incr_stat stat_formula_detected;
        (* remember the clause that made us add the fact to datalog *)
        meta.meta_clauses <- TermMap.add term hc meta.meta_clauses;
        (* add the clause to datalog *)
        Utils.enter_prof prof_datalog;
        Logic.db_add meta.meta_db clause;
        Utils.exit_prof prof_datalog;
      end);
  (* get lemmas, and clear the list for next use *)
  let lemmas = meta.meta_lemmas in
  meta.meta_lemmas <- [];
  Utils.exit_prof prof_scan_clause;
  lemmas

(** {2 Json encoding} *)

let lemma_to_json lemma = failwith "nope"

let lemma_of_json json = failwith "nope"

let theory_to_json th = failwith "nope"

let theory_of_json json = failwith "nope"

let kb_to_json kb = failwith "nope"

let kb_of_json kb = failwith "nope"

let kb_printer_json formatter disjunctions = failwith "nope"

(* ----------------------------------------------------------------------
 * serialization/deserialization for abstract logic structures
 * ---------------------------------------------------------------------- *)

type kb_parser = in_channel -> disjunction Sequence.t
  (** A parser reads a sequence of disjunctions from a channel *)

type kb_printer = Format.formatter -> disjunction Sequence.t -> unit
  (** A printer prints a sequence of disjunction on a channel *)

(** parse content of the file (as a list of disjunctions), and add it to the KB *)
let read_kb ~file ~kb_parser kb =
  try
    let input = Unix.openfile file [Unix.O_RDONLY] 0o644 in
    let input = Unix.in_channel_of_descr input in
    let disjunctions = kb_parser input in
    load_kb kb disjunctions;
    close_in input
  with
  | Unix.Unix_error (e,_,_) ->
    Format.printf "%% Unix error while reading %s: %s@." file (Unix.error_message e)
  | Failure e -> Format.printf "%% error while reading %s: %s@." file e; ()

(** save the KB to the file *)
let save_kb ~file ~kb_printer kb =
  let out = Unix.openfile file [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC] 0o644 in
  let out = Unix.out_channel_of_descr out in
  let formatter = Format.formatter_of_out_channel out in
  (* use given printer to print each disjunction *)
  let sequence = dump_kb kb in
  Format.fprintf formatter "@[<v>%a@]@." kb_printer sequence;
  flush out;
  close_out out

let clear_kb ~lock ~file =
  Utils.with_lock_file lock
    (fun () ->
      Format.printf "%% clear Knowledge Base stored in %s@." file;
      try Unix.unlink file
      with Unix.Unix_error (e,_,_) ->
        Format.printf "%% error: %s@." (Unix.error_message e))
