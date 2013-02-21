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

open Types
open Symbols

module T = Terms
module Utils = FoUtils

(** {1 Utils for parsing theory files} *)

(** {2 Table of definitions} *)

type table = (string, table_val) Hashtbl.t
  (** Table of definitions *)
and table_val =
  | TableNamed of Pattern.t
  | TableTheory of sort list

let create () = Hashtbl.create 5

let clear table = Hashtbl.clear table

let lookup_th ~table name =
  try
    match Hashtbl.find table name with
    | TableTheory sorts -> sorts
    | TableNamed _ -> failwith (name ^ " is a named formula, not a theory")
  with Not_found ->
    failwith ("no such theory: " ^ name)

let lookup_named ~table name =
  try
    match Hashtbl.find table name with
    | TableNamed pat -> pat
    | TableTheory _ -> failwith (name ^ " is a theory, not an axiom")
  with Not_found ->
    failwith ("no such axiom: " ^ name)

let define_th ~table name sorts =
  Hashtbl.replace table name (TableTheory sorts)

let define_named ~table name pattern =
  Hashtbl.replace table name (TableNamed pattern)

(** {2 Premise for a definition} *)

type premise =
  [ `Theory of string * symbol list
  | `Named of string * symbol list
  | `Term of term
  ]

(** Lookup (symbol,sort) for the given premise(s) *)

let lookup_premise ~table (premise : premise) : (symbol * sort) Sequence.t =
  match premise with
  | `Theory (name, symbols) ->
    let sorts = lookup_th ~table name in
    Sequence.of_list (List.combine symbols sorts)
  | `Named (name, symbols) ->
    let (p, sorts) = lookup_named ~table name in
    Sequence.of_list (List.combine symbols sorts)
  | `Term t ->
    let signature = T.signature (Sequence.singleton t) in
    SMapSeq.to_seq signature

let lookup_premises ~table premises =
  Sequence.flatMap
    (lookup_premise ~table)
    (Sequence.of_list premises)

let signature_of_premises ~table premises =
  sig_of_seq (lookup_premises ~table premises)

(** {2 Conversion utils} *)

(** Maps a list of symbols to terms, using [s_to_t] *)
let symbs_to_terms s_to_t symbs =
  List.map
    (fun s ->
      try List.assq s s_to_t
      with Not_found -> failwith ("undefined symbol: "^name_symbol s))
    symbs

(** Maps a list of currified constants into terms using [s_to_t] *)
let consts_to_terms s_to_t consts =
  List.map
    (fun const -> match const.term with
      | Node (s, []) ->
        (try List.assoc s s_to_t
         with Not_found -> failwith ("undefined symbol: "^name_symbol s))
      | _ -> failwith (Utils.sprintf "not a const: %a" !T.pp_term#pp const))
    consts

(** Given a signature, maps symbols of the signature to variables
    in an association list. *)
let map_symbols_to_vars signature =
  let seq = sig_to_seq signature in
  let seq = Sequence.mapi
    (fun i (symb, sort) ->
      let var = T.mk_var i sort in
      symb, var)
    seq in
  Sequence.to_list seq

(** Given the mapping symbol->term, convert premise into KB.premise *)
let convert_premise ~table s_to_t premise =
  match premise with
  | `Theory (name, symbols) ->
    let vars = symbs_to_terms s_to_t symbols in
    KB.IfTheory (name, vars)
  | `Named (name, symbols) ->
    let vars = symbs_to_terms s_to_t symbols in
    KB.IfNamed (name, vars)
  | `Term t ->
    let symbols = SSet.elements (T.symbols (Sequence.singleton t)) in
    let vars = symbs_to_terms s_to_t symbols in
    let p, _ = Pattern.of_term_with t symbols in 
    KB.IfPattern (p, vars)

(** {2 Build definitions from raw parsing data} *)

(** Build a lemma that has a non-named conclusion [t] *)
let mk_lemma_term ~table t premises =
  let signature = signature_of_premises ~table premises in
  let s_to_t = map_symbols_to_vars signature in
  (* convert premises *)
  let premises = List.map (convert_premise ~table s_to_t) premises in
  (* convert conclusion *)
  let pattern, consts = Pattern.of_term t in
  let args = consts_to_terms s_to_t consts in
  (* build lemma *)
  KB.Lemma ((pattern, args), premises)

(** Build a lemma that has a named conclusion [named] *)
let mk_lemma_named ~table (name,symbols) premises =
  let signature = signature_of_premises ~table premises in
  let s_to_t = map_symbols_to_vars signature in
  (* convert premises *)
  let premises = List.map (convert_premise ~table s_to_t) premises in
  (* convert conclusion *)
  let pattern = lookup_named ~table name in
  let args = symbs_to_terms s_to_t symbols in
  (* build lemma *)
  KB.Lemma ((pattern, args), premises)

(** Build the definition of a named pattern by a formula *)
let mk_named ~table (name, (symbols : symbol list)) t =
  let signature = T.signature (Sequence.singleton t) in
  (* safety checks *)
  (if not (List.for_all (fun s -> SMap.mem s signature) symbols)
    then failwith ("some symbol does not appear in the definition of " ^ name));
  (if not (SMap.cardinal signature = List.length symbols)
    then failwith ("wrong number of symbols in the definition of " ^name));
  (* abstract formula in the same order as the given symbol list *)
  let pattern, _ = Pattern.of_term_with t symbols in
  KB.Named (name, pattern)

let mk_theory ~table (name, symbols) premises =
  failwith "TODO" (* TODO *)

(* theory
  let th_name, th_symbols = $1 in
  let premises = $3 in
  (* obtain list of (item,symbol), and signature of the premises *)
  let items, signature = items_of_premises premises in
  (* map symbols to variables of the correct sort *)
  let var_map = List.mapi
    (fun i (s,sort) -> s, Terms.mk_var i sort)
    (Sequence.to_list (SMapSeq.to_seq signature)) in
  (* TODO recover sorts of premises with meta_table *)
  let theory = failwith "TODO" in
  (* remember name -> definition *)
  Hashtbl.replace meta_table th_name theory;
  theory
*)

let mk_gc ~table eqns (ord,prec) premises =
  failwith "TODO"  (* TODO *)

(* GC
  (* name of theory, and symbols the theory binds *)
  let th_name, th_symbols = $9 in
  (* map symbols to types *)
  let gc_eqns = $2 in
  let gc_eqns = List.map Terms.curry gc_eqns in
  let signature = Terms.signature (Sequence.of_list gc_eqns) in
  let consts = Meta.Pattern.find_functions
    (Sequence.of_list gc_eqns) th_symbols in
  (* abstract equations *)
  let gc_eqns = List.map Meta.Pattern.of_term gc_eqns in
  (* now map constants to variables! *)
  let var_map = List.mapi
    (fun i const -> const, Terms.mk_var i const.sort)
    consts in
  let gc_eqns = List.map
    (fun eqn,eqn_consts ->
      let eqn_vars = List.map (fun c -> List.assq c var_map) eqn_consts in
      eqn, eqn_vars)
    gc_eqns in
  (* now equations are properly parametrized *)
  let gc_vars = List.map snd var_map in
  let gc_ord = $4 in
  (* precedence *)
  let gc_prec = $6 in
  let gc_prec = List.map
    (fun s -> List.assq (SMap.find s signature) var_map)
    gc_prec in
  (* sorts of theory *)
  let th_sorts = List.map (fun s -> SMap.find s signature) th_symbols in
  let th_vars = List.map
    (fun s -> List.assq (SMap.find s signature) var_map)
    th_symbols in
  (* build GC system *)
  let gc = Meta.KB.GC {
    gc_vars;
    gc_ord;
    gc_prec;
    gc_eqns;
  } and theory = Meta.KB.Theory (th_name, th_sorts) in
  Meta.KB.Rule ((gc, gc.gc_vars), [theory, th_vars]) 
*)
