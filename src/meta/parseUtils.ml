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

let lookup ~table name =
  try Hashtbl.find table name
  with Not_found ->
    failwith ("no such axiom/theory: " ^ name)

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

let pp_premise formatter premise =
  match premise with
  | `Theory (name, args) | `Named (name, args) ->
    Format.fprintf formatter "%s(%a)" name (Utils.pp_list pp_symbol) args
  | `Term t -> !T.pp_term#pp formatter t

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
    let signature = T.signature (Sequence.singleton (T.curry t)) in
    SMapSeq.to_seq signature

let lookup_premises ~table premises =
  Sequence.flatMap
    (lookup_premise ~table)
    (Sequence.of_list premises)

let signature_of_premises ~table premises =
  sig_of_seq (lookup_premises ~table premises)

let signature_of_term t =
  let signature = T.signature (Sequence.singleton t) in
  SMap.filter (fun s _ -> not (is_base_symbol s)) signature

let signature_of_terms seq =
  let signature = T.signature seq in
  SMap.filter (fun s _ -> not (is_base_symbol s)) signature

let symbols_of_term t =
  let signature = signature_of_term t in
  Sequence.to_list (SMapSeq.keys signature)

(** Compute the index of the maximal variable occurring in premises *)
let offset_of_premises (premises : premise Sequence.t) =
  1 + Sequence.fold
    (fun offset premise -> match premise with
      | `Theory _ | `Named _ -> offset
      | `Term t -> max (T.max_var (T.vars t)) offset)
    0 premises

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
        (try  let t' = List.assoc s s_to_t in
              (if t'.sort != const.sort
                then failwith "consts_to_terms: incompatible sorts");
              t'
         with Not_found -> failwith ("undefined symbol: "^name_symbol s))
      | _ -> failwith (Utils.sprintf "not a const: %a" !T.pp_term#pp const))
    consts

(** Given a signature, maps symbols of the signature to variables
    in an association list. *)
let map_symbols_to_vars ?(offset=0) signature =
  let seq = sig_to_seq signature in
  let seq = Sequence.mapi
    (fun i (symb, sort) ->
      let var = T.mk_var (i+offset) sort in
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
    let symbols = symbols_of_term t in
    let vars = symbs_to_terms s_to_t symbols in
    let p, _ = Pattern.of_term_with (T.curry t) symbols in 
    KB.IfPattern (p, vars)

(** {2 Build definitions from raw parsing data} *)

(** Build a lemma that has a non-named conclusion [t] *)
let mk_lemma_term ~table t premises =
  let t = T.curry t in
  let signature = signature_of_premises ~table premises in
  let offset = offset_of_premises (Sequence.of_list premises) in
  let s_to_t = map_symbols_to_vars ~offset signature in
  (* convert premises *)
  let premises = List.map (convert_premise ~table s_to_t) premises in
  (* convert conclusion *)
  let pattern, consts = Pattern.of_term (T.curry t) in
  let args = consts_to_terms s_to_t consts in
  (* build lemma *)
  let lemma = KB.Lemma ((pattern, args), premises) in
  Utils.debug 2 "%% @[<hov2>%a@]" KB.pp_definition lemma;
  lemma

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
  let lemma = KB.Lemma ((pattern, args), premises) in
  Utils.debug 2 "%% @[<hov2>%a@]" KB.pp_definition lemma;
  lemma

(** Build the definition of a named pattern by a formula *)
let mk_named ~table (name, (symbols : symbol list)) t =
  let signature = signature_of_term (T.curry t) in
  Utils.debug 3 "%% @[<h>define %s(%a) with %a@]" name
    (Utils.pp_list pp_symbol) symbols !T.pp_term#pp t;
  (* safety checks *)
  (if not (List.for_all (fun s -> SMap.mem s signature) symbols)
    then failwith ("some symbol does not appear in the definition of " ^ name));
  (if not (SMap.cardinal signature = List.length symbols)
    then failwith ("wrong number of symbols in the definition of " ^name));
  (* abstract formula in the same order as the given symbol list *)
  let pattern, _ = Pattern.of_term_with (T.curry t) symbols in
  define_named ~table name pattern;
  let named = KB.Named (name, pattern) in
  Utils.debug 2 "%% @[<hov2>%a@]" KB.pp_definition named;
  named

let mk_theory ~table (name, (symbols : symbol list)) premises =
  let signature = signature_of_premises ~table premises in
  (if not (List.for_all (fun s -> SMap.mem s signature) symbols)
    then failwith ("some symbol does not appear in def of theory " ^ name));
  Utils.debug 3 "%% @[<h>define theory %s(%a) with %a@]" name
    (Utils.pp_list pp_symbol) symbols (Utils.pp_list pp_premise) premises;
  let offset = offset_of_premises (Sequence.of_list premises) in
  let s_to_t = map_symbols_to_vars ~offset signature in
  (* convert premises *)
  let premises = List.map (convert_premise ~table s_to_t) premises in
  (* convert theory *)
  let args = symbs_to_terms s_to_t symbols in
  define_th ~table name (List.map (fun x -> x.sort) args);
  let th = KB.Theory ((name, args), premises) in
  Utils.debug 2 "%% theory is @[<hov2>%a@]" KB.pp_definition th;
  th

let mk_gc ~table eqns (gc_theory,gc_ord,(prec : symbol list)) premises =
  let signature = signature_of_premises ~table premises in
  (if not (List.for_all (fun s -> SMap.mem s signature) prec)
    then failwith ("some symbol does not appear in precedence of GC"));
  (* TODO more safety checks *) 
  (* mapping to variables *)
  let offset = offset_of_premises (Sequence.of_list premises) in
  let offset = List.fold_left
    (fun offset eqn -> max offset (T.max_var (T.vars eqn)))
    offset eqns in
  let s_to_t = map_symbols_to_vars ~offset signature in
  (* convert precedence *)
  let gc_prec = symbs_to_terms s_to_t prec in
  (* convert equations *)
  let gc_eqns = List.map
    (fun eqn ->
      let p, consts = Pattern.of_term (T.curry eqn) in
      let args = consts_to_terms s_to_t consts in
      p, args)
    eqns in
  let gc_vars = List.map snd s_to_t in
  (* convert premises *)
  let premises = List.map (convert_premise ~table s_to_t) premises in
  KB.GC ({ KB.gc_eqns; KB.gc_theory; KB.gc_vars; KB.gc_ord; KB.gc_prec; }, premises)
