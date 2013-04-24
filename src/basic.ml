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

(** Most of the useful types *)

(** a symbol is just a string *)
open Symbols

(* TODO: a literal should be (term, term, int) where the int is a set of flags. It's enough
   to tell well the lit is positive/negative, and if it's oriented (a > b) or eq/incomparable.
   Always orient with the bigger term on left. *)

module Json = Yojson.Basic
type json = Json.json

(** term with a simple sort *)
type term = {
  term : term_cell;             (** the term itself *)
  sort : sort;                  (** the sort of the term *)
  mutable flags : int;          (** boolean flags about the term *)
  mutable tsize : int;          (** number of symbol/vars occurrences in the term (weight) *)
  mutable tag : int;            (** hashconsing tag *)
}
(** content of the term *)
and term_cell =
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Bind of symbol * sort * term(** bind one variable (of given sort), with the symbol *)
  | Node of symbol * term list  (** term application *)
and sourced_term =
  term * string * string        (** Term + file,name *)

(** list of variables *)
type varlist = term list            

(** A logical first order object, with a context for its free variables.
    The context is an offset, so that X_i inside the 'a really is X_{i+offset} *)
type 'a bind = ('a * int)
and offset = int

(** An object parametrized by a list of variables *)
type 'a parametrized = ('a * varlist)

(** substitution, a list of (variable -> term) *)
type substitution =
  | SubstBind of (term * int * term * int * substitution)
  | SubstEmpty

(** (Church-Rosser) term rewriting system *)
type rewriting_system = term -> term

(** partial order comparison *)
type comparison = Lt | Eq | Gt | Incomparable

let string_of_comparison = function
  | Lt -> "=<="
  | Gt -> "=>="
  | Eq -> "==="
  | Incomparable -> "=?="

(** position in a term *)
type position = int list

let left_pos = 0
let right_pos = 1

let opposite_pos p = match p with
  | _ when p = left_pos -> right_pos
  | _ when p = right_pos -> left_pos
  | _ -> assert false

let string_of_pos s = match s with
  | _ when s == left_pos -> "left"
  | _ when s == right_pos -> "right"
  | _ -> assert false

let pp_pos formatter pos =
  if pos = []
    then Format.pp_print_string formatter "ε"
    else
      Format.fprintf formatter "@[<h>%a@]"
        (Sequence.pp_seq ~sep:"." Format.pp_print_int) (Sequence.of_list pos)

(** compact position, as an integer *)
type compact_position = int

(** an ordering constraint (a possibly non-total ordering on symbols) *)
type precedence_constraint = symbol -> symbol -> int

(** A total ordering on symbols *)
type precedence = {
  prec_snapshot : symbol list;  (** symbols in decreasing order *)
  prec_add_symbols : symbol list -> precedence * int;
    (** add the given symbols (returns how many were new) *)
  prec_compare : symbol -> symbol -> int;       (** Compare symbols *)
  prec_weight : symbol -> int;
  prec_set_weight : (symbol -> int) -> precedence;
}

(** A reduction ordering on terms *)
type ordering = {
  ord_clear_cache : unit -> unit;               (** Clear underlying cache *)
  ord_compare : term -> term -> comparison;     (** Compare two terms *)
  ord_precedence : precedence;                  (** Current precedence *)
  ord_set_precedence : precedence -> ordering;  (** Change the precedence *)
  ord_name : string;                            (** Name of the ordering *)
}

(** a literal, that is, a signed equation *)
type literal = 
 | Equation of    term  (** lhs *)
                * term  (** rhs *)
                * bool  (** sign (equality, ie true, or difference) *)
                * comparison   (* TODO remove? or just orient equations? *)

(** a first order clause *)
type clause = hclause
(** a first order clause (TODO rename into clause) *)
and hclause = {
  hclits : literal array;                 (** the literals *)
  hcctx : context;                        (** context of the clause *)
  mutable hctag : int;                    (** unique ID of the clause *)
  mutable hcflags : int;                  (** boolean flags for the clause *)
  mutable hcweight : int;                 (** weight of clause *)
  mutable hcselected : Bitvector.t;       (** bitvector for selected literals *)
  mutable hcvars : term list;             (** the free variables *)
  mutable hcproof : compact_clause proof; (** Proof of the clause *)
  mutable hcparents : hclause list;       (** parents of the clause *)
  mutable hcdescendants : int SmallSet.t ;(** the set of IDs of descendants of the clause *)
} 
(** A context for clauses. TODO add a structure for local term hashconsing? *)
and context = {
  mutable ctx_ord : ordering;             (** current ordering on terms *)
  mutable ctx_select : selection_fun;     (** selection function for literals *)
  mutable ctx_complete : bool;            (** Completeness preserved? *)
}
(** A compact clause: ID and literals *)
and compact_clause = int * literal array
(** A proof step for a 'a. This allows for genericity of proofs. *)
and 'a proof =
  | Axiom of 'a * string * string (** file, axiom name *)
  | Proof of 'a * string * 'a proof list
(** a selection function *)
and selection_fun = hclause -> int list

let mk_ctx ord select = { ctx_ord=ord; ctx_select=select; ctx_complete = true; }

(** Create a compact clause from a clause *)
let compact_clause hc = (hc.hctag, hc.hclits)

(** selects no literals *)
let no_select c = []

exception UnificationFailure

(** a statistic object: name and count *)
type statistics = string * int64 ref

let mk_stat, print_global_stats =
  let stats = ref [] in
  (* create a stat *)
  (fun name ->
    let stat = (name, ref 0L) in
    stats := stat :: !stats;
    stat),
  (* print stats *)
  (fun () ->
    List.iter
      (fun (name, cnt) -> Format.printf "%% %-30s ... %s@." name (Int64.to_string !cnt))
      !stats)

let incr_stat (_, count) = count := Int64.add !count Int64.one  (** increment given statistics *)
let add_stat (_, count) num = count := Int64.add !count (Int64.of_int num) (** add to stat *)

(** parameters for the main procedure *)
type parameters = {
  param_ord : precedence -> ordering;
  param_seed : int;
  param_steps : int;
  param_version : bool;
  param_calculus : string;
  param_timeout : float;
  param_files : string list;
  param_split : bool;             (** use splitting *)
  param_theories : bool;          (** detect theories *)
  param_precedence : bool;        (** use heuristic for precedence? *)
  param_select : string;          (** name of the selection function *)
  param_progress : bool;          (** print progress during search *)
  param_proof : string;           (** how to print proof? *)
  param_dot_file : string option; (** file to print the final state in *)
  param_plugins : string list;    (** plugins to load *)
  param_kb : string;              (** file to use for KB *)
  param_kb_load : string list;    (** theory files to read *)
  param_kb_clear : bool;          (** do we need to clear the KB? *)
  param_kb_print : bool;          (** print knowledge base and exit *)
  param_learn : bool;             (** learn lemmas? *)
  param_presaturate : bool;       (** initial interreduction of proof state? *)
  param_unary_depth : int;        (** Maximum successive levels of unary inferences *)
  param_index : string;           (** indexing structure *)
}
