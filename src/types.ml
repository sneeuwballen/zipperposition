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

(** exception raised when sorts are mismatched *)
exception SortError of string

(* TODO: (maybe) substitutions as list of pairs of *bound* terms,
  where bound term = (int * term), the int being an offset for renaming vars.
  mk_hclause would then perform normalization of variables, and renaming of
  terms/vars would been useless but for normalization and instantiation *)

(* TODO: in Clauses, constructors from clauses for inferences and simplifications,
         that take care of the parent/child and proof bookeeping *)

(* TODO: a 'type_' type for simple types, like
  type_ = Sort of symbol | Arrow of type_t list * type_. Problem is for "or" and "and"
  that are n-ary; also, "=" is polymorphic... Maybe polymorphism then? *)
(* TODO: a literal should be (term, term, int) where the int is a set of flags. It's enough
   to tell well the lit is positive/negative, and if it's oriented (a > b) or eq/incomparable.
   Always orient with the bigger term on left. *)
(* TODO: remove proof from clauses, make it external (a proof is a tree that
   contains clauses). This should allow to disable proof-handling. *)
(* TODO: do not compute set of variables for clause, only groundness and max variable (offset) *)

module Json = Yojson.Basic
type json = Json.json

(** term with a simple sort *)
type term = {
  term : term_cell;             (** the term itself *)
  sort : sort;                  (** the sort of the term *)
  mutable flags : int;          (** boolean flags about the term *)
  mutable tsize : int;          (** number of symbol/vars occurrences in the term (weight) *)
  mutable tag : int;            (** hashconsing tag *)
  mutable hkey : int;           (** hash *)
}
(** content of the term *)
and term_cell =
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Bind of symbol * term       (** bind one variable, with the symbol *)
  | Node of symbol * term list  (** term application *)

(** list of variables *)
type varlist = term list            

(** A logical first order object, with a context for its free variables.
    The context is an offset, so that X_i inside the 'a really is X_{i+offset} *)
type 'a bind = ('a * int)
let bind_with x offset = (x, offset)

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
    else begin
      let pos = Array.of_list pos in
      let n = Array.length pos in
      Format.fprintf formatter "@[<h>";
      for i = 0 to n-1 do
        (if i > 0 then Format.fprintf formatter ".");
        Format.pp_print_int formatter pos.(i);
      done;
      Format.fprintf formatter "@]"
    end

(** compact position, as an integer *)
type compact_position = int

(** an ordering constraint (a possibly non-total ordering on symbols) *)
type precedence_constraint = symbol -> symbol -> int

(** the interface of a total ordering on symbols *)
class type precedence =
  object
    method version : int                        (** version of the precedence (length of history) *)
    method snapshot : symbol list               (** current symbols in decreasing order *)
    method add_symbols : symbol list -> int     (** add the given symbols (returns how many were new) *)
    method compare : symbol -> symbol -> int    (** total order on symbols *)
    method weight : symbol -> int               (** weight of symbol (for KBO) *)
    method set_weight : (symbol -> int) -> unit (** change the weight function *)
    method multiset_status : symbol -> bool     (** does the symbol have a multiset status? *)
    method set_multiset : (symbol -> bool) -> unit  (** set the function that recognized multiset symbols *)
  end

(** the interface of an ordering type *)
class type ordering =
  object
    method clear_cache : unit -> unit           (** clear cache, if any *)
    method precedence : precedence              (** underlying precedence on symbols *)
    method set_precedence : precedence -> unit  (** update the precedence *)
    method compare : term -> term -> comparison (** compare two terms *)
    method name : string
  end

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
  mutable hcdescendants : Ptset.t;        (** the set of IDs of descendants of the clause *)
}
(** A context for clauses. TODO add a structure for local term hashconsing? *)
and context = {
  ctx_ord : ordering;                     (** ordering used to build clauses *)
  ctx_select : selection_fun;             (** selection function for literals *)
}
(** A compact clause: ID and literals *)
and compact_clause = int * literal array
(** A proof step for a 'a. This allows for genericity of proofs. *)
and 'a proof =
  | Axiom of 'a * string * string (** file, axiom name *)
  | Proof of 'a * string * 'a proof list
(** a selection function *)
and selection_fun = hclause -> int list

(** Create a compact clause from a clause *)
let compact_clause hc = (hc.hctag, hc.hclits)

(** selects no literals *)
let no_select c = []

exception UnificationFailure

(** A pretty printer of 'a values *)
class type ['a] pp_printer =
  object
    method pp : Format.formatter -> 'a -> unit
  end

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
