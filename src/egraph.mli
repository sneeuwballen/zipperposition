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

(** E-graph imperative data structure *)

open Types

(** A node of the E-graph, that stands for some term *)
type egraph_node
(** The E-graph, contains the current congruence closure on terms *)
type egraph

val empty: unit -> egraph                               (** get a new, empty E-graph *)
val push: egraph -> unit                                (** push a backtracking point *)
val pop: egraph -> unit                                 (** pop last backtracking point,
                                                            or Invalid_argument *)

val are_equal: egraph_node -> egraph_node -> bool       (** test whether nodes are equal *)
val equiv_class: egraph_node -> egraph_node list        (** equivalence class of the node *)
val representative: egraph_node -> egraph_node          (** representative of the node *)
val merge: egraph -> egraph_node -> egraph_node -> unit (** merge two nodes in the E-graph *)

val term_of_node: egraph_node -> foterm                 (** get the term back from the node *)
val node_of_term: egraph -> foterm -> egraph_node       (** get the node representing the term *)
val term_in_graph: egraph -> foterm -> bool             (** is the term represented in the DAG? *)
val from_symbol: egraph -> string -> egraph_node list   (** list of nodes labelled by symbol *)
val maxvar: egraph -> int                               (** max var index in E-graph *)

(** A substitution maps (var) nodes to nodes (to their equivalence class). *)
type subst = (egraph_node * egraph_node) list

(** All possible linear unifications between the two terms, modulo congruence.
    If a variable is to be bound several times, it will be bound several times
    in the substitution *)
val linear_soft_unify: egraph -> egraph_node -> egraph_node ->
                       subst -> (subst -> unit) -> unit

(** Linear unification of the term t against the E-graph. Any substitution
    sigma returned is such that sigma(t) and sigma(t'), where t' is
    a term in the E-graph, top-unify. *)
val linear_hard_unify: egraph -> foterm -> substitution ->
                       (substitution -> unit) -> unit

(** Proper E-matching of the terms against the E-graph. Proper means
    that if a variable x occurs several times in the list of terms,
    all its occurrences will match nodes in the same equivalence class.

    It returns a list of results, where each result is a list of nodes
    that match the input terms, and a substitution to bind variables.

    For instance, when matching [f(x,x), x] against an E-graph
    where a = b, and f(a,b) and b occur, then [f(a,b),b] and sigma={x->a} will be
    a proper matcher since f(x,x) matches f(a,b) modulo the congruence. *)
val proper_match: egraph -> foterm list -> substitution -> 
                  (egraph_node list -> substitution -> unit) -> unit

type theory = (foterm * foterm) list

(** Close the E-graph w.r.t some set of equations *)
val theory_close: egraph -> theory -> unit

(** Set of possible paramodulation inferences. Each inference is a 
    (possibly speculative) top-unification of the side of an equation,
    and of some node in the E-graph. *)
val find_paramodulations: egraph -> theory -> (foterm * foterm * substitution) list

(** Apply the paramodulation to the E-graph *)
val apply_paramodulation: egraph -> (foterm * foterm * substitution) -> unit

(** Apply a substitution *)
val apply_subst: egraph -> subst -> unit

(** Try to close the E-graph by unifying the two given nodes. It
    returns a list of E-substitutions on success,
    or an empty list on failure. *)
val try_unify: egraph -> egraph_node -> egraph_node -> subst list

(** Search the tree of possible paramodulations, down to the given
    depth, and returns all substitutions that close some branch *)
val e_unify: egraph -> theory -> foterm -> foterm -> int -> substitution list

(** Print the E-graph in DOT format *)
val to_dot: name:string -> egraph -> string
(** print to a file *)
val to_dot_file: name:string -> egraph -> string -> unit
