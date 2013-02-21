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

(** {2 Representation of patterns: higher-order terms} *)

open Symbols
open Types

module T = Terms
module S = FoSubst
module Utils = FoUtils
module Unif = FoUnif
module Lits = Literals

(** The datalog provers reasons over first-order formulas. However, to make
    those formulas signature-independent, we curryfy them and abstract their
    symbols into lambda-bound variables.

    This way, the pattern for "f(X,Y)=f(Y,X)"
    is "\F. ((= @ ((F @ x) @ y)) @ ((F @ y) @ x))" *)

type t = term * sort list
  (** A pattern is a curryfied formula, whose symbols are abstracted into
      lambda-bound variables. The list is the list of the sorts of
      the bound variables, such that if [t1,...,tn] are terms whose sorts
      pairwise match the list of sort, then [instantiate p t1...tn] will
      be well-typed. *)

let eq_pattern ((t1,sorts1) : t) ((t2, sorts2) : t) =
  t1 == t2 &&
    try List.for_all2 (==) sorts1 sorts2 with Invalid_argument _ -> false

let hash_pattern ((t,sorts) : t) =
  let h = T.hash_term t in
  Hash.hash_list hash_sort h sorts

let pp_pattern formatter (p:t) =
  Format.fprintf formatter "@[<h>%a@]" !T.pp_term#pp (fst p)

let to_json (p : t) : json =
  `Assoc [
    "term", T.to_json (fst p);
    "vars", `List (List.map sort_to_json (snd p))]

let of_json (json : json) : t = match json with
  | `Assoc ["term", t; "sorts", `List sorts]
  | `Assoc ["sorts", `List sorts; "term", t] ->
    let t = T.of_json t
    and sorts = List.map sort_of_json sorts in
    (t, sorts)
  | _ -> raise (Json.Util.Type_error ("expected pattern", json))

(** {2 Conversion pattern <-> clause, and matching *)

(** Given a curryfied term, find the symbols that occur as head constants
    (ie "f" in "f @ _" where f is not a "_@_") *)
let find_symbols ?(symbols=SSet.empty) seq =
  (* traverse term *)
  let rec search set t = match t.term with
  | Var _ | BoundVar _ -> set
  | Bind (s, t') -> search set t'
  | Node (s, [a;b]) when s == at_symbol ->
    search (search set a) b
  | Node (s, []) when not (is_base_symbol s) ->
    SSet.add s set  (* found symbol *)
  | Node (s, l) when is_base_symbol s ->
    List.fold_left search set l
  | Node _ -> assert false
  in
  Sequence.fold search symbols seq

(** [find_functions t (s1,...,sn)] where t is currified
    maps s1,...,sn to constants that have the correct sort *)
let find_functions seq (symbols : symbol list) =
  let signature = T.signature seq in
  assert (List.for_all (fun s -> SMap.mem s signature) symbols);
  List.map (fun s -> T.mk_const s (SMap.find s signature)) symbols

(** Abstracts the given constants out, in the given order. *)
let of_term_with t symbols : (t * term list) =
  let constants = find_functions (Sequence.singleton t) symbols in
  let t = List.fold_left
    (fun t const ->
      let sort = t.sort <=. const.sort in
      T.mk_lambda sort (T.db_from_term t const))
    t constants in
  let sorts = List.map (fun x -> x.sort) constants in
  (t, sorts), constants

(** Convert a term into a pattern *)
let of_term t : t * term list =
  let symbols = find_symbols (Sequence.singleton t) in
  let symbols = Sequence.to_list (SSetSeq.to_seq symbols) in
  of_term_with t symbols

(** Abstracts the clause out *)
let abstract_clause lits : t * term list =
  let t = Lits.term_of_lits lits in
  let p, consts = of_term t in
  Utils.debug 2 "%% @[<h>%a@] abstracted into @[<h>%a@]"
                Lits.pp_lits lits pp_pattern p;
  p, consts

(** Sorts of arguments that are accepted by the pattern *)
let sorts (p : t) = snd p

(** number of arguments that have to be provided
    to instantiate the pattern *)
let arity (p : t)  = List.length (sorts p)

(** This applies the pattern to the given arguments, beta-reduces,
    and uncurry the term back. It will fail if the result is not
    first-order. *)
let instantiate (p : t) terms =
  (* check compatibility *)
  (try
    let ok = List.for_all2 (fun t sort -> t.sort == sort) terms (sorts p) in
    if not ok then failwith "sort mismatch for instantiate"
  with Invalid_argument _ -> failwith "bad arity for instantiate");
  let t, _ = p in
  (* apply constants from the right *)
  let t' = List.fold_right
    (fun t const -> T.mk_at t const)
    terms t in
  let t' = T.beta_reduce t' in
  if T.is_fo t'
    then T.uncurry t'
    else failwith "non-FO pattern instantiation"

(** Apply the substitution to variables that parametrize the pattern,
    then [instantiate] the pattern (beta-reduced and uncurryfied).
    [apply_subst (p,vars) subst] is equivalent to
    [instantiate p (List.map (S.apply_subst subst) vars)]. *)
let apply_subst (((p, args),offset) : t parametrized bind) subst =
  let args = List.map
    (fun arg -> match arg.term with
      | Var _ -> S.apply_subst subst (arg,offset)
      | _ -> arg)
    args in
  instantiate p args

(** [matching p lits] attempts to match the literals against the pattern.
    It yields a list of solutions, each solution [s1,...,sn] satisfying
    [instantiate p [s1,...,sn] =_AC c] modulo associativity and commutativity
    of "or" and "=". *)
let matching (p : t) lits =
  let _, sorts = p in
  let right = Lits.term_of_lits lits in
  (* apply the pattern to a list of new variables *)
  let offset = T.max_var (T.vars right) + 1 in
  let vars = List.mapi T.mk_var sorts in
  let left = instantiate p vars in
  (* match pattern against [right] *)
  let substs = Unif.matching_ac S.id_subst (left,offset) (right,0) in
  (* now apply the substitution to the list of variables *)
  let substs = Sequence.map
    (fun subst ->
      (* convert variables back to terms *)
      let args = List.map (fun v -> S.apply_subst subst (v,offset)) vars in
      (* restriction: only bind function symbols to constants for now *)
      if List.for_all T.is_const args
        then Sequence.of_list [args]
        else Sequence.of_list [])
    substs in
  Sequence.concat substs

