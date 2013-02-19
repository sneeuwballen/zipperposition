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

type t = term parametrized
  (** A pattern is a curryfied formula, along with a list of variables
      whose order matters. *)

let eq_pattern ((t1,v1) : t) ((t2, v2) : t) =
  t1 == t2 &&
    try List.for_all2 (==) v1 v2 with Invalid_argument _ -> false

let hash_pattern ((t,vars) : t) =
  let h = T.hash_term t in
  Hash.hash_list T.hash_term h vars

let pp_pattern formatter (p:t) =
  Format.fprintf formatter "@[<h>(%a)[%a]@]"
    !T.pp_term#pp (fst p) (Utils.pp_list !T.pp_term#pp) (snd p)

let to_json (p : t) : json =
  `Assoc [
    "term", T.to_json (fst p);
    "vars", `List (List.map T.to_json (snd p))]

let of_json (json : json) : t = match json with
  | `Assoc ["term", t; "vars", `List vars]
  | `Assoc ["vars", `List vars; "term", t] ->
    let t = T.of_json t
    and vars = List.map T.of_json vars in
    (t, vars)
  | _ -> raise (Json.Util.Type_error ("expected pattern", json))

type atom =
  | MString of string     (** Just a string *)
  | MPattern of t         (** A pattern, ie a signature-independent formula *)
  | MTerm of term         (** A ground term (constant...) *)
  | MList of atom list    (** List of atoms *)
  (** A Datalog atom, in which we may want to fit any structure we want *)

let rec eq_atom a1 a2 = match a1, a2 with
  | MString s1, MString s2 -> s1 = s2
  | MPattern p1, MPattern p2 -> eq_pattern p1 p2
  | MTerm t1, MTerm t2 -> t1 == t2
  | MList l1, MList l2 ->
    (try List.for_all2 eq_atom l1 l2 with Invalid_argument _ -> false)
  | _ -> false

let rec hash_atom = function
  | MString s -> Hash.hash_string s
  | MPattern p -> hash_pattern p
  | MTerm t -> T.hash_term t
  | MList l -> Hash.hash_list hash_atom 0 l

let rec pp_atom formatter a = match a with
  | MString s -> Format.pp_print_string formatter s
  | MPattern p -> pp_pattern formatter p
  | MTerm t -> T.pp_term_debug#pp formatter t
  | MList l ->
    Format.fprintf formatter "[%a]" (Utils.pp_list pp_atom) l

let rec atom_to_json a : json = match a with
  | MString s -> `String s
  | MPattern p -> `Assoc ["pattern", to_json p]
  | MTerm t -> `Assoc ["term", T.to_json t]
  | MList l -> `List (List.map atom_to_json l)

let rec atom_of_json (json : json) : atom = match json with
  | `String s -> MString s
  | `Assoc ["pattern", p] -> MPattern (of_json p)
  | `Assoc ["term", t] -> MTerm (T.of_json t)
  | `List l -> MList (List.map atom_of_json l)
  | _ -> raise (Json.Util.Type_error ("expected atom", json))

(** The Datalog prover that reasons over atoms. *)
module Logic = Datalog.Logic.Make(struct
  type t = atom
  let equal = eq_atom
  let hash = hash_atom
  let to_string a = Utils.sprintf "%a" pp_atom a
  let of_string s = atom_of_json (Json.from_string s)  (* XXX should not happen *)
end)

(** {2 Conversion pattern <-> clause, and matching *)

(** Find constant/function symbols in the term *)
let find_symbols t =
  (* traverse term *)
  let rec search set t = match t.term with
  | Var _ | BoundVar _ -> set
  | Bind (s, t') -> search set t'
  | Node (s, [a;b]) when s == at_symbol ->
    search (search set a) b
  | Node (s, []) when not (SMap.mem s base_signature) ->
    T.TSet.add t set  (* found symbol *)
  | Node _ -> assert false
  in search T.TSet.empty t

(** Convert a term into a pattern *)
let of_term t : t =
  let t = T.curry t in
  (* now replace symbols by variables *)
  let set = find_symbols t in
  let offset = T.max_var (T.vars t) + 1 in
  (* list of symbol,variable to replace the symbol *)
  let vars = Sequence.to_list
    (Sequence.mapi
      (fun i t' -> t', T.mk_var (i+offset) t'.sort)
      (T.TSet.to_seq set)) in
  let t = List.fold_left
    (fun t (symb,var) -> T.replace t ~old:symb ~by:var)
    t vars in
  let vars = List.map snd vars in
  let p = t, vars in
  p

(** Abstracts the clause out *)
let abstract_clause lits : t =
  let t = Lits.term_of_lits lits in
  let p = of_term t in
  Utils.debug 2 "%% @[<h>%a@] abstracted into @[<h>%a@]"
                Lits.pp_lits lits pp_pattern p;
  p

(** number of arguments that have to be provided
    to instantiate the pattern *)
let arity (p : t)  = List.length (snd p)

(** This applies the pattern to the given arguments, beta-reduces,
    and uncurry the term back. It will fail if the result is not
    first-order. *)
let instantiate (p : t) terms =
  assert (List.length terms = arity p);
  let t, vars = p in
  let offset = T.max_var (T.vars t) + 1 in
  (* build the substitution that replaces variables by the given terms *)
  let subst = List.fold_left2
    (fun subst v t' -> S.bind subst (v,0) (t',offset))
    S.id_subst vars terms in
  let t = S.apply_subst ~recursive:true subst (t,0) in
  if T.is_fo t
    then T.uncurry t
    else failwith "non-FO pattern instantiation"

(** [matching p lits] attempts to match the literals against the pattern.
    It yields a list of solutions, each solution [s1,...,sn] satisfying
    [instantiate p [s1,...,sn] =_AC c] modulo associativity and commutativity
    of "or" and "=". *)
let matching (p : t) lits =
  let left, vars = p in
  let right = Lits.term_of_lits lits in
  let substs = Unif.matching_ac S.id_subst (left,0) (right,1) in
  (* now apply the substitution to the list of variables *)
  let substs = Sequence.map
    (fun subst ->
      let vars = List.map (fun v -> S.apply_subst subst (v,0)) vars in
      (* restriction: only bind function symbols to constants for now *)
      if List.for_all T.is_const vars
        then Sequence.of_list [vars]
        else Sequence.of_list [])
    substs in
  Sequence.concat substs

(** Rename the variables in the pattern. The provided list of variables
    must be of the same length as [arity pattern]. *)
let rename p vars : t =
  assert (List.length vars = arity p);
  let subst = List.fold_left2
    (fun subst old_v new_v ->
      S.bind ~recursive:false subst (old_v,1) (new_v,0))
    S.id_subst (snd p) vars in
  let t = S.apply_subst ~recursive:false subst ((fst p),1) in
  t, vars

