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

(** Representative patterns for terms and clauses, signature-independent *)

open Types
open Symbols

module T = Terms
module C = Clauses
module Utils = FoUtils

type psymbol = int
type psort = int

(** A pattern term. Symbols, sorts and variables can all be bound. *)
type pterm =
  | PVar of int * psort
  | PNode of psymbol * psort * pterm list

let rec compare_pterm t1 t2 =
  match t1, t2 with
  | PVar _, PNode _ -> -1
  | PNode _, PVar _ -> 1
  | PVar (i1, s1), PVar (i2, s2) -> if i1 <> i2 then i1 - i2 else s1 - s2
  | PNode (f1, s1, l1), PNode (f2, s2, l2) ->
    if f1 <> f2 then f1 - f2
    else if s1 <> s2 then s1 - s2
    else lexico l1 l2
and lexico l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x1::l1', x2::l2' ->
    let cmp = compare_pterm x1 x2 in
    if cmp <> 0 then cmp else lexico l1' l2'

let eq_pterm t1 t2 = compare_pterm t1 t2 = 0

(** A pattern literal is a pair of pattern terms + the sign *)
type pliteral = {
  lterm: pterm;
  lweight: int;
  rterm : pterm;
  rweight: int;
  psign : bool;
}

let compare_pliteral lit1 lit2 =
  let weight1 = lit1.lweight + lit1.rweight
  and weight2 = lit2.lweight + lit2.rweight in
  if weight1 <> weight2 then weight1 - weight2
  else let cmp_left = compare_pterm lit1.lterm lit2.lterm in
  if cmp_left <> 0 then cmp_left
  else compare_pterm lit1.rterm lit2.rterm

(** A pattern clause is just a list of pliterals *)
type pclause = pliteral list

let compare_pclause c1 c2 =
  let rec lexico l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | lit1::l1', lit2::l2' ->
    let cmp = compare_pliteral lit1 lit2 in
    if cmp <> 0 then cmp else lexico l1' l2'
  in lexico c1 c2

(** An abstract substitution maps abstract symbols to symbols, variables and sorts *)
type mapping = {
  mutable m_var : int Ptmap.t;
  mutable m_symbol : symbol Ptmap.t;
}

let empty_mapping () = {
  m_var = Ptmap.empty;
  m_symbol = Ptmap.empty;
}

(** Reverse mapping, from concrete vars/symbols/sorts to abstract ones. *)
type rev_mapping = {
  mutable rm_var : int Ptmap.t;
  rm_symbol : psymbol SHashtbl.t;
  mutable rm_varnum : int;
  mutable rm_symbolnum : int;
}

let empty_rev_mapping () = {
  rm_var = Ptmap.empty;
  rm_symbol = SHashtbl.create 3;
  rm_varnum = 0;
  rm_symbolnum = 0;
}

(*s canonical patterns for terms, literals and clauses *)

(** Get a unique number for this symbol, using the rev_map *)
let rm_get_symbol ~rev_map symbol =
  try SHashtbl.find rev_map.rm_symbol symbol
  with Not_found ->
    (* the symbol is new *)
    let n = rev_map.rm_symbolnum in
    SHashtbl.replace rev_map.rm_symbol symbol n;
    rev_map.rm_symbolnum <- n + 1;
    n

(** Get a unique number for this var, using the rev_map *)
let rm_get_var ~rev_map i =
  try Ptmap.find i rev_map.rm_var
  with Not_found ->
    (* the variable is new *)
    let n = rev_map.rm_varnum in
    rev_map.rm_var <- Ptmap.add i n rev_map.rm_var;
    rev_map.rm_varnum <- n + 1;
    n

let pterm_of_term ?rev_map t =
  let rev_map = match rev_map with | None -> empty_rev_mapping () | Some m -> m in
  (* recursive conversion *)
  let rec convert t = match t.term with
  | Var i ->
    let psort = rm_get_symbol ~rev_map t.sort in
    let i' = rm_get_var ~rev_map i in
    PVar (i', psort)
  | Node (f, l) ->
    let psort = rm_get_symbol ~rev_map t.sort in
    let psymbol = rm_get_symbol ~rev_map f in
    let l' = List.map convert l in
    PNode (psymbol, psort, l')
  in
  convert t

let plit_of_lit ?rev_map lit =
  let rev_map = match rev_map with | None -> empty_rev_mapping () | Some m -> m in
  (* order the terms by decreasing weight, or by lexicographic order of their
     canonical form *)
  let l, r, psign = match lit with
  | Equation (l, r, sign, _) -> begin
    let pl, pr = pterm_of_term l, pterm_of_term r in
    if l.tsize > r.tsize then l, r, sign
    else if l.tsize < r.tsize then r, l, sign
    else match compare_pterm pl pr with
     | n when n < 0 -> r, l, sign
     | n when n >= 0 -> l, r, sign
     | _ -> assert false
     end
  in
  (* convert first l, then r *)
  let lterm = pterm_of_term ~rev_map l in
  let rterm = pterm_of_term ~rev_map r in
  { lterm; rterm; lweight = l.tsize; rweight = r.tsize; psign; }

let pclause_of_clause ?rev_map hc =
  let rev_map = match rev_map with | None -> empty_rev_mapping () | Some m -> m in
  (* sort the literals by weight, or by lexicographic order on their canonical form *)
  let lits = Array.to_list (Array.map (fun lit -> plit_of_lit lit, lit) hc.hclits) in
  let lits = List.sort (fun (plit1, _) (plit2, _) -> compare_pliteral plit1 plit2) lits in
  (* convert the literals to pliterals using the rev_map *)
  let lits = List.map (fun (_, lit) -> plit_of_lit ~rev_map lit) lits in
  lits

(*s instantiate an abstract pattern *)

let rec instantiate_pterm ~map pterm = match pterm with
  | PVar (i, s) ->
    (* we may keep the variable unbounded, in which case it is preserved *)
    let i' = try Ptmap.find i map.m_var with Not_found -> i in
    let s' = Ptmap.find s map.m_symbol in
    T.mk_var i' s'
  | PNode (f, s, l) ->
    let f' = Ptmap.find f map.m_symbol in
    let s' = Ptmap.find s map.m_symbol in
    let l' = List.map (instantiate_pterm ~map) l in
    T.mk_node f' s' l'

let instantiate_plit ~map ~ord plit =
  let l = instantiate_pterm ~map plit.lterm
  and r = instantiate_pterm ~map plit.rterm in
  C.mk_lit ~ord l r plit.psign

let instantiate_pclause ~map ~ord pclause proof parents =
  let lits = List.map (instantiate_plit ~map ~ord) pclause in
  C.mk_hclause ~ord lits proof parents

(*s match an abstract pattern against a term of a clause. Failure is
    indicated by an empty list, but several mappings can exist for
    literals and clauses. *)

let match_pterm ~map t pt = failwith "not implemented"

let match_plit ~map lit plit = failwith "not implemented"

let match_pclause ?map hc pclause =
  (* let map = match map with | None -> empty_mapping () | Some m -> m in *)
  failwith "not implemented"

(** An indexing structure that maps pclauses to values *)
module PMap = Map.Make(struct type t = pclause let compare = compare_pclause end)
