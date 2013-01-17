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

module T = Terms
module Utils = FoUtils

let id_subst = []

let is_empty s = s = []

let eq_subst s1 s2 =
  try List.for_all2
    (fun (v1, t1) (v2, t2) -> T.eq_term v1 v2 && T.eq_term t1 t2)
    s1 s2
  with Invalid_argument _ -> false

let hash_subst s =
  let rec recurse h s = match s with
  | [] -> h
  | (v,t)::s' ->
    let h' = (Utils.murmur_hash (v.hkey lxor t.hkey)) lxor h in
    recurse h' s'
  in recurse 1913 s

let compare_substs s1 s2 =
  let h1 = hash_subst s1
  and h2 = hash_subst s2 in
  if h1 <> h2  (* first compare hashes *)
    then h1 - h2
    else Utils.lexicograph (* else compare by lexicographic order *)
      (fun (v1,t1) (v2,t2) ->
        Utils.lexicograph Pervasives.compare [v1.tag; t1.tag] [v2.tag; t2.tag])
      s1 s2

module SSet = Set.Make(
  struct
    type t = substitution
    let compare = compare_substs
  end)

let rec lookup var subst = match subst with
  | [] -> var
  | ((v,t) :: tail) ->
    if T.eq_term v var then t else lookup var tail

let is_in_subst var subst = lookup var subst != var

let domain subst =
  List.fold_left (fun set (v,_) -> T.TSet.add v set) T.TSet.empty subst

let codomain subst =
  List.fold_left (fun set (_,t) -> T.TSet.add t set) T.TSet.empty subst

let is_renaming subst =
  let c = domain subst
  and cd = codomain subst in
  (* check that codomain is made of vars, and that domain and codomain have same size *)
  T.TSet.cardinal c = T.TSet.cardinal cd && T.TSet.for_all T.is_var cd

let rec reset_bindings subst =
  match subst with
  | [] -> ()
  | (v, t) :: subst' ->
    (T.reset_binding v;
     T.reset_vars t;
     reset_bindings subst')

let rec apply_subst_bind subst =
  match subst with
  | [] -> ()
  | (v, t) :: subst' -> (T.set_binding v t; apply_subst_bind subst')

let apply_subst ?(recursive=true) subst t =
  if subst = [] then t else
  begin
    T.reset_vars t;
    (* reset bindings in codom(subst), we are not interested in them *)
    List.iter (fun (_, t) -> T.reset_vars t) subst;  
    apply_subst_bind subst;
    T.expand_bindings ~recursive t
  end

let build_subst ?(recursive=false) subst v t =
  assert (v.sort = t.sort);
  if recursive
    then (
      let new_t = apply_subst ~recursive subst t in
      (* v -> v, no need to add to subst *)
      if T.eq_term v new_t then subst
      (* v -> t[v], not well-formed substitution *)
      else if T.member_term v new_t then failwith "occur check while building subst"
      (* append to list *)
      else (v, new_t) :: subst)
    else if T.eq_term v t
      then subst
      else (v,t) :: subst

let update_binding ?(recursive=false) subst v =
  assert (T.is_var v);
  let t = if recursive then T.get_binding v else v.binding in
  if t == v then subst else (v,t)::subst

let update_bindings ?(recursive=false) subst l =
  List.fold_left (update_binding ~recursive) subst l

let expand_bindings subst =
  List.map (fun (v, t) -> (v, T.expand_bindings t)) subst

let relocate offset l =
  let rec recurse subst offset l = match l with
  | [] -> subst
  | v::l' ->
    let v' = T.mk_var offset v.sort in
    let subst' = build_subst ~recursive:false subst v v' in
    recurse subst' (offset+1) l'
  in recurse id_subst offset l

(** Returns a term t' that is unique for all alpha equivalent
    representations of t, and a subst s such that s(t') = t *)
let normalize_term t =
  let subst_from_t, subst_to_t =
    List.fold_left
      (fun (s_from, s_to) var ->
        match var.term with
        | Var i ->
          let new_var = (T.mk_var i var.sort) in
          build_subst ~recursive:false s_from var new_var,
          build_subst ~recursive:false s_to new_var var
        | _ -> assert false)
      (id_subst, id_subst) (T.vars t)
  in
  let normalized_t = apply_subst ~recursive:false subst_from_t t in
  normalized_t, subst_to_t

let pp_substitution formatter subst = 
  let pp_pair formatter (v, t) =
    Format.fprintf formatter "%a → %a" !T.pp_term#pp v !T.pp_term#pp t
  in
  Format.fprintf formatter "@[<h>{%a}@]" (Utils.pp_list ~sep:", " pp_pair) subst

let pp_set formatter set =
  Format.fprintf formatter "{";
  let prev = ref false in
  SSet.iter
    (fun subst ->
      (if !prev
        then Format.fprintf formatter "@[<h>%a@],@ " pp_substitution subst
        else Format.fprintf formatter "@[<h>%a@]" pp_substitution subst);
      prev := true)
    set;
  Format.fprintf formatter "}"
