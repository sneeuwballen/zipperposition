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

(** {1 Operations on substitutions} *)

open Basic

module T = Terms
module Utils = FoUtils

let id_subst = SubstEmpty

let is_empty = function
  | SubstEmpty -> true
  | SubstBind _ -> false

let rec eq_subst s1 s2 = match s1, s2 with
  | SubstEmpty, SubstEmpty -> true
  | SubstBind (v1, o1, t1, o1', s1'), SubstBind (v2, o2, t2, o2', s2') ->
    o1 = o2 && o1' = o2' && v1 == v2 && t1 == t2 && eq_subst s1' s2'
  | SubstBind _, SubstEmpty | SubstEmpty, SubstBind _ -> false

let rec compare_substs s1 s2 = match s1, s2 with
  | SubstEmpty, SubstEmpty -> 0
  | SubstBind (v1, o1, t1, o1', s1'), SubstBind (v2, o2, t2, o2', s2') ->
    let cmp = Utils.lexicograph_combine
      [compare o1 o2; compare o1' o2'; T.compare_term v1 v2; T.compare_term t1 t2] in
    if cmp <> 0 then cmp else compare_substs s1' s2'
  | SubstBind _, SubstEmpty -> 1
  | SubstEmpty, SubstBind _ -> -1

(** lookup variable in substitution *)
let rec lookup subst ((var,offset) as bind_var) = match subst with
  | SubstEmpty -> raise Not_found
  | SubstBind (v, o_v, t, o_t, subst') ->
    if v == var && o_v = offset then (t, o_t) else lookup subst' bind_var

(** Recursively lookup a variable in the substitution, until we get a value
    that is not a variable or that is not bound *)
let rec get_var subst (v, o_v) =
  try let (t, o_t) = lookup subst (v, o_v) in
      if T.is_var t && (t != v || o_t <> o_v)
        then get_var subst (t, o_t)
        else t, o_t (* fixpoint of lookup *)
  with Not_found -> v, o_v

(** check whether the variable is bound by the substitution *)
let is_in_subst subst bind_var =
  try ignore (lookup subst bind_var); true
  with Not_found -> false

(** Add v -> t to the substitution. Both terms have a context. Raise
    Invalid_argument if v is already bound in the same context, to another term. *)
let bind ?(recursive=true) subst ((v, _) as var_bind) (t, o_t) =
  assert (v.sort == t.sort && T.is_var v);
  let (t', o_t') = if recursive then get_var subst var_bind else var_bind in
  if t' == t && o_t' = o_t
    then subst (* compatible (absence of) bindings *)
    else if T.is_var t'
      then SubstBind (t', o_t', t, o_t, subst)  (* add binding at front *)
      else raise (Invalid_argument "Subst.bind: inconsistent binding")

(** Disambiguation of variables between different contexts *)
module Renaming = struct
  module H = Hashtbl.Make(struct
    type t = (term * offset)
    let equal (t1,o1) (t2,o2) = t1 == t2 && o1 = o2
    let hash (t,o) = t.tag lxor o
  end)

  type t = term H.t   (* hashtable (term,offset) -> term *)

  let create size = H.create size

  let clear ren = H.clear ren

  let rename ren (t, offset) =
    match t.term with
    | Var _ ->
      begin try
        H.find ren (t, offset)
      with Not_found ->
        (* new name *)
        let n = H.length ren in
        let t' = T.mk_var n t.sort in
        H.add ren (t, offset) t';
        t'
      end
    | _ -> assert false
end

(** Apply substitution to term, replacing variables by the terms they are bound to.
    The [renaming] is used to rename free variables (not bound
    by [subst]) while avoiding collisions.
    [recursive] decides whether, when [v] is replaced by [t], [subst] is
    applied to [t] recursively or not (default true). *)
let apply_subst ?(recursive=true) ?renaming subst (t, offset) =
  (* apply subst to bound term. We need to keep track of
     how many binders are on the path to the variable, because of non-DB-closed
     terms that may occur in the codomain of [subst] *)
  let rec replace binder_depth subst ((t, offset) as bound_t) =
    if T.is_ground_term t
    || (renaming == None && is_empty subst && offset = 0)
    then t (* subst(t) = t, if t ground *)
    else match t.term with
    | BoundVar _ -> t
    | Bind (s, a_sort, t') ->
      let t'' = replace (binder_depth + 1) subst (t',offset) in
      if t' == t''
        then t
        else T.mk_bind s t.sort a_sort t''
    | Node (s, l) ->
      let l' = replace_list binder_depth subst offset l in
      if List.for_all2 (==) l l'
        then t
        else T.mk_node s t.sort l'
    | Var i ->
      (* two cases, depending on whether [t] is bound by [subst] or not *)
      begin try
        let (t', o_t') = lookup subst bound_t in
          (* if t' contains free De Bruijn symbols, lift them by [binder_depth] *)
          let t' = if T.db_closed t'
            then t' else T.db_lift binder_depth t' in
          (* also apply [subst] to [t']? *)
          if recursive && (t' != t || o_t' <> offset)
            then (* replace also in the image of t *)
              replace binder_depth subst (t', o_t')
            else (* return image, in which variables are shifted *)
              replace binder_depth id_subst (t', o_t') 
      with Not_found -> (* variable not bound by [subst], rename it *)
        match renaming with
        | None when offset = 0 -> t
        | None -> T.mk_var (i+offset) t.sort        (* shift *)
        | Some r -> Renaming.rename r (t,offset)    (* rename *)
      end
  (* apply subst to the list, all elements of which have the given offset *)
  and replace_list binder_depth subst offset l = match l with
  | [] -> []
  | t::l' ->
    let new_t = replace binder_depth subst (t, offset) in
    new_t :: replace_list binder_depth subst offset l'
  in
  replace 0 subst (t, offset)

(** Set of bound terms *)
module Domain = Set.Make(
  struct
    type t = term bind
    let compare (t1,o1) (t2,o2) =
      if o1 <> o2 then compare o1 o2 else T.compare_term t1 t2
  end)

(** Domain of substitution *)
let domain subst =
  let rec gather set subst = match subst with
  | SubstEmpty -> set
  | SubstBind (v, o_v, _, _, subst') ->
    gather (Domain.add (v, o_v) set) subst'
  in gather Domain.empty subst

(** Codomain (range) of substitution *)
let codomain subst =
  let rec gather set subst = match subst with
  | SubstEmpty -> set
  | SubstBind (_, _, t, o_t, subst') ->
    gather (Domain.add (t, o_t) set) subst'
  in gather Domain.empty subst

(** Check whether the substitution is a variable renaming *)
let is_renaming subst =
  let c = domain subst
  and cd = codomain subst in
  (* check that codomain is made of vars, and that domain and codomain have same size *)
  Domain.cardinal c = Domain.cardinal cd &&
  Domain.for_all (fun (v,_) -> T.is_var v) cd

let pp_substitution formatter subst = 
  (* is the binding the last one? *)
  let is_last subst = match subst with
  | SubstBind (_, _, _, _, SubstEmpty) -> true
  | SubstBind _ | SubstEmpty -> false
  in
  (* print bindings *)
  let rec pp subst = match subst with
  | SubstEmpty -> ()
  | SubstBind (v, o_v, t, o_t, subst') ->
    Format.fprintf formatter "%a[%d] → %a[%d]"
      !T.pp_term#pp v o_v !T.pp_term#pp t o_t;
    (if not (is_last subst) then Format.fprintf formatter ", ");
    pp subst'
  in
  Format.fprintf formatter "@[{";
  pp subst;
  Format.fprintf formatter "}@]"

(** Sequence of pairs of bound terms *)
let to_seq subst =
  let seq k =
    let rec iter subst = match subst with
    | SubstEmpty -> ()
    | SubstBind (v, o_v, t, o_t, subst') ->
      k ((v, o_v), (t, o_t));
      iter subst'
    in iter subst
  in Sequence.from_iter seq

let of_seq ?(recursive=true) seq =
  Sequence.fold
    (fun subst (v, t) ->
      bind ~recursive subst v t)
    id_subst seq

let to_json subst =
  let items = Sequence.map
    (fun ((v, o_v), (t, o_t)) ->
      `List [T.to_json v; `Int o_v; T.to_json v; `Int o_t])
    (to_seq subst)
  in
  `List (Sequence.to_list items)

let of_json ?(recursive=true) json =
  let l = Json.Util.to_list json in
  let seq = Sequence.map
    (fun json -> match json with
      | `List [v; `Int o_v; t; `Int o_t] ->
        let v = T.of_json v in
        let t = T.of_json t in
        ((v, o_v), (t, o_t))
      | _ -> raise (Json.Util.Type_error ("expected subst", json)))
    (Sequence.of_list l) in
  of_seq ~recursive seq
