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

(** {1 Substitutions} *)

module T = Term
module F = Formula

(** substitution, a list of (variable -> term) *)
type t =
  | SubstBind of (Term.t * int * Term.t * int * t)
  | SubstEmpty
and scope = int
and 'a scoped = 'a * int
  (** A scope is an integer. Variables can only be bound in one scope,
      and variables from distinct scopes are distinct too. *)

let empty = SubstEmpty

let is_empty = function
  | SubstEmpty -> true
  | SubstBind _ -> false

let rec eq s1 s2 = match s1, s2 with
  | SubstEmpty, SubstEmpty -> true
  | SubstBind (v1, o1, t1, o1', s1'), SubstBind (v2, o2, t2, o2', s2') ->
    o1 = o2 && o1' = o2' && v1 == v2 && t1 == t2 && eq s1' s2'
  | SubstBind _, SubstEmpty | SubstEmpty, SubstBind _ -> false

let rec compare s1 s2 = match s1, s2 with
  | SubstEmpty, SubstEmpty -> 0
  | SubstBind (v1, o1, t1, o1', s1'), SubstBind (v2, o2, t2, o2', s2') ->
    let cmp = Util.lexicograph_combine
      [o1 - o2; o1' - o2'; T.compare v1 v2; T.compare t1 t2] in
    if cmp <> 0 then cmp else compare s1' s2'
  | SubstBind _, SubstEmpty -> 1
  | SubstEmpty, SubstBind _ -> -1

(** lookup variable in substitution *)
let rec lookup subst var scope = match subst with
  | SubstEmpty -> raise Not_found
  | SubstBind (v, sc_v, t, sc_t, subst') ->
    if v == var && sc_v = scope then (t, sc_t) else lookup subst' var scope

(** Recursively lookup a variable in the substitution, until we get a value
    that is not a variable or that is not bound *)
let rec get_var subst v sc_v =
  try let (t, sc_t) = lookup subst v sc_v in
      if T.is_var t && (t != v || sc_t <> sc_v)
        then get_var subst t sc_t
        else t, sc_t (* fixpoint of lookup *)
  with Not_found -> v, sc_v

(** check whether the variable is bound by the substitution *)
let is_in_subst subst v sc_v =
  try ignore (lookup subst v sc_v); true
  with Not_found -> false

(** Add v -> t to the substitution. Both terms have a context. Raise
    Invalid_argument if v is already bound in the same context, to another term. *)
let bind ?(recursive=true) subst v sc_v t sc_t =
  let (t', sc_t') = if recursive then get_var subst v sc_v else v, sc_v in
  if t' == t && sc_t' = sc_t
    then subst (* compatible (absence of) bindings *)
    else if T.is_var t'
      then SubstBind (t', sc_t', t, sc_t, subst)  (* add binding at front *)
      else raise (Invalid_argument "Subst.bind: inconsistent binding")

let rec remove subst v sc_v = match subst with
  | SubstEmpty -> SubstEmpty
  | SubstBind (v', sc_v', t', sc_t', subst') ->
    let subst' = remove subst' v sc_v in
    if v == v' && sc_v = sc_v'
      then subst'  (* remove binding *)
      else SubstBind (v', sc_v', t', sc_t', subst')  (* keep binding *)

let rec append s1 s2 =
  match s1 with
  | SubstEmpty -> s2
  | SubstBind (t1, o1, t2, o2, s1') ->
    let s2' = bind ~recursive:true s2 t1 o1 t2 o2 in
    append s1' s2'

(** Disambiguation of variables between different contexts *)
module Renaming = struct
  module H = Hashtbl.Make(struct
    type t = Term.t * int
    let equal (t1,o1) (t2,o2) = t1 == t2 && o1 = o2
    let hash (t,o) = t.T.tag lxor o
  end)

  type t = Term.t H.t   (* hashtable (term,scope) -> term *)

  let create size = H.create size

  let clear ren = H.clear ren

  let rename ren t scope =
    match t.T.term with
    | T.Var _ ->
      begin try
        H.find ren (t, scope)
      with Not_found ->
        (* new name *)
        let n = H.length ren in
        let t' = T.mk_var n in
        H.add ren (t, scope) t';
        t'
      end
    | _ -> assert false
end

(** Apply substitution to term, replacing variables by the terms they are bound to.
    The [renaming] is used to rename free variables (not bound
    by [subst]) while avoiding collisions.
    [recursive] decides whether, when [v] is replaced by [t], [subst] is
    applied to [t] recursively or not (default true). *)
let apply ?(recursive=true) ?renaming ?(depth=0) subst t scope =
  (* apply subst to bound term. We need to keep track of
     how many binders are on the path to the variable, because of non-DB-closed
     terms that may occur in the codomain of [subst] *)
  let rec replace binder_depth subst t scope =
    if T.is_ground t
    || (renaming == None && is_empty subst && scope = 0)
    then t (* subst(t) = t, if t ground *)
    else match t.T.term with
    | T.BoundVar _ -> t
    | T.Bind (s, t') ->
      let t'' = replace (binder_depth + 1) subst t' scope in
      if t' == t''
        then t
        else T.mk_bind s t''
    | T.Node (s, l) ->
      let l' = replace_list binder_depth subst scope l in
      if List.for_all2 (==) l l'
        then t
        else T.mk_node s l'
    | T.Var i ->
      (* two cases, depending on whether [t] is bound by [subst] or not *)
      begin try
        let (t', sc_t') = lookup subst t scope in
          (* if t' contains free De Bruijn symbols, lift them by [binder_depth] *)
          let t' = if T.db_closed t'
            then t' else T.db_lift binder_depth t' in
          (* also apply [subst] to [t']? *)
          if recursive && (t' != t || sc_t' <> scope)
            then (* replace also in the image of t *)
              replace binder_depth subst t' sc_t'
            else (* return image, in which variables are shifted *)
              replace binder_depth empty t' sc_t' 
      with Not_found -> (* variable not bound by [subst], rename it *)
        match renaming with
        | None when scope = 0 -> t
        | None -> T.mk_var (i+scope)             (* shift *)
        | Some r -> Renaming.rename r t scope    (* rename *)
      end
    | T.At (t1, t2) ->
      let t1' = replace binder_depth subst t1 scope in
      let t2' = replace binder_depth subst t2 scope in
      if t1 == t1' && t2 == t2' then t else T.mk_at t1' t2'
  (* apply subst to the list, all elements of which have the given scope *)
  and replace_list binder_depth subst scope l = match l with
  | [] -> []
  | t::l' ->
    let new_t = replace binder_depth subst t scope in
    new_t :: replace_list binder_depth subst scope l'
  in
  replace depth subst t scope

let apply_f ?recursive ?renaming ?(depth=0) subst f scope =
  F.map_depth ~depth
    (fun depth' t -> apply ?renaming ?recursive ~depth subst t scope)
    f

(** Set of bound terms *)
module Domain = Set.Make(struct
  type t = Term.t * int
  let compare (t1,o1) (t2,o2) =
    if o1 <> o2 then o1 - o2 else T.compare t1 t2
end)

(** Domain of substitution *)
let domain subst =
  let rec gather set subst = match subst with
  | SubstEmpty -> set
  | SubstBind (v, sc_v, _, _, subst') ->
    gather (Domain.add (v, sc_v) set) subst'
  in gather Domain.empty subst

(** Codomain (range) of substitution *)
let codomain subst =
  let rec gather set subst = match subst with
  | SubstEmpty -> set
  | SubstBind (_, _, t, sc_t, subst') ->
    gather (Domain.add (t, sc_t) set) subst'
  in gather Domain.empty subst

(** Check whether the substitution is a variable renaming *)
let is_renaming subst =
  let c = domain subst
  and cd = codomain subst in
  (* check that codomain is made of vars, and that domain and codomain have same size *)
  Domain.cardinal c = Domain.cardinal cd &&
  Domain.for_all (fun (v,_) -> T.is_var v) cd

(** Printing *)
let pp_full pp_term buf subst =
  let is_last subst = match subst with
  | SubstBind (_, _, _, _, SubstEmpty) -> true
  | SubstBind _ | SubstEmpty -> false
  in
  let rec pp_rec buf subst =
    match subst with
    | SubstEmpty -> ()
    | SubstBind (v, sc_v, t, sc_t, subst') ->
      Printf.bprintf buf "%a[%d] -> %a[%d]" pp_term v sc_v pp_term t sc_t;
      (if not (is_last subst) then Buffer.add_string buf ", ");
      pp_rec buf subst'
  in
  Buffer.add_string buf "{";
  pp_rec buf subst;
  Buffer.add_string buf "}"

let pp buf subst = pp_full Term.pp buf subst

let to_string subst = Util.sprintf "%a" pp subst

let fmt fmt subst = Format.pp_print_string fmt (to_string subst)

let rec iter subst k = match subst with
| SubstEmpty -> ()
| SubstBind (v, o_v, t, o_t, subst') ->
  k (v, o_v, t, o_t);
  iter subst' k

(** Sequence of pairs of bound terms *)
let to_seq subst =
  Sequence.from_iter (fun k -> iter subst k)

let of_seq ?(recursive=true) ?(subst=empty) seq =
  Sequence.fold
    (fun subst (v, o_v, t, o_t) ->
      bind ~recursive subst v o_v t o_t)
    subst seq

let of_list ?recursive ?subst l = of_seq ?recursive ?subst (Sequence.of_list l)
