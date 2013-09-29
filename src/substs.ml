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

  let dummy = create 3

  let clear ren = H.clear ren

  let rename ren t scope =
    if ren == dummy then t  (* special case: no renaming *)
    else match t.T.term with
    | T.Var _ ->
      begin try
        H.find ren (t, scope)
      with Not_found ->
        (* new name *)
        let n = H.length ren in
        let t' = T.mk_var ?ty:t.T.type_ n in
        H.add ren (t, scope) t';
        t'
      end
    | _ -> assert false
end

let rec _apply_rec ~recursive ~renaming ~depth subst t scope =
  if T.is_ground t then t (* subst(t) = t, if t ground *)
  else match t.T.term with
  | T.BoundVar _ -> t
  | T.Bind (s, t') ->
    let t'' = _apply_rec ~recursive ~renaming ~depth:(depth+1) subst t' scope in
    T.mk_bind s t''
  | T.Node (s, l) ->
    let l' = _apply_rec_list ~recursive ~renaming ~depth subst scope l in
    T.mk_node s l'
  | T.Var i ->
    (* two cases, depending on whether [t] is bound by [subst] or not *)
    begin try
      let t', sc_t' = lookup subst t scope in
      (* if t' contains free De Bruijn symbols, lift them by [binder_depth] *)
      let t' = T.db_lift ~depth depth t' in
      (* also apply [subst] to [t']? *)
      if recursive && (t' != t || scope <> sc_t')
        then (* _apply_rec also in the image of t *)
          _apply_rec ~recursive ~renaming ~depth subst t' sc_t'
        else t'
    with Not_found ->
      (* variable not bound by [subst], rename it *)
      Renaming.rename renaming t scope
    end
  | T.At (t1, t2) ->
    let t1' = _apply_rec ~recursive ~renaming ~depth subst t1 scope in
    let t2' = _apply_rec ~recursive ~renaming ~depth subst t2 scope in
    T.mk_at t1' t2'
(* apply subst to the list, all elements of which have the given scope *)
and _apply_rec_list ~recursive ~renaming ~depth subst scope l = match l with
  | [] -> []
  | t::l' ->
    let new_t = _apply_rec ~recursive ~renaming ~depth subst t scope in
    new_t :: _apply_rec_list ~recursive ~renaming ~depth subst scope l'

(** Apply substitution to term, replacing variables by the terms they are bound to.
    [renaming] is used to rename free variables (not bound
    by [subst]) while avoiding collisions.
    [recursive] decides whether, when [v] is replaced by [t], [subst] is
    applied to [t] recursively or not (default true). *)
let apply ?(recursive=true) ?(depth=0) ~renaming subst t scope =
  (* apply subst to bound term. We need to keep track of
     how many binders are on the path to the variable, because of non-DB-closed
     terms that may occur in the codomain of [subst] *)
  _apply_rec ~recursive ~renaming ~depth subst t scope

let apply_no_renaming ?(recursive=true) ?(depth=0) subst t scope =
  _apply_rec ~recursive ~renaming:Renaming.dummy ~depth subst t scope

let apply_f ?recursive ?(depth=0) ~renaming subst f scope =
  F.map_depth ~depth
    (fun depth' t -> apply ?recursive ~depth ~renaming subst t scope)
    f

(** Set of bound terms *)
module VarSet = Set.Make(struct
  type t = Term.t * scope
  let compare (t1,sc1) (t2,sc2) =
    if sc1 <> sc2 then sc1 - sc2 else T.compare t1 t2
end)

(** VarSet of substitution *)
let domain subst =
  let rec gather set subst = match subst with
  | SubstEmpty -> set
  | SubstBind (v, sc_v, _, _, subst') ->
    gather (VarSet.add (v, sc_v) set) subst'
  in gather VarSet.empty subst

(** Codomain (range) of substitution *)
let codomain subst =
  let rec gather set subst = match subst with
  | SubstEmpty -> set
  | SubstBind (_, _, t, sc_t, subst') ->
    gather (VarSet.add (t, sc_t) set) subst'
  in gather VarSet.empty subst

let introduced subst =
  let rec gather set subst = match subst with
  | SubstEmpty -> set
  | SubstBind (_, _, t, sc_t, subst') ->
    let vars = T.vars t in
    let set = List.fold_left (fun set v -> VarSet.add (v, sc_t) set) set vars in
    gather set subst'
  in
  gather VarSet.empty subst

let rec compose s1 s2 = match s1 with
  | SubstEmpty -> s2
  | SubstBind (v, s_v, t, s_t, s1') ->
    let s1'' = compose s1' s2 in
    SubstBind (v, s_v, t, s_t, s1'')

(** Check whether the substitution is a variable renaming *)
let is_renaming subst =
  let c = domain subst
  and cd = codomain subst in
  (* check that codomain is made of vars, and that domain and codomain have same size *)
  VarSet.cardinal c = VarSet.cardinal cd &&
  VarSet.for_all (fun (v,_) -> T.is_var v) cd

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

let rec fold subst acc f = match subst with
| SubstEmpty -> acc
| SubstBind (v, scv, t, sct, subst') ->
  let acc' = f acc v scv t sct in
  fold subst' acc' f

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

let bij =
  Bij.(map
    ~inject:(fun s -> Sequence.to_list (to_seq s))
    ~extract:(fun seq -> of_seq (Sequence.of_list seq))
    (list_ (quad T.bij int_ T.bij int_)))
