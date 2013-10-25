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

type scope = int
  (** A scope is an integer. Variables can only be bound in one scope,
      and variables from distinct scopes are distinct too. *)

type 'a scoped = 'a * scope

(** {2 Signature of substitutions} *)

module type S = sig
  type term
    (** Some term structure *)

  (** {3 Basics} *)

  (** substitution, a list of (variable -> term) *)
  type t = private
    | Bind of term * int * term * int * t
    | Empty

  val empty : t
    (** The identity substitution *)

  val is_empty : t -> bool
    (** Is the substitution empty? *)

  val eq : t -> t -> bool
    (** Check (naively, ie structurally) whether two substitutions are equal *)

  val compare : t -> t -> scope
    (** Compare substitutions (arbitrary but total order) *)

  (** {3 Disambiguation of Variables between different Scopes} *)

  module Renaming : sig
    type t
      (** A renaming, from (variable,offset) to variable *)
    
    val create : int -> t
      (** Create a new general-purpose renaming, which manages to rename
          variables of any number of contexts without ambiguities *)

    val dummy : t
      (** Renaming that does not rename (yes...). It maps all variables to
          themselves, regardless of the scope they occur in. Use with caution! *)

    val clear : t -> unit
      (** Clear the content of the renaming *)

    val rename : t -> term -> scope -> term
      (** Rename the given variable, scoped by the given context *)
  end

  (** {3 Operations on Substitutions} *)

  val lookup : t -> term -> scope -> term * scope
    (** Lookup variable in substitution. Raise Not_found if not present. *)

  val get_var : t -> term -> scope -> term * scope
    (** Lookup recursively the var in the substitution, until it is not a
        variable anymore, or it is not bound *)

  val is_in_subst : t -> term -> scope -> bool
    (** Check whether the variable is bound by the substitution *)

  val bind : ?recursive:bool -> t -> term -> scope -> term -> scope -> t
    (** Add v -> t to the substitution. Both terms have a context. Raise
        Invalid_argument if v is already bound in the same context, to another term. *)

  val append : t -> t -> t
    (** [append s1 s2] is the substitution that maps [t] to [s2 (s1 t)]. *)

  val remove : t -> term -> int -> t
    (** Remove the given binding. No other variable should depend on it... *)

  val apply : ?recursive:bool -> ?depth:int -> renaming:Renaming.t ->
              t -> term -> scope -> term
    (** Apply substitution to term, replacing variables by the terms they are bound to.

        [renaming] is used to rename free variables (not bound by [subst])
        while avoiding collisions.    
        [recursive] decides whether, when [v] is replaced by [t], [subst] is
        applied to [t] recursively or not (default true). *)

  val apply_no_renaming : ?recursive:bool -> ?depth:int ->
                          t -> term -> scope -> term
    (** Apply the substitution, and does not rename variables. {b Caution}, this
        can entail collisions between scopes! *)

  module VarSet : Set.S with type elt = term * scope
    (** Set of bound terms *)

  val domain : t -> VarSet.t
    (** Domain of substitution *)

  val codomain : t -> VarSet.t
    (** Codomain (image terms) of substitution *)

  val introduced : t -> VarSet.t
    (** Variables introduced by the substitution (ie vars of codomain) *)

  val compose : t -> t -> t
    (** [compose s1 s2] is the substitution that to [x] associates
        [s1 (s2 x)]. *)

  (* XXX is it possible to express it with this representation of substs?
  val join : t -> t -> t
    (** [join s1 s2] maps [x] to [s1 (s2 x)] if [x] is in the domain of [s2],
        and to [s1 x] if [x] is in the domain of s1 but not in [introduced s2].
        Basically, it hides the variables introduced in [s2] and bound in [s1] *)
  *)

  val is_renaming : t -> bool
    (** Check whether the substitution is a variable renaming *)

  val infer : TypeInference.Ctx.t -> t -> unit
    (** Infer types using the signature in the given context.
        @raise Type.Error if types are not consistent *)

  val check_type : TypeInference.Ctx.t -> t -> bool
    (** Is the substitution well-typeable in the given context? *)

  val check_type_sig : Signature.t -> t -> bool

  val pp_full : (Buffer.t -> term -> unit) -> Buffer.t -> t -> unit
  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit

  val fold : t -> 'a -> ('a -> term -> scope -> term -> scope -> 'a) -> 'a
  val iter : t -> (term * scope * term * scope -> unit) -> unit

  val to_seq : t -> (term * scope * term * scope) Sequence.t
  val of_seq : ?recursive:bool -> ?subst:t ->
                (term * scope * term * scope) Sequence.t -> t
  val of_list : ?recursive:bool -> ?subst:t ->
                (term * scope * term * scope) list -> t

  val bij : t Bij.t
end

(** {2 Instances} *)

module FO = struct
  module T = FOTerm
  module F = FOFormula

  type term = T.t

  (** substitution, a list of (variable -> term) *)
  type t =
    | Bind of T.t * int * T.t * int * t
    | Empty

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | Bind _ -> false

  let rec eq s1 s2 = match s1, s2 with
    | Empty, Empty -> true
    | Bind (v1, o1, t1, o1', s1'), Bind (v2, o2, t2, o2', s2') ->
      o1 = o2 && o1' = o2' && T.eq v1 v2 && T.eq t1 t2 && eq s1' s2'
    | Bind _, Empty | Empty, Bind _ -> false

  let rec compare s1 s2 = match s1, s2 with
    | Empty, Empty -> 0
    | Bind (v1, o1, t1, o1', s1'), Bind (v2, o2, t2, o2', s2') ->
      let cmp = Util.lexicograph_combine
        [o1 - o2; o1' - o2'; T.compare v1 v2; T.compare t1 t2] in
      if cmp <> 0 then cmp else compare s1' s2'
    | Bind _, Empty -> 1
    | Empty, Bind _ -> -1

  (** lookup variable in substitution *)
  let rec lookup subst var scope = match subst with
    | Empty -> raise Not_found
    | Bind (v, sc_v, t, sc_t, subst') ->
      if T.eq v var && sc_v = scope
        then t, sc_t
        else lookup subst' var scope

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
    if T.eq t' t && sc_t' = sc_t
      then subst (* compatible (absence of) bindings *)
      else if T.is_var t'
        then Bind (t', sc_t', t, sc_t, subst)  (* add binding at front *)
        else raise (Invalid_argument "Subst.bind: inconsistent binding")

  let rec remove subst v sc_v = match subst with
    | Empty -> Empty
    | Bind (v', sc_v', t', sc_t', subst') ->
      let subst' = remove subst' v sc_v in
      if T.eq v v' && sc_v = sc_v'
        then subst'  (* remove binding *)
        else Bind (v', sc_v', t', sc_t', subst')  (* keep binding *)

  let rec append s1 s2 =
    match s1 with
    | Empty -> s2
    | Bind (t1, o1, t2, o2, s1') ->
      let s2' = bind ~recursive:true s2 t1 o1 t2 o2 in
      append s1' s2'

  (** Disambiguation of variables between different contexts *)
  module Renaming = struct
    module H = Hashtbl.Make(struct
      type t = T.t * int
      let equal (t1,o1) (t2,o2) = T.eq t1 t2 && o1 = o2
      let hash (t,o) = T.hash t lxor o
    end)

    type t = T.t H.t   (* hashtable (term,scope) -> term *)

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
          let ty = T.get_type t in
          (* new name *)
          let n = H.length ren in
          let t' = T.mk_var ~ty n in
          H.add ren (t, scope) t';
          t'
        end
      | _ -> assert false
  end

  let rec _apply_rec ~recursive ~renaming subst t scope =
    if T.is_ground t then t (* subst(t) = t, if t ground *)
    else match t.T.term with
    | T.BoundVar _ -> t
    | T.Node (s, l) ->
      let l' = _apply_rec_list ~recursive ~renaming subst scope l in
      T.mk_node s l'
    | T.Var i ->
      (* two cases, depending on whether [t] is bound by [subst] or not *)
      begin try
        let t', sc_t' = lookup subst t scope in
        (* also apply [subst] to [t']? *)
        if recursive && (t' != t || scope <> sc_t')
          then (* _apply_rec also in the image of t *)
            _apply_rec ~recursive ~renaming subst t' sc_t'
          else t'
      with Not_found ->
        (* variable not bound by [subst], rename it *)
        Renaming.rename renaming t scope
      end
  (* apply subst to the list, all elements of which have the given scope *)
  and _apply_rec_list ~recursive ~renaming subst scope l = match l with
    | [] -> []
    | t::l' ->
      let new_t = _apply_rec ~recursive ~renaming subst t scope in
      new_t :: _apply_rec_list ~recursive ~renaming subst scope l'

  (** Apply substitution to term, replacing variables by the terms they are bound to.
      [renaming] is used to rename free variables (not bound
      by [subst]) while avoiding collisions.
      [recursive] decides whether, when [v] is replaced by [t], [subst] is
      applied to [t] recursively or not (default true). *)
  let apply ?(recursive=true) ?depth ~renaming subst t scope =
    (* apply subst to bound term. We need to keep track of
       how many binders are on the path to the variable, because of non-DB-closed
       terms that may occur in the codomain of [subst] *)
    _apply_rec ~recursive ~renaming subst t scope

  let apply_no_renaming ?(recursive=true) ?depth subst t scope =
    _apply_rec ~recursive ~renaming:Renaming.dummy subst t scope

  let apply_f ?recursive ~renaming subst f scope =
    F.map_depth
      (fun depth' t -> apply ?recursive ~renaming subst t scope)
      f

  (** Set of bound terms *)
  module VarSet = Set.Make(struct
    type t = T.t * scope
    let compare (t1,sc1) (t2,sc2) =
      if sc1 <> sc2 then sc1 - sc2 else T.compare t1 t2
  end)

  (** VarSet of substitution *)
  let domain subst =
    let rec gather set subst = match subst with
    | Empty -> set
    | Bind (v, sc_v, _, _, subst') ->
      gather (VarSet.add (v, sc_v) set) subst'
    in gather VarSet.empty subst

  (** Codomain (range) of substitution *)
  let codomain subst =
    let rec gather set subst = match subst with
    | Empty -> set
    | Bind (_, _, t, sc_t, subst') ->
      gather (VarSet.add (t, sc_t) set) subst'
    in gather VarSet.empty subst

  let introduced subst =
    let rec gather set subst = match subst with
    | Empty -> set
    | Bind (_, _, t, sc_t, subst') ->
      let vars = T.vars t in
      let set = List.fold_left (fun set v -> VarSet.add (v, sc_t) set) set vars in
      gather set subst'
    in
    gather VarSet.empty subst

  let rec compose s1 s2 = match s1 with
    | Empty -> s2
    | Bind (v, s_v, t, s_t, s1') ->
      let s1'' = compose s1' s2 in
      Bind (v, s_v, t, s_t, s1'')

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
    | Bind (_, _, _, _, Empty) -> true
    | Bind _ | Empty -> false
    in
    let rec pp_rec buf subst =
      match subst with
      | Empty -> ()
      | Bind (v, sc_v, t, sc_t, subst') ->
        Printf.bprintf buf "%a[%d] -> %a[%d]" pp_term v sc_v pp_term t sc_t;
        if not (is_last subst) then Buffer.add_string buf ", ";
        pp_rec buf subst'
    in
    Buffer.add_string buf "{";
    pp_rec buf subst;
    Buffer.add_string buf "}"

  let pp buf subst = pp_full T.pp buf subst

  let to_string subst = Util.sprintf "%a" pp subst

  let fmt fmt subst = Format.pp_print_string fmt (to_string subst)

  let rec fold subst acc f = match subst with
  | Empty -> acc
  | Bind (v, scv, t, sct, subst') ->
    let acc' = f acc v scv t sct in
    fold subst' acc' f

  let rec iter subst k = match subst with
  | Empty -> ()
  | Bind (v, o_v, t, o_t, subst') ->
    k (v, o_v, t, o_t);
    iter subst' k

  let infer ctx subst =
    iter subst (fun (v, _, t, _) -> TypeInference.FO.constrain_term_term ctx v t)

  let check_type ctx subst =
    TypeInference.Ctx.protect ctx
      (fun () ->
        try
          infer ctx subst;
          true
        with Type.Error _ -> false)

  let check_type_sig signature subst =
    let ctx = TypeInference.Ctx.of_signature signature in
    check_type ctx subst

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
end

module HO = struct
  module T = HOTerm

  type term = HOTerm.t

  (** substitution, a list of (variable -> term) *)
  type t =
    | Bind of T.t * int * T.t * int * t
    | Empty

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | Bind _ -> false

  let rec eq s1 s2 = match s1, s2 with
    | Empty, Empty -> true
    | Bind (v1, o1, t1, o1', s1'), Bind (v2, o2, t2, o2', s2') ->
      o1 = o2 && o1' = o2' && T.eq v1 v2 && T.eq t1 t2 && eq s1' s2'
    | Bind _, Empty | Empty, Bind _ -> false

  let rec compare s1 s2 = match s1, s2 with
    | Empty, Empty -> 0
    | Bind (v1, o1, t1, o1', s1'), Bind (v2, o2, t2, o2', s2') ->
      let cmp = Util.lexicograph_combine
        [o1 - o2; o1' - o2'; T.compare v1 v2; T.compare t1 t2] in
      if cmp <> 0 then cmp else compare s1' s2'
    | Bind _, Empty -> 1
    | Empty, Bind _ -> -1

  (** lookup variable in substitution *)
  let rec lookup subst var scope = match subst with
    | Empty -> raise Not_found
    | Bind (v, sc_v, t, sc_t, subst') ->
      if T.eq v var && sc_v = scope
        then t, sc_t
        else lookup subst' var scope

  (** Recursively lookup a variable in the substitution, until we get a value
      that is not a variable or that is not bound *)
  let rec get_var subst v sc_v =
    try let t, sc_t = lookup subst v sc_v in
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
    let t', sc_t' = if recursive then get_var subst v sc_v else v, sc_v in
    if T.eq t' t && sc_t' = sc_t
      then subst (* compatible (absence of) bindings *)
      else if T.is_var t'
        then Bind (t', sc_t', t, sc_t, subst)  (* add binding at front *)
        else raise (Invalid_argument "Subst.bind: inconsistent binding")

  let rec remove subst v sc_v = match subst with
    | Empty -> Empty
    | Bind (v', sc_v', t', sc_t', subst') ->
      let subst' = remove subst' v sc_v in
      if T.eq v v' && sc_v = sc_v'
        then subst'  (* remove binding *)
        else Bind (v', sc_v', t', sc_t', subst')  (* keep binding *)

  let rec append s1 s2 =
    match s1 with
    | Empty -> s2
    | Bind (t1, o1, t2, o2, s1') ->
      let s2' = bind ~recursive:true s2 t1 o1 t2 o2 in
      append s1' s2'

  (** Disambiguation of variables between different contexts *)
  module Renaming = struct
    module H = Hashtbl.Make(struct
      type t = T.t * int
      let equal (t1,o1) (t2,o2) = T.eq t1 t2 && o1 = o2
      let hash (t,o) = T.hash t lxor o
    end)

    type t = T.t H.t   (* hashtable (term,scope) -> term *)

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
          let ty = T.get_type t in
          (* new name *)
          let n = H.length ren in
          let t' = T.mk_var ~ty n in
          H.add ren (t, scope) t';
          t'
        end
      | _ -> assert false
  end

  let rec _apply_rec ~recursive ~renaming ~depth subst t scope =
    if T.is_ground t then t (* subst(t) = t, if t ground *)
    else match t.T.term with
    | T.Const _
    | T.BoundVar _ -> t
    | T.Bind (s, t') ->
      let t'' = _apply_rec ~recursive ~renaming ~depth:(depth+1) subst t' scope in
      let ty = T.get_type t in
      T.mk_bind s ~ty t''
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

  (** Set of bound terms *)
  module VarSet = Set.Make(struct
    type t = T.t * scope
    let compare (t1,sc1) (t2,sc2) =
      if sc1 <> sc2 then sc1 - sc2 else T.compare t1 t2
  end)

  (** VarSet of substitution *)
  let domain subst =
    let rec gather set subst = match subst with
    | Empty -> set
    | Bind (v, sc_v, _, _, subst') ->
      gather (VarSet.add (v, sc_v) set) subst'
    in gather VarSet.empty subst

  (** Codomain (range) of substitution *)
  let codomain subst =
    let rec gather set subst = match subst with
    | Empty -> set
    | Bind (_, _, t, sc_t, subst') ->
      gather (VarSet.add (t, sc_t) set) subst'
    in gather VarSet.empty subst

  let introduced subst =
    let rec gather set subst = match subst with
    | Empty -> set
    | Bind (_, _, t, sc_t, subst') ->
      let vars = T.vars t in
      let set = List.fold_left (fun set v -> VarSet.add (v, sc_t) set) set vars in
      gather set subst'
    in
    gather VarSet.empty subst

  let rec compose s1 s2 = match s1 with
    | Empty -> s2
    | Bind (v, s_v, t, s_t, s1') ->
      let s1'' = compose s1' s2 in
      Bind (v, s_v, t, s_t, s1'')

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
    | Bind (_, _, _, _, Empty) -> true
    | Bind _ | Empty -> false
    in
    let rec pp_rec buf subst =
      match subst with
      | Empty -> ()
      | Bind (v, sc_v, t, sc_t, subst') ->
        Printf.bprintf buf "%a[%d] -> %a[%d]" pp_term v sc_v pp_term t sc_t;
        (if not (is_last subst) then Buffer.add_string buf ", ");
        pp_rec buf subst'
    in
    Buffer.add_string buf "{";
    pp_rec buf subst;
    Buffer.add_string buf "}"

  let pp buf subst = pp_full T.pp buf subst

  let to_string subst = Util.sprintf "%a" pp subst

  let fmt fmt subst = Format.pp_print_string fmt (to_string subst)

  let rec fold subst acc f = match subst with
  | Empty -> acc
  | Bind (v, scv, t, sct, subst') ->
    let acc' = f acc v scv t sct in
    fold subst' acc' f

  let rec iter subst k = match subst with
  | Empty -> ()
  | Bind (v, o_v, t, o_t, subst') ->
    k (v, o_v, t, o_t);
    iter subst' k

  let infer ctx subst =
    iter subst (fun (v, _, t, _) -> TypeInference.HO.constrain_term_term ctx v t)

  let check_type ctx subst =
    TypeInference.Ctx.protect ctx
      (fun () ->
        try
          infer ctx subst;
          true
        with Type.Error _ -> false)

  let check_type_sig signature subst =
    let ctx = TypeInference.Ctx.of_signature signature in
    check_type ctx subst

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
end
