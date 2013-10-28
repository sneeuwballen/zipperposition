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

  type t
    (** A substitution that binds term variables to other terms *)

  val empty : t
    (** The identity substitution *)

  val create : int -> t
    (** Substitution with an initial "size". The more elements you expect
        to bind within this substitution, the bigger this initial size should be *)

  val is_empty : t -> bool
    (** Is the substitution empty? *)

  (** {3 Operations on Substitutions} *)

  val lookup : t -> term -> scope -> term * scope
    (** Lookup variable in substitution.
        @raise Not_found if variable not bound. *)

  val get_var : t -> term -> scope -> term * scope
    (** Lookup recursively the var in the substitution, until it is not a
        variable anymore, or it is not bound *)

  val mem : t -> term -> scope -> bool
    (** Check whether the variable is bound by the substitution *)

  val bind : t -> term -> scope -> term -> scope -> t
    (** Add [v] -> [t] to the substitution. Both terms have a context.
        @raise Invalid_argument if [v] is already bound in
          the same context, to another term. *)

  val append : t -> t -> t
    (** [append s1 s2] is the substitution that maps [t] to [s2 (s1 t)]. *)

  val remove : t -> term -> int -> t
    (** Remove the given binding. No other variable should depend on it... *)

  module H : Hashtbl.S with type key = term * scope
    (** Set of bound terms *)

  val domain : t -> unit H.t
    (** Domain of substitution *)

  val codomain : t -> unit H.t
    (** Codomain (image terms) of substitution *)

  val introduced : t -> unit H.t
    (** Variables introduced by the substitution (ie vars of codomain) *)

  val compose : t -> t -> t
    (** [compose s1 s2] is the substitution that to [x] associates
        [s1 (s2 x)].
        XXX not implemented *)

  val is_renaming : t -> bool
    (** Check whether the substitution is a variable renaming *)

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit

  val fold : t -> 'a -> ('a -> term -> scope -> term -> scope -> 'a) -> 'a
  val iter : t -> (term -> scope -> term -> scope -> unit) -> unit

  val to_seq : t -> (term * scope * term * scope) Sequence.t
  val to_list : t -> (term * scope * term * scope) list
  val of_seq : ?init:t -> (term * scope * term * scope) Sequence.t -> t
  val of_list : ?init:t -> (term * scope * term * scope) list -> t

  val bij : t Bij.t
end

(** {2 Renaming} *)

module type RENAMING = sig
  type t

  val create : int -> t
    (** Fresh renaming *)

  val clear : t -> unit
    (** Cleanup the content of the renaming. It is as new afterwards! *)
end

(** {2 Hidden functor}

This functor provides the base types for substitutions and renamings.
Application depends too much on the term structure. It must
be implemented separately.
*)

module type TERM = sig
  type t
  val eq : t -> t -> bool
  val is_var : t -> bool
  val vars : t -> t list  (* list of vars of the term *)
  val hash : t -> int
  val pp : Buffer.t -> t -> unit
  val bij : t Bij.t
end

module Common(T : TERM) = struct
  type term = T.t

  module TermInt = struct
    type t = T.t * int
    let equal (t1,s_1) (t2,s_2) = s_1 = s_2 && T.eq t1 t2
    let hash (t,s) = Hash.combine (T.hash t) s
  end

  module PH = PersistentHashtbl.Make(TermInt)
  module H = Hashtbl.Make(TermInt)

  type t =
    | E
    | T of (term * int) PH.t

  let empty = E

  let create size =
    let size = max size 4 in
    T (PH.create size)

  let is_empty = function
    | E -> true
    | T tbl -> PH.is_empty tbl

  let lookup subst v s_v = match subst with
    | E -> raise Not_found
    | T tbl -> PH.find tbl (v, s_v)

  let mem subst v s_v = match subst with
    | E -> false
    | T tbl -> PH.mem tbl (v, s_v)

  (** Recursively lookup a variable in the substitution, until we get a value
      that is not a variable or that is not bound *)
  let rec get_var subst v sc_v =
    try let t, sc_t = lookup subst v sc_v in
        if T.is_var t && (sc_t <> sc_v || not (T.eq t v))
          then get_var subst t sc_t
          else t, sc_t (* fixpoint of lookup *)
    with Not_found -> v, sc_v

  let bind subst v s_v t s_t =
    let t', s_t' = get_var subst v s_v in
    if s_t' = s_t && T.eq t' t
      then subst (* compatible (absence of) bindings *)
      else if T.is_var t'
        then
          (* obtain a hashtable and bind the variable *)
          let tbl = match subst with
          | E -> PH.create 5
          | T tbl -> tbl
          in
          T (PH.replace tbl (t', s_t') (t, s_t))
        else
          let msg = Util.sprintf
            "Subst.bind: inconsistent binding for %a[%d]: %a[%d] and %a[%d]"
              T.pp v s_v T.pp t s_t T.pp t' s_t'
          in
          raise (Invalid_argument msg)

  let remove subst v s_v = match subst with
    | E -> E
    | T tbl ->
      if PH.mem tbl (v,s_v)
        then T (PH.remove tbl (v,s_v))
        else subst

  let rec append s1 s2 = match s1, s2 with
    | E, _ -> s2
    | _, E -> s1
    | T tbl1, T tbl2 ->
      if PH.length tbl1 > PH.length tbl2
        then append s2 s1
        else
          (* add bindings of s1 to s2. XXX "batch" mode? *)
          PH.fold
            (fun subst (v,s_v) (t,s_t) -> bind s2 v s_v t s_t)
            s2 tbl1
  
  let compose s1 s2 = failwith "Subst.compose: not implemented"

  let fold subst acc f = match subst with
    | E -> acc
    | T tbl ->
      PH.fold (fun acc (v,s_v) (t,s_t) -> f acc v s_v t s_t) acc tbl

  let iter subst k = match subst with
    | E -> ()
    | T tbl -> PH.iter tbl (fun (v,s_v) (t,s_t) -> k v s_v t s_t)

  (* is the substitution a renaming? *)
  let is_renaming s = match s with
    | E -> true
    | T tbl ->
      begin try
        let codomain = H.create 5 in
        PH.iter tbl (fun _ (t,s_t) ->
          (* is some var bound to a non-var term? *)
          if not (T.is_var t) then raise Exit;
          H.replace codomain (t,s_t) ());
        (* as many variables in codomain as variables in domain *)
        H.length codomain = PH.length tbl
      with Exit -> false
      end

  (* set of variables bound by subst, with their scope *)
  let domain s =
    let set = H.create 5 in
    begin match s with
    | E -> ()
    | T tbl -> PH.iter tbl (fun (v,s_v) _ -> H.replace set (v,s_v) ())
    end;
    set

  (* set of terms that some variables are bound to by the substitution *)
  let codomain s =
    let set = H.create 5 in
    begin match s with
    | E -> ()
    | T tbl -> PH.iter tbl (fun _ (t,s_t) -> H.replace set (t,s_t) ())
    end;
    set

  (* variables introduced by the subst *)
  let introduced subst =
    let set = H.create 5 in
    begin match subst with
    | E -> ()
    | T tbl ->
      PH.iter tbl
        (fun _ (t,s_t) ->
          let vars = T.vars t in
          List.iter (fun v -> H.replace set (v,s_t) ()) vars)
    end;
    set

  let to_seq subst =
    fun k ->
      iter subst (fun v s_v t s_t -> k (v, s_v, t, s_t))

  let to_list subst = match subst with
    | E -> []
    | _ ->
      let seq = to_seq subst in
      Sequence.to_rev_list seq

  let of_seq ?(init=E) seq =
    Sequence.fold (fun subst (v,s_v,t,s_t) -> bind subst v s_v t s_t) init seq

  let of_list ?(init=E) l = match l with
    | [] -> init
    | _::_ ->
      List.fold_left (fun subst (v,s_v,t,s_t) -> bind subst v s_v t s_t) init l

  let pp buf subst =
    let pp_binding buf (v,s_v,t,s_t) =
      Printf.bprintf buf "%a[%d] → %a[%d]" T.pp v s_v T.pp t s_t
    in
    match to_list subst with
    | [] -> Buffer.add_string buf "{}"
    | l -> Printf.bprintf buf "{%a}" (Util.pp_list ~sep:", " pp_binding) l

  let to_string = Util.on_buffer pp

  let fmt fmt subst =
    Format.pp_print_string fmt (to_string subst)

  let bij =
    Bij.(map
      ~inject:(fun s -> Sequence.to_list (to_seq s))
      ~extract:(fun seq -> of_seq (Sequence.of_list seq))
      (list_ (quad T.bij int_ T.bij int_)))
end

(** {2 Substitutions on types} *)

module Ty = struct
  module T = struct
    type t = Type.t
    let hash = Type.hash
    let eq = Type.eq
    let is_var = Type.is_var
    let vars = Type.free_vars
    let bij = Type.bij
    let pp = Type.pp
  end

  include Common(T)
  
  module Renaming = struct
    type t = Type.t H.t

    let create n = H.create n

    let dummy = create 2

    let clear h = H.clear h

    (* rename variable *)
    let rename h ty s_ty =
      if not (Type.is_var ty) then invalid_arg "renaming: expected type var";
      if h == dummy then ty else
      try H.find h (ty, s_ty)
      with Not_found ->
        let v = Type.var (H.length h) in
        H.add h (ty, s_ty) v;
        v
  end

  (* apply substitution *)
  let apply subst ~renaming ty sc_ty =
    let rec _apply ty sc_ty = match ty with
    | Type.App (s, l) ->
      let l' = List.map (fun ty' -> _apply ty' sc_ty) l in
      Type.app s l'
    | Type.Fun (ret, l) ->
      let ret' = _apply ret sc_ty in
      let l' = List.map (fun ty' -> _apply ty' sc_ty) l in
      Type.mk_fun ret' l'
    | Type.Var _ ->
      begin try
        (* type variable is bound, recurse *)
        let ty', sc_ty' = lookup subst ty sc_ty in
        _apply ty' sc_ty'
      with Not_found ->
        Renaming.rename renaming ty sc_ty
      end
    in
    _apply ty sc_ty

  let apply_no_renaming subst ty sc_ty =
    apply subst ~renaming:Renaming.dummy ty sc_ty
end

(** {2 Substitutions on various Terms} *)

module type TYPED_TERM = sig
  include TERM
  val get_type : t -> Type.t  (* only on variables *)
  val mk_var : ty:Type.t -> int -> t  (* build variable *)
end

(* Functor that builds substitutions from term substitution +
  type substitution *)
module MakeProd(T : TYPED_TERM) = struct
  module TSubst = Common(T)

  type term = T.t

  type t = {
    term : TSubst.t;
    ty : Ty.t;
  }

  let empty = {
    term = TSubst.empty;
    ty = Ty.empty;
  }

  let create size = { term = TSubst.create size; ty= Ty.create size; }
  
  let ty_subst s = s.ty

  let bind_ty s v s_v t s_t =
    { s with ty = Ty.bind s.ty v s_v t s_t }

  let update_ty s f =
    { s with ty = f s.ty }

  let is_empty s = TSubst.is_empty s.term && Ty.is_empty s.ty

  let lookup s = TSubst.lookup s.term

  let get_var s = TSubst.get_var s.term

  let mem s = TSubst.mem s.term

  let bind s v s_v t s_t =
    { s with term = TSubst.bind s.term v s_v t s_t }

  let append s1 s2 =
    { term = TSubst.append s1.term s2.term;
      ty = Ty.append s1.ty s2.ty; }

  let remove s v s_v =
    { s with term = TSubst.remove s.term v s_v; }

  module H = TSubst.H

  let domain s = TSubst.domain s.term

  let codomain s = TSubst.codomain s.term

  let introduced s = TSubst.introduced s.term

  let compose s1 s2 =
    failwith "compose: not implemented"

  let is_renaming s = TSubst.is_renaming s.term && Ty.is_renaming s.ty

  let pp buf s =
    Printf.bprintf buf "{term:%a, ty:%a}" TSubst.pp s.term Ty.pp s.ty

  let to_string = Util.on_buffer pp

  let fmt fmt s = Format.pp_print_string fmt (to_string s)

  let fold s = TSubst.fold s.term

  let iter s = TSubst.iter s.term

  let to_seq t = TSubst.to_seq t.term
  let to_list t = TSubst.to_list t.term
  let of_seq ?(init=empty) seq = { init with term=TSubst.of_seq ~init:init.term seq; }
  let of_list ?(init=empty) l = { init with term=TSubst.of_list ~init:init.term l; }

  let bij = Bij.(map
    ~inject:(fun s -> s.term, s.ty)
    ~extract:(fun (term,ty) -> {term; ty; })
    (pair TSubst.bij Ty.bij))

  module Renaming = struct
    type t = {
      term : T.t TSubst.H.t;
      ty : Ty.Renaming.t;
    }

    let create size = {
      term = TSubst.H.create size;
      ty = Ty.Renaming.create size;
    }

    let clear h =
      TSubst.H.clear h.term;
      Ty.Renaming.clear h.ty;
      ()

    let dummy = create 2

    let rename h v s_v =
      if not (T.is_var v) then invalid_arg "renaming: expected variable";
      if h == dummy then v else
      try TSubst.H.find h.term (v, s_v)
      with Not_found ->
        let ty = T.get_type v in
        let v' = T.mk_var ~ty (TSubst.H.length h.term) in
        TSubst.H.add h.term (v, s_v) v';
        v'

    let rename_ty h v s_v =
      Ty.Renaming.rename h.ty v s_v
  end
end

module FO = struct
  module T = FOTerm
  module F = FOFormula

  include MakeProd(T)

  let rec apply ~renaming subst t scope =
    if T.is_ground t then t (* subst(t) = t, if t ground *)
    else match t.T.term with
    | T.BoundVar _ -> t
    | T.Node (s, l) ->
      let l' = _apply_rec_list ~renaming subst scope l in
      T.mk_node s l'
    | T.Var i ->
      (* two cases, depending on whether [t] is bound by [subst] or not *)
      begin try
        let t', sc_t' = lookup subst t scope in
        (* also apply [subst] to [t']? *)
        if t' != t || scope <> sc_t'
          then (* _apply_rec also in the image of t *)
            apply ~renaming subst t' sc_t'
          else t'
      with Not_found ->
        (* variable not bound by [subst], rename it *)
        Renaming.rename renaming t scope
      end
  (* apply subst to the list, all elements of which have the given scope *)
  and _apply_rec_list ~renaming subst scope l = match l with
    | [] -> []
    | t::l' ->
      let new_t = apply ~renaming subst t scope in
      new_t :: _apply_rec_list ~renaming subst scope l'

  let apply_no_renaming subst t scope =
    apply ~renaming:Renaming.dummy subst t scope

  let apply_f ~renaming subst f scope =
    F.map
      (fun t -> apply ~renaming subst t scope)
      f
end

module HO = struct
  module T = HOTerm

  include MakeProd(T)

  let rec _apply_rec ~renaming ~depth subst t scope =
    if T.is_ground t then t (* subst(t) = t, if t ground *)
    else match t.T.term with
    | T.Const _
    | T.BoundVar _ -> t
    | T.Bind (s, t') ->
      let t'' = _apply_rec ~renaming ~depth:(depth+1) subst t' scope in
      let ty = T.get_type t in
      T.mk_bind s ~ty t''
    | T.Var i ->
      (* two cases, depending on whether [t] is bound by [subst] or not *)
      begin try
        let t', sc_t' = lookup subst t scope in
        (* if t' contains free De Bruijn symbols, lift them by [binder_depth] *)
        let t' = T.db_lift ~depth depth t' in
        (* also apply [subst] to [t']? *)
        if t' != t || scope <> sc_t'
          then (* _apply_rec also in the image of t *)
            _apply_rec ~renaming ~depth subst t' sc_t'
          else t'
      with Not_found ->
        (* variable not bound by [subst], rename it *)
        Renaming.rename renaming t scope
      end
    | T.At (t1, t2) ->
      let t1' = _apply_rec ~renaming ~depth subst t1 scope in
      let t2' = _apply_rec ~renaming ~depth subst t2 scope in
      T.mk_at t1' t2'

  (** Apply substitution to term, replacing variables by the terms they are bound to.
      [renaming] is used to rename free variables (not bound
      by [subst]) while avoiding collisions.
      [recursive] decides whether, when [v] is replaced by [t], [subst] is
      applied to [t] recursively or not (default true). *)
  let apply ?(depth=0) ~renaming subst t scope =
    (* apply subst to bound term. We need to keep track of
       how many binders are on the path to the variable, because of non-DB-closed
       terms that may occur in the codomain of [subst] *)
    _apply_rec ~renaming ~depth subst t scope

  let apply_no_renaming ?(depth=0) subst t scope =
    _apply_rec ~renaming:Renaming.dummy ~depth subst t scope
end
