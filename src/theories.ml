(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {6 Specifications of Built-in Theories} *)

open Logtk

module T = FOTerm
module F = Formula.FO

type scope = Substs.scope
type term = FOTerm.t

(** {2 Associativity-Commutativity} *)

module AC = struct
  type t = {
    sym : Symbol.t;
    ty : Type.t;
  }

end

(** {2 Total Ordering} *)

module TotalOrder = struct
  type t = {
    less : Symbol.t;
    lesseq : Symbol.t;
    ty : Type.t;
  } (** A single instance of total ordering *)

  type lit = {
    left : term;
    right : term;
    tyargs : Type.t list;
    strict : bool;
    order : t;
  } (** A literal is an atomic inequality. [strict] is [true] iff the
      literal is a strict inequality, and the ordering itself
      is also provided. *)

  let eq i1 i2 =
    Symbol.eq i1.less i2.less
    && Symbol.eq i1.lesseq i2.lesseq
    && Type.eq i1.ty i2.ty

  let hash i =
    Hash.hash_int3 (Symbol.hash i.less) (Symbol.hash i.lesseq) (Type.hash i.ty)

  let map f lit =
    { lit with left=f lit.left; right=f lit.right; }

  let apply_subst ~renaming subst lit s_lit =
    let tyargs = List.map
      (fun ty -> Substs.Ty.apply ~renaming subst ty s_lit)
      lit.tyargs
    in
    let left = Substs.FO.apply ~renaming subst lit.left s_lit in
    let right = Substs.FO.apply ~renaming subst lit.right s_lit in
    { lit with left; right; tyargs; }

  (* not a<b is b<=a in a total ordering *)
  let neg lit =
    { lit with left=lit.right; right=lit.left; strict=not lit.strict; }

  let less_const instance = T.const ~ty:instance.ty instance.less
  let lesseq_const instance = T.const ~ty:instance.ty instance.lesseq

  let pp buf i =
    Printf.bprintf buf "total_order{%a, %a}" Symbol.pp i.less Symbol.pp i.lesseq

  let to_string i = Util.on_buffer pp i

  let fmt fmt i = Format.pp_print_string fmt (to_string i)
end

(** {2 Set Theory} *)

module Sets = struct
  type t = {
    member : Symbol.t;
    subset : Symbol.t;
    subseteq : Symbol.t;
    union : Symbol.t;
    inter : Symbol.t;
    diff : Symbol.t;
    emptyset : Symbol.t;
    singleton : Symbol.t;
    complement : Symbol.t;
    set_type : Symbol.t;
  }

  let _ty_member ~sets =
    let x = Type.var 0 in
    let _set a = Type.app sets.set_type [a] in
    Type.(forall [x] (TPTP.o <== [x; _set x]))

  let _ty_subset ~sets =
    let x = Type.var 0 in
    let _set a = Type.app sets.set_type [a] in
    Type.(forall [x] (TPTP.o <== [_set x; _set x]))

  let _ty_union ~sets =
    let x = Type.var 0 in
    let _set a = Type.app sets.set_type [a] in
    Type.(forall [x] (_set x <== [_set x; _set x]))

  let _ty_diff ~sets =
    let x = Type.var 0 in
    let _set a = Type.app sets.set_type [a] in
    Type.(forall [x] (_set x <== [_set x; _set x]))

  let _ty_empty ~sets =
    let x = Type.var 0 in
    let _set a = Type.app sets.set_type [a] in
    Type.(forall [x] (_set x))

  let _ty_singleton ~sets =
    let x = Type.var 0 in
    let _set a = Type.app sets.set_type [a] in
    Type.(forall [x] (_set x <== [x]))

  let _ty_complement ~sets =
    let x = Type.var 0 in
    let _set a = Type.app sets.set_type [a] in
    Type.(forall [x] (_set x <== [_set x]))

  let signature sets =
    Signature.of_list
      [ sets.member, _ty_member ~sets
      ; sets.subset, _ty_subset ~sets
      ; sets.subseteq, _ty_subset ~sets
      ; sets.union, _ty_union ~sets
      ; sets.inter, _ty_union ~sets
      ; sets.diff, _ty_diff ~sets
      ; sets.emptyset, _ty_empty ~sets
      ; sets.singleton, _ty_singleton ~sets
      ; sets.complement, _ty_complement ~sets
      ]

  let default = {
    member = Symbol.of_string "member";
    subset = Symbol.of_string "subset";
    subseteq = Symbol.of_string "subseteq";
    union = Symbol.of_string "union";
    inter = Symbol.of_string "intersection";
    diff = Symbol.of_string "difference";
    emptyset = Symbol.of_string "emptyset";
    singleton = Symbol.of_string "singleton";
    complement = Symbol.of_string "complement";
    set_type = Symbol.of_string "set";
  }

  type view =
    | Member of term * term
    | Subset of term * term
    | Subseteq of term * term
    | Union of term list
    | Inter of term list
    | Diff of term * term
    | Emptyset of Type.t
    | Singleton of term
    | Complement of term
    | Other of term  (** not a set constructor *)

  (** flattens a union or internsection into a list of sets
      helps the preprocessing *)
  let rec unfold symbol t acc =
    let module TC = T.Classic in
    match TC.view t with
      | TC.App (s, _, [s1;s2]) when Symbol.eq s symbol -> unfold symbol s1 (unfold symbol s2 acc)
      | _ -> t::acc

  let view ~sets t =
    let module TC = T.Classic in
    match TC.view t with
    | TC.App (s, _, [x;set]) when Symbol.eq s sets.member -> Member (x,set)
    | TC.App (s, _, [s1;s2]) when Symbol.eq s sets.subset -> Subset (s1,s2)
    | TC.App (s, _, [s1;s2]) when Symbol.eq s sets.subseteq -> Subseteq (s1,s2)
    | TC.App (s, _, _) when Symbol.eq s sets.union -> Union (unfold sets.union t [])
    | TC.App (s, _, _) when Symbol.eq s sets.inter -> Inter (unfold sets.inter t [])
    | TC.App (s, _, [s1;s2]) when Symbol.eq s sets.diff -> Diff(s1,s2)
    | TC.App (s, [ty], []) when Symbol.eq s sets.emptyset -> Emptyset ty
    | TC.App (s, _, [t]) when Symbol.eq s sets.singleton -> Singleton t
    | TC.App (s, _, [t]) when Symbol.eq s sets.complement -> Complement t
    | _ -> Other t

  let mk_member ~sets x set =
    T.app_full (T.const ~ty:(_ty_member ~sets) sets.member) [T.ty x] [x;set]

  (* if t:set(alpha) then return alpha, otherwise raise Invalid_argument *)
  let _get_set_type ~sets t =
    match Type.view (T.ty t) with
      | Type.App (s, [alpha]) when Symbol.eq s sets.set_type -> alpha
      | ty -> invalid_arg (Util.sprintf "%a does not a set type" T.pp t)

  let is_set ~sets t =
    match Type.view (T.ty t) with
      | Type.App (s,_) when Symbol.eq s sets.set_type -> true
      | _ -> false

  let mk_subset ~sets s1 s2 =
    let alpha = _get_set_type ~sets s1 in
    T.app_full (T.const ~ty:(_ty_subset ~sets) sets.subset) [alpha] [s1;s2]

  let mk_subseteq ~sets s1 s2 =
    let alpha = _get_set_type ~sets s1 in
    T.app_full (T.const ~ty:(_ty_subset ~sets) sets.subseteq) [alpha] [s1;s2]

  let rec mk_union ~sets s_list =
    match s_list with
      | [] -> failwith "type of intersection not defined"
      | [set] -> set
      | [s1;s2] ->
        let alpha = _get_set_type ~sets s1 in
          T.app_full (T.const ~ty:(_ty_union ~sets) sets.union) [alpha] [s1;s2]
      | s1::s2::t ->
        let alpha = _get_set_type ~sets s1 in
          T.app_full (T.const ~ty:(_ty_union ~sets) sets.union) [alpha] [s1;mk_union ~sets (s2::t)]

  let rec mk_inter ~sets s_list =
    match s_list with
      | [] -> failwith "type of intersection not defined"
      | [set] -> set
      | [s1;s2] ->
        let alpha = _get_set_type ~sets s1 in
          T.app_full (T.const ~ty:(_ty_union ~sets) sets.inter) [alpha] [s1;s2]
      | s1::s2::t ->
        let alpha = _get_set_type ~sets s1 in
          T.app_full (T.const ~ty:(_ty_union ~sets) sets.inter) [alpha] [s1;mk_inter ~sets (s2::t)]

  let mk_diff ~sets s1 s2 =
    let alpha = _get_set_type ~sets s1 in
    T.app_full (T.const ~ty:(_ty_diff ~sets) sets.diff) [alpha] [s1;s2]

  let mk_empty ~sets ty =
    T.tyapp (T.const ~ty:(_ty_empty ~sets) sets.emptyset) ty

  let mk_singleton ~sets t =
    T.app_full (T.const ~ty:(_ty_singleton ~sets) sets.singleton) [T.ty t] [t]

  let mk_complement~sets s =
    let alpha = _get_set_type ~sets s in
    T.app_full (T.const ~ty:(_ty_complement ~sets) sets.complement) [alpha] [s]

  let mk_set_type ~sets ty =
    Type.app sets.set_type [ty]

  let pp buf sets = failwith "TODO" (* TODO *)
end
