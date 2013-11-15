
(*
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

(** {1 Unification of Types} *)

module Ty = Type
module S = Substs.Ty

let prof_unify = Util.mk_profiler "TypeUnif.unify"
let prof_match = Util.mk_profiler "TypeUnif.match"
let prof_variant = Util.mk_profiler "TypeUnif.variant"

type scope = Substs.scope

type error = {
  left : Type.t;
  s_left : scope;
  right : Type.t;
  s_right : scope;
  subst : Substs.Ty.t;
}

exception Error of error

let fail subst ty1 s1 ty2 s2 =
  raise (Error {
    left = ty1;
    s_left = s1;
    right = ty2;
    s_right = s2;
    subst;
  })

let pp_error buf e =
  Printf.bprintf buf "type error when unifying %a[%d] with %a[%d]"
    Ty.pp e.left e.s_left Ty.pp e.right e.s_right

let error_to_string = Util.on_buffer pp_error

let fmt_error fmt e = Format.pp_print_string fmt (error_to_string e)

let () =
  Printexc.register_printer (function
    | Error e -> Some (error_to_string e)
    | _ -> None)

(* occur-check *)
let _occur_check subst v s_v t s_t =
  let rec check t s_t = match t.Ty.ty with
  | Ty.Var _ when s_v = s_t && Ty.eq v t -> true
  | Ty.Var _ ->
    begin try
      let t', s_t' = S.lookup subst t s_t in
      check t' s_t'
    with Not_found -> false
    end
  | Ty.BVar _ -> false
  | Ty.App (_, l) -> List.exists (fun t' -> check t' s_t) l
  | Ty.Fun (ret, l) ->
    check ret s_t || List.exists (fun t' -> check t' s_t) l
  | Ty.Forall ty' -> check ty' s_t
  in
  check t s_t

(** {2 Unification} *)

(* unification *)
let rec _unify_rec subst ty1 s1 ty2 s2 =
  let ty1, s1 = S.get_var subst ty1 s1 in
  let ty2, s2 = S.get_var subst ty2 s2 in
  match ty1.Ty.ty, ty2.Ty.ty with
  | _ when Ty.eq ty1 ty2 && (s1 = s2 || Ty.is_ground ty1)-> subst
  | Ty.Var _, _ ->
    if _occur_check subst ty1 s1 ty2 s2
      then fail subst ty1 s1 ty2 s2
      else S.bind subst ty1 s1 ty2 s2
  | _, Ty.Var _ ->
    if _occur_check subst ty2 s2 ty1 s1
      then fail subst ty2 s2 ty1 s1
      else S.bind subst ty2 s2 ty1 s1
  | Ty.App (sym1, l1), Ty.App (sym2, l2) when sym1 = sym2 && List.length l1 = List.length l2 ->
    List.fold_left2
      (fun subst ty1 ty2 -> _unify_rec subst ty1 s1 ty2 s2)
      subst
      l1 l2
  | Ty.Fun (ret1, l1), Ty.Fun (ret2, l2) when List.length l1 = List.length l2 ->
    let subst = _unify_rec subst ret1 s1 ret2 s2 in
    List.fold_left2
      (fun subst ty1 ty2 -> _unify_rec subst ty1 s1 ty2 s2)
      subst
      l1 l2
  | Ty.BVar i1, Ty.BVar i2 when i1 = i2 -> subst
  | Ty.Forall ty1', Ty.Forall ty2' ->
    _unify_rec subst ty1' s1 ty2' s2
  | _ -> fail subst ty1 s1 ty2 s2

let unify ?(subst=S.empty) ty1 s1 ty2 s2 =
  Util.enter_prof prof_unify;
  try
    let subst =
      match ty1.Ty.ty, ty2.Ty.ty with
      | Ty.App (str1, []), Ty.App (str2, []) ->
        (* optimize for frequent case! *)
        if Ty.eq ty1 ty2
          then subst
          else fail subst ty1 s1 ty2 s2
      | _ -> _unify_rec subst ty1 s1 ty2 s2
    in
    Util.exit_prof prof_unify;
    subst
  with e ->
    Util.exit_prof prof_unify;
    raise e

let unify_fo ?(subst=Substs.FO.empty) ty1 s1 ty2 s2 =
  Substs.FO.update_ty subst
    (fun subst -> unify ~subst ty1 s1 ty2 s2)

let unify_ho ?(subst=Substs.HO.empty) ty1 s1 ty2 s2 =
  Substs.HO.update_ty subst
    (fun subst -> unify ~subst ty1 s1 ty2 s2)

let are_unifiable ty1 ty2 =
  try
    ignore (unify ty1 0 ty2 1);
    true
  with Error _ ->
    false

let unifier ty1 ty2 =
  let subst = unify ty1 0 ty2 1 in
  let renaming = S.Renaming.create 5 in
  let ty = S.apply subst ~renaming ty1 0 in
  ty

(** {2 Matching} *)

let rec _match_rec ~protect subst ty1 s1 ty2 s2 =
  let ty1, s1 = S.get_var subst ty1 s1 in
  let ty2, s2 = S.get_var subst ty2 s2 in
  match ty1.Ty.ty, ty2.Ty.ty with
  | _ when Ty.eq ty1 ty2 && (s1 = s2 || Ty.is_ground ty1)-> subst
  | Ty.Var _, _ ->
    (* fail if occur check, or if we need to bind a protected variable *)
    if _occur_check subst ty1 s1 ty2 s2 || (List.memq ty1 protect && s1 = s2)
      then fail subst ty1 s1 ty2 s2
      else S.bind subst ty1 s1 ty2 s2
  | Ty.App (sym1, l1), Ty.App (sym2, l2) when sym1 = sym2 && List.length l1 = List.length l2 ->
    List.fold_left2
      (fun subst ty1 ty2 -> _match_rec ~protect subst ty1 s1 ty2 s2)
      subst
      l1 l2
  | Ty.Fun (ret1, l1), Ty.Fun (ret2, l2) when List.length l1 = List.length l2 ->
    let subst = _match_rec ~protect subst ret1 s1 ret2 s2 in
    List.fold_left2
      (fun subst ty1 ty2 -> _match_rec ~protect subst ty1 s1 ty2 s2)
      subst
      l1 l2
  | Ty.BVar i1, Ty.BVar i2 when i1 = i2 -> subst
  | Ty.Forall ty1', Ty.Forall ty2' ->
    _match_rec ~protect subst ty1' s1 ty2' s2
  | _ -> fail subst ty1 s1 ty2 s2

let match_ ?(subst=S.empty) ty1 s1 ty2 s2 =
  Util.enter_prof prof_match;
  let protect = Ty.free_vars ty2 in
  try
    let subst = _match_rec ~protect subst ty1 s1 ty2 s2 in
    Util.exit_prof prof_match;
    subst
  with e ->
    Util.exit_prof prof_match;
    raise e

let match_fo ?(subst=Substs.FO.empty) ty1 s1 ty2 s2 =
  Substs.FO.update_ty subst
    (fun subst -> match_ ~subst ty1 s1 ty2 s2)

let match_ho ?(subst=Substs.HO.empty) ty1 s1 ty2 s2 =
  Substs.HO.update_ty subst
    (fun subst -> match_ ~subst ty1 s1 ty2 s2)


(** {2 Alpha-equivalence} *)

(* alpha-equivalence check *)
let rec _variant_rec subst ty1 s1 ty2 s2 =
  let ty1, s1 = S.get_var subst ty1 s1 in
  let ty2, s2 = S.get_var subst ty2 s2 in
  match ty1.Ty.ty, ty2.Ty.ty with
  | _ when Ty.eq ty1 ty2 && (s1 = s2 || Ty.is_ground ty1)-> subst
  | Ty.Var i1, Ty.Var i2 ->
    (* can bind variables if they do not belong to the same scope *)
    if s1 <> s2
      then S.bind subst ty1 s1 ty2 s2
      else fail subst ty1 s1 ty2 s2
  | _, Ty.Var _
  | Ty.Var _, _ -> fail subst ty2 s2 ty1 s1
  | Ty.App (sym1, l1), Ty.App (sym2, l2) when sym1 = sym2 && List.length l1 = List.length l2 ->
    List.fold_left2
      (fun subst ty1 ty2 -> _unify_rec subst ty1 s1 ty2 s2)
      subst
      l1 l2
  | Ty.Fun (ret1, l1), Ty.Fun (ret2, l2) when List.length l1 = List.length l2 ->
    let subst = _unify_rec subst ret1 s1 ret2 s2 in
    List.fold_left2
      (fun subst ty1 ty2 -> _unify_rec subst ty1 s1 ty2 s2)
      subst
      l1 l2
  | Ty.BVar i1, Ty.BVar i2 when i1 = i2 -> subst
  | Ty.Forall ty1', Ty.Forall ty2' ->
    _variant_rec subst ty1' s1 ty2' s2
  | _ -> fail subst ty1 s1 ty2 s2

let variant ?(subst=S.empty) ty1 s1 ty2 s2 =
  Util.enter_prof prof_variant;
  try
    let subst = _variant_rec subst ty1 s1 ty2 s2 in
    Util.exit_prof prof_variant;
    subst
  with e ->
    Util.exit_prof prof_variant;
    raise e

let variant_fo ?(subst=Substs.FO.empty) ty1 s1 ty2 s2 =
  Substs.FO.update_ty subst
    (fun subst -> variant ~subst ty1 s1 ty2 s2)

let variant_ho ?(subst=Substs.HO.empty) ty1 s1 ty2 s2 =
  Substs.HO.update_ty subst
    (fun subst -> variant ~subst ty1 s1 ty2 s2)


let are_variants ty1 ty2 =
  try
    ignore (variant ty1 0 ty2 1);
    true
  with Error _ ->
    false
