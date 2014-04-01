
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

(** {1 Type erasure for terms and formulas} *)

module FO = struct
  module T = FOTerm
  module BT = Basic.FO

  let erase ?(depth=0) t =
    let rec erase t = match t.T.term with
    | T.Var i -> BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "X%d" i)
    | T.BoundVar i -> BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "Y%d" (depth-i-1))
    | T.Node (s, tyargs, l) ->
      let tyargs = List.map (fun ty -> BT.of_ty (TypeConversion.to_basic ty)) tyargs in
      let s = Symbol.to_basic s in
      BT.app s (tyargs @ List.map erase l)
    in
    erase t
end

module Form = struct
  module F = FOFormula
  module BT = Basic.FO
  module BF = Basic.Form

  let erase ?(close_ty=true) ?(depth=0) f =
    let rec erase depth f = match f.F.form with
      | F.True -> BF.mk_true
      | F.False -> BF.mk_false
      | F.Atom p -> BF.atom (FO.erase ~depth p)
      | F.Equal (t1, t2) -> BF.mk_eq (FO.erase ~depth t1) (FO.erase ~depth t2)
      | F.And l -> BF.mk_and (List.map (erase depth) l)
      | F.Or l -> BF.mk_or (List.map (erase depth) l)
      | F.Not f' -> BF.mk_not (erase depth f')
      | F.Imply (f1, f2) -> BF.mk_imply (erase depth f1) (erase depth f2)
      | F.Equiv (f1, f2) -> BF.mk_equiv (erase depth f1) (erase depth f2)
      | F.Forall (ty, f') ->
        let v = BT.var ~ty:(TypeConversion.to_basic ty) (Util.sprintf "Y%d" depth) in
        BF.forall [v] (erase (depth+1) f')
      | F.Exists (ty, f') ->
        let v = BT.var ~ty:(TypeConversion.to_basic ty) (Util.sprintf "Y%d" depth) in
        BF.exists [v] (erase (depth+1) f')
    in
    let f' = erase depth f in
    let f' = if close_ty
      then
        let tyvars = Type.Set.elements (F.ty_vars Type.Set.empty f) in
        let tyvars = List.map (fun ty -> BT.of_ty (TypeConversion.to_basic ty)) tyvars in
        BF.forall tyvars f'
      else f' in
    f'
end

module HO = struct
  module T = HOTerm
  module BT = Basic.HO

  let erase ?(close_ty=true) ?(depth=0) t =
    let rec erase depth t = match t.T.term with
    | T.Const s -> BT.const (Symbol.to_basic s)
    | T.At (t, tyargs, l) ->
      let tyargs = List.map (fun ty -> BT.of_ty (TypeConversion.to_basic ty)) tyargs in
      BT.app (erase depth t) (tyargs @ List.map (erase depth) l)
    | T.Var i ->
      BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "X%d" i)
    | T.BoundVar i ->
      BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "Y%d" (depth-i-1))
    | T.Lambda t' ->
      let var = BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "Y%d" depth) in
      BT.lambda ~var (erase (depth+1) t')
    in
    let t' = erase depth t in
    let t' = if close_ty
      then
        let tyvars = Type.Set.elements (T.ty_vars Type.Set.empty t) in
        let tyvars = List.map (fun ty -> BT.of_ty (TypeConversion.to_basic ty)) tyvars in
        BT.forall_list tyvars t'
      else t' in
    t'
end
