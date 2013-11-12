
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

(** {1 Arbitrary Typed Terms and Formulas} *)

open Logtk
open QCheck

module T = FOTerm
module F = FOFormula
module HOT = HOTerm

type 'a arbitrary = 'a QCheck.Arbitrary.t

module ArbitraryBasic = struct
  module UT = Basic.FO

  let ground =
    let a = UT.const (Symbol.mk_const "a") in
    let b = UT.const (Symbol.mk_const "b") in
    let c = UT.const (Symbol.mk_const "c") in
    let d = UT.const (Symbol.mk_const "d") in
    let e = UT.const (Symbol.mk_const "e") in
    let f x y = UT.app (Symbol.mk_const "f") [x; y] in
    let sum x y = UT.app (Symbol.mk_const "sum") [x; y] in
    let g x = UT.app (Symbol.mk_const "g") [x] in
    let h x = UT.app (Symbol.mk_const "h") [x] in
    let ite x y z = UT.app (Symbol.mk_const "ite") [x; y; z] in
    Arbitrary.(
      let base = among [a; b; c; d; e; ] in
      let t = fix ~max:6 ~base (fun sub ->
        choose [ lift2 f sub sub; lift g sub; lift h sub; sub;
          choose [lift2 sum sub sub; lift3 ite sub sub sub]])
      in
      t)

  let default =
    let a = UT.const (Symbol.mk_const "a") in
    let b = UT.const (Symbol.mk_const "b") in
    let c = UT.const (Symbol.mk_const "c") in
    let d = UT.const (Symbol.mk_const "d") in
    let e = UT.const (Symbol.mk_const "e") in
    let x = UT.var ~ty:(Basic.Ty.var "A") "X" in
    let y = UT.var ~ty:(Basic.Ty.var "B") "Y" in
    let z = UT.var ~ty:(Basic.Ty.var "C") "Z" in
    let f x y = UT.app (Symbol.mk_const "f") [x; y] in
    let sum x y = UT.app (Symbol.mk_const "sum") [x; y] in
    let g x = UT.app (Symbol.mk_const "g") [x] in
    let h x = UT.app (Symbol.mk_const "h") [x] in
    let ite x y z = UT.app (Symbol.mk_const "ite") [x; y; z] in
    Arbitrary.(
      let base = among [a; b; c; d; e; x; y; z] in
      let t = fix ~max:6 ~base (fun sub ->
        choose [ lift2 f sub sub; lift g sub; lift h sub; sub;
          choose [lift2 sum sub sub; lift3 ite sub sub sub]])
      in
      t)

  let pred = 
    let p x y = UT.app (Symbol.mk_const "p") [x; y] in
    let q x = UT.app (Symbol.mk_const "q") [x] in
    let r x = UT.app (Symbol.mk_const "r") [x] in
    let s = UT.const (Symbol.mk_const "s") in
    let sub = default in
    QCheck.Arbitrary.(choose
      [ lift2 p sub sub; lift q sub; lift r sub; return s; ])

  module HO = struct
    let ground st = assert false

    let default st = assert false
  end
end

let default =
  Arbitrary.(ArbitraryBasic.default >>= fun t ->
    let ctx = TypeInference.Ctx.create () in
    return (TypeInference.FO.convert ~ctx t))

let ground =
  Arbitrary.(ArbitraryBasic.ground >>= fun t ->
    let ctx = TypeInference.Ctx.create () in
    return (TypeInference.FO.convert ~ctx t))

let pred =
  Arbitrary.(ArbitraryBasic.pred >>= fun t ->
    let ctx = TypeInference.Ctx.create () in
    let ty, closure = TypeInference.FO.infer ctx t 0 in
    TypeInference.Ctx.unify_and_set ctx ty 0 Type.o 0;
    let t = TypeInference.Ctx.apply_closure ctx closure in
    return t)

let pos t =
  let module PB = Position.Build in
  Arbitrary.(
    let rec recurse t pb st =
      let stop = return (PB.to_pos pb) in
      match t.T.term with
        | T.Node (_, [])
        | T.Var _
        | T.BoundVar _ -> PB.to_pos pb
        | T.Node (_, l) ->
          choose (stop :: List.mapi (fun i t' -> recurse t' (PB.add pb i)) l) st
    in
    recurse t (PB.of_pos []))

module HO = struct
  let ground =
    Arbitrary.(ArbitraryBasic.HO.ground >>= fun t ->
      let ctx = TypeInference.Ctx.create () in
      return (TypeInference.HO.convert ~ctx t))

  let default =
    Arbitrary.(ArbitraryBasic.HO.default >>= fun t ->
      let ctx = TypeInference.Ctx.create () in
      return (TypeInference.HO.convert ~ctx t))
end

(*
let arbitrary signature =
  let open QCheck.Arbitrary in
  let base, recur = Symbol.Map.partition (fun _ ty -> Type.arity ty = 0) signature in
  let consts = among (Symbol.Map.fold (fun s ty acc -> mk_const ~ty s :: acc) base []) in
  let funs = Sequence.to_list (Symbol.Map.to_seq recur) in
  _ar_any_ty ~ground:false ~depth:0 ~consts ~funs signature

let arbitrary_ground signature =
  let open QCheck.Arbitrary in
  let base, recur = Symbol.Map.partition (fun _ ty -> Type.arity ty = 0) signature in
  let consts = among (Symbol.Map.fold (fun s ty acc -> mk_const ~ty s :: acc) base []) in
  let funs = Sequence.to_list (Symbol.Map.to_seq recur) in
  _ar_any_ty ~ground:true ~depth:0 ~consts ~funs signature
      
let arbitrary_pred signature =
  let open QCheck.Arbitrary in
  let types = Sequence.to_list (Symbol.Map.values signature) in
  let some_ty = among types in
  let mk_preds st =
    [ Symbol.mk_const "p", Type.(o <== [some_ty st; some_ty st])
    ; Symbol.mk_const "q", Type.(o <== [some_ty st])
    ; Symbol.mk_const "r", Type.(o <== [some_ty st])
    ; Symbol.mk_const "s", Type.o
    ]
  in
  mk_preds >>= fun preds ->
  let signature = List.fold_left
    (fun sign (s,ty) -> Signature.declare sign s ty)
    signature preds in
  let base, recur = Symbol.Map.partition (fun _ ty -> Type.arity ty = 0) signature in
  let consts = among (Symbol.Map.fold (fun s ty acc -> mk_const ~ty s :: acc) base []) in
  let funs = Sequence.to_list (Symbol.Map.to_seq recur) in
  _ar_ty ~ground:false ~depth:0 ~consts ~funs Type.o

(* TODO *)

(* generate a term of the given type *)
let rec _ar_ty ~ground ~depth ~consts ~funs ty =
  let open QCheck.Arbitrary in
  (* choice: either a variable, or a constant, or a recursive case *)
  let vars = among [mk_var ~ty 0; mk_var ~ty 1; mk_var ~ty 2] in
  if depth > 5
    then if ground then consts
    else choose [consts; vars]
  else if ground
    then choose
      [ consts
      ; _non_const ~ground ~depth:(depth+1) ~consts ~funs ty
      ]
    else choose
      [ consts
      ; vars
      ; _non_const ~ground ~depth:(depth+1) ~consts ~funs ty
      ]
(* generate a composite term using [gen] to make subterms. The generated
    term has type [ty] *)
and _non_const ~ground ~depth ~consts ~funs ty =
  let open QCheck.Arbitrary in
  let funs = List.filter (fun (_,ty') -> Type.eq ty ty') funs in
  among funs >>= fun (s, fun_ty) ->
  match fun_ty with
  | Type.Fun (ty, args) ->
    list_sequence (List.map (_ar_ty ~ground ~depth ~consts ~funs) args) >>= fun l ->
    return (mk_node ~ty s l)
  | _ -> assert false 
(* term of any type in the const set *)
and _ar_any_ty ~ground ~depth ~consts ~funs signature =
  let open QCheck.Arbitrary in
  let types = Sequence.to_list (Symbol.Map.values signature) in
  among types >>= fun ty ->
  _ar_ty ~ground ~depth ~consts ~funs ty


let arbitrary_atom =
  QCheck.Arbitrary.(choose
    [ lift mk_atom T.arbitrary_pred
    ; lift (fun t -> mk_not (mk_atom t)) T.arbitrary_pred
    ; lift2 mk_eq T.arbitrary T.arbitrary
    ; lift2 mk_neq T.arbitrary T.arbitrary
    ; choose [ return mk_true; return mk_false ]
    ])

let arbitrary =
  QCheck.Arbitrary.(
    let f = fix ~max:10 ~base:arbitrary_atom
      (fun sub_f -> choose
        [ lift mk_or (list sub_f)
        ; lift mk_and (list sub_f)
        ; lift2 mk_equiv sub_f sub_f
        ; lift2 mk_imply sub_f sub_f
        ; lift mk_not sub_f
        ; lift close_forall sub_f
        ; lift close_exists sub_f
        ])
    in
    f)

let arbitrary_clause = QCheck.Arbitrary.(list arbitrary_atom)
*)
