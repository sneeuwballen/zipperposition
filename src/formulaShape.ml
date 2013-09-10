
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

(** {6 Detect some specific formulas} *)

module T = Term
module F = Formula

(* map terms to distinct variables of same type *)
let __mk_vars args =
  List.mapi (fun i t -> T.mk_var ?ty:t.T.type_ i) args

let is_definition f =
  (* check that r is a definition of l=f(x1,...,xn) *)
  let check_def l r =
    match l.T.term with
    | T.Var _ | T.BoundVar _ | T.Bind _ | T.At _ -> false
    | T.Node (f, ts) ->
      (* l=f(x1,...,xn) where r contains no other var than x1,...,xn, and n >= 0 *)
      let l' = T.mk_node f (__mk_vars ts) in
      (try ignore(Unif.variant l 0 l' 1); true with Unif.Fail -> false)
      && not (T.contains_symbol f r)
      && List.for_all (fun x -> T.var_occurs x l) (T.vars r)
  in
  let f = F.open_forall f in
  match f.F.form with
  | F.Equal(l,r) when check_def l r -> Some (l,r)
  | F.Equal(l,r) when check_def r l -> Some (r,l)
  | _ -> None

let is_pred_definition f =
  (* check that r is a predicate definition of l=f(x1,...,xn) *)
  let check_def l r =
    match l.T.term with
    | T.Var _ | T.BoundVar _ | T.Bind _ | T.At _ -> false
    | T.Node (f, ts) ->
      (* l=f(x1,...,xn) where r contains no other var than x1,...,xn, and n >= 0 *)
      let l' = T.mk_node f (__mk_vars ts) in
      (try ignore(Unif.variant l 0 l' 1); true with Unif.Fail -> false)
      && not (F.contains_symbol f r)
      && List.for_all (fun x -> T.var_occurs x l) (F.free_variables r)
  in
  let f = F.open_forall f in
  match f.F.form with
  | F.Equiv({F.form=F.Atom l},r) when check_def l r -> Some (l,r)
  | F.Equiv(l,{F.form=F.Atom r}) when check_def r l -> Some (r,l)
  | _ -> None

let is_rewrite_rule f =
  (* check that l -> r is an acceptable rewrite rule *)
  let check_rule l r =
    match l.T.term with
    | T.Var _ | T.Bind _ | T.BoundVar _ | T.At _ -> false
    | T.Node (_, _) -> List.for_all (fun x -> T.var_occurs x l) (T.vars r)
  in
  let f = F.open_forall f in
  match f.F.form with
  | F.Equal (l,r) ->
    (if check_rule l r then [l,r] else []) @ (if check_rule r l then [r, l] else [])
  | _ -> []

let is_pred_rewrite_rule f =
  (* check that l -> r is an acceptable predicate rewrite rule *)
  let check_rule l r =
    match l.T.term with
    | T.Var _ | T.Bind _ | T.BoundVar _ | T.At _ -> false
    | T.Node (_, _) ->
      List.for_all (fun x -> T.var_occurs x l) (F.free_variables r)
  in
  let f = F.open_forall f in
  match f.F.form with
  | F.Equiv ({F.form=F.Atom l},r) when check_rule l r -> Some (l,r)
  | F.Equiv (l,{F.form=F.Atom r}) when check_rule r l -> Some (r,l)
  | _ -> None

let is_const_definition f =
  match f.F.form with
  | F.Equal ({T.term=T.Node (s, [])}, r) -> Some (s, r)
  | F.Equal (l, {T.term=T.Node (s, [])}) -> Some (s, l)
  | _ -> None

let is_const_pred_definition f =
  match f.F.form with
  | F.Equiv ({F.form=F.Atom {T.term=T.Node (s, [])}}, r) -> Some (s,r)
  | F.Equiv (l,{F.form=F.Atom {T.term=T.Node (s, [])}}) -> Some (s,l)
  | _ -> None
