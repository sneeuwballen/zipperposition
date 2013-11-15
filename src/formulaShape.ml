
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

module T = FOTerm
module F = FOFormula

(* map terms to distinct variables of same type *)
let __mk_vars args =
  List.mapi (fun i v -> T.mk_var ~ty:(T.ty v) i) args

let is_definition f =
  (* check that r is a definition of l=f(x1,...,xn) *)
  let check_def l r =
    match l.T.term with
    | T.Var _ | T.BoundVar _ -> false
    | T.Node (f, tyargs, ts) ->
      (* l=f(x1,...,xn) where r contains no other var than x1,...,xn, and n >= 0 *)
      List.for_all T.is_var ts &&
      let l' = T.mk_node ~tyargs f (__mk_vars ts) in
      FOUnif.are_variant l l'
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
    | T.Var _ | T.BoundVar _ -> false
    | T.Node (f, tyargs, ts) ->
      (* l=f(x1,...,xn) where r contains no other var than x1,...,xn, and n >= 0 *)
      List.for_all T.is_var ts &&
      let l' = T.mk_node ~tyargs f (__mk_vars ts) in
      (try ignore(FOUnif.variant l 0 l' 1); true with FOUnif.Fail -> false)
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
    | T.Var _ | T.BoundVar _ -> false
    | T.Node (_, _, _) -> List.for_all (fun x -> T.var_occurs x l) (T.vars r)
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
    | T.Var _ | T.BoundVar _ -> false
    | T.Node (_, _, _) ->
      List.for_all (fun x -> T.var_occurs x l) (F.free_variables r)
  in
  let f = F.open_forall f in
  match f.F.form with
  | F.Equiv ({F.form=F.Atom l},r) when check_rule l r -> Some (l,r)
  | F.Equiv (l,{F.form=F.Atom r}) when check_rule r l -> Some (r,l)
  | _ -> None

let is_const_definition f =
  match f.F.form with
  | F.Equal ({T.term=T.Node (s, _, [])}, r) -> Some (s, r)
  | F.Equal (l, {T.term=T.Node (s, _, [])}) -> Some (s, l)
  | _ -> None

let is_const_pred_definition f =
  match f.F.form with
  | F.Equiv ({F.form=F.Atom {T.term=T.Node (s, _, [])}}, r) -> Some (s,r)
  | F.Equiv (l,{F.form=F.Atom {T.term=T.Node (s, _, [])}}) -> Some (s,l)
  | _ -> None

(** {2 Interface to Tranform} *)

(* mconcat on a list of lazy options *)
let rec _lazy_mappend l = match l with
  | (lazy (Some x)) :: _ -> Some x
  | (lazy None) :: l' -> _lazy_mappend l'
  | [] -> None

(* fmap on options *)
let (>|=) opt f = match opt with
  | None -> None
  | Some x -> Some (f x)

(* fmap on the head of the list *)
let (@|=) l f = match l with
  | [] -> None
  | x::_ -> Some (f x)

let detect seq =
  let seq = Sequence.fmap
    (fun form ->
      _lazy_mappend
        [ lazy (is_definition form >|= Transform.of_term_rule)
        ; lazy (is_pred_definition form >|= Transform.of_form_rule)
        ; lazy (is_rewrite_rule form @|= Transform.of_term_rule)
        ; lazy (is_pred_rewrite_rule form >|= Transform.of_form_rule)
        ]
    )
    seq
  in
  Sequence.to_rev_list seq

let detect_list l = detect (Sequence.of_list l)

let detect_def ?(only:[`Pred|`Term] option) ?(arity:[`Zero|`Nonzero] option) forms =
  let pred_ok, term_ok = match only with
  | Some `Pred -> true, false
  | Some `Term -> false, true
  | None -> true, true
  and const_ok, fun_ok = match arity with
  | Some `Zero -> true, false
  | Some `Nonzero -> false, true
  | None -> true, true
  in
  let seq = Sequence.fmap
    (fun form ->
      _lazy_mappend
      [ lazy
        (if pred_ok then match is_pred_definition form with
        | Some (l,p) ->
          if (T.is_const l && const_ok) || (not (T.is_const l) && fun_ok)
            then Some (Transform.of_form_rule (l,p))
            else None
        | None -> None else None)
      ; lazy
        (if term_ok then match is_definition form with
        | Some (l,r) ->
          if (T.is_const l && const_ok) || (not (T.is_const l) && fun_ok)
            then Some (Transform.of_term_rule (l,r))
            else None
        | None -> None else None)
      ])
    forms
  in Sequence.to_rev_list seq
