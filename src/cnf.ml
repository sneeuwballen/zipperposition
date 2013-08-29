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

(** {1 Reduction to CNF and simplifications} *)

module T = Term
module S = Symbol
module F = Formula

(* check whether the formula is already in CNF *)
let rec is_cnf f = match f.F.form with
  | F.Or l -> List.for_all is_lit l
  | F.Not f' -> is_lit f'
  | F.True
  | F.False
  | F.Atom _
  | F.Equal _ -> true
  | F.And _ 
  | F.Equiv _
  | F.Imply _
  | F.Forall _
  | F.Exists _ -> false

and is_lit f = match f.F.form with
  | F.Not f' -> F.is_atomic f'
  | F.Equal _
  | F.Atom _
  | F.True
  | F.False -> true
  | F.Or _
  | F.And _
  | F.Equiv _
  | F.Imply _
  | F.Forall _
  | F.Exists _ -> false

(* miniscoping (push quantifiers as deep as possible in the formula) *)
let rec miniscope f =
  let f = F.simplify f in
  (* recursive miniscoping *)
  match f.F.form with
  | F.Forall (v, {F.form=F.And l}) ->
    (* forall x (and l) -> and (forall x f' \ f' in l) *)
    let l = List.map miniscope l in
    let with_v, without_v = List.partition (F.subterm v) l in
    F.mk_and (List.map (F.mk_forall v) with_v @ without_v)
  | F.Forall (v, {F.form=F.Or l}) ->
    let l = List.map miniscope l in
    let with_v, without_v = List.partition (F.subterm v) l in
    F.mk_and (F.mk_forall v (F.mk_or with_v) :: without_v)
  | F.Exists (v, {F.form=F.And l}) ->
    let l = List.map miniscope l in
    let with_v, without_v = List.partition (F.subterm v) l in
    F.mk_and (F.mk_exists v (F.mk_and with_v) :: without_v)
  | F.Exists (v, {F.form=F.Or l}) ->
    let l = List.map miniscope l in
    let with_v, without_v = List.partition (F.subterm v) l in
    F.mk_or (List.map (F.mk_exists v) with_v @ without_v)
  | F.And l -> F.mk_and (List.map miniscope l)
  | F.Or l -> F.mk_or (List.map miniscope l)
  | F.Imply (f1, f2) -> F.mk_imply (miniscope f1) (miniscope f2)
  | F.Equiv (f1, f2) -> F.mk_equiv (miniscope f1) (miniscope f2)
  | F.True
  | F.False
  | F.Equal _
  | F.Atom _ -> f

(* negation normal form (also remove equivalence and implications) *) 
let rec nnf f = match f.F.form with
  | F.Atom _
  | F.Equal _ -> f
  | F.And l -> F.mk_and (List.map nnf l)
  | F.Or l -> F.mk_or (List.map nnf l)
  | F.Not {F.form=F.Imply(f1,f2)} ->
    F.mk_and [ nnf f1; nnf (F.mk_not f2) ]
  | F.Imply (f1, f2) ->
    F.mk_or [ F.mk_not (F.mk_not f1); nnf f2 ]
  | F.Not {F.form=F.Equiv(f1,f2)} ->
    F.mk_or
      [ F.mk_and [ nnf (F.mk_not f1); nnf (F.mk_not f2) ]
      ; F.mk_and [ nnf f1; nnf f2 ]
      ]
  | F.Equiv (f1, f2) ->
    F.mk_and
      [ nnf (mk_imply f1 f2)
      ; nnf (mk_imply f2 f1)
      ]
  | F.Not {F.form=F.Forall(v, f')} ->
    F.mk_exists v (nnf (F.mk_not f'))
  | F.Forall (v, f') -> F.mk_forall v (nnf f')
  | F.Not {F.form=F.Exists(v, f')} ->
    F.mk_forall v (nnf (F.mk_not f'))
  | F.Exists (v, f') -> F.mk_exists v (nnf f')
  | F.Not f' -> F.mk_not (nnf f')

let skolemize ~ctx f =
  let rec skolemize f = match f.F.form with
  | F.And l -> F.mk_and (List.map skolemize l)
  | F.Or l -> F.mk_or (List.map skolemize l)
  | F.Not f' -> F.mk_not (skolemize f')
  | F.

(* skolemization of existentials, removal of forall *)
let rec skolemize ~ctx t = match t.T.term with
  | T.Var _
  | T.Node (_, [])
  | T.BoundVar _ -> t
  | T.Node (s, [{T.term=T.Node (s', [t])}])
    when s == S.not_symbol && s' == S.not_symbol ->
    skolemize ~ctx t (* double negation *)
  | T.Bind (s, t') when s == S.forall_symbol ->
    (* a fresh variable *)
    let ty = T.db_type t' 0 in
    Skolem.update_var ~ctx t';
    let v = T.mk_var ?ty (Skolem.fresh_var ~ctx) in
    let new_t' = T.db_unlift (T.db_replace t' v) in
    skolemize ~ctx new_t' (* remove forall *)
  | T.Bind (s, t') when s == S.exists_symbol ->
    (* make a skolem symbol *)
    let new_t' = Skolem.skolem_term ~ctx t' in
    skolemize ~ctx new_t' (* remove forall *)
  | T.Bind (s, t') -> T.mk_bind s (skolemize ~ctx t')
  | T.Node (s, l) -> T.mk_node s (List.map (skolemize ~ctx) l)
  | T.At (t1, t2) -> T.mk_at (skolemize ~ctx t1) (skolemize ~ctx t2)

(* helper: reduction to cnf using De Morgan laws. Returns a list of list of terms *)
let rec to_cnf t =
  match t.T.term with
  | T.Var _
  | T.Node (_, [])
  | T.BoundVar _ -> [[t]]
  | T.Node (s, [t']) when S.eq s S.not_symbol ->
    assert (T.atomic_rec t' ||
            match t'.T.term with T.Node (s', _) when S.eq s' S.eq_symbol -> true | _ -> false);
    [[T.mk_not t']]
  | T.Node (s, [a; b]) when s == S.and_symbol ->
    let ca = to_cnf a
    and cb = to_cnf b in
    List.rev_append ca cb
  | T.Node (s, [a; b]) when s == S.or_symbol ->
    product (to_cnf a) (to_cnf b)
  | T.Node _
  | T.At _
  | T.Bind _ -> [[t]]
(* cartesian product of lists of lists *)
and product a b =
  List.fold_left
    (fun acc litsa -> List.fold_left
      (fun acc' litsb -> (litsa @ litsb) :: acc')
      acc b)
    [] a

type clause = Term.t list
  (** Basic clause representation, as list of literals *)

(* Transform the clause into proper CNF; returns a list of clauses *)
let cnf_of ?(ctx=Skolem.create ()) t =
  if is_cnf t
    then
      let c = T.flatten_ac Symbol.or_symbol [t] in
      [c]
    else (
      let t = simplify t in
      let t = nnf t in
      let t = miniscope t in
      Skolem.update_var ~ctx t;
      let t = skolemize ~ctx t in
      let clauses = to_cnf t in
      clauses )

let cnf_of_list ?(ctx=Skolem.create ()) l =
  Util.list_flatmap (fun t -> cnf_of ~ctx t) l
