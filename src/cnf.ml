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

(** check whether the clause is already in CNF *)
let rec is_cnf t = match t.T.term with
  | T.Node (s, [a;b]) when S.eq s S.or_symbol ->
    is_cnf a && is_cnf b
  | T.Node (s, [t']) when S.eq s S.not_symbol ->
    is_lit t'
  | T.Node (s, [a;b]) when S.eq s S.eq_symbol ->
    T.atomic a && T.atomic b
  | T.Node (s, _) when S.eq s S.and_symbol || S.eq s S.equiv_symbol
      || S.eq s S.imply_symbol -> false
  | T.Node _ -> true
  | T.Bind (_, _) -> false
  | T.BoundVar _ -> false
  | T.Var _ -> true
  | T.At (t1, t2) -> true
and is_lit t = match t.T.term with
  | T.Node (s, [a;b]) when S.eq s S.eq_symbol -> T.atomic a && T.atomic b
  | T.Node (s, _) when S.eq s S.imply_symbol || S.eq s S.and_symbol
    || S.eq s S.or_symbol || S.eq s S.equiv_symbol -> false
  | T.Node (s, [t']) when S.eq s S.not_symbol -> T.atomic t'
  | T.Node (_, _) -> true
  | T.Var _ -> true
  | T.At _ -> true
  | T.BoundVar _ -> false
  | T.Bind (_, _) -> false

let mark_simplified t = T.set_flag T.flag_simplified t true

(** Simplify a boolean term (a formula) *)
let rec simplify t =
  if T.get_flag T.flag_simplified t
    then t
    else match t.T.term with
    | T.Var _
    | T.Node (_, [])
    | T.BoundVar _ -> mark_simplified t; t
    | T.At (t1, t2) ->
      let new_t = T.mk_at (simplify t1) (simplify t2) in
      mark_simplified new_t;
      new_t
    | T.Bind (f, t') when not (T.db_contains t' 0) ->
      simplify (T.db_unlift t')  (* eta-reduction: binder binds nothing, remove it *)
    | T.Bind (f, t') ->
      T.mk_bind f (simplify t')
    | T.Node (s, [{T.term=T.Bind (s', t')}]) when s == S.not_symbol && s' == S.forall_symbol ->
      simplify (T.mk_exists (T.mk_not t'))  (* not forall t -> exists (not t) *)
    | T.Node (s, [{T.term=T.Bind (s', t')}]) when s == S.not_symbol && s' == S.exists_symbol ->
      simplify (T.mk_forall (T.mk_not t'))  (* not exists t -> forall (not t) *)
    | T.Node (s, [{T.term=T.Node (s', [t'])}]) when s == S.not_symbol && s' == S.not_symbol ->
      simplify t'  (* double negation *)
    | T.Node (s, [t']) when s == S.not_symbol && t' == T.true_term ->
      T.false_term  (* not true -> false *)
    | T.Node (s, [t']) when s == S.not_symbol && t' == T.false_term ->
      T.true_term   (* not false -> true *)
    | T.Node (s, [a; b]) when s == S.and_symbol && (a == T.false_term || b == T.false_term) ->
      T.false_term  (* a and false -> false *)
    | T.Node (s, [a; b]) when s == S.or_symbol && (a == T.true_term || b == T.true_term) ->
      T.true_term  (* a or true -> true *)
    | T.Node (s, [a; b]) when s == S.or_symbol && a == T.false_term ->
      simplify b  (* b or false -> b *)
    | T.Node (s, [a; b]) when s == S.or_symbol && b == T.false_term ->
      simplify a  (* a or false -> a *)
    | T.Node (s, [a; b]) when s == S.and_symbol && a == T.true_term ->
      simplify b  (* b and true -> b *)
    | T.Node (s, [a; b]) when s == S.and_symbol && b == T.true_term ->
      simplify a  (* a and true -> a *)
    | T.Node (s, [a; b]) when s == S.imply_symbol && (a == T.false_term || b == T.true_term) ->
      T.true_term  (* (false => a) or (a => true) -> true *)
    | T.Node (s, [a; b]) when s == S.imply_symbol && a == T.true_term ->
      simplify b  (* (true => a) -> a *)
    | T.Node (s, [a; b]) when (S.eq s S.eq_symbol || S.eq s S.equiv_symbol) && a == b ->
      T.true_term  (* a = a -> true *)
    | T.Node (s, [a; b]) when (S.eq s S.eq_symbol || S.eq s S.equiv_symbol) && 
      ((a == T.true_term && b == T.false_term) ||
       (b == T.true_term && a == T.false_term)) ->
      T.false_term (* true = false -> false *)
    | T.Node (s, [a; b]) when S.eq s S.equiv_symbol && b == T.true_term ->
      simplify a  (* a <=> true -> a *)
    | T.Node (s, [a; b]) when S.eq s S.equiv_symbol && a == T.true_term ->
      simplify b  (* b <=> true -> b *)
    | T.Node (s, [a; b]) when S.eq s S.equiv_symbol && b == T.false_term ->
      simplify (T.mk_not a)  (* a <=> false -> not a *)
    | T.Node (s, [a; b]) when S.eq s S.equiv_symbol && a == T.false_term ->
      simplify (T.mk_not b)  (* b <=> false -> not b *)
    | T.Node (s, l) ->
      let l' = List.map simplify l in
      if List.for_all2 (==) l l'
        then (mark_simplified t; t)
        else 
          let new_t = T.mk_node s l' in
          simplify new_t

(* Apply miniscoping (push quantifiers as deep as possible in the formula) to the term *)
let rec miniscope t =
  (* build a n-ary and/or *)
  let rec mk_n_ary s l = match l with
  | [] -> assert false
  | x::[] -> x
  | x::y::l' ->  (* pop x, y from stack *)
    let t = T.mk_node s [x;y] in
    mk_n_ary s (t::l')  (* push back (x op y) on stack *)
  in
  (* simplify the term *)
  let t = simplify t in
  (* recursive miniscoping *)
  match t.T.term with
  | T.Bind (s, {T.term=T.Node (s', l)})
    when (s == S.forall_symbol || s == S.exists_symbol)
    && (s' == S.and_symbol || s' == S.or_symbol) ->
    (* Q x. a and/or b -> (Q x. a) and/or b  if x \not\in vars(b) *)
    let a, b = List.partition (fun f -> T.db_contains f 0) l in
    assert (a <> []);  (* eta-reduction should have worked! *)
    if b <> []
      then
        (* distribute forall over and, or exists over or; otherwise keep it outside *)
        let a =
          if ((s == S.forall_symbol && s' == S.and_symbol)
            || (s == S.exists_symbol && s' == S.or_symbol))
          then mk_n_ary s' (List.map (fun t -> miniscope (T.mk_bind s t)) a)
          else T.mk_bind s (mk_n_ary s' a)
        in
        (* some subformulas do not contain x, put them outside of quantifier *)
        let b = mk_n_ary s' (List.map (fun t -> miniscope (T.db_unlift t)) b) in
        simplify (T.mk_node s' [a; b])
      else t
  | T.Bind (_, _) -> t
  | T.BoundVar _
  | T.Var _
  | T.At _
  | T.Node _ -> t

(** negation normal form (also remove equivalence and implications) *) 
let rec nnf t =
  match t.T.term with
  | T.Var _
  | T.Node (_, [])
  | T.BoundVar _ -> t
  | T.Bind (f, t') -> T.mk_bind f (nnf t')
  | T.Node (s, [{T.term=T.Node (s', [a; b])}])
    when s == S.not_symbol && s' == S.and_symbol ->
    nnf (T.mk_or (T.mk_not a) (T.mk_not b))  (* de morgan *)
  | T.Node (s, [{T.term=T.Node (s', [a; b])}])
    when s == S.not_symbol && s' == S.or_symbol ->
    nnf (T.mk_and (T.mk_not a) (T.mk_not b)) (* de morgan *)
  | T.Node (s, [a; b]) when s == S.imply_symbol ->
    nnf (T.mk_or (T.mk_not a) b) (* (a => b) -> (not a or b) *)
  | T.Node (s, [a; b]) when s == S.equiv_symbol ->
    (* (a <=> b) -> (not a or b) and (not b or a) *)
    nnf (T.mk_and
      (T.mk_or (T.mk_not a) b)
      (T.mk_or (T.mk_not b) a))
  | T.Node (s, [{T.term=T.Node (s', [a; b])}])
    when s == S.not_symbol && s' == S.imply_symbol ->
    nnf (T.mk_and a (T.mk_not b)) (* not (a => b) -> (a and not b) *)
  | T.Node (s, [{T.term=T.Node (s', [a; b])}])
    when s == S.not_symbol && s' == S.equiv_symbol ->
    (* not (a <=> b) -> (a <=> (not b)) *)
    nnf (T.mk_or
      (T.mk_and a (T.mk_not b))
      (T.mk_and b (T.mk_not a)))
  | T.Node (s, [{T.term=T.Bind (s', t')}]) when s == S.not_symbol && s' == S.forall_symbol ->
    nnf (T.mk_exists (T.mk_not t')) (* not forall -> exists not *)
  | T.Node (s, [{T.term=T.Bind (s', t')}])
    when s == S.not_symbol && s' == S.exists_symbol ->
    nnf (T.mk_forall (T.mk_not t')) (* not exists -> forall not *)
  | T.Node (s, [{T.term=T.Node (s', [t])}])
    when s == S.not_symbol && s' == S.not_symbol ->
    nnf t (* double negation *)
  | T.Node (s, l) ->
    let t' = T.mk_node s (List.map nnf l) in
    if t == t' then t' else nnf t'
  | T.At (t1, t2) -> t

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
