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

module T = FOTerm
module ST = ScopedTerm
module S = Symbol
module F = Formula.FO

type form = Formula.FO.t

(* check whether the formula is already in CNF *)
let rec is_cnf f = match F.view f with
  | F.Or l -> List.for_all is_lit l
  | F.Not f' -> is_lit f
  | F.True
  | F.False
  | F.Atom _
  | F.Neq _
  | F.Eq _ -> true
  | F.And _
  | F.Equiv _
  | F.Xor _
  | F.Imply _
  | F.Forall _
  | F.Exists _ -> false

and is_lit f = match F.view f with
  | F.Not f' -> F.is_atomic f'
  | F.Eq _
  | F.Neq _
  | F.Atom _
  | F.True
  | F.False -> true
  | F.Or _
  | F.And _
  | F.Equiv _
  | F.Xor _
  | F.Imply _
  | F.Forall _
  | F.Exists _ -> false

let is_clause l = List.for_all is_lit l

let __reconvert t =
  match F.of_term t with
  | Some f -> f
  | None ->
      assert (T.is_term t);
      F.Base.atom (T.of_term_exn t)

let __unshift n f =
  __reconvert (ST.DB.unshift n (f : F.t :> ST.t))

let __contains_db n f =
  ST.DB.contains (f : F.t :> ST.t) n

(* miniscoping (push quantifiers as deep as possible in the formula) *)
let miniscope ?(distribute_exists=false) f =
  (* recursive miniscoping *)
  let rec miniscope f = match F.view f with
  | F.Forall (varty, f') ->
    begin match F.view f' with
    | F.And l ->
      (* forall x (and l) -> and (forall x f' \ f' in l) *)
      let l = List.map miniscope l in
      let with_v, without_v = List.partition (__contains_db 0) l in
      F.Base.and_
        ( List.map (F.Base.__mk_forall ~varty) with_v
        @ List.map (__unshift 1) without_v)
    | F.Or l ->
      let l = List.map miniscope l in
      let with_v, without_v = List.partition (__contains_db 0) l in
      F.Base.or_
        (  F.Base.__mk_forall ~varty (F.Base.or_ with_v)
        :: List.map (__unshift 1) without_v)
    | _ -> F.Base.__mk_forall ~varty (miniscope f')
    end
  | F.Exists (varty, f') ->
    begin match F.view f' with
    | F.And l ->
      let l = List.map miniscope l in
      let with_v, without_v = List.partition (fun f -> __contains_db 0 f) l in
      F.Base.and_
        (  F.Base.__mk_exists ~varty (F.Base.and_ with_v)
        :: List.map (__unshift 1) without_v)
    | F.Or l ->
      let l = List.map miniscope l in
      let with_v, without_v = List.partition (fun f -> __contains_db 0 f) l in
      (* see whether we push the existential into the formulas [with_v], in
          which the variable occurs, or whether we keep it outside *)
      let with_v = if distribute_exists
        then List.map (F.Base.__mk_exists ~varty) with_v
        else [F.Base.__mk_exists ~varty (F.Base.or_ with_v)]
      in
      let without_v = List.map (__unshift 1) without_v in
      F.Base.or_ (with_v @ without_v)
    | _ -> F.Base.__mk_exists ~varty (miniscope f')
    end
  | F.And l -> F.Base.and_ (List.map miniscope l)
  | F.Or l -> F.Base.or_ (List.map miniscope l)
  | F.Imply (f1, f2) -> F.Base.imply (miniscope f1) (miniscope f2)
  | F.Equiv (f1, f2) -> F.Base.equiv (miniscope f1) (miniscope f2)
  | F.Xor (f1, f2) -> F.Base.xor (miniscope f1) (miniscope f2)
  | F.Not f' -> F.Base.not_ (miniscope f')
  | F.True
  | F.False
  | F.Neq _
  | F.Eq _
  | F.Atom _ -> f
  in
  miniscope (F.simplify f)

(* what's the surrounding connective? *)
type surrounding =
  | SurroundOr
  | SurroundAnd

(* TODO: actually the handbook still uses a notion of "polarity"
   to choose how to encode equivalence (and xor). See whether that
   would lead to smaller CNF. *)

(* negation normal form (also remove equivalence and implications).
   [surrounding] is either [Or] or [And], and denotes which connective
   is immediately surrouding the current formula. This information is
   used to choose among the possible destructions of equiv/xor, to
   avoid nesting opposite connectives *)
let rec nnf surrounding f =
  Util.debug 5 "nnf of %a..." F.pp f;
  match F.view f with
  | F.Atom _
  | F.Neq _
  | F.Eq _ -> f
  | F.Not f' ->
      begin match F.view f' with
      | F.Not f'' -> nnf surrounding f''
      | F.Neq (a,b) -> F.Base.eq a b
      | F.Eq (a,b) -> F.Base.neq a b
      | F.And l ->
        F.Base.or_ (List.map (fun f -> nnf SurroundOr (F.Base.not_ f)) l)
      | F.Or l ->
        F.Base.and_ (List.map (fun f -> nnf SurroundAnd (F.Base.not_ f)) l)
      | F.Xor (a,b) ->
        nnf surrounding (F.Base.equiv a b)
      | F.Equiv (a,b) ->
        nnf surrounding (F.Base.xor a b)
      | F.Imply (a,b) -> (* not (a=>b)  is a and not b *)
        nnf surrounding (F.Base.and_ [a; F.Base.not_ b])
      | F.Forall (varty, f'') ->
        F.Base.__mk_exists ~varty (nnf surrounding (F.Base.not_ f''))
      | F.Exists (varty, f'') ->
        F.Base.__mk_forall ~varty (nnf surrounding (F.Base.not_ f''))
      | F.True -> F.Base.false_
      | F.False -> F.Base.true_
      | F.Atom _ -> f
      end
  | F.And l -> F.Base.and_ (List.map (nnf SurroundAnd) l)
  | F.Or l -> F.Base.or_ (List.map (nnf SurroundOr) l)
  | F.Imply (f1, f2) ->
    nnf surrounding (F.Base.or_ [ (F.Base.not_ f1); f2 ])
  | F.Equiv(f1,f2) ->
    begin match surrounding with
      | SurroundAnd ->
        nnf surrounding (F.Base.and_
          [ F.Base.imply f1 f2; F.Base.imply f2 f1 ])
      | SurroundOr ->
        nnf surrounding (F.Base.or_
          [ F.Base.and_ [f1; f2]; F.Base.and_ [F.Base.not_ f1; F.Base.not_ f2] ])
    end
  | F.Xor (f1,f2) ->
    begin match surrounding with
      | SurroundOr ->
        nnf surrounding (F.Base.or_
          [ F.Base.and_ [F.Base.not_ f1; f2]; F.Base.and_ [f1; F.Base.not_ f2] ])
      | SurroundAnd ->
        nnf surrounding (F.Base.and_
          [ F.Base.imply (F.Base.not_ f1) f2; F.Base.imply f2 (F.Base.not_ f1) ])
    end
  | F.Forall (varty,f') -> F.Base.__mk_forall ~varty (nnf surrounding f')
  | F.Exists (varty,f') -> F.Base.__mk_exists ~varty (nnf surrounding f')
  | F.True
  | F.False -> f

(* evaluate [f] in the given [env], and then unshift remaining free DB vars *)
let __eval_and_unshift env f =
  __reconvert (ST.DB.unshift 1 (ST.DB.eval env (f : F.t :> ST.t)))

let skolemize ~ctx f =
  let rec skolemize f = match F.view f with
  | F.And l -> F.Base.and_ (List.map skolemize l)
  | F.Or l -> F.Base.or_ (List.map skolemize l)
  | F.Not f' -> F.Base.not_ (skolemize f')
  | F.Xor _
  | F.Imply _
  | F.Equiv _ -> failwith "can only skolemize a NNF formula"
  | F.Atom _
  | F.Eq _
  | F.Neq _
  | F.True
  | F.False -> f
  | F.Exists (ty,f') ->
    (* replace [v] by a fresh skolem term *)
    let new_f' = Skolem.skolem_form ~ctx ~ty f' in
    skolemize new_f'
  | F.Forall (ty,f') ->
    (* remove quantifier, replace by fresh variable *)
    F.iter (Skolem.update_var ~ctx) f';
    Util.debug 5 "type of variable: %a" Type.pp ty;
    let v = T.var ~ty (Skolem.fresh_var ~ctx) in
    let env = DBEnv.singleton (v:T.t:>ST.t) in
    let new_f' = __eval_and_unshift env f' in
    skolemize new_f'
  in
  skolemize f

(* helper: reduction to cnf using De Morgan laws. Returns a list of list of
  atomic formulas *)
let rec to_cnf f = match F.view f with
  | F.Eq _
  | F.Neq _
  | F.Atom _
  | F.True
  | F.False -> [[f]]
  | F.Not f' ->
      begin match F.view f' with
      | F.Eq (a,b) -> [[F.Base.neq a b]]
      | F.Neq (a,b) -> [[F.Base.eq a b]]
      | _ when F.is_atomic f' ->[[f]]
      | _ -> failwith (Util.sprintf "should be atomic: %a" F.pp f')
      end
  | F.And l ->
    (* simply concat sub-CNF *)
    Util.list_flatmap to_cnf l
  | F.Or (f'::l) ->
    (* cartesian products of sub-CNF *)
    List.fold_left
      (fun cnf f' -> product (to_cnf f') cnf)
      (to_cnf f')
      l
  | F.Forall _
  | F.Exists _ -> failwith "Cnf.to_cnf: can only clausify a skolemized formula"
  | F.Xor _
  | F.Imply _
  | F.Equiv _ -> failwith "Cnf.to_cnf: can only clausify a NNF formula"
  | F.Or [] -> assert false
(* cartesian product of lists of lists *)
and product a b =
  List.fold_left
    (fun acc litsa -> List.fold_left
      (fun acc' litsb -> (litsa @ litsb) :: acc')
      acc b)
    [] a

type clause = F.t list
  (** Basic clause representation, as list of literals *)

type options =
  | DistributeExists

(* Transform the clause into proper CNF; returns a list of clauses *)
let cnf_of ?(opts=[]) ?(ctx=Skolem.create Signature.empty) f =
  let f = F.flatten f in
  Util.debug 4 "reduce %a to CNF..." F.pp f;
  let clauses = if is_cnf f
    then
      match F.view f with
      | F.Or l -> [l]  (* works because [f] flattened before *)
      | F.False
      | F.True
      | F.Eq _
      | F.Neq _
      | F.Atom _ -> [[f]]
      | F.Not f'  ->
        begin match F.view f' with
        | F.Eq (a,b) -> [[F.Base.neq a b]]
        | F.Neq (a,b) -> [[F.Base.eq a b]]
        | _ when F.is_atomic f' ->[[f]]
        | _ -> failwith (Util.sprintf "should be atomic: %a" F.pp f')
        end
      | F.Equiv _
      | F.Imply _
      | F.Xor _
      | F.And _
      | F.Forall _
      | F.Exists _ -> assert false
    else begin
      let f = F.simplify f in
      Util.debug 4 "... simplified: %a" F.pp f;
      let f = nnf SurroundAnd f in
      Util.debug 4 "... NNF: %a" F.pp f;
      let distribute_exists = List.mem DistributeExists opts in
      let f = miniscope ~distribute_exists f in
      Util.debug 4 "... miniscoped: %a" F.pp f;
      (* adjust the variable counter to [f] before skolemizing *)
      Skolem.clear_var ~ctx;
      F.iter (Skolem.update_var ~ctx) f;
      let f = skolemize ~ctx f in
      Util.debug 4 "... skolemized: %a" F.pp f;
      let clauses = to_cnf f in
      clauses
    end
  in
  assert (List.for_all is_clause clauses);
  clauses

let cnf_of_list ?(opts=[]) ?(ctx=Skolem.create Signature.empty) l =
  Util.list_flatmap (fun f -> cnf_of ~opts ~ctx f) l
