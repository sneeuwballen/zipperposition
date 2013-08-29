
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

(** {1 First-order Formulas} *)

module T = Term
module S = Symbol

type t = {
  form : cell;
  mutable id : int;
}
and cell =
  | True
  | False
  | Atom of Term.t
  | And of t list
  | Or of t list
  | Not of t
  | Imply of t * t
  | Equiv of t * t
  | Equal of Term.t * Term.t
  | Forall of Term.t * t    (** Quantified variable, plus formula *)
  | Exists of Term.t * t

type form = t

let compare f1 f2 = f1.id - f2.id
let eq f1 f2 = f1.id = f2.id
let hash f = f.id

let compare_rec f1 f2 = Pervasives.compare f1 f2
let hash_rec f = match f.form with
  | True -> 13
  | False -> 14
  | Atom t -> T.hash t
  | And l -> Hash.hash_list hash 17 l
  | Or l -> Hash.hash_list hash 19 l
  | Not f' -> Hash.hash_int2 (hash f') 23
  | Imply (f1,f2) -> Hash.hash_int2 (hash f1) (hash f2)
  | Equiv (f1,f2) -> Hash.hash_int3 (hash f1) (hash f2) 11
  | Equal (t1,t2) -> Hash.hash_int3 (T.hash t1) (T.hash t2) 13
  | Forall (v, f') -> Hash.hash_int2 (T.hash v) (hash f')
  | Exists (v, f') -> Hash.hash_int3 (T.hash v) (hash f') 11

module H = Hashcons.Make(struct
  type t = form
  let equal f1 f2 = compare_rec f1 f2 = 0
  let hash f = hash_rec f
  let tag id f = f.id <- id
end)

let mk_true =
  H.hashcons { form=True; id= ~-1; }

let mk_false = 
  H.hashcons { form=False; id= ~-1; }

let mk_atom p =
  H.hashcons { form=Atom p; id= ~-1; }

let mk_not f =
  match f.form with
  | Not f' -> f'  (* double negation *)
  | _ -> H.hashcons { form=Not f; id= ~-1; }

let mk_and = function
  | [] -> mk_true
  | l when List.memq mk_false l -> mk_false
  | [f] -> f
  | l -> 
    H.hashcons { form=And l; id= ~-1; }

let mk_or = function
  | [] -> mk_false
  | l when List.memq mk_true l -> mk_true
  | [f] -> f
  | l -> 
    H.hashcons { form=Or l; id= ~-1; }

let mk_imply f1 f2 =
  match f1.form, f2.form with
  | True, _ -> f2
  | False, _ -> mk_true
  | _, True -> mk_not f1
  | _, False -> f1
  | _ -> 
    H.hashcons { form=Imply (f1,f2); id= ~-1; }

let mk_equiv f1 f2 =
  match f1.form, f2.form with
  | _ when eq f1 f2 -> mk_true
  | True, _ -> f2
  | _, True -> f1
  | False, _ -> mk_not f2
  | _, False -> mk_not f1
  | _ -> 
    H.hashcons { form=Imply (f1,f2); id= ~-1; }

let mk_xor f1 f2 = mk_not (mk_equiv f1 f2)

let mk_eq t1 t2 =
  if T.eq t1 t2
    then mk_true
    else H.hashcons { form=Equal(t1,t2); id= ~-1; }

let mk_neq t1 t2 = mk_not (mk_eq t1 t2)

let mk_forall v f =
  H.hashcons { form=Forall(v,f); id= ~-1; }

let mk_exists v f =
  H.hashcons { form=Exists(v,f); id= ~-1; }

let rec add_terms set f = match f.form with
  | And l
  | Or l -> List.iter (add_terms set) l
  | Imply (f1, f2)
  | Equiv (f1, f2) -> add_terms set f1; add_terms set f2
  | Not f' -> add_terms set f'
  | True | False -> ()
  | Forall (_, f')
  | Exists (_, f') -> add_terms set f'
  | Atom p -> T.THashSet.add set p
  | Equal (t1, t2) -> T.THashSet.add set t1; T.THashSet.add set t2

let terms f =
  let set = T.THashSet.create () in
  add_terms set f;
  set

let terms_seq f =
  let set = terms f in
  Sequence.from_iter (T.THashSet.iter set)

let bound_variables f =
  let rec recurse acc f = match f.form with
  | Atom _
  | Equal _
  | True
  | False -> acc
  | And l
  | Or l -> List.fold_left recurse acc l
  | Imply (f1, f2)
  | Equiv (f1, f2) ->
    recurse (recurse acc f1) f2
  | Not f' -> recurse acc f'
  | Forall (v, f')
  | Exists (v, f') ->
    let acc = if List.memq v acc then acc else v :: acc in
    recurse acc f'
  in
  recurse [] f

let is_atomic f = match f.form with
  | And _
  | Or _
  | Imply _
  | Equiv _
  | Not _
  | Forall _
  | Exists _ -> false
  | True
  | False
  | Atom _
  | Equal _ -> true

let rec is_ground f = match f.form with
  | And l
  | Or l -> List.for_all is_ground l
  | Imply (f1, f2)
  | Equiv (f1, f2) -> is_ground f1 && is_ground f2
  | Not f' -> is_ground f'
  | Forall _
  | Exists _ -> false
  | Atom p -> T.is_ground p
  | Equal (t1, t2) -> T.is_ground t1 && T.is_ground t2
  | True
  | False -> true

let is_closed f =
  let vars = T.vars_seq (terms_seq f) in
  let bvars = bound_variables f in
  (* all variables are bound? *)
  List.for_all (fun v -> List.memq v bvars) vars

let rec _gather_or f = match f.form with
  | Or l -> Util.list_flatmap _gather_or l
  | False -> []
  | _ -> [f]

let rec _gather_and f = match f.form with
  | And l -> Util.list_flatmap _gather_and l
  | True -> []
  | _ -> [f]

let rec flatten f = match f.form with
  | Or l ->
    let l' = _gather_or f in
    let l' = List.map flatten l' in
    mk_or l'
  | And l ->
    let l' = _gather_and f in
    let l' = List.map flatten l' in
    mk_and l'
  | Imply (f1, f2) -> mk_imply (flatten f1) (flatten f2)
  | Equiv (f1, f2) -> mk_equiv (flatten f1) (flatten f2)
  | Not f' -> mk_not (flatten f')
  | Forall (v, f') -> mk_forall v (flatten f')
  | Exists (v, f') -> mk_exists v (flatten f')
  | True
  | False
  | Atom _
  | Equal _ -> f

(* does the var [v] occur in some term of [f]? *)
let _var_occurs v f =
  let vars = T.vars_seq (terms_seq f) in
  List.memq v vars

let rec simplify f = match f.form with
  | And l ->
    let l' = List.map simplify l in
    flatten (mk_and l')
  | Or l ->
    let l' = List.map simplify l in
    flatten (mk_or l')
  | Forall (v, f')
  | Exists (v, f') when not (_var_occurs v f') -> simplify f'
  | Forall (v, f') -> mk_forall v (simplify f')
  | Exists (v, f') -> mk_exists v (simplify f')
  | Equal (a,b) when T.eq a b -> mk_true
  | Equal _ -> f
  | Atom _
  | True
  | False -> f
  | Not f' -> mk_not (simplify f')
  | Imply ({form=True}, f') -> simplify f'
  | Imply ({form=False}, _) -> mk_true
  | Imply (_, {form=True}) -> mk_true
  | Imply (f', {form=False}) -> mk_not (simplify f')
  | Imply (f1, f2) -> mk_imply (simplify f1) (simplify f2)
  | Equiv (f1, f2) when eq f1 f2 -> mk_true
  | Equiv ({form=True}, f')
  | Equiv (f', {form=True}) -> simplify f'
  | Equiv ({form=False}, f')
  | Equiv (f', {form=False}) -> mk_not (simplify f')
  | Equiv (f1, f2) -> mk_equiv (simplify f1) (simplify f2)

let rec to_term f = match f.form with
  | True -> T.true_term
  | False -> T.false_term
  | And l -> T.mk_and_list (List.map to_term l)
  | Or l -> T.mk_or_list (List.map to_term l)
  | Equiv (f1, f2) -> T.mk_equiv (to_term f1) (to_term f2)
  | Imply (f1, f2) -> T.mk_imply (to_term f1) (to_term f2)
  | Equal (t1, t2) -> T.mk_eq t1 t2
  | Not f' -> T.mk_not (to_term f')
  | Forall (v, f') -> T.mk_forall_var [v] (to_term f')
  | Exists (v, f') -> T.mk_exists_var [v] (to_term f')
  | Atom p -> p

let of_term t =
  let rec recurse t = match t.T.term with
  | _ when t == T.true_term -> mk_true
  | _ when t == T.false_term -> mk_false
  | T.Node (s, l) when S.eq s S.and_symbol -> mk_and (List.map recurse l)
  | T.Node (s, l) when S.eq s S.or_symbol -> mk_or (List.map recurse l)
  | T.Node (s, [a;b]) when S.eq s S.equiv_symbol ->
    mk_equiv (recurse a) (recurse b)
  | T.Node (s, [a;b]) when S.eq s S.imply_symbol ->
    mk_imply (recurse a) (recurse b)
  | T.Node (s, [a]) when S.eq s S.not_symbol ->
    mk_not (recurse a)
  | T.Node (s, [a;b]) when S.eq s S.eq_symbol ->
    mk_eq a b
  | T.Node (s, [v;f']) when S.eq s S.forall_symbol ->
    mk_forall v (recurse f')
  | T.Node (s, [v;f']) when S.eq s S.exists_symbol ->
    mk_exists v (recurse f')
  | _ -> mk_atom t  (* default: atom *)
  in
  let varindex = ref (T.max_var (T.vars t) + 1) in
  recurse (T.db_to_classic ~varindex t)

(** {2 IO} *)

let rec pp buf f =
  assert false

let to_string f =
  Util.on_buffer pp f

let fmt fmt f =
  Format.pp_print_string fmt (to_string f)

let rec pp_tstp buf f =
  assert false

(* KISS: use the term bijection *)
let bij =
  Bij.(map
    ~inject:to_term
    ~extract:of_term
    T.bij)
