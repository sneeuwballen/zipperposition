
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

module T = FOTerm
module S = Symbol

type term = FOTerm.t

type t = {
  form : cell;
  mutable flags : int;
  mutable id : int;
}
and cell =
  | True
  | False
  | Atom of term
  | And of t list
  | Or of t list
  | Not of t
  | Imply of t * t
  | Equiv of t * t
  | Equal of term * term
  | Forall of Type.t * t  (** Quantified formula, with De Bruijn index *)
  | Exists of Type.t * t

type sourced_form = t * string * string    (* form, filename, axiom name *)

type form = t

let compare f1 f2 = f1.id - f2.id
let eq f1 f2 = f1.id = f2.id
let hash f = f.id

let eq_rec f1 f2 = match f1.form, f2.form with
  | True, True
  | False, False -> true
  | Atom p1, Atom p2 -> T.eq p1 p2
  | Equal (t11,t12), Equal(t21,t22) ->
    (T.eq t11 t21 && T.eq t12 t22) ||
    (T.eq t11 t22 && T.eq t12 t21)
  | And l1, And l2 when List.length l1 = List.length l2 ->
    List.for_all2 (==) l1 l2
  | Or l1, Or l2 when List.length l1 = List.length l2 ->
    List.for_all2 (==) l1 l2
  | Imply (f11, f12), Imply (f21, f22) ->
    eq f11 f21 && eq f12 f22
  | Equiv (f11, f12), Equiv (f21, f22) ->
    (eq f11 f21 && eq f12 f22) ||
    (eq f11 f22 && eq f12 f21)
  | Not f1', Not f2' -> eq f1' f2'
  | Forall (ty1,f1'), Forall (ty2,f2')
  | Exists (ty1,f1'), Exists (ty2,f2') -> Type.eq ty1 ty2 && eq f1' f2'
  | _, _ -> false

let hash_rec f = match f.form with
  | True -> 13
  | False -> 14
  | Atom t -> T.hash t
  | And l -> Hash.hash_list hash 17 l
  | Or l -> Hash.hash_list hash 19 l
  | Not f' -> Hash.hash_int2 (hash f') 23
  | Imply (f1,f2) -> Hash.hash_int2 (hash f1) (hash f2)
  | Equiv (f1,f2) -> Hash.hash_int2 (hash f1 lxor hash f2) 11
  | Equal (t1,t2) -> Hash.hash_int2 (T.hash t1 lxor T.hash t2) 13
  | Forall (_,f') -> Hash.hash_int (hash f')
  | Exists (_,f') -> Hash.hash_int2 (hash f') 11

let rec hash_novar f = match f.form with
  | True -> 13
  | False -> 14
  | Atom t -> T.hash t
  | And l -> Hash.hash_list hash 17 l
  | Or l -> Hash.hash_list hash 19 l
  | Not f' -> Hash.hash_int2 (hash_novar f') 23
  | Imply (f1,f2) -> Hash.hash_int2 (hash_novar f1) (hash_novar f2)
  | Equiv (f1,f2) -> Hash.hash_int2 (hash_novar f1 lxor hash_novar f2) 11
  | Equal (t1,t2) -> Hash.hash_int2 (T.hash_novar t1 lxor T.hash_novar t2) 13
  | Forall (_,f') -> Hash.hash_int (hash_novar f')
  | Exists (_,f') -> Hash.hash_int2 (hash_novar f') 11

(** {2 Flags} *)

let __gen = Util.Flag.create ()
let new_flag () = Util.Flag.get_new __gen

let flag_simplified = new_flag ()
let flag_ground = new_flag ()

let set_flag f flag = f.flags <- f.flags lor flag
let has_flag f flag = (f.flags land flag) != 0

(** {2 Constructors} *)

module H = Hashcons.Make(struct
  type t = form
  let equal f1 f2 = eq_rec f1 f2
  let hash f = hash_rec f
  let tag id f = f.id <- id
end)

let mk_true =
  H.hashcons { form=True; flags=(flag_ground lor flag_simplified); id= ~-1; }

let mk_false =
  H.hashcons { form=False; flags=(flag_ground lor flag_simplified); id= ~-1; }

let mk_atom p = match p with
  | _ when T.eq p T.true_term -> mk_true
  | _ when T.eq p T.false_term -> mk_false
  | _ -> 
    if not (Type.eq p.T.ty Type.o)
      then failwith
        (Util.sprintf "Formula.mk_atom: expected boolean term, got %a" T.pp p);
    let flags =
      if T.is_ground p
        then flag_simplified lor flag_ground
        else flag_simplified
    in
    H.hashcons { form=Atom p; flags; id= ~-1; }

let mk_not f =
  match f.form with
  | True -> mk_false
  | False -> mk_true
  | Not f' -> f'  (* double negation *)
  | _ -> H.hashcons { form=Not f; flags=f.flags; id= ~-1; }

let mk_and = function
  | [] -> mk_true
  | l when List.memq mk_false l -> mk_false
  | [f] -> f
  | l -> H.hashcons { form=And l; flags=0; id= ~-1; }

let mk_or = function
  | [] -> mk_false
  | l when List.memq mk_true l -> mk_true
  | [f] -> f
  | l -> H.hashcons { form=Or l; flags=0; id= ~-1; }

let mk_imply f1 f2 =
  match f1.form, f2.form with
  | True, _ -> f2
  | False, _
  | _, True -> mk_true
  | _, False -> mk_not f1
  | _ ->
    let f = { form=Imply (f1,f2); flags=0; id= ~-1; } in
    let f' = H.hashcons f in
    (if f == f' then
      let is_ground = has_flag f1 flag_ground && has_flag f2 flag_ground in
      if is_ground then set_flag f flag_ground);
    f'

let mk_equiv f1 f2 =
  match f1.form, f2.form with
  | _ when eq f1 f2 -> mk_true
  | True, _ -> f2
  | _, True -> f1
  | False, _ -> mk_not f2
  | _, False -> mk_not f1
  | _ ->
    let f = { form=Equiv(f1,f2); flags=0; id= ~-1; } in
    let f' = H.hashcons f in
    (if f == f' then
      let is_ground = has_flag f1 flag_ground && has_flag f2 flag_ground in
      if is_ground then set_flag f flag_ground);
    f'

let mk_xor f1 f2 = mk_not (mk_equiv f1 f2)

let mk_eq t1 t2 =
  if T.eq t1 t2
    then mk_true
  else if not (Type.eq t1.T.ty t2.T.ty)
    then failwith "Formula.mk_eq: expected same type on both sides"
  else
    let f = { form=Equal(t1,t2); flags=flag_simplified; id= ~-1; } in
    let f' = H.hashcons f in
    let _ = if f == f' && T.is_ground t1 && T.is_ground t2
      then set_flag f flag_ground
    in
    f'

let mk_neq t1 t2 = mk_not (mk_eq t1 t2)

let mk_forall ~ty f =
  H.hashcons { form=Forall (ty,f); flags=0; id= ~-1; }

let mk_exists ~ty f =
  H.hashcons { form=Exists (ty,f); flags=0; id= ~-1; }

module FCache = Cache.Replacing(struct
  type t = form
  let equal = eq
  let hash = hash
end)

(** {2 Combinators} *)

let rec map_leaf f form = match form.form with
  | And l -> mk_and (List.map (map_leaf f) l)
  | Or l -> mk_or (List.map (map_leaf f) l)
  | Imply (f1, f2) -> mk_imply (map_leaf f f1) (map_leaf f f2)
  | Equiv (f1, f2) -> mk_equiv (map_leaf f f1) (map_leaf f f2)
  | Not f' -> mk_not (map_leaf f f')
  | True
  | False
  | Atom _
  | Equal _ -> f form  (* replace by image *)
  | Forall (ty,f') -> mk_forall ~ty (map_leaf f f')
  | Exists (ty,f') -> mk_exists ~ty (map_leaf f f')

let map f form =
  map_leaf
    (fun form -> match form.form with
      | True
      | False -> form
      | Atom p -> mk_atom (f p)
      | Equal (t1, t2) -> mk_eq (f t1) (f t2)
      | _ -> assert false)
    form

let rec fold f acc form = match form.form with
  | And l
  | Or l -> List.fold_left (fold f) acc l
  | Imply (f1, f2)
  | Equiv (f1, f2) -> fold f (fold f acc f1) f2
  | Not f' -> fold f acc f'
  | True
  | False -> acc
  | Atom p -> f acc p
  | Equal (t1, t2) -> f (f acc t1) t2
  | Forall (_,f')
  | Exists (_,f') -> fold f acc f'

let iter f form = fold (fun () t -> f t) () form

let rec map_depth ?(depth=0) f form = match form.form with
  | And l -> mk_and (List.map (map_depth ~depth f) l)
  | Or l -> mk_or (List.map (map_depth ~depth f) l)
  | Imply (f1, f2) -> mk_imply (map_depth ~depth f f1) (map_depth ~depth f f2)
  | Equiv (f1, f2) -> mk_equiv (map_depth ~depth f f1) (map_depth ~depth f f2)
  | Not f' -> mk_not (map_depth ~depth f f')
  | True
  | False -> form
  | Atom p -> mk_atom (f depth p)
  | Equal (t1, t2) -> mk_eq (f depth t1) (f depth t2)
  | Forall (ty,f') -> mk_forall ~ty (map_depth ~depth:(depth+1) f f')
  | Exists (ty,f') -> mk_exists ~ty (map_depth ~depth:(depth+1) f f')

let rec map_leaf_depth ?(depth=0) f form = match form.form with
  | And l -> mk_and (List.map (map_leaf_depth ~depth f) l)
  | Or l -> mk_or (List.map (map_leaf_depth ~depth f) l)
  | Imply (f1, f2) -> mk_imply (map_leaf_depth ~depth f f1) (map_leaf_depth ~depth f f2)
  | Equiv (f1, f2) -> mk_equiv (map_leaf_depth ~depth f f1) (map_leaf_depth ~depth f f2)
  | Not f' -> mk_not (map_leaf_depth ~depth f f')
  | True
  | False
  | Atom _
  | Equal _ -> f depth form  (* replace by image *)
  | Forall (ty,f') -> mk_forall ~ty (map_leaf_depth ~depth:(depth+1) f f')
  | Exists (ty,f') -> mk_exists ~ty (map_leaf_depth ~depth:(depth+1) f f')

let fold_depth ?(depth=0) f acc form =
  let rec recurse f acc depth form = match form.form with
  | And l
  | Or l -> List.fold_left (fun acc f' -> recurse f acc depth f') acc l
  | Imply (f1, f2)
  | Equiv (f1, f2) ->
    let acc = recurse f acc depth f1 in
    let acc = recurse f acc depth f2 in
    acc
  | Not f' -> recurse f acc depth f'
  | True
  | False -> acc
  | Atom p -> f acc depth p
  | Equal (t1, t2) ->
    let acc = f acc depth t1 in
    let acc = f acc depth t2 in
    acc
  | Forall (ty, f')
  | Exists (ty, f') ->
    recurse f acc (depth+1) f'
  in
  recurse f acc depth form

let weight f =
  let rec count n f = match f.form with
  | True
  | False -> n + 1
  | Not f'
  | Forall (_,f')
  | Exists (_,f') -> count (n+1) f'
  | Equal (t1, t2) -> n + T.size t1 + T.size t2
  | Atom p -> n + T.size p
  | Or l
  | And l -> List.fold_left count (n+1) l
  | Imply (f1, f2)
  | Equiv (f1, f2) ->
    let n = count (n+1) f1 in
    count n f2
  in
  count 0 f

let add_terms set f = iter (fun t -> T.Tbl.replace set t ()) f

let terms f =
  let set = T.Tbl.create 6 in
  add_terms set f;
  set

let terms_seq f =
  Sequence.from_iter (fun k -> iter k f)

let subterm t f =
  try
    iter (fun t' -> if T.subterm ~sub:t t' then raise Exit) f;
    false
  with Exit ->
    true

let var_occurs v f =
  assert (T.is_var v);
  subterm v f

let free_variables f =
  let set = T.Tbl.create 5 in
  iter (fun t -> T.add_vars set t) f;
  T.Tbl.fold (fun v () acc -> v :: acc) set []

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

let rec is_ground f =
  if has_flag f flag_ground
    then true
    else
      let res = match f.form with
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
      in
      let () = if res then set_flag f flag_ground in
      res

let is_closed f =
  match free_variables f with
  | [] -> true
  | _ -> false

let contains_symbol sy f =
  let terms = terms_seq f in
  Sequence.exists (T.contains_symbol sy) terms

let symbols ?(init=Symbol.Set.empty) f =
  T.symbols ~init (terms_seq f)

(** {2 De Bruijn indexes} *)

let db_closed f =
  try
    fold_depth
      (fun () depth t -> if not (T.db_closed ~depth t) then raise Exit)
      () f;
    true
  with Exit ->
    false

let db_contains f n =
  try
    fold_depth
      (fun () depth t -> if T.db_contains t (n+depth) then raise Exit)
      () f;
    false
  with Exit ->
    true

let db_replace f t =
  map_depth
    ~depth:0
    (fun depth t' -> T.db_replace ~depth ~into:t' ~by:(T.db_lift depth t))
    f

exception FoundType of Type.t

let db_lift f =
  map_depth (fun depth t -> T.db_lift ~depth 1 t) f

let db_unlift ?(depth=0) f =
  map_depth ~depth (fun depth t -> T.db_unlift ~depth t) f

let db_from_term f t =
  map_depth (fun depth t' -> T.db_from_term ~depth t' t) f

let db_from_var f v =
  db_from_term f v

let mk_forall_list vars f =
  List.fold_right
    (fun v f ->
      let ty = T.get_type v in
      mk_forall ~ty (db_from_var (db_lift f) v))
    vars f

let mk_exists_list vars f =
  List.fold_right
    (fun v f ->
      let ty = T.get_type v in
      mk_exists ~ty (db_from_var (db_lift f) v))
    vars f

let close_forall f =
  let fv = free_variables f in
  mk_forall_list fv f

let close_exists f =
  let fv = free_variables f in
  mk_exists_list fv f

let open_forall ?(offset=0) f =
  let offset = max offset (T.max_var (free_variables f) + 1) in
  let rec open_one offset f = match f.form with
  | Forall (ty,f') ->
    let v = T.mk_var ~ty offset in
    let new_f' = db_replace f' v in
    open_one (offset+1) new_f'
  | _ -> f
  in
  open_one offset f

let rec open_and f = match f.form with
  | And l -> Util.list_flatmap open_and l
  | True -> []
  | _ -> [f]

let rec open_or f = match f.form with
  | Or l -> Util.list_flatmap open_or l
  | False -> []
  | _ -> [f]

(** {2 Simplifications} *)

let rec flatten f =
  if has_flag f flag_simplified then f
  else match f.form with
  | Or l ->
    let l' = open_or f in
    let l' = List.map flatten l' in
    mk_or l'
  | And l ->
    let l' = open_and f in
    let l' = List.map flatten l' in
    mk_and l'
  | Imply (f1, f2) -> mk_imply (flatten f1) (flatten f2)
  | Equiv (f1, f2) -> mk_equiv (flatten f1) (flatten f2)
  | Not f' -> mk_not (flatten f')
  | Forall (ty,f') -> mk_forall ~ty (flatten f')
  | Exists (ty,f') -> mk_exists ~ty (flatten f')
  | True
  | False
  | Atom _
  | Equal _ -> f

(* does the var [v] occur in some term of [f]? *)
let _var_occurs v f =
  if is_ground f
    then false
    else
      Sequence.exists (T.var_occurs v) (terms_seq f)

let simplify f =
  let rec simplify ~depth f =
  if has_flag f flag_simplified then f
  else
    let f' = match f.form with
    | And l ->
      let l' = List.map (simplify ~depth) l in
      flatten (mk_and l')
    | Or l ->
      let l' = List.map (simplify ~depth) l in
      flatten (mk_or l')
    | Forall (_,f')
    | Exists (_,f') when not (db_contains f' 0) ->
      simplify ~depth (db_unlift ~depth f')
    | Forall (ty,f') -> mk_forall ~ty (simplify ~depth:(depth+1) f')
    | Exists (ty,f') -> mk_exists ~ty (simplify ~depth:(depth+1) f')
    | Equal (a,b) when T.eq a b -> mk_true
    | Equal _ -> f
    | Atom _
    | True
    | False -> f
    | Not f' -> mk_not (simplify ~depth f')
    | Imply ({form=True}, f') -> simplify ~depth f'
    | Imply ({form=False}, _) -> mk_true
    | Imply (_, {form=True}) -> mk_true
    | Imply (f', {form=False}) -> mk_not (simplify ~depth f')
    | Imply (f1, f2) -> mk_imply (simplify ~depth f1) (simplify ~depth f2)
    | Equiv (f1, f2) when eq f1 f2 -> mk_true
    | Equiv ({form=True}, f')
    | Equiv (f', {form=True}) -> simplify ~depth f'
    | Equiv ({form=False}, f')
    | Equiv (f', {form=False}) -> mk_not (simplify ~depth f')
    | Equiv (f1, f2) -> mk_equiv (simplify ~depth f1) (simplify ~depth f2)
    in
    let () = set_flag f' flag_simplified in
    f'
  in simplify ~depth:0 f

let rec is_trivial f = match f.form with
  | True -> true
  | Equal (l,r) -> T.eq l r 
  | False
  | Atom _ -> false
  | Imply ({form=False},_) -> true
  | Equiv (l,r) -> eq l r
  | Or l -> List.exists is_trivial l
  | And _
  | Imply _
  | Not _ -> false
  | Exists (_,f')
  | Forall (_,f') -> is_trivial f'

let ac_normal_form f =
  let rec recurse f = match f.form with
  | True
  | False
  | Atom _ -> f
  | Imply (f1, f2) -> mk_imply (recurse f1) (recurse f2)
  | Equiv (f1, f2) -> mk_equiv (recurse f1) (recurse f2)
  | Not f' -> mk_not (recurse f')
  | Equal (t1, t2) ->
    (* put bigger term first *)
    begin match T.compare t1 t2 with
    | n when n >= 0 -> f
    | _ -> mk_eq t2 t1
    end
  | And l ->
    let l' = List.map recurse l in
    let l' = List.sort compare l' in
    mk_and l'
  | Or l ->
    let l' = List.map recurse l in
    let l' = List.sort compare l' in
    mk_or l'
  | Forall (ty,f') -> mk_forall ~ty (recurse f')
  | Exists (ty,f') -> mk_exists ~ty (recurse f')
  in
  recurse (flatten f)

let ac_eq f1 f2 =
  let f1 = ac_normal_form f1 in
  let f2 = ac_normal_form f2 in
  eq f1 f2

(** {2 Conversion} *)

let __cache_f2t = FCache.create 513

let to_term f =
  let module T = HOTerm in
  FCache.with_cache_rec __cache_f2t
    (fun to_term f ->
    match f.form with
    | True -> T.true_term
    | False -> T.false_term
    | And l -> T.mk_and_list (List.map to_term l)
    | Or l -> T.mk_or_list (List.map to_term l)
    | Equiv (f1, f2) -> T.mk_equiv (to_term f1) (to_term f2)
    | Imply (f1, f2) -> T.mk_imply (to_term f1) (to_term f2)
    | Equal (t1, t2) -> T.mk_eq (T.curry t1) (T.curry t2)
    | Not f' -> T.mk_not (to_term f')
    | Forall (ty,f') -> T.mk_forall ~varty:ty (to_term f')
    | Exists (ty,f') -> T.mk_exists ~varty:ty (to_term f')
    | Atom p -> T.curry p)
    f

let of_term t =
  let module T = HOTerm in
  let rec recurse t = match t.T.term with
  | _ when t == T.true_term -> mk_true
  | _ when t == T.false_term -> mk_false
  | T.At ({T.term=T.Const s}, [{T.term=T.Lambda t'} as lam]) when S.eq s S.forall_symbol ->
    let ty = T.lambda_var_ty lam in
    mk_forall ~ty (recurse t')
  | T.At ({T.term=T.Const s}, [{T.term=T.Lambda t'} as lam]) when S.eq s S.exists_symbol ->
    let ty = T.lambda_var_ty lam in
    mk_exists ~ty (recurse t')
  | T.Lambda _ -> failwith "FOFormula.of_term: unexpected lambda term"
  | T.Const s ->
    let ty = T.get_type t in
    mk_atom (FOTerm.mk_const ~ty s)
  | T.Var _ | T.BoundVar _ -> failwith "F.of_term: not first-order, var under formula"
  | T.At ({T.term=T.Const s}, l) when S.eq s S.and_symbol ->
      mk_and (List.map recurse l)
  | T.At ({T.term=T.Const s}, l) when S.eq s S.or_symbol ->
      mk_or (List.map recurse l)
  | T.At ({T.term=T.Const s}, [a; b]) when S.eq s S.equiv_symbol ->
    mk_equiv (recurse a) (recurse b)
  | T.At ({T.term=T.Const s}, [a; b]) when S.eq s S.imply_symbol ->
    mk_imply (recurse a) (recurse b)
  | T.At ({T.term=T.Const s}, [a; b]) when S.eq s S.or_symbol ->
    mk_and [recurse a; recurse b]
  | T.At ({T.term=T.Const s}, [a; b]) when S.eq s S.and_symbol ->
    mk_or [recurse a; recurse b]
  | T.At ({T.term=T.Const s}, [a; b]) when S.eq s S.eq_symbol ->
    mk_eq (T.uncurry a) (T.uncurry b)
  | T.At ({T.term=T.Const s}, [t']) when S.eq s S.not_symbol ->
    mk_not (recurse t')
  | _ -> mk_atom (T.uncurry t)
  in
  recurse t

(** {2 IO} *)

let pp_debug ?(hooks=[]) buf f =
  let depth = ref 0 in
  (* outer formula *)
  let rec pp_outer buf f = match f.form with
  | True -> Buffer.add_string buf "true"
  | False -> Buffer.add_string buf "false"
  | Atom t -> (T.pp_depth ~hooks !depth) buf t
  | Not {form=Equal (t1, t2)} ->
    T.pp_depth ~hooks !depth buf t1;
    Buffer.add_string buf " ≠ ";
    T.pp_depth ~hooks !depth buf t2
  | Equal (t1, t2) ->
    T.pp_depth ~hooks !depth buf t1;
    Buffer.add_string buf " = ";
    T.pp_depth ~hooks !depth buf t2
  | Not {form=Equiv (f1, f2)} ->
    pp_inner buf f1; Buffer.add_string buf " <~> "; pp_inner buf f2
  | Equiv (f1, f2) ->
    pp_inner buf f1; Buffer.add_string buf " <=> "; pp_inner buf f2
  | Imply (f1, f2) ->
    pp_inner buf f1; Buffer.add_string buf " => "; pp_inner buf f2
  | Not f' -> Buffer.add_string buf "¬ "; pp_inner buf f'
  | Forall (ty,f') ->
    let v = !depth in
    incr depth;
    if Type.eq ty Type.i
      then Printf.bprintf buf "∀ Y%d. %a" v pp_inner f'
      else Printf.bprintf buf "∀ Y%d: %a. %a" v Type.pp ty pp_inner f';
    decr depth
  | Exists (ty, f') ->
    let v = !depth in
    incr depth;
    if Type.eq ty Type.i
      then Printf.bprintf buf "∃ Y%d. %a" v pp_inner f'
      else Printf.bprintf buf "∃ Y%d: %a. %a" v Type.pp ty pp_inner f';
    decr depth
  | And l -> Util.pp_list ~sep:" ∧ " pp_inner buf l
  | Or l -> Util.pp_list ~sep:" ∨ " pp_inner buf l
  and pp_inner buf f = match f.form with
  | And _
  | Or _
  | Equiv _
  | Imply _ ->
    (* cases where ambiguities could arise *)
    Buffer.add_char buf '('; pp_outer buf f; Buffer.add_char buf ')';
  | _ -> pp_outer buf f
  in
  pp_outer buf (flatten f)

let pp_tstp buf f =
  let depth = ref 0 in
  (* outer formula *)
  let rec pp_outer buf f = match f.form with
  | True -> Buffer.add_string buf "$true"
  | False -> Buffer.add_string buf "$false"
  | Atom t -> (T.pp_tstp_depth !depth) buf t
  | Not {form=Equal (t1, t2)} ->
    (T.pp_tstp_depth !depth) buf t1;
    Buffer.add_string buf " != ";
    (T.pp_tstp_depth !depth) buf t2
  | Equal (t1, t2) ->
    (T.pp_tstp_depth !depth) buf t1;
    Buffer.add_string buf " = ";
    (T.pp_tstp_depth !depth) buf t2
  | Not {form=Equiv (f1, f2)} ->
    pp_inner buf f1; Buffer.add_string buf " <~> "; pp_inner buf f2
  | Equiv (f1, f2) ->
    pp_inner buf f1; Buffer.add_string buf " <=> "; pp_inner buf f2
  | Imply (f1, f2) ->
    pp_inner buf f1; Buffer.add_string buf " => "; pp_inner buf f2
  | Not f' -> Buffer.add_string buf "~ "; pp_inner buf f'
  | Forall (ty,f') ->
    let v = !depth in
    incr depth;
    if Type.eq ty Type.i
      then Printf.bprintf buf "![Y%d]: %a" v pp_inner f'
      else Printf.bprintf buf "![Y%d:%a]: %a" v Type.pp ty pp_inner f';
    decr depth
  | Exists (ty, f') ->
    let v = !depth in
    incr depth;
    if Type.eq ty Type.i
      then Printf.bprintf buf "?[Y%d]: %a" v pp_inner f'
      else Printf.bprintf buf "?[Y%d:%a]: %a" v Type.pp ty pp_inner f';
    decr depth
  | And l -> Util.pp_list ~sep:" & " pp_inner buf l
  | Or l -> Util.pp_list ~sep:" | " pp_inner buf l
  and pp_inner buf f = match f.form with
  | And _
  | Or _
  | Equiv _
  | Imply _ ->
    (* cases where ambiguities could arise *)
    Buffer.add_char buf '('; pp_outer buf f; Buffer.add_char buf ')';
  | _ -> pp_outer buf f
  in
  pp_outer buf (flatten f)

let pp_arith buf f = pp_debug ~hooks:[T.arith_hook] buf f

let __default_pp = ref (pp_debug ~hooks:[])

let pp buf f = !__default_pp buf f

let set_default_pp pp = __default_pp := pp

let to_string f =
  Util.on_buffer pp f

let to_string_debug f =
  Util.on_buffer pp_debug f

let to_string_tstp f =
  Util.on_buffer pp_tstp f

let fmt fmt f =
  Format.pp_print_string fmt (to_string f)

(* XXX KISS: use the term bijection *)
let bij =
  Bij.(map
    ~inject:to_term
    ~extract:of_term
    HOTerm.bij)

(** {2 Containers} *)

module Tbl = Hashtbl.Make(struct
  type t = form
  let equal = eq
  let hash = hash
end)

module UT = Untyped.FO
module UF = Untyped.Form

let erase_types f =
  let rec erase depth f = match f.form with
    | True -> UF.mk_true
    | False -> UF.mk_false
    | Atom p -> UF.atom (T.erase_types ~depth p)
    | Equal (t1, t2) -> UF.mk_eq (T.erase_types ~depth t1) (T.erase_types ~depth t2)
    | And l -> UF.mk_and (List.map (erase depth) l)
    | Or l -> UF.mk_or (List.map (erase depth) l)
    | Not f' -> UF.mk_not (erase depth f')
    | Imply (f1, f2) -> UF.mk_imply (erase depth f1) (erase depth f2)
    | Equiv (f1, f2) -> UF.mk_equiv (erase depth f1) (erase depth f2)
    | Forall (ty, f') ->
      let v = UT.var ~ty:(Type.to_parsed ty) (Util.sprintf "Y%d" depth) in
      UF.forall [v] (erase (depth+1) f')
    | Exists (ty, f') ->
      let v = UT.var ~ty:(Type.to_parsed ty) (Util.sprintf "Y%d" depth) in
      UF.exists [v] (erase (depth+1) f')
  in
  erase 0 f
