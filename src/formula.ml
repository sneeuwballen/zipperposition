
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

type sourced_form = t * string * string    (* form, filename, axiom name *)

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

let mk_forall_list vars f =
  List.fold_right mk_forall vars f

let mk_exists_list vars f =
  List.fold_right mk_exists vars f

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
  | Forall (v, f') -> mk_forall v (map_leaf f f')
  | Exists (v, f') -> mk_exists v (map_leaf f f')

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
  | Forall (v, f') 
  | Exists (v, f') -> fold f acc f'

let iter f form = fold (fun () t -> f t) () form

let fold_bv ?(bv=[]) f acc form =
  let rec recurse f acc bv form = match form.form with
  | And l
  | Or l -> List.fold_left (fun acc f' -> recurse f acc bv f') acc l
  | Imply (f1, f2)
  | Equiv (f1, f2) ->
    let acc = recurse f acc bv f1 in
    let acc = recurse f acc bv f2 in
    acc
  | Not f' -> recurse f acc bv f'
  | True
  | False -> acc
  | Atom p -> f acc bv p
  | Equal (t1, t2) ->
    let acc = f acc bv t1 in
    let acc = f acc bv t2 in
    acc
  | Forall (v, f') 
  | Exists (v, f') ->
    let bv = if List.memq v bv then bv else v :: bv in
    recurse f acc bv f'
  in
  recurse f acc bv form

let add_terms set f = iter (T.THashSet.add set) f

let terms f =
  let set = T.THashSet.create () in
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

let free_variables f =
  (* start with a fresh set *)
  let rec start f =
    let set = T.THashSet.create ~size:3 () in
    recurse set f;
    set
  (* recurse with the same set of variables *)
  and recurse set f = match f.form with
  | Atom p -> T.add_vars set p
  | Equal (t1, t2) ->
    T.add_vars set t1;
    T.add_vars set t2
  | True
  | False -> ()
  | And l
  | Or l -> List.iter (recurse set) l
  | Imply (f1, f2)
  | Equiv (f1, f2) -> recurse set f1; recurse set f2
  | Not f' -> recurse set f'
  | Forall (v, f')
  | Exists (v, f') ->
    (* compute free vars of [f'] independently *)
    let set' = start f' in
    (* then remove [v] from it *)
    T.THashSet.remove set' v;
    (* and merge it back to [set] *)
    T.THashSet.merge set set';
  in
  let set = start f in
  T.THashSet.to_list set

let close_forall f =
  let fv = free_variables f in
  mk_forall_list fv f

let close_exists f =
  let fv = free_variables f in
  mk_exists_list fv f

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
  | Forall (v, f') -> mk_forall v (recurse f')
  | Exists (v, f') -> mk_exists v (recurse f')
  in
  recurse (flatten f)

let ac_eq f1 f2 =
  let f1 = ac_normal_form f1 in
  let f2 = ac_normal_form f2 in
  eq f1 f2

let apply_subst ?renaming ?recursive ~subst f scope =
  let rec recurse subst f = match f.form with
  | Atom p ->
    let p' = Substs.apply_subst ?recursive ?renaming subst p scope in
    mk_atom p'
  | Equal (t1, t2) ->
    let t1' = Substs.apply_subst ?recursive ?renaming subst t1 scope in
    let t2' = Substs.apply_subst ?recursive ?renaming subst t2 scope in
    mk_eq t1' t2'
  | True
  | False -> f
  | And l -> mk_and (List.map (recurse subst) l)
  | Or l -> mk_or (List.map (recurse subst) l)
  | Imply (f1, f2) -> mk_imply (recurse subst f1) (recurse subst f2)
  | Equiv (f1, f2) -> mk_equiv (recurse subst f1) (recurse subst f2)
  | Not f' -> mk_not (recurse subst f')
  | Forall (v, f') ->
    Util.debug 5 "remove %a %d from subst %a" T.pp v scope Substs.pp subst; (* dangerous *)
    let subst = Substs.remove subst v scope in
    mk_forall v (recurse subst f')
  | Exists (v, f') ->
    let subst = Substs.remove subst v scope in
    mk_exists v (recurse subst f')
  in
  recurse subst f

(** {2 Conversion} *)

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

(** {2 Typing} *)

let rec infer_type ctx f = match f.form with
  | True
  | False -> ()
  | Atom p ->
    TypeInference.constrain_term_type ctx p Type.o
  | Equal (t1, t2) ->
    TypeInference.constrain_term_term ctx t1 t2
  | And l
  | Or l -> List.iter (infer_type ctx) l
  | Forall (v, f')
  | Exists (v, f') -> infer_type ctx f'
  | Equiv (f1, f2)
  | Imply (f1, f2) -> infer_type ctx f1; infer_type ctx f2
  | Not f' -> infer_type ctx f'

let signature ?(signature=Signature.empty) f =
  let ctx = TypeInference.Ctx.of_signature signature in
  infer_type ctx f;
  TypeInference.default_to_i ctx;
  TypeInference.Ctx.to_signature ctx

(** {2 IO} *)

let pp buf f =
  (* outer formula *)
  let rec pp_outer buf f = match f.form with
  | True -> Buffer.add_string buf "true"
  | False -> Buffer.add_string buf "false"
  | Atom t -> T.pp buf t
  | Not {form=Equal (t1, t2)} ->
    T.pp buf t1; Buffer.add_string buf " != "; T.pp buf t2
  | Equal (t1, t2) ->
    T.pp buf t1; Buffer.add_string buf " = "; T.pp buf t2
  | Not {form=Equiv (f1, f2)} ->
    pp_inner buf f1; Buffer.add_string buf " <~> "; pp_inner buf f2
  | Equiv (f1, f2) ->
    pp_inner buf f1; Buffer.add_string buf " <=> "; pp_inner buf f2
  | Imply (f1, f2) ->
    pp_inner buf f1; Buffer.add_string buf " => "; pp_inner buf f2
  | Not f' -> Buffer.add_string buf "¬ "; pp_inner buf f'
  | Forall (v, f') ->
    Printf.bprintf buf "∀ %a. %a" T.pp v pp_inner f'
  | Exists (v, f') ->
    Printf.bprintf buf "∃ %a. %a" T.pp v pp_inner f'
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

let to_string f =
  Util.on_buffer pp f

let fmt fmt f =
  Format.pp_print_string fmt (to_string f)

let pp_tstp buf f =
  (* outer formula *)
  let rec pp_outer buf f = match f.form with
  | True -> Buffer.add_string buf "$true"
  | False -> Buffer.add_string buf "$false"
  | Atom t -> T.pp buf t
  | Not {form=Equal (t1, t2)} ->
    T.pp buf t1; Buffer.add_string buf " != "; T.pp buf t2
  | Equal (t1, t2) ->
    T.pp buf t1; Buffer.add_string buf " = "; T.pp buf t2
  | Not {form=Equiv (f1, f2)} ->
    pp_inner buf f1; Buffer.add_string buf " <~> "; pp_inner buf f2
  | Equiv (f1, f2) ->
    pp_inner buf f1; Buffer.add_string buf " <=> "; pp_inner buf f2
  | Imply (f1, f2) ->
    pp_inner buf f1; Buffer.add_string buf " => "; pp_inner buf f2
  | Not f' -> Buffer.add_string buf "~ "; pp_inner buf f'
  | Forall (v, f') ->
    Printf.bprintf buf "![%a]: %a" T.pp v pp_inner f'
  | Exists (v, f') ->
    Printf.bprintf buf "?[%a]: %a" T.pp v pp_inner f'
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

(* KISS: use the term bijection *)
let bij =
  Bij.(map
    ~inject:to_term
    ~extract:of_term
    T.bij)
