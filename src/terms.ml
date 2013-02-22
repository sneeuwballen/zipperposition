(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

open Hashcons
open Types
open Symbols

(** {1 First-order terms} *)

module Utils = FoUtils

let hash_term t = match t.term with
  | Var i -> Hash.hash_int2 i (hash_sort t.sort)
  | BoundVar i -> Hash.hash_int2 i (hash_sort t.sort)
  | Node (s, l) ->
    let h = Hash.hash_list (fun x -> x.hkey) 0 l in
    let h = Hash.combine h (hash_symbol s) in
    Hash.combine h (hash_sort t.sort)
  | Bind (s, sort, t) ->
    Hash.combine (hash_symbol s) t.hkey

let prof_mk_node = Utils.mk_profiler "Terms.mk_node"

(** {2 Comparison, equality, containers} *)

let rec member_term a b =
  a == b ||
  (match b.term with
  | Var _ | BoundVar _ -> false
  | Node (_, subterms) -> List.exists (member_term a) subterms
  | Bind (_, _, b') -> member_term a b')

let eq_term x y = x == y  (* because of hashconsing *)

let compare_term x y = x.tag - y.tag

module THashtbl = Hashtbl.Make(
  struct
    type t = term
    let hash t = t.hkey
    let equal t1 t2 = eq_term t1 t2
  end)

module TSet = Sequence.Set.Make(struct
  type t = term
  let compare = compare_term
end)

(** {2 Hashset of terms} *)

module THashSet =
  struct
    type t = unit THashtbl.t
    let create () = THashtbl.create 3
    let cardinal t = THashtbl.length t
    let member t term = THashtbl.mem t term
    let iter set f = THashtbl.iter (fun t () -> f t) set
    let add set t = THashtbl.replace set t ()
    let merge s1 s2 = iter s2 (add s1)
    let to_list set =
      let l = ref [] in
      iter set (fun t -> l := t :: !l); !l
    let from_list l =
      let set = create () in
      List.iter (add set) l; set
  end

(** {2 Global terms table (hashconsing)} *)

let hashcons_equal x y =
  (* pairwise comparison of subterms *)
  let rec eq_subterms a b = match (a, b) with
    | ([],[]) -> true
    | (a::a1, b::b1) ->
      if a == b then eq_subterms a1 b1 else false
    | (_, _) -> false
  in
  (* compare sorts, then subterms, if same structure *)
  if x.sort != y.sort then false
  else match x.term, y.term with
  | Var i, Var j | BoundVar i, BoundVar j -> i = j
  | Node (sa, la), Node (sb, lb) -> sa == sb && eq_subterms la lb
  | Bind (sa, sorta, ta), Bind (sb, sortb, tb) ->
    sa == sb && sorta == sortb && ta == tb
  | _ -> false

(** hashconsing for terms *)
module H = Hashcons.Make(struct
  type t = term

  let equal x y = hashcons_equal x y

  let hash t = t.hkey

  let tag i t = (t.tag <- i; t)
end)

let iter_terms f = H.iter f

let all_terms () =
  let l = ref [] in
  iter_terms (fun t -> l := t :: !l);
  !l
  
let stats () = H.stats ()

(** {2 Boolean flags} *)

let flag_db_closed = 1 lsl 0
and flag_simplified = 1 lsl 1
and flag_normal_form = 1 lsl 2
and flag_ground = 1 lsl 3
and flag_db_closed_computed = 1 lsl 4

let set_flag flag t truth =
  if truth
    then t.flags <- t.flags lor flag
    else t.flags <- t.flags land (lnot flag)

let get_flag flag t = (t.flags land flag) != 0

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. Some of them
    accept a [?old] optional argument. This argument is an already existing
    term that the caller believes is likely to be equal to the result.
    This makes hashconsing faster if the result is equal to [old]. *)

(** Compare [t] with [old], returning [old] if they are equal. Otherwise
    it hashconses [t] and returns the result *)
let hashcons ?old t =
  match old with
  | Some old when hashcons_equal old t -> old
  | _ ->  (* [old] is not correct, return [hashcons t] *)
    t.hkey <- hash_term t;
    H.hashcons t

let mk_var ?old idx sort =
  assert (idx >= 0);
  let rec my_v = {term = Var idx; sort=sort;
                  flags=(flag_db_closed lor flag_db_closed_computed lor
                         flag_simplified lor flag_normal_form);
                  tsize=1; tag= -1; hkey=0} in
  hashcons ?old my_v

let mk_bound_var ?old idx sort =
  assert (idx >= 0);
  let rec my_v = {term = BoundVar idx; sort=sort;
                  flags=(flag_db_closed_computed lor flag_simplified lor flag_normal_form);
                  tsize=1; tag= -1; hkey=0} in
  hashcons ?old my_v

let rec sum_sizes acc l = match l with
  | [] -> acc
  | x::l' -> sum_sizes (x.tsize + acc) l'

let rec compute_is_ground l = match l with
  | [] -> true
  | x::l' -> (get_flag flag_ground x) && compute_is_ground l'

let mk_bind ?old s r_sort arg_sort t' =
  assert (has_attr attr_binder s);
  let rec my_t = {term=Bind (s, arg_sort, t'); sort=r_sort; flags=0;
                  tsize=t'.tsize+1; tag= -1; hkey=0} in
  let t = hashcons ?old my_t in
  (if t == my_t
    then (* compute ground-ness of term *)
      set_flag flag_ground t (get_flag flag_ground t'));
  t

let mk_node ?old s sort l =
  Utils.enter_prof prof_mk_node;
  let rec my_t = {term=Node (s, l); sort; flags=0;
                  tsize=0; tag= -1; hkey=0} in
  my_t.hkey <- hash_term my_t;
  let t = hashcons ?old my_t in
  (if t == my_t
    then begin
      (* compute size of term *)
      t.tsize <- sum_sizes 1 l;
      (* compute ground-ness of term *)
      set_flag flag_ground t (compute_is_ground l);
    end);
  Utils.exit_prof prof_mk_node;
  t

let mk_const ?old s sort = mk_node ?old s sort []

let true_term = mk_const true_symbol bool_
let false_term = mk_const false_symbol bool_

let pp_symbol_tstp =
  object
    method pp formatter s = match s with
      | _ when s == not_symbol -> Format.pp_print_string formatter "~"
      | _ when s == eq_symbol -> Format.pp_print_string formatter "="
      | _ when s == lambda_symbol -> failwith "^"
      | _ when s == exists_symbol -> Format.pp_print_string formatter "?"
      | _ when s == forall_symbol -> Format.pp_print_string formatter "!"
      | _ when s == and_symbol -> Format.pp_print_string formatter "&"
      | _ when s == or_symbol -> Format.pp_print_string formatter "|"
      | _ when s == imply_symbol -> Format.pp_print_string formatter "=>"
      | _ -> Format.pp_print_string formatter (name_symbol s) (* default *)
    method infix s = has_attr attr_infix s
  end

let rec pp_sort formatter sort = match sort with
  | Sort s -> Format.pp_print_string formatter s
  | Fun (s, l) ->
    Format.fprintf formatter "(%a) > %a"
      (Utils.pp_list ~sep:" * " pp_sort) l pp_sort s

(* constructors for terms *)
let check_bool t = assert (t.sort == bool_)
let check_same t1 t2 =
  (if t1.sort != t2.sort then Format.printf "different sort %a and %a@." pp_sort t1.sort pp_sort t2.sort);
  assert (t1.sort == t2.sort)

let mk_not t = (check_bool t; mk_node not_symbol bool_ [t])
let mk_and a b = (check_bool a; check_bool b; mk_node and_symbol bool_ [a; b])
let mk_or a b = (check_bool a; check_bool b; mk_node or_symbol bool_ [a; b])
let mk_imply a b = (check_bool a; check_bool b; mk_node imply_symbol bool_ [a; b])
let mk_equiv a b = (check_bool a; check_bool b; mk_node eq_symbol bool_ [a; b])
let mk_xor a b = mk_not (mk_equiv a b)
let mk_eq a b = (check_same a b; mk_node eq_symbol bool_ [a; b])
let mk_neq a b = mk_not (mk_eq a b)
let mk_lambda sort a_sort t = mk_bind lambda_symbol sort a_sort t
let mk_forall a_sort t = (check_bool t; mk_bind forall_symbol bool_ a_sort t)
let mk_exists a_sort t = (check_bool t; mk_bind exists_symbol bool_ a_sort t) 

let mk_at ?old t1 t2 =
  match t1.sort, t2.sort with
  | Fun (a, [b]), b' when b == b' ->
    mk_node ?old at_symbol a [t1; t2]
  | _ -> raise (SortError "incompatible types for @")

let rec cast t sort =
  let new_t = {t with sort=sort} in
  new_t.hkey <- hash_term new_t;
  H.hashcons new_t

(** {2 Subterms and positions} *)

let is_var t = match t.term with
  | Var _ -> true
  | _ -> false

let is_bound_var t = match t.term with
  | BoundVar _ -> true
  | _ -> false

let is_bind t = match t.term with
  | Bind _ -> true
  | _ -> false

let is_const t = match t.term with
  | Node (s, []) -> true
  | _ -> false

let is_node t = match t.term with
  | Node _ -> true
  | _ -> false

let rec at_pos t pos = match t.term, pos with
  | _, [] -> t
  | Var _, _::_ -> invalid_arg "wrong position in term"
  | Node (_, l), i::subpos when i < List.length l ->
    at_pos (Utils.list_get l i) subpos
  | Bind (_, _, t'), 0::subpos -> at_pos t' subpos
  | _ -> invalid_arg "index too high for subterm"

let rec replace_pos t pos new_t = match t.term, pos with
  | _, [] -> new_t
  | Var _, _::_ -> invalid_arg "wrong position in term"
  | Node (s, l), i::subpos when i < List.length l ->
    let new_subterm = replace_pos (Utils.list_get l i) subpos new_t in
    mk_node s t.sort (Utils.list_set l i new_subterm)
  | Bind (_, _, t'), 0::subpos -> replace_pos t' subpos new_t
  | _ -> invalid_arg "index too high for subterm"

(** [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let rec replace t ~old ~by = match t.term with
  | _ when t == old -> by
  | Var _ | BoundVar _ -> t
  | Bind (s, a_sort, t') ->
    mk_bind ~old:t s t.sort a_sort (replace t' ~old ~by)
  | Node (s, l) ->
    let l' = List.map (fun t' -> replace t' ~old ~by) l in
    mk_node ~old:t s t.sort l'

(** get subterm by its position *)
let at_cpos t pos = 
  let rec recurse t pos =
    match t.term, pos with
    | _, 0 -> t
    | Node (_, l), _ -> get_subpos l (pos - 1)
    | Bind (_, _, t'), _ -> recurse t' (pos-1)
    | _ -> assert false
  and get_subpos l pos =
    match l, pos with
    | t::l', _ when t.tsize > pos -> recurse t pos  (* search inside the term *)
    | t::l', _ -> get_subpos l' (pos - t.tsize) (* continue to next term *)
    | [], _ -> assert false
  in recurse t pos

let max_cpos t = t.tsize - 1

let pos_to_cpos pos = failwith "not implemented"

let cpos_to_pos cpos = failwith "not implemented"

let var_occurs x t =
  let rec check x t = match t.term with
  | Var _ -> x == t
  | BoundVar _ -> false
  | Bind (_, _, t') -> check x t'
  | Node (_, l) -> check_list x l
  and check_list x l =
    match l with
    | [] -> false
    | y::l' -> check x y || check_list x l'
  in
  check x t

let is_ground_term t = get_flag flag_ground t

let max_var vars =
  let rec aux idx = function
  | [] -> idx
  | ({term=Var i}::vars) -> aux (max i idx) vars
  | _::vars -> assert false
  in
  aux 0 vars

let min_var vars =
  let rec aux idx = function
  | [] -> idx
  | ({term=Var i}::vars) -> aux (min i idx) vars
  | _ -> assert false
  in
  aux max_int vars

(** add variables of the term to the set *)
let add_vars set t =
  let rec add set t = match t.term with
  | Var _ -> THashSet.add set t
  | BoundVar _ -> ()
  | Bind (_, _, t') -> add set t'
  | Node (_, l) -> add_list set l
  and add_list set l = match l with
  | [] -> ()
  | x::l' -> add set x; add_list set l'
  in
  add set t

(** compute variables of the term *)
let vars t =
  let set = THashSet.create () in
  add_vars set t;
  THashSet.to_list set

(** Compute variables of terms in the list *)
let vars_list l =
  let set = THashSet.create () in
  List.iter (add_vars set) l;
  THashSet.to_list set

(** Compute variables of terms in the sequence *)
let vars_seq seq =
  let set = THashSet.create () in
  Sequence.iter (add_vars set) seq;
  THashSet.to_list set

(** depth of term *)
let depth t =
  let rec depth t = match t.term with
  | Var _ | BoundVar _ -> 1
  | Bind (_, _, t') -> 1 + depth t'
  | Node (_, l) -> 1 + depth_list 0 l
  and depth_list m l = match l with
  | [] -> m
  | t::l' -> depth_list (max m (depth t)) l'
  in depth t

(** {2 De Bruijn indexes} *)

(** check whether the term is a term or an atomic proposition *)
let rec atomic t = match t.term with
  | _ when t.sort != bool_ -> true
  | Var _ | BoundVar _ -> true
  | Bind (s, _, t') -> not (s == forall_symbol || s == exists_symbol || not (atomic t'))
  | Node (s, l) -> not (s == and_symbol || s == or_symbol
    || s == imply_symbol || s == not_symbol || s == eq_symbol)

(** check whether the term contains connectives or quantifiers *)
let rec atomic_rec t = match t.term with
  | _ when t.sort != bool_ -> true  (* first order *)
  | Var _ | BoundVar _ -> true
  | Bind (s, _, t') -> not (s == forall_symbol || s == exists_symbol || not (atomic_rec t'))
  | Node (s, l) ->
    not (s == and_symbol || s == or_symbol || s == imply_symbol
      || s == not_symbol || s == eq_symbol)
    && List.for_all atomic_rec l

(* compute whether the term is closed w.r.t. De Bruijn (bound) variables *)
let compute_db_closed depth t =
  let rec recurse depth t = match t.term with
  | BoundVar i -> i < depth
  | Bind (s, _, t') -> recurse (depth+1) t'
  | Var _ -> true
  | Node (_, l) -> recurse_list depth l
  and recurse_list depth l = match l with
  | [] -> true
  | x::l' -> recurse depth x && recurse_list depth l'
  in
  recurse depth t

(** check wether the term is closed w.r.t. De Bruijn variables *)
let db_closed t =
  (* compute it, if not already computed *)
  (if not (get_flag flag_db_closed_computed t) then begin
    set_flag flag_db_closed_computed t true;
    set_flag flag_db_closed t (compute_db_closed 0 t);
    end);
  get_flag flag_db_closed t

(** check whether t contains the De Bruijn symbol n *)
let rec db_contains t n = match t.term with
  | BoundVar i -> i = n
  | Var _ -> false
  | Bind (_, _, t') -> db_contains t' (n+1)
  | Node (_, l) -> List.exists (fun t' -> db_contains t' n) l

(** replace 0 by s in t *)
let db_replace t s =
  (* replace db by s in t *)
  let rec replace depth s t = match t.term with
  | BoundVar n -> if n = depth then s else t
  | Var _ -> t
  | Bind (symb, a_sort, t') ->
    (* lift the De Bruijn to replace *)
    mk_bind ~old:t symb t.sort a_sort (replace (depth+1) s t')
  | Node (_, []) -> t
  | Node (f, l) ->
    mk_node ~old:t f t.sort (List.map (replace depth s) l)
  (* replace the 0 De Bruijn index by s in t *)
  in
  replace 0 s t

(** lift the non-captured De Bruijn indexes in the term by n *)
let db_lift n t =
  (* traverse the term, looking for non-captured DB indexes.
     [depth] is the number of binders on the path from the root of the
     term, to the current position. *)
  let rec recurse depth t = 
    match t.term with
    | _ when db_closed t -> t  (* closed. *)
    | BoundVar i when i >= depth ->
      mk_bound_var (i+n) t.sort (* lift by n, term not captured *)
    | Var _ | BoundVar _ -> t
    | Bind (s, a_sort, t') ->
      mk_bind ~old:t s t.sort a_sort (recurse (depth+1) t')  (* increase depth and recurse *)
    | Node (_, []) -> t
    | Node (s, l) ->
      let l' = List.map (recurse depth) l in
      mk_node ~old:t s t.sort l'  (* recurse in subterms *)
  in
  assert (n >= 0);
  if n = 0 then t else recurse 0 t

(* unlift the term (decrement indices of all De Bruijn variables inside *)
let db_unlift t =
  (* only unlift DB symbol that are free. [depth] is the number of binders
     on the path from the root term. *)
  let rec recurse depth t =
    match t.term with
    | BoundVar i -> if i >= depth then mk_bound_var (i-1) t.sort else t
    | Node (_, []) | Var _ -> t
    | Bind (s, a_sort, t') ->
      mk_bind ~old:t s t.sort a_sort (recurse (depth+1) t')
    | Node (s, l) ->
      mk_node ~old:t s t.sort (List.map (recurse depth) l)
  in recurse 0 t

(** Replace [t'] by a fresh De Bruijn index in [t]. *)
let db_from_term t t' =
  (* recurse and replace [t']. *)
  let rec replace depth t = match t.term with
  | _ when t == t' -> mk_bound_var depth t'.sort
  | Var _ -> t
  | Bind (s, a_sort, t') ->
    mk_bind ~old:t s t.sort a_sort (replace (depth+1) t')
  | BoundVar _ -> t
  | Node (_, []) -> t
  | Node (s, l) -> mk_node ~old:t s t.sort (List.map (replace depth) l)
  in
  replace 0 t

  (** [db_from_var t v] replace v by a De Bruijn symbol in t.
    Same as db_from_term. *)
let db_from_var t v =
  assert (is_var v);
  db_from_term t v

exception FoundSort of sort

(** [look_db_sort n t] find the sort of the De Bruijn index [n] in [t].
    Raise Not_found otherwise. *)
let look_db_sort i t =
  let rec lookup depth t = match t.term with
  | BoundVar i -> if i = depth then raise (FoundSort t.sort) else ()
  | Var _ -> ()
  | Bind (_, _, t') -> lookup (depth+1) t'
  | Node (_, l) -> List.iter (lookup depth) l
  in try lookup i t; None
     with FoundSort s -> Some s

(** {2 High-level operations} *)

(** constructors with free variables. The first argument is the
    list of variables that is bound, then the quantified/abstracted
    term. *)

let mk_lambda_var vars t =
  List.fold_right
    (fun var t ->
      let sort = t.sort <=. var.sort in
      mk_lambda sort var.sort (db_from_var t var))
    vars t

let mk_forall_var vars t =
  List.fold_right
    (fun var t -> mk_forall var.sort (db_from_var t var))
    vars t

let mk_exists_var vars t =
  List.fold_right
    (fun var t -> mk_exists var.sort (db_from_var t var))
    vars t

(** Compute the signature of the set of terms *)
let signature seq =
  (* explore a term *)
  let rec explore_term signature t = match t.term with
  | Var _ | BoundVar _ -> signature
  | Bind (s, _, t') ->
    let sort = t.sort in
    let signature' = update_sig signature s sort in
    explore_term signature' t'
  | Node (s, l) when s == at_symbol || s == eq_symbol ->
    (* ad-hoc polymorphism *)
    List.fold_left explore_term signature l
  | Node (f, l) ->
    let sort = t.sort <== (List.map (fun x -> x.sort) l) in
    let signature' = update_sig signature f sort in
    List.fold_left explore_term signature' l
  (* Update signature with s -> sort.
     Checks consistency with current value, if any. *)
  and update_sig signature f sort =
    (try
      let sort' = SMap.find f signature in
      if sort != sort' then Format.printf "sort %a != %a@." pp_sort sort pp_sort sort';
      assert (sort == sort');
    with Not_found -> ());
    let signature' = SMap.add f sort signature in
    signature'
  in
  Sequence.fold explore_term empty_signature seq

let symbols seq =
  let s = signature seq in
  let keys = SMapSeq.keys s in
  SSetSeq.of_seq keys

(** Bind all free variables by 'forall' *)
let close_forall t =
  let vars = vars t in
  List.fold_left
    (fun t var ->
      let sort = bool_ in
      mk_bind forall_symbol sort var.sort (db_from_var t var))
    t vars

(** Bind all free variables by 'exists' *)
let close_exists t =
  let vars = vars t in
  List.fold_left
    (fun t var ->
      let sort = bool_ in
      mk_bind exists_symbol sort var.sort (db_from_var t var))
    t vars

(** Transform binders and De Bruijn indexes into regular variables.
    [varindex] is a variable counter used to give fresh variables
    names to De Bruijn indexes. *)
let rec db_to_classic ?(varindex=ref 0) t =
  match t.term with
  | Bind (s, a_sort, t') ->
    let v = mk_var !varindex a_sort in
    incr varindex;
    let new_t = mk_node s t.sort [v; db_unlift (db_replace t' v)] in
    db_to_classic ~varindex new_t
  | Node (_, []) | Var _ -> t
  | BoundVar _ ->  (* free variable *)
    let n = !varindex in
    incr varindex;
    mk_var n t.sort
  | Node (s, l) ->
    mk_node s t.sort (List.map (db_to_classic ~varindex) l)

(** Currify all subterms *)
let rec curry t =
  match t.term with
  | Var _ | BoundVar _ -> t
  | Bind (s, a_sort, t') -> mk_bind s t.sort a_sort (curry t')
  | Node (f, [a;b]) when f == at_symbol -> mk_at ~old:t (curry a) (curry b)
  | Node (f, []) -> t
  | Node (f, [t']) ->
    let sort = t.sort <=. t'.sort in
    mk_at (mk_const f sort) (curry t')
  | Node (f, l) ->
    (* compute sort of [f] *)
    let sorts = List.map (fun x -> x.sort) l in
    let sort = List.fold_right (fun arg res -> res <=. arg) sorts t.sort in
    (* build the curryfied application of [f] to [l] *)
    List.fold_left
      (fun left t' -> mk_at left (curry t'))
      (mk_const f sort) l

(** Uncurrify all subterms *)
let uncurry t =
  (* uncurry any kind of term, except the '@' terms that are
     handled over to unfold_left *)
  let rec uncurry t =
    match t.term with
    | Var _ | BoundVar _ -> t
    | Bind (s, a_sort, t') -> mk_bind s t.sort a_sort (uncurry t')
    | Node (_, []) -> t  (* constant *)
    | Node (f, [a;b]) when f == at_symbol ->
      unfold_left a [uncurry b]  (* remove the '@' *)
    | Node (f, l) -> mk_node f t.sort (List.map uncurry l)
  (* transform "(((f @ a) @ b) @ c) into f(a,b,c)". Here, we
     deconstruct "f @ a" into "unfold f (a :: args)"*)
  and unfold_left head args = match head.term with
    | Node (f, []) ->
      (* totally unfolded, compute the resulting sort and build node *)
      let fun_sort = uncurry_sort [] head.sort in
      let sort = fun_sort @@ (List.map (fun x -> x.sort) args) in
      mk_node f sort args 
    | Node (f, [a;b]) when f == at_symbol ->
      unfold_left a (uncurry b :: args)
    | _ -> failwith "not a curryfied term"
  and uncurry_sort args sort = match sort with
    | Sort _ -> sort <== args
    | Fun (s, [l]) -> uncurry_sort (l::args) s
    | _ -> failwith "not a curryfied sort"
  in
  uncurry t

let rec curryfied t =
  failwith "not implemented" (* TODO *)

let rec is_fo t = match t.term with
  | Var _ | BoundVar _ -> true
  | Bind (s, _, t') when s == lambda_symbol -> false
  | Bind (_, _, t') -> is_fo t'
  | Node (s, [a;b]) when s == at_symbol ->
    (* X @ _ is not first-order  *)
    not (is_var a) && is_fo a && is_fo b
  | Node (_, l) -> List.for_all is_fo l

(** Beta reduce the (curryfied) term, ie [(^[X]: t) @ t']
    becomes [subst(X -> t')(t)] *)
let rec beta_reduce t =
  match t.term with
  | Var _ | BoundVar _ -> t
  | Bind (s, a_sort, t') -> mk_bind ~old:t s t.sort a_sort (beta_reduce t')
  | Node (a, [{term=Bind (s, _, t1)} as fun_; t2])
    when a == at_symbol && s == lambda_symbol ->
    (* a beta-redex! Fire!! *)
    let _ = fun_.sort @@ [t2.sort] in
    let t1' = db_replace t1 t2  in
    let t1' = db_unlift t1' in
    beta_reduce t1'
  | Node (f, l) ->
    mk_node ~old:t f t.sort (List.map beta_reduce l)

(** Eta-reduce the (curryfied) term, ie [^[X]: (t @ X)]
    becomes [t] if [X] does not occur in [t]. *)
let rec eta_reduce t =
  match t.term with
  | Var _ | BoundVar _ -> t
  | Bind (s, _, {term=Node (a, [t'; {term=BoundVar 0} as x])})
    when s == lambda_symbol && not (db_contains t' 0) ->
    let _ = t.sort @@ [x.sort] in
    eta_reduce (db_unlift t')  (* remove the lambda and variable *)
  | Bind (s, a_sort, t') ->
    mk_bind ~old:t s t.sort a_sort (eta_reduce t')
  | Node (f, l) ->
    mk_node ~old:t f t.sort (List.map eta_reduce l)

(** [eta_lift t sub_t], applied to a currified term [t], and a
    subterm [sub_t] of [t], gives [t'] such that
    [beta_reduce (t' @ sub_t) == t] holds.
    It basically abstracts out [sub_t] with a lambda.

    For instance (@ are omitted), [eta_lift f(a,g @ b,c) g] will return
    the term [^[X]: f(a, X @ b, c)] *)
let eta_lift t sub_t =
  let sort = t.sort <=. sub_t.sort in
  mk_lambda sort sub_t.sort (db_from_term t sub_t)

(** {2 Some AC-utils} *)

(** [flatten_ac f l] flattens the list of terms [l] by deconstructing all its
    elements that have [f] as head symbol. For instance, if l=[1+2; 3+(4+5)]
    with f="+", this will return [1;2;3;4;5], perhaps in a different order *)
let flatten_ac f l =
  let rec flatten acc l = match l with
  | [] -> acc
  | x::l' -> flatten (deconstruct acc x) l'
  and deconstruct acc t = match t.term with
  | Node (f', l') when f == f' ->
    flatten acc l'
  | _ -> t::acc
  in flatten [] l

(** normal form of the term modulo AC *)
let ac_normal_form ?(is_ac=fun s -> has_attr attr_ac s)
                   ?(is_com=fun s -> has_attr attr_commut s)
                   t =
  let rec normalize t = match t.term with
    | Var _ -> t
    | BoundVar _ -> t
    | Bind (s, a_sort, t') -> mk_bind ~old:t s t.sort a_sort (normalize t')
    | Node (f, ([_;_] as l)) when is_ac f ->
      let l = flatten_ac f l in
      let l = List.map normalize l in
      let l = List.sort compare_term l in
      (match l with
        | x::l' -> List.fold_left
          (fun subt x -> mk_node f t.sort [x;subt])
          x l'
        | [] -> assert false)
    | Node (f, [a;b]) when is_com f ->
      if compare_term a b > 0
        then mk_node f t.sort [b; a]
        else t
    | Node (f, l) ->
      let l = List.map normalize l in
      mk_node ~old:t f t.sort l
  in
  normalize t

(** Check whether the two terms are AC-equal. Optional arguments specify
    which symbols are AC or commutative (by default by looking at
    attr_ac and attr_commut) *)
let ac_eq ?(is_ac=fun s -> has_attr attr_ac s)
          ?(is_com=fun s -> has_attr attr_commut s)
          t1 t2 =
  let t1' = ac_normal_form ~is_ac ~is_com t1
  and t2' = ac_normal_form ~is_ac ~is_com t2 in
  t1' == t2'

(** {2 Pretty printing} *)

(** type of a pretty printer for terms *)
class type pprinter_term =
  object
    method pp : Format.formatter -> term -> unit    (** pretty print a term *)
  end

let pp_term_debug =
  let _sort = ref false
  in
  (* printer itself *)
  object (self)
    method pp formatter t =
      let maxvar = max (max_var (vars t)) 0 in
      let varindex = ref (maxvar+1) in
      let t = db_to_classic ~varindex t in
      (match t.term with
      | Var i ->
        (match t.sort with
        | Sort _ -> Format.fprintf formatter "X%d" i
        | Fun _ -> Format.fprintf formatter "F%d" i)
      | BoundVar _ -> assert false
      | Bind _ -> assert false
      | Node (s, [{term=Node (s', [a;b])}]) when s == not_symbol
        && s' == eq_symbol && a.sort == bool_ ->
        Format.fprintf formatter "(%a <~> %a)" self#pp a self#pp b
      | Node (s, [a;b]) when s == eq_symbol && a.sort == bool_ ->
        Format.fprintf formatter "(%a <=> %a)" self#pp a self#pp b
      | Node (s, [{term=Node (s', [a; b])}])
        when s == not_symbol && s' == eq_symbol ->
        Format.fprintf formatter "%a != %a" self#pp a self#pp b
      | Node (s, [a; b]) when s == eq_symbol ->
        Format.fprintf formatter "(%a = %a)" self#pp a self#pp b
      | Node (s, [v; t']) when has_attr attr_binder s ->
        assert (is_var v);
        Format.fprintf formatter "%a[%a]: %a" pp_symbol s self#pp v self#pp t'
      | Node (s, [t]) when s == not_symbol ->
        Format.fprintf formatter "%a%a" pp_symbol s self#pp t
      | Node (s, []) -> pp_symbol formatter s
      | Node (s, args) ->
        (* general case for nodes *)
        if has_attr attr_infix s
          then begin
            match args with
            | [l;r] -> Format.fprintf formatter "@[<h>(%a %a %a)@]"
                self#pp l pp_symbol s self#pp r
            | _ -> assert false (* infix and not binary? *)
          end else Format.fprintf formatter "@[<h>%a(%a)@]" pp_symbol s
            (Utils.pp_list ~sep:", " self#pp) args);
      (* also print the sort if needed *)
      if !_sort then Format.fprintf formatter ":%a" pp_sort t.sort
    method sort s = _sort := s
  end

let pp_term_tstp =
  object (self)
    method pp formatter t =
      (* recursive printing function *)
      let rec pp_rec t = match t.term with
      | Node (s, [{term=Node (s', [a;b])}]) when s == not_symbol
        && s' == eq_symbol && a.sort == bool_ ->
        Format.fprintf formatter "(%a <~> %a)" self#pp a self#pp b
      | Node (s, [a;b]) when s == eq_symbol && a.sort == bool_ ->
        Format.fprintf formatter "(%a <=> %a)" self#pp a self#pp b
      | Node (s, [{term=Node (s', [a; b])}])
        when s == not_symbol && s' == eq_symbol ->
        Format.fprintf formatter "%a != %a" self#pp a self#pp b
      | Node (s, [t]) when s == not_symbol ->
        Format.fprintf formatter "%a%a" pp_symbol s self#pp t
      | Node (s, [v; t']) when has_attr attr_binder s ->
        assert (is_var v);
        Format.fprintf formatter "%a[%a]: %a" pp_symbol s self#pp v self#pp t'
      | BoundVar _ | Bind _ ->
        failwith "De Bruijn index in term, cannot be printed in TSTP"
      | Node (s, []) -> pp_symbol formatter s
      | Node (s, args) ->
        (* general case for nodes *)
        if has_attr attr_infix s
          then begin
            match args with
            | [l;r] -> Format.fprintf formatter "@[<h>(%a %a %a)@]"
                self#pp l pp_symbol s self#pp r
            | _ -> assert false (* infix and not binary? *)
          end else Format.fprintf formatter "@[<h>%a(%a)@]" pp_symbol s
            (Utils.pp_list ~sep:", " self#pp) args
      | Var i ->
        (match t.sort with
        | Sort _ -> Format.fprintf formatter "X%d" i
        | Fun _ -> Format.fprintf formatter "F%d" i)
      in
      let maxvar = max (max_var (vars t)) 0 in
      let varindex = ref (maxvar+1) in
      (* convert everything to named variables, then print *)
      pp_rec (db_to_classic ~varindex t)
  end

let pp_term = ref (pp_term_debug :> pprinter_term)

(** {2 JSON} *)

let rec to_json t =
  match t.term with
  | BoundVar i ->
    `List [`String "bound"; `Int i; sort_to_json t.sort]
  | Var i ->
    `List [`String "var"; `Int i; sort_to_json t.sort]
  | Bind (s, a_sort, t') ->
    `List [`String "bind"; Symbols.to_json s;
      sort_to_json t.sort; sort_to_json a_sort; to_json t']
  | Node (f, l) ->
    let l' = `List (List.map to_json l) in
    let f' = Symbols.to_json f in
    let sort' = Symbols.sort_to_json t.sort in
    `List [`String "node"; f'; sort'; l']

let of_json json =
  let rec of_json json = 
    match json with
    | `List [`String "bound"; `Int i; sort] ->
      let sort = sort_of_json sort in mk_bound_var i sort
    | `List [`String "var"; `Int i; sort] ->
      let sort = sort_of_json sort in mk_var i sort
    | `List [`String "bind"; s; sort; a_sort; t'] ->
      let s = Symbols.of_json s in
      let sort = sort_of_json sort in
      let a_sort = sort_of_json a_sort in
      let t' = of_json t' in
      mk_bind s sort a_sort t'
    | `List [`String "node"; f; sort; `List l] ->
      let f = Symbols.of_json f in
      let sort = Symbols.sort_of_json sort in
      let l = List.map of_json l in
      mk_node f sort l
    | _ -> let msg = "expected term" in
      raise (Json.Util.Type_error (msg, json))
  in
  of_json json

let varlist_to_json l =
  `List (List.map to_json l)

let varlist_of_json json =
  List.map of_json (Json.Util.to_list json)

(** {2 Skolem terms} *)

(** Prefix used for skolem symbols *)
let skolem_prefix = ref "sk"

(** Skolemize the given term at root (assumes it occurs just under an
    existential quantifier, whose De Bruijn variable is replaced
    by a fresh symbol applied to free variables). This also
    caches symbols, so that the same term is always skolemized
    the same way.

    It also refreshes the ordering (the signature has changed) *)
let classic_skolem =
  let cache = THashtbl.create 13 (* global cache for skolemized terms *)
  and count = ref 0 in  (* current symbol counter *)
  (* find an unused skolem symbol, beginning with [prefix] *)
  let rec find_skolem () = 
    let skolem = !skolem_prefix ^ (string_of_int !count) in
    incr count;
    if Symbols.is_used skolem then find_skolem () else skolem
  in
  fun ~ord t sort ->
    Utils.debug 4 "skolem %a@." !pp_term#pp t;
    let vars = vars t in
    (* find the skolemized normalized term *)
    let t'= try
      THashtbl.find cache t
    with Not_found ->
      (* actual skolemization of normalized_t *)
      let new_symbol = find_skolem () in
      let new_symbol = mk_symbol ~attrs:attr_skolem new_symbol in  (* build symbol *)
      let skolem_term = mk_node new_symbol sort vars in
      (* update the precedence *)
      ignore (ord#precedence#add_symbols [new_symbol]);
      (* build the skolemized term *)
      db_unlift (db_replace t skolem_term)
    in
    THashtbl.replace cache t t';
    (* get back to the variables of the given term *)
    Utils.debug 4 "skolem %a gives %a@." !pp_term#pp t !pp_term#pp t';
    t'

(** Skolemization with a special non-first order symbol. The purpose is
    not to introduce too many terms. A proposition p is skolemized
    into $$skolem(p), which makes naturally for inner skolemization.

    The advantage is that it does not modify the signature, and also that
    rewriting can be performed inside the skolem terms. *)
let unamed_skolem ~ord t sort =
  Utils.debug 4 "@[<h>magic skolem %a@]@." !pp_term#pp t;
  let symb = mk_symbol ~attrs:attr_skolem "$$sk" in
  (* the existential witness, parametrized by the 'quoted' formula. The
     lambda is used to keep the formula closed. *)
  let args = [mk_node lambda_symbol t.sort [t]] in
  let skolem_term = mk_node symb sort args in
  (* update the precedence *)
  ignore (ord#precedence#add_symbols [symb]);
  (* build the skolemized term by replacing first DB index with skolem symbol *)
  db_unlift (db_replace t skolem_term)

let skolem = ref classic_skolem
