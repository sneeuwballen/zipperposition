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

(** {1 First-order terms} *)

module PB = Position.Build

(** term *)
type t = {
  term : term_cell;             (** the term itself *)
  type_ : Type.t option;        (** optional type *)
  mutable tsize : int;          (** size (number of subterms) *)
  mutable flags : int;          (** boolean flags about the term *)
  mutable tag : int;            (** hashconsing tag *)
}
(** content of the term *)
and term_cell =
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Bind of Symbol.t * t        (** bind one variable (of given sort), with the symbol *)
  | Node of Symbol.t * t list   (** term application *)
  | At of t * t                 (** HO application (curried) *)
and sourced_term =
  t * string * string           (** Term + file,name *)

type term = t

(** list of variables *)
type varlist = t list            

let hash_term t = match t.term, t.type_ with
  | Var i, Some ty -> Hash.hash_int2 (Hash.hash_int i) (Type.hash ty)
  | BoundVar i, Some ty -> Hash.hash_int3 27 (Hash.hash_int i) (Type.hash ty)
  | (Var _ | BoundVar _), None -> assert false
  | Node (s, l), _ -> Hash.hash_list (fun x -> x.tag) (Symbol.hash s) l
  | Bind (s, t), _ -> Hash.hash_int3 13 (Symbol.hash s) t.tag
  | At (t1, t2), _ -> Hash.hash_int3 1025 t1.tag t2.tag

let rec hash_novar t = match t.term with
  | Var _ -> 42
  | BoundVar _ -> 43
  | Node (s, l) ->
    let h = Symbol.hash s in
    Hash.hash_list hash_novar h l
  | Bind (s, t') ->
    Hash.combine (Symbol.hash s) (hash_novar t')
  | At (t1,t2) ->
    Hash.combine (hash_novar t1) (hash_novar t2)

let prof_mk_node = Util.mk_profiler "Term.mk_node"

(** {2 Comparison, equality, containers} *)

let rec subterm ~sub b =
  sub == b ||
  (match b.term with
  | Var _ | BoundVar _ -> false
  | Node (_, subterms) -> List.exists (subterm ~sub) subterms
  | Bind (_, b') -> subterm ~sub b'
  | At (t1, t2) -> subterm ~sub t1 || subterm ~sub t2)

let eq x y = x == y  (* because of hashconsing *)

let compare x y = x.tag - y.tag

let hash x = x.tag

let has_type t = match t.type_ with
  | None -> false
  | Some _ -> true

let compatible_type t1 t2 = match t1.type_, t2.type_ with
  | Some ty1, Some ty2 -> Type.unifiable ty1 ty2
  | _ -> false

let same_type t1 t2 = match t1.type_, t2.type_ with
  | Some ty1, Some ty2 -> Type.alpha_equiv ty1 ty2
  | _ -> false

let compare_type t1 t2 = match t1.type_, t2.type_ with
  | Some ty1, Some ty2 -> Type.cmp ty1 ty2
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0

module TermHASH = struct
  type t = term
  let equal t1 t2 = t1 == t2
  let hash t = t.tag
end

module THashtbl = Hashtbl.Make(TermHASH)

module TSet = Sequence.Set.Make(struct
  type t = term
  let compare = compare
end)

module TMap = Sequence.Map.Make(struct
  type t = term
  let compare = compare
end)

module TCache = Cache.Replacing(TermHASH)
module T2Cache = Cache.Replacing2(TermHASH)(TermHASH)

(** {2 Hashset of terms} *)

module THashSet = struct
  type t = unit THashtbl.t
  let create ?(size=7) () = THashtbl.create size
  let cardinal t = THashtbl.length t
  let member t term = THashtbl.mem t term
  let iter set f = THashtbl.iter (fun t () -> f t) set
  let add set t = THashtbl.replace set t ()
  let remove set t = THashtbl.remove set t
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
  let rec eq_subterms a b = match a, b with
    | [], [] -> true
    | a::a1, b::b1 ->
      if a == b then eq_subterms a1 b1 else false
    | _, _ -> false
  in
  let eq_types x y = match x.type_, y.type_ with
    | None, None -> true
    | Some ty_x, Some ty_y when ty_x == ty_y -> true
    | _ -> false
  in
  (* compare types and subterms, if same structure *)
  eq_types x y &&
  match x.term, y.term with
  | Var i, Var j
  | BoundVar i, BoundVar j -> i = j
  | Node (sa, la), Node (sb, lb) -> sa == sb && eq_subterms la lb
  | Bind (sa, ta), Bind (sb, tb) ->
    sa == sb && ta == tb
  | _ -> false

(** hashconsing for terms *)
(* module H  = Hashcons.Make(struct *)
module H = Hashcons.Make(struct
  type t = term

  let equal x y = hashcons_equal x y

  let hash t = hash_term t

  let tag i t = (assert (t.tag = -1); t.tag <- i)
end)

(** {2 Boolean flags} *)

let __gen = Util.Flag.create ()
let new_flag () = Util.Flag.get_new __gen

let flag_db_closed = new_flag ()
and flag_simplified = new_flag ()
and flag_normal_form = new_flag ()
and flag_ground = new_flag ()
and flag_db_closed_computed = new_flag ()

let set_flag flag t truth =
  if truth
    then t.flags <- t.flags lor flag
    else t.flags <- t.flags land (lnot flag)

let get_flag flag t = (t.flags land flag) != 0

(** {2 Typing} *)

let cast t ty =
  H.hashcons { t with type_ = Some ty; tag = ~-1; }

let is_bool t = match t.type_ with
  | Some ty when Type.eq ty Type.o -> true
  | _ -> false

let arity t = match t.term with
  | Node (_, l) -> List.length l
  | At (_, _) -> 1
  | _ -> 0

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. Some of them
    accept a [?old] optional argument. This argument is an already existing
    term that the caller believes is likely to be equal to the result.
    This makes hashconsing faster if the result is equal to [old]. *)

let mk_var ?(ty=Type.i) idx =
  assert (idx >= 0);
  let my_v = {term = Var idx; type_= Some ty; tsize = 1;
              flags=(flag_db_closed lor flag_db_closed_computed lor
                     flag_simplified lor flag_normal_form);
              tag= -1} in
  H.hashcons my_v

let mk_bound_var ?(ty=Type.i) idx =
  assert (idx >= 0);
  let my_v = {term = BoundVar idx; type_=Some ty; tsize = 1;
              flags=(flag_db_closed_computed lor flag_simplified lor flag_normal_form);
              tag= -1} in
  H.hashcons my_v

let rec compute_is_ground l = match l with
  | [] -> true
  | x::l' -> (get_flag flag_ground x) && compute_is_ground l'

let rec compute_tsize l = match l with
  | [] -> 1  (* with the initial symbol! *)
  | x::l' -> x.tsize + compute_tsize l'

let mk_bind s t' =
  assert (Symbol.has_attr Symbol.attr_binder s);
  let my_t = {term=Bind (s, t'); type_=None; flags=0; tsize=0; tag= -1} in
  let t = H.hashcons my_t in
  (if t == my_t
    then begin (* compute ground-ness of term *)
      set_flag flag_ground t (get_flag flag_ground t');
      t.tsize <- t'.tsize + 1;
    end);
  t

let mk_node s l =
  Util.enter_prof prof_mk_node;
  let my_t = {term=Node (s, l); type_ = None; flags=0; tsize=0; tag= -1} in
  let t = H.hashcons my_t in
  (if t == my_t
    then begin
      (* compute ground-ness of term *)
      set_flag flag_ground t (compute_is_ground l);
      t.tsize <- compute_tsize l;
    end);
  Util.exit_prof prof_mk_node;
  t

let mk_at t1 t2 =
  let my_t = {term=At (t1,t2); type_=None; tsize=0; flags=0; tag= -1} in
  let t = H.hashcons my_t in
  (if t == my_t
    then begin
      (* compute ground-ness of term *)
      let is_ground = get_flag flag_ground t1 && get_flag flag_ground t2 in
      set_flag flag_ground t is_ground;
      t.tsize <- t1.tsize + t2.tsize;
    end);
  t

let mk_at_list t l =
  List.fold_left mk_at t l

let mk_const s = mk_node s []

let true_term = cast (mk_const Symbol.true_symbol) Type.o
let false_term = cast (mk_const Symbol.false_symbol) Type.o

(* constructors for terms *)

let mk_not t = mk_node Symbol.not_symbol [t]
let mk_and a b = mk_node Symbol.and_symbol [a; b]
let mk_or a b = mk_node Symbol.or_symbol [a; b]
let mk_imply a b = mk_node Symbol.imply_symbol [a; b]
let mk_equiv a b = mk_node Symbol.equiv_symbol [a; b]
let mk_xor a b = mk_not (mk_equiv a b)
let mk_eq a b = mk_node Symbol.eq_symbol [a; b]
let mk_neq a b = mk_not (mk_eq a b)
let mk_lambda t = mk_bind Symbol.lambda_symbol t
let mk_forall t = mk_bind Symbol.forall_symbol t
let mk_exists t = mk_bind Symbol.exists_symbol t

let rec mk_and_list l = match l with
  | [] -> true_term
  | [x] -> x
  | x::l' -> mk_and x (mk_and_list l')

let rec mk_or_list l = match l with
  | [] -> true_term
  | [x] -> x
  | x::l' -> mk_or x (mk_or_list l')

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

let is_at t = match t.term with
  | At _ -> true
  | _ -> false

let is_node t = match t.term with
  | Node _ -> true
  | _ -> false

let rec at_pos t pos = match t.term, pos with
  | _, [] -> t
  | Var _, _::_ -> invalid_arg "wrong position in term"
  | Node (_, l), i::subpos when i < List.length l ->
    at_pos (List.nth l i) subpos
  | Bind (_, t'), 0::subpos -> at_pos t' subpos
  | At (t1, _), 0::subpos -> at_pos t1 subpos
  | At (_, t2), 1::subpos -> at_pos t2 subpos
  | _ -> invalid_arg "index too high for subterm"

let rec replace_pos t pos new_t = match t.term, pos with
  | _, [] -> new_t
  | Var _, _::_ -> invalid_arg "wrong position in term"
  | Node (s, l), i::subpos when i < List.length l ->
    let new_subterm = replace_pos (Util.list_get l i) subpos new_t in
    mk_node s (Util.list_set l i new_subterm)
  | Bind (_, t'), 0::subpos -> replace_pos t' subpos new_t
  | At (t1, _), 0::subpos -> replace_pos t1 subpos new_t
  | At (_, t2), 1::subpos -> replace_pos t2 subpos new_t
  | _ -> invalid_arg "index too high for subterm"

(** [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let rec replace t ~old ~by = match t.term with
  | _ when t == old -> by
  | Var _ | BoundVar _ -> t
  | Bind (s, t') ->
    mk_bind s (replace t' ~old ~by)
  | Node (s, l) ->
    let l' = List.map (fun t' -> replace t' ~old ~by) l in
    mk_node s l'
  | At (t1, t2) ->
    mk_at (replace t1 ~old ~by) (replace t2 ~old ~by)

(** Size of the term (number of subterms) *)
let size t = t.tsize

(** get subterm by its position *)
let at_cpos t pos = 
  let rec recurse t pos =
    match t.term, pos with
    | _, 0 -> t
    | Node (_, l), _ -> get_subpos l (pos - 1)
    | Bind (_, t'), _ -> recurse t' (pos-1)
    | At (t1, t2), _ ->
      let s1 = size t1 in
      if s1 > pos then recurse t1 pos else recurse t2 (pos - s1)
    | _ -> assert false
  and get_subpos l pos =
    match l, pos with
    | t::l', _ when size t > pos -> recurse t pos  (* search inside the term *)
    | t::l', _ -> get_subpos l' (pos - size t) (* continue to next term *)
    | [], _ -> assert false
  in recurse t pos

let max_cpos t = size t - 1

let is_ground t = get_flag flag_ground t

let rec var_occurs x t = match t.term with
  | Var _
  | BoundVar _ -> x == t
  | Bind (_, t') -> var_occurs x t'
  | _ when is_ground t -> false  (* no variable *)
  | At (t1, t2) -> var_occurs x t1 || var_occurs x t2
  | Node (s, []) -> false
  | Node (s, l) -> List.exists (var_occurs x) l

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
  | Bind (_, t') -> add set t'
  | Node (_, l) -> add_list set l
  | At (t1, t2) -> add set t1; add set t2
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

let vars_prefix_order t =
  let rec traverse acc t = match t.term with
  | BoundVar _ -> acc
  | Var _ when List.memq t acc -> acc
  | Var _ -> t :: acc
  | Node (_, l) -> List.fold_left traverse acc l
  | Bind (_, t') -> traverse acc t'
  | At (t1, t2) -> traverse (traverse acc t1) t2
  in List.rev (traverse [] t)

(** depth of term *)
let depth t =
  let rec depth t = match t.term with
  | Var _ | BoundVar _ -> 1
  | Bind (_, t') -> 1 + depth t'
  | Node (_, l) -> 1 + depth_list 0 l
  | At (t1, t2) -> max (depth t1) (depth t2)
  and depth_list m l = match l with
  | [] -> m
  | t::l' -> depth_list (max m (depth t)) l'
  in depth t

let rec head t = match t.term with
  | Bind (s, _) -> s
  | Node (s, _) -> s
  | At (t1, _) -> head t1
  | BoundVar _
  | Var _ -> raise (Invalid_argument "Term.head")

(** {2 De Bruijn indexes} *)

(** check whether the term is a term or an atomic proposition *)
let rec atomic t =
  let open Symbol in
  match t.term with
  | Var _ | BoundVar _ -> true
  | Bind (s, t') -> not (eq s forall_symbol || eq s exists_symbol || not (atomic t'))
  | Node (s, l) -> not (eq s and_symbol || eq s or_symbol
    || eq s imply_symbol || eq s not_symbol || eq s eq_symbol || eq s equiv_symbol)
  | At (t1, t2) -> true

(** check whether the term contains connectives or quantifiers *)
let rec atomic_rec t =
  let open Symbol in
  match t.term with
  | Var _ | BoundVar _ -> true
  | Bind (s, t') -> not (s == forall_symbol || s == exists_symbol || not (atomic_rec t'))
  | Node (s, l) ->
    not (eq s and_symbol || eq s or_symbol || eq s imply_symbol
      || eq s not_symbol || eq s eq_symbol || eq s equiv_symbol)
    && List.for_all atomic_rec l
  | At (t1, t2) -> atomic_rec t1 && atomic_rec t2

(* compute whether the term is closed w.r.t. De Bruijn (bound) variables *)
let compute_db_closed depth t =
  let rec recurse depth t = match t.term with
  | BoundVar i -> i < depth
  | Bind (s, t') -> recurse (depth+1) t'
  | Var _ -> true
  | Node (_, l) -> recurse_list depth l
  | At (t1, t2) -> recurse depth t1 && recurse depth t2
  and recurse_list depth l = match l with
  | [] -> true
  | x::l' -> recurse depth x && recurse_list depth l'
  in
  recurse depth t

(** check wether the term is closed w.r.t. De Bruijn variables *)
let db_closed ?(depth=0) t =
  match depth with
  | 0 ->
    (* compute it, if not already computed *)
    (if not (get_flag flag_db_closed_computed t) then begin
      set_flag flag_db_closed_computed t true;
      set_flag flag_db_closed t (compute_db_closed 0 t);
      end);
    get_flag flag_db_closed t
  | _ -> compute_db_closed depth t

(** check whether t contains the De Bruijn symbol n *)
let rec db_contains t n = match t.term with
  | BoundVar i -> i = n
  | Var _ -> false
  | Bind (_, t') -> db_contains t' (n+1)
  | Node (_, l) -> List.exists (fun t' -> db_contains t' n) l
  | At (t1, t2) -> db_contains t1 n || db_contains t2 n

(** lift the non-captured De Bruijn indexes in the term by n *)
let db_lift ?(depth=0) n t =
  (* traverse the term, looking for non-captured DB indexes.
     [depth] is the number of binders on the path from the root of the
     term, to the current position. *)
  let rec recurse depth t = 
    match t.term with
    | _ when db_closed t -> t  (* closed. *)
    | BoundVar i when i >= depth ->
      mk_bound_var ?ty:t.type_ (i+n) (* lift by n, term not captured *)
    | Var _ | BoundVar _ -> t
    | Bind (s, t') ->
      mk_bind s (recurse (depth+1) t')  (* increase depth and recurse *)
    | Node (_, []) -> t
    | Node (s, l) ->
      let l' = List.map (recurse depth) l in
      mk_node s l'  (* recurse in subterms *)
    | At (t1, t2) -> mk_at (recurse depth t1) (recurse depth t2)
  in
  assert (n >= 0);
  if depth=0 && n = 0 then t else recurse depth t

(** replace 0 by [by] into [into] *)
let db_replace ?(depth=0) ~into ~by =
  (* replace db by s in t *)
  let rec replace depth s t = match t.term with
  | BoundVar n ->
    if n = depth
      then db_lift depth s   (* free vars must be lifted *)
      else t
  | Var _ -> t
  | Bind (symb, t') ->
    (* lift the De Bruijn to replace *)
    mk_bind symb (replace (depth+1) s t')
  | Node (_, []) -> t
  | Node (f, l) ->
    mk_node f (List.map (replace depth s) l)
  | At (t1, t2) -> mk_at (replace depth s t1) (replace depth s t2)
  (* replace the 0 De Bruijn index by s in t *)
  in
  replace depth by into

(** Type of the [n]-th De Bruijn index in [t] *)
let rec db_type t n = match t.term with
  | BoundVar i when i = n -> t.type_
  | BoundVar _
  | Var _ -> None
  | At (t1, t2) ->
    begin match db_type t1 n with
    | Some ty -> Some ty
    | None -> db_type t2 n
    end
  | Node (_, l) ->
    List.fold_left
      (fun acc t' -> match acc with
        | Some _ -> acc
        | None -> db_type t' n)
      None l
  | Bind (_, t') -> db_type t' (n+1)

(* unlift the term (decrement indices of all free De Bruijn variables inside *)
let db_unlift ?(depth=0) t =
  (* only unlift DB symbol that are free. [depth] is the number of binders
     on the path from the root term. *)
  let rec recurse depth t =
    match t.term with
    | _ when db_closed t -> t
    | BoundVar i -> if i >= depth then mk_bound_var ?ty:t.type_ (i-1) else t
    | Node (_, []) | Var _ -> t
    | Bind (s, t') ->
      mk_bind s (recurse (depth+1) t')
    | Node (s, l) ->
      mk_node s (List.map (recurse depth) l)
    | At (t1, t2) ->
      mk_at (recurse depth t1) (recurse depth t2)
  in recurse depth t

(** Replace [t'] by a fresh De Bruijn index in [t]. *)
let db_from_term ?(depth=0) ?(ty=Type.i) t t' =
  (* recurse and replace [t']. *)
  let rec replace depth t = match t.term with
  | _ when t == t' -> mk_bound_var ~ty depth
  | Var _ -> t
  | Bind (s, t') -> mk_bind s (replace (depth+1) t')
  | BoundVar _ -> t
  | Node (_, []) -> t
  | Node (s, l) -> mk_node s (List.map (replace depth) l)
  | At (t1, t2) -> mk_at (replace depth t1) (replace depth t2)
  in
  replace depth t

(** [db_from_var t v] replace v by a De Bruijn symbol in t.
  Same as db_from_term. *)
let db_from_var ?depth t v =
  assert (is_var v);
  db_from_term ?depth t v

(** {2 High-level operations} *)

(** constructors with free variables. The first argument is the
    list of variables that is bound, then the quantified/abstracted
    term. *)

let mk_lambda_var vars t =
  List.fold_right
    (fun var t ->
      mk_lambda (db_from_var (db_lift 1 t) var))
    vars t

let mk_forall_var vars t =
  List.fold_right
    (fun var t ->
      assert (is_var var);
      mk_forall (db_from_var (db_lift 1 t) var))
    vars t

let mk_exists_var vars t =
  List.fold_right
    (fun var t ->
      assert (is_var var);
      mk_exists (db_from_var (db_lift 1 t) var))
    vars t

let symbols seq =
  let rec symbols set t = match t.term with
    | Var _
    | BoundVar _ -> set
    | Node (s, l) ->
      let set = Symbol.SSet.add s set in
      List.fold_left symbols set l
    | Bind (s, t') ->
      let set = Symbol.SSet.add s set in
      symbols set t'
    | At (t1, t2) -> symbols (symbols set t1) t2
  in
  Sequence.fold symbols Symbol.SSet.empty seq

(** Does t contains the symbol f? *)
let rec contains_symbol f t =
  match t.term with
  | Var _
  | BoundVar _ -> false
  | Bind (s, t') -> s == f || contains_symbol f t'
  | Node (g, ts) -> g == f || List.exists (contains_symbol f) ts
  | At (t1, t2) -> contains_symbol f t1 || contains_symbol f t2

(** Bind all free variables by 'forall' *)
let close_forall t =
  let vars = vars t in
  mk_forall_var vars t

(** Bind all free variables by 'exists' *)
let close_exists t =
  let vars = vars t in
  mk_exists_var vars t

(** Transform binders and De Bruijn indexes into regular variables.
    [varindex] is a variable counter used to give fresh variables
    names to De Bruijn indexes. *)
let rec db_to_classic ?(varindex=ref 0) t =
  match t.term with
  | Bind (s, t') ->
    let ty = db_type t' 0 in
    let v = mk_var ?ty !varindex in
    incr varindex;
    let new_t = mk_node s [v; db_unlift (db_replace t' v)] in
    db_to_classic ~varindex new_t
  | Node (_, []) | Var _ -> t
  | BoundVar _ ->  (* free variable *)
    let n = !varindex in
    incr varindex;
    mk_var ?ty:t.type_ n
  | Node (s, l) ->
    mk_node s (List.map (db_to_classic ~varindex) l)
  | At (t1, t2) ->
    mk_at (db_to_classic ~varindex t1) (db_to_classic ~varindex t2)

(** {2 Fold} *)

let rec _all_pos_rec f vars acc pb t = match t.term with
  | Var _ | BoundVar _ ->
    if vars then f acc t (PB.to_pos pb) else acc
  | Bind (_, t') ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    _all_pos_rec f vars acc (PB.add pb 0) t'
  | Node (hd, tl) ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    _all_pos_rec_list f vars acc pb tl 0
  | At(t1, t2) ->
    let acc = f acc t (PB.to_pos pb) in
    let acc = _all_pos_rec f vars acc (PB.add pb 0) t1 in
    let acc = _all_pos_rec f vars acc (PB.add pb 1) t2 in
  acc
and _all_pos_rec_list f vars acc pb l i = match l with
  | [] -> acc
  | t::l' ->
    let acc = _all_pos_rec f vars acc (PB.add pb i) t in
    _all_pos_rec_list f vars acc pb l' (i+1)

let all_positions ?(vars=false) ?(pos=[]) t acc f =
  _all_pos_rec f vars acc (PB.of_pos pos) t

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
let ac_normal_form ?(is_ac=fun s -> Symbol.has_attr Symbol.attr_ac s)
                   ?(is_com=fun s -> Symbol.has_attr Symbol.attr_commut s)
                   t =
  let rec normalize t = match t.term with
    | Var _ -> t
    | BoundVar _ -> t
    | Bind (s, t') -> mk_bind s (normalize t')
    | Node (f, ([_;_] as l)) when is_ac f ->
      let l = flatten_ac f l in
      let l = List.map normalize l in
      let l = List.sort compare l in
      (match l with
        | x::l' -> List.fold_left
          (fun subt x -> mk_node f [x;subt])
          x l'
        | [] -> assert false)
    | Node (f, [a;b]) when is_com f ->
      if compare a b > 0
        then mk_node f [b; a]
        else t
    | Node (f, l) ->
      let l = List.map normalize l in
      mk_node f l
    | At (t1, t2) -> mk_at (normalize t1) (normalize t2)
  in
  normalize t

(** Check whether the two terms are AC-equal. Optional arguments specify
    which symbols are AC or commutative (by default by looking at
    attr_ac and attr_commut) *)
let ac_eq ?(is_ac=fun s -> Symbol.has_attr Symbol.attr_ac s)
          ?(is_com=fun s -> Symbol.has_attr Symbol.attr_commut s)
          t1 t2 =
  let t1' = ac_normal_form ~is_ac ~is_com t1
  and t2' = ac_normal_form ~is_ac ~is_com t2 in
  t1' == t2'

(** {2 Printing/parsing} *)

let pp_tstp_depth depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t = match t.term with
  | Node (s, [{term=Node (s', [a;b])}]) when Symbol.eq s Symbol.not_symbol
    && Symbol.eq s' Symbol.equiv_symbol ->
    Printf.bprintf buf "%a <~> %a" pp_surrounded a pp_surrounded b
  | Node (s, [{term=Node (s', [a; b])}])
    when s == Symbol.not_symbol && s' == Symbol.eq_symbol ->
    Printf.bprintf buf "%a != %a" pp_surrounded a pp_surrounded b
  | Node (s, [t]) when s == Symbol.not_symbol ->
    Printf.bprintf buf "%a%a" Symbol.pp s pp_rec t
  | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
  | Bind (s,t') ->
    Printf.bprintf buf "%a[%a]: " Symbol.pp s pp_bvar t';
    incr depth;
    pp_surrounded buf t';
    decr depth
  | Node (s, [a;b]) when Symbol.has_attr Symbol.attr_infix s ->
    Printf.bprintf buf "%a %a %a" pp_surrounded a Symbol.pp s pp_surrounded b
  | Node (s, body1::((_::_) as body)) when Symbol.has_attr Symbol.attr_infix s ->
    let sep = Util.sprintf " %a " Symbol.pp s in
    Printf.bprintf buf "%a%s%a" pp_surrounded body1 sep
      (Util.pp_list ~sep pp_surrounded) body
  | Node (s, []) -> Symbol.pp buf s
  | Node (s, args) -> (* general case for nodes *)
    Printf.bprintf buf "%a(%a)" Symbol.pp s (Util.pp_list ~sep:", " pp_rec) args
  | Var i -> Printf.bprintf buf "X%d" i
  | At (t1, t2) -> pp_rec buf t1; Buffer.add_string buf " @ "; pp_rec buf t2
  and pp_surrounded buf t = match t.term with
  | Node (s, _::_::_) when Symbol.has_attr Symbol.attr_infix s ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  and pp_bvar buf t = match db_type t 0 with
  | None -> Printf.bprintf buf "Y%d" !depth
  | Some ty -> Printf.bprintf buf "Y%d: %a" !depth Type.pp ty
  in
  pp_rec buf t

(* lightweight printing *)
let rec pp_depth depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t = match t.term with
  | Node (s, [{term=Node (s', [a;b])}]) when Symbol.eq s Symbol.not_symbol
    && Symbol.eq s' Symbol.equiv_symbol ->
    Printf.bprintf buf "%a <~> %a" pp_surrounded a pp_surrounded b
  | Node (s, [{term=Node (s', [a; b])}])
    when s == Symbol.not_symbol && s' == Symbol.eq_symbol ->
    Printf.bprintf buf "%a != %a" pp_surrounded a pp_surrounded b
  | Node (s, [t]) when s == Symbol.not_symbol ->
    Printf.bprintf buf "%a%a" Symbol.pp s pp_rec t
  | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
  | Bind (s,t') ->
    Printf.bprintf buf "%a[%a]: " Symbol.pp s pp_bvar t';
    incr depth;
    pp_surrounded buf t';
    decr depth
  | Node (s, [a;b]) when Symbol.has_attr Symbol.attr_infix s ->
    Printf.bprintf buf "%a %a %a" pp_surrounded a Symbol.pp s pp_surrounded b
  | Node (s, body1::((_::_) as body)) when Symbol.has_attr Symbol.attr_infix s ->
    let sep = Util.sprintf " %a " Symbol.pp s in
    Printf.bprintf buf "%a%s%a" pp_surrounded body1 sep
      (Util.pp_list ~sep pp_surrounded) body
  | Node (s, []) -> Symbol.pp buf s
  | Node (s, args) ->
    Printf.bprintf buf "%a(%a)" Symbol.pp s (Util.pp_list ~sep:", " pp_rec) args
  | Var i -> Printf.bprintf buf "X%d" i
  | At (t1, t2) ->
    pp_rec buf t1; Buffer.add_char buf ' ';
    pp_surrounded buf t2
  and pp_surrounded buf t = match t.term with
  | Node (s, _::_::_) when Symbol.has_attr Symbol.attr_infix s ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | At _ ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  and pp_bvar buf t = match db_type t 0 with
  | None -> Printf.bprintf buf "Y%d" !depth
  | Some ty -> Printf.bprintf buf "Y%d: %a" !depth Type.pp ty
  in
  pp_rec buf t

let pp buf t = pp_depth 0 buf t

let pp_tstp buf t = pp_tstp_depth 0 buf t

let to_string t = Util.sprintf "%a" pp t

let fmt fmt t = Format.pp_print_string fmt (to_string t)

let rec debug fmt t = match t.term with
  | Var i -> Format.fprintf fmt "X%d" i
  | BoundVar i -> Format.fprintf fmt "Y%d" i
  | Bind (s, t') -> 
    Format.fprintf fmt "(%s %a)" (Symbol.to_string s) debug t'
  | Node (s, []) ->
    Format.pp_print_string fmt (Symbol.to_string s)
  | Node (s, l) ->
    Format.fprintf fmt "(%s %a)" (Symbol.to_string s)
      (Sequence.pp_seq debug) (Sequence.of_list l)
  | At (t1, t2) -> Format.fprintf fmt "%a %a" debug t1 debug t2

let bij =
  let open Bij in
  fix
    (fun bij ->
      let bij_bind = lazy (pair Symbol.bij (Lazy.force bij)) in
      let bij_node = lazy (pair Symbol.bij (list_ (Lazy.force bij))) in
      let bij_var = lazy (pair int_ (opt Type.bij)) in
      let bij_pair = lazy (pair (Lazy.force bij) (Lazy.force bij)) in
      switch
        ~inject:(fun t -> match t.term with
        | BoundVar i -> "bvar", BranchTo (Lazy.force bij_var, (i, t.type_))
        | Var i -> "var", BranchTo (Lazy.force bij_var, (i, t.type_))
        | Bind (s, t') -> "bind", BranchTo (Lazy.force bij_bind, (s, t'))
        | Node (s, l) -> "node", BranchTo (Lazy.force bij_node, (s, l))
        | At (t1, t2) -> "at", BranchTo (Lazy.force bij_pair, (t1, t2)))
        ~extract:(function
        | "bvar" -> BranchFrom (Lazy.force bij_var, fun (i,ty) -> mk_bound_var ?ty i)
        | "var" -> BranchFrom (Lazy.force bij_var, fun (i,ty) -> mk_var ?ty i)
        | "bind" -> BranchFrom (Lazy.force bij_bind, fun (s,t') -> mk_bind s t')
        | "node" -> BranchFrom (Lazy.force bij_node, fun (s,l) -> mk_node s l)
        | "at" -> BranchFrom (Lazy.force bij_pair, fun (a, b) -> mk_at a b)
        | _ -> raise (DecodingError "expected Term")))
