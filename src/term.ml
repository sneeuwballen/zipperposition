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

let hash_term t = match t.term with
  | Var i -> Hash.hash_int i
  | BoundVar i -> Hash.hash_int i
  | Node (s, l) ->
    let h = Hash.hash_list (fun x -> x.tag) 0 l in
    let h = Hash.combine h (Symbol.hash s) in
    h
  | Bind (s, t) ->
    Hash.combine (Symbol.hash s) t.tag
  | At (t1, t2) ->
    Hash.combine t1.tag t2.tag

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

let var_occurs x t = subterm x t

let is_ground t = get_flag flag_ground t

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
  | Bind (_, t') -> db_contains t' (n+1)
  | Node (_, l) -> List.exists (fun t' -> db_contains t' n) l
  | At (t1, t2) -> db_contains t1 n || db_contains t2 n

(** replace 0 by s in t *)
let db_replace t s =
  (* replace db by s in t *)
  let rec replace depth s t = match t.term with
  | BoundVar n -> if n = depth then s else t
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
  replace 0 s t

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

(** lift the non-captured De Bruijn indexes in the term by n *)
let db_lift n t =
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
  if n = 0 then t else recurse 0 t

(* unlift the term (decrement indices of all De Bruijn variables inside *)
let db_unlift t =
  (* only unlift DB symbol that are free. [depth] is the number of binders
     on the path from the root term. *)
  let rec recurse depth t =
    match t.term with
    | BoundVar i -> if i >= depth then mk_bound_var ?ty:t.type_ (i-1) else t
    | Node (_, []) | Var _ -> t
    | Bind (s, t') ->
      mk_bind s (recurse (depth+1) t')
    | Node (s, l) ->
      mk_node s (List.map (recurse depth) l)
    | At (t1, t2) ->
      mk_at (recurse depth t1) (recurse depth t2)
  in recurse 0 t

(** Replace [t'] by a fresh De Bruijn index in [t]. *)
let db_from_term t t' =
  (* recurse and replace [t']. *)
  let rec replace depth t = match t.term with
  | _ when t == t' -> mk_bound_var ?ty:t.type_ depth
  | Var _ -> t
  | Bind (s, t') ->
    mk_bind s (replace (depth+1) t')
  | BoundVar _ -> t
  | Node (_, []) -> t
  | Node (s, l) -> mk_node s (List.map (replace depth) l)
  | At (t1, t2) -> mk_at (replace depth t1) (replace depth t2)
  in
  replace 0 t

  (** [db_from_var t v] replace v by a De Bruijn symbol in t.
    Same as db_from_term. *)
let db_from_var t v =
  assert (is_var v);
  db_from_term t v

(** {2 High-level operations} *)

(** constructors with free variables. The first argument is the
    list of variables that is bound, then the quantified/abstracted
    term. *)

let mk_lambda_var vars t =
  List.fold_right
    (fun var t ->
      mk_lambda (db_from_var t var))
    vars t

let mk_forall_var vars t =
  List.fold_right
    (fun var t ->
      assert (is_var var);
      mk_forall (db_from_var t var))
    vars t

let mk_exists_var vars t =
  List.fold_right
    (fun var t ->
      assert (is_var var);
      mk_exists (db_from_var t var))
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

(** Bind all free variables by 'forall' *)
let close_forall t =
  let vars = vars t in
  List.fold_left
    (fun t var ->
      mk_bind Symbol.forall_symbol (db_from_var t var))
    t vars

(** Bind all free variables by 'exists' *)
let close_exists t =
  let vars = vars t in
  List.fold_left
    (fun t var ->
      mk_bind Symbol.exists_symbol (db_from_var t var))
    t vars

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

(** Curry all subterms *)
let rec curry t =
  match t.term with
  | Var _ | BoundVar _ -> t
  | Bind (s, t') -> mk_bind s (curry t')
  | Node (f, []) -> t
  | Node (f, l) when Symbol.is_connective f ->
    mk_node f (List.map curry l)
  | Node (f, [t']) ->
    mk_at (mk_const f) (curry t')
  | Node (f, l) ->
    (* build the curryfied application of [f] to [l] *)
    List.fold_left
      (fun left t' -> mk_at left (curry t'))
      (mk_const f) l
  | At (t1, t2) -> mk_at (curry t1) (curry t2)  (* already curried *)

(** Uncurry all subterms *)
let uncurry t =
  (* uncurry any kind of term, except the '@' terms that are
     handled over to unfold_left *)
  let rec uncurry t =
    match t.term with
    | Var _ | BoundVar _ -> t
    | Bind (s, t') -> mk_bind s (uncurry t')
    | Node (_, []) -> t  (* constant *)
    | Node (f, l) -> mk_node f (List.map uncurry l)
    | At (t1, t2) -> unfold_left t1 [uncurry t2]
  (* transform "(((f @ a) @ b) @ c) into f(a,b,c)". Here, we
     deconstruct "f @ a" into "unfold f (a :: args)"*)
  and unfold_left head args = match head.term with
    | At (a, b) -> unfold_left a (uncurry b :: args)
    | Node (f, []) ->
      mk_node f args  (* constant symbol, ok *)
    | _ -> failwith "not a curried term"
  in
  uncurry t

let rec curryfied t =
  failwith "not implemented" (* TODO *)

let rec is_fo t = match t.term with
  | Var _ | BoundVar _ -> true
  | Bind (s, t') when Symbol.eq s Symbol.lambda_symbol -> false
  | Bind (_, t') -> is_fo t'
  | At ({term=(Var _ | BoundVar _)}, _) -> false (* X @ _ is not first-order  *)
  | At (a, b) -> is_fo a && is_fo b
  | Node (_, l) -> List.for_all is_fo l

(** Beta reduce the (curryfied) term, ie [(^[X]: t) @ t']
    becomes [subst(X -> t')(t)] *)
let rec beta_reduce t =
  match t.term with
  | Var _ | BoundVar _ -> t
  | Bind (s, t') -> mk_bind s (beta_reduce t')
  | At ({term=Bind (s, t1)}, t2) when Symbol.eq s Symbol.lambda_symbol ->
    (* a beta-redex! Fire!! *)
    let t1' = db_replace t1 t2  in
    let t1' = db_unlift t1' in
    beta_reduce t1'
  | At (t1, t2) -> mk_at (beta_reduce t1) (beta_reduce t2)
  | Node (f, l) ->
    mk_node f (List.map beta_reduce l)

(** Eta-reduce the (curryfied) term, ie [^[X]: (t @ X)]
    becomes [t] if [X] does not occur in [t]. *)
let rec eta_reduce t =
  match t.term with
  | Var _ | BoundVar _ -> t
  | Bind (s, {term=Node (a, [t'; {term=BoundVar 0}])})
    when Symbol.eq s Symbol.lambda_symbol && not (db_contains t' 0) ->
    eta_reduce (db_unlift t')  (* remove the lambda and variable *)
  | Bind (s, t') ->
    mk_bind s (eta_reduce t')
  | Node (f, l) ->
    mk_node f (List.map eta_reduce l)
  | At (t1, t2) -> mk_at (eta_reduce t1) (eta_reduce t2)

(** [lambda_abstract t sub_t], applied to a currified term [t], and a
    subterm [sub_t] of [t], gives [t'] such that
    [beta_reduce (t' @ sub_t) == t] holds.
    It basically abstracts out [sub_t] with a lambda. If [sub_t] is not
    a subterm of [t], then [t' == ^[X]: t].

    For instance (@ are omitted), [lambda_abstract f(a,g @ b,c) g] will return
    the term [^[X]: f(a, X @ b, c)] *)
let lambda_abstract t sub_t =
  mk_lambda (db_from_term t sub_t)

let lambda_abstract_list t args =
  List.fold_left lambda_abstract t args

let lambda_apply_list t args =
  let t' = List.fold_right (fun arg t -> beta_reduce (mk_at t arg)) args t in
  t'

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

let pp_tstp buf t =
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
  | Node (s, [v; t']) when Symbol.has_attr Symbol.attr_binder s ->
    assert (is_var v);
    Printf.bprintf buf "%a[%a]: %a" Symbol.pp s pp_var v pp_surrounded t'
  | BoundVar _ | Bind _ ->
    failwith "De Bruijn index in term, cannot be printed in TSTP"
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
  and pp_var buf t = match t.term with
  | Var i -> 
    begin match t.type_ with
    | Some ty when not (Type.eq ty Type.i) ->
      Printf.bprintf buf "X%d:%a" i Type.pp_tstp ty
    | _ -> Printf.bprintf buf "X%d" i
    end
  | _ -> assert false
  in
  let maxvar = max (max_var (vars t)) 0 in
  let varindex = ref (maxvar+1) in
  (* convert everything to named variables, then print *)
  pp_rec buf (db_to_classic ~varindex t)

(* lightweight printing *)
let rec pp_debug buf t =
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
  | Node (s, [v; t']) when Symbol.has_attr Symbol.attr_binder s ->
    assert (is_var v);
    Printf.bprintf buf "%a[%a]: %a" Symbol.pp s pp_var v pp_surrounded t'
  | BoundVar _ | Bind _ ->
    failwith "De Bruijn index in term, cannot be printed in debug"
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
  | _ -> pp_rec buf t
  and pp_var buf t = match t.term with
  | Var i -> 
    begin match t.type_ with
    | Some ty when not (Type.eq ty Type.i) ->
      Printf.bprintf buf "X%d:%a" i Type.pp_tstp ty
    | _ -> Printf.bprintf buf "X%d" i
    end
  | _ -> assert false
  in
  let maxvar = max (max_var (vars t)) 0 in
  let varindex = ref (maxvar+1) in
  (* convert everything to named variables, then print *)
  pp_rec buf (db_to_classic ~varindex t);
  ()

let pp = pp_debug

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
      let bij_bind () = pair Symbol.bij (bij ()) in
      let bij_node () = pair Symbol.bij (list_ (bij ())) in
      let bij_var () = pair int_ (opt Type.bij) in
      switch
        ~inject:(fun t -> match t.term with
        | BoundVar i -> 'd', BranchTo (bij_var (), (i, t.type_))
        | Var i -> 'v', BranchTo (bij_var (), (i, t.type_))
        | Bind (s, t') -> 'b', BranchTo (bij_bind (), (s, t'))
        | Node (s, l) -> 'n', BranchTo (bij_node (), (s, l))
        | At (t1, t2) -> 'a', BranchTo (pair (bij ()) (bij ()), (t1, t2)))
        ~extract:(function
        | 'd' -> BranchFrom (bij_var (), fun (i,ty) -> mk_bound_var ?ty i)
        | 'v' -> BranchFrom (bij_var (), fun (i,ty) -> mk_var ?ty i)
        | 'b' -> BranchFrom (bij_bind (), fun (s,t') -> mk_bind s t')
        | 'n' -> BranchFrom (bij_node (), fun (s,l) -> mk_node s l)
        | 'a' -> BranchFrom (pair (bij ()) (bij ()), fun (a, b) -> mk_at a b)
        | _ -> raise (DecodingError "expected Term")))
