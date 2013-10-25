
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

(** {1 Higher Order Terms} *)

module FOT = FOTerm

(** {2 Type Definitions} *)

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
  | Bind of Symbol.t * t        (** bind one variable *)
  | Const of Symbol.t           (** Constant *)
  | At of t * t                 (** HO application (curried) *)
and sourced_term =
  t * string * string           (** Term + file,name *)

type term = t

type varlist = t list

(** {2 Basics} *)

let hash_term t = match t.term, t.type_ with
  | Var i, Some ty -> Hash.hash_int2 (Hash.hash_int i) (Type.hash ty)
  | BoundVar i, Some ty -> Hash.hash_int3 27 (Hash.hash_int i) (Type.hash ty)
  | BoundVar i, None -> Hash.hash_int2 22 (Hash.hash_int i)
  | Var _ , None -> assert false
  | Const s, _ -> Symbol.hash s
  | Bind (s, t), _ -> Hash.hash_int3 13 (Symbol.hash s) t.tag
  | At (t1, t2), _ -> Hash.hash_int3 1025 t1.tag t2.tag

let rec hash_novar t = match t.term with
  | Var _ -> 42
  | BoundVar _ -> 43
  | Const s -> Symbol.hash s
  | Bind (s, t') ->
    Hash.combine (Symbol.hash s) (hash_novar t')
  | At (t1,t2) ->
    Hash.combine (hash_novar t1) (hash_novar t2)

(** {2 Comparison, equality, containers} *)

let rec subterm ~sub b =
  sub == b ||
  match b.term with
  | Var _ | BoundVar _ | Const _ -> false
  | Bind (_, b') -> subterm ~sub b'
  | At (t1, t2) -> subterm ~sub t1 || subterm ~sub t2

let eq x y = x == y  (* because of hashconsing *)

let compare x y = x.tag - y.tag

let hash x = x.tag

let compare_type t1 t2 = match t1.type_, t2.type_ with
  | Some ty1, Some ty2 -> Type.cmp ty1 ty2
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0

(* get the type of this term. The term MUST be a variable or bind,
  in which case it must have a type. *)
let get_type t = match t.term, t.type_ with
  | Var _, Some ty -> ty
  | Bind _, Some ty -> ty
  | (Var _ | Bind _), None -> assert false
  | At _, _ -> failwith "HOTerm.At is not typed"
  | Const _, _ -> failwith "HOTerm.Const is not typed"
  | BoundVar _, _ -> failwith "HOTerm.BoundVar is not typed"

module TermHASH = struct
  type t = term
  let equal = eq
  let hash = hash
end

module Tbl = Hashtbl.Make(TermHASH)

module Set = Sequence.Set.Make(struct
  type t = term
  let compare = compare
end)

module Map = Sequence.Map.Make(struct
  type t = term
  let compare = compare
end)

module Cache = Cache.Replacing(TermHASH)

(** {2 Global terms table (hashconsing)} *)

let hashcons_equal x y =
  let eq_types x y = match x.type_, y.type_ with
    | None, None -> true
    | Some ty_x, Some ty_y when Type.eq ty_x ty_y -> true
    | _ -> false
  in
  (* compare types and subterms, if same structure *)
  match x.term, y.term with
  | Var i, Var j
  | BoundVar i, BoundVar j -> i = j && eq_types x y
  | Const s1, Const s2 -> Symbol.eq s1 s2
  | Bind (sa, ta), Bind (sb, tb) -> Symbol.eq sa sb && ta == tb
  | At (ta1, ta2), At (tb1, tb2) -> ta1 == tb1 && ta2 == tb2
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

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags) *)

let mk_var ~ty idx =
  assert (idx >= 0);
  if not (Type.is_instantiated ty)
    then failwith "HOT.mk_var: needs instantiated type";
  let my_v = {term = Var idx; type_= Some ty; tsize = 1;
              flags=(flag_db_closed lor flag_db_closed_computed lor
                     flag_normal_form);
              tag= -1} in
  H.hashcons my_v

let mk_bound_var idx =
  assert (idx >= 0);
  let my_v = {term = BoundVar idx; type_=None; tsize = 1;
              flags=(flag_db_closed_computed lor flag_normal_form);
              tag= -1} in
  H.hashcons my_v

let mk_bind s ~ty t' =
  assert (Symbol.has_attr Symbol.attr_binder s);
  if not (Type.is_instantiated ty)
    then failwith "HOT.mk_bind: needs instantiated type";
  let my_t = {term=Bind (s, t'); type_=Some ty; flags=0; tsize=0; tag= -1} in
  let t = H.hashcons my_t in
  (if t == my_t
    then begin (* compute ground-ness of term *)
      set_flag flag_ground t (get_flag flag_ground t');
      t.tsize <- t'.tsize + 1;
    end);
  t

let mk_const s =
  let my_t = {term=Const s; type_=None;
              flags=(flag_db_closed lor flag_db_closed_computed lor flag_normal_form);
              tsize=1; tag= -1;} in
  let t = H.hashcons my_t in
  t

let mk_at t1 t2 =
  let my_t = {term=At (t1,t2); type_=None; tsize=0; flags=0; tag= -1} in
  let t = H.hashcons my_t in
  (if t == my_t
    then begin
      (* compute ground-ness of term *)
      let is_ground = get_flag flag_ground t1 && get_flag flag_ground t2 in
      set_flag flag_ground t is_ground;
      t.tsize <- t1.tsize + t2.tsize + 1;
    end);
  t

let mk_at_list t l =
  List.fold_left mk_at t l

let true_term = mk_const Symbol.true_symbol
let false_term = mk_const Symbol.false_symbol

(** Easy constructors for formulas *)

let not_term = mk_const Symbol.not_symbol
let and_term = mk_const Symbol.and_symbol
let or_term = mk_const Symbol.or_symbol
let imply_term = mk_const Symbol.imply_symbol
let equiv_term = mk_const Symbol.equiv_symbol
let eq_term = mk_const Symbol.eq_symbol


let mk_not t = mk_at not_term t
let mk_and a b = mk_at_list and_term [a; b]
let mk_or a b = mk_at_list or_term [a; b]
let mk_imply a b = mk_at_list imply_term [a; b]
let mk_equiv a b = mk_at_list equiv_term [a; b]
let mk_xor a b = mk_not (mk_equiv a b)
let mk_eq a b = mk_at_list eq_term [a; b]
let mk_neq a b = mk_not (mk_eq a b)
let mk_lambda ~ty t = mk_bind Symbol.lambda_symbol ~ty t
let mk_forall ~ty t = mk_bind Symbol.forall_symbol ~ty t
let mk_exists ~ty t = mk_bind Symbol.exists_symbol ~ty t

let rec mk_and_list l = match l with
  | [] -> true_term
  | [x] -> x
  | x::l' -> mk_and x (mk_and_list l')

let rec mk_or_list l = match l with
  | [] -> false_term
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
  | Const _ -> true
  | _ -> false

let is_at t = match t.term with
  | At _ -> true
  | _ -> false

let rec at_pos t pos = match t.term, pos with
  | _, [] -> t
  | Var _, _::_ -> invalid_arg "wrong position in term"
  | Bind (_, t'), 0::subpos -> at_pos t' subpos
  | At (t1, _), 0::subpos -> at_pos t1 subpos
  | At (_, t2), 1::subpos -> at_pos t2 subpos
  | _ -> invalid_arg "index too high for subterm"

let rec replace_pos t pos new_t = match t.term, pos with
  | _, [] -> new_t
  | (Var _ | BoundVar _), _::_ -> invalid_arg "wrong position in term"
  | Bind (_, t'), 0::subpos -> replace_pos t' subpos new_t
  | At (t1, _), 0::subpos -> replace_pos t1 subpos new_t
  | At (_, t2), 1::subpos -> replace_pos t2 subpos new_t
  | _ -> invalid_arg "index too high for subterm"

(** [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let rec replace t ~old ~by = match t.term with
  | _ when t == old -> by
  | Var _ | BoundVar _ | Const _ -> t
  | Bind (s, t') ->
    let ty = get_type t in
    mk_bind ~ty s (replace t' ~old ~by)
  | At (t1, t2) ->
    mk_at (replace t1 ~old ~by) (replace t2 ~old ~by)

(** Size of the term (number of subterms) *)
let size t = t.tsize

(** get subterm by its position *)
let rec at_cpos t pos = match t.term, pos with
  | _, 0 -> t
  | Bind (_, t'), _ -> at_cpos t' (pos-1)
  | At (t1, t2), _ ->
    let s1 = size t1 in
    if s1 > pos then at_cpos t1 pos else at_cpos t2 (pos - s1)
  | _ -> assert false

let max_cpos t = size t - 1

let is_ground t = get_flag flag_ground t

let rec var_occurs x t = match t.term with
  | Var _
  | BoundVar _ -> x == t
  | Bind (_, t') -> var_occurs x t'
  | _ when is_ground t -> false  (* no variable *)
  | At (t1, t2) -> var_occurs x t1 || var_occurs x t2
  | Const s -> false

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
  aux 0 vars

(** add variables of the term to the set *)
let rec add_vars set t = match t.term with
| Var _ -> Tbl.replace set t ()
| Const _
| BoundVar _ -> ()
| Bind (_, t') -> add_vars set t'
| At (t1, t2) -> add_vars set t1; add_vars set t2

(** compute variables of the term *)
let vars t =
  let set = Tbl.create 5 in
  add_vars set t;
  Tbl.fold (fun t () acc -> t :: acc) set []

(** Compute variables of terms in the list *)
let vars_list l =
  let set = Tbl.create 5 in
  List.iter (add_vars set) l;
  Tbl.fold (fun t () acc -> t :: acc) set []

(** Compute variables of terms in the sequence *)
let vars_seq seq =
  let set = Tbl.create 5 in
  Sequence.iter (add_vars set) seq;
  Tbl.fold (fun t () acc -> t :: acc) set []

let vars_prefix_order t =
  let rec traverse acc t = match t.term with
  | Const _
  | BoundVar _ -> acc
  | Var _ when List.memq t acc -> acc
  | Var _ -> t :: acc
  | Bind (_, t') -> traverse acc t'
  | At (t1, t2) -> traverse (traverse acc t1) t2
  in List.rev (traverse [] t)

(** depth of term *)
let rec depth t = match t.term with
  | Var _ | BoundVar _ | Const _ -> 1
  | Bind (_, t') -> 1 + depth t'
  | At (t1, t2) -> max (depth t1) (depth t2)

let rec head t = match t.term with
  | Bind (s, _) -> s
  | Const s -> s
  | At (t1, _) -> head t1
  | BoundVar _
  | Var _ -> raise (Invalid_argument "Term.head: variable")

let open_at t =
  let rec recurse acc t = match t.term with
  | Var _ | BoundVar _ | Const _ | Bind _ -> t, acc
  | At (t1, t2) -> recurse (t2 :: acc) t1
  in
  recurse [] t

let symbols seq =
  let rec symbols set t = match t.term with
    | Var _
    | BoundVar _ -> set
    | Const s -> Symbol.SSet.add s set
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
  | Const s -> Symbol.eq s f
  | Bind (s, t') -> Symbol.eq s f || contains_symbol f t'
  | At (t1, t2) -> contains_symbol f t1 || contains_symbol f t2

(** [flatten_ac f l] flattens the list of terms [l] by deconstructing all its
    elements that have [f] as head symbol. For instance, if l=[1+2; 3+(4+5)]
    with f="+", this will return [1;2;3;4;5], perhaps in a different order *)
let flatten_ac f l =
  let rec flatten acc l = match l with
  | [] -> acc
  | x::l' -> flatten (deconstruct acc x) l'
  and deconstruct acc t = match open_at t with
  | {term=Const f'}, l' when Symbol.eq f f' ->
    flatten acc l'
  | _ -> t::acc
  in flatten [] l

(** {2 De Bruijn indexes} *)

(** check whether the term is a term or an atomic proposition *)
let rec atomic t =
  match t.term with
  | Var _ | BoundVar _ -> true
  | Bind (s, t') ->
    not Symbol.(eq s forall_symbol || eq s exists_symbol || not (atomic t'))
  | Const s ->
    not Symbol.(eq s and_symbol || eq s or_symbol || eq s imply_symbol || eq s
      not_symbol || eq s eq_symbol || eq s equiv_symbol)
  | At (t1, t2) -> atomic t1 && atomic t2

(* compute whether the term is closed w.r.t. De Bruijn (bound) variables *)
let rec compute_db_closed depth t = match t.term with
  | BoundVar i -> i < depth
  | Bind (s, t') -> compute_db_closed (depth+1) t'
  | Const _
  | Var _ -> true
  | At (t1, t2) -> compute_db_closed depth t1 && compute_db_closed depth t2

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
  | Const _
  | Var _ -> false
  | Bind (_, t') -> db_contains t' (n+1)
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
      mk_bound_var (i+n) (* lift by n, term not captured *)
    | Var _ | BoundVar _ | Const _ -> t
    | Bind (s, t') ->
      let ty = get_type t in
      mk_bind ~ty s (recurse (depth+1) t')  (* increase depth and recurse *)
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
  | Const _
  | Var _ -> t
  | Bind (symb, t') ->
    (* lift the De Bruijn to replace *)
    let ty = get_type t in
    mk_bind symb ~ty (replace (depth+1) s t')
  | At (t1, t2) -> mk_at (replace depth s t1) (replace depth s t2)
  in
  replace depth by into

(* unlift the term (decrement indices of all free De Bruijn variables inside *)
let db_unlift ?(depth=0) t =
  (* only unlift DB symbol that are free. [depth] is the number of binders
     on the path from the root term. *)
  let rec recurse depth t =
    match t.term with
    | _ when db_closed t -> t
    | BoundVar i -> if i >= depth then mk_bound_var (i-1) else t
    | Const _ | Var _ -> t
    | Bind (s, t') ->
      let ty = get_type t in
      mk_bind ~ty s (recurse (depth+1) t')
    | At (t1, t2) ->
      mk_at (recurse depth t1) (recurse depth t2)
  in recurse depth t

(** Replace [t'] by a fresh De Bruijn index in [t]. *)
let db_from_term ?(depth=0) t t' =
  (* recurse and replace [t']. *)
  let rec replace depth t = match t.term with
  | _ when t == t' -> mk_bound_var depth
  | Bind (s, t') ->
    let ty = get_type t in
    mk_bind ~ty s (replace (depth+1) t')
  | Var _
  | Const _
  | BoundVar _ -> t
  | At (t1, t2) -> mk_at (replace depth t1) (replace depth t2)
  in
  replace depth t

(** [db_from_var t v] replace v by a De Bruijn symbol in t.
  Same as db_from_term. *)
let db_from_var ?depth t v =
  assert (is_var v);
  db_from_term ?depth t v

(** {2 High-level operations} *)

(** Transform binders and De Bruijn indexes into regular variables.
    [varindex] is a variable counter used to give fresh variables
    names to De Bruijn indexes. *)
let rec db_to_classic ?(varindex=ref 0) t =
  match t.term with
  | Bind (s, t') ->
    let ty = get_type t in
    let v = mk_var ~ty !varindex in
    incr varindex;
    let new_t = mk_at_list (mk_const s) [v; db_unlift (db_replace t' v)] in
    db_to_classic ~varindex new_t
  | Const _ | Var _ -> t
  | BoundVar _ ->  (* free variable *)
    failwith "HOTerm.db_to_classic: free De Bruijn index"
  | At (t1, t2) ->
    mk_at (db_to_classic ~varindex t1) (db_to_classic ~varindex t2)

(** constructors with free variables. The first argument is the
    list of variables that is bound, then the quantified/abstracted
    term. *)

let mk_lambda_var vars t =
  List.fold_right
    (fun var t ->
      assert (is_var var);
      let ty = get_type var in
      mk_lambda ~ty (db_from_var (db_lift 1 t) var))
    vars t

let mk_forall_var vars t =
  List.fold_right
    (fun var t ->
      assert (is_var var);
      let ty = get_type var in
      mk_forall ~ty (db_from_var (db_lift 1 t) var))
    vars t

let mk_exists_var vars t =
  List.fold_right
    (fun var t ->
      assert (is_var var);
      let ty = get_type var in
      mk_exists ~ty (db_from_var (db_lift 1 t) var))
    vars t

(** Bind all free variables by 'forall' *)
let close_forall t =
  let vars = vars t in
  mk_forall_var vars t

(** Bind all free variables by 'exists' *)
let close_exists t =
  let vars = vars t in
  mk_exists_var vars t


(** {2 High level operations} *)

(* Curry all subterms *)
let rec curry t =
  match t.FOT.term with
  | FOT.Var i ->
    let ty = match t.FOT.type_ with
    | None -> failwith "FOTerm.Var must have a type"
    | Some ty -> Type.curry ty
    in
    mk_var ~ty i
  | FOT.BoundVar i -> mk_bound_var i
  | FOT.Node (f, []) -> mk_const f
  | FOT.Node (f, l) ->
    mk_at_list (mk_const f) (List.map curry l)

let uncurry t =
  (* uncurry any kind of term, except the '@' terms that are
     handled over to unfold_left *)
  let rec uncurry t =
    match t.term with
    | Var i ->
      let ty = Type.uncurry (get_type t) in
      FOT.mk_var ~ty i
    | BoundVar i -> FOT.mk_bound_var i
    | Bind (s, t') -> failwith "cannot uncurry binder"
    | Const s -> FOT.mk_const s
    | At (t1, t2) -> unfold_left t1 [uncurry t2]
  (* transform "(((f @ a) @ b) @ c) into f(a,b,c)". Here, we
     deconstruct "f @ a" into "unfold f (a :: args)"*)
  and unfold_left head args = match head.term with
    | At (a, b) -> unfold_left a (uncurry b :: args)
    | Const f -> FOT.mk_node f args (* constant symbol, ok *)
    | _ -> failwith "cannot uncurry term"
  in
  uncurry t

let rec is_fo t = match t.term with
  | Var _ -> true
  | BoundVar _ -> false
  | Bind _ -> false
  | At ({term=(Var _ | BoundVar _)}, _) -> false (* X @ _ is not first-order  *)
  | At (a, b) -> is_fo a && is_fo b
  | Const _ -> true

(** {2 IO} *)

let pp_tstp_depth depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t = match t.term with
  | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
  | Bind (s,t') ->
    let ty = get_type t in
    if Type.eq ty Type.i
      then Printf.bprintf buf "%a[%a]: " Symbol.pp s pp_bvar ()
      else Printf.bprintf buf "%a[%a:%a]: " Symbol.pp s pp_bvar () Type.pp ty;
    incr depth;
    pp_surrounded buf t';
    decr depth
  | Const s -> Symbol.pp buf s
  | Var i -> Printf.bprintf buf "X%d" i
  | At (t1, t2) -> pp_rec buf t1; Buffer.add_string buf " @ "; pp_surrounded buf t2
  and pp_surrounded buf t = match t.term with
  | At (_, _) ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  and pp_bvar buf () =  Printf.bprintf buf "Y%d" !depth in
  pp_rec buf t

(* lightweight printing *)
let rec pp_depth depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t = match t.term with
  | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
  | Bind (s, t') ->
    let ty = get_type t in
    if Type.eq ty Type.i
      then Printf.bprintf buf "%a[%a]: " Symbol.pp s pp_bvar ()
      else Printf.bprintf buf "%a[%a:%a]: " Symbol.pp s Type.pp ty pp_bvar ();
    incr depth;
    pp_surrounded buf t';
    decr depth
  | Const s -> Symbol.pp buf s
  | Var i ->
    let ty = get_type t in
    if Type.eq ty Type.i
      then Printf.bprintf buf "X%d" i
      else Printf.bprintf buf "X%d:%a" i Type.pp ty
  | At (t1, t2) ->
    pp_rec buf t1; Buffer.add_char buf ' ';
    pp_surrounded buf t2
  and pp_surrounded buf t = match t.term with
  | At _ ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  and pp_bvar buf () = Printf.bprintf buf "Y%d" !depth in
  pp_rec buf t

let pp_debug buf t = pp_depth 0 buf t

let pp_tstp buf t = pp_tstp_depth 0 buf t

let __default_pp = ref pp_debug

let pp buf t = !__default_pp buf t

let set_default_pp pp = __default_pp := pp

let to_string t = Util.sprintf "%a" pp t

let fmt fmt t = Format.pp_print_string fmt (to_string t)

let rec debug fmt t = match t.term with
  | Var i ->
    let ty = get_type t in
    Format.fprintf fmt "X%d:%a" i Type.fmt ty
  | BoundVar i -> Format.fprintf fmt "Y%d" i
  | Bind (s, t') -> 
    let ty = get_type t in
    Format.fprintf fmt "(%s %a %a)" (Symbol.to_string s) Type.fmt ty debug t'
  | Const s -> Symbol.fmt fmt s
  | At (t1, t2) -> Format.fprintf fmt "%a %a" debug t1 debug t2

let bij =
  let open Bij in
  fix
    (fun bij ->
      let bij_bind = lazy (triple Symbol.bij Type.bij (Lazy.force bij)) in
      let bij_var = lazy (pair int_ Type.bij) in
      let bij_pair = lazy (pair (Lazy.force bij) (Lazy.force bij)) in
      switch
        ~inject:(fun t -> match t.term with
        | BoundVar i -> "bv", BranchTo (int_, i)
        | Var i -> "v", BranchTo (Lazy.force bij_var, (i, get_type t))
        | Bind (s, t') -> "bind", BranchTo (Lazy.force bij_bind, (s, get_type t, t'))
        | Const s -> "c", BranchTo (Symbol.bij, s)
        | At (t1, t2) -> "at", BranchTo (Lazy.force bij_pair, (t1, t2)))
        ~extract:(function
        | "bv" -> BranchFrom (int_, fun i -> mk_bound_var i)
        | "v" -> BranchFrom (Lazy.force bij_var, fun (i,ty) -> mk_var ~ty i)
        | "bind" -> BranchFrom (Lazy.force bij_bind, fun (s,ty,t') -> mk_bind s ~ty t')
        | "c" -> BranchFrom (Symbol.bij, mk_const)
        | "at" -> BranchFrom (Lazy.force bij_pair, fun (a, b) -> mk_at a b)
        | _ -> raise (DecodingError "expected Term")))
