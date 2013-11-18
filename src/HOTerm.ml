
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
  mutable ty : Type.t;          (** type *)
  mutable tsize : int;          (** size (number of subterms) *)
  mutable flags : int;          (** boolean flags about the term *)
  mutable tag : int;            (** hashconsing tag *)
}
(** content of the term *)
and term_cell =
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Lambda of t                 (** lambda abstraction over one variable. *)
  | Const of Symbol.t           (** Constant *)
  | At of t * Type.t list * t list    (** HO application *)

and sourced_term =
  t * string * string           (** Term + file,name *)

type term = t

type varlist = t list

(** {2 Basics} *)

let rec hash_novar t =
  let h_type = Type.hash t.ty in
  let h_term = match t.term with
  | Var _ -> 42
  | BoundVar _ -> 43
  | Const s -> Symbol.hash s
  | Lambda t' -> Hash.hash_int t'.tag
  | At (t,_,l) -> Hash.hash_list hash_novar (hash_novar t) l
  in
  Hash.combine h_type h_term

(** {2 Comparison, equality, containers} *)

let subterm ~sub t =
  let rec check t =
    sub == t ||
    match t.term with
    | Var _ | BoundVar _ | Const _ -> false
    | Lambda t' -> check t'
    | At (t, _, l) -> check t || List.exists check l
  in
  check t

let eq x y = x == y  (* because of hashconsing *)

let compare x y = x.tag - y.tag

let hash x = x.tag

let ty t = t.ty

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

let hash_term t =
  match t.term with
  | Var i -> Hash.combine (Hash.hash_int i) (Type.hash t.ty)
  | BoundVar i -> Hash.hash_int2 (Type.hash t.ty) (Hash.hash_int i)
  | Const s -> Symbol.hash s
  | Lambda t' -> Hash.hash_int t'.tag
  | At (t, tyargs, l) ->
    let h = Hash.hash_list Type.hash t.tag tyargs in
    Hash.hash_list (fun t' -> t'.tag) h l

let hashcons_equal x y =
  (* pairwise comparison of subterms *)
  let rec eq_subterms a b = match a, b with
    | [], [] -> true
    | a::a1, b::b1 -> a == b && eq_subterms a1 b1
    | _ -> false
  and eq_types a b = match a, b with
    | [], [] -> true
    | xa::a', xb::b' -> Type.eq xa xb && eq_types a' b'
    | _ -> false
  in
  (* compare subterms, if same structure *)
  match x.term, y.term with
  | Var i, Var j
  | BoundVar i, BoundVar j -> i = j && Type.eq x.ty y.ty
  | Const s1, Const s2 -> Symbol.eq s1 s2
  | Lambda ta, Lambda tb -> ta == tb && Type.eq x.ty y.ty
  | At (t1, tyargs1, l1), At (t2, tyargs2, l2) ->
    t1 == t2 && eq_subterms l1 l2 && eq_types tyargs1 tyargs2
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
  begin match t.term with
    | Var _ | BoundVar _ -> ()
    | _ -> raise (Invalid_argument "cast")
  end;
  H.hashcons { t with ty; tag = ~-1; }

let lambda_var_ty t = match t.term with
  | Lambda _ ->
    begin match t.ty.Type.ty with
    | Type.Fun (_, arg::_) -> arg
    | _ -> raise (Invalid_argument "lambda_var_ty: expected function type")
    end
  | _ -> raise (Invalid_argument "lambda_var_ty: expected lambda term")

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags) *)

let mk_var ~ty idx =
  assert (idx >= 0);
  let my_v = {term = Var idx; ty; tsize = 1;
              flags=(flag_db_closed lor flag_db_closed_computed lor
                     flag_normal_form);
              tag= -1} in
  H.hashcons my_v

let __mk_bound_var ~ty idx =
  assert (idx >= 0);
  let my_v = {term = BoundVar idx; ty; tsize = 1;
              flags=(flag_db_closed_computed lor flag_normal_form);
              tag= -1} in
  H.hashcons my_v

let __mk_lambda ~varty t' =
  let ty = Type.mk_fun t'.ty [varty] in
  let my_t = {term=Lambda t'; ty; flags=0; tsize=0; tag= -1} in
  let t = H.hashcons my_t in
  (if t == my_t
    then begin (* compute ground-ness of term *)
      set_flag flag_ground t (get_flag flag_ground t');
      t.tsize <- t'.tsize + 1;
    end);
  t

let mk_const s =
  let my_t = {term=Const s; ty=Symbol.ty s;
              flags=(flag_db_closed lor flag_db_closed_computed lor flag_normal_form);
              tsize=1; tag= -1;} in
  let t = H.hashcons my_t in
  t

(* compute type of t(tyargs @ l) *)
let _compute_ty t tyargs l =
  match tyargs, l with
    | [], [] -> t.ty
    | _ ->
      let all_ty_args = tyargs @ (List.map ty l) in
      Type.apply t.ty all_ty_args

(* enforce invariant:  (t @ l) @ l' -----> t @ (concat l l') *)
let rec _mk_at t tyargs l =
  match t.term, tyargs, l with
  | At (_,_,_::_), _::_, _ ->
    failwith "HOTerm.mk_at: type arguments must be in prefix position"
  | At (t', tyargs', l'), [], _ ->
    _mk_at t' tyargs' (l' @ l) (* flatten (t @ ty @ l) @ l' ---> t @ ty @ (concat l l') *)
  | At (t', tyargs', []), _::_, _  ->
    _mk_at t' (tyargs' @ tyargs) l   (* flatten (t @ ty) @ ty' @ l ---> t @ (concat ty ty') l *)
  | _, [], [] ->
    t (* t @ [] ---> t *)
  | _, _, _ ->
    let ty = _compute_ty t tyargs l in
    let my_t = {term=At (t,tyargs,l); ty; tsize=0; flags=0; tag= -1} in
    let t = H.hashcons my_t in
    if t == my_t
      then begin
        (* compute ground-ness of term *)
        let is_ground = get_flag flag_ground t &&
          List.for_all (get_flag flag_ground) l
        in
        set_flag flag_ground t is_ground;
        t.tsize <- List.fold_left (fun s t' -> s + t'.tsize) (t.tsize+1) l;
      end;
    t

let mk_at ?(tyargs=[]) t l =
  match tyargs, l with
  | [], [] -> t
  | _ -> _mk_at t tyargs l

let true_term = mk_const Symbol.true_symbol
let false_term = mk_const Symbol.false_symbol

(** Easy constructors for formulas *)

let not_term = mk_const Symbol.not_symbol
let and_term = mk_const Symbol.and_symbol
let or_term = mk_const Symbol.or_symbol
let imply_term = mk_const Symbol.imply_symbol
let equiv_term = mk_const Symbol.equiv_symbol

let eq_term = mk_const Symbol.eq_symbol
let forall_term = mk_const Symbol.forall_symbol
let exists_term = mk_const Symbol.exists_symbol

let mk_not t = mk_at not_term [t]
let mk_and a b = mk_at and_term [a; b]
let mk_or a b = mk_at or_term [a; b]
let mk_imply a b = mk_at imply_term [a; b]
let mk_equiv a b = mk_at equiv_term [a; b]
let mk_xor a b = mk_not (mk_equiv a b)
let mk_eq a b = mk_at ~tyargs:[a.ty] eq_term [a; b]   (* use type of left arg *)
let mk_neq a b = mk_not (mk_eq a b)

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

let is_lambda t = match t.term with
  | Lambda _ -> true
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
  | Lambda t', 0::subpos -> at_pos t' subpos
  | At (t, _, _), 0::subpos -> at_pos t subpos
  | At (_, _, l), n::subpos when n <= List.length l -> at_pos (List.nth l (n-1)) subpos
  | _ -> invalid_arg "index too high for subterm"

let rec replace_pos t pos new_t = match t.term, pos with
  | _, [] -> new_t
  | (Var _ | BoundVar _), _::_ -> invalid_arg "wrong position in term"
  | Lambda t', 0::subpos ->
    let varty = lambda_var_ty t in
    __mk_lambda ~varty (replace_pos t' subpos new_t)
  | At (t, tyargs, l), 0::subpos -> mk_at ~tyargs (replace_pos t subpos new_t) l
  | At (t, tyargs, l), n::subpos when n <= List.length l ->
    let t' = replace_pos (List.nth l (n-1)) subpos new_t in
    let l' = Util.list_set l n t' in
    mk_at ~tyargs t l'
  | _ -> invalid_arg "index too high for subterm"

(** [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let rec replace t ~old ~by = match t.term with
  | _ when t == old -> by
  | Var _ | BoundVar _ | Const _ -> t
  | Lambda t' ->
    let varty = lambda_var_ty t in
    __mk_lambda ~varty (replace t' ~old ~by)
  | At (t, tyargs, l) ->
    mk_at ~tyargs (replace t ~old ~by) (List.map (fun t' -> replace t' ~old ~by) l)

(** Size of the term (number of subterms) *)
let size t = t.tsize

let rec ty_vars set t = match t.term with
  | Var _
  | BoundVar _ 
  | Const _ -> Type.free_vars_set set t.ty
  | Lambda t' -> ty_vars set t'
  | At (head, tyargs, l) ->
    let set = ty_vars set head in
    let set = List.fold_left Type.free_vars_set set tyargs in
    List.fold_left ty_vars set l

(** get subterm by its position *)
let rec at_cpos t pos = match t.term, pos with
  | _, 0 -> t
  | Lambda t', _ -> at_cpos t' (pos-1)
  | At (t, _, l), _ ->
    let s = size t in
    if s > pos then at_cpos t pos else get_subpos l (pos-s)
  | _ -> assert false
and get_subpos l pos =
  match l, pos with
  | t::l', _ when size t > pos -> at_cpos t pos  (* search inside the term *)
  | t::l', _ -> get_subpos l' (pos - size t) (* continue to next term *)
  | [], _ -> assert false

let max_cpos t = size t - 1

let is_ground t = get_flag flag_ground t

let rec monomorphic t =
  Type.is_ground t.ty &&
  match t.term with
  | Var _ | BoundVar _ | Const _ -> true
  | Lambda t' -> monomorphic t'
  | At (t, tyargs, l) ->
    monomorphic t && List.for_all Type.is_ground tyargs && List.for_all monomorphic l
  

let rec var_occurs x t = match t.term with
  | Const s -> false
  | Var _
  | BoundVar _ -> x == t
  | Lambda t' -> var_occurs x t'
  | _ when is_ground t -> false  (* no variable *)
  | At (t, _, l) -> var_occurs x t || List.exists (var_occurs x) l

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
| Lambda t' -> add_vars set t'
| At (t, _, l) -> add_vars set t; List.iter (add_vars set) l

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
  | Lambda t' -> traverse acc t'
  | At (t, _, l) ->
    let acc = traverse acc t in
    List.fold_left traverse acc l
  in
  traverse [] t

(** depth of term *)
let rec depth t = match t.term with
  | Var _ | BoundVar _ | Const _ -> 1
  | Lambda t' -> 1 + depth t'
  | At (t, _, l) ->
    List.fold_left (fun acc t' -> max acc (depth t')) (depth t) l

let rec head t = match t.term with
  | Const s -> s
  | At (t, _, _) -> head t
  | BoundVar _
  | Lambda _ -> raise (Invalid_argument "Term.head: lambda")
  | Var _ -> raise (Invalid_argument "Term.head: variable")

let symbols seq =
  let rec symbols set t = match t.term with
    | Var _
    | BoundVar _ -> set
    | Const s -> Symbol.Set.add s set
    | Lambda t' -> symbols set t'
    | At (t, _, l) ->
      List.fold_left symbols (symbols set t) l
  in
  Sequence.fold symbols Symbol.Set.empty seq

(** Does t contains the symbol f? *)
let rec contains_symbol f t =
  match t.term with
  | Var _
  | BoundVar _ -> false
  | Const s -> Symbol.eq s f
  | Lambda t' -> contains_symbol f t'
  | At (t, _, l) ->
    contains_symbol f t || List.exists (fun t' -> contains_symbol f t') l

(** [flatten_ac f l] flattens the list of terms [l] by deconstructing all its
    elements that have [f] as head symbol. For instance, if l=[1+2; 3+(4+5)]
    with f="+", this will return [1;2;3;4;5], perhaps in a different order *)
let flatten_ac f l =
  let rec flatten acc l = match l with
  | [] -> acc
  | x::l' -> flatten (deconstruct acc x) l'
  and deconstruct acc t = match t.term with
  | At ({term=Const f'}, _, l) when Symbol.eq f f' ->
    flatten acc l
  | _ -> t::acc
  in flatten [] l

(** {2 De Bruijn indexes} *)

(** check whether the term is a term or an atomic proposition *)
let rec atomic t =
  match t.term with
  | Var _ | BoundVar _ -> true
  | Lambda t' -> atomic t'
  | Const s ->
    not Symbol.(eq s and_symbol || eq s or_symbol || eq s imply_symbol || eq s
      not_symbol || eq s eq_symbol || eq s equiv_symbol)
  | At (t, _, l) -> atomic t && List.for_all atomic l

module DB = struct
  (* compute whether the term is closed w.r.t. De Bruijn (bound) variables *)
  let rec compute_db_closed depth t = match t.term with
    | BoundVar i -> i < depth
    | Lambda t' -> compute_db_closed (depth+1) t'
    | Const _
    | Var _ -> true
    | At (t, _, l) ->
      compute_db_closed depth t && List.for_all (compute_db_closed depth) l

  (** check wether the term is closed w.r.t. De Bruijn variables *)
  let closed ?(depth=0) t =
    match depth with
    | 0 ->
      (* compute and store result, if not already computed *)
      (if not (get_flag flag_db_closed_computed t) then begin
        set_flag flag_db_closed_computed t true;
        set_flag flag_db_closed t (compute_db_closed 0 t);
        end);
      (* return flag *)
      get_flag flag_db_closed t
    | _ -> compute_db_closed depth t

  (** check whether t contains the De Bruijn symbol n *)
  let rec contains t n = match t.term with
    | BoundVar i -> i = n
    | Const _
    | Var _ -> false
    | Lambda t' -> contains t' (n+1)
    | At (t, _, l) ->
      contains t n || List.exists (fun t' -> contains t' n) l

  (** shift the non-captured De Bruijn indexes in the term by n *)
  let shift ?(depth=0) n t =
    (* traverse the term, looking for non-captured DB indexes.
       [depth] is the number of binders on the path from the root of the
       term, to the current position. *)
    let rec recurse depth t = 
      match t.term with
      | _ when closed t -> t  (* closed. *)
      | BoundVar i when i >= depth ->
        __mk_bound_var ~ty:t.ty (i+n) (* lift by n, term not captured *)
      | Var _ | BoundVar _ | Const _ -> t
      | Lambda t' ->
        let varty = lambda_var_ty t in
        __mk_lambda ~varty (recurse (depth+1) t') (* increase depth and recurse *)
      | At (t, tyargs, l) ->
        mk_at ~tyargs (recurse depth t) (List.map (recurse depth) l)
    in
    assert (n >= 0);
    if depth=0 && n = 0 then t else recurse depth t

  (* unshift the term (decrement indices of all free De Bruijn variables inside *)
  let unshift ?(depth=0) n t =
    (* only unlift DB symbol that are free. [depth] is the number of binders
       on the path from the root term. *)
    let rec recurse depth t =
      match t.term with
      | _ when closed t -> t
      | BoundVar i ->
        if i >= depth then __mk_bound_var ~ty:t.ty (i-n) else t
      | Const _ | Var _ -> t
      | Lambda t' ->
        let varty = lambda_var_ty t in
        __mk_lambda ~varty (recurse (depth+1) t')
      | At (t, tyargs, l) ->
        mk_at ~tyargs (recurse depth t) (List.map (recurse depth) l)
    in recurse depth t

  let replace ?(depth=0) t ~sub =
    (* recurse and replace [sub]. *)
    let rec replace depth t = match t.term with
    | _ when t == sub -> __mk_bound_var ~ty:t.ty depth
    | Lambda t' ->
      let varty = lambda_var_ty t in
      __mk_lambda ~varty (replace (depth+1) t')
    | Var _
    | Const _
    | BoundVar _ -> t
    | At (t, tyargs, l) ->
      mk_at ~tyargs (replace depth t) (List.map (replace depth) l)
    in
    replace depth t

  let from_var ?depth t ~var =
    assert (is_var var);
    replace ?depth t ~sub:var

  let eval ?(depth=0) env t =
    let rec eval depth env t = match t.term with
      | _ when closed t || is_ground t -> t
      | Const _
      | Var _ -> t
      | BoundVar i ->
        begin match DBEnv.find env i with
        | None -> t
        | Some t' -> shift depth t'
        end
      | Lambda t' ->
        let varty = lambda_var_ty t in
        let t' = eval (depth+1) (DBEnv.push_none env) t' in
        __mk_lambda ~varty t'
      | At (t, tyargs, l) ->
        let t' = eval depth env t in
        let l' = List.map (eval depth env) l in
        mk_at ~tyargs t' l'
    in
    eval depth env t
end

(** {2 High-level operations} *)

(** constructors with free variables. The first argument is the
    list of variables that is bound, then the quantified/abstracted
    term. *)

let rec mk_lambda vars t = match vars with
  | [] -> t
  | var::vars' ->
    let t' = mk_lambda vars' t in
    let varty = var.ty in
    __mk_lambda ~varty (DB.from_var (DB.shift 1 t') ~var)

let rec mk_forall vars t = match vars with
  | [] -> t
  | var::vars' ->
    let t' = mk_forall vars' t in
    let varty = var.ty in
    mk_at ~tyargs:[varty] forall_term [mk_lambda [var] t']

let rec mk_exists vars t = match vars with
  | [] -> t
  | var::vars' ->
    let t' = mk_exists vars' t in
    let varty = var.ty in
    mk_at ~tyargs:[varty] exists_term [mk_lambda [var] t']

let __mk_forall ~varty t =
  mk_at ~tyargs:[varty] forall_term [__mk_lambda ~varty t]

let __mk_exists ~varty t =
  mk_at ~tyargs:[varty] exists_term [__mk_lambda ~varty t]


(** Bind all free variables by 'forall' *)
let close_forall t =
  let vars = vars t in
  mk_forall vars t

(** Bind all free variables by 'exists' *)
let close_exists t =
  let vars = vars t in
  mk_exists vars t

(** {2 High level operations} *)

(* Curry all subterms *)
let rec curry t =
  let ty = FOT.ty t in
  match t.FOT.term with
  | FOT.Var i -> mk_var ~ty i
  | FOT.BoundVar i -> __mk_bound_var ~ty i
  | FOT.Node (f, [], []) -> mk_const f
  | FOT.Node (f, tyargs, l) ->
    mk_at ~tyargs (mk_const f) (List.map curry l)

let rec uncurry t =
  let ty = t.ty in
  match t.term with
  | Var i -> FOT.mk_var ~ty i
  | BoundVar i -> FOT.__mk_bound_var ~ty i
  | Lambda t' -> failwith "cannot uncurry lambda"
  | Const s -> FOT.mk_const s
  | At ({term=Const s}, tyargs, l) ->
    let l = List.map uncurry l in
    FOT.mk_node ~tyargs s l
  | _ -> failwith "cannot uncurry higher-order application"

let rec is_fo t = match t.term with
  | Var _ -> true
  | BoundVar _ -> false
  | Lambda _ -> false
  | At ({term=Const _}, _, l) -> List.for_all is_fo l
  | At _ -> false (* (X|lambda t) @ _ is not first-order  *)
  | Const _ -> true

(** {2 IO} *)

let print_all_types = ref false

let pp_tstp_depth depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t = match t.term with
  | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
  | Lambda t' ->
    let varty = lambda_var_ty t in
    Printf.bprintf buf "^[%a:%a]: " pp_bvar () Type.pp varty;
    incr depth;
    pp_surrounded buf t';
    decr depth
  | Const s -> Symbol.pp buf s
  | Var i -> Printf.bprintf buf "X%d" i
  | At (t, tyargs, l) ->
    pp_surrounded buf t;  Buffer.add_string buf " @ ";
    Util.pp_list ~sep:" @ " Type.pp_tstp buf tyargs;
    (if l<>[] && tyargs<>[] then Buffer.add_string buf " @ ");
    Util.pp_list ~sep:" @ " pp_surrounded buf l
  and pp_surrounded buf t = match t.term with
  | At (_, _, _) ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  and pp_bvar buf () =  Printf.bprintf buf "Y%d" !depth in
  pp_rec buf t

(* lightweight printing *)
let rec pp_depth depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t =
    begin match t.term with
    | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
    | Lambda t' ->
      let varty = lambda_var_ty t in
      Printf.bprintf buf "λ%a:%a. " pp_bvar () Type.pp varty;
      incr depth;
      pp_surrounded buf t';
      decr depth
    | Const s -> Symbol.pp buf s
    | Var i ->
      if Type.eq t.ty Type.i
        then Printf.bprintf buf "X%d" i
        else Printf.bprintf buf "X%d:%a" i Type.pp t.ty
    | At (t, tyargs, l) ->
      pp_surrounded buf t;  Buffer.add_string buf " @ ";
      Util.pp_list ~sep:" @ " Type.pp buf tyargs;
      (if l<>[] && tyargs<>[] then Buffer.add_string buf " @ ");
      Util.pp_list ~sep:" @ " pp_surrounded buf l
    end;
    if !print_all_types then Printf.bprintf buf ":%a" Type.pp t.ty
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
    Format.fprintf fmt "X%d:%a" i Type.fmt t.ty
  | BoundVar i -> Format.fprintf fmt "Y%d" i
  | Lambda t' -> 
    let varty = lambda_var_ty t in
    Format.fprintf fmt "(lambda %a %a)" Type.fmt varty debug t'
  | Const s -> Symbol.fmt fmt s
  | At (t, tyargs, l) ->
    Format.fprintf fmt "(%a %a %a)" debug t
      (Sequence.pp_seq ~sep:" " Type.fmt) (Sequence.of_list tyargs)
      (Sequence.pp_seq ~sep:" " debug) (Sequence.of_list l)

let bij =
  let open Bij in
  let (!!!) = Lazy.force in
  fix
    (fun bij ->
      let bij_lam = lazy (pair Type.bij !!!bij) in
      let bij_var = pair int_ Type.bij in
      let bij_cst = Symbol.bij in
      let bij_at = lazy (triple !!!bij (list_ Type.bij) (list_ !!!bij)) in
      switch
        ~inject:(fun t -> match t.term with
        | BoundVar i -> "bv", BranchTo (bij_var, (i,t.ty))
        | Var i -> "v", BranchTo (bij_var, (i, t.ty))
        | Const s -> "c", BranchTo (bij_cst, s)
        | Lambda t' -> "lam", BranchTo (!!!bij_lam, (lambda_var_ty t, t'))
        | At (t, tyargs, l) -> "at", BranchTo (!!!bij_at, (t, tyargs, l)))
        ~extract:(function
        | "bv" -> BranchFrom (bij_var, fun (i,ty) -> __mk_bound_var ~ty i)
        | "v" -> BranchFrom (bij_var, fun (i,ty) -> mk_var ~ty i)
        | "c" -> BranchFrom (bij_cst, fun s -> mk_const s)
        | "lam" -> BranchFrom (!!!bij_lam, fun (varty,t') -> __mk_lambda ~varty t')
        | "at" -> BranchFrom (!!!bij_at, fun (t, tyargs, l) -> mk_at ~tyargs t l)
        | _ -> raise (DecodingError "expected Term")))
