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

module Utils = FoUtils

let is_symmetric_symbol s =
  s = eq_symbol || s = or_symbol || s = and_symbol

let is_infix_symbol s =
  s = eq_symbol || s = or_symbol || s = and_symbol || s = imply_symbol

let is_binder_symbol s =
  s = lambda_symbol

let hash_term t =
  let hash t = match t.term with
  | Var i -> 17 lxor (Utils.murmur_hash i)
  | Node (s, l) ->
    let rec aux h = function
    | [] -> h
    | head::tail -> aux (Utils.murmur_hash (head.hkey lxor h)) tail
    in
    let h = Utils.murmur_hash (2749 lxor Hashtbl.hash s) in
    aux h l
  in (Hashtbl.hash t.sort) lxor (hash t)

(* ----------------------------------------------------------------------
 * comparison, equality, containers
 * ---------------------------------------------------------------------- *)

let rec member_term a b =
  a == b ||
  (match b.term with
  | Var _ -> false
  | Node (_, subterms) -> List.exists (member_term a) subterms)

let rec member_term_rec a b =
  match b.term with
  | _ when a == b -> true
  | Var _ when b.binding != b -> member_term_rec a b.binding
  | Var _ -> false
  | Node (s, subterms) -> List.exists (member_term_rec a) subterms

let eq_term x y = x == y  (* because of hashconsing *)

let compare_term x y = x.tag - y.tag

module TSet = Set.Make(struct type t = term let compare = compare_term end)

module TPairSet = Set.Make(
  struct
    type t = term * term
    let compare (t1, t1') (t2, t2') =
      if eq_term t1 t2
        then compare_term t1' t2'
        else compare_term t1 t2
  end)

module THashtbl = Hashtbl.Make(
  struct
    type t = term
    let hash t = t.hkey
    let equal t1 t2 = eq_term t1 t2
  end)

module THashSet =
  struct
    type t = unit THashtbl.t
    let create () = THashtbl.create 13
    let member t term = THashtbl.mem t term
    let iter set f = THashtbl.iter (fun t () -> f t) set
    let add set t = THashtbl.replace set t ()
    let size t = THashtbl.length t
    let merge s1 s2 = iter s2 (add s1)
    let append_array set a =
      for i = 0 to Array.length a - 1 do add set a.(i); done
    let to_list set =
      let l = ref [] in
      iter set (fun t -> l := t :: !l); !l
    let to_vector set =
      let v = Vector.create (size set) in
      iter set (Vector.push v);
      v
    let from_list l =
      let set = create () in
      List.iter (add set) l; set
  end

(* ----------------------------------------------------------------------
 * access global terms table (hashconsing)
 * ---------------------------------------------------------------------- *)

(** hashconsing for terms *)
module H = Hashcons.Make(struct
  type t = typed_term

  let equal x y =
    (* pairwise comparison of subterms *)
    let rec eq_subterms a b = match (a, b) with
      | ([],[]) -> true
      | (a::a1, b::b1) ->
        if eq_term a b then eq_subterms a1 b1 else false
      | (_, _) -> false
    in
    (* compare sorts, then subterms, if same structure *)
    if x.sort <> y.sort then false
    else match (x.term, y.term) with
    | Var i, Var j -> i = j
    | Node (sa, la), Node (sb, lb) -> sa = sb && eq_subterms la lb
    | _ -> false

  let hash t = t.hkey

  let tag i t = (t.tag <- i; t)
end)

let iter_terms f = H.iter f

let all_terms () =
  let l = ref [] in
  iter_terms (fun t -> l := t :: !l);
  !l
  
let stats () = H.stats ()

let sig_version = ref 0

(* ----------------------------------------------------------------------
 * boolean flags
 * ---------------------------------------------------------------------- *)

let flag_db_closed = 0x1
and flag_simplified = 0x2
and flag_normal_form = 0x4

let set_flag flag t truth =
  if truth
    then t.flags <- t.flags lor flag
    else t.flags <- t.flags land (lnot flag)

let get_flag flag t = (t.flags land flag) != 0

(* ----------------------------------------------------------------------
 * smart constructors, with a bit of type-checking
 * ---------------------------------------------------------------------- *)

(** compute set of variables from the list of terms *)
let compute_vars l =
  let v = Vector.create 10 in
  List.iter (fun t -> Vector.append_array v t.vars) l;
  (* sort and remove duplicates *)
  let v' = Vector.uniq_sort ~cmp:compare_term v in
  Vector.to_array v'

let rec compute_db_closed depth t = match t.term with
  | Node (s, []) when s = db_symbol -> depth < 0
  | Node (s, l) when is_binder_symbol s ->
    List.for_all (compute_db_closed (depth-1)) l
  | Node (s, [t']) when s = succ_db_symbol -> 
    compute_db_closed (depth+1) t'
  | Var _ -> true
  | Node (_, l) -> List.for_all (compute_db_closed depth) l

let mk_var idx sort =
  let rec my_v = {term = Var idx; sort=sort; vars=[|my_v|];
                  flags=(flag_db_closed lor flag_simplified lor flag_normal_form);
                  binding=my_v; tsize=1; tag= -1; hkey=0} in
  my_v.hkey <- hash_term my_v;
  H.hashcons my_v

let mk_node s sort l =
  let rec my_t = {term=Node (s, l); sort; vars=[||]; flags=0;
                  binding=my_t; tsize=0; tag= -1; hkey=0} in
  my_t.hkey <- hash_term my_t;
  let t = H.hashcons my_t in
  (if t == my_t
    then begin  (* compute additional data, the term is new *)
      set_flag flag_db_closed t (compute_db_closed 0 t);
      t.vars <- compute_vars l;
      t.tsize <- List.fold_left (fun acc subt -> acc + subt.tsize) 1 l;
    end);
  t

let mk_const s sort = mk_node s sort []

let true_term = mk_const true_symbol bool_sort
let false_term = mk_const false_symbol bool_sort

(* constructors for terms *)
let check_bool t = assert (t.sort = bool_sort)

let mk_not t = (check_bool t; mk_node not_symbol bool_sort [t])
let mk_and a b = (check_bool a; check_bool b; mk_node and_symbol bool_sort [a; b])
let mk_or a b = (check_bool a; check_bool b; mk_node or_symbol bool_sort [a; b])
let mk_imply a b = (check_bool a; check_bool b; mk_node imply_symbol bool_sort [a; b])
let mk_equiv a b = (check_bool a; check_bool b; mk_node eq_symbol bool_sort [a; b])
let mk_eq a b = (assert (a.sort = b.sort); mk_node eq_symbol bool_sort [a; b])
let mk_lambda t = mk_node lambda_symbol t.sort [t]
let mk_forall t = (check_bool t; mk_node forall_symbol bool_sort [mk_lambda t])
let mk_exists t = (check_bool t; mk_node exists_symbol bool_sort [mk_lambda t])

let rec cast t sort =
  let new_t = {t with sort=sort} in
  new_t.hkey <- hash_term new_t;
  H.hashcons new_t

(* ----------------------------------------------------------------------
 * examine term/subterms, positions...
 * ---------------------------------------------------------------------- *)

let is_var t = match t.term with
  | Var _ -> true
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
  | _ -> invalid_arg "index too high for subterm"

let rec replace_pos t pos new_t = match t.term, pos with
  | _, [] -> new_t
  | Var _, _::_ -> invalid_arg "wrong position in term"
  | Node (s, l), i::subpos when i < List.length l ->
    let new_subterm = replace_pos (Utils.list_get l i) subpos new_t in
    mk_node s t.sort (Utils.list_set l i new_subterm)
  | _ -> invalid_arg "index too high for subterm"

let depth t =
  let rec depth t n = match t.term with
  | Var _ -> n
  | Node (_, l) ->
    List.fold_left (fun acc t -> max acc (depth t (n+1))) n l
  in depth t 1

let var_occurs x t =
  let rec check i =
    if i > Array.length t.vars then false
    else if x.vars.(i) == x then true
    else check (i+1)
  in check 0

let is_ground_term t = Array.length t.vars = 0

let merge_varset l1 l2 =
  let v = Vector.copy l1 in
  Vector.append v l2;
  let v' = Vector.uniq_sort ~cmp:compare_term v in
  v'

let max_var vars =
  let m = ref min_int in
  for i = 0 to Array.length vars - 1 do
    match vars.(i).term with
    | Var idx -> m := max !m idx
    | _ -> assert false
  done;
  !m

let min_var vars =
  let m = ref max_int in
  for i = 0 to Array.length vars - 1 do
    match vars.(i).term with
    | Var idx -> m := min !m idx
    | _ -> assert false
  done;
  !m

(* ----------------------------------------------------------------------
 * De Bruijn terms, and dotted formulas
 * ---------------------------------------------------------------------- *)

(** check whether the term is a term or an atomic proposition *)
let rec atomic t = match t.term with
  | _ when t.sort <> bool_sort -> true
  | Var _ -> true
  | Node (s, l) -> not (s = and_symbol || s = or_symbol
    || s = forall_symbol || s = exists_symbol || s = imply_symbol
    || s = not_symbol || s = eq_symbol)

(** check whether the term contains connectives or quantifiers *)
let rec atomic_rec t = match t.term with
  | _ when t.sort <> bool_sort -> true  (* first order *)
  | Var _ -> true
  | Node (s, l) ->
    not (s = and_symbol || s = or_symbol || s = forall_symbol || s =
    exists_symbol || s = imply_symbol || s = not_symbol || s = eq_symbol)
    && List.for_all atomic_rec l

(** check wether the term is closed w.r.t. De Bruijn variables *)
let db_closed t = get_flag flag_db_closed t

let rec db_var t =
  match t.term with
  | Node (s, []) when s = db_symbol -> true
  | Node (s, [t]) when s = succ_db_symbol -> db_var t
  | _ -> false

(** check whether t contains the De Bruijn symbol n *)
let rec db_contains t n = match t.term with
  | Node (s, []) when s = db_symbol -> n = 0
  | Node (_, []) | Var _ -> false
  | Node (s, [t']) when is_binder_symbol s -> db_contains t' (n+1)
  | Node (s, [t']) when s = succ_db_symbol -> db_contains t' (n-1)
  | Node (_, l) -> List.exists (fun t' -> db_contains t' n) l

(** replace 0 by s in t *)
let db_replace t s =
  (* lift the De Bruijn symbol *)
  let mk_succ db = mk_node succ_db_symbol univ_sort [db] in
  (* replace db by s in t *)
  let rec replace db s t = match t.term with
  | _ when eq_term t db -> s
  | Var _ -> t
  | _ when is_const t -> t
  | Node (symb, l) when is_binder_symbol symb ->
    (* lift the De Bruijn to replace *)
    mk_node symb t.sort (List.map (replace (mk_succ db) s) l)
  | Node (f, _) when f = succ_db_symbol || f = db_symbol ->
    t (* no the good De Bruijn symbol *)
  | Node (f, l) -> mk_node f t.sort (List.map (replace db s) l)
  (* replace the 0 De Bruijn index by s in t *)
  in
  replace (mk_const db_symbol univ_sort) s t

(** create a De Bruijn variable of index n *)
let rec db_make n sort = match n with
  | 0 -> mk_const db_symbol sort
  | n when n > 0 ->
    let next = db_make (n-1) sort in
    mk_node succ_db_symbol sort [next]
  | _ -> assert false

(** lift the non-captured De Bruijn indexes in the term by n *)
let db_lift n t =
  (* traverse the term, looking for non-captured DB indexes.
     db_balance is (height of DB - number of binders on path) *)
  let rec recurse db_balance t = 
    match t.term with
    | _ when db_closed t -> t  (* closed. *)
    | Var _ -> t
    | Node (s, []) when s = db_symbol && db_balance >= 0 ->
      db_make n t.sort  (* lift by n, term not captured *)
    | Node (_, []) -> t
    | Node (s, [t']) when s = succ_db_symbol ->
      mk_node s t.sort [recurse (db_balance + 1) t']  (* ++ db_balance *)
    | Node (s, l) when is_binder_symbol s ->
      mk_node s t.sort (List.map (recurse (db_balance - 1)) l)  (* -- db_balance *)
    | Node (s, l) ->
      let l' = List.map (recurse db_balance) l in
      mk_node s t.sort l'  (* recurse in subterms *)
  in
  assert (n >= 0);
  if n = 0 then t else recurse 0 t

(* unlift the term (decrement indices of all De Bruijn variables inside *)
let db_unlift t =
  (* int indice of this DB term *)
  let rec db_index t = match t.term with
    | Node (s, []) when s = db_symbol -> 0
    | Node (s, [t']) when s = succ_db_symbol -> (db_index t') + 1
    | _ -> assert false
  (* only unlift DB symbol that are free *)
  and recurse depth t =
    match t.term with
    | Node (s, []) when s = db_symbol && depth = 0 -> assert false (* cannot unlift this *)
    | Node (_, []) | Var _ -> t
    | Node (s, [t']) when s = succ_db_symbol ->
      if db_index t >= depth then t' else t (* unlift only if not bound *)
    | Node (s, l) when is_binder_symbol s ->
      (* unlift, but index of unbound variables is +1 *)
      mk_node s t.sort (List.map (recurse (depth+1)) l)
    | Node (s, l) -> mk_node s t.sort (List.map (recurse depth) l)
  in recurse 0 t

(* replace v by a De Bruijn symbol in t *)
let db_from_var t v =
  assert (is_var v);
  (* go recursively and replace *)
  let rec replace_and_lift depth t = match t.term with
  | Var _ -> if eq_term t v then db_make depth v.sort else t
  | Node (_, []) -> t
  | Node (s, l) when is_binder_symbol s ->
    mk_node s t.sort (List.map (replace_and_lift (depth+1)) l)  (* increment depth *) 
  | Node (s, l) -> mk_node s t.sort (List.map (replace_and_lift depth) l)
  (* make De Bruijn index of given index *)
  in
  replace_and_lift 0 t

(* index of the De Bruijn symbol *) 
let rec db_depth t = match t.term with
  | Node (s, []) when s = db_symbol -> 0
  | Node (s, [t']) when s = succ_db_symbol -> (db_depth t') + 1
  | _ -> failwith "not a proper De Bruijn term"

exception FoundSort of sort

(** [look_db_sort n t] find the sort of the De Bruijn index n in t *)
let look_db_sort index t =
  let rec lookup depth t = match t.term with
    | Node (s, subterms) when is_binder_symbol s ->
      List.iter (lookup (depth+1)) subterms  (* increment for binder *)
    | Node (s, [t]) when s = succ_db_symbol ->
      lookup (depth-1) t  (* decrement for lifted De Bruijn *)
    | Node (s, []) when s = db_symbol && depth = 0 -> raise (FoundSort t.sort)
    | Node (_, l) -> List.iter (lookup depth) l
    | Var _ -> ()
  in try lookup index t; None
     with FoundSort s -> Some s

(* ----------------------------------------------------------------------
 * bindings and normal forms
 * ---------------------------------------------------------------------- *)

(** [set_binding t d] set variable binding or normal form of t *)
let set_binding t d = t.binding <- d

(** reset variable binding/normal form *)
let reset_binding t = t.binding <- t

(** get the binding of variable/normal form of term *)
let rec get_binding t = 
  if t.binding == t then t else get_binding t.binding

(** check whether all variables of t are bound to themselves *)
let no_var_bound t =
  try
    for i = 0 to Array.length t.vars - 1 do
      let x = t.vars.(i) in
      if x.binding != x then raise Exit
    done;
    true
  with Exit -> false

(** replace variables by their bindings *)
let expand_bindings ?(recursive=true) t =
  (* recurse to expand bindings, returns new term.  Also keeps track of the
    number of binders met so far, for lifting non-closed De Bruijn indexes in
    substituted terms. *)
  let rec recurse binder_depth t =
    (* if no variable of t is bound (or t ground), nothing to do *)
    if is_ground_term t || no_var_bound t then t
    else match t.term with
    | Var _ ->
      if t.binding == t then t
      else
        let t' = if db_closed t.binding  (* maybe have to lift DB vars *)
          then t.binding
          else db_lift binder_depth t.binding in
        if recursive then recurse binder_depth t' else t'
      (* lift open De Bruijn symbols in t.binding by the number of binders encountered *)
    | Node (s, l) when is_binder_symbol s ->
      (* increase number of binders met *)
      mk_node s t.sort (List.map (recurse (binder_depth+1)) l)
    | Node (s, l) ->
      let l' = List.map (recurse binder_depth) l in
      mk_node s t.sort l' (* recursive replacement in subterms *)
  in recurse 0 t

(** reset bindings of variables of the term *)
let reset_vars t =
  for i = 0 to Array.length t.vars - 1 do
    reset_binding t.vars.(i);
  done

(* ----------------------------------------------------------------------
 * Pretty printing
 * ---------------------------------------------------------------------- *)

(** type of a pretty printer for symbols *)
class type pprinter_symbol =
  object
    method pp : Format.formatter -> symbol -> unit    (** pretty print a symbol *)
    method infix : symbol -> bool                     (** which symbol is infix? *)
  end

let pp_symbol_unicode =
  object
    method pp formatter s = match s with
      | _ when s = not_symbol -> Format.pp_print_string formatter "•¬"
      | _ when s = eq_symbol -> Format.pp_print_string formatter "•="
      | _ when s = lambda_symbol -> Format.pp_print_string formatter "•λ"
      | _ when s = exists_symbol -> Format.pp_print_string formatter "•∃"
      | _ when s = forall_symbol -> Format.pp_print_string formatter "•∀"
      | _ when s = and_symbol -> Format.pp_print_string formatter "•&"
      | _ when s = or_symbol -> Format.pp_print_string formatter "•|"
      | _ when s = imply_symbol -> Format.pp_print_string formatter "•→"
      | _ when s = db_symbol -> Format.pp_print_string formatter "•0"
      | _ when s = succ_db_symbol -> Format.pp_print_string formatter "•s"
      | _ -> Format.pp_print_string formatter s (* default *)
    method infix s = s = or_symbol || s = eq_symbol || s = and_symbol || s = imply_symbol
  end

let pp_symbol_tstp =
  object
    method pp formatter s = match s with
      | _ when s = not_symbol -> Format.pp_print_string formatter "~"
      | _ when s = eq_symbol -> Format.pp_print_string formatter "="
      | _ when s = lambda_symbol -> failwith "no lambdas in TSTP"
      | _ when s = exists_symbol -> Format.pp_print_string formatter "?"
      | _ when s = forall_symbol -> Format.pp_print_string formatter "!"
      | _ when s = and_symbol -> Format.pp_print_string formatter "&"
      | _ when s = or_symbol -> Format.pp_print_string formatter "|"
      | _ when s = imply_symbol -> Format.pp_print_string formatter "=>"
      | _ when s = db_symbol -> failwith "no DB symbols in TSTP"
      | _ when s = succ_db_symbol -> failwith "no DB symbols in TSTP"
      | _ -> Format.pp_print_string formatter s (* default *)
    method infix s = s = or_symbol || s = eq_symbol || s = and_symbol || s = imply_symbol
  end

let pp_symbol = ref pp_symbol_unicode

(** type of a pretty printer for terms *)
class type pprinter_term =
  object
    method pp : Format.formatter -> term -> unit    (** pretty print a term *)
  end

let pp_term_debug =
  (* print a De Bruijn term as nice unicode *)
  let rec pp_db formatter t =
    let n = db_depth t in
    Format.fprintf formatter "•%d" n in
  let _sort = ref false
  and _bindings = ref false
  and _skip_lambdas = ref true
  and _skip_db = ref true in
  (* printer itself *)
  object (self)
    method pp formatter t =
      (match t.term with
      | Node (s, [{term=Node (s', [a; b])}])
        when s = not_symbol && s' = eq_symbol ->
        Format.fprintf formatter "%a != %a" self#pp a self#pp b
      | Node (s, [a; b]) when s = eq_symbol ->
        Format.fprintf formatter "%a = %a" self#pp a self#pp b
      | Node (s, [t]) when s = not_symbol ->
        Format.fprintf formatter "%a%a" pp_symbol_unicode#pp s self#pp t
      | Node (s, [{term=Node (s', [t'])}])
        when s = forall_symbol || s = exists_symbol ->
        assert (s' = lambda_symbol);
        if !_skip_lambdas
          then Format.fprintf formatter "%a(%a)" pp_symbol_unicode#pp s self#pp t'
          else Format.fprintf formatter "%a%a(%a)"
                pp_symbol_unicode#pp s pp_symbol_unicode#pp s' self#pp t'
      | Node (s, [_]) when s = succ_db_symbol && !_skip_db ->
        pp_db formatter t (* print de bruijn symbol *)
      | Node (s, []) -> pp_symbol_unicode#pp formatter s
      | Node (s, args) ->
        (* general case for nodes *)
        if pp_symbol_unicode#infix s
          then begin
            match args with
            | [l;r] -> Format.fprintf formatter "@[<h>(%a %a %a)@]"
                self#pp l pp_symbol_unicode#pp s self#pp r
            | _ -> assert false (* infix and not binary? *)
          end else Format.fprintf formatter "@[<h>%a(%a)@]" pp_symbol_unicode#pp s
            (Utils.pp_list ~sep:", " self#pp) args
      | Var i -> if !_bindings && t != t.binding
        then (_bindings := false;
              Format.fprintf formatter "X%d → %a" i self#pp t.binding;
              _bindings := true)
        else Format.fprintf formatter "X%d" i);
      (* also print the sort if needed *)
      if !_sort then Format.fprintf formatter ":%s" t.sort else ()
    method sort s = _sort := s
    method bindings s = _bindings := s
    method skip_lambdas s = _skip_lambdas := s
    method skip_db s = _skip_db := s
  end

let pp_term_tstp =
  object (self)
    method pp formatter t =
      (* convert De Bruijn to regular variables *)
      let rec db_to_var varindex t = match t.term with
      | Node (s, [{term=Node (s', [t'])}])
        when (s = forall_symbol || s = exists_symbol) ->
        (* use a fresh variable, and convert to a named-variable representation *)
        let v = mk_var !varindex t'.sort in
        incr varindex;
        db_to_var varindex (mk_node s t.sort [v; db_unlift (db_replace t' v)])
      | Node (_, []) | Var _  -> t
      | Node (s, l) -> mk_node s t.sort (List.map (db_to_var varindex) l)
      (* recursive printing function *)
      and pp_rec t = match t.term with
      | Node (s, [{term=Node (s', [a; b])}])
        when s = not_symbol && s' = eq_symbol ->
        Format.fprintf formatter "%a != %a" self#pp a self#pp b
      | Node (s, [t]) when s = not_symbol ->
        Format.fprintf formatter "%a%a" pp_symbol_tstp#pp s self#pp t
      | Node (s, [v; t'])
        when (s = forall_symbol || s = exists_symbol) ->
        assert (is_var v);
        Format.fprintf formatter "%a[%a]: %a" pp_symbol_tstp#pp s self#pp v self#pp t'
      | Node (s, _) when s = succ_db_symbol ||  s = db_symbol ->
        failwith "De Bruijn symbol in term, cannot be printed in TSTP"
      | Node (s, []) -> pp_symbol_tstp#pp formatter s
      | Node (s, args) ->
        (* general case for nodes *)
        if pp_symbol_tstp#infix s
          then begin
            match args with
            | [l;r] -> Format.fprintf formatter "@[<h>(%a %a %a)@]"
                self#pp l pp_symbol_tstp#pp s self#pp r
            | _ -> assert false (* infix and not binary? *)
          end else Format.fprintf formatter "@[<h>%a(%a)@]" pp_symbol_tstp#pp s
            (Utils.pp_list ~sep:", " self#pp) args
      | Var i -> Format.fprintf formatter "X%d" i
      in
      let maxvar = max (max_var t.vars) 0 in
      let varindex = ref (maxvar+1) in
      (* convert everything to named variables, then print *)
      pp_rec (db_to_var varindex t)
  end

let pp_term = ref (pp_term_debug :> pprinter_term)

let pp_signature formatter symbols =
  Format.fprintf formatter "@[<h>sig %a@]"
    (Utils.pp_list ~sep:" > " !pp_symbol#pp) symbols
