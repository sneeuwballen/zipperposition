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

(* hashconsing for terms *)
module H = Hashcons.Make(struct
  type t = typed_term
  let rec equal x y =
    if x.sort <> y.sort then false
    else match (x.term, y.term) with
    | (Var i, Var j) -> i = j
    | (Leaf a, Leaf b) -> a = b
    | (Node a, Node b) -> eq_subterms a b
    | (_, _) -> false
  and eq_subterms a b = match (a, b) with
    | ([],[]) -> true
    | (a::a1, b::b1) -> if equal a.node b.node
      then eq_subterms a1 b1 else false
    | (_, _) -> false
  let hash t =
    let hash_term = match t.term with
    | Var i -> 17 lxor (Utils.murmur_hash i)
    | Leaf s -> Utils.murmur_hash (2749 lxor Hashtbl.hash s)
    | Node l ->
      let rec aux h = function
      | [] -> h
      | head::tail -> aux (Utils.murmur_hash (head.hkey lxor h)) tail
      in aux 23 l
    in (Hashtbl.hash t.sort) lxor hash_term
end)

(* the terms table *)
let terms = H.create 251

let iter_terms f = H.iter f terms

let all_terms () =
  let l = ref [] in
  iter_terms (fun t -> l := t :: !l);
  !l

let compute_vars t =  (* compute free vars of the term *)
  let rec aux acc t = match t.node.term with
    | Leaf _ -> acc
    | Var _ -> if (List.mem t acc) then acc else t::acc
    | Node l -> List.fold_left aux acc l
  in aux [] t

let rec compute_db_closed depth t = match t.node.term with
  | Leaf s when s = db_symbol -> depth > 0
  | Leaf s when s = succ_db_symbol -> false (* not a proper term *)
  | Node [{node={term=Leaf s}}; t'] when s = lambda_symbol ->
    compute_db_closed (depth+1) t'
  | Node [{node={term=Leaf s}}; t'] when s = succ_db_symbol -> 
    compute_db_closed (depth-1) t'
  | Leaf _ | Var _ -> true
  | Node l -> List.for_all (compute_db_closed depth) l

(* constructors *)
let mk_var idx sort =
  let my_v = {term = Var idx; sort=sort; vars=lazy []; db_closed=lazy false} in
  let closed = lazy (compute_db_closed 0 (H.hashcons terms my_v)) in
  let v = H.hashcons terms
    {my_v with vars=lazy [H.hashcons terms my_v]; db_closed=closed} in
  ignore (Lazy.force v.node.vars); v

let mk_leaf symbol sort =
  let db_closed = lazy (if symbol = db_symbol then false else true) in
  H.hashcons terms {term = Leaf symbol; sort=sort;
                    vars=lazy []; db_closed=db_closed}

let rec mk_node = function
  | [] -> failwith "cannot build empty node term"
  | [_] -> failwith "cannot build node term with no arguments"
  | {node={term=Var _}}::_ -> assert false
  | (head::_) as subterms ->
      let my_t = {term=(Node subterms); sort=head.node.sort;
                  vars=lazy []; db_closed=lazy false} in
      let lazy_vars = lazy (compute_vars (H.hashcons terms my_t)) in
      let db_closed = lazy (compute_db_closed 0 (H.hashcons terms my_t)) in
      let t = H.hashcons terms { my_t with vars=lazy_vars; db_closed=db_closed} in
      ignore (Lazy.force t.node.vars); t

let mk_apply f sort args =
  let head = mk_leaf f sort in
  if args = [] then head else mk_node (head :: args)

let is_var t = match t.node.term with
  | Var _ -> true
  | _ -> false

let is_leaf t = match t.node.term with
  | Leaf _ -> true
  | _ -> false

let is_node t = match t.node.term with
  | Node _ -> true
  | _ -> false

let hd_term t = match t.node.term with
  | Leaf _ -> Some t
  | Var _ -> None
  | Node (h::_) -> Some h
  | Node _ -> assert false

let hd_symbol t = match hd_term t with
  | None -> None
  | Some ({node={term=Leaf s}}) -> Some s
  | Some _ -> assert false

let true_term = mk_leaf true_symbol bool_sort
let false_term = mk_leaf false_symbol bool_sort

(* constructors for terms *)
let check_bool t = assert (t.node.sort = bool_sort)

let mk_not t = (check_bool t; mk_apply not_symbol bool_sort [t])
let mk_and a b = (check_bool a; check_bool b; mk_apply and_symbol bool_sort [a; b])
let mk_or a b = (check_bool a; check_bool b; mk_apply or_symbol bool_sort [a; b])
let mk_imply a b = (check_bool a; check_bool b; mk_apply imply_symbol bool_sort [a; b])
let mk_eq a b = (assert (a.node.sort = b.node.sort); mk_apply eq_symbol bool_sort [a; b])
let mk_lambda t = mk_apply lambda_symbol t.node.sort [t]
let mk_forall t = (check_bool t; mk_apply forall_symbol bool_sort [mk_lambda t])
let mk_exists t = (check_bool t; mk_apply exists_symbol bool_sort [mk_lambda t])

let rec member_term a b = a == b || match b.node.term with
  | Leaf _ | Var _ -> false
  | Node subterms -> List.exists (member_term a) subterms

let eq_foterm x y = x == y  (* because of hashconsing *)

let compare_foterm x y = x.tag - y.tag

let rec cast t sort =
  match t.node.term with
  | Var _ | Leaf _ -> H.hashcons terms { t.node with sort=sort }
  | Node (h::tail) -> mk_node ((cast h sort) :: tail)
  | Node [] -> assert false

let rec at_pos t pos = match t.node.term, pos with
  | _, [] -> t
  | Leaf _, _::_ | Var _, _::_ -> invalid_arg "wrong position in term"
  | Node l, i::subpos when i < List.length l ->
      at_pos (Utils.list_get l i) subpos
  | _ -> invalid_arg "index too high for subterm"

let rec replace_pos t pos new_t = match t.node.term, pos with
  | _, [] -> new_t
  | Leaf _, _::_ | Var _, _::_ -> invalid_arg "wrong position in term"
  | Node l, i::subpos when i < List.length l ->
      let new_subterm = replace_pos (Utils.list_get l i) subpos new_t in
      mk_node (Utils.list_set l i new_subterm)
  | _ -> invalid_arg "index too high for subterm"

let vars_of_term t = Lazy.force t.node.vars

let is_ground_term t = match vars_of_term t with
  | [] -> true
  | _ -> false

let merge_varlist l1 l2 = Utils.list_merge compare_foterm l1 l2

let max_var vars =
  let rec aux idx = function
  | [] -> idx
  | ({node={term=Var i}}::vars) -> aux (max i idx) vars
  | _::vars -> assert false
  in
  aux 0 vars

let min_var vars =
  let rec aux idx = function
  | [] -> idx
  | ({node={term=Var i}}::vars) -> aux (min i idx) vars
  | _::vars -> assert false
  in
  aux max_int vars

(* check whether the term is a term or an atomic proposition *)
let rec atomic t = match t.node.term with
  | Leaf s -> t.node.sort <> bool_sort || (not (s = and_symbol || s = or_symbol
    || s = forall_symbol || s = exists_symbol || s = imply_symbol
    || s = not_symbol || s = eq_symbol))
  | Var _ -> true
  | Node (hd::_) -> atomic hd
  | Node [] -> assert false

(* check whether the term contains connectives or quantifiers *)
let rec atomic_rec t = match t.node.term with
  | Leaf s -> t.node.sort <> bool_sort || (not (s = and_symbol || s = or_symbol
    || s = forall_symbol || s = exists_symbol || s = imply_symbol
    || s = not_symbol || s = eq_symbol))
  | Var _ -> true
  | Node l -> List.for_all atomic_rec l

(* check wether the term is closed w.r.t. De Bruijn variables *)
let db_closed t = Lazy.force t.node.db_closed

(* check whether t contains the De Bruijn symbol n *)
let rec db_contains t n = match t.node.term with
  | Leaf s when s = db_symbol -> n = 0
  | Leaf _ | Var _ -> false
  | Node [{node={term=Leaf s}}; t'] when s = lambda_symbol -> db_contains t' (n+1)
  | Node [{node={term=Leaf s}}; t'] when s = succ_db_symbol -> db_contains t' (n-1)
  | Node l -> List.exists (fun t' -> db_contains t' n) l

(* replace 0 by s in t *)
let db_replace t s =
  (* lift the De Bruijn symbol *)
  let mk_succ db = mk_node [mk_leaf succ_db_symbol univ_sort; db] in
  (* replace db by s in t *)
  let rec replace db s t = match t.node.term with
  | _ when eq_foterm t db -> s
  | Leaf _ | Var _ -> t
  | Node (({node={term=Leaf symb}} as hd)::tl) when symb = lambda_symbol ->
    (* lift the De Bruijn to replace *)
    mk_node (hd :: (List.map (replace (mk_succ db) s) tl))
  | Node ({node={term=Leaf s}}::_) when s = succ_db_symbol || s = db_symbol ->
    t (* no the good De Bruijn symbol *)
  | Node l -> mk_node (List.map (replace db s) l)
  (* replace the 0 De Bruijn index by s in t *)
  in
  replace (mk_leaf db_symbol univ_sort) s t

(* create a De Bruijn variable of index n *)
let rec db_make n sort = match n with
  | 0 -> mk_leaf db_symbol sort
  | n when n > 0 ->
    let next = db_make (n-1) sort in
    mk_apply succ_db_symbol sort [next]
  | _ -> assert false

(* unlift the term (decrement indices of all De Bruijn variables inside *)
let db_unlift t =
  (* int indice of this DB term *)
  let rec db_index t = match t.node.term with
    | Leaf s when s = db_symbol -> 0
    | Node [{node={term=Leaf s}}; t'] when s = succ_db_symbol -> (db_index t') + 1
    | _ -> assert false
  (* only unlift DB symbol that are free *)
  and recurse depth t =
    match t.node.term with
    | Leaf s when s = db_symbol && depth = 0 -> assert false (* cannot unlift this *)
    | Leaf _ | Var _ -> t
    | Node [{node={term=Leaf s}}; t'] when s = succ_db_symbol ->
      if db_index t >= depth then t' else t (* unlift only if not bound *)
    | Node [{node={term=Leaf s}} as hd; t'] when s = lambda_symbol ->
      mk_node [hd; recurse (depth+1) t'] (* unlift, but index of unbound variables is +1 *)
    | Node l -> mk_node (List.map (recurse depth) l)
  in recurse 0 t

(* replace v by a De Bruijn symbol in t *)
let db_from_var t v =
  assert (is_var v);
  (* go recursively and replace *)
  let rec replace_and_lift depth t = match t.node.term with
  | Var _ -> if eq_foterm t v then db_make depth v.node.sort else t
  | Leaf _ -> t
  | Node [{node={term=Leaf s}} as hd; t'] when s = lambda_symbol ->
    mk_node [hd; replace_and_lift (depth+1) t']  (* increment depth *) 
  | Node l -> mk_node (List.map (replace_and_lift depth) l)
  (* make De Bruijn index of given index *)
  in
  replace_and_lift 0 t

(* index of the De Bruijn symbol *) 
let rec db_depth t = match t.node.term with
  | Leaf s when s = db_symbol -> 0
  | Node [{node={term=Leaf s}}; t'] when s = succ_db_symbol -> (db_depth t') + 1
  | _ -> failwith "not a proper De Bruijn term"

exception FoundSort of sort

(** [look_db_sort n t] find the sort of the De Bruijn index n in t *)
let look_db_sort index t =
  let rec lookup depth t = match t.node.term with
    | Node ({node={term=Leaf s}}::subterms) when s = lambda_symbol ->
      List.iter (lookup (depth+1)) subterms  (* increment for binder *)
    | Node [{node={term=Leaf s}}; t] when s = succ_db_symbol ->
      lookup (depth-1) t  (* decrement for lifted De Bruijn *)
    | Node l -> List.iter (lookup depth) l
    | Leaf s when s = db_symbol && depth = 0 -> raise (FoundSort t.node.sort)
    | Leaf _ -> ()
    | Var _ -> ()
  in try lookup index t; None
     with FoundSort s -> Some s

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
    method pp : Format.formatter -> foterm -> unit    (** pretty print a term *)
  end

let pp_term_debug =
  (* print a De Bruijn term as nice unicode *)
  let rec pp_db formatter t =
    let n = db_depth t in
    Format.fprintf formatter "•%d" n in
  let _sort = ref false
  and _skip_lambdas = ref true
  and _skip_db = ref true in
  (* printer itself *)
  object (self)
    method pp formatter t =
      (match t.node.term with
      | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; a; b]}}]
        when s = not_symbol && s' = eq_symbol ->
        Format.fprintf formatter "%a != %a" self#pp a self#pp b
      | Node [{node={term=Leaf s}}; a; b] when s = eq_symbol ->
        Format.fprintf formatter "%a = %a" self#pp a self#pp b
      | Node [{node={term=Leaf s}} as hd; t] when s = not_symbol ->
        Format.fprintf formatter "%a%a" self#pp hd self#pp t
      | Node [{node={term=Leaf s}} as hd;
        {node={term=Node [{node={term=Leaf s'}} as hd'; t']}}]
        when (s = forall_symbol || s = exists_symbol) ->
        assert (s' = lambda_symbol);
        if !_skip_lambdas
          then Format.fprintf formatter "%a(%a)" self#pp hd self#pp t'
          else Format.fprintf formatter "%a%a(%a)" self#pp hd self#pp hd' self#pp t'
      | Node [{node={term=Leaf s}}; _] when s = succ_db_symbol && !_skip_db ->
        pp_db formatter t (* print de bruijn symbol *)
      | Node (({node={term=Leaf s}} as head)::args) ->
        (* general case for nodes *)
        if pp_symbol_unicode#infix s
          then begin
            match args with
            | [l;r] -> Format.fprintf formatter "@[<h>(%a %a %a)@]"
                self#pp l self#pp head self#pp r
            | _ -> assert false (* infix and not binary? *)
          end else Format.fprintf formatter "@[<h>%a(%a)@]" self#pp head
            (Utils.pp_list ~sep:", " self#pp) args
      | Leaf s -> pp_symbol_unicode#pp formatter s
      | Var i -> Format.fprintf formatter "X%d" i
      | Node (hd::tl) ->
          Format.fprintf formatter "@[<h>(%a)(%a)@]" self#pp hd
            (Utils.pp_list ~sep:", " self#pp ) tl
      | Node [] -> failwith "bad term");
      (* also print the sort if needed *)
      if !_sort then Format.fprintf formatter ":%s" t.node.sort else ()
    method sort s = _sort := s
    method skip_lambdas s = _skip_lambdas := s
    method skip_db s = _skip_db := s
  end

let pp_term_tstp =
  object (self)
    method pp formatter t =
      (* convert De Bruijn to regular variables *)
      let rec db_to_var varindex t = match t.node.term with
      | Node [{node={term=Leaf s}} as hd; {node={term=Node [{node={term=Leaf s'}}; t']}}]
        when (s = forall_symbol || s = exists_symbol) ->
        (* use a fresh variable, and convert to a named-variable representation *)
        let v = mk_var !varindex t'.node.sort in
        incr varindex;
        db_to_var varindex (mk_node [hd; v; db_unlift (db_replace t' v)])
      | Leaf _ | Var _  -> t
      | Node l -> mk_node (List.map (db_to_var varindex) l)
      (* recursive printing function *)
      and pp_rec t = match t.node.term with
      | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; a; b]}}]
        when s = not_symbol && s' = eq_symbol ->
        Format.fprintf formatter "%a != %a" self#pp a self#pp b
      | Node [{node={term=Leaf s}} as hd; t] when s = not_symbol ->
        Format.fprintf formatter "%a%a" self#pp hd self#pp t
      | Node [{node={term=Leaf s}} as hd; v; t']
        when (s = forall_symbol || s = exists_symbol) ->
        assert (is_var v);
        Format.fprintf formatter "%a[%a]: %a" self#pp hd self#pp v self#pp t'
      | Node [{node={term=Leaf s}}; _] when s = succ_db_symbol ->
        failwith "De Bruijn symbol in term, cannot be printed in TSTP"
      | Leaf s when s = db_symbol ->
        failwith "De Bruijn symbol in term, cannot be printed in TSTP"
      | Node (({node={term=Leaf s}} as head)::args) ->
        (* general case for nodes *)
        if pp_symbol_tstp#infix s
          then begin
            match args with
            | [l;r] -> Format.fprintf formatter "@[<h>(%a %a %a)@]"
                self#pp l self#pp head self#pp r
            | _ -> assert false (* infix and not binary? *)
          end else Format.fprintf formatter "@[<h>%a(%a)@]" self#pp head
            (Utils.pp_list ~sep:", " self#pp) args
      | Leaf s -> pp_symbol_tstp#pp formatter s
      | Var i -> Format.fprintf formatter "X%d" i
      | Node (hd::tl) ->
          Format.fprintf formatter "@[<h>(%a)(%a)@]" self#pp hd
            (Utils.pp_list ~sep:", " self#pp ) tl
      | Node [] -> failwith "bad term";
      in
      let maxvar = max_var (vars_of_term t) in
      let varindex = ref (maxvar+1) in
      (* convert everything to named variables, then print *)
      pp_rec (db_to_var varindex t)
  end

let pp_term = ref (pp_term_debug :> pprinter_term)

let pp_signature formatter symbols =
  Format.fprintf formatter "@[<h>sig %a@]"
    (Utils.pp_list ~sep:" > " !pp_symbol#pp) symbols
