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

(* constructors *)
let mk_var idx sort =
  let my_v = {term = Var idx; sort=sort; vars=lazy []} in
  let v = H.hashcons terms
    {my_v with vars=lazy [H.hashcons terms my_v]} in
  ignore (Lazy.force v.node.vars); v

let mk_leaf symbol sort =
  H.hashcons terms {term = Leaf symbol; sort=sort; vars=lazy []}

let rec mk_node = function
  | [] -> failwith "cannot build empty node term"
  | [_] -> failwith "cannot build node term with no arguments"
  | (head::_) as subterms ->
      let my_t = {term=(Node subterms); sort=head.node.sort; vars=lazy []} in
      let lazy_vars = lazy (compute_vars (H.hashcons terms my_t)) in
      let t = H.hashcons terms { my_t with vars=lazy_vars } in
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

let merge_varlist l1 l2 = List.merge compare_foterm l1 l2

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

let pp_symbol formatter s = Format.pp_print_string formatter s
  
(* readable representation of a term *)
let rec pp_foterm formatter t =
  (* print a De Bruijn symbol *)
  let rec pp_db formatter t =
    let n = db_depth t in
    Format.fprintf formatter "•%d" n
  (* compute index of a De Bruijn symbol *)
  and db_depth t = match t.node.term with
  | Leaf s when s = db_symbol -> 0
  | Node [{node={term=Leaf s}}; t'] when s = succ_db_symbol -> (db_depth t') + 1
  | _ -> assert false
  in
  match t.node.term with
  | Node [{node={term=Leaf s}} as hd; t] when s = not_symbol ->
    Format.fprintf formatter "%a%a" pp_foterm hd pp_foterm t
  | Node [{node={term=Leaf s}} as hd; {node={term=Node [{node={term=Leaf s'}}; t]}}]
    when (s = forall_symbol || s = exists_symbol) && s' = lambda_symbol ->
    Format.fprintf formatter "%a%a" pp_foterm hd pp_foterm t
  | Node [{node={term=Leaf s}}; _] when s = succ_db_symbol ->
    pp_db formatter t (* print de bruijn symbol *)
  | Node (({node={term=Leaf s}} as head)::args) ->
    (* general case for nodes *)
    if is_infix_symbol s
      then begin
        match args with
        | [l;r] -> Format.fprintf formatter "@[<h>(%a %a %a)@]" pp_foterm l
            pp_foterm head pp_foterm r
        | _ -> assert false (* infix and not binary? *)
      end else Format.fprintf formatter "@[<h>%a(%a)@]" pp_foterm head
        (Utils.pp_list ~sep:", " pp_foterm) args
  | Leaf s when s = not_symbol -> Format.pp_print_string formatter "•¬"
  | Leaf s when s = eq_symbol -> Format.pp_print_string formatter "•="
  | Leaf s when s = lambda_symbol -> Format.pp_print_string formatter "•λ"
  | Leaf s when s = exists_symbol -> Format.pp_print_string formatter "•∃"
  | Leaf s when s = forall_symbol -> Format.pp_print_string formatter "•∀"
  | Leaf s when s = and_symbol -> Format.pp_print_string formatter "•&"
  | Leaf s when s = or_symbol -> Format.pp_print_string formatter "•|"
  | Leaf s when s = imply_symbol -> Format.pp_print_string formatter "•→"
  | Leaf s when s = db_symbol -> pp_db formatter t
  | Leaf s -> Format.pp_print_string formatter s
  | Var i -> Format.fprintf formatter "X%d" i
  | Node _ -> failwith "bad term"

let rec pp_foterm_sort formatter ?(sort=false) t =
  if not sort then pp_foterm formatter t  (* not debugging... *)
  else
  (match t.node.term with
  | Leaf x -> pp_symbol formatter x
  | Var i -> Format.fprintf formatter "X%d" i
  | Node (head::args) -> Format.fprintf formatter
      "@[<h>%a(%a)@]" (pp_foterm_sort ~sort:false) head
      (Utils.pp_list ~sep:", " (pp_foterm_sort ~sort)) args
  | Node [] -> failwith "bad term");
  if sort then Format.fprintf formatter ":%s" t.node.sort else ()


let pp_signature formatter symbols =
  Format.fprintf formatter "@[<h>sig %a@]" (Utils.pp_list ~sep:" > " pp_symbol) symbols
