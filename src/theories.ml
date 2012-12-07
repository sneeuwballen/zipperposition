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

(** Recognition of theories *)

open Types
open Symbols

module T = Terms
module C = Clauses
module Utils = FoUtils

(* ----------------------------------------------------------------------
 * recognize some shapes of clauses
 * ---------------------------------------------------------------------- *)

(** Does t contains the symbol f? *)
let rec contains_symbol f t =
  match t.term with
  | Var _ -> false
  | Node (g, _) when f == g -> true
  | Node (_, ts) -> List.exists (contains_symbol f) ts

(** Recognized whether the clause is a Range-Restricted Horn clause *)
let is_RR_horn_clause hc = 
  let lit = ref None in
  (* find whether there is exactly one positive literal *)
  let rec find_uniq_pos n i =
    if i = Array.length hc.hclits
      then if n = 1 then !lit else None
      else begin
        match hc.hclits.(i) with
        | Equation (l,r,true,_) as lit' ->
          lit := Some lit';
          find_uniq_pos (n+1) (i+1)
        | _ -> find_uniq_pos n (i+1)
      end
  in
  match find_uniq_pos 0 0 with
  | None -> false
  | Some lit' -> (* check that all variables of the clause occur in the head *)
    List.length (C.vars_of_lit lit') = List.length hc.hcvars

(** Check whether the clause defines a symbol, e.g.
    subset(X,Y) = \forall Z(Z in X -> Z in Y). It means the LHS
    is a flat symbol with variables, and all variables in RHS
    are also in LHS *)
let is_definition hc =
  (* check that r is a definition of l=f(x1,...,xn) *)
  let check_def l r =
    match l.term with
    | Var _ -> false
    | Node (f, ts) ->
      (* l=f(x1,...,xn) where r contains no other var than x1,...,xn, and n > 0 *)
      T.atomic_rec l && ts <> [] && not (contains_symbol f r) && l != T.true_term && r != T.true_term
      && List.for_all T.is_var ts && List.for_all (fun x -> T.var_occurs x l) r.vars
  in
  match hc.hclits with
  | [|Equation (({term=Node(_, _)} as l), r, true, _)|] when check_def l r -> Some (l, r)
  | [|Equation (l, ({term=Node(_, _)} as r), true, _)|] when check_def r l -> Some (r, l)
  | _ -> None

(** More general than definition. It means the clause is an
    equality where all variables in RHS are also in LHS. It
    can return two rewrite rules if the clause can be oriented
    in both ways, e.g. associativity axiom. *)
let is_rewrite_rule hc =
  (* check that l -> r is an acceptable rewrite rule *)
  let check_rule l r =
    match l.term with
    | Var _ -> false
    | Node (_, _) ->
      T.atomic_rec l && l != T.true_term && r != T.true_term && List.for_all (fun x -> T.var_occurs x l) r.vars
  in
  match hc.hclits with
  | [|Equation (l, r, true, _)|] ->
    (if check_rule l r then [l, r] else []) @ (if check_rule r l then [r, l] else [])
  | _ -> []

let is_pos_eq hc =
  match hc.hclits with
  | [|Equation (l,r,true,_)|] -> Some (l,r)
  | _ -> None


(** detect whether the clause is "p(x,y,z) & p(x,y,z') => z=z'", and
    returns p in this case *)
let is_functional_symbol hc =
  (* kind of the literal *)
  let rec kind = function
  | Equation (l,r,false,_) when r == T.true_term -> atom_kind l false
  | Equation (l,r,false,_) when l == T.true_term -> atom_kind r false
  | Equation ({term=Var i}, {term=Var j},true,_) -> `Eq (i,j)
  | _ -> `None
  (* kind of atom, with given sign *)
  and atom_kind t sign = match t.term with
  | Node (p, [{term=Var i};{term=Var j}; {term=Var k}]) -> `Ternary (p,i,j,k)
  | _ -> `None
  in
  match hc.hclits with
  | [|a;b;c|] ->
    (match kind a, kind b, kind c with
    | `Ternary (p,i,j,k), `Ternary (p',i',j',k'), `Eq (k1, k2)
    | `Ternary (p,i,j,k), `Eq (k1, k2), `Ternary (p',i',j',k')
    | `Eq (k1, k2), `Ternary (p,i,j,k), `Ternary (p',i',j',k') ->
    if p == p' && i=i' && j=j' && ((k=k1 && k'=k2) || (k=k2 && k'=k1))
        then begin 
          Utils.debug 1 (lazy (Utils.sprintf "%% ternary predicate %s is functional" (name_symbol p)));
          `Functional p
        end else `None
    | _ -> `None)
  | _ -> `None

(** detect whether the clause is "p(x,y,f(x,y))", and returns (p,f)
    in this case *)
let is_total_symbol hc =
  let is_total t = match t.term with
  | Node (p, [{term=Var i}; {term=Var j}; {term=Node(f, [{term=Var i'}; {term=Var j'}])}])
    when i = i' && j = j' ->
    (* total ternary relation detected *)
    Utils.debug 1 (lazy (Utils.sprintf "%% ternary predicate %s is total" (name_symbol p)));
    `Total (p, f)
  | _ -> `None
  in
  match hc.hclits with
  | [| Equation (l, r, true,_) |] when r == T.true_term -> is_total l
  | [| Equation (l, r, true,_) |] when l == T.true_term -> is_total r
  | _ -> `None

(* ----------------------------------------------------------------------
 * add some axioms when detecting some axioms
 * ---------------------------------------------------------------------- *)

(** define f(x,y)=z as p(x,y,z) *)
let function_definition ~ord p f =
  let x, y, z = T.mk_var 1 univ_sort, T.mk_var 2 univ_sort, T.mk_var 3 univ_sort in
  let lhs = T.mk_node p bool_sort [x;y;z]
  and rhs = T.mk_eq (T.mk_node f univ_sort [x;y]) z in
  let axiom_name = Utils.sprintf "function_definition_%s_%s" (name_symbol p) (name_symbol f) in
  (* TODO a proper proof ;) *)
  let proof = Lazy.lazy_from_val (Axiom (axiom_name, "/dev/null.ax")) in
  C.mk_hclause ~ord [C.mk_eq ~ord lhs rhs] proof []

let detect_total_relations ~ord clauses =
  let totals = ref []
  and functionals = ref [] in
  (* detect axioms *)
  List.iter
    (fun hc ->
      match is_functional_symbol hc, is_total_symbol hc with
      | `Functional p, _ -> functionals := p :: !functionals
      | _, `Total (p, f) -> totals := (p,f) :: !totals
      | `None, `None -> ()
      | _ -> assert false)
    clauses;
  (* perform a join on totals,functionals to find common predicate symbols *)
  let definitions = ref [] in
  List.iter
    (fun (p,f) ->
      List.iter
        (fun p' -> if p == p'
          then begin
            Utils.debug 0 (lazy (Utils.sprintf "%% symbol %s is a function definition of %s"
                          (name_symbol p) (name_symbol f)));
            definitions := (function_definition ~ord p f) :: !definitions
          end)
        !functionals)
    !totals;
  (* return definitions *)
  !definitions
