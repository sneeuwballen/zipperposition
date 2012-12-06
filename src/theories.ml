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
  
