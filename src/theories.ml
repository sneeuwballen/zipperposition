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
module S = FoSubst
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

(** Checks whether the clause is "const = ground composite term", e.g.
    a clause "aIbUc = inter(a, union(b, c))". In this case it returns
    Some(constant, definition of constant) *)
let is_const_definition hc =
  match hc.hclits with
  | [|Equation (l,r,true,_)|] when T.is_const l && T.is_ground_term r && not (T.member_term l r) ->
    Some (l,r)
  | [|Equation (l,r,true,_)|] when T.is_const r && T.is_ground_term l && not (T.member_term r l) ->
    Some (r,l)
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

(* TODO detect equivalences in CNF (a => b and b => a) *)

(** define f(x,y)=z as p(x,y,z) *)
let function_definition ~ord def1 def2 p f =
  let x, y, z = T.mk_var 1 univ_sort, T.mk_var 2 univ_sort, T.mk_var 3 univ_sort in
  let lhs = T.mk_node p bool_sort [x;y;z]
  and rhs = T.mk_eq (T.mk_node f univ_sort [x;y]) z in
  let proof = Proof ("lemma", [C.base_clause def1, [], S.id_subst;
                               C.base_clause def2, [], S.id_subst]) in
  C.mk_hclause ~ord [C.mk_eq ~ord lhs rhs] proof []

let detect_total_relations ~ord clauses =
  let totals = ref []
  and functionals = ref [] in
  (* detect axioms *)
  List.iter
    (fun hc ->
      match is_functional_symbol hc, is_total_symbol hc with
      | `Functional p, _ -> functionals := (p, hc) :: !functionals
      | _, `Total (p, f) -> totals := (p, f, hc) :: !totals
      | `None, `None -> ()
      | _ -> assert false)
    clauses;
  (* perform a join on totals,functionals to find common predicate symbols *)
  let definitions = ref [] in
  List.iter
    (fun (p,f,def1) ->
      List.iter
        (fun (p', def2) -> if p == p'
          then begin
            Utils.debug 0 (lazy (Utils.sprintf "%% symbol %s is a function definition of %s"
                          (name_symbol p) (name_symbol f)));
            definitions := (function_definition ~ord def1 def2 p f) :: !definitions
          end)
        !functionals)
    !totals;
  (* return definitions *)
  !definitions

(* ----------------------------------------------------------------------
 * generic representation of theories and formulas (persistent)
 * ---------------------------------------------------------------------- *)

type tterm =
  | TVar of int
  | TNode of string * tterm list
  (** an abstract term *)

type tformula = tterm list
  (** an abstract clause *)

let rec tterm_of_term t = match t.term with
  | Var i -> TVar i
  | Node (f, l) -> TNode (name_symbol f, List.map tterm_of_term l)

let rec tformula_of_hclause hc =
  let convert_lit = function
  | Equation (l,r,true,_) when r == T.true_term -> tterm_of_term l
  | Equation (l,r,true,_) when l == T.true_term -> tterm_of_term r
  | Equation (l,r,false,_) when r == T.true_term -> TNode ("not", [tterm_of_term l])
  | Equation (l,r,false,_) when l == T.true_term -> TNode ("not", [tterm_of_term r])
  | Equation (l,r,true,_) -> TNode ("=", [tterm_of_term l; tterm_of_term r])
  | Equation (l,r,false,_) -> TNode ("!=", [tterm_of_term l; tterm_of_term r])
  in
  Array.to_list (Array.map convert_lit hc.hclits)

type lemma = tformula * tformula list
  (** a lemma is a clause, with some hypothesis *)

type theory = tformula list
  (** a theory is a list of formula *) 

type kb = {
  kb_lemma_idx : int;
  kb_potential_lemmas : lemma list;     (** potential lemma, to explore *)
  kb_lemmas : (int * lemma) list;       (** lemma with their unique ID *)
  kb_theories : (string * theory) list; (** theories, with their name *)
} (** a Knowledge Base for lemma and theories *)

let empty_kb = {
  kb_lemma_idx = 0;
  kb_potential_lemmas = [];
  kb_lemmas = [];
  kb_theories = [];
}

let add_potential_lemmas kb pot_lemmas =
  let kb_potential_lemmas = 
    List.fold_left (fun kb_potential_lemmas lemma ->
      if List.mem lemma kb_potential_lemmas then kb_potential_lemmas
        else lemma :: kb_potential_lemmas)
    kb.kb_potential_lemmas pot_lemmas in
  { kb with kb_potential_lemmas;  }

let add_lemmas kb lemmas =
  let lemmas, idx = List.fold_left
    (fun (lemmas, idx) lemma -> ((idx, lemma) :: lemmas, idx+1))
    (kb.kb_lemmas, kb.kb_lemma_idx) lemmas in
  { kb with kb_lemma_idx=idx; kb_lemmas=lemmas; }

module L = Datalog.Logic

(* ----------------------------------------------------------------------
 * (heuristic) search of "interesting" lemma in a proof.
 * ---------------------------------------------------------------------- *)

(** given an empty clause (and its proof), look in the proof for
    potential lemma. *)
let search_lemmas hc =
  assert (hc.hclits = [||]);
  let axioms = [[TNode ("true", [])]] in
  [(tformula_of_hclause hc, axioms)]  (* TODO *)

(* ----------------------------------------------------------------------
 * serialization/deserialization for abstract logic structures
 * ---------------------------------------------------------------------- *)

exception ReadKB of kb

(* read KB without locking (may crash if wrong format) *)
let read_kb_nolock filename =
  try
    let file = Unix.openfile filename [Unix.O_RDONLY] 0o644 in
    let file = Unix.in_channel_of_descr file in
    let kb = (Marshal.from_channel file : kb) in
    close_in file;
    kb
  with
  | Unix.Unix_error _ -> empty_kb
  | Failure e -> Format.printf "%% [error while reading %s: %s]" filename e; empty_kb


let read_kb ~lock ~file =
  Utils.with_lock_file lock
    (fun () -> read_kb_nolock file)

let save_kb ~lock ~file kb =
  Utils.with_lock_file lock
    (fun () ->
    let out = Unix.openfile file [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
    let out = Unix.out_channel_of_descr out in
    Marshal.to_channel out kb [];
    flush out;
    close_out out)

let update_kb ~lock ~file f =
  Format.printf "%% update knowledge base... ";
  Format.print_flush ();
  Utils.with_lock_file lock
    (fun () ->
    let kb = read_kb_nolock file in
    (* tranform kb with function *)
    let kb = f kb in
    (* write modified kb to file *)
    let out = Unix.openfile file [Unix.O_CREAT; Unix.O_WRONLY] 0o644 in
    let out = Unix.out_channel_of_descr out in
    Marshal.to_channel out kb [];
    flush out;
    close_out out);
  Format.printf "done@."
