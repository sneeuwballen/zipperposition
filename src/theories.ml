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

type name = string
  (** The name of a formula. If a formula is part of a known axiomatisation
      it can have a specific name, otherwise just "lemmaX" with X a number
      (e.g. f(X,Y)=f(Y,X) is named "commutativity") *)

let string_of_name name = name

type named_formula = {
  nf_name : name;
  nf_vars : int list;                   (* symbols (indexes) to bind. Order matters. *)
  nf_pclause : Patterns.pclause;        (* the pattern of the formula itself *)
} (** A named formula is a pattern clause, plus a name (used for the datalog
      representation of instances of this formula *)

type theory = {
  th_name : name;                       (* name of the theory *)
  th_vars : int list;                   (* symbols to bind *)
  th_definition : named_formula list;   (* definition (set of axioms) *)
} (** A theory is a named set of formulas (axioms) *)

type lemma = {
  lemma_name : name;                    (* unique name of the lemma *)
  lemma_conclusion : named_formula;     (* conclusion of the lemma *)
  lemma_premises : named_formula list;  (* hypotheses of the lemma *)
  lemma_vars : int list;                (* symbols to instantiate *)
} (** A lemma is a named formula that can be deduced from a list
      of other named formulas. It will be translated as a datalog rule. *)

type kb = {
  mutable kb_name_idx : int;
  mutable kb_lemma_idx : int;
  mutable kb_potential_lemmas : lemma list;           (** potential lemma, to explore *)
  mutable kb_formulas : named_formula Patterns.Map.t; (** named formulas, indexed by pattern *)
  kb_theories : (name, theory) Hashtbl.t;             (** theories, with their name *)
  mutable kb_lemmas : (name, lemma) Hashtbl.t;        (** lemma *)
} (** a Knowledge Base for lemma and theories *)

(** Create an empty Knowledge Base *)
let empty_kb () = {
  kb_name_idx = 0;
  kb_lemma_idx = 0;
  kb_potential_lemmas = [];
  kb_formulas = Patterns.Map.create ();
  kb_theories = Hashtbl.create 5;
  kb_lemmas = Hashtbl.create 5;
}

(** Add a potential lemma to the KB. The lemma must be checked before
    it is used. *)
let add_potential_lemmas kb pot_lemmas =
  let kb_potential_lemmas =
    List.fold_left (fun kb_potential_lemmas lemma ->
      if List.mem lemma kb_potential_lemmas then kb_potential_lemmas
        else lemma :: kb_potential_lemmas)
    kb.kb_potential_lemmas pot_lemmas in
  kb.kb_potential_lemmas <- kb_potential_lemmas

(* ----------------------------------------------------------------------
 * reasoning over a problem using Datalog
 * ---------------------------------------------------------------------- *)

type meta_prover = {
  meta_db : Datalog.Logic.db;
  meta_kb : kb;
  meta_ord : ordering;
  mutable meta_lemmas : hclause list;
} (** The main type used to reason over the current proof, detecting axioms
      and theories, inferring lemma... *)

(** When a lemma is discovered, this translates it into a hclause and
    adds it to the list of lemmas. The [term] is a datalog term whose head
    symbol is "lemma". *)
let lemma_handler ~ord meta term =
  assert (Array.length term >= 2);
  assert (Datalog.Symbols.get_symbol term.(0) = "lemma");
  (* extract the name of the lemma, and the list of symbols *)
  let lemma_name = Datalog.Symbols.get_symbol term.(1) in
  let lemma_args = Array.to_list (Array.sub term 2 (Array.length term - 2)) in
  let lemma_args = List.map
    (fun s -> mk_symbol (Datalog.Symbols.get_symbol s)) lemma_args in
  (* find the definition of the lemma *)
  let lemma =
    try Hashtbl.find meta.meta_kb.kb_lemmas lemma_name
    with Not_found ->
      failwith ("no such named formula: " ^ lemma_name)
  in
  (* bind the pattern symbols with the proper, concrete problem symbols *)
  assert (List.length lemma_args = List.length lemma.lemma_vars);
  let mapping = List.fold_left2 Patterns.bind_symbol
    Patterns.empty_mapping lemma.lemma_vars lemma_args in
  Utils.debug 1 (lazy (Utils.sprintf "%% instantiate lemma %s with mapping %a"
                 (string_of_name lemma_name) Patterns.pp_mapping mapping));
  (* instantiate the premises and conclusion of the lemma *)
  let parents = List.map
    (fun nf ->
      let proof = Axiom ("kb", "kb") and parents = [] in
      Patterns.instantiate_pclause ~ord ~map:mapping nf.nf_pclause proof parents)
    lemma.lemma_premises in
  let premises = List.map (fun pc -> (C.base_clause pc, [], S.id_subst)) parents in
  let proof = Proof ("lemma", premises) in
  let conclusion = Patterns.instantiate_pclause ~map:mapping ~ord
    lemma.lemma_conclusion.nf_pclause proof parents in
  (* add the lemma in meta.meta_lemmas *)
  meta.meta_lemmas <- conclusion :: meta.meta_lemmas

let convert_vars vars = List.map (fun v -> `Var (-v)) vars

(** Add a lemma to the Datalog engine *)
let db_add_lemma db lemma =
  (* add lemma(name, args) :- premise1(args), ..., premise_n(args). *)
  let () =
    let vars = convert_vars lemma.lemma_vars in
    let head = Datalog.Logic.mk_term "lemma" vars in
    let body = List.map
      (fun premise ->
        let args = convert_vars premise.nf_vars in
        Datalog.Logic.mk_term premise.nf_name args)
      lemma.lemma_premises in
    let rule = Datalog.Logic.mk_rule head body in
    Utils.debug 2 (lazy (Utils.sprintf "%% add rule @[<h>%a@] to meta-prover"
                   (Datalog.Logic.pp_rule ?to_s:None) rule));
    Datalog.Logic.db_add db rule;
  in
  (* add conclusion(args) :- premise1(args), ..., premise_n(args), for
     further propagations. *)
  let () =
    let vars = convert_vars lemma.lemma_conclusion.nf_vars in
    let head = Datalog.Logic.mk_term lemma.lemma_conclusion.nf_name vars in
    let body = List.map
      (fun premise ->
        let args = convert_vars premise.nf_vars in
        Datalog.Logic.mk_term premise.nf_name args)
      lemma.lemma_premises in
    let rule = Datalog.Logic.mk_rule head body in
    Utils.debug 2 (lazy (Utils.sprintf "%% add rule @[<h>%a@] to meta-prover"
                   (Datalog.Logic.pp_rule ?to_s:None)  rule));
    Datalog.Logic.db_add db rule;
  in
  ()
  
(** Add the definition of a theory to the Datalog engine *)
let db_add_theory db theory =
  let vars = convert_vars theory.th_vars in
  let head = Datalog.Logic.mk_term "theory" vars in
  let body = List.map
    (fun premise ->
      let args = convert_vars premise.nf_vars in
      Datalog.Logic.mk_term premise.nf_name args)
    theory.th_definition in
  let rule = Datalog.Logic.mk_rule head body in
  Utils.debug 2 (lazy (Utils.sprintf "%% add rule @[<h>%a@] to meta-prover"
                 (Datalog.Logic.pp_rule ?to_s:None) rule));
  Datalog.Logic.db_add db rule

(** Create a meta_prover, using a knowledge base *)
let create_meta ~ord kb =
  let meta = {
    meta_db = Datalog.Logic.db_create ();
    meta_kb = kb;
    meta_ord = ord;
    meta_lemmas = [];
  } in
  (* set lemma handler, that captures newly discovered lemma *)
  let lemma_symbol = Datalog.Symbols.mk_symbol "lemma" in
  Datalog.Logic.db_subscribe meta.meta_db lemma_symbol (lemma_handler ~ord meta);
  (* add definitions of lemma *) 
  Hashtbl.iter (fun _ lemma -> db_add_lemma meta.meta_db lemma) meta.meta_kb.kb_lemmas;
  (* add definitions of theories *)
  Hashtbl.iter (fun _ theory -> db_add_theory meta.meta_db theory) meta.meta_kb.kb_theories; 
  (* return the meta-prover *)
  meta

(** Scan the given clause to recognize if it matches axioms from the KB;
    if it does, return the lemma that are newly discovered by the Datalog engine.

    It returns lemma that have been discovered by adding the clause. Those
    lemma can be safely added to the problem.
    *)
let scan_clause meta hc =
  meta.meta_lemmas <- [];
  (* retrieve patterns that match this clause *)
  Patterns.Map.retrieve meta.meta_kb.kb_formulas hc ()
    (fun () pclause mapping nf ->
      (* a named formula is detected, assert the corresponding datalog
         predicate *)
      let args = List.map
        (fun s -> `Symbol (name_symbol (Ptmap.find s mapping.Patterns.m_symbol)))
        nf.nf_vars in
      let term = Datalog.Logic.mk_term nf.nf_name args in
      let rule = Datalog.Logic.mk_rule term [] in
      Utils.debug 1 (lazy (Utils.sprintf "%% detected formula %a in problem"
                     (Datalog.Logic.pp_rule ?to_s:None) rule));
      (* add fact *)
      Datalog.Logic.db_add meta.meta_db rule);
  (* get lemmas, and clear the list for next use *)
  let lemmas = meta.meta_lemmas in
  meta.meta_lemmas <- [];
  lemmas

(* ----------------------------------------------------------------------
 * (heuristic) search of "interesting" lemma in a proof.
 * ---------------------------------------------------------------------- *)

(** Heuristic "simplicity and elegance" measure for clauses. The smaller,
    the better. *)
let rate_clause pclause =
  let rate = ref 1. in
  (* many symbols is not simple *)
  let symbols = Patterns.pclause_symbols pclause in
  let num_symbols = List.length symbols in
  rate := !rate +. (float_of_int (num_symbols - 1));
  (* many literals is not simple *)
  let length = List.length pclause.Patterns.pc_lits in
  rate := !rate +. (2. *. float_of_int (length - 1));
  (* result *)
  Utils.debug 2 (lazy (Utils.sprintf "%% simplicity of @[<h>%a@] is %.2f (%d symbols, %d lits)"
                Patterns.pp_pclause pclause !rate num_symbols length));
  !rate 
  

(** given an empty clause (and its proof), look in the proof for
    potential lemma. *)
let search_lemmas hc =
  assert (hc.hclits = [||]);
  []  (* TODO *)

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
  | Unix.Unix_error _ -> empty_kb ()
  | Failure e -> Format.printf "%% [error while reading %s: %s]" filename e; empty_kb ()

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
