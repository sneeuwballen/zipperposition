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

(** {1 Toy plugin for Peano arithmetic} *)

open Basic
open Symbols

module T = Terms
module C = Clauses

(* term rewriting system for Peano *)
let trs =
  let trs = Rewriting.TRS.create () in
  let s_symb = Symbols.mk_symbol "s" in
  let z_symb = Symbols.mk_symbol "zero" in
  let plus_symb = Symbols.mk_symbol "plus" in
  let mult_symb = Symbols.mk_symbol "mult" in
  (* term constructors *)
  let zero = T.mk_const z_symb univ_ in
  let s x = T.mk_node s_symb univ_ [x] in
  let plus x y = T.mk_node plus_symb univ_ [x; y] in
  let mult x y = T.mk_node mult_symb univ_ [x; y] in
  let x = T.mk_var 0 univ_ in
  let y = T.mk_var 1 univ_ in
  let z = T.mk_var 2 univ_ in
  Rewriting.TRS.add_rules trs
    [ plus (s x) y, s (plus x y);
      plus zero x, x;
      plus (plus x y) z, plus x (plus y z);
      mult zero x, zero;
      mult (s x) y, plus y (mult x y);
    ];
  trs

(** Normal form of t w.r.t to Peano *)
let rewrite_peano t =
  Rewriting.TRS.rewrite trs t

let rec expert ~ctx =
  let open Experts in
  let signature = Symbols.set_of_signature (Rewriting.TRS.signature trs) in
  { expert_name = "peano_arith";
    expert_descr = "evaluation for Peano arithmetic";
    expert_equal = (fun t1 t2 -> rewrite_peano t1 == rewrite_peano t2);
    expert_sig = signature;
    expert_clauses = [];
    expert_canonize = rewrite_peano;
    expert_ord = (fun _ -> true);
    expert_ctx = ctx;
    expert_update_ctx = (fun ctx -> [expert ~ctx]);
    expert_solve = None;
  }

let ext =
  let open Extensions in
  let actions = [Ext_expert expert; Ext_signal_incompleteness] in
  { name = "peano";
    actions;
  }

let _ =
  Extensions.register ext
