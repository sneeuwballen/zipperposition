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

let rewrite_peano_lit ~ctx lit =
  match lit with
  | Equation (l, r, sign, _) ->
    let l' = rewrite_peano l in
    let r' = rewrite_peano r in
    if l == l' && r == r'
      then lit  (* same literal *)
      else Literals.mk_lit ~ord:ctx.ctx_ord l' r' sign

(** Simplification rule that reduces arithmetic expressions *)
let rewrite_peano_clause hc =
  FoUtils.debug 1 "%% try to Peano evaluate %a" !C.pp_clause#pp_h hc;
  let ctx = hc.hcctx in
  let lits = hc.hclits in
  let lits' = Array.map (rewrite_peano_lit ~ctx:hc.hcctx) lits in
  if Literals.eq_lits lits lits'
    then [hc]  (* same clause *)
    else begin  (* construct new clause *)
      let proof c' = Proof (c', "peano", [hc.hcproof]) in
      let parents = [hc] in
      let new_hc = C.mk_hclause_a ~parents ~ctx lits' proof in
      FoUtils.debug 3 "@[<h>rewrite with peano %a into %a@]"
                     !C.pp_clause#pp hc !C.pp_clause#pp_h new_hc;
      (* return simplified clause *)
      [new_hc]
    end

let ext =
  let open Extensions in
  Format.printf "@[<hov2>TRS:@; %a@]@." Rewriting.TRS.pp_trs trs;
  let actions = [Ext_simplification_rule rewrite_peano_clause] in
  { name = "peano";
    actions;
  }

let _ =
  Extensions.register ext
