
(*
Zipperposition: a functional superposition prover for prototyping
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

(** {6 AC redundancy} *)

open Logtk
open Logtk_meta

module HOT = HOTerm
module T = FOTerm
module C = Clause
module Lit = Literal

let prof_simplify = Util.mk_profiler "ac.simplify"

let stat_ac_simplify = Util.mk_stat "ac.simplify"
let stat_ac_redundant = Util.mk_stat "ac.redundant"

type spec = Theories.AC.t

let axioms ~ctx s =
  let signature = Ctx.signature ~ctx in
  let ord = Ctx.ord ~ctx in
  let ty = Signature.find signature s in
  match ty with
  | Type.Fun (ret, [ret1;ret2]) when Type.eq ret ret1 && Type.eq ret ret2 ->
    (* type is ok. *)
    let x = T.mk_var ~ty:ret 0 in
    let y = T.mk_var ~ty:ret 1 in
    let z = T.mk_var ~ty:ret 2 in
    let f x y = T.mk_node s [x;y] in
    let res = ref [] in
    (* build clause l=r *)
    let add_clause l r =
      let name = Util.sprintf "ac_%a_%d" Symbol.pp s (List.length !res) in
      let proof cc = Proof.mk_c_axiom cc ~file:"" ~name in
      let c = C.create ~ctx [ Lit.mk_eq ~ord l r ] proof in
      C.set_flag C.flag_persistent c true;
      res := c :: !res
    in
    add_clause (f x y) (f y x);
    add_clause (f (f x y) z) (f x (f y z));
    add_clause (f x (f y z)) (f z (f x y));
    add_clause (f x (f y z)) (f y (f x z));
    add_clause (f x (f y z)) (f z (f y x));
    !res
  | _ ->
    Util.debug 1 "AC symbol %a has wrong type %a" Symbol.pp s Type.pp ty;
    assert false
  
(** {2 Rules} *)

let is_trivial_lit ~spec lit =
  if not (Theories.AC.exists_ac ~spec) then false else
  let is_ac = Theories.AC.is_ac ~spec in
  match lit with
  | Lit.Equation (l, r, true, _) -> T.ac_eq ~is_ac l r
  | Lit.Equation _
  | Lit.Prop _
  | Lit.False -> false
  | Lit.True -> true

let is_trivial ~spec c =
  let res = Util.array_exists (is_trivial_lit ~spec) c.C.hclits in
  if res then Util.incr_stat stat_ac_redundant;
  res

(* simplify: remove literals that are redundant modulo AC *)
let simplify ~spec ~ctx c =
  Util.enter_prof prof_simplify;
  if not (Theories.AC.exists_ac ~spec) then c else
  let n = Array.length c.C.hclits in
  let is_ac = Theories.AC.is_ac ~spec in
  let lits = Array.to_list c.C.hclits in
  let lits = List.filter
    (fun lit -> match lit with
    | Lit.Equation (l, r, false, _) -> not (T.ac_eq ~is_ac l r)
    | Lit.Equation _
    | Lit.Prop _
    | Lit.False
    | Lit.True -> true)
    lits
  in
  let n' = List.length lits in
  if n' < n && not (C.get_flag C.flag_persistent c)
    then begin
      let symbols = Theories.AC.symbols_of_terms ~spec (C.terms c) in
      let symbols = Sequence.to_list (Symbol.SSet.to_seq symbols) in
      let ac_proof = Util.list_flatmap (Theories.AC.find_proof ~spec) symbols in
      let premises = c.C.hcproof :: ac_proof in
      let proof cc = Proof.mk_c_step cc ~rule:"ac" premises in
      let parents = c :: c.C.hcparents in
      let new_c = C.create ~parents ~ctx lits proof in
      Util.exit_prof prof_simplify;
      Util.incr_stat stat_ac_simplify;
      Util.debug 3 "%a AC-simplify into %a" C.pp c C.pp new_c;
      new_c
    end else
      let _ = Util.exit_prof prof_simplify in
      c (* no simplification *)

let add_ac ?proof ~env s =
  Util.debug 1 "enable AC redundancy criterion for %a" Symbol.pp s;
  let ctx = Env.ctx env in
  let spec = Ctx.ac ~ctx in
  (* is this the first case of AC symbols? If yes, then add inference rules *)
  let first = not (Theories.AC.exists_ac ~spec) in
  if first then begin
    Env.add_is_trivial ~env (is_trivial ~spec);
    Env.add_simplify ~env (simplify ~spec ~ctx);
    end;
  (* remember that the symbols is AC *)
  Ctx.add_ac ?proof ~ctx s;
  (* add clauses *)
  let clauses = axioms ~ctx s in
  Env.add_passive ~env (Sequence.of_list clauses);
  ()

let setup_penv ~penv =
  ()

let setup_env ~env =
  let ctx = Env.ctx env in
  let spec = Ctx.ac ctx in
  (* enable AC inferences if needed *)
  if Theories.AC.exists_ac ~spec
    then begin
    Env.add_is_trivial ~env (is_trivial ~spec);
    Env.add_simplify ~env (simplify ~spec ~ctx);
    end;
  match Env.get_meta ~env with
  | None -> ()
  | Some meta ->
    (* react to future detected theories *)
    let signal = MetaProver.on_theory (MetaProverState.prover meta) in
    Signal.on signal
      (function
        | MetaKB.NewTheory ("ac", [{HOT.term=HOT.Const f}], lit) ->
          let proof = MetaProverState.explain meta lit in
          add_ac ~env ~proof f; true
        | MetaKB.NewTheory ("ac", [t], _) ->
          Util.debug 1 "ignore AC instance for term %a" HOT.pp t; true
        | _ -> true);
    (* see whether AC symbols have already been detected *)
    Sequence.iter
      (function
        | MetaKB.NewTheory ("ac", [{HOT.term=HOT.Const f}], lit) ->
          let proof = MetaProverState.explain meta lit in
          add_ac ~env ~proof f
        | MetaKB.NewTheory ("ac", [t], _) ->
          Util.debug 1 "ignore AC instance for term %a" HOT.pp t
        | _ -> ())
      (MetaKB.cur_theories (MetaProverState.reasoner meta));
    ()
