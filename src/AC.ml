
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

module HOT = HOTerm
module T = FOTerm
module Lit = Literal

let prof_simplify = Util.mk_profiler "ac.simplify"

let stat_ac_simplify = Util.mk_stat "ac.simplify"
let stat_ac_redundant = Util.mk_stat "ac.redundant"

type spec = Theories.AC.t

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  val axioms : Symbol.t -> Type.t -> C.t list
    (** List of (persistent) axioms that are needed for simplifications to
        be complete for the given symbol. The [ctx] is required for type inference
        and building clauses . *)

  (** {2 Rules} *)

  val is_trivial_lit : Literal.t -> bool
    (** Is the literal AC-trivial? *)

  val is_trivial : C.t -> bool
    (** Check whether the clause is AC-trivial *)

  val simplify : C.t -> C.t
    (** Simplify the clause modulo AC *)

  val add_ac : ?proof:Proof.t list -> Symbol.t -> Type.t -> unit
    (** Declare that the given symbol is AC, and update the Env subsequently
        by adding clauses, etc. *)

  val setup : unit -> unit
    (** Register on Env *)
end

module Make(Env : Env.S) : S with module Env = Env = struct
  module Ctx = Env.Ctx
  module Env = Env
  module C = Env.C

  let axioms s ty =
    let ty_args_n, args_n = match Type.arity ty with
      | Type.Arity (i,j) -> i,j
      | Type.NoArity -> 0, 0
    in
    if args_n <> 2 then failwith "AC.axioms: AC symbol must be of arity 2";
    let ty_var = List.hd (Type.expected_args ty) in
    let x = T.var ~ty:ty_var 0 in
    let y = T.var ~ty:ty_var 1 in
    let z = T.var ~ty:ty_var 2 in
    (* type parameters should be the same as the concrete type? FIXME *)
    let tyargs = Sequence.(to_list (map (fun _ -> ty_var) (1 -- ty_args_n))) in
    let f x y = T.app_full (T.const ~ty s) tyargs [x;y] in
    let res = ref [] in
    (* build clause l=r *)
    let add_clause l r =
      let theories = [Util.sprintf "ac(%a)" Symbol.pp s] in
      let proof cc = Proof.mk_c_trivial ~theories cc in
      let c = C.create [ Lit.mk_eq l r ] proof in
      C.set_flag C.flag_persistent c true;
      res := c :: !res
    in
    add_clause (f x y) (f y x);
    add_clause (f (f x y) z) (f x (f y z));
    add_clause (f x (f y z)) (f z (f x y));
    add_clause (f x (f y z)) (f y (f x z));
    add_clause (f x (f y z)) (f z (f y x));
    !res

  (** {2 Rules} *)

  let is_trivial_lit lit =
    if not (Ctx.Theories.AC.exists_ac ())
    then false
    else
      let is_ac = Ctx.Theories.AC.is_ac  in
      let module A = T.AC(struct let is_ac = is_ac let is_comm _ = false end) in
      match Lit.View.as_eqn lit with
        | Some (l, r, true) -> A.eq l r
        | Some _ -> false
        | None -> false


  let is_trivial c =
    let res = Util.array_exists is_trivial_lit (C.lits c) in
    if res then Util.incr_stat stat_ac_redundant;
    res

  (* simplify: remove literals that are redundant modulo AC *)
  let simplify c =
    Util.enter_prof prof_simplify;
    if not (Ctx.Theories.AC.exists_ac ()) then c else
    let n = Array.length (C.lits c) in
    let is_ac = Ctx.Theories.AC.is_ac in
    let module A = T.AC(struct let is_ac = is_ac let is_comm _ = false end) in
    let lits = Array.to_list (C.lits c) in
    let lits = List.filter
      (fun lit -> match Lit.View.as_eqn lit with
        | Some (l, r, false) -> not (A.eq l r)
        | Some _
        | None -> true)
      lits
    in
    let n' = List.length lits in
    if n' < n && not (C.get_flag C.flag_persistent c)
      then begin
        let symbols = Ctx.Theories.AC.symbols_of_terms (C.Seq.terms c) in
        let symbols = Sequence.to_list (Symbol.Set.to_seq symbols) in
        let ac_proof = Util.list_flatmap Ctx.Theories.AC.find_proof symbols in
        let premises = C.proof c :: ac_proof in
        let proof cc = Proof.mk_c_simp ~theories:["ac"]
          ~rule:"normalize" cc premises in
        let parents = c :: C.parents c in
        let new_c = C.create ~parents lits proof in
        Util.exit_prof prof_simplify;
        Util.incr_stat stat_ac_simplify;
        Util.debug 3 "%a AC-simplify into %a" C.pp c C.pp new_c;
        new_c
      end else
        let _ = Util.exit_prof prof_simplify in
        c (* no simplification *)

  let setup () =
    (* enable AC inferences if needed *)
    if Ctx.Theories.AC.exists_ac ()
      then begin
      Env.add_is_trivial is_trivial;
      Env.add_simplify simplify;
      end;
    ()

  let add_ac ?proof s ty =
    Util.debug 1 "enable AC redundancy criterion for %a" Symbol.pp s;
    (* is this the first case of AC symbols? If yes, then add inference rules *)
    let first = not (Ctx.Theories.AC.exists_ac ()) in
    if first then setup ();
    (* remember that the symbols is AC *)
    Ctx.Theories.AC.add ?proof ~ty s;
    (* add clauses *)
    let clauses = axioms s ty in
    Env.add_passive (Sequence.of_list clauses);
    ()
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module AC = Make(E) in
    AC.setup ()
  in
  Extensions.({
    default with
    name="ac";
    actions=[Do action];
  })
