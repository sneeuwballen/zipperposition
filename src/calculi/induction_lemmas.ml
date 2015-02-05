
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013-2015, Simon Cruanes
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

(** {1 Induction through Cut} *)

module Sym = Logtk.Symbol
module Util = Logtk.Util
module Lits = Literals
module T = Logtk.FOTerm
module Ty = Logtk.Type
module IH = Induction_helpers

module type S = sig
  module Env : Env.S
  module Ctx : module type of Env.Ctx

  val scan : Env.C.t Sequence.t -> unit
  (** Scan clauses (typically initial set) for inductive constants *)

  val register : unit -> unit
end

let section_bool = BoolSolver.section
let section = Util.Section.make
  ~inheriting:[section_bool] ~parent:Const.section "ind.lemma"

module Make(E : Env.S)(Solver : BoolSolver.SAT) = struct
  module Env = E
  module Ctx = E.Ctx
  module CI = Ctx.Induction
  module C = E.C

  module BoolLit = Ctx.BoolLit

  let pp_bclause buf c =
    CCList.pp ~start:"[" ~stop:"]" ~sep:" ⊔ " BoolLit.pp buf c

  module IH_ctx = IH.Make(Ctx)

  (* scan clauses for ground terms of an inductive type,
    and declare those terms *)
  let scan seq : unit =
    Sequence.map C.lits seq
    |> Sequence.flat_map IH_ctx.find_inductive_cst
    |> Sequence.iter CI.declare

  (* TODO: generic mechanism for adding a formula (with restricted shape?)
      and make a lemma out of it, including Skolemization, etc. *)

  (* TODO: when a clause has inductive constants, take its negation
      and add it as a lemma *)

  let register () =
    IH_ctx.declare_types ();
    assert false
end

let extension =
  let action (module E : Env.S) =
    E.Ctx.lost_completeness ();
    let module Solver = (val BoolSolver.get_sat() : BoolSolver.SAT) in
    Util.debug ~section:section_bool 2
      "created SAT solver \"%s\"" Solver.name;
    let module A = Make(E)(Solver) in
    A.register();
  (* add an ordering constraint: ensure that constructors are smaller
    than other terms *)
  and add_constr penv = PEnv.add_constr ~penv 15 IH.constr_cstors in
  Extensions.({default with
    name="induction_lemma";
    actions=[Do action];
    penv_actions=[Penv_do add_constr];
  })

let () =
  Signal.on IH.on_enable
    (fun () ->
      Extensions.register extension;
      Signal.ContinueListening
    )
