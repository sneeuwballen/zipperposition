
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

(** {1 Heuristics} *)

open Logtk

module T = FOTerm
module Lit = Literal

let _depth_limit = ref None

let enable_depth_limit i =
  assert (i>0);
  _depth_limit := Some i

let section = Util.Section.make ~parent:Const.section "heuristics"

(** {2 Rules} *)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  val register : unit -> unit
end

module Make(E : Env.S) = struct
  module Env = E
  module PS = Env.ProofState
  module C = Env.C
  module Ctx = Env.Ctx

  let _depth_types lits =
    Literals.Seq.terms lits
    |> Sequence.map T.ty
    |> Sequence.map (fun t -> ScopedTerm.depth (t : Type.t :> ScopedTerm.t))
    |> Sequence.max ?lt:None
    |> CCOpt.maybe CCFun.id 0

  let is_too_deep c =
    match !_depth_limit with
    | None -> false
    | Some d ->
      let lits = C.lits c in
      let depth = max (_depth_types lits) (Literals.depth lits) in
      if depth > d
      then (
        Ctx.lost_completeness();
        Util.debug ~section 5 "clause dismissed (too deep at %d): %a" depth C.pp c;
        true
      ) else false

  let register () =
    Util.debug ~section 2 "register heuristics...";
    Env.add_is_trivial is_too_deep;
    ()
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module H = Make(E) in
    H.register ()
  in
  Extensions.{ default with name="heuristics"; actions=[Do action]; }

let () =
  Params.add_opts
    [ "-depth-limit"
      , Arg.Int enable_depth_limit
      , " set maximal term depth"
    ];
  ()

