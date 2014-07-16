
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

  let is_too_deep c =
    match !_depth_limit with
    | None -> false
    | Some d ->
      let depth = Literals.depth (C.lits c) in
      if depth > d
      then (
        Ctx.lost_completeness();
        Util.debug 5 "clause dismissed (too deep at %d): %a" depth C.pp c;
        true
      ) else false

  let register () =
    Util.debug 2 "register heuristics...";
    Env.add_is_trivial is_too_deep;
    ()
end

let extension =
  let module DOIT(Env : Env.S) = struct
    include Extensions.MakeAction(Env)
    let actions =
      let module H = Make(Env) in
      [Ext_general H.register]
  end in
  { Extensions.default with
    Extensions.name="heuristics";
    Extensions.make=(module DOIT : Extensions.ENV_TO_S);
  }

let () =
  Params.add_opts
    [ "-depth-limit"
      , Arg.Int enable_depth_limit
      , "set maximal term depth"
    ];
  ()

