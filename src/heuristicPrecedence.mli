(*
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

(** {1 Heuristic precedence} *)

open Logtk

(** This module is used to search for a precedence that optimizes some
    criteria. It uses hill-climbing to search for a solution, but
    does not guarantee that  the optimum will be reached.
    *)

val compute : ?initial_signature:Signature.t ->
              (Precedence.t -> Ordering.t) ->
              Precedence.constr list ->
              Precedence.constr list ->
              Clause.t Sequence.t ->
              Precedence.t
  (** define a constraint on symbols that is believed to improve
      the search by enabling as many simplifications as possible. It takes
      an ordering as a parameter, to be able to decide the orientation of
      terms in a given precedence, and ordering constraints that are
      respectively weaker/stronger than the optimization (the first one, weaker,
      is applied to break ties, the second one, stronger, is always enforced first) *)

