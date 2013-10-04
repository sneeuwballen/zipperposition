
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

(** {1 Test indexing structures} *)

open Logtk

module T = Term

(* a simple instance of equation *)
module E : Index.EQUATION with type rhs = int = struct
  type t = T.t * int
  type rhs = int
  let compare (t1,i1) (t2,i2) = if t1 == t2 then i1-i2 else T.compare t1 t2
  let extract (t,i) = t, i, true
  let priority _ = 1
end

(* test unit index *)
module TestUnit(I : Index.UNIT_IDX with module E = E) = struct

  let props =
    [ ]
end

let props =
  let module DT = Dtree.Make(E) in
  let module TestDtree = TestUnit(DT) in
  let module NPDT = NPDtree.Make(E) in
  let module TestNPDtree = TestUnit(DT) in
  QCheck.flatten
    [ TestDtree.props
    ; TestNPDtree.props
    ]
