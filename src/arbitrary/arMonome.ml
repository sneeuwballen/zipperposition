
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

(** {1 Arbitrary Monomes} *)

open Logtk
open Libzipperposition

let t st = assert false

(*
(* arbitrary instance for the given constant generators *)
let _arbitrary_for ty any any_nonzero =
  let open QCheck.Arbitrary in
  0 -- 3 >>= fun n ->
  list_repeat n (pair any_nonzero (T.arbitrary_ty ty)) >>= fun terms ->
  any >>= fun constant ->
  any_nonzero >>= fun divby ->
  let m = of_list constant terms in
  return { m with divby; }

let arbitrary_int =
  QCheck.Arbitrary.(
    let any_int = lift Symbol.mk_int small_int in
    let any_int_nonzero = lift Symbol.mk_int (1 -- 10) in
    _arbitrary_for Type.int any_int any_int_nonzero)

let arbitrary_rat =
  QCheck.Arbitrary.(
    let any_rat = lift2 Symbol.mk_rat small_int (1 -- 10) in
    let any_rat_nonzero = lift2 Symbol.mk_rat (1 -- 50) (1 -- 10) in
    _arbitrary_for Type.rat any_rat any_rat_nonzero)

let arbitrary_ty ty =
  if Type.eq ty Type.int
    then arbitrary_int
  else if Type.eq ty Type.rat
    then arbitrary_rat
  else failwith ("Monome.arbitrary_ty: cannot deal with type " ^ Type.to_string ty)

let arbitrary =
  QCheck.Arbitrary.choose [ arbitrary_int; arbitrary_rat ]
*)
