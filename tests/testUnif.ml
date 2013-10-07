
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

(** Test unification *)

open Logtk
open QCheck

module T = Term
module S = Substs

let check_unify_gives_unifier =
  let gen = Arbitrary.(pair T.arbitrary T.arbitrary) in
  let pp = PP.(pair T.to_string T.to_string) in
  let name = "unify_gives_unifier" in
  let prop (t1, t2) =
    try
      let subst = Unif.unification t1 0 t2 1 in
      let renaming = S.Renaming.create 5 in
      let t1' = S.apply ~renaming subst t1 0 in
      let t2' = S.apply ~renaming subst t2 1 in
      T.eq t1' t2'
    with Unif.Fail ->
      Prop.assume false;
      true
  in
  mk_test ~n:1000 ~pp ~name gen prop

let props =
  [ check_unify_gives_unifier
  ]
