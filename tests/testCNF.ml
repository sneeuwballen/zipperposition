
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

(** Tests for CNF *)

open Logtk
open Logtk_arbitrary
open QCheck

module F = FOFormula

let pp = F.to_string

let check_cnf_idempotent =
  let gen = Arbitrary.(lift F.close_forall ArForm.default) in
  let name = "cnf_idempotent" in
  (* check that if [f] is in CNF, then [cnf_of f] is [f] *)
  let prop f =
    Prop.assume (Cnf.is_cnf f);
    match Cnf.cnf_of f with
    | [] -> F.is_trivial f
    | [[f']] -> F.eq f f'
    | _ -> false
  in
  mk_test ~name ~pp gen prop

let check_cnf_gives_clauses =
  let gen = Arbitrary.(lift F.close_forall ArForm.default) in
  let name = "cnf_gives_clauses" in
  (* check that the CNf of a formula is in clausal form *)
  let prop f =
    let clauses = Cnf.cnf_of f in
    let clauses = List.map F.mk_or clauses in
    List.for_all Cnf.is_cnf clauses
  in
  mk_test ~name ~pp gen prop

let check_miniscope_db_closed =
  let gen = Arbitrary.(lift F.close_forall ArForm.default) in
  let name = "cnf_miniscope_db_closed" in
  (* check that miniscoping preserved db_closed *)
  let prop f =
    F.db_closed f = F.db_closed (Cnf.miniscope f)
  in
  mk_test ~name ~pp gen prop

let props =
  [ check_cnf_idempotent
  ; check_cnf_gives_clauses
  ; check_miniscope_db_closed
  ]
