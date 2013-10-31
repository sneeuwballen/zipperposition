
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

(** Test meta prover stuff *)

open Logtk
open Logtk_meta
open Logtk_arbitrary
open QCheck

let check_pattern_cmp_self =
  let gen = ArPattern.default in
  let pp = MetaPattern.to_string in
  let name = "meta_pattern_cmp_self_zero" in
  let prop p =
    MetaPattern.compare p p = 0
  in
  mk_test ~name ~n:100 ~pp gen prop

let check_kb_axiom_cmp_self =
  let gen = Arbitrary.(
    map (pair ArPattern.apply (among ["foo"; "bar"; "baaz"]))
      (fun ((p,terms), name) -> MetaKB.Axiom (name, terms, p, terms)))
  in
  let pp = Util.on_buffer MetaKB.pp_axiom in
  let name = "meta_kb_axiom_cmp_self_zero" in
  let prop ax =
    MetaKB.compare_axiom ax ax = 0
  in
  mk_test ~name ~pp ~n:100 gen prop

let props =
  [ check_pattern_cmp_self
  ; check_kb_axiom_cmp_self
  ]
