
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

open Logtk
open Logtk_arbitrary
open QCheck

module T = FOTerm
module CC = Congruence.FO

(* build a congruence closure from a list of list of terms.
  each list of terms represents a congruence. *)
let _cc_of_classes classes =
  let cc = CC.create () in
  List.iter (fun cls -> match cls with
    | [] -> ()
    | t::cls' -> List.iter (fun t' -> CC.mk_eq cc t t') cls')
    classes;
  cc

let _size_class l =
  List.fold_left (fun acc t -> T.size t + acc) 0 l

let _size_classes l =
  List.fold_left (fun acc l -> (_size_class l) + acc) 0 l

let check_term_eq_itself =
  let gen = ArTerm.default in
  let prop t =
    let cc = CC.create () in
    CC.is_eq cc t t
  in
  let name = "congruence_term_eq_to_itself" in
  mk_test ~name ~size:T.size ~pp:T.to_string gen prop

(* if we build a congruence closure with classes, in each class,
    all elements are equal *)
let check_classes_are_eq =
  let gen = Arbitrary.(list (list ArTerm.default)) in
  let prop classes =
    let cc = _cc_of_classes classes in
    List.for_all
      (fun cls -> match cls with
      | [] -> true
      | t::cls' -> List.for_all (fun t' -> CC.is_eq cc t t') cls')
      classes
  in
  let name = "congruence_class_members_are_eq" in
  let size = _size_classes in
  let pp = PP.(list (list T.to_string)) in
  mk_test ~name ~pp ~size gen prop

let props =
  [ check_term_eq_itself
  ; check_classes_are_eq
  ]
