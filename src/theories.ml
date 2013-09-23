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

(** {6 Specifications of Built-in Theories} *)

open Logtk

module STbl = Symbol.SHashtbl

(** {2 Associativity-Commutativity} *)

module AC = struct
  type t = {
    is_ac : Symbol.t -> bool;
    symbols : unit -> Symbol.SSet.t;
    add : Symbol.t -> unit;
  } (** Specification: which symbols are AC? *)

  let create () =
    let h = STbl.create 13 in
    let is_ac s = STbl.mem h s in
    let symbols () =
      STbl.fold
        (fun s () set -> Symbol.SSet.add s set)
        h Symbol.SSet.empty
    in
    let add s = STbl.replace h s () in
    { is_ac; symbols; add; }

  let add ~spec s = spec.add s

  let is_ac ~spec s = spec.is_ac s

  let symbols ~spec = spec.symbols ()
end

(** {2 Total Ordering} *)

module TotalOrder = struct
  type instance = {
    less : Symbol.t;
    lesseq : Symbol.t;
  } (** A single instance of total ordering *)

  type t = {
    less_tbl : instance STbl.t;
    lesseq_tbl : instance STbl.t;
  } (** Specification of which symbols implement total orderings *)

  let create () =
    { less_tbl = STbl.create 13; lesseq_tbl = STbl.create 13; }

  let is_less ~spec s = STbl.mem spec.less_tbl s

  let is_lesseq ~spec s = STbl.mem spec.lesseq_tbl s

  let find ~spec s =
    try
      STbl.find spec.less_tbl s
    with Not_found ->
      STbl.find spec.lesseq_tbl s

  let orders ~spec =
    STbl.fold
      (fun _ instance acc -> instance :: acc)
      spec.less_tbl []

  let is_order_symbol ~spec s =
    STbl.mem spec.less_tbl s || STbl.mem spec.lesseq_tbl s

  let add ~spec ~less ~lesseq =
    if STbl.mem spec.less_tbl less || STbl.mem spec.lesseq_tbl lesseq
      then raise (Invalid_argument "ordering instances overlap");
    let instance = { less; lesseq; } in
    STbl.add spec.less_tbl less instance;
    STbl.add spec.lesseq_tbl lesseq instance;
    instance
end
