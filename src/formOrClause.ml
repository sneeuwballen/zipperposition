
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

(** {1 Formula Or Clause} *)

open Logtk

module PF = PFormula
module C = Clause

type t =
  | F of PFormula.t * forward_cell
  | C of Clause.t
and forward_cell = t option ref

type form_or_clause = t

let eq fc1 fc2 = match fc1, fc2 with
  | F (f1, _), F (f2, _) -> PF.eq f1 f2
  | C c1, C c2 -> C.eq c1 c2
  | _ -> false

let hash fc = match fc with
  | F (f, _) -> PF.hash f
  | C c -> C.hash c

let compare fc1 fc2 = match fc1, fc2 with
  | F (f1, _), F (f2, _) -> PF.cmp f1 f2
  | C c1, C c2 -> C.compare c1 c2
  | F _, C _ -> 1
  | C _, F _ -> ~-1

let of_form f = F (f, ref None)
let of_clause c = C c

let get_proof = function
  | F (f, _) -> PF.get_proof f
  | C c -> C.get_proof c

let rec follow_simpl fc = match fc with
  | F (_, {contents=Some fc'}) -> follow_simpl fc'
  | F (_, {contents=None}) -> fc
  | C c ->
    let c' = C.follow_simpl c in
    if C.eq c c'
      then fc
      else of_clause c'

let simpl_to ~from ~into =
  let from = follow_simpl from in
  if not (eq from into)
    then match from, into with
    | F (_, r), _ -> r := Some into
    | C c, C c' -> C.simpl_to ~from:c ~into:c'
    | C _, F _ -> failwith "FormOrClaus: clause cannot simplify into formula"

(** {2 IO} *)

let pp buf fc = match fc with
  | F (f, _) -> PF.pp buf f
  | C c -> C.pp buf c

let pp_tstp buf fc = match fc with
  | F (f, _) -> PF.pp_tstp buf f
  | C c -> C.pp_tstp buf c

let fmt fmt fc = match fc with
  | F (f, _) -> PF.fmt fmt f
  | C c -> C.fmt fmt c

let to_string fc = Util.on_buffer pp fc

let bij ~ctx = Bij.(
  let bij_pf = PF.bij ~ord:(Ctx.ord ctx) in
  let bij_c = C.bij ~ctx in
  switch
  ~inject:(function
    | F (f,_) -> "form", BranchTo (bij_pf, f)
    | C c -> "clause", BranchTo (bij_c, c))
  ~extract:(function
    | "form" -> BranchFrom (bij_pf, of_form)
    | "clause" -> BranchFrom (bij_c, of_clause)
    | n -> raise (DecodingError "expected FormOrClause")))

module Set = Set.Make(struct
  type t = form_or_clause
  let compare = compare
end)
