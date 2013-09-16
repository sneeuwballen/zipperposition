
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

(** {6 Transformations on Formulas and Terms} *)

module T = Term
module F = Formula

type t =
| RwTerm of Rewriting.TRS.t
| RwForm of Rewriting.FormRW.t
| Tr of string * (Formula.t -> Formula.t list)
  (** the function can return a conjunction of formulas. The
      string is a short name/description of the transformation *)

let of_term_rule rule =
  RwTerm (Rewriting.TRS.(add empty rule))

let of_term_rules_seq seq =
  let trs = Rewriting.TRS.(add_seq empty seq) in
  RwTerm trs

let of_term_rules l =
  let trs = Rewriting.TRS.(add_list empty l) in
  RwTerm trs

let of_form_rule rule =
  RwForm (Rewriting.FormRW.(add empty rule))

let of_form_rules_seq seq =
  let frs = Rewriting.FormRW.(add_seq empty seq) in
  RwForm frs

let of_form_rules l =
  let frs = Rewriting.FormRW.(add_list empty l) in
  RwForm frs

let of_term_tr name term2term =
  let transform f = [F.map term2term f] in
  Tr (name, transform)

let open_and =
  Tr ("open_and", F.open_and)

let remove_trivial =
  Tr ("remove_trivial", fun f -> if F.is_trivial f then [] else [f])

let rec apply tr f = match tr with
  | RwTerm trs ->
    let f' = F.map_depth (fun depth t -> Rewriting.TRS.rewrite ~depth trs t) f in
    [f']
  | RwForm frs ->
    let f' = Rewriting.FormRW.rewrite frs f in
    [f']
  | Tr (_, transform) ->
    let f' = transform f in
    match f' with
    | [f''] when F.eq f f'' -> f'
    | _ -> Util.list_flatmap (apply tr) f'

let apply_set tr set =
  F.FSet.flatMap set (fun f -> apply tr f)

let fix tr_list set =
  let rec step l prev_set set = match l with
  | [] ->
    if F.FSet.eq prev_set set
      then set
      else step tr_list set set
        (* start again with all transformations, but remember the set
          at the end of this turn *)
  | tr::l' ->
    let set = apply_set tr set in
    step l' prev_set set
  in
  step tr_list set set

let pp buf tr = match tr with
  | RwTerm trs -> Buffer.add_string buf "TRS"
  | RwForm trs -> Buffer.add_string buf "FormRW"
  | Tr (name, _) -> Buffer.add_string buf name

let fmt fmt tr =
  Format.pp_print_string fmt (Util.on_buffer pp tr)
