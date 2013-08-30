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

(** {1 Theory AST} *)

module T = Term

type statement =
  | Lemma of string * string list * premise list
  | LemmaInline of Term.t * premise list
  | Axiom of string * string list * Term.t
  | Theory of string * string list * premise list
  | Include of string
  | Error of string * Lexing.position * Lexing.position
  (** Parse statement *)
and premise =
  | IfAxiom of Term.t
  | IfFact of string * string list

(** Print a statement *)
let fmt fmt statement =
  let fmt_datalog fmt (s, args) = match args with
  | [] -> Format.pp_print_string fmt s
  | _ -> Format.fprintf fmt "%s(%a)" s
    (Sequence.pp_seq Format.pp_print_string) (Sequence.of_list args)
  in
  let fmt_premise fmt premise = match premise with
  | IfAxiom t -> Format.fprintf fmt "axiom %a" T.fmt t
  | IfFact (s, args) -> fmt_datalog fmt (s, args)
  in
  let fmt_premises fmt premises =
    Sequence.pp_seq ~sep:" and " fmt_premise fmt (Sequence.of_list premises)
  in
  match statement with
  | Lemma (s, args, premises) ->
    Format.fprintf fmt "lemma %a if %a." fmt_datalog (s,args) fmt_premises premises
  | LemmaInline (f, premises) ->
    Format.fprintf fmt "lemma axiom %a if @[<hov>%a@]." T.fmt f fmt_premises premises
  | Axiom (s, args, f) ->
    Format.fprintf fmt "%a is axiom %a." fmt_datalog (s, args) T.fmt f
  | Theory (s, args, premises) ->
    Format.fprintf fmt "theory %a is@ @[<hov>%a@]." fmt_datalog (s, args) fmt_premises premises
  | Include s ->
    Format.fprintf fmt "include %s." s
  | Error (e, start, stop) ->
    Format.fprintf fmt "parse error %s (%s to %s)" e (Util.pp_pos start) (Util.pp_pos stop)

(** Print a list of statements *)
let fmt_statements formatter statements =
  let fmt_stmt formatter st = Format.fprintf formatter "@[<h>%a@]" fmt st in
  Sequence.pp_seq fmt_stmt formatter (Sequence.of_list statements)
