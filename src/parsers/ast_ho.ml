
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

(** {1 Very Simple AST}

AST that holds Horn Clauses and type declarations, nothing more. *)

open Logtk

module PT = PrologTerm

type t =
  | Clause of PT.t * PT.t list
  | Type of string * PT.t

let pp buf t = match t with
  | Clause (head, []) ->
      Printf.bprintf buf "%a.\n" PT.pp head
  | Clause (head, body) ->
      Printf.bprintf buf "%a <-\n  %a.\n"
        PT.pp head (Util.pp_list ~sep:",\n  " PT.pp) body
  | Type (s, ty) ->
      Printf.bprintf buf "val %s : %a\n" s PT.pp ty

let to_string = Util.on_buffer pp
let fmt fmt s = Format.pp_print_string fmt (to_string s)

module Seq = struct
  let terms decl k = match decl with
    | Type (_, ty) -> k ty
    | Clause (head, l) ->
        k head; List.iter k l

  let vars decl =
    terms decl
      |> Sequence.flatMap PT.Seq.vars 
end
