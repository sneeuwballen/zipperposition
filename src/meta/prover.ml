
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

(** {1 Meta-Prover} *)

open Logtk
open Logtk_parsers

module P = Plugin
module R = Reasoner
module PT = PrologTerm
module Loc = ParseLocation

type t = {
  reasoner : Reasoner.t;
  signature : Signature.t;
}

type clause = Reasoner.clause

let empty = {
  reasoner = R.empty;
  signature = Signature.merge P.Base.signature Encoding.signature;
}

let reasoner p = p.reasoner

let signature p = p.signature

let add p clause =
  let r', consequences = R.add p.reasoner clause in
  {p with reasoner=r'; }, consequences

let add_fact p fact =
  add p (R.Clause.rule fact [])

let add_fo_clause p clause =
  let fact = Plugin.holds#to_fact clause in
  add_fact p fact

let add_signature p signature =
  let signature = Signature.merge p.signature signature in
  { p with signature; }

module Seq = struct
  let to_seq p = R.Seq.to_seq p.reasoner
  let of_seq p seq =
    let r', consequences = R.Seq.of_seq p.reasoner seq in
    {p with reasoner=r'; }, consequences
end

(** {6 IO} *)

(* convert an AST to a clause, if needed. In any case update the
   context *)
let __clause_of_ast ~ctx ast =
  let ast' = match ast with
  | Ast_ho.Clause (head, body) ->
      (* expected type *)
      let ret = Reasoner.property_ty in
      (* infer types for head, body, and force all types to be [ret] *)
      let ty_head, head' = TypeInference.HO.infer ctx head in
      TypeInference.Ctx.constrain_type_type ctx ty_head ret;
      let body' = List.map
        (fun t ->
          let ty, t' = TypeInference.HO.infer ctx t in
          TypeInference.Ctx.constrain_type_type ctx ty ret;
          t')
        body
      in
      let body' = TypeInference.Closure.seq body' in
      (* generalize *)
      TypeInference.Ctx.generalize ctx;
      let head' = head' ctx in
      let body' = body' ctx in
      TypeInference.Ctx.exit_scope ctx;
      Some (Reasoner.Clause.rule head' body')
  | Ast_ho.Type (s, ty) ->
      (* declare the type *)
      begin match TypeInference.Ctx.ty_of_prolog ctx ty with
      | None -> None
      | Some ty ->
        TypeInference.Ctx.declare ctx (Symbol.of_string s) ty;
        None
      end
  in
  ast'

let of_ho_ast p decls =
  try
    let ctx = TypeInference.Ctx.create (signature p) in
    let clauses = Sequence.fmap (__clause_of_ast ~ctx) decls in
    (* add clauses to the prover *)
    let p', consequences = Seq.of_seq p clauses in
    (* enrich signature *)
    let signature = TypeInference.Ctx.to_signature ctx in
    let p' = add_signature p' signature in
    Monad.Err.Ok (p', consequences)
  with Type.Error msg
  | Invalid_argument msg -> Monad.Err.fail msg

let parse_file p filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    Loc.set_file lexbuf filename;
    begin try
      let decls = Parse_ho.parse_decls Lex_ho.token lexbuf in
      let res = of_ho_ast p (Sequence.of_list decls) in
      close_in ic;
      res
    with
    | Parse_ho.Error ->
      close_in ic;
      let loc = Loc.of_lexbuf lexbuf in
      Monad.Err.fail ("parse error at "^Loc.to_string loc)
    | Lex_ho.Error msg ->
      close_in ic;
      Monad.Err.fail ("lexing error: " ^ msg)
    end
  with Sys_error msg ->
    let msg = Printf.sprintf "could not open file %s: %s" filename msg in
    Monad.Err.fail msg
