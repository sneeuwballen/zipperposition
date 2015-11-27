
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

(** {1 Parse Rewrite Rules} *)

open Logtk
open Logtk_parsers

module PT = STerm
module E = CCError
module FOT = FOTerm
module Loc = ParseLocation

type 'a or_error = [`Error of string | `Ok of 'a]

type statement =
  | Rule of PT.t * PT.t
  | Type of string * PT.t

let rule_arrow = Symbol.of_string "-->"

type rule = FOTerm.t * FOTerm.t

(* parse file into rewrite rules *)
let parse_file filename ic =
  let lexbuf = Lexing.from_channel ic in
  Loc.set_file lexbuf filename;
  try
    (* parse declarations *)
    let terms = Parse_ho.parse_decls Lex_ho.token lexbuf in
    (* keep the facts of the form  a --> b *)
    let rules = CCList.filter_map
      (function
        | Ast_ho.Clause (t, []) ->
            begin match PT.view t with
            | PT.App ({PT.term=PT.Const s;_}, [_;l;r]) when Symbol.equal s rule_arrow ->
                Util.debugf 5 "parsed rule %a --> %a" (fun k->k PT.pp l PT.pp r);
                Some (Rule (l, r))
            | _ -> None
            end
        | Ast_ho.Type (s, ty) -> Some (Type (s,ty))
        | _ -> None)
      terms
    in
    E.return rules
  with
  | Parse_ho.Error ->
    let msg = CCFormat.sprintf "parse error at %a" Loc.pp (Loc.of_lexbuf lexbuf) in
    E.fail msg

(* list of pairs of terms --> list of pairs of FOTerms *)
let rules_of_pairs signature pairs =
  try
    let ctx = TypeInference.Ctx.create signature in
    (* infer types *)
    let pairs = CCList.filter_map
      (function
        | Rule (l,r) ->
          let ty_l, l' = TypeInference.FO.infer_exn ctx l in
          let ty_r, r' = TypeInference.FO.infer_exn ctx r in
          TypeInference.Ctx.constrain_type_type ctx ty_l ty_r;
          TypeInference.Ctx.exit_scope ctx;
          Some (fun ctx -> l' ctx, r' ctx)
        | Type (s, ty) ->
          (* declare the type *)
          begin match TypeInference.Ctx.ty_of_simple_term ctx ty with
          | None -> ()
          | Some ty -> TypeInference.Ctx.declare ctx (Symbol.of_string s) ty
          end; None
      ) pairs
    in
    let pairs = TypeInference.Closure.seq pairs in
    TypeInference.Ctx.generalize ctx;
    let signature = TypeInference.Ctx.to_signature ctx in
    (* extract typed rules and signature *)
    let rules = pairs ctx in
    E.return (signature,rules)
  with Type.Error msg ->
    E.fail msg

let print_rule out (l,r) =
  Format.fprintf out "%a --> %a" FOT.pp l FOT.pp r

let print_rules oc rules =
  Format.fprintf oc "rules: @[%a@]@."
    (CCFormat.list ~sep:" " print_rule) rules
