
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Meta-Prover} *)

open Libzipperposition
open Libzipperposition_parsers

type 'a or_error = [`Error of string | `Ok of 'a]

let section = Util.Section.(make ~parent:zip "meta")

module P = Plugin
module R = Reasoner
module PT = STerm
module T = TypedSTerm
module Loc = ParseLocation
module Err = CCError

type t = {
  reasoner : Reasoner.t;
  signature : T.Ty.t ID.Map.t;
}

type clause = Reasoner.clause

let empty = {
  reasoner = R.empty;
  signature = P.Base.signature;
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
        let ret = T.Ty.const Reasoner.property_id in
        (* infer types for head, body, and force all types to be [ret] *)
        let head' = TypeInference.infer_exn ctx head in
        TypeInference.unify (T.ty_exn head') ret;
        let body' = List.map
            (fun t ->
               let t' = TypeInference.infer_exn ctx t in
               TypeInference.unify (T.ty_exn t') ret;
               t')
            body
        in
        (* forget types of free variables *)
        TypeInference.Ctx.exit_scope ctx;
        Some (Reasoner.Clause.rule head' body')
    | Ast_ho.Type (s, ty) ->
        (* declare the type *)
        let ty' = TypeInference.infer_exn ctx ty in
        TypeInference.Ctx.declare ctx (ID.make s) ty';
        TypeInference.Ctx.exit_scope ctx;
        None
  in
  ast'

let of_ho_ast p decls =
  try
    let ctx = TypeInference.Ctx.create () in
    (* TODO declare builtin symbols here *)
    let clauses = Sequence.fmap (__clause_of_ast ~ctx) decls in
    (* add clauses to the prover *)
    let p', consequences = Seq.of_seq p clauses in
    (* enrich signature *)
    let tys = TypeInference.Ctx.pop_new_types ctx in
    let p' = {p' with signature= ID.Map.add_list p'.signature tys} in
    Err.return (p', consequences)
  with e -> CCError.of_exn_trace e

let parse_file p filename =
  Util.debugf ~section 1 "parse theory file `%s`" (fun k->k filename);
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
      | UntypedAST.Parse_error _ as e ->
          close_in ic;
          let msg = Printexc.to_string e in
          Err.fail msg
    end
  with
  | Sys_error msg ->
    let msg = Printf.sprintf "could not open file `%s`: %s" filename msg in
    Err.fail msg
