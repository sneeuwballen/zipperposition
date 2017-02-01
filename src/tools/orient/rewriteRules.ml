
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parse Rewrite Rules} *)

open Libzipperposition
open Libzipperposition_parsers

module PT = STerm
module T = TypedSTerm
module E = CCResult
module Loc = ParseLocation

type 'a or_error = ('a, string) CCResult.t

type statement =
  | Rule of PT.t * PT.t
  | Type of string * PT.t

type rule = TypedSTerm.t * TypedSTerm.t

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
            | PT.App ({PT.term=PT.Const "-->";_}, [_;l;r]) ->
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
let rules_of_pairs pairs =
  try
    let ctx = TypeInference.Ctx.create () in
    (* infer types *)
    let rules = CCList.filter_map
      (function
        | Rule (l,r) ->
          let l' = TypeInference.infer_exn ctx l in
          let r' = TypeInference.infer_exn ctx r in
          TypeInference.unify (T.ty_exn l') (T.ty_exn r');
          TypeInference.Ctx.exit_scope ctx;
          Some (l', r')
        | Type (s, ty) ->
          (* declare the type *)
          begin match TypeInference.infer_ty ctx ty with
          | E.Error _ -> ()
          | E.Ok ty -> TypeInference.Ctx.declare ctx (ID.make s) ty
          end; None
      ) pairs
    in
    TypeInference.Ctx.exit_scope ctx;
    (* extract typed rules and signature *)
    E.return rules
  with e -> E.of_exn_trace e

let signature seq =
  seq
  |> Sequence.flat_map (fun (l,r) -> Sequence.doubleton l r)
  |> Sequence.flat_map T.Seq.subterms
  |> Sequence.filter_map
    (fun t -> match T.view t with T.Const id -> Some (id, T.ty_exn t) | _ -> None)
  |> ID.Map.of_seq

let print_rule out (l,r) =
  Format.fprintf out "%a --> %a" T.pp l T.pp r

let print_rules oc rules =
  Format.fprintf oc "rules: @[%a@]@."
    (CCFormat.list ~sep:" " print_rule) rules
