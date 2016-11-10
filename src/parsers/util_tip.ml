
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Utils for TIP} *)

open Libzipperposition

module E = CCError
module A = Tip_ast
module UA = UntypedAST
module T = STerm

type parser_res = (A.statement Sequence.t, string) CCError.t
type 'a parser_ = 'a -> parser_res

let parse_lexbuf_ lex =
  let l = Tip_parser.parse_list Tip_lexer.token lex in
  Sequence.of_list l

let parse_lexbuf file : parser_res =
  try parse_lexbuf_ file |> E.return
  with e -> E.of_exn e

let parse_stdin () : parser_res =
  let lexbuf = Lexing.from_channel stdin in
  ParseLocation.set_file lexbuf "stdin";
  parse_lexbuf lexbuf

let parse_file file : parser_res =
  if file="stdin"
  then parse_stdin()
  else
    try
      CCIO.with_in file
        (fun ic ->
           let lexbuf = Lexing.from_channel ic in
           ParseLocation.set_file lexbuf file;
           parse_lexbuf_ lexbuf)
      |> E.return
  with e -> E.of_exn e

let conv_loc (loc:A.Loc.t): ParseLocation.t =
  let {A.Loc.file; start_line; start_column; stop_line; stop_column} = loc in
  ParseLocation.mk file start_line start_column stop_line stop_column

let rec conv_ty ty = match ty with
  | A.Ty_bool -> T.prop
  | A.Ty_arrow (args,ret) ->
    T.fun_ty (List.map conv_ty args) (conv_ty ret)
  | A.Ty_app (s, []) ->
    T.var s (* var or const: let type inference decide *)
  | A.Ty_app (s, args) ->
    T.app (T.const s) (List.map conv_ty args)

let app ?loc x y = T.app ?loc x [y]
let app_l = T.app

let conv_tyvar v = T.V v, Some T.tType
let conv_var (v,ty) = T.V v, Some (conv_ty ty)
let conv_vars = List.map conv_var

let rec conv_term (t:A.term): T.t =
  match t with
    | A.True -> T.true_
    | A.False -> T.false_
    | A.Const s ->
      (* const of variable: let type inference decide *)
      T.var s
    | A.App (f,l) ->
      app_l (T.const f) (List.map conv_term l)
    | A.HO_app (a,b) ->
      app (conv_term a) (conv_term b)
    | A.If (a,b,c) ->
      T.ite (conv_term a)(conv_term b)(conv_term c)
    | A.Match (u,l) ->
      let u = conv_term u in
      let l = List.map
          (function
            | A.Match_default t -> T.Match_default (conv_term t)
            | A.Match_case (s,vars,t) ->
              let vars = List.map (fun v->T.V v) vars in
              T.Match_case (s,vars,conv_term t))
          l
      in
      T.match_ u l
    | A.Let (l,u) ->
      let l = List.map (fun (v,t) -> T.V v, conv_term t) l in
      let u = conv_term u in
      T.let_ l u
    | A.Fun (v,t) ->
      let v = conv_var v in
      let t = conv_term t in
      T.lambda [v] t
    | A.Eq (a,b) ->
      T.eq (conv_term a)(conv_term b)
    | A.Imply (a,b) -> T.imply (conv_term a)(conv_term b)
    | A.And l -> T.and_ (List.map conv_term l)
    | A.Or l -> T.or_ (List.map conv_term l)
    | A.Not a -> T.not_ (conv_term a)
    | A.Forall (vars,body) ->
      let vars = conv_vars vars in
      let body = conv_term body in
      T.forall vars body
    | A.Exists (vars,body) ->
      let vars = conv_vars vars in
      let body = conv_term body in
      T.exists vars body
    | A.Cast (a,_) -> conv_term a

let conv_decl (d:A.ty A.fun_decl): string * T.t =
  let tyvars = List.map conv_tyvar d.A.fun_ty_vars in
  let ty_args = List.map conv_ty d.A.fun_args in
  let ty_ret = conv_ty d.A.fun_ret in
  let ty = T.forall_ty tyvars (T.fun_ty ty_args ty_ret) in
  d.A.fun_name, ty

(* return [f, vars, ty_f] *)
let conv_def (d:A.typed_var A.fun_decl): string * T.typed_var list * T.t =
  let tyvars = List.map conv_tyvar d.A.fun_ty_vars in
  let vars = List.map conv_var d.A.fun_args in
  let ty_args = List.map (CCFun.compose snd conv_ty) d.A.fun_args in
  let ty_ret = conv_ty d.A.fun_ret in
  let ty = T.forall_ty tyvars (T.fun_ty ty_args ty_ret) in
  d.A.fun_name, tyvars @ vars, ty

let conv_def ?loc decl body =
  (* translate into definitions *)
  let f, vars, ty_f = conv_def decl in
  let vars_as_t =
    List.map
      (function
        | T.Wildcard, _ -> assert false
        | T.V s, _ -> T.var s)
      vars
  in
  let def =
    let body = conv_term body in
    T.forall ?loc vars (T.eq (T.app_const f vars_as_t) body)
  in
  UA.mk_def f ty_f [def]

let convert (st:A.statement): UA.statement list =
  Util.debugf 3 "convert TIP statement@ @[%a@]"
    (fun k->k A.pp_stmt st);
  let loc = CCOpt.map conv_loc st.A.loc in
  match st.A.stmt with
    | A.Stmt_decl_sort (s,i) ->
      let ty = T.fun_ty (CCList.init i (fun _ -> T.tType) ) T.tType in
      [UA.decl ?loc s ty]
    | A.Stmt_decl d ->
      let s, ty = conv_decl d in
      [UA.decl ?loc s ty]
    | A.Stmt_assert t ->
      let t = conv_term t in
      [UA.assert_ ?loc t]
    | A.Stmt_assert_not (tyvars,g) ->
      (* goal *)
      let tyvars = List.map conv_tyvar tyvars in
      let g = conv_term g in
      let g = T.forall_ty ?loc tyvars g in
      [UA.goal ?loc g]
    | A.Stmt_data (tyvars, l) ->
      let l = List.map
          (fun (id, cstors) ->
             let cstors =
               List.map
                 (fun c ->
                    let args = c.A.cstor_args |> List.map snd |> List.map conv_ty in
                    c.A.cstor_name, args)
                 cstors
             in
             {UA.
               data_name=id;
               data_vars=tyvars;
               data_cstors=cstors;
             })
          l
      in
      [UA.data ?loc l]
    | A.Stmt_check_sat -> [] (* trivial *)
    | A.Stmt_fun_def fr
    | A.Stmt_fun_rec fr ->
      (* translate into definitions *)
      let l = [conv_def ?loc fr.A.fr_decl fr.A.fr_body] in
      [UA.def ?loc l]
    | A.Stmt_funs_rec {A. fsr_decls; fsr_bodies } ->
      assert (List.length fsr_decls = List.length fsr_bodies);
      let l = List.map2 (conv_def ?loc) fsr_decls fsr_bodies in
      [UA.def ?loc l]

let convert_seq = Sequence.flat_map_l convert


