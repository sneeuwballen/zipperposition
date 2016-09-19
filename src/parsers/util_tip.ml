
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

module Choice = struct
  type cond = T.term list (* side conditions *)
  type 'a t = T.subst -> (T.subst * cond * 'a) Sequence.t

  let empty : 'a t = fun _ -> Sequence.empty

  let return
    : type a. a -> a t
    = fun x subst -> Sequence.return (subst, [], x)

  let (>>=)
    : type a b. a t -> (a -> b t) -> b t
    = fun seq f subst yield ->
      seq subst (fun (subst,conds,x) ->
        f x subst (fun (subst',conds',y) ->
          yield (T.merge_subst subst subst', conds@conds',y)))

  let (>|=)
    : type a b. a t -> (a -> b) -> b t
    = fun seq f subst yield ->
      seq subst (fun (subst,conds,x) ->
        yield (subst,conds,f x))

  let (<*>)
    : ('a -> 'b) t -> 'a t -> 'b t
    = fun f x ->
      f >>= fun f ->
      x >|= fun x -> f x

  let (<$>) f x = x >|= f ?loc:None

  let add_cond
    : cond -> unit t
    = fun c subst ->
      Sequence.return (subst, c, ())

  let add_subst
    : T.subst -> unit t
    = fun sigma subst ->
      Sequence.return (T.merge_subst subst sigma, [], ())

  (* non deterministic choice *)
  let (<+>)
    : type a. a t -> a t -> a t
    = fun x y subst ->
      Sequence.append (x subst) (y subst)

  let rec map_m f l = match l with
    | [] -> return []
    | x :: tail ->
      f x >>= fun x ->
      map_m f tail >|= fun tail -> x::tail

  let rec fold_m f acc l = match l with
    | [] -> return acc
    | [x] -> f acc x
    | x :: tail ->
      f acc x >>= fun acc -> fold_m f acc tail

  let choice_l
    : 'a t list -> 'a t
    = fun l subst ->
      Sequence.flat_map (fun x -> x subst) (Sequence.of_list l)

  let to_list
    : 'a t -> (T.subst * cond * 'a) list
    = fun seq ->
      seq T.StrMap.empty |> Sequence.to_rev_list

  let to_list_applied
    : T.t t -> T.t list
    = fun seq ->
      seq T.empty_subst
      |> Sequence.map
        (fun (subst,cond,t) ->
           let t = match cond with [] -> t | _ -> T.imply (T.and_ cond) t in
           T.apply_subst subst t)
      |> Sequence.to_rev_list
end

let app ?loc x y = T.app ?loc x [y]
let app_l = T.app

let conv_tyvar v = T.V v, Some T.tType
let conv_var (v,ty) = T.V v, Some (conv_ty ty)
let conv_vars = List.map conv_var

(* conversion of terms can yield several possible terms, by
   eliminating if and match *)
let rec conv_term (t:A.term): T.t Choice.t =
  let open Choice in
  match t with
    | A.True -> return T.true_
    | A.False -> return T.false_
    | A.Const s ->
      (* const of variable: let type inference decide *)
      return (T.var s)
    | A.App (f,l) ->
      app_l <$> return (T.const f) <*> map_m conv_term l
    | A.HO_app (a,b) ->
      app <$> conv_term a <*> conv_term b
    | A.If (a,b,c) ->
      conv_term a >>= fun a ->
      (add_cond [a] >>= fun () -> conv_term b)
      <+>
      (add_cond [T.not_ a] >>= fun () -> conv_term c)
    | A.Match (_,_) -> assert false (* TODO *)
    | A.Let (_,_) -> assert false (* TODO *)
    | A.Fun (_,_) -> assert false (* TODO *)
    | A.Eq (a,b) ->
      T.eq <$> conv_term a <*> conv_term b
    | A.Imply (a,b) -> T.imply <$> conv_term a <*> conv_term b
    | A.And l -> T.and_ <$> map_m conv_term l
    | A.Or l -> T.or_ <$> map_m conv_term l
    | A.Not a -> T.not_ <$> conv_term a
    | A.Forall (vars,body) ->
      T.forall <$> return (conv_vars vars) <*> conv_term body
    | A.Exists (vars,body) ->
      T.exists <$> return (conv_vars vars) <*> conv_term body
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
      let ts = conv_term t |> Choice.to_list_applied in
      List.map (UA.assert_ ?loc) ts
    | A.Stmt_assert_not (tyvars,g) ->
      (* goal *)
      let tyvars = List.map conv_tyvar tyvars in
      let goals = conv_term g |> Choice.to_list_applied in
      let g = T.forall_ty ?loc tyvars (T.and_ goals) in
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
      (* definition becomes a decl + universal axioms *)
      let f, vars, ty_f = conv_def fr.A.fr_decl in
      let decl = UA.decl ?loc f ty_f in
      let vars_as_t =
        List.map
          (function
            | T.Wildcard, _ -> assert false
            | T.V s, _ -> T.var s)
          vars
      in
      let defs =
        conv_term fr.A.fr_body
        |> Choice.to_list_applied
        |> List.map
          (fun body ->
             let ax = T.forall ?loc vars (T.eq (T.app_const f vars_as_t) body) in
             UA.assert_ ?loc ax)
      in
      decl :: defs
    | A.Stmt_funs_rec _
      ->
      assert false (* TODO *)

let convert_seq = Sequence.flat_map_l convert


