
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Transform higher-order problems into first-order using the applicative encoding } *)

open Logtk
open Logtk_parsers

module T = TypedSTerm

let extensional_ = ref false

let pp_stmt_in o =
  Statement.pp_input_in o

let pp_stmts_in o out seq =
  CCList.pp ~sep:"" (pp_stmt_in o) out seq

(* Replacement for the function type in the encoding: *)

let function_type_id = ID.make "app_encode_fun"

let function_type = T.const ~ty:T.tType function_type_id

(* Replacement for the application in the encoding: *)

let app_id = ID.make "app_encode_app"

let app_type =
  let alpha = Var.make ~ty:T.tType (ID.make "alpha") in
  let beta = Var.make ~ty:T.tType (ID.make "beta") in
  T.bind ~ty:T.tType Binder.ForallTy alpha (
    T.bind ~ty:T.tType Binder.ForallTy beta (
      T.app_builtin ~ty:T.tType Builtin.Arrow
        [T.var beta;
         T.app ~ty:T.tType (function_type) [T.var alpha; T.var beta];
         T.var alpha]
    )
  )

let app_const = T.const ~ty:app_type app_id

(** encode a type *)
let rec app_encode_ty ty =
  match T.view ty with
    | T.App (f, args) ->
      assert (not (T.equal f function_type));
      T.app ~ty:T.tType (app_encode_ty f) (CCList.map app_encode_ty args)
    | T.AppBuiltin (Builtin.Arrow, ret::args) when (not (T.Ty.is_tType ret)) ->
      CCList.fold_right
        (fun arg t ->
           T.app ~ty:T.tType function_type [app_encode_ty arg;t]
        )
        args
        (app_encode_ty ret)
    | T.AppBuiltin (f,args) ->
      T.app_builtin ~ty:T.tType f (CCList.map app_encode_ty args)
    | T.Const _ -> ty
    | T.Var _ -> ty
    | T.Meta _ -> failwith "Not implemented"
    | T.Bind (b,v,t) -> T.bind ~ty:T.tType b v (app_encode_ty t)
    | T.Ite (_,_,_) -> failwith "Not implemented"
    | T.Let _ -> failwith "Not implemented"
    | T.Match (_,_) -> failwith "Not implemented"
    | T.Multiset _ -> failwith "Not implemented"
    | T.Record (_,_) -> failwith "Not implemented"

(** Is a term a type? i.e. is a term of type tType? *)
let is_type a =
  T.Ty.is_tType (T.ty_exn a)

(** encode a variable *)
let app_encode_var var =
  let ty = app_encode_ty (Var.ty var) in
  (Var.update_ty var ~f:(fun _ -> ty))

(** encode a term *)
let rec app_encode_term toplevel t  =
  let ty = app_encode_ty (CCOpt.get_exn (T.ty t)) in
  Util.debugf 5 "Encoded type %a into %a" (fun k -> k T.pp (CCOpt.get_exn (T.ty t)) T.pp ty);
  match T.view t with
    | T.App (f, []) -> app_encode_term false f
    | T.App (f, args) ->
      Util.debugf 5 "Attempting to encode application: %a" (fun k -> k T.pp_with_ty t);
      (* app encode *)
      CCList.fold_left
        (fun term arg ->
           Util.debugf 5 "Encoding application of %a to %a" (fun k -> k T.pp_with_ty term T.pp arg);
           match T.view (T.ty_exn term) with
             | T.App (f, types) ->
               assert (T.equal f function_type);
               assert (List.length types == 2);
               assert (not (is_type arg));
               let arg' = app_encode_term false arg in
               T.app
                 ~ty:(List.nth types 1)
                 app_const
                 [CCOpt.get_exn (T.ty arg'); List.nth types 1; term; arg']
             | T.Bind (Binder.ForallTy, var, t) ->
               assert (is_type arg);
               let arg' = app_encode_ty arg in
               let t' = T.Subst.eval (Var.Subst.singleton var arg') t in
               T.app ~ty:t' term [arg']
             | _ -> failwith "Not implemented"
        )
        (app_encode_term false f)
        args
    | T.AppBuiltin (f, ts) ->
      Util.debugf 5 "Term: %a" (fun k -> k T.pp t);
      (* Assert that the problem does not require first-class booleans: *)
      if not toplevel then failwith "requires FOOL";
      (* These builtins cannot be easily app-encoded: *)
      if f == Builtin.ExistsConst then failwith "contains ExistsConst";
      if f == Builtin.ForallConst then failwith "contains ExistsConst";
      (* Assert that no other weird builtins are used: *)
      assert (
        f == Builtin.Eq ||
        f == Builtin.Neq ||
        f == Builtin.And ||
        f == Builtin.Or ||
        f == Builtin.Not ||
        f == Builtin.Imply ||
        f == Builtin.Equiv ||
        f == Builtin.False ||
        f == Builtin.True ||
        f == Builtin.Equiv ||
        f == Builtin.Xor
      );
      T.app_builtin ~ty f (List.map (app_encode_term (not (f == Builtin.Eq || f == Builtin.Neq))) ts)
    | T.Const c -> T.const ~ty c
    | T.Var v -> T.var (app_encode_var v)
    | T.Bind (Binder.Forall | Binder.Exists as b, v, u) ->
      if not toplevel then failwith "requires FOOL";
      T.bind ~ty b (app_encode_var v) (app_encode_term true u)
    | _ -> failwith "Not implemented"

(** encode a statement *)
let app_encode stmt =
  match Statement.view stmt with
    | Statement.Def _ -> failwith "Not implemented"
    | Statement.Rewrite _ -> failwith "Not implemented"
    | Statement.Data _ -> failwith "Not implemented"
    | Statement.Lemma _ -> failwith "Not implemented"
    | Statement.Goal f -> Statement.goal ~proof:Proof.Step.trivial (app_encode_term true f)
    | Statement.NegatedGoal (_,_) -> failwith "Not implemented"
    | Statement.Assert f -> Statement.assert_ ~proof:Proof.Step.trivial (app_encode_term true f)
    | Statement.TyDecl (id, ty) ->
      Statement.ty_decl ~proof:Proof.Step.trivial id (app_encode_ty ty)

let extensionality_axiom =
  let alpha = Var.make ~ty:T.tType (ID.make "alpha") in
  let beta = Var.make ~ty:T.tType (ID.make "beta") in
  let fun_alpha_beta = T.app ~ty:T.tType (function_type) [T.var alpha; T.var beta] in
  let x = Var.make ~ty:fun_alpha_beta (ID.make "x") in
  let y = Var.make ~ty:fun_alpha_beta (ID.make "y") in
  let z = Var.make ~ty:(T.var alpha) (ID.make "z") in
  let xz = T.app ~ty:(T.var beta) app_const [T.var alpha; T.var beta; T.var x; T.var z] in
  let yz = T.app ~ty:(T.var beta) app_const [T.var alpha; T.var beta; T.var y; T.var z] in
  let prop = T.builtin ~ty:T.tType Builtin.Prop in
  Statement.assert_ ~proof:Proof.Step.trivial
    (T.bind_list ~ty:prop Binder.forall [x; y]
       (T.app_builtin ~ty:prop Builtin.Imply [
           T.bind ~ty:prop Binder.forall z
             (T.app_builtin ~ty:prop Builtin.Eq [xz; yz]);
           T.app_builtin ~ty:prop Builtin.Eq [T.var x; T.var y]
         ]
       )
    )

let process file =
  let o = !Options.output in
  let input = Input_format.I_tptp in
  let parse = Util_tptp.parse_file ~recursive:true file in
  Util.debugf 5 "Parse: %s" (fun k -> k (match parse with | CCResult.Error e -> e | CCResult.Ok _ -> "OK"));
  let ast = Sequence.map Util_tptp.to_ast (CCResult.get_exn parse) in
  let typed_ast = TypeInference.infer_statements ?ctx:None
      ~on_var:(Input_format.on_var input)
      ~on_undef:(Input_format.on_undef_id input)
      ~on_shadow:(Input_format.on_shadow input)
      ~implicit_ty_args:false ast in
  let typed_ast = CCVector.to_list (CCResult.get_exn typed_ast) in
  let app_encoded = CCList.map app_encode typed_ast in
  let decl_fun =
    Statement.ty_decl ~proof:Proof.Step.trivial function_type_id
      (T.app_builtin ~ty:T.tType Builtin.arrow [T.tType;T.tType;T.tType])
  in
  let decl_app = Statement.ty_decl ~proof:Proof.Step.trivial app_id app_type in
  Format.printf "@[<v>%a@]@."
    (pp_stmt_in o) decl_fun;
  Format.printf "@[<v>%a@]@."
    (pp_stmt_in o) decl_app;
  if !extensional_ then
    Format.printf "@[<v>%a@]@."
      (pp_stmt_in o) extensionality_axiom;
  Format.printf "@[<v>%a@]@."
    (pp_stmts_in o) app_encoded

let options =
  Options.add_opts
    [ "--app-encode-extensional", Arg.Set extensional_, " enable extensionality axiom in app-encoding"];
  Options.make()

let () =
  CCFormat.set_color_default true;
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse (Arg.align options) add_file "app_encode [options] [file|stdin]";
  let file = match !files with
    | [] -> "stdin"
    | [f] -> f
    | _::_ -> failwith "expected at most one file"
  in
  process file
