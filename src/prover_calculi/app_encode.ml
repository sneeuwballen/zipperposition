
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Applicative Encoding} *)

open Logtk
open Libzipperposition

let section = Util.Section.make ~parent:Const.section "app_encode"

let mode_ : [ `None | `Extensional | `Intensional ] ref = ref `None

module T = TypedSTerm

(* Replacement for the function type in the encoding: *)
let id_ae_fun = ID.make "app_encode_fun"
let ty_ae_fun = T.app_builtin ~ty:T.tType Builtin.Arrow [T.tType; T.tType; T.tType]
let function_type = T.const ~ty:ty_ae_fun id_ae_fun

(* Replacement for application in the encoding: *)
let id_ae_app = ID.make "app_encode_app"
let ty_ae_app =
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
let const_ae_app = T.const ~ty:ty_ae_app id_ae_app

(** Type declarations for these new symbols *)
let ty_decls = 
  let ae_fun_decl = Statement.ty_decl ~proof:Proof.Step.trivial id_ae_fun ty_ae_fun in
  let ae_app_decl = Statement.ty_decl ~proof:Proof.Step.trivial id_ae_app ty_ae_app in
  Iter.of_list [ae_fun_decl; ae_app_decl]

(** Encode a type *)
let rec app_encode_ty ty =
  let ty' = match T.view ty with
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
    | T.Meta _ -> failwith "Not implemented: Meta"
    | T.Bind (b,v,t) -> T.bind ~ty:T.tType b v (app_encode_ty t)
    | T.Ite (_,_,_) -> failwith "Not implemented: Ite"
    | T.Let _ -> failwith "Not implemented: Let"
    | T.Match (_,_) -> failwith "Not implemented: Match"
    | T.Multiset _ -> failwith "Not implemented: Multiset"
    | T.Record (_,_) -> failwith "Not implemented: Record"
  in
  Util.debugf 5 "Encoded type @[%a@] into @[%a@]" (fun k -> k T.pp ty T.pp ty');
  ty'

(** Is a term a type? i.e. is a term of type tType? *)
let is_type a =
  T.Ty.is_tType (T.ty_exn a)

(** Encode a variable *)
let app_encode_var var =
  let ty = app_encode_ty (Var.ty var) in
  (Var.update_ty var ~f:(fun _ -> ty))

(** Encode a term *)
let rec app_encode_term toplevel t  =
  if toplevel then Util.debugf ~section 3 "Encoding toplevel term @[%a@]" (fun k -> k T.pp t);
  let ty = app_encode_ty (CCOpt.get_exn (T.ty t)) in
  let t' = match T.view t with
    | T.App (f, []) -> app_encode_term false f
    | T.App (f, args) ->
      Util.debugf 5 "Attempting to encode application: %a" (fun k -> k T.pp_with_ty t);
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
               const_ae_app
               [CCOpt.get_exn (T.ty arg'); List.nth types 1; term; arg']
           | T.Bind (Binder.ForallTy, var, t) ->
             assert (is_type arg);
             let arg' = app_encode_ty arg in
             let t' = T.Subst.eval (Var.Subst.singleton var arg') t in
             T.app ~ty:t' term [arg']
           | _ -> failwith "Expected quantified or function type"
        )
        (app_encode_term false f)
        args
    | T.AppBuiltin (f, ts) ->
      Util.debugf 5 "AppBuiltin-Term: %a" (fun k -> k T.pp t); 
      failwith "Not implemented: AppBuiltin"
    | T.Const c -> T.const ~ty c
    | T.Var v -> T.var (app_encode_var v)
    | T.Bind (Binder.Forall, _, _) -> failwith "Not implemented: Forall"
    | T.Bind ( Binder.Exists, _, _) -> failwith "Not implemented: Exist"
    | T.Bind (Binder.Lambda, _, _) -> failwith "Not implemented: Lambda"
    | _ -> failwith "Not implemented: Other kind of term"
  in
  Util.debugf 5 "Encoded term @[%a@] into @[%a@]" (fun k -> k T.pp t T.pp t');
  t'

(** Encode a literal *)
let app_encode_lit lit = 
  SLiteral.map (app_encode_term true) lit

(** Encode a clause *)
let app_encode_lits lits = List.map app_encode_lit lits


exception E_i of ((T.t SLiteral.t) list, T.t, T.t) Statement.t


let pp_in pp_f pp_t pp_ty = function
  | Output_format.O_zf -> Statement.ZF.pp pp_f pp_t pp_ty
  | Output_format.O_tptp -> Statement.TPTP.pp pp_f pp_t pp_ty
  | Output_format.O_normal -> Statement.pp pp_f pp_t pp_ty
  | Output_format.O_none -> CCFormat.silent

let pp_clause_in o =
  let pp_t = T.pp_in o in
  pp_in (Util.pp_list ~sep:" ∨ " (SLiteral.pp_in o pp_t)) pp_t pp_t o


let result_tc =
  Proof.Result.make_tc
    ~of_exn:(function E_i c -> Some c | _ -> None)
    ~to_exn:(fun i -> E_i i)
    ~compare:compare
    ~pp_in:pp_clause_in
    ~is_stmt:true
    ~name:Statement.name
    ~to_form:(fun ~ctx st ->
        let conv_c (c:(T.t SLiteral.t) list) : _ =
          c 
          |> List.map SLiteral.to_form
          |> T.Form.or_
        in
        Statement.Seq.forms st
        |> Iter.map conv_c
        |> Iter.to_list
        |> T.Form.and_)
    ()

(** encode a statement *)
let app_encode_stmt stmt =
  let as_proof = Proof.S.mk (Statement.proof_step stmt) (Proof.Result.make result_tc stmt) in
  let proof = Proof.Step.esa ~rule:(Proof.Rule.mk "app_encode") [as_proof |> Proof.Parent.from] in
  match Statement.view stmt with
  | Statement.Def _ -> failwith "Not implemented: Def"
  | Statement.Rewrite _ -> failwith "Not implemented: Rewrite"
  | Statement.Data _ -> failwith "Not implemented: Data"
  | Statement.Lemma _ -> failwith "Not implemented: Lemma"
  | Statement.Goal lits -> failwith "Not implemented: Goal"
  | Statement.NegatedGoal (skolems,clauses) -> 
    let skolems = List.map (fun (id, ty) -> (id, app_encode_ty ty)) skolems in
    Statement.neg_goal ~proof ~skolems (List.map app_encode_lits clauses)
  | Statement.Assert lits -> Statement.assert_ ~proof (app_encode_lits lits)
  | Statement.TyDecl (id, ty) ->
    Statement.ty_decl ~proof:Proof.Step.trivial id (app_encode_ty ty)

(* TODO: fix, add diff (+ ty_decl) *)
let extensionality_axiom =
  let alpha = Var.make ~ty:T.tType (ID.make "alpha") in
  let beta = Var.make ~ty:T.tType (ID.make "beta") in
  let fun_alpha_beta = T.app ~ty:T.tType (function_type) [T.var alpha; T.var beta] in
  let x = Var.make ~ty:fun_alpha_beta (ID.make "x") in
  let y = Var.make ~ty:fun_alpha_beta (ID.make "y") in
  let z = Var.make ~ty:(T.var alpha) (ID.make "z") in
  let xz = T.app ~ty:(T.var beta) const_ae_app [T.var alpha; T.var beta; T.var x; T.var z] in
  let yz = T.app ~ty:(T.var beta) const_ae_app [T.var alpha; T.var beta; T.var y; T.var z] in
  Statement.assert_ ~proof:Proof.Step.trivial
    [SLiteral.neq xz yz; SLiteral.eq (T.var x) (T.var y)]

let extension =
  let modifier seq =
    if !mode_ != `None then (
      Util.debug ~section 2 "Start applicative encoding";
      (* Encode statements *)
      let seq = Iter.map app_encode_stmt seq in
      (* Add type declarations *)
      let seq = Iter.append ty_decls seq in
      (* Add extensionality axiom *)
      let seq = 
        if !mode_ = `Extensional 
        then Iter.append seq (Iter.singleton extensionality_axiom) 
        else seq in
      Util.debug ~section 2 "Finished applicative encoding"; 
      seq
    ) else seq
  in
  Extensions.(
    { default with name="app_encode"; post_cnf_modifiers=[modifier]; }
  )

let options = 
  ["none", `None
  ;"extensional", `Extensional
  ;"intensional", `Intensional]

let () =
  Options.add_opts
    [ "--app-encode", Arg.Symbol (List.map fst options, fun o -> mode_ := List.assoc o options), " enable applicative encoding"]