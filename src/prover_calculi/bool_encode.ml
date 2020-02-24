
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Applicative Encoding} *)

open Logtk
open Libzipperposition
module T = TypedSTerm

let section = Util.Section.make ~parent:Const.section "bool_encode"

let enabled_ = ref false

let bool_clone_id = ID.make "$_bool"
let bool_clone_ty = T.const ~ty:T.tType bool_clone_id
let bool_clone_tydecl =
  Statement.ty_decl ~proof:Proof.Step.trivial bool_clone_id T.tType

let true_clone_id = ID.make "$_true"
let true_clone_term = T.const ~ty:bool_clone_ty true_clone_id
let true_clone_tydecl =
  Statement.ty_decl ~proof:Proof.Step.trivial true_clone_id bool_clone_ty

let bool_encode_ty ?(top=true) ty_orig =
  let rec aux ~top ty =
    match T.view ty with
      | T.App (f, args) ->
        T.app ~ty:T.tType (aux ~top:false f) 
          (CCList.map (aux ~top:false) args)
      | T.AppBuiltin (Builtin.Arrow, ret::args) ->
        let ret' = aux ~top ret in
        let args' = List.map (aux ~top:false) args in
        T.Ty.fun_ args' ret'
      | T.AppBuiltin (f,args) ->
        assert(f != Builtin.Arrow);
        if f == Builtin.Prop then (
          assert(CCList.is_empty args);
          if top then bool_clone_ty
          else (
            let err = 
              CCFormat.sprintf "type @[%a@] is outside of the fragment." 
                T.pp ty_orig in
            invalid_arg err
          )) else (
          T.app_builtin ~ty:T.tType f (List.map (aux ~top:false) args))
      | T.Const _ -> ty
      | T.Var _ -> ty
      | T.Bind (b,v,t) -> T.bind ~ty:T.tType b v (aux ~top t)
      | T.Meta _ -> failwith "Not implemented: Meta"
      | T.Ite (_,_,_) -> failwith "Not implemented: Ite"
      | T.Let _ -> failwith "Not implemented: Let"
      | T.Match (_,_) -> failwith "Not implemented: Match"
      | T.Multiset _ -> failwith "Not implemented: Multiset"
      | T.Record (_,_) -> failwith "Not implemented: Record" in
    aux ~top ty_orig


let bool_encode_term t_orig  =
  let rec aux ~top t = 
    let encode_var v =  
      let ty = bool_encode_ty ~top:false (Var.ty v) in
      Var.update_ty v ~f:(fun _ -> ty) in
    
    try
      let ty = bool_encode_ty ~top (CCOpt.get_exn (T.ty t)) in
      match T.view t with
        | T.Const c -> T.const ~ty c
        | T.Var v ->
          T.var (encode_var v)
        | T.App (f, []) -> aux ~top f
        | T.App (f, args) ->
          let f' = aux ~top f in
          let args' = List.map (aux ~top:false) args in
          T.app ~ty f' args'
        | T.AppBuiltin (f, ts) ->
          assert (not ((T.equal t T.Form.true_) || (T.equal t T.Form.false_)));
          T.app_builtin ~ty f (List.map (aux ~top:false) ts)
        | T.Bind (Binder.Lambda, var, body) ->
          let var' = encode_var var in
          let body' = aux ~top:false body in
          let ty = bool_encode_ty ~top:false (CCOpt.get_exn (T.ty t)) in
          T.bind ~ty Binder.Lambda var' body'
        | T.Bind (Binder.Forall, _, _) -> failwith "Not implemented: Forall"
        | T.Bind (Binder.Exists, _, _) -> failwith "Not implemented: Exist"
        | _ -> failwith "Not implemented: Other kind of term"
    with Invalid_argument ty_err ->
      let err = 
        CCFormat.sprintf 
          "Subterm @[%a@]:@[%a@] of @[%a@] cannot be encoded because of type error: @[%s@]"
          T.pp t T.pp (T.ty_exn t) T.pp t_orig ty_err in
      if CCString.prefix ~pre:"type" ty_err then invalid_arg err
      else invalid_arg ty_err in
      
  let res = aux ~top:true t_orig in
  Util.debugf ~section 1 "Encoded term @[%a@] into @[%a@]" (fun k -> k T.pp t_orig T.pp res);
  res

let bool_encode_lit lit = 
  match lit with 
  | SLiteral.True -> SLiteral.eq true_clone_term true_clone_term
  | False -> SLiteral.neq true_clone_term true_clone_term
  | Atom(atom, sign) ->
    let mk_equ_lit = if sign then SLiteral.eq else SLiteral.neq in
    let encoded_atom = bool_encode_term atom in
    assert(T.equal (T.ty_exn true_clone_term) (T.ty_exn encoded_atom));
    mk_equ_lit (bool_encode_term atom) true_clone_term
  | Eq _ | Neq _ ->
    SLiteral.map bool_encode_term lit

(** Encode a clause *)
let bool_encode_lits lits = List.map bool_encode_lit lits

exception E_i of ((T.t SLiteral.t) list, T.t, T.t) Statement.t


let pp_in pp_f pp_t pp_ty = function
  | Output_format.O_zf -> Statement.ZF.pp pp_f pp_t pp_ty
  | Output_format.O_tptp -> Statement.TPTP.pp pp_f pp_t pp_ty
  | Output_format.O_normal -> Statement.pp pp_f pp_t pp_ty
  | Output_format.O_none -> CCFormat.silent

let pp_clause_in o =
  let pp_t = T.pp_in o in
  pp_in (Util.pp_list ~sep:" ∨ " (SLiteral.pp_in o pp_t)) pp_t pp_t o

let res_tc =
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
let bool_encode_stmt stmt =
  let as_proof = Proof.S.mk (Statement.proof_step stmt) (Proof.Result.make res_tc stmt) in
  let proof = Proof.Step.esa ~rule:(Proof.Rule.mk "bool_encode") [as_proof |> Proof.Parent.from] in
  let res = 
    match Statement.view stmt with
    | Statement.Data _ -> failwith "Not implemented: Data"
    | Statement.Lemma _ -> failwith "Not implemented: Lemma"
    | Statement.Goal lits -> failwith "Not implemented: Goal"
    | Statement.Def def ->
      let map_single = 
        Statement.map_def ~form:bool_encode_lits 
                          ~term:bool_encode_term 
                          ~ty:bool_encode_ty in
      Statement.def ~proof (List.map map_single def)
    | Statement.Rewrite def ->
      let new_def = 
        Statement.map_def_rule ~form:bool_encode_lits 
                               ~term:bool_encode_term 
                               ~ty:bool_encode_ty def in
      Statement.rewrite ~proof new_def
    | Statement.NegatedGoal (skolems,clauses) -> 
      let skolems = List.map (fun (id, ty) -> (id, bool_encode_ty ty)) skolems in
      Statement.neg_goal ~proof ~skolems (List.map bool_encode_lits clauses)
    | Statement.Assert lits -> Statement.assert_ ~proof (bool_encode_lits lits)
    | Statement.TyDecl (id, ty) ->
      Statement.ty_decl ~proof:Proof.Step.trivial id (bool_encode_ty ty) in
  res

let extension =
  let modifier seq =
    if !enabled_ then (
      Util.debug ~section 2 "Start boolean encoding";
      (* Encode statements *)
      let seq = Iter.map bool_encode_stmt seq in
      (* Add type declarations *)
      let ty_decls = Iter.of_list [bool_clone_tydecl;true_clone_tydecl] in
      let seq = Iter.append ty_decls seq in
      (* Add extensionality axiom *)
      Util.debug ~section 2 "Finished boolean encoding"; 
      seq
    ) else seq
  in
  Extensions.(
    { default with name="bool_encode"; post_cnf_modifiers=[modifier]; }
  )

let () =
  Options.add_opts
    [ "--encode-booleans", Arg.Bool ((:=) enabled_), " enable encoding of booleans into a fresh type"]