
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Applicative Encoding} *)

open Logtk
open Libzipperposition
module T = TypedSTerm

let section = Util.Section.make ~parent:Const.section "bool_encode"

let enabled_ = ref false

let (==>) = T.Ty.(==>)

let decl id ty = Statement.ty_decl ~proof:Proof.Step.trivial id ty

let bool_clone_id = ID.make "$_bool"
let bool_clone_ty = T.const ~ty:T.tType bool_clone_id
let bool_clone_tydecl = decl bool_clone_id bool_clone_ty

let true_clone_id = ID.make "$_true"
let true_term = T.const ~ty:bool_clone_ty true_clone_id
let true_clone_tydecl = decl true_clone_id bool_clone_ty

let false_clone_id = ID.make "$_false"
let false_term = T.const ~ty:bool_clone_ty false_clone_id
let false_clone_tydecl = decl false_clone_id bool_clone_ty

let and_id = ID.make "$_and"
let and_type = [bool_clone_ty; bool_clone_ty] ==> bool_clone_ty
let and_term = T.const ~ty:and_type and_id
let and_clone_tydecl = decl and_id and_type 

let or_id = ID.make "$_or"
let or_type = [bool_clone_ty; bool_clone_ty] ==> bool_clone_ty
let or_term = T.const ~ty:or_type or_id
let or_clone_tydecl = decl or_id or_type

let not_id = ID.make "$_not"
let not_type = [bool_clone_ty] ==> bool_clone_ty
let not_term = T.const ~ty:not_type not_id
let not_clone_tydecl = decl not_id not_type

let impl_id = ID.make "$_impl"
let impl_type = [bool_clone_ty; bool_clone_ty] ==> bool_clone_ty
let impl_term = T.const ~ty:impl_type impl_id
let impl_clone_tydecl = decl impl_id impl_type

let equiv_id = ID.make "$_equiv"
let equiv_type = [bool_clone_ty; bool_clone_ty] ==> bool_clone_ty
let equiv_term = T.const ~ty:equiv_type equiv_id
let equiv_clone_tydecl = decl equiv_id equiv_type

let xor_id = ID.make "$_xor"
let xor_type = [bool_clone_ty; bool_clone_ty] ==> bool_clone_ty
let xor_term = T.const ~ty:xor_type xor_id
let xor_clone_tydecl = decl xor_id xor_type


let eq_id = ID.make "$_eq"
let eq_type = 
  let alpha = Var.make ~ty:T.tType (ID.make "alpha") in
  let body = [T.var alpha; T.var alpha] ==> bool_clone_ty in
  T.Ty.close_forall body
let eq_term = T.const ~ty:eq_type eq_id
let eq_clone_tydecl = decl eq_id eq_type

let neq_id = ID.make "$_neq"
let neq_term = T.const ~ty:eq_type neq_id
let neq_clone_tydecl = decl neq_id eq_type

let forall_id = ID.make "$_forall"
let forall_type = 
  let alpha = Var.make ~ty:T.tType (ID.make "alpha") in
  let body = [([T.var alpha] ==> bool_clone_ty)] ==> bool_clone_ty in
  T.Ty.close_forall body
let forall_term = T.const ~ty:forall_type forall_id
let forall_clone_tydecl = decl forall_id forall_type

let exists_id = ID.make "$_exists"
let exists_term = T.const ~ty:forall_type exists_id
let exists_clone_tydecl = decl exists_id forall_type

let choice_id = ID.make "$_choice"
let choice_type = 
  let alpha = Var.make ~ty:T.tType (ID.make "alpha") in
  let body = [([T.var alpha] ==> bool_clone_ty)] ==> T.var alpha in
  T.Ty.close_forall body
let choice_term = T.const ~ty:choice_type choice_id
let choice_clone_tydecl = decl choice_id choice_type


let ty_decls = 
  Iter.of_list [bool_clone_tydecl;true_clone_tydecl;
                false_clone_tydecl; and_clone_tydecl;
                or_clone_tydecl; not_clone_tydecl;
                impl_clone_tydecl; equiv_clone_tydecl;
                xor_clone_tydecl; eq_clone_tydecl;
                neq_clone_tydecl; forall_clone_tydecl;
                exists_clone_tydecl; choice_clone_tydecl]

let app_bool hd args =
    T.app hd ~ty:bool_clone_ty args

let boolean_axioms = 
  let bool_x = T.var (Var.make ~ty:bool_clone_ty (ID.make "X")) in
  let bool_y = T.var (Var.make ~ty:bool_clone_ty (ID.make "Y")) in

  let alpha_var = Var.make ~ty:T.tType (ID.make "alpha") in
  let alpha = T.var alpha_var  in
  let alpha_x = T.var (Var.make ~ty:alpha (ID.make "X")) in
  let alpha_y = T.var (Var.make ~ty:alpha (ID.make "Y")) in

  let alpha2bool_p =
    T.var (Var.make ~ty:([alpha] ==> bool_clone_ty) (ID.make "P")) in
  
  (* x = T \/ x = F *)
  let either_true_or_false =
    [SLiteral.eq bool_x true_term; SLiteral.eq bool_x false_term] in
  (* T != F *)
  let true_neq_false =
    [SLiteral.neq true_term false_term] in
  (* and T x = x *)
  let and_true =
    [SLiteral.eq (app_bool and_term [true_term; bool_x]) bool_x] in
  (* and F x = F *)
  let and_false =
    [SLiteral.eq (app_bool and_term [false_term; bool_x]) false_term] in
  (* not T = F *)
  let not_true =
    [SLiteral.eq (app_bool not_term [true_term]) false_term] in
  (* not F = T *)
  let not_false =
    [SLiteral.eq (app_bool not_term [false_term]) true_term] in
  (* or T x = T *)
  let or_true =
    [SLiteral.eq (app_bool or_term [true_term; bool_x]) true_term] in
  (* or F x = x *)
  let or_false =
    [SLiteral.eq (app_bool or_term [false_term; bool_x]) bool_x] in
  (* impl T x = x *)
  let impl_t =
    [SLiteral.eq (app_bool impl_term [true_term; bool_x]) bool_x] in
  (* impl F x = T *)
  let impl_f =
    [SLiteral.eq (app_bool impl_term [false_term; bool_x]) true_term] in
  let equiv_def =
    [SLiteral.eq 
      (app_bool equiv_term [bool_x; bool_y]) 
      (app_bool and_term [app_bool impl_term [bool_x; bool_y];
                          app_bool impl_term [bool_y; bool_x]])] in
  let xor_def =
    [SLiteral.eq 
      (app_bool xor_term [bool_x; bool_y]) 
      (app_bool not_term [app_bool equiv_term [bool_x; bool_y]])] in

  let eq_x_y = app_bool eq_term [alpha;alpha_x;alpha_y] in
  let neq_x_y = app_bool neq_term [alpha;alpha_x;alpha_y] in
  
  (* x != y \/ eq x y = T *)
  let eq_true =
    [SLiteral.neq alpha_x alpha_y; SLiteral.eq eq_x_y true_term] in
  (* x = y \/ eq x y = F *)
  let eq_false =
    [SLiteral.eq alpha_x alpha_y; SLiteral.eq eq_x_y false_term] in
  (* neq x y = not (eq x y) *)
  let neq_is_not_eq =
    [SLiteral.eq neq_x_y (app_bool not_term [eq_x_y])] in
  
  let lambda_x_true = T.close_with_vars ~binder:Binder.Lambda [alpha_x] true_term in

  (* forall (\x. T) = T *)
  let forall_true =
    [SLiteral.eq (app_bool forall_term [alpha; lambda_x_true]) true_term] in
  (* P = \x. T \/ forall P = F *)
  let forall_false =
    [SLiteral.eq alpha2bool_p lambda_x_true;
     SLiteral.eq (app_bool forall_term [alpha; alpha2bool_p]) false_term] in

  (* \x. not (P x) *)
  let l_not_p_x = 
    T.close_with_vars ~binder:Binder.Lambda [alpha_x]
      (app_bool not_term [app_bool alpha2bool_p [alpha_x]]) in
  (* exists P = not (forall (\x. not (P x))) *)
  let exists_def = 
    [SLiteral.eq 
      (app_bool exists_term [alpha; alpha2bool_p])
      (app_bool not_term [app_bool forall_term [alpha; l_not_p_x]])] in
  
  (* P X *)
  let p_x = 
    app_bool alpha2bool_p [alpha_x] in
  
  let p_choice_p =
    app_bool alpha2bool_p 
      [T.app ~ty:alpha choice_term [alpha; alpha2bool_p]] in
  
  (* p x \/ p (choice p) *)
  let choice_def = 
    [SLiteral.eq (p_x) (false_term);
     SLiteral.eq (p_choice_p) (true_term);] in

    Iter.of_list [either_true_or_false; true_neq_false; and_true; and_false;
                  not_true; not_false; or_true; or_false; eq_true; eq_false;
                  neq_is_not_eq; forall_true; forall_false; exists_def;
                  impl_t; impl_f; equiv_def; xor_def; choice_def] 


let bool_encode_ty ty_orig =
  let rec aux ty =
    match T.view ty with
      | T.App (f, args) ->
        T.app ~ty:T.tType (aux f) 
          (CCList.map aux args)
      | T.AppBuiltin (Builtin.Arrow, ret::args) ->
        let ret' = aux ret in
        let args' = List.map aux args in
        T.Ty.fun_ args' ret'
      | T.AppBuiltin (f,args) ->
        assert(f != Builtin.Arrow);
        if f == Builtin.Prop then bool_clone_ty
        else T.app_builtin ~ty:T.tType f (List.map aux args)
      | T.Const _ -> ty
      | T.Var _ -> ty
      | T.Bind (b,v,t) -> T.bind ~ty:T.tType b v (aux t)
      | T.Meta _ -> failwith "Not implemented: Meta"
      | T.Ite (_,_,_) -> failwith "Not implemented: Ite"
      | T.Let _ -> failwith "Not implemented: Let"
      | T.Match (_,_) -> failwith "Not implemented: Match"
      | T.Multiset _ -> failwith "Not implemented: Multiset"
      | T.Record (_,_) -> failwith "Not implemented: Record" in
    aux ty_orig


let bool_encode_term t_orig  =
  let rec aux t = 
    let encode_var v =  
      let ty = bool_encode_ty (Var.ty v) in
      Var.update_ty v ~f:(fun _ -> ty) in
    
    try
      if T.equal (T.ty_exn t) T.tType 
      then bool_encode_ty t
      else (
        let ty = bool_encode_ty (CCOpt.get_exn (T.ty t)) in
        match T.view t with
          | T.Const c -> 
            T.const ~ty c
          | T.Var v ->
            T.var (encode_var v)
          | T.App (f, []) -> aux f
          | T.App (f, args) ->
            let f' = aux f in
            let args' = List.map aux args in
            T.app ~ty f' args'
          | T.AppBuiltin (((And|Or) as b), ts) ->
            let head = if b == And then and_term else or_term in
            begin match ts with 
            | [] -> head
            | [x] -> app_bool head [aux x]
            | x :: y :: tts ->
              let init = app_bool head [aux x; aux y] in
              List.fold_left (fun acc arg -> app_bool head [acc;aux arg]) init tts
            end
          | T.AppBuiltin (((Eq|Equiv|Neq|Xor) as b), [x;y]) when (T.Ty.is_prop (T.ty_exn x)) ->
            assert (T.equal (T.ty_exn x) (T.ty_exn y));
            let head = if b = Equiv || b = Eq then equiv_term else xor_term in
            let x = aux x and y = aux y in
            app_bool head [x; y]
          | T.AppBuiltin (((Eq|Neq) as b), ([x;y]|[_;x;y])) ->
            assert (T.equal (T.ty_exn x) (T.ty_exn y));
            let head = if b = Eq then eq_term else neq_term in
            let x = aux x and y = aux y in
            let ty_arg = T.ty_exn x in
            app_bool head [ty_arg; x; y]
          | T.AppBuiltin ((Imply), [x;y]) ->
            assert (T.equal (T.ty_exn x) (T.ty_exn y));
            let x = aux x and y = aux y in
            app_bool impl_term [x;y]
          | T.AppBuiltin (((ForallConst|ExistsConst) as b), ([x]|[_;x])) ->
            let x = aux x in
            let _, args, _ = T.Ty.unfold (T.ty_exn x) in
            assert(List.length args = 1);
            let ty_arg = List.hd args in
            let head = if b = ForallConst then forall_term else exists_term in
            app_bool head [ty_arg; x]
          | T.AppBuiltin (Not, l) -> app_bool not_term (List.map aux l)
          | T.AppBuiltin (True, []) -> true_term
          | T.AppBuiltin (False, []) -> false_term
          | T.AppBuiltin (f, ts) ->
            assert (not ((T.equal t T.Form.true_) || (T.equal t T.Form.false_)));
            T.app_builtin ~ty f (List.map aux ts)
          | T.Bind (Binder.Lambda, var, body) ->
            let var' = encode_var var in
            let body' = aux body in
            let ty = bool_encode_ty (CCOpt.get_exn (T.ty t)) in
            T.bind ~ty Binder.Lambda var' body'
          | T.Bind (((Forall|Exists as b)), x, body) ->
            let head = if b = Forall then forall_term else exists_term in
            let x = encode_var x in
            let ty_arg = Var.ty x in
            let fun_body = T.close_with_vars ~binder:Binder.Lambda [T.var x] (aux body) in
            app_bool head [ty_arg; fun_body]
          | _ -> failwith "Not implemented: Other kind of term")
      with Invalid_argument ty_err ->
        let err = 
          CCFormat.sprintf 
            "Subterm @[%a@]:@[%a@] of @[%a@] cannot be encoded because of type error: @[%s@]"
            T.pp t T.pp (T.ty_exn t) T.pp t_orig ty_err in
        if CCString.prefix ~pre:"type" ty_err then invalid_arg err
        else invalid_arg ty_err in
        
  let res = aux t_orig in
  Util.debugf ~section 1 "Encoded term @[%a@] into @[%a@]" (fun k -> k T.pp t_orig T.pp res);
  res

let bool_encode_lit lit = 
  match lit with 
  | SLiteral.True -> SLiteral.eq true_term true_term
  | False -> SLiteral.neq true_term true_term
  | Atom(atom, sign) ->
    let mk_equ_lit = if sign then SLiteral.eq else SLiteral.neq in
    let encoded_atom = bool_encode_term atom in
    assert(T.equal (T.ty_exn true_term) (T.ty_exn encoded_atom));
    mk_equ_lit (bool_encode_term atom) true_term
  | Eq _ | Neq _ ->
    SLiteral.map ~f:bool_encode_term lit

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
      let axioms = 
        Iter.map (Statement.assert_ ~proof:Proof.Step.trivial) boolean_axioms in
      (* Add type declarations *)
      let seq = Iter.append ty_decls (Iter.append axioms seq) in
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
