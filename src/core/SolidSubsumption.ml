module TS = Term.Set
module T  = Term
module C  = Clause

exception SolidMatchFail

type multiterm =
  (* Application of Builtin constant to a list of multiterms.
     Third argument are possible replacements. *)
  | AppBuiltin of Builtin.t * multiterm list * TS.t
  (* Application of constants or bound variables to a list of multiterms.
     Head can be represented possibly in many ways!
     Third argument are possible replacements. *)
  | App of TS.t * multiterm list * TS.t
  (* Lambda abstraction of multiterms *)
  | Fun of Type.t * multiterm 
  (* Replacements that are either bound variables or constants *)
  | Repl of TS.t

let bvar_or_const t =
  Term.is_const t || Term.is_bvar t

let app_builtin b args repls =
  if TS.for_all bvar_or_const repls then (
    AppBuiltin(b,args,repls)
  ) else invalid_arg "replacements must be bound vars or constants"

let app hd args repls =
  if TS.for_all bvar_or_const hd then (
    if TS.for_all bvar_or_const repls then App (hd,args,repls)
    else invalid_arg "replacements must be bound vars or constants"
  ) else invalid_arg "head of multiterm is bound var or constant"

let fun_ ty body =
  Fun(ty,body)

let fun_l ty_args body =
  List.fold_right fun_ ty_args body

let open_builtin = function
  | AppBuiltin(hd,args,repls) -> (hd,args,repls)
  | _ -> invalid_arg "cannot open builtin"

let open_app = function
  | App(hds,args,repls) -> (hds,args,repls)
  | _ -> invalid_arg "cannot open app"

let open_fun = function
  | Fun(ty,bodys) -> (ty,bodys)
  | _ -> invalid_arg "cannot open fun"

let open_repl = function
  | Repl repls -> repls
  | _ -> invalid_arg "cannot open repl"

let repl repls = 
  if TS.for_all bvar_or_const repls then (
    Repl repls
  ) else invalid_arg "replacements must be bound vars or constants"


let cover t solids : multiterm = 
  assert(List.for_all T.is_ground solids);
  (* If the term is not of base type, then it must be a bound variable *)
  assert(List.for_all (fun t -> not @@ Type.is_fun @@ T.ty t || T.is_bvar t) solids);
  let n = List.length solids in

  let rec aux ~depth s_args t : multiterm  =
    (* All the ways in which we can represent term t using solids *)
    let sols_as_db = List.mapi (fun i t -> 
      (t,T.bvar ~ty:(T.ty t) (n-i-1+depth))) s_args in
    let find_solids target = 
      (CCList.filter_map (fun (s, s_db) -> 
        if T.equal s target then Some s_db else None) 
      sols_as_db)
      |> TS.of_list in
    let db_hits = find_solids t in

    match T.view t with
    | AppBuiltin (hd,args) ->
      app_builtin hd (List.map (aux ~depth s_args) args) db_hits
    | App(hd,args) ->
      assert(not (CCList.is_empty args));
      assert(bvar_or_const hd);
      let hds = TS.add hd @@ find_solids hd in
      let args = List.map (aux ~depth s_args) args in
      app hds args db_hits
    | Fun _ -> 
      assert(TS.is_empty db_hits);
      let ty_args, body = T.open_fun t in
      let d_inc = List.length ty_args in
      let s_args' = List.map (T.DB.shift d_inc) s_args in
      let res = aux ~depth:(depth+d_inc) s_args' body in
      fun_l ty_args res
    | DB i when i >= depth ->
      if TS.is_empty db_hits then raise SolidMatchFail
      else repl db_hits
    | _ -> repl (TS.add t db_hits) in
  
  aux ~depth:0 solids t

let term_intersection s t =
  let rec aux s t =  
    match s with 
    | AppBuiltin(s_b,s_args,s_repls) ->
      let (t_b,t_args,t_repls) = open_builtin t in
      if s_b = t_b then (
        let args = List.map (fun (s,t) -> aux s t) @@ 
                    List.combine s_args t_args in
        app_builtin s_b args (TS.inter s_repls t_repls)
      ) else raise SolidMatchFail
    | App(s_hds,s_args,s_repls) ->
      let (t_hds,t_args,t_repls) = open_app t in
      let i_hds = TS.inter s_hds t_hds in
      if TS.is_empty i_hds then (
        let args = List.map (fun (s,t) -> aux s t) @@ 
                      List.combine s_args t_args in
        app i_hds args (TS.inter s_repls t_repls)
      ) else raise SolidMatchFail
    | Fun(s_ty,s_bodys) ->
      let t_ty,t_bodys = open_fun t in
      if Type.equal s_ty t_ty then (
        fun_ s_ty (aux s_bodys t_bodys)
      ) else raise SolidMatchFail
    | Repl(repls) ->
      repl (TS.inter repls (open_repl t))
  in
  try 
    aux s t
  with Invalid_argument s ->
    Util.debugf 3 "Incompatible constructors: %s" (fun k -> k s);
    raise SolidMatchFail

  (* let normaize_clauses subsumer target =
    let target' = Clause_intf. *)