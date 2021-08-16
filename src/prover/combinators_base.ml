open Logtk

module T = Term
module Ty = Type

exception IsNotCombinator

(* Helper function for defining combinators *)
(* see mk_s *)
let ty_s =
  let db_alpha = Ty.bvar 2 and db_beta = Ty.bvar 1 and db_gamma = Ty.bvar 0 in
  
  let open Type in
  let prefix ty = forall @@ forall @@ forall ty in
  prefix
    ([[db_alpha; db_beta] ==> db_gamma; [db_alpha] ==> db_beta; db_alpha]
      ==> db_gamma)

(* see mk_c *)
let ty_c =
  let db_alpha = Ty.bvar 2 and db_beta = Ty.bvar 1 and db_gamma = Ty.bvar 0 in
  
  let open Type in
  let prefix ty = forall @@ forall @@ forall ty in
  prefix
    ([[db_alpha; db_beta] ==> db_gamma; db_beta; db_alpha] 
      ==> db_gamma)

(* see mk_b *)
let ty_b =
  let db_alpha = Ty.bvar 2 and db_beta = Ty.bvar 1 and db_gamma = Ty.bvar 0 in
  
  let open Type in
  let prefix ty = forall @@ forall @@ forall ty in
  prefix
    ([[db_alpha] ==> db_beta; [db_gamma] ==> db_alpha; db_gamma] 
    ==> db_beta)

(* see mk_k *)
let ty_k =
  let db_alpha = Ty.bvar 1 and db_beta = Ty.bvar 0 in  

  let open Type in
  forall @@ forall ([db_beta; db_alpha] ==> db_beta)

(* see mk_i *)
let ty_i =
  let db_alpha = Ty.bvar 0 in  

  let open Type in
  forall ([db_alpha] ==> db_alpha)


let [@inline] mk_comb comb_head ty ty_args args =
  (* optmization: if args is empty, the whole 
    ty_args will be traversed *)
  if CCList.is_empty args then (
    T.app_builtin ~ty comb_head ty_args
  ) else T.app (T.app_builtin ~ty comb_head ty_args) args

(* make S combinator with the type:
  Παβγ. (α→β→γ) → (α→β) → α → γ *)
let mk_s ~args ~alpha ~beta ~gamma =
  let ty = Ty.apply ty_s [Type.of_term_unsafe (alpha : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (beta : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (gamma : Term.t :> InnerTerm.t);] in
  mk_comb Builtin.SComb ty [alpha;beta;gamma] args

(* make C combinator with the type:
  Παβγ. (α→β→γ) → β → α → γ *)
let mk_c ~args ~alpha ~beta ~gamma =
  let ty = Ty.apply ty_c [Type.of_term_unsafe (alpha : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (beta : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (gamma : Term.t :> InnerTerm.t);] in
  mk_comb Builtin.CComb ty [alpha;beta;gamma] args

(* make B combinator with the type:
  Παβγ. (α→β) → (γ→α) → γ → β *)
let mk_b ~args ~alpha ~beta ~gamma =
  let ty = Ty.apply ty_b [Type.of_term_unsafe (alpha : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (beta : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (gamma : Term.t :> InnerTerm.t);]  in
  mk_comb Builtin.BComb ty [alpha;beta;gamma] args

(* make K combinator with the type:
  Παβ. β → α → β *)
let mk_k ~args ~alpha ~beta =
  let ty = Ty.apply ty_k [Type.of_term_unsafe (alpha : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (beta : Term.t :> InnerTerm.t)] in
  mk_comb Builtin.KComb ty [alpha;beta] args

(* make I combinator with the type:
  Πα. α → α *)
let mk_i ~args ~alpha =
  let ty = Ty.apply ty_i [Type.of_term_unsafe (alpha : Term.t :> InnerTerm.t)] in
  mk_comb Builtin.IComb ty [alpha] args

(* {2 Helper functions} *)
let [@inline] term_has_comb ~comb t =
  match T.view t with
  | T.AppBuiltin(hd, _) when Builtin.equal comb hd -> true
  | _ -> false

(* Returns the cobminator head, type arguments and real arguments 
  of a combinator *)
let [@inline] unpack_comb t =
  match T.view t with 
  | T.AppBuiltin(hd, args) when Builtin.is_combinator hd ->
    let ty_args, real_args = List.partition Term.is_type args in
    (hd, ty_args, real_args)
  | _ -> raise IsNotCombinator
  (* Given type arguments of S, calculate correct type arguments 
    for B *)
  let s2b_tyargs ~alpha ~beta ~gamma =
    (beta, gamma, alpha)

(* {3 Narrowing and optimization functions} *)

(* Rules for optimizing the abf algorithm, as laid out in the paper 
  Martin W. Bunder -- Some Improvements to Turner's Algorithm for 
  Bracket Abstraction \url{https://ro.uow.edu.au/eispapers/1962/}

  They are numbered as they are numbered in the paper.
  Some of the rules are applicable only for SKBCI combinators,
  but due to the abf algorithm design it is easy to extend it.
*)

(* [1]. S (K X) (K Y) -> K (X Y) *)
let opt1 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.SComb c_kind then (
      match args,ty_args with 
      | [u;v],[alpha;_;beta] ->
        begin match unpack_comb u, unpack_comb v with
        | (Builtin.KComb,_,[x]), (Builtin.KComb,_,[y]) ->
          let xy = Term.app x [y] in
          Some (mk_k ~args:[xy] ~alpha ~beta )
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* [2]. S (K X) I -> X *)
let opt2 t =
  try
    let c_kind,_,args = unpack_comb t in
    if Builtin.equal Builtin.SComb c_kind then (
      match args with 
      | [u;v] ->
        begin match unpack_comb u, unpack_comb v with 
        | (Builtin.KComb, _, [x]), (Builtin.IComb, _, []) ->
          Some x
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* [3]. S (K X) Y -> B X Y *)
let opt3 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.SComb c_kind then (
      match args,ty_args with 
      | [u;y], [alpha;beta;gamma] ->
        begin match unpack_comb u with 
        | (Builtin.KComb, _, [x])->
          let alpha,beta,gamma = s2b_tyargs ~alpha ~beta ~gamma in
          Some (mk_b ~args:[x;y] ~alpha ~beta ~gamma)
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* [4]. S X (K Y) -> C X Y *)
let opt4 t =
  try
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.SComb c_kind then (
      match args,ty_args with 
      | [x;u], [alpha;beta;gamma] ->
        begin match unpack_comb u with 
        | (Builtin.KComb, _, [y])->
          Some (mk_c ~args:[x;y] ~alpha ~beta ~gamma)
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* Definition of S:
    S X Y Z t1 ... tn -> X Z (Y Z) t1 ... tn *)
let narrowS t =
  try
    let c_kind,_,args = unpack_comb t in
    if Builtin.equal Builtin.SComb c_kind then (
      match args with 
      | x :: y :: z :: rest ->
        Some (T.app x (z :: (T.app y [z]) ::rest))
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* Definition of B:
    B X Y Z t1 ... tn -> X (Y Z) t1 ... tn *)
let narrowB t =
  try
    let c_kind,_,args = unpack_comb t in
    if Builtin.equal Builtin.BComb c_kind then (
      match args with 
      | x :: y :: z :: rest ->
        Some (T.app x ((T.app y [z]) ::rest))
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* Definition of C:
    C X Y Z t1 ... tn -> X Z Y t1 ... tn *)
let narrowC t =
  try
    let c_kind,_,args = unpack_comb t in
    if Builtin.equal Builtin.CComb c_kind then (
      match args with 
      | x :: y :: z :: rest ->
        Some (T.app x (z :: y ::rest))
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* Definition of K:
    K X Y t1 ... tn -> X t1 ... tn *)
let narrowK t =
  try
    let c_kind,_,args = unpack_comb t in
    if Builtin.equal Builtin.KComb c_kind then (
      match args with 
      | x :: y :: rest ->
        Some (T.app x rest)
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* Definition of I:
    I X t1 ... tn -> X t1 ... tn *)
let narrowI t =
  try
    let c_kind,_,args = unpack_comb t in
    if Builtin.equal Builtin.IComb c_kind then (
      match args with 
      | x :: rest ->
        Some (T.app x rest)
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* 5. S (K X) = B X *)
let opt5 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.SComb c_kind then (
      match args, ty_args with 
      | [kx], [alpha;beta;gamma] ->
        begin match unpack_comb kx with 
        | (Builtin.KComb, _, [x])->
          let alpha,beta,gamma = s2b_tyargs ~alpha ~beta ~gamma in
          Some (mk_b ~args:[x] ~alpha ~beta ~gamma)
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* Optimizations from Bunder's paper *)
(* 6. B X (K Y) = K (X Y) *)
let opt6 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.BComb c_kind then (
      match args with 
      | [x;ky] ->
        begin match unpack_comb ky with 
        | (Builtin.KComb, [alpha;_], [y])->
          let xy = T.app x [y] in
          Some (mk_k ~args:[xy] ~alpha ~beta:(Term.of_ty @@ T.ty xy))
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* 7. B X I = X *)
let opt7 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.BComb c_kind then (
      match args with 
      | [x;i] ->
        begin match unpack_comb i with 
        | (Builtin.IComb, _, [])->
          Some x
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* 8. C (K X) Y = K (X Y) *)
let opt8 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.CComb c_kind then (
      match args with 
      | [kx;y] ->
        begin match unpack_comb kx with 
        | (Builtin.KComb, [alpha;_], [x])->
          let xy = T.app x [y] in
          Some (mk_k ~args:[xy] ~alpha ~beta:(Term.of_ty @@ T.ty xy))
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* 9. B I (x) = I (x) *)
let opt9 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.BComb c_kind then (
      match args with 
      | [i] ->
        let alpha = Term.of_ty @@ List.hd @@ Type.expected_args @@ T.ty t in
        begin match unpack_comb i with 
        | (Builtin.IComb, _, []) -> Some (mk_i ~args:[] ~alpha)
        | _ -> None end
      | [i; x] ->
        begin match unpack_comb i with 
        | (Builtin.IComb, _, []) -> Some x
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* 10. S K X = I *)
let opt10 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.SComb c_kind then (
      match args with 
      | [k;x] ->
        begin match unpack_comb k with 
        | (Builtin.KComb, [_;beta], []) -> Some (mk_i ~args:[] ~alpha:beta)
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* 11. S (B K X) Y = X *)
let opt11 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.SComb c_kind then (
      match args with 
      | [bkx;y] ->
        begin match unpack_comb bkx with 
        | (Builtin.BComb, _, [k;x]) ->
          begin match unpack_comb k with 
          | (Builtin.KComb, _, []) -> Some x
          | _ -> None end
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

(* 12. S (B K X) = K X *)
let opt12 t =
  try 
    let c_kind,ty_args,args = unpack_comb t in
    if Builtin.equal Builtin.SComb c_kind then (
      match args with 
      | [bkx] ->
        begin match unpack_comb bkx with
        | (Builtin.BComb, _, [k;x]) ->
          begin match unpack_comb k with
          | (Builtin.KComb, _, []) ->
            let alpha = Term.of_ty @@ List.hd @@ Type.expected_args @@ T.ty t in
            let beta = T.of_ty @@ T.ty x in
            Some (mk_k ~args:[x] ~alpha ~beta)
          | _ -> None end
        | _ -> None end
      | _ -> None
    ) else None
  with IsNotCombinator -> None

let bunder_optimizations = [opt5;opt6;opt7;opt8;opt9;opt10;opt11;opt12]
let curry_optimizations = [opt1;opt2;opt3;opt4]
let narrow_rules = [narrowS; narrowB; narrowC; narrowK; narrowI]

let apply_rw_rules ~rules t =
  let rec aux = function 
  | f :: fs ->
    begin match f t with 
    | Some t' -> 
      if not (Type.equal (T.ty t) (T.ty t')) then (
        CCFormat.printf "t:@[%a@]::@[%a@]@." T.pp t Ty.pp (T.ty t);
        CCFormat.printf "t':@[%a@]::@[%a@]@." T.pp t' Ty.pp (T.ty t');
        assert false;
      );
      t'
    | None -> aux fs end
  | [] -> t in
  aux rules

let narrow t =
  let rules = narrow_rules @ curry_optimizations @ bunder_optimizations in
  let rec do_narrow t =
    match T.view t with 
    | T.Const _ | T.Var _ | T.DB _-> t
    | T.AppBuiltin(hd, args) -> 
      let t' = apply_rw_rules ~rules t in
      if T.equal t t' then (
        (* If no rule is applicable, then this is either not 
           a combinator or a paritally applied combinator *)
        let args' = List.map do_narrow args in
        if T.same_l args args' then t
        else T.app_builtin hd args' ~ty:(T.ty t)
      ) else (do_narrow t')
    | T.App(hd, args) ->
      let hd' = do_narrow hd and args' = List.map do_narrow args in
      if T.equal hd hd' && T.same_l args args' then t
      else T.app hd' args'
    | T.Fun _ ->
      let tys, body = T.open_fun t in
      let body' = do_narrow body in
      if T.equal body body' then t
      else T.fun_l tys body' 
    in
  do_narrow t

type state = {
  mutable pos_counter : int;
  mutable neg_counter : int;
  mutable balance : CCInt.t Term.Tbl.t;
  mutable var_map : T.t Term.Tbl.t
}

let comb_map_args t new_args =
  let hd, args = T.as_app_mono t in
  assert(List.length args = List.length new_args);
  T.app hd new_args

(** add a positive variable *)
let add_pos_var state var =
  let n = Term.Tbl.get_or state.balance var ~default:0 in
  if n = 0  then state.pos_counter <- state.pos_counter + 1
  else (
    if n = -1 then state.neg_counter <- state.neg_counter - 1
  );
  Term.Tbl.add state.balance var (n + 1)

(** add a negative variable *)
let add_neg_var state var =
  let n = Term.Tbl.get_or state.balance var ~default:0 in
  if n = 0 then state.neg_counter <- state.neg_counter + 1
  else (
    if n = 1 then state.pos_counter <- state.pos_counter - 1
  );
  Term.Tbl.add state.balance var (n - 1)

let max_weak_reduction_length var_handler ~state orig_t =
  let rec aux t  = 
    match T.view t with
    | T.DB _ | T.Fun _ ->
      let err_msg = 
        CCFormat.sprintf "lambdas should be removed@.orig:@[%a@];subterm:@[%a@]@." T.pp orig_t T.pp t in
      invalid_arg err_msg
    | T.Const _ -> 0
    | T.Var _ ->
      var_handler state t;
      0
    | T.App (hd, l) ->
      assert(not @@ T.is_var hd);
      let steps_hd = aux hd in
      let steps_args = aux_l l in
      steps_hd + steps_args
    | T.AppBuiltin(b, l) when Builtin.is_combinator b ->
      assert(T.is_ground t);
      let c_kind, _, args =  unpack_comb t in
      begin match c_kind with 
      | Builtin.IComb ->
        if CCList.is_empty args then 0
        else (aux (narrow_one t) + 1)
      | Builtin.KComb ->
        if CCList.length args < 2 then (
          aux_l args)
        else (
          let steps_inc = 1 + (aux (List.nth args 1)) in
          let steps_rest = aux (narrow_one t) in
          steps_rest + steps_inc)
      | Builtin.SComb | Builtin.CComb | Builtin.BComb -> 
        if CCList.length args < 3 then (aux_l args)
        else (aux (narrow_one t) + 1)  
      | _ -> invalid_arg "only combinators are supported" end
    | T.AppBuiltin(b, l) -> aux_l l
  and aux_l = function 
    | [] -> 0
    | t :: ts ->  aux t + aux_l ts
  and narrow_one t = 
    let t' = apply_rw_rules ~rules:narrow_rules t in
    assert (not @@ T.equal t' t);
    t'; in
  aux orig_t

let cmp_by_max_weak_r_len t1 t2 =
  let numvars = Iter.length (T.Seq.vars t1) + Iter.length (T.Seq.vars t2) in
  let state =
    { pos_counter = 0; neg_counter = 0;
      balance = Term.Tbl.create numvars;
      var_map = Term.Tbl.create 16 } in

  let rec encode_vars t =
    let encode t =
      match T.Tbl.get state.var_map t with 
      | Some t' -> t'
      | None ->
        let fresh_var = T.var (HVar.fresh ~ty:(T.ty t) ()) in
        T.Tbl.add state.var_map t fresh_var;
        fresh_var in

    if T.is_ground t then t
    else (
      match T.view t with
      | T.App(hd,l) ->
        if T.is_var hd then (
          encode t
        ) else (
          (* head is not the variable, only args have to be managed *)
          let l' = List.map encode_vars l in
          if T.same_l l l' then t else T.app hd l'
        )
      | T.AppBuiltin(hd, l) ->
        if Builtin.is_combinator hd then (
          assert(not (T.is_ground t));
          encode t
        ) else (
          let l' = List.map encode_vars l in
          if T.same_l l l' then t else T.app_builtin ~ty:(T.ty t) hd l'
        )
      | _ -> t) in

  let t1,t2 = CCPair.map_same encode_vars (t1,t2) in
  let return res = res,t1,t2 in
  let t1_len = max_weak_reduction_length add_pos_var ~state t1 in
  let t2_len = max_weak_reduction_length add_neg_var ~state t2 in
  if t1_len > t2_len then (
    if state.neg_counter > 0 then return Comparison.Incomparable else return Comparison.Gt
  ) else if t1_len < t2_len then (
    if state.pos_counter > 0 then return Comparison.Incomparable else return Comparison.Lt
  ) else return Comparison.Eq

(* Assumes beta-reduced, eta-short term *)
let abf ~rules t =
  let rec abstract ~bvar_ty t =
    match T.view t with 
    | T.DB 0 -> mk_i ~alpha:bvar_ty ~args:[]
    | T.DB i -> 
      let ty = T.ty t in
      mk_k ~alpha:bvar_ty ~beta:(Term.of_ty ty) ~args:[T.bvar ~ty (i-1)]
    | T.Const _ | T.Var _ ->
      let beta = Term.of_ty @@ T.ty t in
      mk_k ~alpha:bvar_ty ~beta ~args:[t]
    | T.AppBuiltin _ | T.App _ -> 
      let hd_mono, args = T.as_app_mono t in
      assert(not @@ T.is_fun hd_mono);
      let hd_conv =
        if T.is_app hd_mono || T.is_appbuiltin hd_mono then (
          let beta = Term.of_ty @@ T.ty hd_mono in
          mk_k ~alpha:bvar_ty ~beta ~args:[hd_mono]
        ) else abstract ~bvar_ty hd_mono in
      let _, raw_res = List.fold_left (fun (l_ty, l_conv) r ->
        let r_conv = abstract ~bvar_ty r in
        let ret_ty = Ty.apply_unsafe l_ty [(r :> InnerTerm.t)] in
        let raw_res =
          mk_s ~alpha:bvar_ty ~beta:(Term.of_ty @@ T.ty r)
              ~gamma:(Term.of_ty ret_ty) ~args:[l_conv;r_conv] in
        ret_ty, apply_rw_rules ~rules raw_res
      ) (T.ty hd_mono, hd_conv) args in
      apply_rw_rules ~rules raw_res
    | T.Fun _ ->
      invalid_arg "all lambdas should be abstracted away!" in

  let rec aux t =
    match T.view t with 
    | T.AppBuiltin _ | T.App _ ->
      let hd_mono, args = T.as_app_mono t in
      let args' = List.map aux args in
      
      assert (not (T.is_fun hd_mono));
      begin try
        if T.same_l args args' then t
        else T.app hd_mono args' (* flattens AppBuiltin if necessary *)
      with Type.ApplyError _ ->
        CCFormat.printf "hd:@[%a@]; type:@[%a@]@." T.pp hd_mono Type.pp (T.ty hd_mono);
        CCFormat.printf "tys of args: @[%a@]" (CCList.pp Ty.pp) (CCList.map T.ty args');
        CCFormat.printf "error for @[%a@]@." T.pp t;
        assert false; end
    | T.Fun(ty, body) ->
      let body' = aux body in
      abstract ~bvar_ty:(Term.of_ty ty) body'
    | _ ->  t in
  let reduced = Lambda.eta_reduce ~expand_quant:false @@ Lambda.snf @@ t in
  let res = aux reduced in
  (* CCFormat.printf "abf(@[%a@])=" T.pp reduced;
  CCFormat.printf "@.   @[%a@]@." T.pp res; *)
  res


let comb_normalize t =
  let changed = ref false in
  let rules = curry_optimizations @ bunder_optimizations in
  let t =
    if T.has_lambda t then (
      changed := true;
      abf ~rules t
    ) else t in
  let t' = narrow t in
  if not (T.equal t t') then changed := true;
  if !changed then Some t' else None

let comb2lam t =
  let rec aux t =
    match T.view t with
    | T.App(hd, args) ->
      let hd' = aux hd in
      let args' = List.map aux args in
      T.app hd' args'
    | T.AppBuiltin(b, args) when Builtin.is_combinator b ->
      let hd, args = T.as_app_mono t in
      let args' = List.map aux args in
      let ty_args = (fst (Type.open_fun (T.ty hd))) in
      let (--) = List.nth in
      let (-|) = (fun l i -> CCList.take i l) in
      let lam = 
        begin match b with
        | Builtin.SComb ->
          let x = T.bvar ~ty:(ty_args -- 0) 2 in
          let y = T.bvar ~ty:(ty_args -- 1) 1 in
          let z = T.bvar ~ty:(ty_args -- 2) 0 in
          let xz_yz = T.app x [z; T.app y [z]] in
          T.fun_l (ty_args -| 3) xz_yz
        | Builtin.BComb ->
          let x = T.bvar ~ty:(ty_args -- 0) 2 in
          let y = T.bvar ~ty:(ty_args -- 1) 1 in
          let z = T.bvar ~ty:(ty_args -- 2) 0 in
          let x_yz = T.app x [T.app y [z]] in
          T.fun_l (ty_args -| 3) x_yz
        | Builtin.CComb ->
          let x = T.bvar ~ty:(ty_args -- 0) 2 in
          let y = T.bvar ~ty:(ty_args -- 1) 1 in
          let z = T.bvar ~ty:(ty_args -- 2) 0 in
          let xzy = T.app x [z; y] in
          T.fun_l (ty_args -| 3) xzy
        | Builtin.KComb ->
          T.fun_l (ty_args -| 2) (T.bvar ~ty:(ty_args -- 0) 1)
        | Builtin.IComb ->
          T.fun_l (ty_args -| 1) (T.bvar ~ty:(ty_args -- 0) 0)
        | _ -> assert false
        end in
      let res = Lambda.eta_reduce ~expand_quant:false @@ Lambda.snf @@ T.app lam args' in
      assert (Type.equal (T.ty t) (T.ty res));
      res
    | T.AppBuiltin(b, args) ->
      let args' = List.map aux args in
      T.app_builtin ~ty:(T.ty t) b args'
    | T.Fun(ty, body) -> T.fun_ ty (aux body)
    | _ -> t in
  let res = aux t in
  assert(Iter.for_all (fun sub -> 
    not (T.is_comb sub))
    (T.Seq.subterms ~include_builtin:true ~include_app_vars:true res));
  res
