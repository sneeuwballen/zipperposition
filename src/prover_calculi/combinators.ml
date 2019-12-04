open Logtk
open Libzipperposition

module T = Term
module Ty = Type
module Lits = Literals
module Lit = Literal


let section = Util.Section.make ~parent:Const.section "combs"

type conv_rule = T.t -> T.t option
exception IsNotCombinator

let k_enable_combinators = Saturate.k_enable_combinators
let k_s_penalty = Flex_state.create_key ()
let k_b_penalty = Flex_state.create_key ()
let k_c_penalty = Flex_state.create_key ()
let k_k_penalty = Flex_state.create_key ()
let k_deep_app_var_penalty = Flex_state.create_key ()



module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)
  val setup : unit -> unit
  val maybe_conv_lams : Env.C.t -> Env.C.t
end

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
let mk_s ?(args=[]) ~alpha ~beta ~gamma =
  let ty = Ty.apply ty_s [Type.of_term_unsafe (alpha : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (beta : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (gamma : Term.t :> InnerTerm.t);] in
  mk_comb Builtin.SComb ty [alpha;beta;gamma] args

(* make C combinator with the type:
  Παβγ. (α→β→γ) → β → α → γ *)
let mk_c ?(args=[]) ~alpha ~beta ~gamma =
  let ty = Ty.apply ty_c [Type.of_term_unsafe (alpha : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (beta : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (gamma : Term.t :> InnerTerm.t);] in
  mk_comb Builtin.CComb ty [alpha;beta;gamma] args

(* make B combinator with the type:
  Παβγ. (α→β) → (γ→α) → γ → β *)
let mk_b ?(args=[]) ~alpha ~beta ~gamma =
  let ty = Ty.apply ty_b [Type.of_term_unsafe (alpha : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (beta : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (gamma : Term.t :> InnerTerm.t);]  in
  mk_comb Builtin.BComb ty [alpha;beta;gamma] args

(* make K combinator with the type:
  Παβ. β → α → β *)
let mk_k ?(args=[]) ~alpha ~beta =
  let ty = Ty.apply ty_k [Type.of_term_unsafe (alpha : Term.t :> InnerTerm.t);
                          Type.of_term_unsafe (beta : Term.t :> InnerTerm.t)] in
  mk_comb Builtin.KComb ty [alpha;beta] args

(* make I combinator with the type:
  Πα. α → α *)
let mk_i ?(args=[]) ~alpha =
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

(* 9. B I = I *)
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

let binder_optimizations = [opt5;opt6;opt7;opt8;opt9;opt10;opt11;opt12]
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
  let rules = narrow_rules @ binder_optimizations in
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

let max_weak_reduction_length var_handler ~state t =
  let rec aux t  = 
    match T.view t with
    | T.DB _ | T.Fun _ ->
      let err_msg = 
        CCFormat.sprintf "lambdas should be removed:@[%a@]@." T.pp t in
      invalid_arg err_msg
    | T.Const _ -> 0, t 
    | T.Var _ ->
      var_handler state t;
      0, t
    | T.App (hd, l)->
      if T.is_var hd then (
        match T.Tbl.get state.var_map t with 
        | Some t' ->
          var_handler state t';
          0, t'
        | None ->
          let fresh_var = T.var (HVar.fresh ~ty:(T.ty t) ()) in
          T.Tbl.add state.var_map t fresh_var;
          var_handler state fresh_var;
          0, fresh_var)
      else (
        let steps_hd, hd' = aux hd in
        let steps_args, l' = aux_l l in
        if T.equal hd hd' && T.same_l l' l then (
          assert(steps_hd = 0 && steps_args = 0);
          0, t)
        else steps_hd + steps_args, T.app hd' l')
    | T.AppBuiltin(b, l) when Builtin.is_combinator b ->
      if T.is_ground t then (
        let c_kind, ty_args, args =  unpack_comb t in
        begin match c_kind with 
        | Builtin.IComb ->
          if CCList.is_empty args then 0, t
          else (
            let steps, t' = aux (narrow_one t) in
            steps+1, t')
        | Builtin.KComb ->
          if CCList.length args < 2 then (
            let steps, args' = aux_l args in
            if T.same_l args args' then (
              assert(steps = 0);
              0, t
            ) else steps, comb_map_args t args')
          else (
            let steps_inc = 1 + fst (aux (List.nth args 1)) in
            let steps_rest, t' = aux (narrow_one t) in
            steps_rest + steps_inc, t')
        | Builtin.SComb | Builtin.CComb | Builtin.BComb -> 
          if CCList.length args < 3 then (
            let steps, args' = aux_l args in
            if T.same_l args args' then (
              assert(steps = 0);
              0, t
            ) else steps, comb_map_args t args')
          else (
            let steps_rest, t' = aux (narrow_one t) in
            steps_rest + 1, t')  
        | _ -> invalid_arg "only combinators are supported" end)
      else (
        let fresh_var = Ordering.ty1comb_to_var t state.var_map in
        var_handler state fresh_var;
        0, fresh_var)
    | T.AppBuiltin(b, l) ->
      let steps, l' = aux_l l in
      if T.same_l l l' then (
        assert (steps = 0);
        0, t) 
      else (steps, T.app_builtin ~ty:(T.ty t) b l')
  and aux_l = function 
    | [] -> 0, []
    | t :: ts -> 
      let steps, t' = aux t in
      let steps_rest , t_rest = aux_l ts in
      steps+steps_rest, t' :: t_rest
  and narrow_one t = 
    let t' = apply_rw_rules ~rules:narrow_rules t in
    assert (not @@ T.equal t' t);
    t'; in
  aux t

let cmp_by_max_weak_r_len t1 t2 =
  let numvars = Iter.length (T.Seq.vars t1) + Iter.length (T.Seq.vars t2) in
  let state =
    { pos_counter = 0; neg_counter = 0;
      balance = Term.Tbl.create numvars;
      var_map = Term.Tbl.create 16 } in
  let t1_len,_ = max_weak_reduction_length add_pos_var ~state t1 in
  let t2_len,_ = max_weak_reduction_length add_neg_var ~state t2 in
  if t1_len > t2_len then (
    if state.neg_counter > 0 then Comparison.Incomparable else Comparison.Gt
  ) else if t1_len < t2_len then (
    if state.pos_counter > 0 then Comparison.Incomparable else Comparison.Lt
  ) else Comparison.Eq

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
      if T.same_l args args' then t
      else T.app hd_mono args' (* flattens AppBuiltin if necessary *)
    | T.Fun(ty, body) ->
      let body' = aux body in
      abstract ~bvar_ty:(Term.of_ty ty) body'
    | _ ->  t in
  let reduced = Lambda.eta_reduce @@ Lambda.snf @@ t in
  let res = aux reduced in
  (* CCFormat.printf "abf(@[%a@])=" T.pp reduced;
  CCFormat.printf "@.   @[%a@]@." T.pp res; *)
  res

let comb_normalize t =
  let changed = ref false in
  let rules = curry_optimizations @ binder_optimizations in
  let t =
    if T.has_lambda t then (
      changed := true;
      abf ~rules t
    ) else t in
  let t' = narrow t in
  if not (T.equal t t') then changed := true;
  if !changed then Some t' else None

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx
  module Fool = Fool.Make(Env)

    let has_lams_aux = 
      Iter.exists (fun t ->
        T.Seq.subterms ~include_builtin:true ~ignore_head:false t 
        |> Iter.exists T.is_fun)

    let has_lams_c c = 
      has_lams_aux @@ C.Seq.terms c
    
    let has_lams_lits lits =
      CCList.to_seq lits 
      |> Iter.flat_map (SLiteral.to_seq)
      |> has_lams_aux

    let enocde_stmt st =
      let rule = Proof.Rule.mk "lams2combs" in
      let rules = curry_optimizations @ binder_optimizations in
      E.cr_return @@ List.map (fun c -> 
        if has_lams_c c then (
          let proof = Proof.Step.simp [C.proof_parent c] ~rule in
          let lits' = Literals.map (abf ~rules) (C.lits c) in
          C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
          (Array.to_list lits') proof
        ) else c
      )(E.C.of_statement st)
      
    let comb_narrow c =
      let new_lits = Literals.map narrow (C.lits c) in
      if Literals.equal (C.lits c) new_lits then (
        SimplM.return_same c
      ) else (
        let proof = Proof.Step.simp [C.proof_parent c] 
                      ~rule:(Proof.Rule.mk "narrow combinators") in
        let new_ = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
                    (Array.to_list new_lits) proof in
        SimplM.return_new new_
      )

    let tyvarA = HVar.fresh ~ty:Ty.tType ()
    let tyvarB = HVar.fresh ~ty:Ty.tType ()
    let tyvarC = HVar.fresh ~ty:Ty.tType ()

    let type_of_vars ?(args=[]) ~ret =
      let open Ty in
      if CCList.is_empty args then Ty.var ret
      else List.map Ty.var args ==> Ty.var ret

    (* Create the arguments of type appropriate to be applied to the combinator *)
    let s_arg1 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarA;tyvarB] ~ret:tyvarC) ()
    let s_arg2 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarA] ~ret:tyvarB) ()

    let b_arg1 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarA] ~ret:tyvarB) ()
    let b_arg2 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarC] ~ret:tyvarA) ()

    let c_arg1 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[tyvarA;tyvarB] ~ret:tyvarC) ()
    let c_arg2 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[] ~ret:tyvarB) ()

    let k_arg1 =
      T.var @@ HVar.fresh ~ty:(type_of_vars ~args:[] ~ret:tyvarB) ()

    (* Partially applies a combinator with arguments
        arguments:
          comb: original combinator with penalty for instantianting clause with it
          args: arguments with corresponding pentalties *)
    let partially_apply ~comb args =
      let orig_comb, penalty = comb in
      let rec aux acc = function 
        | [] -> []
        | (a,p) :: aas ->
          let acc = T.app acc [a] in
          (acc,p) :: aux acc aas in
      (orig_comb,penalty) :: aux orig_comb args

    let alpha = T.var tyvarA 
    let beta = T.var tyvarB
    let gamma = T.var tyvarC

    let partially_applied_s =
      partially_apply ~comb:(mk_s ~alpha ~beta ~gamma ~args:[], 0)
        [s_arg1, 1; s_arg2, Env.flex_get k_s_penalty]

    let partially_applied_b =
      partially_apply ~comb:(mk_b ~alpha ~beta ~gamma ~args:[], 0)
        [b_arg1, 1; b_arg2, Env.flex_get k_b_penalty]

    let partially_applied_c =
      partially_apply ~comb:(mk_c ~alpha ~beta ~gamma ~args:[], 0)
        [c_arg1, 1; c_arg2, Env.flex_get k_c_penalty]
    
    let partially_applied_k =
      partially_apply ~comb:(mk_k ~alpha ~beta ~args:[], 0)
        [k_arg1, Env.flex_get k_k_penalty]
    
    let partially_applied_i =
      [mk_i ~alpha ~args:[], 0]

    let partially_applied_combs =
      partially_applied_s @ partially_applied_b @ partially_applied_c @ 
      partially_applied_k @ partially_applied_i
      (* partially_applied_b @ partially_applied_s @ partially_applied_i *)
      


    let instantiate_var_w_comb ~var =
      CCList.filter_map (fun (comb, penalty) ->
        try
          Some (Unif.FO.unify_syn (comb, 0) (var, 1), penalty)
        with Unif.Fail -> None
      ) partially_applied_combs


    let narrow_app_vars clause =
      let rule = Proof.Rule.mk "narrow applied variable" in
      let tags = [Proof.Tag.T_ho] in

      let ord = Env.ord () in 
      let eligible = C.Eligible.(res clause) in
      let lits = C.lits clause in
      (* do the inferences in which clause is passive (rewritten),
        so we consider both negative and positive literals *)
      Lits.fold_terms ~vars:(false) ~var_args:(true) ~fun_bodies:(false) 
                      ~subterms:true ~ord ~which:`Max ~eligible ~ty_args:false
      lits
      (* Variable has at least one arugment *)
      |> Iter.filter (fun (u_p, _) -> T.is_app_var u_p)
      |> Iter.flat_map_l (fun (u, u_pos) -> 
        (* variable names as in Ahmed's paper (unpublished) *)
        let var = T.head_term u in
        assert(T.is_var var);
        CCList.filter_map (fun (subst, comb_penalty) ->
          let renaming = Subst.Renaming.create () in
          let lit_idx, lit_pos = Lits.Pos.cut u_pos in

          Util.debugf ~section 2 "narrow vars:@[%a@]:@[%d@]" (fun k -> k C.pp clause lit_idx);

          let lit = Lit.apply_subst_no_simp renaming subst (lits.(lit_idx), 1) in
          if not (Lit.Pos.is_max_term ~ord lit lit_pos) ||
             not (CCBV.get (C.eligible_res (clause, 1) subst) lit_idx) then (
            Util.debugf ~section 2 "ordering restriction fail: @[%a@]@." (fun k -> k Subst.pp subst);
            None)
          else (
            let t_depth = Position.size (Literal.Pos.term_pos (lits.(lit_idx)) lit_pos) in
            let depth_mul = 
              if not @@ Env.flex_get k_deep_app_var_penalty then 1
              else max t_depth 1 in
            let lits' = CCArray.to_list @@ Lits.apply_subst renaming subst (lits, 1) in
            let proof = 
              Proof.Step.inference ~rule ~tags
                [C.proof_parent_subst renaming (clause,1) subst] in
            let penalty = depth_mul * comb_penalty + C.penalty clause in
            let new_clause = C.create ~trail:(C.trail clause) ~penalty lits' proof in

            Util.debugf ~section 2 "success: @[%a@]@." (fun k -> k C.pp new_clause);

            Some new_clause
          )) (instantiate_var_w_comb ~var))
        |> Iter.to_list

    let lams2combs_otf c =
      if not @@ has_lams_c c then SimplM.return_same c
      else (
        let rules = curry_optimizations @ binder_optimizations in
        let proof = Proof.Step.simp [C.proof_parent c] 
                      ~rule:(Proof.Rule.mk "lams2combs on-the-fly") in
        let lits' = Literals.map (abf ~rules) (C.lits c) in
        let new_ = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
                    (Array.to_list lits') proof in
        SimplM.return_new new_)

    let maybe_conv_lams c =
      if E.flex_get k_enable_combinators then (
        SimplM.get (lams2combs_otf c)
      ) else c
    
    let setup () =
      if E.flex_get k_enable_combinators then (
        E.add_clause_conversion enocde_stmt;
        E.add_unary_inf "narrow applied variable" narrow_app_vars;
        E.Ctx.set_ord (Ordering.compose cmp_by_max_weak_r_len (E.Ctx.ord ()));
        Unif.disable_pattern_unif := true;
      )

end

let _enable_combinators = ref false
let _s_penalty = ref 6
let _b_penalty = ref 2
let _c_penalty = ref 3
let _k_penalty = ref 2
let _app_var_constraints = ref false
let _deep_app_var_penalty = ref false


let extension =
  let lam2combs seq = seq in

  let register env =
    let module E = (val env : Env.S) in
    E.flex_add k_enable_combinators !_enable_combinators;
    E.flex_add k_s_penalty !_s_penalty;
    E.flex_add k_c_penalty !_c_penalty;
    E.flex_add k_b_penalty !_b_penalty;
    E.flex_add k_k_penalty !_k_penalty;
    E.flex_add k_deep_app_var_penalty !_deep_app_var_penalty;
    Unif.app_var_constraints := !_app_var_constraints;


    let module ET = Make(E) in
    ET.setup ()
  in
  { Extensions.default with
      Extensions.name = "combinators";
      env_actions=[register];
      post_cnf_modifiers=[lam2combs];
  }

let () =
  Options.add_opts
    [ "--combinator-based-reasoning", Arg.Bool (fun v -> _enable_combinators := v), "enable / disable combinator based reasoning";
     "--app-var-constraints", Arg.Bool (fun v -> _app_var_constraints := v), "enable / disable delaying app var clashes as constraints";
     "--penalize-deep-appvars", Arg.Bool (fun v -> _deep_app_var_penalty := v), "enable / disable penalizing narrow app var inferences with deep variables";
     "--comb-s-penalty", Arg.Set_int _s_penalty, "penalty for narrowing with $S X Y";
     "--comb-c-penalty", Arg.Set_int _c_penalty, "penalty for narrowing with $C X Y";
     "--comb-b-penalty", Arg.Set_int _b_penalty, "penalty for narrowing with $B X Y";
     "--comb-k-penalty", Arg.Set_int _k_penalty, "penalty for narrowing with $K X"];
  Params.add_to_mode "ho-comb-complete" (fun () ->
    _enable_combinators := true;
  );
  Extensions.register extension;
