open Logtk
open Libzipperposition

module T = Term
module Ty = Type

type conv_rule = T.t -> T.t option
exception IsNotCombinator

let k_enable_combinators = Flex_state.create_key ()

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end


module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx
  module Fool = Fool.Make(Env)


  (* see mk_s *)
  let ty_s =
    let db_alpha = Ty.bvar 2 in
    let db_beta = Ty.bvar 1 in
    let db_gamma = Ty.bvar 0 in
    
    let open Type in
    let prefix ty = forall @@ forall @@ forall ty in
      prefix
        ([[db_alpha; db_beta] ==> db_gamma;
          [db_alpha] ==> db_beta;
          db_alpha] ==> db_gamma)

  (* see mk_c *)
  let ty_c =
    let db_alpha = Ty.bvar 2 in
    let db_beta = Ty.bvar 1 in
    let db_gamma = Ty.bvar 0 in
    
    let open Type in
    let prefix ty = forall @@ forall @@ forall ty in
      prefix
        ([[db_alpha; db_beta] ==> db_gamma;
          db_beta;
          db_alpha] ==> db_gamma)

  (* see mk_b *)
  let ty_b =
    let db_alpha = Ty.bvar 2 in
    let db_beta = Ty.bvar 1 in
    let db_gamma = Ty.bvar 0 in
    
    let open Type in
    let prefix ty = forall @@ forall @@ forall ty in
      prefix
        ([[db_alpha] ==> db_beta;
          [db_beta] ==> db_alpha;
          db_gamma] ==> db_beta)

  (* see mk_k *)
  let ty_k =
    let db_alpha = Ty.bvar 1 in
    let db_beta = Ty.bvar 0 in  

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

  let [@inline] hd_is_comb hd =
    match hd with
    | Builtin.SComb | Builtin.CComb | Builtin.BComb 
    | Builtin.KComb | Builtin.IComb -> true
    | _ -> false

  let [@inline] term_is_comb t =
    match T.view t with
    | T.AppBuiltin(hd, _) when hd_is_comb hd -> true
    | _ -> false

  let [@inline] term_has_comb ~comb t =
    match T.view t with
    | T.AppBuiltin(hd, _) when Builtin.equal comb hd -> true
    | _ -> false

  (* Returns the cobminator head, type arguments and real arguments 
    of a combinator *)
  let [@inline] unpack_comb t =
    match T.view t with 
    | T.AppBuiltin(hd, args) when hd_is_comb hd ->
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

  let curry_optimizations = [opt1;opt2;opt3;opt4]

  let optimize ~opts t =
    let rec aux = function 
    | f :: fs ->
      begin match f t with 
      | Some t' -> 
        assert (Type.equal (T.ty t) (T.ty t'));
        t'
      | None -> aux fs end
    | [] -> t in
    aux opts

  (* Assumes beta-reduced, eta-short term *)
  let abf ~opts t =
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
          ret_ty, optimize ~opts raw_res
        ) (T.ty hd_mono, hd_conv) args in
        optimize ~opts raw_res
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
    aux t

  exception E_i of Statement.clause_t


  let pp_in pp_f pp_t pp_ty = function
    | Output_format.O_zf -> Statement.ZF.pp pp_f pp_t pp_ty
    | Output_format.O_tptp -> Statement.TPTP.pp pp_f pp_t pp_ty
    | Output_format.O_normal -> Statement.pp pp_f pp_t pp_ty
    | Output_format.O_none -> CCFormat.silent

  let pp_clause_in o =
    let pp_term = T.pp_in o in
    let pp_type = Type.pp_in o in
    pp_in (Util.pp_list ~sep:" ∨ " (SLiteral.pp_in o pp_term)) pp_term pp_type o


  let result_tc =
    Proof.Result.make_tc
      ~of_exn:(function E_i c -> Some c | _ -> None)
      ~to_exn:(fun i -> E_i i)
      ~compare:compare
      ~pp_in:pp_clause_in
      ~is_stmt:true
      ~name:Statement.name
      ~to_form:(fun ~ctx st ->
        let conv_c c =
          CCList.to_seq c 
          |> Iter.flat_map (fun l -> SLiteral.to_seq l)
          |> Iter.map (fun t -> Term.Conv.to_simple_term ctx t)
          |> Iter.to_list
          |> TypedSTerm.Form.or_ in
        Statement.Seq.forms st
        |> Iter.map conv_c
        |> Iter.to_list
        |> TypedSTerm.Form.and_)
      ()


    let encode_lit ~opts l =
      SLiteral.map (abf ~opts) l
    
    let rec encode_clause ~opts = function 
      | [] -> []
      | l :: ls -> 
        encode_lit ~opts l :: encode_clause ~opts ls

    let enocde_stmt st =
      let opts = curry_optimizations in
      let rule = Proof.Rule.mk "lambdas_to_combs" in
      let as_proof = 
        Proof.S.mk (Statement.proof_step st) (Proof.Result.make result_tc st) in
      let proof = 
        Proof.Step.esa ~rule [Proof.Parent.from as_proof] in

      match Statement.view st with
      | Statement.Def _ | Statement.Rewrite _ | Statement.Data _ 
      | Statement.Lemma _ | Statement.TyDecl _ -> E.cr_skip
      | Statement.Goal lits | Statement.Assert lits ->
        let lits' = encode_clause ~opts lits in
        E.cr_return @@ [E.C.of_forms ~trail:Trail.empty lits' proof]
      | Statement.NegatedGoal (skolems,clauses) -> 
        let clauses' = 
          List.map (fun c -> 
            E.C.of_forms ~trail:Trail.empty (encode_clause ~opts c) proof) 
          clauses in
        E.cr_add clauses'
        (* E.cr_return @@ 
          Statement.neg_goal ~proof ~skolems (List.map (encode_clause ~opts) clauses) *)
    
    let setup () =
      if E.flex_get k_enable_combinators then (
        E.add_clause_conversion enocde_stmt;
      )

end

let _enable_combinators = ref false


let extension =
  let lam2combs seq = seq in

  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    E.flex_add k_enable_combinators !_enable_combinators;

    ET.setup ()
  in
  { Extensions.default with
      Extensions.name = "combinators";
      env_actions=[register];
      post_cnf_modifiers=[lam2combs];
  }

let () =
  Options.add_opts
    [ "--combinator-based-reasoning", Arg.Bool (fun v -> _enable_combinators := v), "enable / disable combinator based reasoning"];
  Extensions.register extension