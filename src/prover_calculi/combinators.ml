open Logtk

module T = Term
module Ty = Type

type conv_rule = T.t -> T.t option

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
  forall @@ forall ([db_alpha; db_beta] ==> db_alpha)

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
  ) else T.app_builtin ~ty comb_head (ty_args @ args)

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
   Παβ. α → β → α *)
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
  | _ -> invalid_arg "argument is not a combinator"

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

(* [2]. S (K X) I -> X *)
let opt2 t = 
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

(* [3]. S (K X) Y -> B X Y *)
let opt3 t = 
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

(* [4]. S X (K Y) -> C X Y *)
let opt4 t = 
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

let curry_optimizations = [opt1;opt2;opt3;opt4]

(* Assumes beta-reduced, eta-short term *)
let abf t =
  let rec abstract ~bvar ~depth t =
    invalid_arg "not implemented" in

  let rec aux ~depth t =
    match T.view t with 
    | T.AppBuiltin _ | T.App _ ->
      let hd_mono, args = T.as_app_mono t in
      let args' = List.map (aux ~depth) args in
      if T.same_l args args' then t
      else T.app hd_mono args' (* flattens AppBuiltin if necessary *)
    | T.Fun(ty, body) ->
      let body' = aux ~depth:(depth+1) in
      abstract ~depth:(depth+1) ~bvar:(T.bvar ~ty 0) body'
    | _ ->  t in
  aux ~depth:0 t