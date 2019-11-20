open Logtk

(* see mk_s *)
let ty_s =
  let db_alpha = Type.bvar 2 in
  let db_beta = Type.bvar 1 in
  let db_gamma = Type.bvar 0 in
  
  let open Type in
  let prefix ty = forall @@ forall @@ forall ty in
    prefix
      ([[db_alpha; db_beta] ==> db_gamma;
        [db_alpha] ==> db_beta;
        db_alpha] ==> db_gamma)

(* see mk_c *)
let ty_c =
  let db_alpha = Type.bvar 2 in
  let db_beta = Type.bvar 1 in
  let db_gamma = Type.bvar 0 in
  
  let open Type in
  let prefix ty = forall @@ forall @@ forall ty in
    prefix
      ([[db_alpha; db_beta] ==> db_gamma;
        db_beta;
        db_alpha] ==> db_gamma)

(* see mk_b *)
let ty_b =
  let db_alpha = Type.bvar 2 in
  let db_beta = Type.bvar 1 in
  let db_gamma = Type.bvar 0 in
  
  let open Type in
  let prefix ty = forall @@ forall @@ forall ty in
    prefix
      ([[db_alpha] ==> db_beta;
        [db_beta] ==> db_alpha;
        db_gamma] ==> db_beta)

(* see mk_k *)
let ty_k =
  let db_alpha = Type.bvar 1 in
  let db_beta = Type.bvar 0 in  

  let open Type in
  forall @@ forall ([db_alpha; db_beta] ==> db_alpha)

(* see mk_i *)
let ty_i =
  let db_alpha = Type.bvar 0 in  

  let open Type in
  forall ([db_alpha] ==> db_alpha)


let mk_comb comb_head ty ty_args =
  Term.app_builtin ~ty comb_head ty_args

(* make S combinator with the type:
   Παβγ. (α→β→γ) → (α→β) → α → γ *)
let mk_s ~alpha ~beta ~gamma =
  let ty = Type.apply ty_s [alpha;beta;gamma] in
  let ty_args = List.map Term.of_ty [alpha;beta;gamma] in
  mk_comb Builtin.SComb ty ty_args

(* make C combinator with the type:
   Παβγ. (α→β→γ) → β → α → γ *)
let mk_c ~alpha ~beta ~gamma =
  let ty = Type.apply ty_c [alpha;beta;gamma] in
  let ty_args = List.map Term.of_ty [alpha;beta;gamma] in
  mk_comb Builtin.CComb ty ty_args

(* make B combinator with the type:
   Παβγ. (α→β) → (γ→α) → γ → β *)
let mk_b ~alpha ~beta ~gamma =
  let ty = Type.apply ty_b [alpha;beta;gamma] in
  let ty_args = List.map Term.of_ty [alpha;beta;gamma] in
  mk_comb Builtin.BComb ty ty_args

(* make K combinator with the type:
   Παβ. α → β → α *)
let mk_k ~alpha ~beta =
  let ty = Type.apply ty_k [alpha;beta] in
  let ty_args = List.map Term.of_ty [alpha;beta] in
  mk_comb Builtin.KComb ty ty_args

(* make I combinator with the type:
   Πα. α → α *)
let mk_i ~alpha =
  let ty = Type.apply ty_i [alpha] in
  let ty_args = [Term.of_ty alpha] in
  mk_comb Builtin.IComb ty ty_args
