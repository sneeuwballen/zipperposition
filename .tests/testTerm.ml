
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test terms *)

open Logtk
open Logtk_arbitrary

module T = Term
module H = Helpers

let (==>) = QCheck.(==>)
let t_test = (module T : Alcotest.TESTABLE with type t = T.t)
let ty_test = (module Type : Alcotest.TESTABLE with type t = Type.t)
(* unit tests *)

let f_ = ID.make "f"
let g_ = ID.make "g"
let h_ = ID.make "h"
let k_ = ID.make "k"
let l_ = ID.make "l"
let p_ = ID.make "p"
let q_ = ID.make "q"


let ty = Type.term
let ty_t = Term.of_ty ty 
let prop = Type.prop
let f_fun = (T.const ~ty:Type.([ty;ty] ==> ty) f_)
let f x y = T.app f_fun [x; y]
let g_fun = T.const ~ty:Type.([ty] ==> ty) g_
let g x = T.app g_fun [x]
let h x y z = T.app (T.const ~ty:Type.([ty;ty;ty] ==> ty) h_) [x;y;z]
let h_fun = T.const ~ty:Type.([ty;ty;ty] ==> ty) h_
let k_fun = (T.const ~ty:Type.([[ty;ty] ==> ty] ==> ty) k_)
let k x = T.app k_fun [x]
let l_fun = (T.const ~ty:Type.([[ty;ty] ==> ty;ty] ==> ty) l_)
let l x = T.app l_fun [x]
let a = T.const ~ty (ID.make "a")
let b = T.const ~ty (ID.make "b")
let x = T.var_of_int ~ty 0
let y = T.var_of_int ~ty 1
let fun_var x y = T.app (T.var_of_int ~ty:Type.([ty;ty] ==> ty) 2) [x; y]
let quant body = T.app_builtin ~ty:prop Builtin.ExistsConst [ty_t; body]
let p_fun = T.const ~ty:Type.([ty] ==> prop) p_
let q_fun = T.const ~ty:Type.([[ty] ==> prop; ty] ==> prop) q_

let test_db_shift = "db shift", `Quick, fun () ->
  let t = T.fun_ ty (f (T.bvar ~ty 0) (g (T.bvar ~ty 1))) in
  let t' = T.of_term_unsafe (InnerTerm.DB.shift 1 (t:T.t:>InnerTerm.t)) in
  let t1 = T.fun_ ty (f (T.bvar ~ty 0) (g (T.bvar ~ty 2))) in
  Alcotest.(check t_test) "db_shift" t1 t';
  ()

let test_app_var = "is_appvar", `Quick, fun () -> 
  let app_var = fun_var a b in
  let not_app_var = x in
  Alcotest.(check bool) "is_appvar" (T.is_app_var app_var) true;
  Alcotest.(check bool) "is_nappvar" (T.is_app_var not_app_var) false;
  ()

let test_db_unshift = "db unshift", `Quick, fun () ->
  let t = T.fun_ ty (f (T.bvar ~ty 0) (g (T.bvar ~ty 2))) in
  let t' = T.of_term_unsafe (InnerTerm.DB.unshift 1 (t:T.t:>InnerTerm.t)) in
  let t1 = T.fun_ ty (f (T.bvar ~ty 0) (g (T.bvar ~ty 1))) in
  Alcotest.(check t_test) "db_unshift" t1 t';
  ()

let test_covers = "cover_correct", `Quick, fun () ->
  let t = T.fun_ ty (h y b (h x b x)) in
  let coverings = T.cover_with_terms t [Some b;Some y;Some a;Some y] in
  let max_cover = T.max_cover t [Some b; Some (h x b x)] in
  let str = CCFormat.sprintf "%a.\n" (CCList.pp T.pp) coverings in
  Alcotest.(check int) str (List.length coverings) 12;
  let str = CCFormat.sprintf "%a.\n" T.pp max_cover in
  Alcotest.(check t_test) str max_cover (T.fun_ ty (h y (T.bvar ~ty 2) (T.bvar ~ty 1)));
  ()


let test_whnf1 = "whnf1", `Quick, fun () ->
  (* eta expansion of [g] *)
  let g_eta = T.fun_ ty (g (T.bvar ~ty 0)) in
  let redex =
    let ty2 = Type.([ty] ==> ty) in
    T.app
      (T.fun_ ty2
         (f
           (T.app (T.bvar ~ty:ty2 0) [a])
           (T.app (T.bvar ~ty:ty2 0) [b])))
      [g_eta]
  in
  let t' = Lambda.whnf redex in
  (* WHNF: does not reduce in subterms *)
  let t1 = f (T.app g_eta [a]) (T.app g_eta [b]) in
  Alcotest.(check t_test) "whnf1" t1 t';
  ()

let test_whnf2 = "whnf2", `Quick, fun () ->
  let redex =
    let ty2 = Type.([ty] ==> ty) in
    T.app
      (T.fun_ ty2
         (f
           (T.app (T.bvar ~ty:ty2 0) [a])
           (T.app (T.bvar ~ty:ty2 0) [b])))
      [g_fun]
  in
  let t' = Lambda.whnf redex in
  let t1 = f (g a) (g b) in
  Alcotest.(check t_test) "whnf2" t1 t';
  ()

let test_whnf2 = "patterns", `Quick, fun () ->
  let t1 = f (g a) (g b) in 
  let t2 = T.fun_ ty (f (T.bvar ~ty 0) (g (T.bvar ~ty 1))) in
  let t3 = T.fun_ ty (f (fun_var (T.bvar ~ty 0) (T.bvar ~ty 1)) (g (T.bvar ~ty 1))) in 
  let t4 = T.fun_ ty (f (fun_var (T.bvar ~ty 0) (T.bvar ~ty 0)) (g (T.bvar ~ty 1))) in 
  let t5 = T.fun_ ty (f (fun_var (T.bvar ~ty 0) (g (T.bvar ~ty 1))) (g (T.bvar ~ty 1))) in


  Alcotest.(check bool) "pattern1" (Lambda.is_lambda_pattern t1) true;
  Alcotest.(check bool) "pattern2" (Lambda.is_lambda_pattern t2) true;
  Alcotest.(check bool) "pattern3" (Lambda.is_lambda_pattern t3) true;
  Alcotest.(check bool) "pattern3" (Lambda.is_lambda_pattern t4) false;
  Alcotest.(check bool) "pattern3" (Lambda.is_lambda_pattern t5) false;

  ()

let test_polymorphic_app = "poly app", `Quick, fun () ->
  (* Π α. α *)
  let polyty = Type.forall_fvars [HVar.make ~ty:Type.tType 0] (Type.var_of_int 0) in
  let f_poly = Term.const ~ty:polyty (ID.make "f_poly") in
  (* ty → ty *)
  let funty = Type.([ty] ==> ty) in
  (* apply term of type `Π α. α` to terms of type `ty → ty` and `ty`: *)
  let result = Term.app f_poly [Term.of_ty funty; a] in
  Alcotest.(check ty_test) "polyapp" (Term.ty result) ty;
  ()

let test_eta_reduce = "eta reduce", `Quick, fun () ->
  (* (λx. (λy. f x y)) -> f *)
  let t1 = T.fun_of_fvars [HVar.make ~ty 0; HVar.make ~ty 1] (T.app f_fun [T.var_of_int ~ty 0; T.var_of_int ~ty 1]) in
  let t2 = f_fun in
  Alcotest.(check t_test) "eta reduce" (Lambda.eta_reduce t1) t2;
  (* (λx. (λy. g y) x) -> g *)
  let t1 = T.fun_of_fvars [HVar.make ~ty 0] (T.app (T.fun_of_fvars [HVar.make ~ty 1] (g (T.var_of_int ~ty 1))) [T.var_of_int ~ty 0]) in
  let t2 = g_fun in
  Alcotest.(check t_test) "eta reduce" (Lambda.eta_reduce t1) t2;
  Alcotest.(check t_test) "eta reduce" (Lambda.eta_reduce t1) t2;
  (* (λx y. f y x) -> does not reduce *)
  let t1 = T.fun_of_fvars [HVar.make ~ty 0; HVar.make ~ty 1] (f (T.var_of_int ~ty 1) (T.var_of_int ~ty 0)) in
  let t2 = t1 in
  Alcotest.(check t_test) "eta reduce" (Lambda.eta_reduce t1) t2;
  (* (λx y. h (g x) x y) -> (λx. h (g x) x) *)
  let t1 = T.fun_of_fvars [HVar.make ~ty 0; HVar.make ~ty 1] (h (g (T.var_of_int ~ty 0)) (T.var_of_int ~ty 0) (T.var_of_int ~ty 1)) in
  let t2 = T.fun_of_fvars [HVar.make ~ty 0] (T.app h_fun [g (T.var_of_int ~ty 0); (T.var_of_int ~ty 0)]) in
  Alcotest.(check t_test) "eta reduce" (Lambda.eta_reduce t1) t2;

  let subterm =  T.fun_of_fvars [HVar.make ~ty 0; HVar.make ~ty 1] (T.app f_fun [T.var_of_int ~ty 0; T.var_of_int ~ty 1]) in
  let unreduced = k subterm in 
  let reduced = k f_fun in
  Alcotest.(check t_test) "eta reduce" (Lambda.eta_reduce unreduced) reduced;
  
  (* ∃ p -> ∃ (λx. p x)
     In HOL, we actually need to expand this subterm *)
  let lamx_px = T.fun_of_fvars [HVar.make ~ty 0] (T.app p_fun [T.var_of_int ~ty 0]) in

  let reduced = quant p_fun in
  let expanded = quant lamx_px in

  CCFormat.printf "t: @[%a@]@." T.pp reduced;
  CCFormat.printf "eta-red(t): @[%a@]@." T.pp (Lambda.eta_reduce reduced);
  CCFormat.printf "expected: @[%a@]@." T.pp expanded;

  CCFormat.printf "trying@.";
  Alcotest.(check t_test) "eta reduce" (Lambda.eta_reduce reduced) expanded;
  CCFormat.printf "failed@.";

  (* ∃ (q (λx. p x)) -> ∃ (λy. (q p y))
     We expand on the top level, but further down we reduce *)
  let unreduced = quant (T.app q_fun [lamx_px]) in
  let reduced = 
    quant @@ 
      T.fun_of_fvars [HVar.make ~ty 0] (T.app q_fun [p_fun; T.var_of_int ~ty 0]) in
  Alcotest.(check t_test) "eta reduce" (Lambda.eta_reduce unreduced) reduced

let test_eta_expand = "eta expand", `Quick, fun () ->
  (* f -> (λx. (λy. f x y)) *)
  let t1 = T.fun_of_fvars [HVar.make ~ty 0; HVar.make ~ty 1] (T.app f_fun [T.var_of_int ~ty 0; T.var_of_int ~ty 1]) in
  let t2 = f_fun in
  Alcotest.(check t_test) "eta expand" (Lambda.eta_expand t2) t1;

  let subterm =  T.fun_of_fvars [HVar.make ~ty 0; HVar.make ~ty 1] (T.app f_fun [T.var_of_int ~ty 0; T.var_of_int ~ty 1]) in
  let unreduced = k subterm in 
  let reduced = k f_fun in
  Alcotest.(check t_test) "eta expand" (Lambda.eta_expand reduced) unreduced;

  let subterm =  T.fun_of_fvars [HVar.make ~ty 0; HVar.make ~ty 1] (T.app f_fun [T.var_of_int ~ty 0; T.var_of_int ~ty 1]) in
  let expanded = T.fun_of_fvars [HVar.make ~ty 0] (T.app l_fun [subterm; T.var_of_int ~ty 0]) in 
  let unexpanded = l f_fun in
  Alcotest.(check t_test) "eta expand"  expanded (Lambda.eta_expand unexpanded)


let suite : unit Alcotest.test_case list =
  [ test_db_shift;
    test_db_unshift;
    test_app_var;
    test_whnf1;
    test_whnf2;
    test_polymorphic_app;
    test_eta_reduce;
    test_eta_expand;
    test_covers;
  ]

(** Properties *)

let ar_fo = ArTerm.default
let ar_t = ArTerm.default_ho
let gen_t = ArTerm.default_ho_g

(* subterm is smaller than term *)
let check_size_subterm =
  (* choose a subterm of t *)
  let gen = QCheck.Gen.(
    gen_t >>= fun t ->
    ArTerm.pos t >>= fun pos ->
    return (t, T.Pos.at t pos))
  in
  let pp = QCheck.Print.(pair T.to_string T.to_string) in
  let gen = QCheck.make ~print:pp gen in
  let prop (t1, t2) =
    T.subterm ~sub:t2 t1 &&
    T.size t1 >= T.size t2
  in
  QCheck.Test.make ~name:"term_size_subterm" gen prop

(* replace subterm by itself yields same term *)
let check_replace_id =
  let gen = QCheck.Gen.(
    gen_t >>= fun t ->
    ArTerm.pos t >>= fun pos ->
    return (t, pos))
  in
  let pp = QCheck.Print.(pair T.to_string Position.to_string) in
  let gen = QCheck.make ~print:pp gen in
  let prop (t, pos) =
    let sub = T.Pos.at t pos in
    if T.DB.is_closed sub
    then (
      let t' = T.Pos.replace t pos ~by:sub in
      T.equal t t'
    ) else QCheck.assume_fail()
  in
  QCheck.Test.make ~name:"term_replace_same_subterm" gen prop

let check_ground_novar =
  let gen = ar_t in
  let prop t =
    not (T.is_ground t) || Iter.is_empty (T.Seq.vars t)  (* ground => no vars *)
  in
  QCheck.Test.make ~count:1000 ~name:"term_ground_has_no_var" gen prop

let check_min_max_vars =
  let gen = ar_t in
  let prop t =
    let vars = T.vars t in
    T.VarSet.is_empty vars || (T.min_var vars <= T.max_var vars)
  in
  QCheck.Test.make ~count:1000 ~name:"term_min_max_var" gen prop

let check_hash_mod_alpha =
  let gen = QCheck.pair ar_fo ar_fo in
  let prop (t1,t2) =
    if not (T.equal t1 t2) && Unif.FO.are_variant t1 t2
    then
      T.hash_mod_alpha t1 = T.hash_mod_alpha t2
    else QCheck.assume_fail ()
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:20
    ~name:"variant_have_same_hash_mod_alpha"

(* TODO: write a term arbitrary instance for DB terms (lambda terms?)
   and check that a shifted/unshifted closed term remains closed *)

let is_fun t = match T.view t with T.Fun _ -> true | _ -> false

let num_lam t =
  T.Seq.subterms t
  |> Iter.filter is_fun
  |> Iter.length

let num_var_app t =
  T.Seq.subterms t
  |> Iter.filter T.is_ho_app
  |> Iter.length

(* NOTE: this enables stats *)
let add_stat = ref false

let gen_ho =
  let a =
    ArTerm.default_ho
    |> QCheck.set_gen (ArTerm.default_ho_fuel 8)
    |> QCheck.set_print T.ZF.to_string
  in
  if !add_stat then (
    a
    |> QCheck.add_stat ("lambdas",num_lam)
    |> QCheck.add_stat ("var_app",num_var_app)
    |> QCheck.add_stat ("size", T.size)
  ) else a

let t_show t = CCFormat.sprintf "`@[%a@]`" T.ZF.pp t
let t_show2_p pp (t,u) = CCFormat.sprintf "(@[`@[%a@]`,@ ref: `@[%a@]`@])" pp t T.ZF.pp u
let t_show2 = t_show2_p T.ZF.pp

let check_whnf_closed =
  let gen = gen_ho in
  let prop t =
    let t' = Lambda.whnf t in
    not (T.DB.is_closed t) || T.DB.is_closed t'
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"whnf_preserve_closed"

let check_whnf_non_redex_preserved =
  let gen = gen_ho in
  let prop t = match T.as_app t with
    | f, _::_ when is_fun f -> QCheck.assume_fail()
    | _ ->
      let t' = Lambda.whnf t in
      T.equal t t'
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"whnf_non_redex_is_preserved"

let check_whnf_correct =
  (* reference implementation *)
  let rec whnf_naive (t:T.t): T.t = match T.view t with
    | T.Var _ | T.DB _ | T.Const _ -> t
    | T.AppBuiltin _ | T.Fun _ -> t
    | T.App (f, l) ->
      begin match T.view f, l with
        | _, [] -> assert false
        | T.Fun (_, bod), a :: tail ->
          (* evaluate β-redex, unshift *)
          let bod = T.DB.eval (DBEnv.push DBEnv.empty a) bod |> T.DB.unshift 1 in
          whnf_naive (T.app bod tail)
        | _ -> t
      end
  in
  let gen =
    QCheck.map_keep_input ~print:t_show2
      (fun t -> Lambda.whnf t, whnf_naive t)
      gen_ho
  in
  let prop (t,(t1,t2)) =
    if T.DB.is_closed t
    then T.equal t1 t2
    else QCheck.assume_fail()
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"whnf_correct"

let check_snf_closed =
  let gen = QCheck.map_keep_input ~print:t_show Lambda.snf gen_ho in
  let prop (t,t') =
    if not (T.DB.is_closed t) then QCheck.assume_fail()
    else(
      T.DB.is_closed t'
    )
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"snf_preserve_closed"

let check_snf_no_redex =
  let gen = QCheck.map_keep_input ~print:t_show Lambda.snf gen_ho in
  let prop (t,t') =
    if not (T.DB.is_closed t) then QCheck.assume_fail()
    else (
      T.Seq.subterms t'
      |> Iter.for_all
        (fun t -> match T.view t with
           | T.App (f, _) -> not (is_fun f)
           | _ -> true)
    )
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"snf_no_remaining_redex"

let check_eta_reduce_preserves_ty =
  let gen = gen_ho in
  let prop t =
    if not (T.DB.is_closed t) then QCheck.assume_fail()
    else (
      let t' = Lambda.eta_reduce t in
      ignore (T.rebuild_rec t');
      Type.equal (T.ty t) (T.ty t')
    )
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"eta_reduce_preserves_ty"

let check_eta_expand_preserves_ty =
  let gen = gen_ho in
  let prop t =
    if not (T.DB.is_closed t) then QCheck.assume_fail()
    else (
      let t' = Lambda.eta_expand t in
      ignore (T.rebuild_rec t');
      Type.equal (T.ty t) (T.ty t')
    )
  in
  QCheck.Test.make gen prop
    ~count:10_000 ~long_factor:10
    ~name:"eta_expand_preserves_ty"

(* reference implementation of SNF *)
module SNF = struct
  let eval_and_unshift ~by t =
    let rec aux depth t = match T.view t with
      | T.Var _ | T.Const _ -> t
      | T.DB i when i = depth -> T.DB.shift depth by (* shift [by] *)
      | T.DB i when i < depth -> t
      | T.DB i -> T.bvar ~ty:(T.ty t) (i-1) (* unshift *)
      | T.App (f, l) ->
        T.app (aux depth f) (List.map (aux depth) l)
      | T.AppBuiltin (b, l) ->
        T.app_builtin ~ty:(T.ty t) b (List.map (aux depth) l)
      | T.Fun (tyarg, body) ->
        T.fun_ tyarg (aux (depth+1) body)
    in
    aux 0 t

  let rec snf_naive (t:T.t): T.t = match T.view t with
    | T.Var _ | T.DB _ | T.Const _ -> t
    | T.Fun (ty_var, bod) ->
      let bod = snf_naive bod in
      T.fun_ ty_var bod
    | T.AppBuiltin (b,l) ->
      T.app_builtin ~ty:(T.ty t) b (List.map snf_naive l)
    | T.App (f, l) ->
      let f = snf_naive f in
      let l = List.map snf_naive l in
      begin match T.view f, l with
        | _, [] -> assert false
        | T.Fun (ty_var, bod), a :: tail ->
          assert (Type.equal (T.ty a) ty_var);
          (* evaluate β-redex: remove binder and replace DB0 by [a] *)
          let bod = eval_and_unshift ~by:a bod in
          snf_naive (T.app bod tail)
        | _ -> T.app f l
      end

end

let check_snf_correct =
  let prop t =
    let t1 = Lambda.snf t in
    let t2 = SNF.snf_naive t in
    if T.DB.is_closed t
    then (
      if T.equal t1 t2 then true
      else
        QCheck.Test.fail_reportf
          "@[t: `@[%a@]`@ snf: `@[%a@]`,@ ref: `@[%a@]@]@."
          T.ZF.pp t T.ZF.pp t1 T.ZF.pp t2
    ) else QCheck.assume_fail()
  in
  QCheck.Test.make gen_ho prop
    ~count:10_000 ~long_factor:10
    ~name:"snf_correct"

let check_snf_idempotent =
  let prop t =
    let t1 = Lambda.snf t in
    let t2 = Lambda.snf t1 in
    if T.equal t1 t2 then true
    else (
      QCheck.Test.fail_reportf
        "@[t: `@[%a@]`@ snf: `@[%a@]`,@ snf2: `@[%a@]@]@."
        T.ZF.pp t T.ZF.pp t1 T.ZF.pp t2
    )
  in
  QCheck.Test.make gen_ho prop
    ~count:10_000 ~long_factor:10
    ~name:"snf_idempotent"

let check_fun_fvars =
  let prop t =
    ignore (T.rebuild_rec t);
    if T.DB.is_closed t then (
      (* quantify on non-ty variables *)
      let vars =
        T.vars t
        |> T.VarSet.filter (fun v -> not (Type.is_tType (HVar.ty v)))
        |> T.VarSet.to_list
      in
      (* check that [(fun x. t[x]) x ==_β t] *)
      let t' = T.app (T.fun_of_fvars vars t) (List.map T.var vars) in
      let t1 =
        ignore (T.rebuild_rec t');
        SNF.snf_naive t'
      in
      let t2 = Lambda.snf t in
      ignore (T.rebuild_rec t);
      if T.equal t1 t2 then true
      else (
        QCheck.Test.fail_reportf
          "@[t: `@[%a@]`@ fun_var_app: `@[%a@]`,@ snf_naive: `@[%a@]`@ snf: `@[%a@]`@]@."
          T.ZF.pp t T.ZF.pp t' T.ZF.pp t1 T.ZF.pp t2
      )
    ) else QCheck.assume_fail()
  in
  QCheck.Test.make gen_ho prop
    ~count:10_000 ~long_factor:10
    ~name:"check_fun_fvars_correct"

let props =
  [ check_size_subterm
  ; check_replace_id
  ; check_ground_novar
  ; check_hash_mod_alpha
  ; check_min_max_vars
  ; check_whnf_closed
  ; check_whnf_correct
  ; check_whnf_non_redex_preserved
  ; check_snf_closed
  ; check_snf_no_redex
  ; check_snf_idempotent
  ; check_snf_correct
  ; check_fun_fvars
  ; check_eta_reduce_preserves_ty
  ; check_eta_expand_preserves_ty
  ]
