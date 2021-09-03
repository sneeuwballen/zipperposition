(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test unification *)

open Logtk
open Logtk_arbitrary
open Logtk_parsers

module Fmt = CCFormat
module T = Term
module S = Subst
module Q = QCheck

let t_test = Alcotest.testable T.ZF.pp T.equal

(** {2 Unit Tests} *)

let psterm, pstmt, pstmt_l, clear_scope, unif_ty, pstyctx =
  let tyctx = TypeInference.Ctx.create ~implicit_ty_args:false () in
  let pt s =
    try
      let t = Parse_zf.parse_term Lex_zf.token (Lexing.from_string s) in
      let t = TypeInference.infer_exn tyctx t in
      (* TypeInference.Ctx.exit_scope tyctx; *)
      t
    with e ->
      Format.eprintf "while parsing '%s'@." s; raise e
  and pst s =
    let t = Parse_zf.parse_statement Lex_zf.token (Lexing.from_string s) in
    let t = TypeInference.infer_statement_exn tyctx t in
    (* TypeInference.Ctx.exit_scope tyctx; *)
    t
  and pst_l s =
    let l = Parse_zf.parse_statement_list Lex_zf.token (Lexing.from_string s) in
    let l = TypeInference.infer_statements_exn
        ~on_var:`Default ~ctx:tyctx ~implicit_ty_args:false
        (Iter.of_list l) in
    (* TypeInference.Ctx.exit_scope tyctx; *)
    CCVector.to_list l
  and unif_ty t u =
    TypedSTerm.unify (TypedSTerm.ty_exn t) (TypedSTerm.ty_exn u)
  in
  pt, pst, pst_l, (fun () -> TypeInference.Ctx.exit_scope tyctx), unif_ty, tyctx

let parse_with_vars vars ~f = 
  TypeInference.Ctx.with_vars pstyctx (CCList.map 
      (fun (name, ty) -> 
        Var.of_string ~ty:(psterm ty) name
      ) 
    vars) ~f

(* prelude *)
let () =
  ignore (pstmt_l
    "val term : type.
     val a : term.
     val b : term.
     val c : term.
     val d : term.
     val e : term.
     val f : term -> term -> term.
     val g : term -> term.
     val gg : (term -> term) -> term.
     val h : term -> term.
     val ite : term -> term -> term -> term.
     val p : term -> term -> prop.
     val q : term -> prop.
     val r : term -> prop.
     val s : prop.
     val f_ho: (term -> term ) -> term.
     val f_ho2: (term -> term ) -> (term -> term) -> term.
     val g_ho: (term -> term -> term) -> term.
     val p_ho2: (term -> term) -> (term -> term) -> prop.
     val p_prop: (prop -> prop) -> prop.
     val a_poly : pi a. a -> a.
     val f_poly : pi a b. (a -> b) -> (a -> b) -> a.
     val sk : term -> term.
   ");
  ()

let tyctx = T.Conv.create()

(* parse Term.t *)
let pterm_ =
  fun ?ty s ->
    let t = psterm s in
    let ty = CCOpt.map psterm ty in
    CCOpt.iter (fun ty -> TypedSTerm.unify ty (TypedSTerm.ty_exn t)) ty;
    T.Conv.of_simple_term_exn tyctx t

let pterm ?ty s =
  try pterm_ ?ty s
  with e ->
    Format.printf "%s@." (Util.err_spf "pterm %s" s);
    raise e

(* parse two terms of same type *)
let pterm2 =
  fun ?(unif_types=true) ?ty s1 s2 ->
    let t1 = psterm s1 in
    let t2 = psterm s2 in
    if unif_types then (
      unif_ty t1 t2;
    );
    let ty = CCOpt.map psterm ty in
    CCOpt.iter (fun ty -> TypedSTerm.unify ty (TypedSTerm.ty_exn t1)) ty;
    CCOpt.iter (fun ty -> TypedSTerm.unify ty (TypedSTerm.ty_exn t2)) ty;
    T.Conv.of_simple_term_exn tyctx t1,
    T.Conv.of_simple_term_exn tyctx t2


module Action : sig
  type 'a t = private
    | Yield of {t: 'a ; ty: 'a option}
    | Eq of {t1: 'a; sc1:int; t2: 'a; sc2: int; ty: 'a option}
    | Eqs of {ts: ('a * 'a * 'a option) list}
    | Count of {count : int}

  val yield : string -> string t
  val eq : string -> int -> string -> int -> string t
  val eqs : (string * string * string option) list -> string t
  val count : int -> string t
  val set_with_ty : 'a -> 'a t -> 'a t
  val parse : string t -> T.t t
end = struct
  type 'a t =
    | Yield of {t: 'a ; ty: 'a option}
    | Eq of {t1: 'a; sc1:int; t2: 'a; sc2: int; ty: 'a option}
    | Eqs of {ts: ('a * 'a * 'a option) list}
    | Count of {count : int}

  let eq t1 sc1 t2 sc2 = Eq{t1;t2;sc1;sc2;ty=None}
  let eqs ts = Eqs{ts}
  let yield t = Yield{t; ty=None}
  let count count = Count{count}

  let set_with_ty ty = function
    | Yield r -> Yield {r with ty=Some ty}
    | Eq r -> Eq {r with ty=Some ty}
    | _ as a -> a

  (* parse action *)
  let parse : string t -> T.t t = function
    | Yield r ->
      let t = pterm ?ty:r.ty r.t in
      Yield {t; ty=None}
    | Eq r ->
      let t1, t2 = pterm2 ~unif_types:false ?ty:r.ty r.t1 r.t2 in
      Eq {t1; t2; sc1=r.sc1; sc2=r.sc2; ty=None}
    | Eqs {ts} -> 
      let ts = CCList.map (fun (t1, t2, ty) -> 
          let t1, t2 = pterm2 ~unif_types:false ?ty t1 t2 in
          t1, t2, None
        ) ts 
      in
      Eqs {ts}
    | Count c -> Count c

end

module Task : sig
  type t
  type op = Match of T.t * T.t | Unif of T.t * T.t | Parses of string
  val mk_parse_and_tycheck : string -> t
  val mk_unif : ?negated:bool -> ?unif_types:bool -> ?with_ty:string -> string -> string -> t
  val mk_match : ?negated:bool -> string -> string -> t
  val set_with_ty : string -> t -> t
  val set_unif_types : bool -> t -> t
  val is_negated : t -> bool
  val add_action : string Action.t -> t -> t
  val add_var_type : string -> string -> t -> t
  val actions : t -> (string Action.t) list
  val var_types : t -> (string * string) list
  val op : t -> op
  val pp : t CCFormat.printer
end = struct
  type op = Match of T.t * T.t | Unif of T.t * T.t | Parses of string

  type t = {
    view: view;
    negated: bool;
    actions: (string Action.t) list; 
    var_types: (string * string) list; 
  }

  and view =
    | TParses of {
        t: string;
      }
    | TUnif of {
        t1: string;
        t2: string;
        unif_types: bool;
        with_ty: string option;
      }
    | TMatch of {
        t1: string;
        t2: string;
      }

  let mk_ ?(negated=false) view : t =
    {negated; var_types=[]; actions=[]; view}

  let mk_parse_and_tycheck t : t = mk_ @@ TParses {t}

  let mk_unif ?negated ?(unif_types=true) ?with_ty t1 t2 : t =
    mk_ ?negated @@ TUnif {t1;t2;unif_types; with_ty; }

  let mk_match ?negated t1 t2 : t =
    mk_ ?negated @@ TMatch {t1;t2}

  let add_action a r = {r with actions = a :: r.actions }
  let actions r = r.actions
  let add_var_type v t r = {r with var_types = (v,t) :: r.var_types}
  let var_types r = r.var_types
  let set_with_ty ty = function
    | {view=TUnif u; _} as r -> {r with view=TUnif {u with with_ty=Some ty}}
    | _ -> assert false

  let set_unif_types b = function
    | {view=TUnif u;_} as r -> {r with view=TUnif {u with unif_types=b}}
    | _ -> assert false

  let is_negated r = r.negated

  let pp out = function
    | {view=TUnif {t1; t2; with_ty=None; _};_} -> Format.fprintf out "Unif (%s, %s)" t1 t2
    | {view=TUnif {t1; t2; with_ty=Some ty; _};_} -> Format.fprintf out "Unif (%s, %s) : %s" t1 t2 ty
    | {view=TMatch {t1; t2; _};_} -> Format.fprintf out "Match (%s, %s) " t1 t2
    | {view=TParses {t};_} -> Format.fprintf out "Parses (%s)" t

  let op r =
    try
      match r.view with
      | TUnif {with_ty; t1; t2; unif_types; _} ->
        let t1, t2 = pterm2 ~unif_types ?ty:with_ty t1 t2 in
        Unif (t1,t2)
      | TMatch {t1; t2; _} ->
        let t1, t2 = pterm2 ~unif_types:false t1 t2 in
        Match (t1,t2)
      | TParses {t} -> Parses t
    with e ->
      print_endline (Util.err_spf "cannot parse/typecheck op %a@." pp r);
      raise e
end

let equal_after_sub ~subst (t1,sc1)(t2,sc2) =
  let renaming = Subst.Renaming.create() in
  let t1 = Subst.FO.apply renaming subst (t1,sc1) |> Lambda.snf |> Lambda.eta_reduce in
  let t2 = Subst.FO.apply renaming subst (t2,sc2) |> Lambda.snf |> Lambda.eta_reduce in
  T.equal t1 t2

let check_variant ?(msg="") t u =
  if Unif.FO.are_variant t u then ()
  else (
    Alcotest.failf
      "@[<2>`%a`@ and `%a`@ should be variant@,%s@]@."
        T.ZF.pp t T.ZF.pp u msg
  )

let check_matches ?(msg="") t u =
  if Unif.FO.matches ~pattern:t u then ()
  else (
    Alcotest.failf
      "@[<2>`%a`@ should match@ `%a`%s@]@."
        T.ZF.pp t T.ZF.pp u msg
  )

let check_eq ?(msg="check eq") t1 t2 =
  Alcotest.(check t_test) msg t1 t2

let unifier2 t u =
    let subst = Unif.FO.unify_syn (t,0)(u,1) in
    let renaming = Subst.Renaming.create() in
    Subst.FO.apply renaming subst (t,0) |> Lambda.snf |> Lambda.eta_reduce,
    Subst.FO.apply renaming subst (u,1) |> Lambda.snf |> Lambda.eta_reduce,
    renaming,
    subst


let check_action t u t' renaming subst a = match a with
  | Action.Yield {t=res;_} -> check_variant t' res
  | Action.Eq {t1;t2;sc1;sc2;_} -> 
    let t1 = Subst.FO.apply renaming subst (t1,sc1) |> Lambda.snf |> Lambda.eta_reduce in
    let t2 = Subst.FO.apply renaming subst (t2,sc2) |> Lambda.snf |> Lambda.eta_reduce in
    let msg = Fmt.sprintf
          "(@[<h>unify `%a`@ `%a`@ :makes-eq @[`%a`[%d]@ and `%a`[%d]@]@])"
        T.ZF.pp t T.ZF.pp u T.ZF.pp t1 sc1 T.ZF.pp t2 sc2 in
      check_eq ~msg t1 t2
  | Action.Eqs _ -> assert false
  | Action.Count _ -> assert false

let check_unifiable ?(negated=false) t u actions : unit Alcotest.test_case =
  "check unifiable", `Quick, fun () ->
    try
      let t', u', renaming, subst = unifier2 t u in
      if negated then (
        Alcotest.failf
         "@[<2>`%a`[0]@ and `%a`[1]@ should not be unifiable@]@."
            T.ZF.pp t T.ZF.pp u
      )
      else (
        Alcotest.(check t_test) "check unified versions are equal" t' u';
        let msg = Fmt.sprintf "(@[check that unified version matches original term `%a`@ `%a`@])" T.ZF.pp t T.ZF.pp u in
        check_matches ~msg (t |> Lambda.snf |> Lambda.eta_reduce) t';
        check_matches ~msg (u |> Lambda.snf |> Lambda.eta_reduce) t';

        (* the important thing is that after unification, the terms are
          equal under substitution (t'=u'). . *)
        if not (equal_after_sub ~subst (t,0) (u,1)) then (
          Alcotest.failf
            "@[terms unify but are not equal under subst=%a,@ \
             t1=`%a`,@ t2=`%a`,@ t1σ=`%a`,@ t2σ=`%a`"
             Subst.pp subst T.ZF.pp t T.ZF.pp u T.ZF.pp t' T.ZF.pp u'
        );

        CCList.iter (check_action t u t' renaming subst) actions
      )
    with Unif.Fail ->
      if not negated then (
        Alcotest.failf "@[<2>`%a`[0]@ and `%a`[1]@ should be unifiable@]@."
            T.ZF.pp t T.ZF.pp u
      )

let check_matchable ?(negated=false) t u actions : unit Alcotest.test_case =
  "check matchable", `Quick, fun () ->
    try
      let subst = Unif.FO.matching ~pattern:(t,0) (u,1) in
      if negated then (
        Alcotest.failf
         "@[<2>`%a`[0]@ should not match pattern `%a`[1]@]@."
            T.ZF.pp u T.ZF.pp t 
      )
      else (
        let renaming = Subst.Renaming.none in
        let t' = Subst.FO.apply renaming subst (t,0) |> Lambda.snf |> Lambda.eta_reduce in
        let u' = Subst.FO.apply renaming subst (u,0) |> Lambda.snf |> Lambda.eta_reduce in
        check_eq ~msg:"matching subst is really a matching subst" t' u;
        check_eq ~msg:"matching subst does not touch term" u' u;
        CCList.iter (check_action t u t' renaming subst) actions
      )
    with Unif.Fail ->
      if not negated then (
        Alcotest.failf 
         "@[<2>`%a`[0]@ should match pattern `%a`[1]@]@."
            T.ZF.pp t T.ZF.pp u
      )

let check_parsable t : unit Alcotest.test_case =
  "check parsable", `Quick, fun () ->
    try
      let _t = pterm t in
      ()
    with e ->
      Alcotest.failf "failed to parse/typecheck '%s':@. %s" t
        (Printexc.to_string e)

let suite_unif1 : unit Alcotest.test_case list =
  let (=?=) a b = Task.mk_unif a b in (* unif pair *)
  let (<?>) a b = Task.mk_unif ~negated:true a b in (* unif pair *)
  (* let (=?=>) a b = Task.mk_match a b in matching pair *)
  let (>->) a b = Task.set_with_ty b a in (* specify return type *)
  let (>?->) a b = Action.set_with_ty b a in (* specify return type *)
  let (>>>) a b = Task.add_action b a in (* specify return type *)

  let mk_tests (task) =
    let op, actions =
      parse_with_vars (Task.var_types task)
       ~f:(fun () ->
        let op = Task.op task in
        let parsed_actions = List.map Action.parse (Task.actions task) in
        op, parsed_actions
        )
    in
    clear_scope();

    match op with
    | Task.Unif (t,u) when Task.is_negated task ->
      check_unifiable ~negated:true t u actions
    | Task.Unif (t, u) ->
      check_unifiable t u actions
    | Task.Match (t, u) ->
      check_matchable ~negated:(Task.is_negated task) t u actions
    | Task.Parses t ->
      check_parsable t
  in
  CCList.map mk_tests
    [ "f X b" =?= "f a Y" 
      >>> Action.yield "f a b"
      >>> Action.eq "X" 0 "a" 0
      >>> Action.eq "Y" 1 "b" 0;

      (* Test deleted since this kind of unification was buggy! *)
      (* "F a" =?= "f a (g (g a))"
      >>> Action.yield "f a (g (g a))"
      >>> Action.eq "F" 0 "fun (x:term). f x (g (g x))" 0; *)

      ("fun (x y:term). F x" =?= "fun x y. G x y") >-> "term -> term -> term"
      >>> (Action.yield "fun x y. H x" >?-> "term -> term -> term")
      >>> (Action.eq "G" 1 "fun x y. F x" 0 >?-> "term -> term -> term");

      ("fun (x y z:term). F x" =?= "fun x y z. G x y z") >-> "term -> term -> term -> term"
      >>> (Action.yield "fun x y z. H x" >?-> "term -> term -> term -> term")
      >>> (Action.eq "G" 1 "fun x y z. F x" 0 >?-> "term -> term -> term -> term");
      
      ("X" =?= "(fun Y. X1) (fun (x y:term). c)") >-> "term"
      >>> (Action.yield "Y" >?-> "term");

      ("p_ho2 (fun a. F1 a) (fun a. F2 a)" =?= "p_ho2 (fun a. G a) (fun a. G a)")
      >>> Action.yield "p_ho2 G G"
      >>> (Action.eq "F1" 0 "G" 1 >?-> "term -> term")
      >>> (Action.eq "F2" 0 "G" 1 >?-> "term -> term");
(* 
      (" fun x y. f (X x) (g (X y)) " =?= "fun x y. f (Y y) (g a)")
      >>> Action.yield "fun x y. f (g a) (g a)"; *)

      (* (" fun x. g (X x)" <?> "fun x. X x"); *)

      Task.mk_parse_and_tycheck
        "with (F0:term->term). F0 (f_ho2 (fun (Y0:term). (F0 Y0)) (fun (Y0:term). X0))";

      ("with (X0:term) (F0:term->term). \
        F0 (f_ho2 (fun (Y0:term). (F0 Y0)) (fun (Y0:term). X0))" =?=
       "with (F56769:term->term) (X56769:term). F56769 X56769");

      ("with (F56771:term ->term). \
        F56771 (f_ho2 (fun (Y0:term). (F56771 Y0)) (fun (Y0:term). X56772))" =?=
       "with (F56769:term->term). F56769 X56770");

      ("p_ho2 (fun Y0. d) (fun Y0. F1 Y0)" =?=
       "p_ho2 (fun Y0. d) (fun Y0. (f_ho2 (fun Y1. Y1) (fun Y2. X)))");
      ("f (f a b) X" =?= "F1 (f_poly A1 A2 F1 F2)") |> Task.set_unif_types false
      >>> Action.eq "f (f a b)" 0 "F1" 1
      >>> Action.yield "f (f a b) (f_poly _ _ (f (f a b)) F_renamed)"
        (* FIXME
         >>> Action.eq "X" 1 "f_poly _ _ (f (f a b)) F2" 0;
           *);

      ( "F (g_ho F)" <?> "a_poly A") |> Task.set_unif_types false;
      ( "(fun (x:term). x)" <?> "(fun (x:term). Y)");
      ( "g_ho (fun (x:term). g)" <?> "g_ho (fun (x:term) (y:term). Y x)");
      ( "g_ho (fun (x:term). g)" =?= "g_ho (fun (x:term) (y:term). Y y)")
       >>> Action.eq "fun (x:term). g x" 0 "Y" 1;

      (*( "(f_ho (X a))" =?=> "(f_ho (fun (x:term). f_ho (fun (y:term). g (Y y))))" ), [
        Action.eq "X" 0 "fun (z:term). fun (x:term). f_ho (fun (y:term). g (Y y))" 1;
      ],["X","term->term->term"];*)
      (* ( "F a" =?=> "fun (x : term). f_ho2 (fun (y:term). y) (fun (y:term). y)") |> Task.add_var_type "F" "term -> term -> term" *)
    ]


let reg_matching1 = "regression matching", `Quick, fun () ->
  let terms =
    [ pterm "p_ho2 (fun a. F a) (fun a. F a)"
    , pterm "p_ho2 (fun a. G a) (fun a. H a)"
    ; pterm "f_ho2 (fun (x:term). Y) g"
    , pterm "f_ho2 (fun (x:term). x) g"
    ]
  in
  terms |> List.iter (fun (t1,t2) ->
    try
      let _ = Unif.FO.matching ~pattern:(t1,0) (t2,1) in
      Alcotest.failf
        "@[<hv>`%a`@ and `%a@ should not match@]" T.ZF.pp t1 T.ZF.pp t2
    with Unif.Fail -> ();
  )

let reg_unif_makes_eq = Printf.sprintf "regression %d" __LINE__, `Quick, fun () ->
    let t1 = pterm
        {|with (Foo:term -> term).
          Foo
          (f_ho2
            (fun (Y0:term). a)
            (fun (Y0:term).
              (f_ho2 (fun (Y1:term). (Foo Y1)) (fun (Y1:term). X))))|}
    and t2 = pterm "with (Bar:term-> term). (Bar X)" in
    let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
    if not (equal_after_sub ~subst (t1,0) (t2,1)) then (
      Alcotest.failf "not equal";
    );
    ()

let reg_stack_overflow = "reg_stack_overflow1", `Quick, fun () ->
  let t = pterm
      {|with (F0 F1 G:term ->term) (P3:term->prop->term).
      F1 (P3 (f (F0 c) (F2 e c))
      (p_ho2 (fun (Y0:term). d) (fun (Y0:term). (G Y0))))|}
  in
  if not (equal_after_sub ~subst:Subst.empty (t,0)(t,0)) then (
    Alcotest.failf "not eq";
  );
  ()

let suite_unif2 = [
  reg_matching1;
  reg_stack_overflow;
  reg_unif_makes_eq;
]

let suite = suite_unif1 @ suite_unif2


(** {2 Properties} *)

let (==>) = QCheck.(==>)

let gen_fo = ArTerm.default
let gen_t = ArTerm.default_ho

let check_unify_gives_unifier =
  let gen = QCheck.(pair gen_t gen_t) in
  let name = "unify_gives_unifier" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t1' = S.FO.apply renaming subst (t1,0) |> Lambda.snf |> Lambda.eta_reduce in
      let t2' = S.FO.apply renaming subst (t2,1) |> Lambda.snf |> Lambda.eta_reduce in
      if T.equal t1' t2' then true
      else QCheck.Test.fail_reportf
          "subst=%a,@ t1'=`%a`,@ t2'=`%a`" Subst.pp subst T.ZF.pp t1' T.ZF.pp t2'
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:15_000 ~name gen prop

(* σ=mgu(t1,t2) means t1,t2 both match t1σ (== t2σ). In practice, with
   our matching, it only works for FO terms *)
let check_unifier_matches =
  let gen = QCheck.(pair gen_fo gen_fo) in
  let name = "unifier_matches_unified_terms" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t1' = S.FO.apply renaming subst (t1,0) |> Lambda.snf |> Lambda.eta_reduce in
      let t2' = S.FO.apply renaming subst (t2,1) |> Lambda.snf |> Lambda.eta_reduce in
      if Unif.FO.matches ~pattern:t1 t1' &&
         Unif.FO.matches ~pattern:t2 t2'
      then true
      else ( QCheck.Test.fail_reportf
          "(@[<hv2>subst=%a,@ t1'=`%a`,@ t2'=`%a`@])"
          Subst.pp subst T.ZF.pp t1' T.ZF.pp t2'
      )
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:15_000 ~name gen prop

let check_unify_makes_eq  =
  let gen = QCheck.(pair gen_t gen_t) in
  let name = "unify_makes_eq" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.unify_syn (t1,0) (t2,1) in
      (* if Unif.FO.equal ~subst (t1,0) (t2,1) then true*)
      if equal_after_sub ~subst (t1,0) (t2,1) then true
      else (
        let renaming = Subst.Renaming.create() in
        QCheck.Test.fail_reportf
          "subst=%a,@ t1=`%a`,@ t2=`%a`,@ t1σ=`%a`,@ t2σ=`%a`"
          Subst.pp subst T.ZF.pp t1 T.ZF.pp t2
          T.ZF.pp (Subst.FO.apply renaming subst (t1,0))
          T.ZF.pp (Subst.FO.apply renaming subst (t2,1))
      )
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:15_000 ~name gen prop

let check_equal =
  let gen = gen_t in
  let name = "unif_term_self_equal" in
  let prop t =
    equal_after_sub ~subst:Subst.empty (t,0) (t,0)
  in
  QCheck.Test.make ~long_factor:20 ~count:2_000 ~name gen prop

let check_variant =
  let gen = gen_fo in
  let name = "unif_term_self_variant" in
  let prop t =
    let renaming = S.Renaming.create () in
    let t' = S.FO.apply renaming S.empty (t,0) in
    Unif.FO.are_variant t t'
  in
  QCheck.Test.make ~long_factor:20 ~count:2_000 ~name gen prop

let check_variant2 =
  let gen = gen_t in
  let name = "unif_term_variant_sound" in
  let prop (t0,t1) =
    try
      let subst = Unif.FO.variant (t0,0)(t1,1) in
      (* check they are really variants *)
      let renaming = Subst.Renaming.create() in
      let t0' = Subst.FO.apply renaming subst (t0,0) |> Lambda.snf |> Lambda.eta_reduce in
      let t1' = Subst.FO.apply renaming subst (t1,1) |> Lambda.snf |> Lambda.eta_reduce in
      T.equal t0' t1'
    with Unif.Fail -> QCheck.assume_fail ()
  in
  QCheck.Test.make ~long_factor:20 ~name (Q.pair gen gen) prop

let check_variant_sym =
  let gen = gen_t in
  let name = "unif_term_variant_sym" in
  let prop (t0,t1) =
    Unif.FO.are_variant t0 t1 = Unif.FO.are_variant t1 t0
  in
  QCheck.Test.make ~long_factor:20 ~name (Q.pair gen gen) prop

let check_matching =
  let gen = QCheck.pair gen_t gen_t in
  let name = "unif_matching_gives_matcher" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.matching ~pattern:(t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t1' = S.FO.apply renaming subst (t1,0) |> Lambda.snf |> Lambda.eta_reduce in
      let t2' = S.FO.apply renaming subst (t2,1) |> Lambda.snf |> Lambda.eta_reduce in
      if T.equal t1' t2'
      then true
      else QCheck.Test.fail_reportf "@[<v>subst=%a,@ t1'=`%a`,@ t2'=`%a`@]"
          Subst.pp subst T.ZF.pp t1' T.ZF.pp t2'
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:1000 ~name gen prop

let check_matching_variant =
  let gen = QCheck.pair gen_t gen_fo in
  let name = "unif_matching_preserves_rhs" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.matching ~pattern:(t1,0) (t2,1) in
      let renaming = S.Renaming.create () in
      let t2' = S.FO.apply renaming subst (t2,1) |> Lambda.snf |> Lambda.eta_reduce in
      if Unif.FO.are_variant t2 t2'
      then true
      else QCheck.Test.fail_reportf "@[<v>subst=%a,@ t2'=`%a`@]"
          Subst.pp subst T.ZF.pp t2'
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:1000 ~name gen prop

let check_matching_variant_same_scope =
  let gen = QCheck.pair gen_t gen_fo in
  let name = "unif_matching_same_scope_preserves_rhs" in
  let prop (t1, t2) =
    try
      let subst = Unif.FO.matching_same_scope ~scope:0 ~pattern:t1 t2 in
      let t2' = S.FO.apply Subst.Renaming.none subst (t2,0) |> Lambda.snf |> Lambda.eta_reduce in
      if Unif.FO.are_variant t2 t2'
      then true
      else QCheck.Test.fail_reportf "@[<v>subst=%a,@ t2'=`%a`@]"
          Subst.pp subst T.ZF.pp t2'
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:1000 ~name gen prop

let check_variant_bidir_match =
  let gen = gen_t in
  let name = "unif_term_variant_bidir_match" in
  let prop (t0,t1) =
    if Unif.FO.are_variant t0 t1
    then Unif.FO.matches ~pattern:t0 t1 && Unif.FO.matches ~pattern:t1 t0
    else QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~name (Q.pair gen gen) prop

let check_lits_variant_bidir_match =
  let gen = ArLiteral.clause in
  let name = "unif_lits_variant_bidir_match" in
  let prop (lits0,lits1) =
    if Literals.are_variant lits0 lits1
    then Literals.matches lits0 lits1 && Literals.matches lits1 lits0
    else QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:5_000 ~name (Q.pair gen gen) prop

let check_ho_unify_gives_unifiers =
  let gen = QCheck.(pair gen_t gen_t) in
  let name = "ho_unify_gives_unifiers" in
  let prop (t1, t2) =
    let offset =
      Iter.doubleton t1 t2
      |> Iter.flat_map T.Seq.vars
      |> T.Seq.max_var |> succ
    in
    (* only keep proper solutions *)
    let l =
      HO_unif.unif_pairs ~fuel:20 ~offset ([[],t1,t2],0)
      |> List.filter
        (fun (pairs,us,_,_) -> pairs=[] && not (Unif_subst.has_constr us))
    in
    if l=[] then QCheck.assume_fail()
    else (
      List.iter
        (fun (_,us,_,_) ->
           let subst = Unif_subst.subst us in
           let renaming = Subst.Renaming.create() in
           let u1 = Subst.FO.apply renaming subst (t1,0) |> Lambda.snf |> Lambda.eta_reduce in
           let u2 = Subst.FO.apply renaming subst (t2,0) |> Lambda.snf |> Lambda.eta_reduce in
           if not (T.equal u1 u2) then (
             QCheck.Test.fail_reportf
               "(@[<hv2>bad solution@ t1'=`%a`@ t2'=`%a`@ :subst %a@])"
               T.ZF.pp u1 T.ZF.pp u2 Subst.pp subst
           ))
        l;
      true
    )
  in
  QCheck.Test.make ~long_factor:20 ~count:8_000 ~name gen prop

(* TODO: generate random Literals.t, then check [variant a b <=> (matches a b && matches b a)] *)

let props =
  [ check_unify_gives_unifier;
    check_unify_makes_eq;
    check_unifier_matches;
    check_equal;
    check_variant;
    check_variant2;
    check_variant_sym;
    check_variant_bidir_match;
    check_lits_variant_bidir_match;
    check_matching;
    check_matching_variant;
    check_matching_variant_same_scope;
    (* 
    REMOVED BECAUSE IT IS NOT USED OTHERWISE!
    check_ho_unify_gives_unifiers; *)
  ]
