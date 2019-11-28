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
    let t = Parse_zf.parse_term Lex_zf.token (Lexing.from_string s) in
    let t = TypeInference.infer_exn tyctx t in
    (* TypeInference.Ctx.exit_scope tyctx; *)
    t
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
     val p_ho2: (term -> term ) -> (term -> term) -> prop.
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
  type op = Match | Unif
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
  val parse : t -> T.t * T.t
end = struct
  type op = Match | Unif

  type t =
    | TUnif of {
        t1: string;
        t2: string;
        unif_types: bool;
        with_ty: string option;
        negated: bool;
        actions: (string Action.t) list; 
        var_types: (string * string) list;
      }
    | TMatch of {
        t1: string;
        t2: string;
        negated: bool;
        actions: (string Action.t) list; 
        var_types: (string * string) list; 
      }

  let mk_unif ?(negated=false) ?(unif_types=true) ?with_ty t1 t2 : t =
    TUnif {t1;t2;unif_types; with_ty; negated; actions=[]; var_types=[]}

  let mk_match ?(negated=false) t1 t2 : t =
    TMatch {t1;t2; negated; actions=[]; var_types=[]}

  let add_action a = function
    | TUnif ({actions; _} as r) -> TUnif {r with actions=a :: actions}
    | TMatch ({actions; _} as r) -> TMatch {r with actions=a :: actions}

  let actions = function
    | TUnif {actions; _} -> actions
    | TMatch {actions; _}-> actions

  let add_var_type v t = function
    | TUnif ({var_types; _} as r) -> TUnif {r with var_types=(v, t) :: var_types}
    | TMatch ({var_types; _} as r) -> TMatch {r with var_types=(v, t) :: var_types}

  let var_types = function
    | TUnif {var_types; _} -> var_types
    | TMatch {var_types; _}-> var_types

  let set_with_ty ty = function
    | TUnif r -> TUnif {r with with_ty=Some ty}
    | TMatch _ -> assert false

  let set_unif_types b = function
    | TUnif r -> TUnif {r with unif_types=b}
    | TMatch _ -> assert false

  let is_negated = function
    | TUnif {negated; _} -> negated
    | TMatch {negated; _} -> negated

  let op = function
    | TUnif _ -> Unif
    | TMatch _ -> Match

  let pp out = function
    | TUnif {t1; t2; with_ty=None; _} -> Format.fprintf out "Unif (%s, %s)" t1 t2
    | TUnif {t1; t2; with_ty=Some ty; _} -> Format.fprintf out "Unif (%s, %s) : %s" t1 t2 ty
    | TMatch {t1; t2; _} -> Format.fprintf out "Match (%s, %s) " t1 t2

  let parse_ = function
    | TUnif {with_ty; t1; t2; unif_types; _} -> pterm2 ~unif_types ?ty:with_ty t1 t2
    | TMatch {t1; t2; _} -> pterm2 ~unif_types:false t1 t2

  let parse p =
    try parse_ p
    with e ->
      print_endline (Util.err_spf "cannot parse/typecheck pair %a@." pp p);
      raise e
end

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

let suite_unif1 : unit Alcotest.test_case list =
  let (=?=) a b = Task.mk_unif a b in (* unif pair *)
  let (<?>) a b = Task.mk_unif ~negated:true a b in (* unif pair *)
  (* let (=?=>) a b = Task.mk_match a b in matching pair *)
  let (>->) a b = Task.set_with_ty b a in (* specify return type *)
  let (>?->) a b = Action.set_with_ty b a in (* specify return type *)
  let (>>>) a b = Task.add_action b a in (* specify return type *)

  let mk_tests (task) =
    let parsed_pair = ref None in
    let parsed_actions = ref None in
    parse_with_vars (Task.var_types task)
     ~f:(fun () ->
      parsed_pair := Some (Task.parse task);
      parsed_actions := Some (List.map Action.parse (Task.actions task));
    );
    let t, u = CCOpt.get_exn !parsed_pair in
    let actions = CCOpt.get_exn !parsed_actions in
    clear_scope();

    match Task.op task with
    | Task.Unif when Task.is_negated task ->
      check_unifiable ~negated:true t u actions
    | Task.Unif ->
      check_unifiable t u actions
    | Task.Match ->
      check_matchable ~negated:(Task.is_negated task) t u actions
  in
  CCList.map mk_tests
    [ "f X b" =?= "f a Y" 
      >>> Action.yield "f a b"
      >>> Action.eq "X" 0 "a" 0
      >>> Action.eq "Y" 1 "b" 0;

      "F a" =?= "f a (g (g a))"
      >>> Action.yield "f a (g (g a))"
      >>> Action.eq "F" 0 "fun (x:term). f x (g (g x))" 0;

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

let jp_check_count t u count : unit Alcotest.test_case =
  "JP-unif check count", `Quick, fun () ->
    let t' = JP_unif.unify_scoped_nonterminating t u in
    if count = OSeq.length t' then ()
    else
      Alcotest.failf
        "@[<hv>Found %d unifiers instead of %d@]" (OSeq.length t') count

let jp_check_nonunifiable ?(msg="") t u : unit Alcotest.test_case =
  "JP-unif check nonunifiable", `Quick, fun () ->
  if OSeq.for_all CCOpt.is_none (OSeq.take 20 (JP_unif.unify_scoped t u)) then ()
  else (
    Alcotest.failf
      "@[<2>`%a`@ should not unify with @ `%a`%s@]@."
       (Scoped.pp T.ZF.pp) t (Scoped.pp T.ZF.pp) u msg
  )

let jp_check_unifier t u ~res =
  "JP-unif check unifier", `Quick, fun () ->
  let is_res subst = match subst with
    | None -> false
    | Some s ->
      let found = Lambda.snf (JP_unif.S.apply s t) in
      Unif.FO.are_variant res found 
  in
  let unifiers = JP_unif.unify_scoped t u in
  if OSeq.exists is_res unifiers then () 
  else (
    Alcotest.failf
      "@[<2>`%a`@ and `%a`@ should unify as @ `%a`@]@."
        (Scoped.pp T.ZF.pp) t (Scoped.pp T.ZF.pp) u T.ZF.pp res
  )

let jp_check_eqs t u ts =
  "JP-unif check equalities", `Quick, fun () ->
  let is_res subst = match subst with
    | None -> false
    | Some s ->
      CCList.for_all (fun (t1,t2,_) -> 
        let t1 = Lambda.eta_reduce (Lambda.snf (JP_unif.S.apply s t1)) in
        let t2 = Lambda.eta_reduce (Lambda.snf (JP_unif.S.apply s t2)) in
        Unif.FO.are_variant t1 t2 
      ) ts
  in
  let unifiers = JP_unif.unify_scoped t u in
  if OSeq.exists is_res unifiers then () 
  else (
    Alcotest.failf
      "@[<2>`%a`@ and `%a`@ should unify this list: @ `%a`@]@."
        (Scoped.pp T.ZF.pp) t (Scoped.pp T.ZF.pp) u (CCList.pp (CCPair.pp T.ZF.pp T.ZF.pp)) (CCList.map (fun ((t1, _), (t2, _), _) -> t1, t2) ts)
  )

let suite_jp_unif : unit Alcotest.test_case list =
  Util.set_debug 1; Printexc.record_backtrace true;
  let (=?=) a b = Task.mk_unif a b in (* unif pair *)
  let (<?>) a b = Task.mk_unif ~negated:true a b in (* unif pair *)
  let (>->) a b = Task.set_with_ty b a in (* specify return type *)
  let (>?->) a b = Action.set_with_ty b a in (* specify return type *)
  let (>>>) a b = Task.add_action b a in (* specify return type *)
  let mk_tests (pair) =
    let parsed_pair = ref None in
    let parsed_actions = ref [] in
    parse_with_vars (Task.var_types pair) ~f:(fun () ->
       parsed_pair := Some (Task.parse pair);
       parsed_actions := List.map Action.parse (Task.actions pair)
    );
    let t, u = CCOpt.get_exn !parsed_pair in
    let t, u = (t,0), (u,0) in
    let actions = !parsed_actions in
    clear_scope();
    if Task.is_negated pair then
      [jp_check_nonunifiable t u]
    else 
      actions |> CCList.flat_map  (fun action -> match action with
        | Action.Count {count} -> [jp_check_count t u count]
        | Action.Yield {t=res;_} -> [jp_check_unifier t u ~res]
        | Action.Eqs {ts} -> [jp_check_eqs t u (List.map (fun (t1,t2,ty) -> (t1,0),(t2,0),ty) ts)]
        | _ -> assert false
      ) 
  in

  CCList.flat_map mk_tests
    [ 
      (* 
        Because of the iteration rule, which was not complete before,
        now we have infinitely many unifiers!
      "X a" =?= "Y b" >-> "term"
      >>> Action.count 17; *)

      "X a" <?> "g (X a)" >-> "term";

      "(g (X a))" =?= "(X (g a))" >-> "term"
      >>> (Action.yield "g (g (g (g a)))" >?-> "term");

      (* Example 3 in the Jensen-Pietrzykowski paper *)
      (* "Z Y X" =?= "Z (fun u. u) (g a)" >-> "term"
      >>> Action.eqs [
            "X", "a", None; 
            "Y", "g", None; 
            "Z", "fun (z : term -> term). fun (x : term). X0 (z x)", Some "(term -> term) -> term -> term"
        ]
      |> Task.add_var_type "X" "term"
      |> Task.add_var_type "Y" "term -> term"; *)

      (* Iterate on head of disagreement pair *)
      "X g" =?= "g a" >-> "term"
      >>> Action.eqs [
            "X", "fun z. z a", Some "(term -> term) -> term"
          ];

      (* Iterate with non-empty w tuple *)
      "X (fun z. f z a)" =?= "X (fun z. f a z)" >-> "term"
      >>> Action.eqs ["X", "fun (z : term -> term). Z (fun (w : alpha). z a)", Some "(term -> term) -> term"];

      "f X Y" =?= "f Y X" >-> "term"
      |> Task.add_var_type "X" "term"
      |> Task.add_var_type "Y" "term"
      (* >>> Action.yield "f W Z" *)
      >>> Action.count 1;


      "X a b" =?= "f b Y"
      |> Task.add_var_type "X" "term -> term -> term"
      |> Task.add_var_type "Y" "term"
      >>> Action.eqs [
         "X", "fun (x : term) (y : term). f y Y ", None;
      ]
      >>> Action.eqs [
         "X", "fun (x : term) (y : term). f y y ", None;
         "Y", "b", None;
      ];

      (* Polymorphism *)

      "fun (x : alpha). x" =?= "fun (x : term). x" |> Task.set_unif_types false
      >>> Action.count 1;

      "f_ho2 (a_poly term) (a_poly term)" =?= "f_ho2 X X" |> Task.set_unif_types false
      >>> Action.count 1;

      (* Example from "Higher-Order Unification, Polymorphism, and Subsorts (Extended Abstract)" by T. Nipkow *)
      "(fun (y : term). y) (X Y)" =?= "a" >-> "term" |> Task.set_unif_types false
      >>> Action.eqs [
              "X", "fun (z : term -> term). z (z (z a))", None;
              "Y", "fun (z : term). z", None
          ]
      >>> Action.eqs[ 
          (* The example actually states the unifier "z (X z) (Y z)", but the following equally general unifier comes out of our procedure: *)
              "X", "fun (z : alpha -> gamma -> term). z (X000 z) (Y000 z (z (X000 z)))", None;
              "Y", "fun (x : alpha) (y : gamma). a", None 
          ]
        |> Task.add_var_type "X" "beta -> term"
        |> Task.add_var_type "Y" "beta";


      (* More *)

      "fun (x : term). x" <?> "fun (x : term). X" |> Task.set_unif_types false
      |> Task.add_var_type "X" "term";

      (* 
        these tests go crazy with the iteration
      "X a" =?= "sk a" 
      >>> Action.count 1
      |> Task.add_var_type "X" "term -> term"; *)

      (* "X a" =?= "g a" 
      >>> Action.count 2
      |> Task.add_var_type "X" "term -> term"; *)
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

(** Jensen-Pietrzykowski auxiliary functions tests *)
let test_jp_unif_aux = "JP unification", `Quick, fun () ->
  Util.set_debug 1; Printexc.record_backtrace true;

  (** Find disagreement tests *)
  
  let test_disagree =
    Alcotest.testable
      Fmt.Dump.(option (pair (pair T.ZF.pp T.ZF.pp) (list @@ pair HVar.pp int)))
      CCEqual.(option @@ pair (pair T.equal T.equal) (list (pair (HVar.equal Type.equal) int)))
  in

  Alcotest.check test_disagree
    "jpunif1"
    (JP_unif.find_disagreement (pterm "g (g a)") (pterm "g (h a)")) 
    (Some ((pterm "g a", pterm "h a"), []));

  Alcotest.check test_disagree
    "jpunif2"
    (JP_unif.find_disagreement (pterm "g (g a)") (pterm "g (g b)")) 
    (Some ((pterm "a", pterm "b"), []));
  
  Alcotest.check test_disagree
    "jpunif3"
    (JP_unif.find_disagreement (pterm "f_ho2 (fun (x:term). x)")
       (pterm "f_ho2 (fun (x:term). a)")) 
    (Some ((T.bvar ~ty:(Type.Conv.of_simple_term_exn (Type.Conv.create ()) (psterm "term")) 0,
            pterm "a"), []));

  (** Rule tests *)

  let test_rule =
    Alcotest.testable Fmt.Dump.(list T.ZF.pp) CCEqual.(list T.equal)
  in

  let scope = 0 in

  let term = pterm ~ty:"term" "X a b" in
  let result = 
    JP_unif.project_onesided ~scope ~counter:(ref 1000) term 
    |> OSeq.map (fun subst -> Lambda.snf (JP_unif.S.apply subst (term,scope)))
    |> OSeq.to_list in
  let expected = [pterm "a"; pterm "b"] in
  Alcotest.check test_rule "jp-unif-rule" expected result;

  clear_scope ();

  let term1 = pterm ~ty:"term" "X a b" in
  let term2 = pterm "f c d" in
  let results = 
    JP_unif.imitate ~scope ~counter:(ref 1000) term1 term2 []
    |> OSeq.map (fun subst -> Lambda.snf (JP_unif.S.apply subst (term1,scope)))
    |> OSeq.to_array in
  Alcotest.(check int) "len_results" 1 (Array.length results);
  check_variant (results.(0)) (pterm ~ty:"term" "f (X a b) (Y a b)");

  clear_scope ();

  let term1 = pterm ~ty:"term" "X a b" in
  let term2 = pterm ~ty:"term" "Y c d" in
  let substs = JP_unif.identify ~scope ~counter:(ref 1000) term1 term2 [] in
  Alcotest.(check int) "len_subst" 1 (OSeq.length substs);
  let subst = OSeq.nth 0 substs in
  let result1 = Lambda.snf (JP_unif.S.apply subst (term1,scope)) in
  let result2 = Lambda.snf (JP_unif.S.apply subst (term2,scope)) in
  check_variant (result1) 
    (T.app (pterm ~ty:"term -> term -> term -> term -> term" "X1") 
      [pterm "a"; pterm "b";pterm ~ty:"term" "Y a b"; pterm ~ty:"term" "Z a b"]);
  check_variant (result2) 
    (T.app (pterm ~ty:"term -> term -> term -> term -> term" "X1") 
      [pterm ~ty:"term" "Y c d"; pterm ~ty:"term" "Z c d"; pterm "c"; pterm "d"]);
  ()

let suite_unif2 = [ reg_matching1; test_jp_unif_aux ]

let suite = suite_unif1 @ suite_unif2 @ suite_jp_unif


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
      if Unif.FO.equal ~subst (t1,0) (t2,1) then true
      else QCheck.Test.fail_reportf
          "subst=%a,@ t1=`%a`,@ t2=`%a`" Subst.pp subst T.ZF.pp t1 T.ZF.pp t2
    with Unif.Fail -> QCheck.assume_fail()
  in
  QCheck.Test.make ~long_factor:20 ~count:15_000 ~name gen prop

let check_equal =
  let gen = gen_t in
  let name = "unif_term_self_equal" in
  let prop t =
    Unif.FO.equal ~subst:Subst.empty (t,0) (t,0)
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
