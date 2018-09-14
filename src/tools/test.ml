
open Logtk
open Logtk_parsers

module T = Term
module US = Unif_subst

let psterm, pstmt, pstmt_l =
    let tyctx = TypeInference.Ctx.create ~implicit_ty_args:true () in
    let pt s =
      let t = Parse_zf.parse_term Lex_zf.token (Lexing.from_string s) in
      let t = TypeInference.infer_exn tyctx t in
      TypeInference.Ctx.exit_scope tyctx;
      t
    and pst s =
      let t = Parse_zf.parse_statement Lex_zf.token (Lexing.from_string s) in
      let t = TypeInference.infer_statement_exn tyctx t in
      TypeInference.Ctx.exit_scope tyctx;
      t
    and pst_l s =
      let l = Parse_zf.parse_statement_list Lex_zf.token (Lexing.from_string s) in
      let l = TypeInference.infer_statements_exn ~implicit_ty_args:true ~on_var:`Default ~ctx:tyctx (Sequence.of_list l) in
      TypeInference.Ctx.exit_scope tyctx;
      CCVector.to_list l
    in
    pt, pst, pst_l
;;
 (* prelude *)
ignore (pstmt_l
  "val term : type.
   val a : term.
   val b : term.
   val c : term.
   val d : term.
   val e : term.
   val f : term -> term -> term.
   val g : term -> term.
   val h : term -> term.
   val ite : term -> term -> term -> term.
   val p : term -> term -> prop.
   val q : term -> prop.
   val r : term -> prop.
   val s : prop.
   val f_ho2: (term -> term ) -> (term -> term) -> term.
   val p_ho2: (term -> term ) -> (term -> term) -> prop.
   ");;
 (* parse Term.t *)
let pterm =
  let ctx = T.Conv.create() in
  fun s ->
    psterm s |> T.Conv.of_simple_term_exn ctx
;;

let options = Options.make()


let () = 
  CCFormat.set_color_default true;
  Arg.parse (Arg.align options) (fun _ -> ()) "test [options]";


    let term1 = pterm "g (g a)" in
    let term2 = pterm "g (h a)" in
    Util.debugf 1 "find_disagreement %a, %a" (fun k -> k T.pp term1 T.pp term2);
    Util.debugf 1 "Result: %a" (fun k -> k (CCOpt.pp (CCPair.pp T.pp T.pp)) (JP_unif.find_disagreement term1 term2));

    let term1 = pterm "g (g a)" in
    let term2 = pterm "g (g b)" in
    Util.debugf 1 "find_disagreement %a, %a" (fun k -> k T.pp term1 T.pp term2);
    Util.debugf 1 "Result: %a" (fun k -> k (CCOpt.pp (CCPair.pp T.pp T.pp)) (JP_unif.find_disagreement term1 term2));

    let term1 = pterm "f_ho2 (fun (x:term). x)" in
    let term2 = pterm "f_ho2 (fun (x:term). a)" in
    Util.debugf 1 "find_disagreement %a, %a" (fun k -> k T.pp term1 T.pp term2);
    Util.debugf 1 "Result: %a" (fun k -> k (CCOpt.pp (CCPair.pp T.pp T.pp)) (JP_unif.find_disagreement term1 term2));


    let term1 = List.hd (snd (T.as_app (pterm "q (x a b)"))) in
    Util.debugf 1 "project %a" (fun k -> k T.pp term1);
    Util.debugf 1 "Result: %a" (fun k -> k (CCList.pp (Subst.pp))  (Sequence.to_list (JP_unif.project_onesided term1)));


    let term1 = pterm "x a b" in
    let term2 = pterm "f c d" in
    Util.debugf 1 "imitate %a, %a" (fun k -> k T.pp term1 T.pp term2);
    Util.debugf 1 "Result: %a" (fun k -> k (CCList.pp (Subst.pp))  (Sequence.to_list (JP_unif.imitate term1 term2)));


    let term1 = List.hd (snd (T.as_app (pterm "q (x a b)"))) in
    let term2 = List.hd (snd (T.as_app (pterm "q (y c d)"))) in
    Util.debugf 1 "identify %a, %a" (fun k -> k T.pp term1 T.pp term2);
    Util.debugf 1 "Result: %a" (fun k -> k (CCList.pp (Subst.pp)) (Sequence.to_list (JP_unif.identify term1 term2)));


    let term1 = List.hd (snd (T.as_app (pterm "q (x a)"))) in
    let term2 = List.hd (snd (T.as_app (pterm "q (y b)"))) in
    Util.debugf 1 "unify %a, %a" (fun k -> k T.pp term1 T.pp term2);
    Util.debugf 1 "Result: %a" (fun k -> k (CCList.pp (Subst.pp)) (Sequence.to_list (JP_unif.unify term1 term2)));
