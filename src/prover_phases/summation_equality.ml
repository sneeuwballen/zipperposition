open Logtk
open Logtk_parsers
open Logtk_proofs
open Libzipperposition
open Phases_impl
(* open Libzipperposition_calculi *)
(* open Phases.Infix *)
open CCVector
open CCFun
module B = Builtin
open Monome
open Type
open Term

let pr = print_endline
(* open Phases *)

(* let saturate env clauses =
  (* let ()= Phases_impl.process_file () in *)
  let result, clauses' = Phases_impl.presaturate_clauses env clauses in
  Phases_impl.try_to_refute env clauses' result *)


module MakeSumSolver(MainEnv: Env.S) = struct
  (* module Env = Env *)
  module C = MainEnv.C
  (* module Ctx = Env.Ctx *)

  let clauseset clauselist = {Clause.c_set= of_list clauselist; c_sos= of_list[]}

  module SubEnv = Env.Make(struct
      module Ctx = MainEnv.Ctx
      (* module C = c *)
      let params = MainEnv.params
      let flex_state = MainEnv.flex_state()
    end)
  
  let index_elimination_environment() =
      let env1 = (module SubEnv: Env.S with type C.t='Ct) in
      env1

  (* let steproof text parents = (Proof.Step.inference ~tags:[] ~rule:(Proof.Rule.mk text) (List.map (fun p -> C.proof_parent_subst (Subst.Renaming.create()) (p,0) Subst.empty) parents)) *)

  let step text parents lits =
    C.create lits ~penalty:1 ~trail:(C.trail_l[]) (Proof.Step.inference ~tags:[] ~rule:(Proof.Rule.mk text) (List.map (fun p -> C.proof_parent_subst (Subst.Renaming.create()) (p,0) Subst.empty) parents))
  
(* abstract "∀m≥0"
—
upto m \ upto(1+m) = {}	by ≤-simp from upto, diff
upto(1+m) \ upto m = {1+m}	by ≤-simp from upto, diff, m≥0
—
probe ∈ upto(1+m) \ upto m
↓ by diff
upto(1+m) probe && ~ upto m probe
↓ by upto
many ≤'s	m≥0
↓ by ≤-simp	↓
probe=1+m
—
(M-1) [sum(upto m) f] = f(1+m) = M[fm]
—
N[gn] = N[fn] + [gn]
(N-1) [gn+f0] = N[fn] *)

  let demo_proof _ =
    let have name par ty = snd(mk_fresh_skolem ~prefix:name [] (arrow par ty)) in
    let (@) = app in
    let (@?) = app_builtin ~ty:prop in
    let (@:) = app_builtin ~ty:int in
    let plus1 n = B.Sum@:[n; B.Int Z.one @:[]] in
    let _0 = B.Int Z.zero @:[] in
    let (=<) a b = B.Lesseq@?[a;b] in
    let (&) a b = B.And@?[a;b] in
    let j = var % HVar.make ~ty:int in
    let s = var % HVar.make ~ty:(arrow [int] prop) in
    let box b = Literal.mk_prop (have "box" [prop] prop @[b]) true in
    let xx = have "probe" [] int in
    let f = have "f" [int] int in
    let g = have "g" [int] int in
    let m = have "m" [] int in
    let upto = have "upto" [int;int] prop in
    let diff = have "diff" [arrow [int] prop; arrow [int] prop; int] prop in

    let m_in_N = step "given" [] [Literal.mk_arith_lesseq (Int.const Z.zero) (Int.singleton Z.one m)] in
    let upto_def = step "given" [] [Literal.mk_eq (upto@[j 0; j 1]) (_0 =< j 1 & j 1 =< j 0)] in
    let diff_def = step "given" [] [Literal.mk_eq (diff@[s 0; s 1; j 0]) (s 0 @[j 0] & B.Not@?[s 1 @[j 0]])] in

    let enum1 = step "helper" [] [box(diff@[upto@[m]; upto@[plus1 m]; xx])] in
    let enum2 = step "rewrite" [enum1; diff_def] [box(upto@[m;xx] & B.Not@?[upto@[plus1 m; xx]])] in
    let enum3 = step "rewrite" [enum2; upto_def] [box((_0=<xx & xx=<m) & B.Not@?[_0=<xx & xx=< plus1 m])] in
    let enum4 = step "≤-simp" [enum3; m_in_N] [box(B.Eq@?[xx; plus1 m])] in
    [step "TODO" [enum4] []]
    
  let inference_function clause =
    (* Printf.printf "%a" Clause.pp clause; *)
    let subenv = index_elimination_environment() in
    let saturated_set = Phases_impl.refute_or_saturate subenv (clauseset[]) in
    snd(saturated_set, [clause])
  
  (* Setup to do when MakeSumSolver(...) is called. *);;
  (* Options.add_opts[
    "-demo", Arg.Bool(fun a -> if a then *)
    MainEnv.add_unary_inf "demo" demo_proof
    (* ), "a test"] *)
end

let env(module Parent: Env.S) =
  let module NewEnvironment = Env.Make(struct
    module Ctx = Parent.Ctx
    let params = Parent.params
    let flex_state = Parent.flex_state()
  end) in
  let env1 = (module NewEnvironment: Env.S) in
  env1

(* Define name and setup action required to registration of this extension in libzipperposition_phases.ml *)
let extension ={
  Extensions.default with
  name = "∑";
  env_actions = [fun env -> let module I= MakeSumSolver(val env: Env.S) in()];
}

(* 
let make_env ~ctx:(module Ctx : Ctx_intf.S) ~params stmts =
  Phases.start_phase Phases.MakeEnv >>= fun () ->
  Phases.get >>= fun state ->
  let module MyEnv = Env.Make(struct
      module Ctx = Ctx
      let params = params
      let flex_state = state
    end) in
  let env1 = (module MyEnv : Env.S) in
  (* use extensions to customize env *)
  Extensions.extensions ()
  |> List.iter
    (fun e -> List.iter (fun f -> f env1) e.Extensions.env_actions);
  (* convert statements to clauses *)
  let c_sets = MyEnv.convert_input_statements stmts in
  let env2 = (module MyEnv : Env.S with type C.t = MyEnv.C.t) in
  Phases.return_phase (Phases.Env_clauses (env2, c_sets)) *)
