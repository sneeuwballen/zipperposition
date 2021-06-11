(* open Batteries opam install batteries + edit src/core/dune *)
open Logtk
open Logtk_parsers
open Logtk_proofs
open Libzipperposition
open Phases_impl
(* open Libzipperposition_calculi *)
(* open Phases.Infix *)
open CCVector
open CCFun
open Literal
module B = Builtin
open Monome
open Type
open Term

let (~..)x = print_endline(Batteries.dump x); x

module MakeSumSolver(MainEnv: Env.S) = struct
  (* module Env = MainEnv *)
  module C = MainEnv.C
  (* module Ctx = MainEnv.Ctx *)

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

  let step text parents lits =
    C.create lits ~penalty:1 ~trail:(C.trail_l[]) (if parents=[]
    then (if text="goal" then Proof.Step.goal' else Proof.Step.assert') ~file:"" ~name:text ()
    else Proof.Step.inference ~tags:[] ~rule:(Proof.Rule.mk text) (List.map (fun p -> C.proof_parent_subst (Subst.Renaming.create()) (p,0) Subst.empty) parents))
  
(* 
box(x∈S) ⊢ box⊥∨x∈S, box⊤∨x∉S
β⇔x∈S clausifies to β∨x∉S, ¬β∨x∈S
Should not select ¬β ⟹ prefer first with box⊥ (β̅) and box⊤ (β) atomic, positive and small.
*)

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

  let demo_proof c =
    let constants = Hashtbl.create 0 in
    let have ?(infix=false) name par ty = match Hashtbl.find_opt constants name with
    | None ->
      let i = ID.make name in
      if infix then ID.set_payload i (ID.Attr_infix name);
      let c = const (arrow par ty) i in
      Hashtbl.add constants name c; c
    | Some c -> c in
    let ( * ) = app in
    let ( *?) = app_builtin ~ty:prop in
    let ( *:) = app_builtin ~ty:int in
    let plus1 n = B.Sum*:[n; B.Int Z.one *:[]] in
    let _0 = B.Int Z.zero *:[] in
    let (=<) a b = B.Lesseq*?[a;b] in
    let (&) a b = B.And*?[a;b] in
    let _Z = builtin ~ty:tType B.ty_int in
    (* let (=~) a b = B.Eq*?[builtin ~ty:tType B.ty_int; a; b] in *)
    let int2 = arrow [int] int in
    let int3 = arrow [int2] int2 in
    let miinus = have "−₂" ~infix:true [int3; int3] int3 in
    let (++) a b = have "+" ~infix:true [int2; int2] int2 *[a;b] in
    let (--) a b = have "−" ~infix:true [int2; int2] int2 *[a;b] in
    let j = var % HVar.make ~ty:int in
    let s = var % HVar.make ~ty:(arrow [int] prop) in
    let j0to = fun_of_fvars[HVar.make ~ty:int 0] in
    let box b = mk_prop (have "box" [prop] prop *[b]) true in
    let xx = have "probe" [] int in
    let sum = have "∑" [arrow [int] prop; arrow [int] int;] int in
    let _I = have "id" [int2] int2 in
    let f = have "f" [int] int in
    let g = have "g" [int] int in
    let m = have "m" [] int in
    let upto = have "upto" [int;int] prop in
    let diff = have "diff" [arrow [int] prop; arrow [int] prop; int] prop in
    let mm = have "T⁺¹" [arrow [int] int; int] int in

    let m_in_N = step "given" [] [mk_arith_lesseq (Int.const Z.zero) (Int.singleton Z.one m)] in
    let upto_def = step "given" [] [mk_eq (upto*[j 0; j 1]) (_0 =< j 1 & j 1 =< j 0)] in
    let diff_def = step "given" [] [mk_eq (diff*[s 0; s 1; j 0]) (s 0 *[j 0] & B.Not*?[s 1 *[j 0]])] in
    let goal = step "goal" [] [mk_neq (sum*[upto*[m]; f]) (Sum*:[g*[m]; f*[_0]])] in

    let enum1 = step "start enumeration" [goal] [box(diff*[upto*[m]; upto*[plus1 m]; xx])] in
    let enum2 = step "rewrite" [enum1; diff_def] [box(upto*[m;xx] & B.Not*?[upto*[plus1 m; xx]])] in
    let enum3 = step "rewrite" [enum2; upto_def] [box((_0=<xx & xx=<m) & B.Not*?[_0=<xx & xx=< plus1 m])] in
    let enum4 = step "≤-simp" [enum3; m_in_N] [box(B.Eq*?[_Z; plus1 m; xx])] in
    let enum'1 = step "start enumeration" [goal] [box(diff*[upto*[plus1 m]; upto*[m]; xx])] in
    let enum'2 = step "rewrite" [enum'1; diff_def] [box(upto*[plus1 m; xx] & B.Not*?[upto*[m; xx]])] in
    let enum'3 = step "rewrite" [enum'2; upto_def] [box((_0=<xx & xx=< plus1 m) & B.Not*?[_0=<xx & xx=<m])] in
    let enum'4 = step "≤-simp" [enum'3; m_in_N] [box(B.False*?[])] in
    let sumf = j0to(sum*[upto*[j 0]; f]) in
    let deltaM = step "sum domain split" [enum4; enum'4] [mk_eq
      (miinus*[mm;_I; sumf])
      (j0to(sum*[app_builtin ~ty:(arrow [int] prop) B.Eq [_Z; plus1(j 0)]; f]))] in
    let sumf_def' = step "sum singletons" [deltaM] [mk_eq (miinus*[mm;_I; sumf]) (mm*[f])] in
    
    let g0_0 = step "given" [] [mk_eq (g*[_0]) _0] in
    let g_def = step "given" [] [mk_eq (g*[plus1(j 0)]) (B.Sum*:[f*[plus1(j 0)]; g*[j 0]])] in
    let g_def' = step "operator format" [g_def] [mk_eq (mm*[g]) (mm*[f] ++ g)] in
    let gf0 = g++j0to(f*[_0]) in
    let gf0_1st = step "sup (additive)" [g_def'] [mk_eq (mm*[gf0] -- j0to(f*[_0])) (mm*[f])] in
    let gf0_def' = step "arithmetic" [gf0_1st] [mk_eq (miinus*[mm;_I; gf0]) (mm*[f])] in

    let sumf_gf0_1st = step "sup (additive)" [sumf_def'; gf0_def'] [mk_eq (mm*[sumf--gf0] -- mm*[f] ++ mm*[f]) (sumf--gf0)] in
    let sumf_gf0_def' = step "arithmetic" [sumf_gf0_1st] [mk_eq (miinus*[mm;_I; sumf--gf0]) (j0to _0)] in
    let initials_only = step "induction (step +1 from 0)" [sumf_gf0_def'; goal] [mk_neq (sum*[upto*[_0]; f]) (Sum*:[g*[_0]; f*[_0]])] in

    let enum_1 = step "start enumeration" [initials_only] [box(upto*[_0;xx])] in
    let enum_2 = step "rewrite" [enum_1; upto_def] [box(_0=<xx & xx=<_0)] in
    let enum_3 = step "≤-simp" [enum_2] [box(B.Eq*?[_Z;_0;xx])] in
    let goal_f0_g0f0 = step "sum singletons" [initials_only; enum_3] [mk_neq (f*[_0]) (Sum*:[g*[_0]; f*[_0]])] in
    let goal_0_g0 = step "arithmetic" [goal_f0_g0f0] [mk_neq _0 (g*[_0])] in
    let contradiction = step "sup" [g0_0; goal_0_g0] [] in
    [contradiction]
    
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
  env_actions = [fun env ->
    let module E= (val env) in (* Solves: “The parameter cannot be eliminated in the result type.” *)
    let module I= MakeSumSolver(E) in()];
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
