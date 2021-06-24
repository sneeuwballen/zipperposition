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
Must not select ¬β (or with β̅ := ¬β clauses β̅∨β∨... should be deleted as tautologies).
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
    let (=<.) a b = mk_arith_lesseq (Int.singleton Z.one a) (Int.singleton Z.one b) in
    let (<.) a b = mk_arith_less (Int.singleton Z.one a) (Int.singleton Z.one b) in
    let int2 = arrow [int] int in
    (* let int3 = arrow [int2] int2 in *)
    (* let miinus = have "−₂" ~infix:true [int3; int3] int3 in *)
    let (++) a b = have "+" ~infix:true [int2; int2] int2 *[a;b] in
    let (--) a b = have "−" ~infix:true [int2; int2] int2 *[a;b] in
    let j = var % HVar.make ~ty:int in
    let s = var % HVar.make ~ty:(arrow [int] prop) in
    let j0to = fun_of_fvars[HVar.make ~ty:int 0] in
    (* let box b = mk_prop (have "box" [prop] prop *[b]) true in *)
    let xx = have "probe" [] int in
    let bb = have "𝕓" [] prop in
    let sum = have "∑" [arrow [int] prop; arrow [int] int;] int in
    let _I = have "id" [int2] int2 in
    let f = have "f" [int] int in
    let g = have "g" [int] int in
    let m = have "m" [] int in
    let upto = have "upto" [int;int] prop in
    let diff = have "diff" [arrow [int] prop; arrow [int] prop; int] prop in
    let mm = have "D⁺¹" [int2] int2 in
    let mm_1 f = have "(D⁺¹ − id)" [int2] int2 *[f] in

    (* Start clauses *)
    let m_in_N = step "given" [] [mk_arith_lesseq (Int.const Z.zero) (Int.singleton Z.one m)] in
    let upto_def = step "given" [] [mk_eq (upto*[j 0; j 1]) (_0 =< j 1 & j 1 =< j 0)] in
    let diff_def = step "given" [] [mk_eq (diff*[s 0; s 1; j 0]) (s 0 *[j 0] & B.Not*?[s 1 *[j 0]])] in
    let goal = step "goal" [] [mk_neq (sum*[upto*[m]; f]) (Sum*:[g*[m]; f*[_0]])] in

    (* Enumerate upto m \ upto(m+1) =∅ *)
    let enumO1 = step "start enumeration" [goal] [mk_eq (diff*[upto*[m]; upto*[plus1 m]; xx]) bb] in
    let enumO2 = step "rewrite" [enumO1; diff_def] [mk_eq (upto*[m;xx] & B.Not*?[upto*[plus1 m; xx]]) bb] in
    let enumO3 = step "rewrite" [enumO2; upto_def] [mk_eq ((_0=<xx & xx=<m) & B.Not*?[_0=<xx & xx=< plus1 m]) bb] in
    let enumO4a = step "clausify" [enumO3] [xx =<. m; mk_prop bb false] in
    let enumO4b = step "clausify" [enumO3] [_0 =<. xx; mk_prop bb false] in
    let enumO4c = step "clausify" [enumO3] [xx <. _0; plus1 m =<. xx; mk_prop bb false] in
    let enumO5 = step "lin. arith." [enumO4a;enumO4b;enumO4c] [mk_prop bb false] in
    (* Enumerate upto(m+1) \ upto m ={m+1} *)
    let enumI1 = step "start enumeration" [goal] [mk_eq (diff*[upto*[plus1 m]; upto*[m]; xx]) bb] in
    let enumI2 = step "rewrite" [enumI1; diff_def] [mk_eq (upto*[plus1 m; xx] & B.Not*?[upto*[m; xx]]) bb] in
    let enumI3 = step "rewrite" [enumI2; upto_def] [mk_eq ((_0=<xx & xx=< plus1 m) & B.Not*?[_0=<xx & xx=<m]) bb] in
    let enumI4a = step "clausify" [enumI3] [xx =<. (plus1 m); mk_prop bb false] in
    let enumI4b = step "clausify" [enumI3] [_0 =<. xx; mk_prop bb false] in
    let enumI4c = step "clausify" [enumI3] [xx <. _0; m <. xx; mk_prop bb false] in
    let enumI4' = step "clausify" [enumI3] [xx <. _0; plus1 m <. xx; xx =<. m; mk_prop bb true] in
    let enumI5a = step "lin. arith." [enumI4a;enumI4b;enumI4c] [mk_eq xx (plus1 m); mk_prop bb false] in
    let enumI5' = step "lin. arith." [enumI4'; m_in_N] [mk_neq xx (plus1 m); mk_prop bb true] in
    (* Use the enumerations *)
    let sumf = j0to(sum*[upto*[j 0]; f]) in
    let deltaM = step "near-commute D⁺¹" [enumO5; enumI5a; enumI5'] [mk_eq (mm_1 sumf)
      (j0to(sum*[app_builtin ~ty:(arrow [int] prop) B.Eq [_Z; plus1(j 0)]; f]))] in
    let sumf_def' = step "sum singletons" [deltaM] [mk_eq (mm_1 sumf) (mm*[f])] in
    
    (* Derive (D⁺¹-1)(g+f0) = D⁺¹f *)
    let g0_0 = step "given" [] [mk_eq (g*[_0]) _0] in
    let g_def = step "given" [] [mk_eq (g*[plus1(j 0)]) (B.Sum*:[f*[plus1(j 0)]; g*[j 0]])] in
    let g_def' = step "operator form" [g_def] [mk_eq (mm*[g]) (mm*[f] ++ g)] in
    let gf0 = g++j0to(f*[_0]) in
    let gf0_1st = step "sup (additive)" [g_def'] [mk_eq (mm*[gf0]) (gf0 ++ mm*[f])] in
    let gf0_def' = step "arithmetic" [gf0_1st] [mk_eq (mm_1 gf0) (mm*[f])] in

    (* Derive final (D⁺¹-1) ... = 0 and use induction. *)
    let sumf_gf0_1st = step "sup (additive)" [sumf_def'; gf0_def'] [mk_eq (mm*[sumf--gf0] -- mm*[f] ++ mm*[f]) (sumf--gf0)] in
    let sumf_gf0_def' = step "arithmetic" [sumf_gf0_1st] [mk_eq (mm_1(sumf--gf0)) (j0to _0)] in
    let initials_only = step "induction (step +1 from 0)" [sumf_gf0_def'; goal] [mk_neq (sum*[upto*[_0]; f]) (Sum*:[g*[_0]; f*[_0]])] in

    (* Finish base case by computing ∑{0}f = f0. *)
    let enum01 = step "start enumeration" [initials_only] [mk_eq (upto*[_0;xx]) bb] in
    let enum02 = step "rewrite" [enum01; upto_def] [mk_eq (_0=<xx & xx=<_0) bb] in
    let enum03a = step "clausify" [enum02] [_0 =<. xx; mk_prop bb false] in
    let enum03b = step "clausify" [enum02] [xx =<. _0; mk_prop bb false] in
    let enum03' = step "clausify" [enum02] [_0 <. xx; xx <. _0; mk_prop bb true] in
    let enum04a = step "lin. arith." [enum03a;enum03b] [mk_eq xx _0; mk_prop bb false] in
    let enum04' = step "lin. arith." [enum03'] [mk_neq xx _0; mk_prop bb true] in
    let goal_f0_g0f0 = step "sum singletons" [initials_only; enum04a; enum04'] [mk_neq (f*[_0]) (Sum*:[g*[_0]; f*[_0]])] in
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
