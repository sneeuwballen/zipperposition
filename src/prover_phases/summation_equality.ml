(* open Batteries opam install batteries + edit src/core/dune *)
open Logtk
open Logtk_parsers
open Logtk_proofs
open Libzipperposition
open Phases_impl
open Comparison
(* open Libzipperposition_calculi *)
(* open Phases.Infix *)
open CCArray
open CCVector
open CCFun
open Literals
open Literal
module B = Builtin
open Monome
open Type
open Term

let (~..)x = print_endline(Batteries.dump x); x

module Ore = struct
type monomial = (term*int)list
type poly = monomial array

(*
int - mul
mon var mul
exp var mul
coef vars mul
sh var dis
neg var dis
mul var dis
dis vars dis
*)
type atoms = {exp:int; vars: int list; id:[
  `Mul of [`Z of term |`Mono |`Exp of term |`Other of term] |
  `Dis of [`Shift of term |`Refl |`Scale of int(*>0*) |`Other of term]]}

let _Z z = app_builtin ~ty:int (Int(Z.of_int z)) []
let _Z' z = {id= `Mul(`Z(_Z z)); vars=[]; exp=1}

let base_mul m w =
  let c = true in
  let mvar = List.hd m.vars in
  let mul_like c' n v =
    if n=v then
      if m.vars=w.vars then
        let exp = m.exp + w.exp in if exp=0 then [] else [{m with exp}]
      else if c' && Stdlib.compare m.vars w.vars > 0 then [w;m] else [m;w]
    else if c' && compare n v > 0 then [w;m] else [m;w]
  in
  let exception Result of Type.t in
  let displace_w op par = match w with {exp; vars; id= `Mul(`Other v)}
    -> {exp; vars; id= `Mul(`Other(
      let ty = try let _= Seq.subterms ~include_builtin:true v (fun t -> match view t with Var{ty;id} when id=mvar -> raise(Result ty) | _->()) in int(*irrelevant*) with Result ty -> ty in
      let old = var(HVar.make ty mvar) in
      replace ~old ~by:(app_builtin ~ty op (old::par)) v))}
    |_-> assert false
  in
  match m.id, w.id with
  (* equal form *)
  |`Mul(`Z n), `Mul(`Z v) -> [{m with id=`Mul(`Z(app_builtin ~ty:int Product [n;v]))}]
  |`Mul(`Exp n), `Mul(`Exp v)
  |`Mul(`Other n), `Mul(`Other v) -> mul_like c n v
  |`Dis(`Shift n), `Dis(`Shift v) -> mul_like true n v
  |`Dis(`Scale n), `Dis(`Scale v) -> mul_like true (_Z n) (_Z v)
  (* classic commutators *)
  |`Dis(`Shift n), `Mul`Mono when c -> [w;m] (* TODO additive terms? *)
  |`Dis(`Shift n), `Mul(`Exp v) when c -> [] (* TODO vⁿ *)
  (* ℤ-scaling *)
  |`Dis`Refl, `Mul`Mono -> [_Z'(-1); w; m]
  |`Dis`Refl, (`Mul(`Exp v)|`Dis(`Shift v))  -> [{w with exp= -w.exp}; m] (* reflection has exponent 1 *)
  |`Dis(`Scale n), `Mul`Mono -> [_Z' n; w; m]
  |`Dis(`Scale n), (`Mul(`Exp v)|`Dis(`Shift v)) when m.exp>0 -> [{w with exp= Batteries.Int.pow n m.exp * w.exp}; m]
  (* (∘d) (×f) = (× f∘d) (∘d) *)
  |`Dis(`Shift n),	`Mul(`Other v) -> [displace_w Sum[n]; m]
  |`Dis`Refl,	`Mul(`Other v) -> [displace_w Uminus[]; m]
  |`Dis(`Scale n),	`Mul(`Other v) -> [displace_w Product[_Z n]; m]
  (* commutatives *)
  | _, `Mul(`Z _)
  |`Dis`Refl, `Dis(`Scale _) -> [w;m]
  |(`Mul(`Exp n), `Mul`Mono
  |`Mul(`Other n), `Mul _) when c ->[w;m]
  |`Dis(`Other n), _ when not(List.mem mvar w.vars) -> [w;m]
  (* good as is *)
  | _ -> [m;w]

(* ℤ<n<xⁿ<other<N₊₁<Nₓ₂<N₋<other *)
(* let weight = function
|`Mul(`Z t) -> 10, t
|`Mul `Mono -> 11, DB 0
|`Mul(`Exp t) -> 12, t
|`Mul(`Other t) -> 13, t
|`Dis(`Shift t) -> 20, t
|`Dis `Refl -> 21, DB 0
|`Dis(`Scale t) -> 22, DB t (* bypass type incompatibility *)
|`Dis(`Other t) -> 23, t
let weighting x y =
  let x1,x2 = weight x in
  let y1,y2 = weight y in
  if x1<y1 then Lt else
  if x1>y1 then Gt else

  match x.value, y.value with
  | Mul _, Dis _ -> Lt
  | 
  | x,y -> match weighting y x with Lt->Gt | Gt->Lt | w->w

type indeterminates = {
  mutable multiplicative_commutators: int list}
let indeterminates = {
  multiplicative_commutators = []
} *)


let (><) = (@)
let rec (^) m = function 0 -> [] | e -> m >< m^e

let commute _ _ = true
let weighting _ _ = Eq

(* Given monomials m1,m2, find f1,f2 s.t. f1><m1 = f2><m2 =: “lcm m1 m2”. *)
let rec lcm_factors m1 m2 = try match m1,m2 with
  | (x1,e1)::_, (x2,e2)::_ ->
    (* Compute heavest factors f1,f2 from the heavest indeterminates x1,x2. *)
    let f1,f2 = match weighting x1 x2 with
    | Eq -> [x1, max(e2-e1)0], [x2, max(e1-e2)0] (* e.g. A²BC³, B³C⁵ ↦ C⁵⁻³, C⁰ *)
    | Lt -> [x2,e2], [] (* e.g. A²B, B³C⁵ ↦ C⁵, 1 *)
    | Gt -> [], [x1,e1]
    | Incomparable -> raise Exit
    in(match f1><m1, f2><m2 with
    (* After update by factors f1,f2, check that the heavest indeterminates y1,y2 now cancel, and recurse to lighter ones. Note that y1=y2 guards againts Some wrong result regardless of f1,f2. Hence e.g. case e1<0 or e2<0 needs no checks. *)
    | y1::n1, y2::n2 when y1=y2 -> (match lcm_factors n1 n2 with
      | Some(k1,k2) -> Some(k1><f1, k2><f2)
      | _ -> raise Exit)
    | _ -> raise Exit)
  | m1,m2 -> Some(m2,m1) (* another is 1 *)
  with Exit -> None
(* at the leading monome level:
nxⁿ = xⁿn
Nn = nN
Nxⁿ = x⬝xⁿN
~n = - n~
~N = N⁻¹~
~xⁿ = x⁻ⁿ~
𝟚n = 2 n𝟚
𝟚N = NN𝟚
𝟚xⁿ = xⁿxⁿ𝟚
𝟚~ = ~𝟚
*)


end

module MakeSumSolver(MainEnv: Env.S) = struct
(* module Env = MainEnv *)
module C = MainEnv.C
(* module Ctx = MainEnv.Ctx *)

let polyform_cache =
  let module NoMemoryLeakMap = Weak.Make(HashLiteral) in
  NoMemoryLeakMap.create 0

let clauseset clauselist = {Clause.c_set= of_list clauselist; c_sos= of_list[]}

module SubEnv = Env.Make(struct
    module Ctx = MainEnv.Ctx
    (* module C = c *)
    let params = MainEnv.params
    let flex_state = MainEnv.flex_state()
  end)

(* Given an inference L¹,L²⊢σC, create and put into use an inference  L¹∨D¹, L²∨D² ⊢ σ(C∨D¹∨D²) , where literals L¹ and L² must be eligible. Allows multiple conclusions. *)
let on_eligible_literals name literal_inference =
  let lifted_inference c1 c2 =
    let rename = Subst.Renaming.create() in
    let c1_lits = C.lits c1 and c2_lits = C.lits c2 in
    (* TODO compute eligible literals only once *)
    fold_lits ~eligible:(C.Eligible.res c1) c1_lits |> Iter.flat_map(fun(l1,pos1) ->
    fold_lits ~eligible:(C.Eligible.res c2) c2_lits |> Iter.map(fun(l2,pos2) ->
      let infered, subst = literal_inference l1 l2 in
      let c1_no_l1 = apply_subst_list rename subst (except_idx c1_lits pos1, 0) in
      let c2_no_l2 = apply_subst_list rename subst (except_idx c2_lits pos2, 1) in
      C.create (infered @ c1_no_l1 @ c2_no_l2)
        ~penalty:(max (C.penalty c1) (C.penalty c2))
        ~trail:(C.trail_l[c1;c2])
        (Proof.Step.inference ~tags:[] ~rule:(Proof.Rule.mk name) (List.map (fun c -> C.proof_parent_subst rename c subst) [c1,0; c2,1]))
    )) |> Iter.to_rev_list in
  MainEnv.add_binary_inf name (fun c ->
    (* TODO use an indexing data structure *)
    Iter.flat_map_l (lifted_inference c) (MainEnv.get_active())
    |> Iter.to_rev_list
  )

let poly_sup c = [c]

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
  MainEnv.add_binary_inf "poly. sup." poly_sup;
  (* MainEnv.add_unary_inf "demo" demo_proof *)
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
    let module E= (val env) in (* Solves error: “The parameter cannot be eliminated in the result type.” *)
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
