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
open Stdlib

let (~..)x = print_endline(Batteries.dump x); x

let (~=) x _ = x
let (%%>) = compose_binop
let (%>>) f g x y = g(f x y)

let ( *** ) c1 c2 (x1,x2) (y1,y2) = match c1 x1 y1 with 0 -> c2 x2 y2 | r -> r

let rec lex_list c = curry(function
| [], [] -> 0
| [], _ -> 1
| _, [] -> -1
| x::xx, y::yy -> (c *** lex_list c) (x,xx) (y,yy))


(* debug print *)
(* let patterns = Hashtbl.create 0
let i x = let x=repr x in if is_int x then [magic x] else let s=size x in tag x :: List.(map magic (init s (fun a->a)));;*)
module Debug = struct
open List
open Obj
type any

(* helper *)
let test (c:int) f x = let x = repr x in
  if is_int x then 0 <= magic x && magic x < c
  else f(tag x, init (size x) (magic(field x)))

let test_tag tag f = test 0 (fun(tag',data) -> tag=tag' && f data)

(* Main building blocks of type test functions *)

let any _ = true (* especially exceptions at the moment *)
let int x = is_int(repr x)

(* Test an algebraic data type: c is the number of constant constructors, and ttt is a list of lists of type tests for the parameters of other constructors, in order of appearence. *)
let union c ttt = test c (fun(tag,data) ->
  tag < length ttt &&
  let tt = nth ttt tag in
  length tt = length data &&
  for_all2 id tt data)

let rec list t = union 1 [[t; list t]]

(* Test flat tuples and records: tt is a list of type test for the components, in order of appearence. *)
let tuple tt = test_tag 0 (fun data -> length tt = length data && for_all2 id tt data)
let array t = test_tag 0 (for_all t)
let string x = test_tag string_tag any x
let custom x = test_tag custom_tag any x (* e.g. int32 *)
let lazy_any x = test_tag lazy_tag any x
let lazy_force t x = test_tag lazy_tag ~=(t(Lazy.force(magic x))) x

(* arbitrary precision *)
let integer x = int x or custom x
let rational x = tuple[integer; integer] x

let builtin x = union 57 [[integer]; [rational]; [int]] x
let rec term x = tuple[view; union 1 [[term]]; int; any; custom; lazy_force int] x
and view x = union 0 [
  [tuple[int;term]]; (* HVar *)
  [int];
  [union 4 []; term; term];
  [tuple[int; string; list any]]; (* ID *)
  [term; list term];
  [builtin; list term]] x

let base = function `Mono -> true | `Exp t |`Move t -> term t | _-> false
let power x = tuple[base;int;int] x
let monomial x = tuple[term; list power; term] x
let poly x = array monomial x


(* Registering adhoc polymorphic pretty printers *)

let string_printers: ((any -> bool) * (any -> string)) list ref = ref[]

let add_pp type_test to_string = string_printers := (type_test, to_string % magic) :: !string_printers

let pp x =
  let exception Result of string in
  try magic!string_printers |> List.iter(fun(type_test, to_string) ->
    if type_test x then raise(Result(to_string x)));
    Batteries.dump x
  with Result s -> s
(* Sprinkle ~< in front of expressions you want to trace—often without rebracketing! *)
let (~<)x = print_endline(pp x); x
let (>~<) msg x = print_endline(msg ^" "^ pp x); x;;

add_pp integer Z.to_string;
add_pp rational Q.to_string;
add_pp string id;
add_pp term Term.to_string;

end


module Ore = struct
type power = {base:[`Mono |`Exp of term |`Move of term]; var:int; exp:int}
type monomial =
    (* Example: (¾-2/a) ⬝ m³2ⁿ⁵aⁿ ⬝ M₊ₐ²N₊₁⁴ ⬝ f(m,n)  (= (¾-2/a) m³ 2⁵ⁿ aⁿ f(m+2a, n+4)) *)
    term * power list * term
type poly = monomial array

let look_old, look_new = "0123456789-zyxwvutsr", String.split_on_char ' ' "⁰ ¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ ⁻ ᶻ ʸ ˣ ʷ ᵛ ᵘ ᵗ ˢ ʳ"
let super = String.to_seq %> List.of_seq %> List.map(fun c -> match String.index_opt look_old c with
  | None -> String.make 1 c
  | Some i -> List.nth look_new i) %> String.concat""

let varstr n = String.make 1 (Char.chr(122 - n))
let varStr n = String.make 1 (Char.chr(90 - n))

let pp_power{base;var;exp} = match base with
|`Mono -> varstr var ^ super(string_of_int exp)
|`Exp t -> Term.to_string t ^ super(varstr var ^ string_of_int exp)
|`Move t -> varStr var ^ super(string_of_int exp)

let pp_mono(c,m,f) = Term.to_string c ^ String.concat"" (List.map pp_power m) ^ Term.to_string f

let pp p = Array.(if length p = 0 then "0" else String.concat " + " (to_list(map pp_mono p)))


(* Coefficient arithmetic. TODO use general simplification instead of or in addition to special casing ℤ constants. *)

let _Z z = app_builtin ~ty:int (Int z) []
let _z = _Z % Z.of_int

let if_Z fZ f' t s = match view t, view s with
| AppBuiltin(Int t', _), AppBuiltin(Int s', _) -> fZ t' s'
| _ -> f' t s

let (-|-) = if_Z (Z.(+)%>>_Z) (fun t s -> app_builtin ~ty:(ty t) Sum [t;s])
let (><) = if_Z (Z.( * )%>>_Z) (fun t s -> app_builtin ~ty:(ty t) Product [t;s])
let rec (^) t = function 0 -> _z 1 | e -> t >< t^(e-1)

let lcm_coefs = if_Z Z.(fun t s -> let l r = _Z(divexact (lcm t s) r) in l t, l s) (fun t s -> s,t)


(* Comparison functions for powers, monomials *)

let rank x = (match x.base with
  |`Mono -> 0, _z 0
  |`Exp t -> 1, t
  |`Move t -> 2, t
), x.var

let compare_rank = rank %%> ((-) *** Term.compare) *** (-)

let mono_total_deg = List.fold_left (fun d x -> d + x.exp) 0
let deg v = List.fold_left (fun d x -> if v.base=x.base && v.var=x.var then d + x.exp else d) 0

(* TODO Make this double purpose: “term order” as by-product? *)
let compare_mono = (fun(c,m,f) -> mono_total_deg m, (m, (f, c))) %%> (-) *** lex_list compare_rank *** Term.compare *** Term.compare

(* Arithmetic of polynomials etc. Main operations to superpose polynomials are: addition, multiplication by monomial, and pre-lcm of monomials. *)

let poly0 = Array.of_list[]
let set_exp x e = if e=0 then [] else [{x with exp=e}]

(* Make a polynomial by summing list of full monomials. *)
let sum_monomials =
  let rec sum_sorted = function
  | (c,n,f)::(c',n',f')::more when n=n' && f=f' -> sum_sorted((c-|-c', n, f)::more)
  | (c,n,f)::more when c = _z 0 -> sum_sorted more
  | m::more -> m:: sum_sorted more
  | [] -> []
  in
  Array.of_list % sum_sorted % List.sort(flip compare_mono)

(* polynomial + polynomial *)
let (++) p r = sum_monomials Array.(to_list p @ to_list r)

(* constant × polynomial *)
let ( *:) a = if a = _z 0 then ~=poly0 else Array.map(fun(c,m,f) -> (a><c, m, f))

(* monomial × polynomial *)
let ( **:) mon =
  let open List in
  (* Multiply (coefficient, monomial, reversed monomial) triplet. Result is a list of monomials. Remember that the lists representing monomials are in reverse visual order. *)
  let rec mul o=o|> flatten % map(function
  | (coef, m), [] -> [coef, m]
  | (coef, []), m_rev -> [coef, rev m_rev]
  | (coef, n::m), v::w_rev -> match compare_rank n v with
    (* merge n, v *)
    | 0 -> [coef, rev w_rev @ set_exp v (n.exp+v.exp) @ m]
    (* right order *)
    | c when c<0 -> [coef, rev w_rev @ [n;v] @ m]
    (* commute n, v *)
    | _ -> let (&) m a = map(fun n -> (n,a)) m in
      let s = n.var=v.var in
      match n.base, v.base with
      |`Move n', `Exp v' when s -> mul(mul[(coef >< v' ^ n.exp*v.exp, m), [n;v]] & w_rev)
      |`Move n', `Mono when s -> mul(
        (* c D₊ₐⁿ dᵛ = c (d+na)ᵛ D₊ₐⁿ = ∑k∈[0,v]: c(ᵛₖ)(na)ᵛ⁻ᵏ dᵏ D₊ₐⁿ *)
        mul(init(v.exp+1)id |> map(fun k ->
          (coef >< _Z Z.(bin (of_int v.exp) k) >< (_z n.exp >< n')^(v.exp-k), m), n :: set_exp v k)
        ) & w_rev)
      | _ -> mul(mul[(coef, m), [n;v]] & w_rev))
  in
  sum_monomials % flatten % map(fun(c,m,f) -> mul[(c, mon), rev m] |> map(fun(c',m') -> c',m',f)) % Array.to_list


(* Given monomials m1,m2, find f1,f2 s.t. f1⬝m1 ≈ f2⬝m2 upto coefficients and lower order terms. *)
let rec lcm_factors m1 m2 = match m1,m2 with
  | [],_ | _,[] -> m2,m1
  | x1::n1, x2::n2 ->
    let e = x1.exp - x2.exp in
    match compare_rank x1 x2 with
    | 0 -> let f1,f2 = lcm_factors n1 n2 in
      (* e.g. A²BC⁵, B³C² ⇒ add C⁰, C⁵⁻² to lcm_ A²B, B³ *)
      set_exp x1 (max (-e) 0) @ f1, set_exp x2 (max e 0) @ f2
    | r when r<0 ->
      (* e.g. A²B, B³C² ⇒ add C², 1 to lcm_ A²B, B³ *)
      let f1,f2 = lcm_factors (x1::n1) n2 in x2::f1, f2
    | _ -> let f1,f2 = lcm_factors n1 (x2::n2) in f1, x1::f2


let superpose p1 p2 =
  let f1, f2 = ((fun(_,m,_)->m) %%> lcm_factors) p1.(0) p2.(0) in
  let p'1, p'2 = f1**:p1, f2**:p2 in
  let a1, a2 = ((fun(c,_,_)->c) %%> lcm_coefs) p'1.(0) p'2.(0) in
  a1*:p'1 ++ (_z(-1)><a2)*:p'2

end




module MakeSumSolver(MainEnv: Env.S) = struct
(* module Env = MainEnv *)
module C = MainEnv.C
(* module Ctx = MainEnv.Ctx *)

let polyform_cache =
  let module NoMemoryLeakMap = Weak.Make(HashLiteral) in
  NoMemoryLeakMap.create 0

let polyform l = None

let fake_poly_lit = const ~ty:int % ID.make % Ore.pp

let clauseset clauselist = {Clause.c_set= of_list clauselist; c_sos= of_list[]}

let superpose_poly l1 = match polyform l1 with
| None -> ~=[]
| Some p1 ->
  fun l2 -> match polyform l2 with
  | None -> []
  | Some p2 -> [fake_poly_lit(Ore.superpose p1 p2)]

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
