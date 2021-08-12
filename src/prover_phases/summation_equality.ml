(* open Batteries opam install batteries + edit src/core/dune *)
open Logtk
open Logtk_parsers
open Logtk_proofs
open Libzipperposition
open Phases_impl
open Comparison
(* open Libzipperposition_calculi *)
(* open Phases.Infix *)
open Util
open Util.UntypedPrint
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

let (~=) x _ = x
let (%%>) = compose_binop
let (%>>) f g x y = g(f x y)

(* type 't comparison = 't -> 't -> int *)
(* Lexicographic product of comparison functions onto tuples. *)
let ( *** ) c1 c2 (x1,x2) (y1,y2) = match c1 x1 y1 with 0 -> c2 x2 y2 | r -> r

(* Example: length_lex_list c = (fun l -> List.length l, l) %%> Stdlib.compare *** lex_list c *)
let rec lex_list c = curry(function
| [], [] -> 0
| [], _ -> 1
| _, [] -> -1
| x::xx, y::yy -> (c *** lex_list c) (x,xx) (y,yy))


(* make constants for debugging *)
let constants = Hashtbl.create 0
let have ?(infix=false) name par ty = match Hashtbl.find_opt constants name with
| None ->
  let i = ID.make name in
  if infix then ID.set_payload i (ID.Attr_infix name);
  let c = const (arrow par ty) i in
  Hashtbl.add constants name c; c
| Some c -> c



(* Given an inference L¹,L²⊢σC, create and put into use an inference  L¹∨D¹, L²∨D² ⊢ σ(C∨D¹∨D²) , where literals L¹ and L² must be eligible. Allow multiple conclusions. *)
let on_eligible_literals(type c)(module Env: Env.S with type C.t=c) name literal_inference =
  let module C = Env.C in
  let lifted_inference c1 c2 =
    let rename = Subst.Renaming.create() in
    let c1_lits = C.lits c1 and c2_lits = C.lits c2 in
    (* TODO compute eligible literals only once *)
    fold_lits ~eligible:(C.Eligible.res c1) c1_lits |> Iter.flat_map(fun(l1,pos1) ->
    fold_lits ~eligible:(C.Eligible.res c2) c2_lits |> Iter.flat_map_l(fun(l2,pos2) ->
      literal_inference l1 l2 |> List.map(fun(infered, subst) ->
        let c1_no_l1 = apply_subst_list rename subst (except_idx c1_lits pos1, 0) in
        let c2_no_l2 = apply_subst_list rename subst (except_idx c2_lits pos2, 1) in
        C.create (infered @ c1_no_l1 @ c2_no_l2)
          ~penalty:(max (C.penalty c1) (C.penalty c2))
          ~trail:(C.trail_l[c1;c2])
          (Proof.Step.inference ~tags:[] ~rule:(Proof.Rule.mk name) (List.map (fun c -> C.proof_parent_subst rename c subst) [c1,0; c2,1]))
    ))) |> Iter.to_rev_list
  in
  Env.add_binary_inf name (fun c ->
    "Active clauses (given first)"|< Iter.to_rev_list(Env.get_active());
    (* TODO use an indexing data structure *)
    Iter.flat_map_l (lifted_inference c) (Env.get_active())
    |> Iter.to_rev_list
  )


(* K,L ⊢ᵧ K,L' ⟹ C∨K, D∨L ⊢ C∨K, D∨L' given γC⊆D and C≺K *)
let add_simplify_in_context(type c)(module Env: Env.S with type C.t=c) name literal_inference =
  let module C = Env.C in
  let open SimplM.Infix in
  let lifted_inference c1 c2 =
    if ~<c1 == ~<c2 then c2,`Same else
    let exception Changed of C.t in
    let rename = Subst.Renaming.create() in
    let c1_lits = C.lits c1 and c2_lits = C.lits c2 in
    (* TODO compute eligible literals only once *)
    (* TODO find largest, not just max *)
    try fold_lits ~eligible:(C.Eligible.max c1) c1_lits (fun(l1,pos1) ->
    fold_lits ~eligible:~= ~=true c2_lits (fun(l2,pos2) ->
      if Trail.subsumes (C.trail c1) (C.trail c2) then
      match literal_inference l1 l2 with
      | Some(simplified, subst) ->
        (* TODO check alternative subsumption *)
        (* let c1_no_l1 = apply_subst_list rename subst (except_idx c1_lits pos1, 0) in *)
        let c2_no_l2 = apply_subst_list rename subst (except_idx c2_lits pos2, 1) in
        raise(Changed(C.create (simplified @ c2_no_l2)
          ~penalty:(C.penalty c2)
          ~trail:(C.trail c2)
          (Proof.Step.simp ~tags:[] ~rule:(Proof.Rule.mk name) (List.map (fun c -> C.proof_parent_subst rename c subst) [c2,1; c1,0]))))
      | None -> ()
    )); c2,`Same
    with Changed c2' -> c2',`New
  in
  (* Note: keep "fun c ->" so that Env.get_active() is recomputed. TODO indexing *)
  let rule = fun c -> SimplM.app_list Iter.(to_rev_list(map lifted_inference (Env.get_active()))) 
   (str(Iter.to_rev_list(Env.get_active()))|<c) in
  Env.add_rw_simplify(rule);
  Env.add_backward_simplify(fun c -> C.ClauseSet.of_iter(Env.get_active()))




module Ore = struct
type power = {base:[`Mono |`Exp of term |`Move of term]; var:int; exp:int}
type monomial =
    (* Example: (¾-2/a) ⬝ m³2ⁿ⁵aⁿM₊ₐ²N₊₁⁴ ⬝ f(m,n)    ( = (¾-2/a) m³ 2⁵ⁿ aⁿ f(m+2a, n+4) )
    The power list is in REVERSE visual writing order as [N₊₁⁴; M₊ₐ²; aⁿ; 2ⁿ⁵; m³]. Everything else follows the visual order. *)
    term * power list * term
type poly = monomial array

let oper_powers(_,m,_) = m
let oper_arg(_,_,f) = f

let varstr n = String.make 1 (Char.chr(122 - n))
let varStr n = String.make 1 (Char.chr(90 - n))

let power_to_string{base;var;exp} = match base with
|`Mono -> varstr var ^ superscript(string_of_int exp)
|`Exp t -> Term.to_string t ^ superscript(varstr var ^ string_of_int exp)
|`Move t -> varStr var ^ superscript(string_of_int exp)

let mono_to_string(c,m,f) = match (match str c with "1"->"" | "-1"->"-" | c'->c') ^ concat_view "" power_to_string (List.rev m) ^ str f with ""->"1" | "-"->"-1" | s->s

let poly_to_string p = Array.(if length p = 0 then "0" else concat_view " + "  mono_to_string (to_list p))


(* Coefficient arithmetic. TODO use general simplification instead of or in addition to special casing ℤ constants. *)

let _Z z = app_builtin ~ty:int (Int z) []
let _z = _Z % Z.of_int

let if_Z fZ f' t' s' = match view t', view s' with
| AppBuiltin(Int t, _), AppBuiltin(Int s, _) -> fZ t s
| _ -> f' t' s'

let (-|-) = if_Z (Z.(+)%>>_Z) (fun t s -> app_builtin ~ty:(ty t) Sum [t;s])
let (><) = if_Z (Z.( * )%>>_Z) (fun t s -> app_builtin ~ty:(ty t) Product [t;s])
let rec (^) t = function 0-> _z 1 | 1-> t | e-> t >< t^(e-1)

let lcm_coefs t' s' = if t'==s' then _z 1, _z 1 else if_Z Z.(fun t s -> let l r = _Z(divexact (lcm t s) r) in l t, l s) (fun t s -> s,t) t' s'


let mono_total_deg = List.fold_left (fun d x -> d + x.exp) 0

(* Use through redefine_elimination_priori. *)
let elimination_priority = ref ~= ~=0

(* Comparison functions for powers, monomials *)

let rank x = (match x.base with
  |`Mono -> 0, _z 0
  |`Exp t -> 1, t
  |`Move t -> 2, t
), x.var

let compare_rank = rank %%> ((-) *** Term.compare) *** (-)

let compare_power = (fun x -> x,x.exp) %%> compare_rank *** (-)

let compare_mono = (fun(c,m,f) -> !elimination_priority m f, (mono_total_deg m, (m, (f, c)))) %%>
  (-) *** (-) *** lex_list compare_power *** Term.compare *** Term.compare

(* Update the given polynomials to follow the given new default elimination priority. Other existing polynomials become garbage and must not be used!
 The elimination priority overrides the default comparison order of monomials (which is a total degree lexicographic one). This can be used to derive equations without certain indeterminate powers or operands by saturation. For example {n²m+m², nm²} with n≺m ... TODO
 Parameters to priority are list of operator powers and operand term. The elimination preordering must extend divisibility: if monomial M=K⬝N then M has ≥ priority than N. Constant priority (default) leaves the tie-breaking total degree ordering unchanged.
 Desing: Having a global switch like this has drawbacks. Namely saturations of polynomial equations cannot be (easily) nested, parallelised or paused-and-resumed. Usual solution would be to substitute the module by a functor taking the elimination priority as parameter. However the global switch is normally simpler to use because operations like superpose and leadrewrite can be repackaged without passing any extra parameters even when the priority needs to vary. *)
let redefine_elimination_priority all_polynomials priority =
  elimination_priority := priority;
  all_polynomials(Array.sort(flip compare_mono))


(* Arithmetic of polynomials etc. Main operations to superpose polynomials are: addition, multiplication by monomial, and pre-lcm of monomials. *)

let _0 = Array.of_list[]

let set_exp x e = if e=0 then [] else [{x with exp=e}]

(* Equality of powers. Differs from plain (=) because of the terms inside. *)
let (=^) m w = m.exp=w.exp && m.var=w.var && match m.base, w.base with
  |`Exp n, `Exp v
  |`Move n, `Move v -> n == v
  | n, v -> n == v (* meaning both =`Mono *)

(* Make a polynomial by summing list of full monomials. *)
let sum_monomials =
  let rec sum_sorted = function
  | (c,n,f)::more when c == _z 0 -> sum_sorted more
  | (c,n,f)::(c',n',f')::more when for_all_2(=^) n n' && f==f' -> sum_sorted((c-|-c', n, f)::more)
  | m::more -> m:: sum_sorted more
  | [] -> []
  in
  Array.of_list % sum_sorted % List.sort(flip compare_mono)

(* polynomial + polynomial *)
let (++) p r = sum_monomials Array.(to_list p @ to_list r)

(* constant × polynomial *)
let ( *:) a = if a == _z 1 then id else if a == _z 0 then ~=_0 else Array.map(fun(c,m,f) -> (a><c, m, f))

(* monomial × polynomial *)
let ( **:) mon =
  let open List in
  (* Multiply (coefficient, monomial, reversed monomial) triplet. Result is a list of monomials. Remember that the lists representing monomials are in reverse visual and rank order. *)
  let rec mul o=o|> flatten % map(function
  | (coef, m), [] -> [coef, m]
  | (coef, []), m_rev -> [coef, rev m_rev]
  | (coef, n::m), v::w_rev -> match compare_rank n v with
    (* merge n, v *)
    | 0 -> [coef, rev w_rev @ set_exp v (n.exp+v.exp) @ m]
    (* right order n < v *)
    | c when c<0 -> [coef, rev w_rev @ [v;n] @ m]
    (* commute n > v *)
    | _ -> let (&) m a = map(fun n -> (n,a)) m in
      match n.base, v.base with
      |`Move n', `Exp v' when n.var=v.var -> mul(mul[(coef >< v' ^ n.exp*v.exp, m), [v;n]] & w_rev)
      |`Move n', `Mono when n.var=v.var -> mul(
        (* Binomial formula: c D₊ₐⁿ dᵛ = c (d+na)ᵛ D₊ₐⁿ = ∑k∈[0,v]: c(ᵛₖ)(na)ᵛ⁻ᵏ dᵏ D₊ₐⁿ *)
        mul(init(v.exp+1)id |> map(fun k ->
          (coef >< _Z Z.(bin (of_int v.exp) k) >< (_z n.exp >< n')^(v.exp-k), m), set_exp v k @[n])
        ) & w_rev)
      | _ -> mul(mul[(coef, m), [v;n]] & w_rev))
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

(* Given monomials m1,m2, find Some f s.t. f⬝m1 ≈ m2—in which case m1 divides m2—upto coefficients and lower order terms. Otherwise None. Satisfies: div_factor m1 m2 = Some f ⟺ lcm_factors m1 m2 = f,[] *)
let rec div_factor m1 m2 = CCOpt.(match m1,m2 with
| [],_ -> Some m2
| _,[] -> None
| x1::n1, x2::n2 -> match compare_rank x1 x2 with
  | r when r>0 -> None
  | 0 -> if_((<=)0) (x2.exp - x1.exp) >>= fun e -> (@)(set_exp x1 e) <$> div_factor n1 n2
  | _ -> CCList.cons x2 <$> div_factor (x1::n1) n2)


let superpose p1 p2 =
  if oper_arg p1.(0) != oper_arg p2.(0) then [] else
  let f1, f2 = (oper_powers %%> lcm_factors) p1.(0) p2.(0) in
  let p'1, p'2 = f1**:p1, f2**:p2 in
  let a1, a2 = ((fun(c,_,_)->c) %%> lcm_coefs) p'1.(0) p'2.(0) in
  [a1*:p'1 ++ (_z(-1)><a2)*:p'2]

(* Try rewrite leading monomial of p by r. *)
let leadrewrite r p =
  if oper_arg r.(0) != oper_arg p.(0) then None else
  match (oper_powers %%> div_factor) r.(0) p.(0) with
  | Some f -> let r' = f**:r in
    let a1,a2 = ((fun(c,_,_)->c) %%> lcm_coefs) p.(0) r'.(0) in
    (* TODO check that a1 is invertible *)
    Some(a1*:p ++ (_z(-1)><a2)*:r')
  | _ -> None



(* quick manual data entry *)
let _p s = let is_low c = String.lowercase c = c in
  let rec go = function
  | v::e::rest -> go rest @ [{exp=int_of_string e; var=122-Char.(code(lowercase v.[0])); base=if is_low v then`Mono else`Move(_z 1)}]
  | _ -> []
  in
  go String.(split_on_char ' ' (trim(concat""[s;" 1"])))
let _P = sum_monomials % List.map(fun(c,s,t)-> (_z c, _p s, have t [] int))

end




module MakeSumSolver(MainEnv: Env.S) = struct
(* module Env = MainEnv *)
module C = MainEnv.C
(* module Ctx = MainEnv.Ctx *)
(* let _= add_pp TypeTests.clause (CCFormat.to_string C.pp_tstp) *)

let polyform, fake_poly_lit =
  (* Set up an automatically garbage collected cache for polyform. *)
  let module Hash_lit_to_poly: Hashtbl.HashedType with type t = Literal.t = struct
    type t = Literal.t 
    let equal =  equal_com
    let h seed op a b = Hash.combine3 seed (Hashtbl.hash op) (a + b)
    let hash =  function
    | Equation(a,b,sign) -> (Term.hash %%> h 1 sign) a b
    | Int(Binary(op,a,b)) -> (Monome.hash %%> h 2 op) a b
    | Rat l -> (Monome.hash %%> h 3 l.op) l.left l.right
    | l -> Literal.hash l
  end in
  let module NoMemoryLeakMap = Hashtbl.Make(Hash_lit_to_poly) in
  let cache = NoMemoryLeakMap.create 0 in
  (* Principle: cache _0 to denote failed conversion when the conversion test was expensive. *)
  (* polyform *)
  Ore.(fun l -> match NoMemoryLeakMap.find_opt cache l with
  | Some p -> CCOpt.if_((<>)_0) p
  | None ->
    (* let result p = NoMemoryLeakMap.add cache (l,p); CCOpt.if_((<>)_0) p in *)
    (* Actual work: test if literal l has polynomial form. *)
    match l with
    | Equation(a,b,true) -> None
    | Int(Binary(Equal,a,b)) -> None
    | Rat{op=Equal; left=a; right=b} -> None (* TODO simplify every Rat to Int *)
    | _ -> None),
  (* fake_poly_lit *)
  fun p -> 
    if p = Ore._0 then mk_tauto else
    let l = mk_eq (const ~ty:int (ID.make(Ore.poly_to_string p))) (Ore._z 0) in
    NoMemoryLeakMap.add cache l p;
    l


let clauseset clauselist = {Clause.c_set= of_list clauselist; c_sos= of_list[]}

let superpose_poly l1 = match polyform l1 with
| None -> ~=[]
| Some p1 ->
  fun l2 -> match polyform l2 with
  | None -> []
  | Some p2 -> List.filter((!=)[mk_tauto] % fst) (List.map(fun p -> [fake_poly_lit p], Subst.empty) (Ore.superpose p1 p2))

let rewrite_poly l1 = match polyform l1 with
| None -> ~=None
| Some p1 ->
  fun l2 -> match polyform l2 with
  | None -> None
  | Some p2 -> CCOpt.map(fun p -> [fake_poly_lit p], Subst.empty) (Ore.leadrewrite p1 p2)


module PolyEnv: Env.S = Env.Make(struct
  module Ctx = MainEnv.Ctx
  (* module C = MainEnv.C *)
  let params = MainEnv.params
  let flex_state = MainEnv.flex_state()
  let export: C.t -> MainEnv.C.t = id

end) (* Cannot be local to below because type C.t = PolyEnv.C.t escapes that scope. *)
let indeterminate_elimination_environment() =
    let env = (module PolyEnv: Env.S with type C.t='Ct) in
    let module C = PolyEnv.C in
    (* on_eligible_literals env "sup. poly." superpose_poly; *)
    add_simplify_in_context env "lead rewrite" rewrite_poly;
    PolyEnv.add_is_trivial ((=)(Array.of_list[mk_tauto]) % C.lits);
    let export: PolyEnv.C.t -> MainEnv.C.t = Obj.magic in (* TODO *)
    env, Iter.map export % PolyEnv.get_clauses, PolyEnv.get_active

let saturate env cc = Phases.(run(
  let (^) label thread = start_phase label >>= return_phase >>= ~=thread in
  Parse_CLI^LoadExtensions^Parse_prelude^Start_file^Parse_file^Typing^CNF^Compute_prec^Compute_ord_select^MakeCtx^MakeEnv^
  Phases_impl.refute_or_saturate env cc >>= fun result ->
  start_phase Exit >>= ~=(return_phase result)))


let step text parents lits =
  C.create lits ~penalty:1 ~trail:(C.trail_l[]) (if parents=[]
  then (if text="goal" then Proof.Step.goal' else Proof.Step.assert') ~file:"" ~name:text ()
  else Proof.Step.inference ~tags:[] ~rule:(Proof.Rule.mk text) (List.map (fun p -> C.proof_parent_subst (Subst.Renaming.create()) (p,0) Subst.empty) parents))
  
let test_hook clause =
  let subenv, get_clauses, get_active = indeterminate_elimination_environment() in
  let eq0' p = PolyEnv.C.create (List.map fake_poly_lit p) ~penalty:1 ~trail:(C.trail_l[]) (Proof.Step.assert' ~file:"" ~name:"" ()) in
  let split_or k d s = match String.split_on_char k s with
  | [a;b] -> a,b | [a] -> if k='.' then d,a else a,d | _ -> raise Exit in
  let eq0 ss = eq0'[Ore._P(List.map(fun cmt ->
    let c,mt = split_or '.' "1" (String.trim cmt) in
    let m,t = split_or '\'' "" mt in
    int_of_string c, m, t
  ) (String.split_on_char '+' ss))] in

  Ore.redefine_elimination_priority Iter.empty String.(fun m f -> 
    match view f with
    | Const f when contains f.name '*' -> 1 
    | Const f when contains f.name '`' -> -1
    | _ -> 0
    (* let e = ref 0 in
    let _= List.map(function {Ore.base=`Mono; var=1; exp} -> e:=exp; () | _->()) m in
    !e *)
  );
  
  let _ = saturate subenv Ore.({Clause.c_set= of_list[
    eq0"z 5"; eq0"z 2 + -1.";
    (* eq0"3.z 19"; eq0"3.z 9 + 1."; *)
    (* eq0"x + y + -1.z"; eq0"x 2 + y 2 + -1.z 2"; *)
    (* eq0"y 2 x + -1.x + -1.y"; eq0"y 1 x 2 + -1.x + -1."; *)
    (* eq0"x 2 + 3.x + 1."; eq0"y 2 + 3.y + 1."; eq0"x 5 + y 5"; *)
    
    (* eq0"2.n 2 N 2 + -1.n 1 N + 3."; eq0"N 3 + 2.n 1 N";  *) 
    (* eq0"2.n 1 N + -1.m + 3."; eq0"N 1 M + 2.m"; *) 
    
    (* eq0"X'S* + -1.'S* + -1.X'f"; eq0"'h + -1.'S* + 'g*";  *)
    (* eq0"X'g* + -1.'g* + -1.X'f"; *)
    (* eq0"-1.X 2'g* + 'g* + X 2'f + X'f"; *)
    
    (* eq0"Y'⬝2ʸ + -2.'⬝2ʸ"; eq0"-4.'g` + y 2'⬝2ʸ + y'⬝2ʸ"; *)
    (* eq0"x 1 X'(ˣᵧ) + -1.y 1 X'(ˣᵧ) + X'(ˣᵧ) + -1.x'(ˣᵧ) + -1.'(ˣᵧ)"; eq0"y 1 Y'(ˣᵧ) + Y'(ˣᵧ) + -1.x'(ˣᵧ) + y'(ˣᵧ)"; eq0"'f` + -1.y 2'(ˣᵧ)"; *)
    (* Change priority for ↓ *)
    (* eq0"x 1 X'f + -1.y 1 X'f + X'f + -1.x'f + -1.'f"; eq0"y 2 Y'f + -1.y 1 x'f + y 2'f + -1.x'f + y'f"; *)

    (* eq0"-1.x 2 X 2'∑f+-2.x 1 X 2'∑f+-1.X 2'∑f + 3.x 2 X'∑f+9.x 1 X'∑f+6.X'∑f + -2.x 2'∑f+-6.x'∑f+-4.'∑f"; eq0"x 1 X'g + -2.x'g + -4.'g"; eq0"'h` + -1.'∑f + 'g"; *)
    eq0'[Ore._0]]; c_sos= of_list[]}) in
  [step "" (Iter.to_rev_list(get_clauses())) []]




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

(* Setup to do when MakeSumSolver(...) is called. *);;
  (* MainEnv.add_unary_inf "demo" demo_proof *)
  MainEnv.add_unary_inf "test" test_hook
end


(* Is this extension enabled? Set by a command line option. *)
let sum_by_recurrences = ref true

(* Define name and setup action required to registration of this extension in libzipperposition_phases.ml *)
let extension ={
  Extensions.default with
  name = "∑";
  env_actions = [fun env -> if !sum_by_recurrences then
    let module E= (val env) in (* Solves error: “The parameter cannot be eliminated in the result type.” *)
    let module I= MakeSumSolver(E) in()];
};;
Options.add_opts[
  "--sum-by-recurrences", Arg.Bool((:=)sum_by_recurrences), " use holonomic sequence methods for sums (∑) in algebras"
]