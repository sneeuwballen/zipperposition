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
    (* TODO use an indexing data structure *)
    Iter.flat_map_l (lifted_inference c) (Env.get_active())
    |> Iter.to_rev_list
  )


(* K,L ⊢ᵧ K,L' ⟹ C∨K, D∨L ⊢ C∨K, D∨L' given γC⊆D and C≺K *)
let add_simplify_in_context
  (type c)(module Env: Env.S with type C.t=c) 
  name
  literal_inference 
  (type i)(module Index: Index_intf.GENERAL_IDX with type element=c and type t=i)
  ?key_to_index
  initial_index
=
  let module C = Env.C in
  let open SimplM.Infix in
  let lifted_inference c1 c2 =
    if c1 == c2 then c2,`Same else
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
  let index_key = CCOpt.get_lazy Flex_state.create_key key_to_index in
  Env.flex_add index_key initial_index;
  let index act c = act (Env.flex_get index_key) c in
  
  Env.add_rw_simplify(fun c -> SimplM.app_list Iter.(to_rev_list(map lifted_inference (index Index.retrieve_generalizations c))) c);
  Env.add_backward_simplify(C.ClauseSet.of_iter % index Index.retrieve_specializations);
  let open Env.ProofState in
  let update_index when' how' = Signal.on_every when' (Env.flex_add index_key % index how') in
  (* update_index ActiveSet.on_add_clause Index.add; *)
  (* update_index ActiveSet.on_remove_clause Index.remove; *)
  update_index SimplSet.on_add_clause Index.add;
  update_index SimplSet.on_remove_clause Index.remove;
    (* (fun i c -> if Iter.exists((==)c) (Env.get_active()) then i else Index.remove i c) *)




module Ore = struct
type power = {base:[`Mono |`Exp of term |`Move of term]; var:int; exp:int}
type monomial =
    (* Example: (¾-2/a) ⬝ m³2ⁿ⁵aⁿM₊ₐ²N₊₁⁴ ⬝ f(m,n)    ( = (¾-2/a) m³ 2⁵ⁿ aⁿ f(m+2a, n+4) )
    The power list is in REVERSE visual writing order as [N₊₁⁴; M₊ₐ²; aⁿ; 2ⁿ⁵; m³]. Everything else follows the visual order. *)
    term * power list * term
type poly = monomial array

let oper_coef(c,_,_) = c
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


(* Coefficient arithmetic: Integer constants are simplified but other simplification—which likely depends on the proof state—is left to the user. *)

let _Z z = app_builtin ~ty:int (Int z) []
let _z = _Z % Z.of_int

let if_Z fZ f' t' s' = match view t', view s' with
| AppBuiltin(Int t, _), AppBuiltin(Int s, _) -> fZ t s
| _ -> f' t' s'

let (-|-) = if_Z (Z.(+)%>>_Z) (fun t s -> app_builtin ~ty:(ty t) Sum [t;s])
let (><) = if_Z (Z.( * )%>>_Z) (fun t s -> app_builtin ~ty:(ty t) Product [t;s])
let rec (^) t = function 0-> _z 1 | 1-> t | e-> t >< t^(e-1)

let (^^) t e = match view e with
| AppBuiltin(Int e', _) -> t ^ Z.to_int e'
| _ -> raise(Failure(Printf.sprintf "Unsupported operation: symbolic exponent %a in coefficient of a polynomial." ~=Term.to_string e))

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
  (-) *** (-) *** flip(lex_list compare_power) *** Term.compare *** Term.compare

(* Update the given polynomials to follow the given new default elimination priority. Other existing polynomials become garbage and must not be used!
 The elimination priority overrides the default comparison order of monomials (which is a total degree lexicographic one). This can be used to derive equations without certain indeterminate powers or operands by saturation. For example {n²m+m², nm²} with n≺m ... TODO
 Parameters to priority are list of operator powers and operand term. The elimination preordering must extend divisibility: if monomial M=K⬝N then M has ≥ priority than N. Constant priority (default) leaves the tie-breaking total degree ordering unchanged.
 Desing: Having a global switch like this has drawbacks. Namely saturations of polynomial equations cannot be (easily) nested, parallelised or paused-and-resumed. Usual solution would be to substitute the module by a functor taking the elimination priority as parameter. However the global switch is normally simpler to use because operations like superpose and leadrewrite can be repackaged without passing any extra parameters even when the priority needs to vary. *)
let redefine_elimination_priority all_polynomials priority =
  elimination_priority := priority;
  all_polynomials(Array.sort(flip compare_mono))

(* An elimination priority function. For example: elim_oper_args[t,2; s,1; r,1] to eliminate terms t,s,r with priority among them in t. *)
let elim_oper_args weights = ~=Hashtbl.(CCOpt.get_or ~default:0 % find_opt(of_seq(List.to_seq weights)))

(* An elimination priority to indeterminates assigned by the given weight function. Example use: elim_indeterminate(function{base=`Mono;var=2}->1|_->0). Ignore the exp field of the tested indeterminate. *)
let rec elim_indeterminate weight m _ = List.fold_left (fun pr x -> pr + weight x * x.exp) 0 m

(* Arithmetic of polynomials etc. Main operations to superpose polynomials are: addition, multiplication by monomial, and lcm of monomials upto leading term. *)

let _0 = [||]

let one t = [|_z 1, [], t|]

let set_exp x e = if e=0 then [] else [{x with exp=e}]

(* Very specific auxiliarity: modify non-zero polynomial's leading monomial to 0. *)
let lead_0 p = Array.set p 0 (_z 0, [], _z 0); p

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
let ( *:) a = Array.map(fun(c,m,f) -> (a><c, m, f))

(* polynomial - polynomial *)
let (--) p r = p ++ _z(-1)*:r

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
      |`Move n', `Exp v' when n.var=v.var -> mul(mul[(coef >< (v'^^n') ^ n.exp*v.exp, m), [v;n]] & w_rev)
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


(* Calculate superposition between two polynomials pⱼ (j=1,2) that represent equations m⬝pⱼ=0 where m runs over all monomials. A result is m₁p₁ - m₂p₂ where mⱼ are least monomials such that leading terms in mⱼpⱼ equal. Leading monomials cancel but others are not simplified. Currently result is unique, if superposition is possible. *)
let superpose p1 p2 =
  if p1=_0 or p2=_0 or oper_arg p1.(0) != oper_arg p2.(0) then [] else
  let f1, f2 = (oper_powers %%> lcm_factors) p1.(0) p2.(0) in
  let p'1, p'2 = f1**:p1, f2**:p2 in
  let a1, a2 = (oper_coef %%> lcm_coefs) p'1.(0) p'2.(0) in
  [lead_0(a1*:p'1) -- lead_0(a2*:p'2)]

(* Try rewrite leading monomial of p by r. *)
let leadrewrite r p =
  if r=_0 or p=_0 or oper_arg r.(0) != oper_arg p.(0) then None else
  match (oper_powers %%> div_factor) r.(0) p.(0) with
  | Some f -> let r' = f**:r in
    let a1,a2 = (oper_coef %%> lcm_coefs) p.(0) r'.(0) in
    (* TODO check that a1 is invertible *)
    Some(lead_0(a1*:p) -- lead_0(a2*:r'))
  | _ -> None


(* Find r,q s.t. p = (x-a)q + r when given number constant a, indeterminate x, and polynomial p whose indeterminates commute with x. Note that r is "p evaluated at x=a". *)
let rec left_divide_by_deg1_monic a x p = 
  if p=_0 then _0,_0 else
  match div_factor (oper_powers p.(0)) [x] with
  | Some f ->
    let r,q' = left_divide_by_deg1_monic a x (p -- f**:[|_z 1, [x], oper_arg p.(0); a, [], oper_arg p.(0)|]) in
    r, q'++f**:[|_z 1, [], oper_arg p.(0)|]
  | None -> p,_0

module type View = sig type t type v val view: t -> v option end
(* Create index from mapping clause→polynomial, and instantiate by empty_with' default_features. *)
module LeadRewriteIndex(P: View with type v=poly) = FV_tree.FV_IDX(struct
  type t = P.t
  let compare = P.view %%> Stdlib.compare (* just for usage in sets *)
  type feature_func = poly -> int
  let compute_feature f p = match P.view p with Some p when p!=_0 -> Some(FV_tree.N(f p)) | _->None
end)

(* Features for a rewriting index testing various sums of operator degrees. Only for ≠0 polynomials. *)
let default_features =
  let sum value p = List.(fold_left (+) 0 (map value (oper_powers p.(0)))) in
  [
    ("total degree", fun p -> mono_total_deg(oper_powers p.(0)));
    "shift degree", sum(function {base=`Move _; exp} -> exp | _ -> 0);
    "coefficient degree", sum(function {base=`Mono; exp} -> exp | _ -> 0);
    "exponential degree", sum(function {base=`Exp _; exp} -> exp | _ -> 0);
    "var. 1 degree", sum(function {var=1; exp} -> exp | _ -> 0);
    (* then: multiset of indeterminates... except that only ID multisets are supported *)
  ]


(* Embedding polynomials to terms and literals. Does not preserve equality. *)

let term0 = const ~ty:term (ID.make "𝟬")

exception RepresentingPolynomial of poly

let poly_as_lit_term_id ?name p =
  (* TODO Control how ordering treats the term. *)
  let id = ID.make(CCOpt.get_lazy(fun()-> poly_to_string p) name) in
  ID.set_payload id (RepresentingPolynomial p);
  let term = const ~ty:term id in
  mk_eq term0 term, term, id

let poly_of_term t = match view t with Const id -> ID.payload_find ~f:(function RepresentingPolynomial p -> Some p |_->None) id |_->None

let poly_of_lit = function Equation(t,p,true) when t==term0 -> poly_of_term p |_->None


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
module C = MainEnv.C
(* module Ctx = MainEnv.Ctx *)

let polyliteral p = if p = Ore._0 then mk_tauto else (fun(l,_,_)->l) (Ore.poly_as_lit_term_id p)

let definitional_poly_clause p = C.create [polyliteral p] ~penalty:1 ~trail:(C.trail_l[]) Proof.Step.trivial

let polys_in = Iter.(concat % map(filter_map Ore.poly_of_lit % of_array % C.lits))

let polys_of_2_lits no yes l1 = match Ore.poly_of_lit l1 with
| None -> ~=no
| Some p1 ->
  fun l2 -> match Ore.poly_of_lit l2 with
  | None -> no
  | Some p2 -> yes p1 p2

let superpose_poly = polys_of_2_lits [] (Ore.superpose %>> List.map(fun p -> [polyliteral p], Subst.empty))

let rewrite_poly = polys_of_2_lits None (Ore.leadrewrite %>> CCOpt.map(fun p -> [polyliteral p], Subst.empty))


let saturate_in env cc = Phases.(run(
  let (^) label thread = start_phase label >>= return_phase >>= ~=thread in
  Parse_CLI^LoadExtensions^Parse_prelude^Start_file^Parse_file^Typing^CNF^Compute_prec^Compute_ord_select^MakeCtx^MakeEnv^
  Phases_impl.refute_or_saturate env {Clause.c_set= of_list cc; c_sos= of_list []}
  >>= fun result -> start_phase Exit >>= ~=(return_phase result)))


let make_polynomial_environment() =
  let module PolyEnv = (val (module Env.Make(struct
      module Ctx = MainEnv.Ctx
      let params = MainEnv.params
      let flex_state = MainEnv.flex_state()
    end) : Env.S)
  (* Env.Make assigns the clause module Env.C as a function of Ctx only. Hence it is safe to cast the clauses of PolyEnv to share the type of the clauses of MainEnv. This in turn is necessary to initialize PolyEnv with clauses from MainEnv. *)
  |> Obj.magic : Env.S with type C.t = MainEnv.C.t)
  in
  let env = (module PolyEnv: Env.S with type C.t = MainEnv.C.t) in
  
  let module LRI = Ore.LeadRewriteIndex(struct type t=C.t type v=Ore.poly
    let view c = match Literals.maxlits_l ~ord:(PolyEnv.ord()) (C.lits c) with
    | [largest,_] -> Ore.poly_of_lit largest
    | _ -> None
  end) in
  add_simplify_in_context env "lead rewrite" rewrite_poly 
    (module LRI) (LRI.empty_with' Ore.default_features);
  PolyEnv.add_is_trivial (Array.mem mk_tauto % C.lits);
  env,
  fun clauses -> match saturate_in env (Iter.to_rev_list clauses) with
    | Error e -> raise(Failure e)
    | Ok(_, Unsat _) -> PolyEnv.C.ClauseSet.to_iter(PolyEnv.get_empty_clauses())
    | _ -> PolyEnv.get_clauses()


(* Filter eligible non-trivial polynomial recurrences by the given condition. *)
let filter_recurrences ok_poly = Iter.filter(fun c ->
  try fold_lits ~eligible:(C.Eligible.res c) (C.lits c) Ore.(fun(l,_) ->
    match poly_of_lit l with
    | Some p -> if p!=_0 && ok_poly p then raise Exit
    | _ -> ());
  false
  with Exit -> true)

(* Filter those clauses that have eligible polynomial with leading term M t where some monomial M operates on an OK term t. *)
let filter_recurrences_of ok_terms = filter_recurrences(fun p -> ok_terms(Ore.oper_arg p.(0)))


let propagate_rec_plus t s ?(sum= app_builtin ~ty:(ty t) Sum [t;s]) =
  let rec_t_s = filter_recurrences_of (fun r -> r==t or r==s) (MainEnv.get_clauses()) in
  let env, saturate = make_polynomial_environment() in
  on_eligible_literals env "sup. poly." superpose_poly;
  (* TODO set exact inferences on env *)
  Ore.redefine_elimination_priority (polys_in rec_t_s) (Ore.elim_oper_args[t,2; s,2; sum,1]);
  Iter.cons (definitional_poly_clause Ore.(one sum -- one t -- one s)) rec_t_s
  |> saturate
  |> filter_recurrences_of((==)sum)
  |> MainEnv.add_passive

let propagate_rec_times t s ?(product= app_builtin ~ty:(ty t) Product [t;s]) =
  let rec_t_s = filter_recurrences_of (fun r -> r==t or r==s) (MainEnv.get_clauses()) in
  (* Iter.cons (definitional_poly_clause(one product --  *)
  (* Computing Krull dimension: Max size of a set S of indeterminates s.t. following. A leading element of basis is interprated as a set of its indeterminates A. Demand A⊈S for all such A. *)
  ()

let propagate_rec_sum sum_index t ~sum =
  let rec_t = filter_recurrences_of ((==)t) (MainEnv.get_clauses()) in
  let env, saturate = make_polynomial_environment() in
  (* TODO also eliminate non-near-bijective `Move *)
  Ore.redefine_elimination_priority (polys_in rec_t) (Ore.elim_indeterminate(function
    | {base=`Move _} -> 0
    | x -> if x.var = sum_index then 1 else 0));
  let prerec_sum = saturate rec_t 
  |> filter_recurrences(Array.for_all(fun(_,m,_) -> m |> List.for_all(function
    | {Ore.base=`Move _} -> true
    | x -> x.var != sum_index)))
  in
  (* TODO Transform all legal recurrences or just the one but not those which are not considered by the above filtering. That means extracting from a clause and then extracting recurrence's terms which operate to the ∑. *)
  (* let rec_sum = Iter.map Ore.(left_divide_by_deg1_monic (_z 1) {base=`Move(_z 1); var=sum_index; exp=1}) prerec_sum in *)
  ()
  (* Steps:
  saturate the summed-over variable away
  apply near-bijective transformations (X ↦ 1) —needs division by (X-1)
  apply near-commutation transformations (∑Pf → P∑f)
   *)
  
let step text parents lits =
  C.create lits ~penalty:1 ~trail:(C.trail_l[]) (if parents=[]
  then (if text="goal" then Proof.Step.goal' else Proof.Step.assert') ~file:"" ~name:text ()
  else Proof.Step.inference ~tags:[] ~rule:(Proof.Rule.mk text) (List.map (fun p -> C.proof_parent_subst (Subst.Renaming.create()) (p,0) Subst.empty) parents))
  
let test_hook clause =
  let subenv, saturate = make_polynomial_environment() in
  let eq0' p = C.create (List.map polyliteral p) ~penalty:1 ~trail:(C.trail_l[]) (Proof.Step.assert' ~file:"" ~name:"" ()) in
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
  
  let _ = saturate_in subenv Ore.([
    (* eq0"z 5"; eq0"z 2 + -1."; *)
    (* eq0"3.z 19"; eq0"3.z 9 + 1."; *)
    (* eq0"x + y + -1.z"; eq0"x 2 + y 2 + -1.z 2"; *)
    (* eq0"y 2 x + -1.x + -1.y"; eq0"y 1 x 2 + -1.x + -1."; *)
    (* eq0"x 2 + 3.x + 1."; eq0"y 2 + 3.y + 1."; eq0"x 5 + y 5"; *)
    
    (* eq0"2.n 1 N + -1.m + 3."; eq0"N 1 M + 2.m";  *)
    (* eq0"2.n 2 N 2 + -1.n 1 N + 3."; eq0"N 3 + 2.n 1 N";   *)
    
    (* eq0"X'S* + -1.'S* + -1.X'f"; eq0"'h + -1.'S* + 'g*";  *)
    (* eq0"X'g* + -1.'g* + -1.X'f"; *)
    (* eq0"-1.X 2'g* + 'g* + X 2'f + X'f"; *)
    
    (* eq0"Y'⬝2ʸ + -2.'⬝2ʸ"; eq0"-4.'g` + y 2'⬝2ʸ + y'⬝2ʸ"; *)
    (* with degREVlex below got 50× slower to ≈1s *)
    eq0"x 1 X'(ˣᵧ) + -1.y 1 X'(ˣᵧ) + X'(ˣᵧ) + -1.x'(ˣᵧ) + -1.'(ˣᵧ)"; eq0"y 1 Y'(ˣᵧ) + Y'(ˣᵧ) + -1.x'(ˣᵧ) + y'(ˣᵧ)"; eq0"'f` + -1.y 2'(ˣᵧ)";
    (* Change priority for ↓ *)
    (* eq0"x 1 X'f + -1.y 1 X'f + X'f + -1.x'f + -1.'f"; eq0"y 2 Y'f + -1.y 1 x'f + y 2'f + -1.x'f + y'f"; *)

    (* eq0"-1.x 2 X 2'∑f+-2.x 1 X 2'∑f+-1.X 2'∑f + 3.x 2 X'∑f+9.x 1 X'∑f+6.X'∑f + -2.x 2'∑f+-6.x'∑f+-4.'∑f"; eq0"x 1 X'g + -2.x'g + -4.'g"; eq0"'h` + -1.'∑f + 'g"; *)
    eq0'[Ore._0]] |> CCList.remove_at_idx(-1)) in
  let module SubEnv = (val subenv) in
  [step "" (Iter.to_rev_list(SubEnv.get_clauses())) []]




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