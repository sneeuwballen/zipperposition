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

let for_all_2 p x y = List.(length x = length y && for_all2 p x y)


(* make constants for debugging *)
let constants = Hashtbl.create 0
let have ?(infix=false) name par ty = match Hashtbl.find_opt constants name with
| None ->
  let i = ID.make name in
  if infix then ID.set_payload i (ID.Attr_infix name);
  let c = const (arrow par ty) i in
  Hashtbl.add constants name c; c
| Some c -> c



(* debug print *)
module Debug = struct
open List
open Obj
type any (* Typing technicality: Dynamic tests lead to many free type variables which are problematic with the value restriction of OCaml. Hence there's "any" to replace some, and all type tests take an explicit parameter. *)

(* A dynamic type test is a predicate 'a->bool. Since types are erased, same data can have multiple types. But if a type test T on data X is succesful, then at least X can be (magic) cast to T safely. The below tests are accure in this sense, or preserve accuracy, except the unsafe tests integer/Z.t, rational/Q.t and lazy_or. There's no test combinators for polymorphic variants and exceptions—they can be safely and accurately tested by pattern matching. *)
module TypeTests = struct
(* helpers *)
let fields x = init (size(repr x)) (magic(field(repr x)))

let test (c:int) f x = let x = magic x in if is_int x then 0<=x && x<c else f(tag x, fields x)

let test_tag tag f = test 0 (fun(tag',data) -> tag=tag' && f data)


(* Type tests for builtin types and type constructs *)

let any _ = true (* especially exceptions at the moment *)
let int x = is_int(repr x)

(* Test an algebraic data type: c is the number of constant constructors, and ttt is a list of lists of type tests for the parameters of other constructors, in order of appearence. *)
let union c ttt = test c (fun(tag,data) -> tag < length ttt && for_all_2 id (nth ttt tag) data)
let enum c = union c []

let rec list t x = union 1 [[t; list t]] x
(* Warning: using opt(tuple[...]) instead of union 1 [[...]] is a hideous error! *)
let opt t = union 1 [[t]]

(* Test flat tuples and records: tt is a list of type tests for the components, in order of appearence. *)
let tuple tt = test_tag 0 (for_all_2 id tt)
let array t = test_tag 0 (for_all t)
let string x = test_tag string_tag any x
let func x = test_tag closure_tag any x
let custom x = test_tag custom_tag any x (* e.g. int32, big Z.t *)
(* Use lazy_force unless there's values that cannot be eagerly computed or you never need the lazy value. The lazy_or does not check the type of an unevaluated expression.
Reference: https://stackoverflow.com/questions/56746374/how-does-ocaml-represent-lazy-values-at-runtime *)
let lazy_aid f t x = test_tag lazy_tag f x or test_tag forward_tag (t % hd) x or t x
let lazy_or t = lazy_aid any t
let lazy_force t x = lazy_aid (fun _-> t(Lazy.force(magic x))) t x
(* First field of an object would be “class” and second an “id”. E.g. modules might be objects in addition to being at least tuples and closures.
Open questions: Is class always string? Is id always int? Is there ever more fields? Which OCaml concepts translate to objects? *)
let object0 x = test_tag object_tag (for_all_2 id [string;int]) x
let exception' x = object0 x or test_tag 0 (object0 % hd) x

(* Common non-primitive types *)

(* Test for the default Map of OCaml which as a type is also the same as CCMap. *)
let rec ccmap key t x = union 1 [[ccmap key t; key; t; ccmap key t; int]] x
(* Test for the default Set of OCaml which as a type is also the same as CCSet. *)
let rec ccset t x = union 1 [[ccset t; t; ccset t; int]] x
let ccbv x = tuple[array int; int] x

(* arbitrary precision types—unsafe! *)
let integer x = int x or custom x
let rational x = tuple[integer; integer] x

(* Types specific to Zipperposition *)

let builtin x = union 57 [[integer]; [rational]; [int]] x
let an_id x = tuple[int; string; list exception'] x
let hvar t = tuple[int;t]
let rec term x = tuple[view; opt term; int; exception'; custom; lazy_force int] x
and view x = union 0 [
  [hvar term];
  [int];
  [enum 4; term; term];
  [an_id];
  [term; list term];
  [builtin; list term]] x
let tvar x = hvar term x

let num_class t = tuple[
  term;
  func;func;func;func;func;func;
  t;t;
  func;func;func;func;func;func;func;func;func]
let monome t = tuple[num_class t; t; list(tuple[t;term])]
let int_literal x = union 0 [
  [enum 4; monome integer; monome integer];
  [tuple[integer; int; monome integer; enum 2]]] x
let rat_literal x = tuple[enum 2; monome rational; monome rational] x
let literal x = union 2 [[term; term; enum 2]; [int_literal]; [rat_literal]] x
let literals x = array literal x

let theory_tag x = union 11 [[an_id]] x
let parse_location x = tuple[string; int; int; int; int] x
let from_file x = tuple[string; opt string; opt parse_location] x
let rec attrs x = list(union 0 [[string; attrs]; [string]; [attrs]]) x
let source x = tuple[int; union 0 [[from_file; attrs]; [attrs]]] x
let kind x = union 1 [
  [source; enum 5];
  [string; list theory_tag];
  [string; list theory_tag];
  [string];
  [an_id; source];
  [an_id]] x

let scoped t = tuple[t;int]
let subst x = ccmap (scoped tvar) (scoped term) x
let renaming x = union 1 [[ccmap (scoped tvar) tvar; int]] x
let subst_projection x = tuple[int; subst; renaming] x
let inf_result x = tuple[tuple[
  int; func;func;func; union 2 []; func;func;func;func; opt func; func
]; exception'] x

let rec proof_step x = tuple[int; kind; opt int; int; list parent; attrs] x
and parent x = union 0 [[proof]; [proof; subst_projection]] x
and proof x = tuple[proof_step; inf_result] x

let rec constructor x = tuple[an_id; term; list(tuple[term; projector])] x
and projector x = tuple[an_id; term; int; lazy_force constructor] x
let ind_type x = tuple[an_id; list tvar; term; list constructor; lazy_force(enum 2); proof] x
let ind_cst x = tuple[an_id; list term; term; ind_type; enum 2; int] x
let inductive_case x = tuple[ind_cst; term; (function`Base|`Rec->true|_->false); list ind_cst; list(tuple[an_id;term])] x

let rec position x = union 1 [[position]; [position]; [position]; [position]; [int;position]; [position]] x
let cut_form x = tuple[ccset tvar; list literals] x
let payload x = union 1 [[literals]; [cut_form]; [list inductive_case]] x
let trail x = ccset(tuple[int; payload; any(*TODO What should this be? See bool_lit.ml line 20*)]) x
let sclause x = tuple[int; literals; trail; int] x
let clause x = tuple[
  sclause;
  int; 
  lazy_force ccbv;
  lazy_force(list(tuple[term; position]));
  lazy_force(list int);
  proof_step;
  opt ccbv;
  opt(ccset(tuple[term; position]))] x

let szs_status x = union 3 [[proof]; [string]] x
let or_error t = union 0 [[t]; [string]]
let exn_pair x = tuple[tuple[object0;int]; exception'] x
let flex_state x = ccmap int exn_pair x
let run's_result x = or_error(tuple[flex_state; szs_status]) x

let p_base = function `Mono -> true | `Exp t |`Move t -> term t | _-> false
let power x = tuple[p_base;int;int] x
let monomial x = tuple[term; list power; term] x
let polynomial x = array monomial x
end
open TypeTests


let concat_view separator view = String.concat separator % map view

let superscript_table = Array.of_list(String.split_on_char ' '
"⁽ ⁾ * ⁺ , ⁻ ᐧ ᐟ ⁰ ¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ : ; ᑉ ⁼ > ˀ @ ᴬ ᴮ ᕪ ᴰ ᴱ ᣘ ᴳ ᴴ ᴵ ᴶ ᴷ ᴸ ᴹ ᴺ ᴼ ᴾ ᶲ ᴿ ᔆ ᵀ ᵁ ⱽ ᵂ ᕁ ˠ ᙆ [ ᐠ ] ᣔ ᗮ ` ᵃ ᵇ ᶜ ᵈ ᵉ ᶠ ᵍ ʰ ⁱ ʲ ᵏ ˡ ᵐ ⁿ ᵒ ᵖ ᵠ ʳ ˢ ᵗ ᵘ ᵛ ʷ ˣ ʸ ᶻ")
(* Raise all characters of a string to superscripts. Error if impossible; see the above list. *)
let superscript = concat_view "" (fun c -> superscript_table.(Char.code c - 40)) % of_seq % String.to_seq

(* Print message msg followed by FILE line LINE of the caller's caller. This is based on stack traces, and unfortunately the relevant call site might not show up as some times happens with anonymous functions. *)
let print_with_caller msg =
  let open String in
  (* get_callstack 3 can contain >3 frames due to inlining *)
  let frame = map(function '\\'->'/' | c->c) (nth (split_on_char '\n' Printexc.(raw_backtrace_to_string(get_callstack 3))) 2) in
  (* s = [_ ^ c1 ^]? between c1 c2 s ^ c2 ^ _ searched backwards *)
  let between c1 c2 s =
    match rindex_opt s c2 with None -> "??"(* no stack trace *)  | Some j2 ->
    let j1 = match rindex_from_opt s (j2-1) c1 with None -> 0 | Some j -> j+1 in
    sub s j1 (j2-j1)
  in
  print_endline(msg ^ "\t"
    ^ between '/' '.' (between '"' '"' frame) ^ between ',' ',' frame
    ^ if length msg < 55(*rough*) then "" else "\n")


(* Registering and using adhoc polymorphic pretty printers *)

let string_printers: ((any -> bool) * (any -> string)) list ref = ref[]

let add_pp type_test to_string = string_printers := (type_test, to_string % magic) :: !string_printers

let str x =
  let exception Result of string in
  try magic!string_printers |> iter(fun(type_test, to_string) ->
    if int x then raise(Result(string_of_int(magic x))); (* prioritize *)
    if type_test x then raise(Result(to_string x)));
  Batteries.dump x
  with Result s -> s

(* Sprinkle these in front of expressions you want to trace—often without rebracketing! *)
let (~<)x = print_with_caller(str x); x
let (|<) info x = print_with_caller(info ^" "^ str x); x;;


(* Overly general assignments first so they end up to the bottom of the printer stack. *)
add_pp (test 0 (fun(tag,data) -> tag < no_scan_tag && not(mem tag [lazy_tag; closure_tag; infix_tag])))
  (fun x -> 
    superscript(str(size x)) (* Prepend length of data tuple for easier exploration. *)
    ^ (match tag x with 0-> "" | t-> "tag" ^ str t) (* Omit multipurpose default tag 0. *)
    ^ "("^ concat_view ", " str (fields x) ^")");

add_pp (list any) (fun l -> "["^ concat_view "; " str l ^"]");

add_pp (ccset any) (
  let rec to_list s = if s == repr 0 then [] else to_list(field s 0) @ field s 1 :: to_list(field s 2) in
  fun s -> "{"^ concat_view ", " str (to_list s) ^"}");

add_pp (test_tag forward_tag any) (str % Lazy.force);

add_pp run's_result (function
| CCResult.Error info -> "🚫 "^info
| CCResult.Ok(state, res) ->
  Saturate.(match res with
    | Unsat proof -> Proof.S.pp_in Options.O_none Format.std_formatter proof; "\n"
    | Sat -> "Satisfiable"
    | Unknown -> "Unknown"
    | Error info -> "OK but "^info
    | Timeout -> "Timeout"
  )^" – with flex_state {"^( (* Don't know if useful but print flex_state anyway. *)
    CCHet.Map.to_list %> concat_view "; " (fun(CCHet.Pair(k,e)) -> str e)
  ) state ^"}");

add_pp exception' Printexc.(fun e ->
  exn_slot_name e ^"#"^ str(exn_slot_id e) ^ if tuple[any;any] e then
    "("^ concat_view ", " str (tl(fields e)) ^")"
  else "");

add_pp string (fun s -> if String.trim s = "" or int_of_string_opt s != None then "“"^s^"”" else s);
add_pp subst Subst.to_string;
add_pp term Term.to_string;
add_pp literal Literal.to_string;
(* add_pp integer Z.to_string; *)
(* add_pp rational Q.to_string; *)

end
open Debug




(* Given an inference L¹,L²⊢σC, create and put into use an inference  L¹∨D¹, L²∨D² ⊢ σ(C∨D¹∨D²) , where literals L¹ and L² must be eligible. Allow multiple conclusions. *)
let on_eligible_literals(type c)(module Env: Env.S with type C.t=c) name literal_inference =
  let module C = Env.C in
  let lifted_inference c1 c2 =
    let rename = Subst.Renaming.create() in
    let c1_lits = C.lits c1 and c2_lits = C.lits c2 in
    (* TODO compute eligible literals only once *)
    fold_lits ~eligible:(C.Eligible.res c1) c1_lits |> Iter.flat_map(fun(l1,pos1) ->
    fold_lits ~eligible:(C.Eligible.res c2) c2_lits |> Iter.flat_map_l(fun(l2,pos2) ->
      "OUT"|< literal_inference l1 l2 |> List.map(fun(infered, subst) ->
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
let add_simplify_in_context(type c)(module Env: Env.S with type C.t=c) name literal_inference =
  let module C = Env.C in
  let open SimplM.Infix in
  let lifted_inference c1 c2 =
    if c1 == c2 then (c2,`Same) else
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
    )); (c2,`Same)
    with Changed c2' -> ~<(c2',`New)
  in
  (* Note: keep "fun c ->" so that Env.get_active() is recomputed. TODO indexing *)
  Env.add_rw_simplify(fun c -> SimplM.app_list Iter.(to_rev_list(map lifted_inference (Env.get_active()))) c);
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

let pp_power{base;var;exp} = match base with
|`Mono -> varstr var ^ superscript(string_of_int exp)
|`Exp t -> Term.to_string t ^ superscript(varstr var ^ string_of_int exp)
|`Move t -> varStr var ^ superscript(string_of_int exp)

let pp_mono(c,m,f) = match (match str c with "1"->"" | "-1"->"-" | c'->c') ^ concat_view "" pp_power (List.rev m) ^ str f with ""->"1" | "-"->"-1" | s->s

let pp p = Array.(if length p = 0 then "0" else concat_view " + "  pp_mono (to_list p))

let _= let open TypeTests in
add_pp power pp_power;
add_pp monomial pp_mono;
add_pp polynomial pp

let _Z z = app_builtin ~ty:int (Int z) []
let _z = _Z % Z.of_int


(* Coefficient arithmetic. TODO use general simplification instead of or in addition to special casing ℤ constants. *)

let if_Z fZ f' t s = match view t, view s with
| AppBuiltin(Int t', _), AppBuiltin(Int s', _) -> fZ t' s'
| _ -> f' t s

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
  if oper_arg p1.(0) != oper_arg p2.(0) then [] else[
  let f1, f2 = (oper_powers %%> lcm_factors) p1.(0) p2.(0) in
  let p'1, p'2 = f1**:p1, f2**:p2 in
  let a1, a2 = ((fun(c,_,_)->c) %%> lcm_coefs) p'1.(0) p'2.(0) in
  a1*:p'1 ++ (_z(-1)><a2)*:p'2]

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
let _= add_pp TypeTests.clause (CCFormat.to_string C.pp_tstp)

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
    let l = mk_eq (const ~ty:int (ID.make(Ore.pp p))) (Ore._z 0) in
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
    on_eligible_literals env "sup. poly." superpose_poly;
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
    (* eq0"3.z 19"; eq0"3.z 9 + 1."; *)
    (* eq0"x + y + -1.z"; eq0"x 2 + y 2 + -1.z 2"; *)
    (* eq0"y 2 x + -1.x + -1.y"; eq0"y 1 x 2 + -1.x + -1."; *)
    (* eq0"x 2 + 3.x + 1."; eq0"y 2 + 3.y + 1."; eq0"x 5 + y 5"; *)
    
    (* eq0"2.n 2 N 2 + -1.n 1 N + 3."; eq0"N 3 + 2.n 1 N";  *)
    (* eq0"2.n 1 N + -1.m + 3."; eq0"N 1 M + 2.m"; *)
    
    eq0"X'S* + -1.'S* + -1.X'f"; eq0"'h + -1.'S* + 'g*";
    eq0"X'g* + -1.'g* + -1.X'f"; 
    (* eq0"-1.X 2'g* + 'g* + X 2'f + X'f"; *)
    
    (* eq0"x 1 X'(ˣᵧ) + -1.y 1 X'(ˣᵧ) + X'(ˣᵧ) + -1.x'(ˣᵧ) + -1.'(ˣᵧ)"; eq0"y 1 Y'(ˣᵧ) + Y'(ˣᵧ) + -1.x'(ˣᵧ) + y'(ˣᵧ)"; eq0"'f` + -1.y 2'(ˣᵧ)"; *)
    (* eq0"Y'⬝2ʸ + -2.'⬝2ʸ"; eq0"-4.'g` + y 2'⬝2ʸ + y'⬝2ʸ"; *)
    (* Change priority for ↓ *)
    (* eq0"x 1 X'f + -1.y 1 X'f + X'f + -1.x'f + -1.'f"; eq0"y 2 Y'f + -1.y 1 x'f + y 2'f + -1.x'f + y'f"; *)
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
  (* MainEnv.add_binary_inf "poly. sup." poly_sup; *)
  (* MainEnv.add_unary_inf "demo" demo_proof *)
  MainEnv.add_unary_inf "test" test_hook
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
