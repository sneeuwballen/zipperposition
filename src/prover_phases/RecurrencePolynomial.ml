open Logtk
open Interfaces
open Str
open Char
open Util
open UntypedPrint
open Precedence.Weight
open Literal
open Type
open Term
open CCPair
open CCCache
open CCOpt
open CCVector
open CCArray
open CCList
open CCFun
open Stdlib

(* Utilities *)
let todo msg = print_string("TO‍‍D‍O "^msg^"\t"); assert false
let (~=) x _ = x
let (@@) = map_same
let (%%>) = compose_binop
let (%>>) f g x y = g(f x y)
let (<:>) r f = r:=f!r
let is0 = Z.(equal zero)
(* Lexicographic product of comparison functions onto tuples. *)
let ( *** ) c1 c2 (x1,x2) (y1,y2) = match c1 x1 y1 with 0 -> c2 x2 y2 | r -> r
(* Example: lex_list(-) [1;4;2] [1;3;3;3] > 0 where (-) compares positive ints without overflow. *)
let rec lex_list c = curry(function
| [], [] -> 0
| [], _ -> -1
| _, [] -> 1
| x::xx, y::yy -> (c *** lex_list c) (x,xx) (y,yy))
let length_lex_list c = (fun l -> length l, l) %%> (-) *** lex_list c
let lex_array c = to_list %%> lex_list c
let length_lex_array c = (fun l -> Array.length l, l) %%> (-) *** lex_array c

let fold_left_1 f = function x::l -> fold_left f x l | _->raise(Invalid_argument "fold_left_1: empty list")
let max_list ?(ord=compare) = fold_left_1(fun x y -> if ord x y < 0 then y else x) (* use e.g. max_list(⊥ :: ...) *)
let min_list ?(ord=compare) = fold_left_1(fun x y -> if ord x y > 0 then y else x) (* use e.g. min_list(⊤ :: ...) *)
let max_array ?(ord=compare) ?bottom l = max_list ~ord (CCOpt.to_list bottom @ to_list l)
let min_array ?(ord=compare) ?top l = min_list ~ord (CCOpt.to_list top @ to_list l)
let sum_list = fold_left (+) 0
let sum_array = Array.fold_left (+) 0
let index_of p l = fst(get_exn(find_idx p l))
let index_of' a = index_of((=)a)
let with_cache_2 c f = curry(with_cache_rec c (uncurry % f % curry))
let with_cache_3 c f = curry(with_cache_2 c (uncurry % f % curry))

let to_formatter: ('x->string) -> 'x CCFormat.printer
  = fun to_string out x -> Format.fprintf out "%s" (to_string x)
let string_part_at ?(split=' ') = get % Array.of_list % String.split_on_char split
let subscript = String.to_seq %> List.of_seq %> concat_view "" (fun c -> match c with
  | '('->"₍" | ')'->"₎" | '+'->"₊" | '-'->"₋" | '='->"₌"
  | '0'..'9' -> string_part_at "₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉" (code c - code '0')
  | 'a'..'y' -> string_part_at "ₐ ᵦ ᵪ ᵨ ₑ ᵩ ₔ ₕ ᵢ ⱼ ₖ ₗ ₘ ₙ ₒ ₚ ᵩ ᵣ ₛ ₜ ᵤ ᵥ ᵥᵥ ₓ ᵧ" (code c - code 'a')
  | c -> raise(Invalid_argument("subscript: " ^ String.make 1 c ^ " has no subscript form assigned to it")))


type var = int
let compare_var = (-) (* variables are positive *)
(* Argument terms of operation: In principle they could be anything. However we need generic sorting and embedability to clauses. Second is biggest of many problems for an extra type variable, while first is problem for using the extensible exception type to hold the arguments. Instead we rely on that terms suffice as arguments—after all polynomials can be embedded to them. *)
type atom = I | V of var | T of Term.t
type poly = op list list
and op =
| C of Z.t (* never C 1; unique if present *)
| A of atom (* unique and last in a monomial if present *)
| D of var
| XD of var
| S of var
| X of op list (* never X(X p · q) or X(C p · q) or X(A I); always X(_@[A _]) *)
| O of (var*poly)list

(* Distinguish operator polynomials and recurrence equations: is A present in monomials or not. *)
let equational = for_all(fun m -> m!=[] & match hd(rev m) with A _ -> true | C o -> is0 o | _ -> false)
let operational = for_all(fun m -> m=[] or match hd(rev m) with A _ -> false | C o -> is0 o | _ -> true)

let _0 = []
(* Use instead of C constructor with monomials. *)
let ( *.) c m = Z.(if equal c zero then [C zero] else match m with
  | C a ::n -> if equal (c*a) one then n else C(c*a)::n
  | m -> if equal c one then m else C c :: m)
(* Use instead of C constructor with polynomials. *)
let ( *:)c = map(( *.)c)
let const_op_poly n = Z.(if equal n zero then [] else if equal n one then [[]] else [[C n]])
let const_eq_poly n = Z.(if equal n zero then [] else if equal n one then [[A I]] else [[C n; A I]])
(* Other safe constructors except that one for X comes after ordering and general compositions after arithmetic. *)
let shift i = O[i,[[A(V i)];[A I]]]
let mul_var i = X[A(V i)]
(* Embed term into polynomial argument. See also poly_of_term to reverse embedding of polynomial in a term. *)
let of_term t = [[A(T t)]]


(* Auxiliarity functions for general algebraic structures *)

(* Append padding elements to the end of the shorter list l1 or l2 to make their lengths equal. *)
let pad_tail fill l1 l2 = (fun l -> l @ repeat ((length%%>max) l1 l2 - length l) [fill]) @@ (l1,l2)
(* Append padding elements to the start of the shorter list l1 or l2 to make their lengths equal. *)
let pad_head fill l1 l2 = (fun l -> repeat ((length%%>max) l1 l2 - length l) [fill] @ l) @@ (l1,l2)

let rec trim_head delete = function o::l when delete o -> trim_head delete l | l->l
let trim_tail delete = rev % trim_head delete % rev

(* Lexicographic comparison after 0-padding and reversal. E.g. [a] ≈ [a,0] ≤ [a,b] < [_,B] when 0≤b<B. *)
let compare_later_dominant l1 l2 = uncurry(lex_list(-)) (rev @@ pad_tail 0 l1 l2)
let compare_later_dominant = rev %%> length_lex_list(-)


(* Some algebraic interfaces / type classes as OCaml objects. The modules EQ, ORD, MONOID etc. also exist in Interfaces.ml and are used elsewhere in Zipperposition code. Modules might be more flexible and efficient than objects. However as the inheritance hierarchy gets deeper, modules become clumsy to use as they require more type conversion annotations. This file does not make heavy use of the abstractions build on top of these type classes, so it all could be specialized and the type classes removed, if preferable. *)
type 't ord = <compare: 't->'t->int>
type 't monoid = <plus: 't->'t->'t; o: 't>
type 't group = <'t monoid; minus: 't->'t->'t>
type 't ring = <'t group; times: 't->'t->'t; of_Z: Z.t->'t>
type 't ord_monoid = <'t ord; 't monoid>
type 't ord_group = <'t ord; 't group>
type 't ord_ring = <'t ord; 't ring>

let theZ: Z.t ord_ring = object
  method compare = Z.compare
  method plus = Z.add
  method minus = Z.sub
  method times = Z.mul
  method of_Z = id
  method o = Z.zero
end

let int_alg: int ord_ring = object
  method compare = compare
  method plus = (+)
  method minus = (-)
  method times = ( * )
  method of_Z = Z.to_int
  method o = 0
end

(* Monoid (R,·,1) from ring (R,+,·,0,1,...) *)
let multiplicative r : 'r monoid = object method plus = r#times method o = r#of_Z Z.one end

(* Derived basic functions *)
let equal' k x y = 0=k#compare x y
let min' k x y = if k#compare x y < 0 then x else y
let max' k x y = if k#compare x y > 0 then x else y
let sum' m = fold_left m#plus m#o
let neg' g = g#minus g#o
let is0' m = equal' m m#o
let rec multiple' m x n =
  assert(n>=0);
  if n=0 then m#o else multiple' m (m#plus x x) (n/2) |> if n mod 2 = 0 then id else m#plus x
let pow' r = multiple'(multiplicative r)

module Monoid_of_object(M: sig type t val monoid: t monoid end) = struct
  type t = M.t
  let zero = M.monoid#o
  let plus = M.monoid#plus
end

module Group_of_object(G: sig type t val group: t group end) = struct
  include Monoid_of_object(struct
    type t = G.t
    let monoid = (G.group :> t monoid)
  end)
  let inverse = neg' G.group
end

module Ring_of_object(R: sig type t val ring: t ring end) = struct
  include Group_of_object(struct
    type t = R.t
    let group = (R.ring :> t group)
  end)
  let times = R.ring#times
  let of_Z = R.ring#of_Z
end

(* From ordered group of {r₀,r₁,r₂,...} form ordered group of pseudo-ordinals r₀+r₁ω+r₂ω²+...+rₙωⁿ represented by lists [r₀;...;rₙ]. Here ω is an infinitely large element. The results are never of the form [...;0] but inputs may be. *)
let infinities r : 'r list ord_group =
  (* let with_same_size f = pad_tail r#o %>> uncurry f in
  let map2_trim f = with_same_size(map2 f) %>> trim_tail(equal' r r#o) in *)
  let map2_trim f = pad_tail r#o %>> uncurry(map2 f) %>> trim_tail(is0' r) in
  object
    method compare = rev%trim_head(is0' r) %%> length_lex_list r#compare
    method plus = map2_trim r#plus
    method minus = map2_trim r#minus
    method o = []
  end


(* These are used to simplify defining orderings. They erase some structure from indeterminates that happens to be ignorable in all the orderings that are required in recurrence propagation. *)
type simple_indeterminate = [`D of var |`O of (var*poly) list |`S of var |`T of Term.t |`V of var]

(* Weight of monomial as a sum of given weights of (simple) indeterminates. Used in total_deg. *)
let total_weight w weight = let rec recW i=i|>
  fold_left(fun sum op -> w#plus sum (match op with
    | C _ | A I -> w#o
    | A(V i) -> weight(`V i)
    | A(T t) -> weight(`T t)
    | D i -> weight(`D i)
    | S i -> weight(`S i)
    | O f -> weight(`O f) (* sum' w (map(fun(n,fn)-> weight(`O(n,fn))) f) *)
    | X f -> recW f
    | XD i -> w#plus (weight(`V i)) (weight(`D i))
  )) w#o in recW

(* Total degree of a monomial, counting every indeterminate as 1. *)
let total_deg = total_weight theZ ~=Z.one

(* Shapes of indeterminates without parameters.
 This is used to reduce the match cases in comparison tiebreaking. We can probably keep this as an internal detail. Nevertheless indeterminate_order is already made a mutable reference for future flexibility. *)
type indeterminate_shape = [`I|`C|`V|`O|`S|`D|`XD|`X|`T]
let indeterminate_order: indeterminate_shape list ref = ref [`I;`C;`V;`X;`O;`S;`D;`XD;`T]

let indeterminate_shape = function C _->`C | S _->`S | D _->`D | XD _->`XD | O _->`O | X _->`X | A(V _)->`V | A I->`I | A(T _)->`T

(* Monomial and other orders
TODO What nonstandard invariants the order must satisfy? At least the C(oefficient)s must contribute last because ++ changes them while linearly merging monomial lists instead of full resorting. *)

(* This implements monomial orders and is parameterized by weights of (simple) indeterminates. (The functions compare_poly_by and tiebreak_by are merely mutually recursive helpers.) Usage interface is provided by rev_cmp_mono and the stateful indeterminate_weights. *)
let rec compare_mono_by weights = dup %%>
  (total_weight(infinities int_alg) weights %%> compare_later_dominant)
  *** tiebreak_by weights

and compare_poly_by weights = lex_list(compare_mono_by weights)

and tiebreak_by weights = rev %%> lex_list(fun x y ->
  let xw,yw = flip index_of' !indeterminate_order % indeterminate_shape @@ (x,y) in
  if xw<yw then -1 else if xw>yw then 1 else match x,y with
    | A I, A I -> 0
    | C x, C y -> Z.((compare***compare) (abs x, x) (abs y, y))
    | D x, D y | S x, S y | XD x, XD y | A(V x), A(V y) -> compare x y
    | X x, X y -> compare_mono_by weights x y
    | O x, O y -> lex_list(compare_var *** compare_poly_by weights) x y
    | A(T x), A(T y) -> Term.compare x y
    | _ -> assert false (* If xw=yw, variant constructors of x,y must be equal and they are covered above. *))

(* Assign this to set the global monomial order. Old polynomials become invalid when the order changes (because they are sorted by the monomial order) and must not be used afterwards. However polynomials stored in clauses can still be reretrieved (see poly_as_lit_term_id and poly_of_{lit,term,id}). The weights are effectively 0-extended and compared as “signed ordinals”. Trailing zeros do not matter.
 Desing: A global switch like this has natural drawbacks. Namely we cannot abstract polynomials computations over the order which prevents e.g. parallelization and pause-resume constructs. This could become an issue with infinitely branching inferences. However the global reference is much simpler to use because otherwise all abstractions on top of polynomial arithmetic would have to take the order as paramters either explicitly or as part of module construction. 
 To define a weight function see elim_oper_args and elim_indeterminate. *)
let indeterminate_weights: (simple_indeterminate -> int list) ref = ref~=[]

(* The main interface to monomial orders, conveniently reversed for sorting maximal monomial first. *)
let rev_cmp_mono n m = compare_mono_by !indeterminate_weights m n (* Keep parameters to reread indeterminate_weights! *)
let sort_poly: poly->poly = sort rev_cmp_mono

(* Some indeterminate weights for common elimination operations *)

(* An elimination priority to certain argument terms. For example: elim_oper_args[t,2; s,1; r,1] to eliminate terms t,s,r with priority among them in t. *)
let elim_oper_args term_weight_list : simple_indeterminate -> int list = function
|`T t -> cons_maybe (assq_opt t term_weight_list) []
| _ -> []

(* An elimination priority to certain indeterminates. This legacy function now almost directly delegates to its weighting parameter. *)
let elim_indeterminate weight : simple_indeterminate -> int list = fun x -> [weight x]


(* Use this instead of the X constructor. *)
let mul_indet = 
  let rec x = function
  | (X _ | C _ as m)::f -> m :: x f
  | [A I] -> []
  | m -> assert(equational[m]); [X m] in
  sort_poly % map x

(* Equality of normalized polynomials *)
let rec poly_eq p = CCList.equal mono_eq p
and mono_eq m n = m==n or CCList.equal indet_eq m n
and indet_eq x y = x==y or match x,y with
  | C a, C b -> Z.equal a b
  | A(T t), A(T s) -> t==s
  | X f, X g -> mono_eq f g
  | O f, O g -> CCList.equal(CCPair.equal (=) poly_eq) f g
  | a, b -> a = b

let bit_rotate_right r b = (b lsr r) + (b lsl lnot r) (* for int lacking 1 bit and r≥0 *)

(* Hash code of normalized polynomials *)
let rec poly_hash ?(depth=2) p = fold_left (fun h m -> 127*h + mono_hash ~depth m) 0 (take 4 p)
and mono_hash ?(depth=2) = fold_left (fun h x -> 31*h + indet_hash ~depth x) 0
and indet_hash ?(depth=2) = if depth=0 then ~=7 else function
  | X f -> mono_hash ~depth:(depth-1) f
  | O f -> fold_left (fun h (i,fi) -> bit_rotate_right i (h + poly_hash ~depth:(depth-1) fi)) 0 f
  | A(T t) -> Term.hash t
  | x -> Hashtbl.hash x

(* Auxiliarity to below. Separate the predicated indeterminates to coefficient polynomials. Inverse is given by
	p: (poly * op list)list ↦ p |> map(fun(c,m) -> c><[m]) |> fold_left(++)_0 *)
let coef_view part_of_coef : poly -> (poly * op list) list =
  map(take_drop_while part_of_coef)
  %> group_by ~eq:(snd%%>mono_eq) ~hash:(snd%>mono_hash)
  %> map(fun coef_mono_list -> map fst coef_mono_list, snd(hd coef_mono_list))
  %> sort(snd %%> rev_cmp_mono)

(* Separate pointwise-multiplicative coefficients, e.g. 2yY+3y+4Y+5 ↦ [(2y+4, Y); (3y+5, 1)] *)
let mul_coef_view = coef_view(function C _ | X _ -> true | _ -> false)
let lead_coef = fst % hd % mul_coef_view	(* e.g. 2y+4 from above *)
let lead_main_ops = snd % hd % mul_coef_view	(* e.g. [Y] from above *)

(* Allows to compute e.g. ∑ₙᑉᵐ s.t. ∑ₙᑉᵐ(M·P) = -Pₘ + M·∑ₙᑉᵐP and ∑ₙᑉᵐ(N·P) = Pₘ - P₀ + ∑ₙᑉᵐP *)
let rec fold_indeterminates a f = function
| [] | [A I] -> a `I
| [A(T t)] -> a(`T t)
| [A(V i)] -> a(`V i)
| x::m -> f (fold_indeterminates a f m) [m] x (* x last ⇒ anonymous pattern match is possible *)


(* Pretty-printing string conversion *)

(* TODO provide an association in an environment *)
let var_name = string_part_at "y x v u t s r p o n m l k j i h e a"
(* Mostly Term.to_string but when polynomials are embedded into terms which are in turn embedded into polynomials, confusion could arise. Hence in one of these two embeddings the names have to be annotated.
 Currently I annotate at the embedding. This has the trade-off of making all polynomials in terms/clauses “ugly” but an embedded polynomial in a term in a polynomial does not need nested annotation. *)
let term_name t = Term.to_string t

(* E.g. ["-2x";"3y";"-4z"] becomes "-2x﹢3y﹣4z". (On Ubuntu command line “﹢” has width 2 which seems nice balance between 1 of “+” and 3 of “ + ”, some times.) *)
let concat_plus_minus view = function
| [] -> "0"
| m::p -> map view p
  |> flat_map String.(fun s -> if rcontains_from s 0 '-' then ["﹣"; sub s 1 (length s -1)] else ["﹢"; s])
  |> cons(view m)
  |> String.concat ""

let rec poly_to_string p = concat_plus_minus mono_to_string p

and poly_view_string p = mul_coef_view p |> concat_plus_minus(function
  | [n], m -> mono_to_string(n@m)
  | c, m -> "("^ poly_to_string c ^")"^ match mono_to_string m with "1"->"" | s->s)

and mono_to_string m = group_succ ~eq:indet_eq m
  |> concat_view "" (fun n -> indet_to_string(hd n) ^ match length n with 1->"" | l -> superscript(string_of_int l))
  |> function ""->"1" | "-"->"-1" | "͘"->"1͘" | s->s

and indet_to_string = function
| C a -> (match Z.to_string a with "-1"->"-" | s->s) (* The trivial a=1 does not occur. *)
| A I -> "͘"
| A(V i) -> var_name i
| A(T t) -> term_name t
| D i -> subscript(var_name i)
| XD i -> "ð"^subscript(var_name i)
| S i -> "∑"^superscript(var_name i)
| X m -> mono_to_string m
| O[i,[[A(V j)];[A I]]] when i=j -> String.uppercase(var_name i)
| O f -> "{"^ concat_view "" (fun(i,fi)-> superscript(var_name i) ^ poly_view_string fi) f ^"}"

let pp_poly = to_formatter poly_to_string


(* Embedding polynomials into terms
 Equality is not preserved! This embedding is a convenient point to make polynomials aware of the monomial order that is varied between computations. When a polynomial is retrieved from a clause, it is resorted to conform to the current order, if necessary. *)

(* List of terms that the given recurrence relates, without duplicates. *)
let terms_in = sort_uniq ~cmp:Term.compare % flat_map(fun m -> match rev m with A(T t)::_->[t] | _->[])

let term0 = Term.const ~ty:term (ID.make "𝟬")
exception RepresentingPolynomial of poly * Precedence.Weight.t * (simple_indeterminate -> int list)

(* Given polynomial P≠0, embed P into a fresh ID idP, that idP into a Term termP, and it into a literal term0≈termP. Return all three.
 ~name is the name given to the idP. If omitted, the name is (somewhat wastefully) taken to be the string representation of P, which is further annotated to support nesting the terms back into polynomials and into terms again.
 ~weight becomes the weight of idP in KBO. Default weight ω is large because the polynomial literals are expected to be the most expensive literals of a clause to process. Note: the weight assignment is separately informed about the embedded polynomials because the weight of an ID is not assigned on construction, and precedence is fixed at the same time. *)
let poly_as_lit_term_id ?name ?(weight=omega) p =
  let id = ID.make(name |> get_lazy(fun()->
    (* Compute default name only if none is given. *)
    let replace = fold_left (%) id % map(fun(old,by) -> global_replace (regexp old) by) in
    let _name = poly_to_string p
    |> replace ["﹢","˖"(*ᚐ*); "﹣","−"; "-","−"] (* avoid ' ' around name *)
    |> flip String.iter
    (* |> Iter.filter(fun c -> not(mem c [' ';'(';')'])) *)
    (* |> Iter.take 20 *)
    (* I annotate by overlining here that has the benefit of not increasing visual length but the down side of being problematic to iterate. As all pretty-printing, this can be changed to fit other needs, should such arise. *)
    |> Iter.to_string ~sep:"" (fun c ->
      (if code c lsr 6 != 2 then "̅" else "") ^ String.make 1 c)
    in String.sub (_name ^ "̅") 2 (String.length _name)))
  in
  ID.set_payload id (RepresentingPolynomial(p, weight, !indeterminate_weights));
  let term = Term.const ~ty:term id in
  Literal.mk_eq term0 term, term, id

(* Includes semantics of 0 giving tautology unlike poly_as_lit_term_id. (Repeated calls still give incomparable literals.) *)
let polyliteral p = if p = _0 then mk_tauto else (fun(l,_,_)->l) (poly_as_lit_term_id p)

(* Retrieve polynomial that was embedded into an id by poly_as_lit_term_id (directly or indirectly). The retrieved polynomial conforms to the current monomial order. *)
let poly_of_id id = id |> ID.payload_find ~f:(fun data -> match data with
  | RepresentingPolynomial(p,w,ord) ->
    if ord != !indeterminate_weights then(
      (* If ordering by indeterminate_weights has changed, resort the polynomial. *)
      let p' = sort_poly p in
      ID.set_payload ~can_erase:((==)data) id (RepresentingPolynomial(p', w, !indeterminate_weights));
      Some p')
    else Some p
  | _ -> None)

(* Retrieve polynomial embedded into a term and make it conform to the current monomial order. See also of_term. *)
let poly_of_term t = match view t with Const id -> poly_of_id id |_->None

(* Polynomial representing the given term. If the term already embeds a polynomial, result is that embedded polynomial, in order to avoid multilayer embeddings. TODO we should not need 3 term→polynomial conversions! *)
(* let of_term' t = get_or~default:[[A(T t)]] (poly_of_term t) *)

(* Retrieve polynomial embedded into a literal and make it conform to the current monomial order. *)
let poly_of_lit = function Equation(o,p,true) when o==term0 -> poly_of_term p |_->None

(* Weight of polynomial id. Used when registering the extension in summation_equality.ml to update the weight assignment to be aware of the embedded polynomials. *)
let polyweight_of_id = ID.payload_find ~f:(function RepresentingPolynomial(_,w,_) -> Some w |_->None)


(* Arithmetic operations ++, --, >< *)

(* a·m, b·m ↦ (a+b)·m where numbers a,b are implicitly 1 if absent. *)
let mono_add_coefficients = curry(function
| C a ::m, C b ::n -> if_~=(mono_eq m n) (Z.(a+b) *. m)
| C a ::m, n | m, C a ::n -> if_~=(mono_eq m n) (Z.(a+one) *. m)
| m, n -> if_~=(mono_eq m n) (Z.of_int 2 *. m))

let rec (++) p q : poly = match p,q with
| p, [] -> p
| m::p, n::q -> (match mono_add_coefficients m n with
  | Some(C o ::_) when is0 o -> p++q
  | Some mn -> mn :: p++q
  | _ when rev_cmp_mono m n < 0 -> m :: p++(n::q)
  | _ -> (n::q) ++ (m::p))
| p, q -> q++p

let (--) p q =
  let negate = function C a ::m -> Z.neg a *. m | m -> Z.minus_one *. m in
  p ++ map negate q


let rec (><) p q = match p,q with
| _,[] | [],_ -> [] (* 0 *)
| p, []::q (* p(1+q) *)-> p ++ (p><q)
| []::p, q (* (1+p)q *)-> q ++ (p><q)
| [[x]], [[y]] -> indeterminate_product x y
| nx::p, (y::m)::q ->
  let n,x = remove_at_idx(-1) nx, hd(rev nx) in
  let xy = [[x]]><[[y]] in
  (* Inputs nx and y::m are valid monomials, so if [[x;y]] is too, then simple concatenation puts indeterminates to the right order: *)
  (if poly_eq xy [[x;y]] then [nx@y::m]
    else [n]><xy><[m])
  ++ ([nx]><q) ++ (p><[y::m]) ++ (p><q)

(* As named. A(rgument) must only appear as the right-most input. (Product of two A's is formed using X.) *)
and indeterminate_product x y =
  let x_ f = mul_indet([[x]]><f) in
  match x,y with
  | C n, C k -> const_op_poly Z.(n*k)
  | _, C o | C o, _ when is0 o -> [[C o]] (* acceptably may eliminate A _ *)
  | x, C k -> [[y;x]]
  | D i, X f -> [[X f; D i]] ++ x_[f]
  | XD i, X f -> [[X f; XD i]] ++ x_[f]
  | D i, O f -> (if mem_assq i f then f else (i,[[A(V i)]])::f) (* identity ∘'s in f are implicit *)
    |> map(fun(n,fn)-> x_ fn >< [o f @[D n]]) (* coordinatewise chain rule *)
    |> fold_left (++) _0 (* sum *)
  | XD i, O f -> fold_left(++)_0 (map(fun(n,fn)-> (todo"X fn⁻¹") >< x_ fn >< [o f @[XD n]]) (if mem_assq i f then f else (i,[[A(V i)]])::f)) (* like previous *)
  | D i, XD j when i=j -> [[XD i; D i]; [D i]]
  | O f, X g -> x_[g] >< [[O f]]
  | X(S i ::f), S j when i=j -> let _Si_Xf = [[S i]]><mul_indet[f] in (_Si_Xf><[[S i]]) ++ [[S i; X(S i :: f)]] ++ _Si_Xf
  | O[i,[[A(V j)];[A I]]], S l when i=j&i=l -> [[S i]; []]
  | S l, O[i,[[A(V j)];[A I]]] when i=j&i=l -> [[S i]; C Z.minus_one :: eval_at Z.zero ~var:i; []]
  | S l, O[i,[[A(V j)];[A I]]] when i=j -> [[y;x]]
  | S l, (X[A(V i)] | D i | XD i) when l!=i -> [[y;x]]
  | O[i,[[C o]]], S l when i=l -> assert(is0 o); [[x]]
  | O[i,[[C c; A I]]], S l when i=l -> Z.(if c < zero
    then map(fun n -> eval_at~var:i (of_int n + c)) (range' 0 (Stdlib.abs(to_int c)))
    else map(fun n -> eval_at~var:i (of_int n)) (range' 0 (to_int c)))
  | O[i,[[A(V j)];[A I]]], O[l,[[A(V k)];[A I]]] when i=j & l=k & i>l -> [[y;x]] (* TODO composite case *)
  | D i, D l | XD i, XD l | S i, S l | X[A(V i)], X[A(V l)] when i>l -> [[y;x]]
  | X f, X g when rev_cmp_mono f g < 0 -> [[y;x]] (* Assumes commutative product! *)
  | X f, A _ when rev_cmp_mono f [y] < 0 -> mul_indet[[y]]><[f] (* Assumes commutative product! *)
  | D _, A I -> []
  | D i, A(V n) -> const_eq_poly Z.(if i=n then one else zero)
  | O f, A(V n) -> get_or~default:[[y]] (assq_opt n f)
  | A _, _ -> raise(Invalid_argument("indeterminate_product ("^ indet_to_string x ^") ("^ indet_to_string y ^")"))
  (*| A _, y -> [[y;x]] (* Delicate: I want to simplify also _,A but only when A is fully right which must be taken into account somewhere at some point! TODO was there a real need for this? *)*)
  | x, y -> [[x;y]]

(* Use these instead of the O constructor. *)
and eval_at' ~var p = o[var, p]
and eval_at ~var at = eval_at' var (const_eq_poly at)
and o subst = match subst
  |> map(map_snd(fun p -> if equational p then p else p>< const_eq_poly Z.one))
  |> filter(function i,[[A(V j)]] -> i!=j | _-> true)
with []->[] | s-> assert(for_all (equational%snd) s); [O s]

(* Does [[x;y]] represent x·y. Equivalently is x≯y in the internal indeterminate order. *)
let join_normalizes x y = match x,y with
  | A _, _ -> false
  | _ -> poly_eq (indeterminate_product x y) [[x;y]]

let poly_alg: poly ring = object
  method o = _0
  method plus = (++)
  method minus = (--)
  method times = (><)
  method of_Z = const_op_poly
end

(* Law product(a@b) = product a >< product b requires to choose to use const_op_poly. *)
let product = fold_left (><) (const_op_poly Z.one)

(* Unification and superposition *)

let rec unifiers m' n' =
  (* Input is reversed for matching but output is in standard writing order. *)
  let rec loop = function
  | [C a], [C b] -> Z.(let g = gcd a b in [C(b/g)], [C(a/g)])
  | m, ([] | [C _] as n) -> n, rev m
  | x::m, x'::n when indet_eq x x' -> loop(m,n)
  | x::m, y::n when join_normalizes x y ->
    (* This'd be faster if monomials were encoded in reverse. Only downside would be unintuitivity. *)
    (match rev(hd([[y]]><[rev(x::m)])) with
    | y'::xm when indet_eq y y' -> let u,v = loop(xm,n) in u@[y], v
    | _ -> raise Exit)
  | m, y::n when m=[] or join_normalizes y (hd m) -> CCPair.swap(loop(y::n, m))
  | _ -> raise Exit
  in
  try Some(loop(rev m', rev n')) with Exit -> None

let lead_unifiers = curry(function x::_, y::_ -> unifiers x y | _ -> None)

let (|~>) general special = match lead_unifiers general special with
| Some(u, []) -> Some u
| Some(u, [C __1]) when Z.(equal __1 minus_one) -> Some(__1*.u)
(* Only reduce the absolute value of the leading coefficient. This is symmetric w.r.t. 0 so a/b rounds correctly towards 0. *)
| Some(C a :: u, [C b]) when Z.(abs a > abs b) -> Some Z.(a/b*.u)
(* TODO is the converse when u=[] necessary to handle? *)
| _ -> None


let superpose p p' = if p==p' then [] else
  match lead_unifiers ~<p ~<p' with
  | Some(u,u') -> [([u]><p) -- ([u']><p')]
  | None -> []

let leadrewrite r p = CCOpt.map(fun u -> p -- ([u]><r)) (r |~> p)


module type View = sig type t type v val view: t -> v option end
(* Create index from mapping clause→polynomial, that can be instantiated by empty_with' default_features. *)
module LeadRewriteIndex(P: View with type v=poly) = FV_tree.FV_IDX(struct
  type t = P.t
  let compare = P.view %%> Stdlib.compare (* only used by sets *)
  type feature_func = poly -> int
  let compute_feature f p = match P.view p with Some p when p!=_0 -> Some(FV_tree.N(f p)) | _->None
end)

(* Features for a rewriting index testing various sums of operator degrees. Only for ≠0 polynomials. *)
let default_features() = (* () because of type variables *)
  let sum value = sum_list % map value % hd in
  [(* TODO double check that these are correctly increasing *)
    "total degree", sum~=1;
    "shift degree", sum(function O _ -> 1 | _ -> 0);
    "coefficient degree", sum(function X _ -> 1 | _ -> 0);
    "1ˢᵗ var. degree", sum(function O[0,_] | X[A(V 0)] -> 1 | _ -> 0);
    (* then: multiset of indeterminates... except that only ID multisets are supported *)
  ]


(* Affine substitutions: accessing variables and matrix form *)

let union_map f = fold_left (fun u a -> Int_set.union u (f a)) Int_set.empty

(* Domain and range (explicit parts) of an O (substitution) indeterminate. *)
let domainO f = Int_set.of_list(map fst f)
let rec rangeO f = union_map(free_variables%snd) f

and free_variables p' = Int_set.(let rec monoFV = function
  | [] -> empty
  | O f :: m -> union (rangeO f) (diff (monoFV m) (domainO f))
  | x::m -> union (monoFV m) (match x with
    | O f -> assert false (* was prior case *)
    | C _ | A I -> empty
    (* Do operators make explicit all implicit dependencies in argument terms? If not, below is wrong! *)
    | A(V i) | S i | D i | XD i -> singleton i
    | X f -> monoFV f
    | A(T t) -> map_or ~default:empty free_variables (poly_of_term t) (* TODO properly track variables of a term *)
  ) in
  union_map monoFV p')

(* Output ϱ,σ,raw. Renaming substitution indeterminate ϱ satisfies dom ϱ = vs\taken and img ϱ ∩ vs∪taken = ∅. Substitution indeterminate σ undoes ϱ meaning [σ]><[ϱ]><p = p if free_variables p ⊆ taken∪vs. Finally raw is the list of the variable pairs (vᵢ,uᵢ) that define σ:vᵢ↦uᵢ and ϱ:uᵢ↦vᵢ. *)
let rename_apart ~taken vs = match Int_set.(
  let collide = diff vs taken in
  let m = lazy(ref(max_elt(union vs taken))) in
  fold Lazy.(fun c pairs -> incr(force m); (c, !(force m))::pairs) collide []) with
  | [] -> [], [], []
  | raw -> o(map(fun(v,u)->v,[[A(V u)]]) raw), o(map(fun(v,u)->u,[[A(V v)]]) raw), raw

(* If f n = M·n+a⃗, return Some(M,a⃗) where the matrix M and vector a⃗ are directly indexed by the present variables (as variables are usually small, expected waste of space is little—except that matrices can still easily waste quadratically space). Moreover, to distinguish compound shifts, we test if M=I by encoding [||] for no-change rows (that is, if Meᵢ=eᵢ then M.(i)=[||]).  Substitution changes variables of the range to variables of the source space. Usually we index by the mapped range variables and hence they are the first indices that happens to coincide with the standard convention of writing matrix element indices. *)
let view_affine f =
  let size = 1 + fold_left(fun s (v,_) -> max s v) 0 f in
  let width = map_or ~default:0 ((+)1) (Int_set.max_elt_opt(rangeO f)) in
  let matrix = Array.make size [||] in
  let shift = Array.make size 0 in
  try f|>map(fun(i,p) ->
    let put vs sh = (* i ↦ vs+sh = variable list + shift constant *)
      shift.(i) <- sh;
      if vs<>[[A(V i)]] then (* identity variable mapping gets encoded by [||] always *)
        matrix.(i) <- fold_left (fun row -> function
          | [A(V n)] -> row.(n) <- 1; row
          | [C a; A(V n)] -> row.(n) <- Z.to_int a; row
          | _ -> raise Exit
        ) (Array.make width 0) vs
    in
    match rev p with
    | [C a; A I]::vs -> put vs (Z.to_int a)
    | [A I]::vs -> put vs 1
    | vs -> put vs 0
  )|> ~=(Some(matrix, shift))
  with Exit -> None

(* After view_affine it is easy to further detect compound shifts from the condition that their matrix is the identity matrix.
 Since substitutions implicitly leave unmentioned variables untouched, it was convenient to extend this to matrices so that identity matrix always has only empty rows. This convention also requires custom indexing operation. *)
let isMatrix1 i = Array.for_all((=)[||])i
let (@.) m (i,j) = if m.(i)=[||] then if i=j then 1 else 0 else m.(i).(j)


(* The input function produces polynomials, and its mapped version is a homomorphism. *)
let map_monomials f = fold_left (++) _0 % map f
let map_indeterminates f = map_monomials(product % map f)
let map_terms f = map_indeterminates(function A(T t) -> of_term(f t) | x -> [[x]])

(* Applicable after (mul_)coef_view. Pack separated (op list)'s into argument terms of polynomial except when they are already packed that way. *)
let to_poly_poly: (poly * op list) list -> poly =
  fold_left (++) _0 % map(fun(coef, arg) -> coef >< match arg with
    | [A(T t)] -> [arg]
    | _ -> (* If arg is not term, pack it into such. Since the packing is not a true function, input is taken in the (poly * op list)list -form grouped by arg. *)
      let _,t,_ = poly_as_lit_term_id [arg] in of_term t)

(* Turn general formula into a recurrence in operator polynomial representation by embedding other parts into argument terms. *)
let oper_coef_view: poly -> poly = to_poly_poly % coef_view(function
  | C _ | X[A(V _)] -> true
  | O f -> let var_or_const = function [A(V _)] | [A I] | [C _; A I] -> true | _ -> false
    in for_all(for_all var_or_const) (map snd f)
  | _ -> false)


(* Parsing polynomials from strings—primarily for testing *)

(* If pre^s=string then Some(act s) else None. *)
let if_start pre act string = String.(
  if length pre <= length string & sub string 0 (length pre) = pre
  then Some(act(sub string (length pre) (length string - length pre)))
  else None)

let match_start(type r) ?(split=' ') patterns string =
  let rules = flat_map (fun(pattern,act)-> mapi(fun i p -> p, act i) (String.split_on_char split pattern)) patterns in
  let exception Result of r in
  let rec apply_first = function
  (* Ocaml top-level (basic and utop) truncates error messages and it is not obvious how to avoid this. Hence to display important info from both of the long inputs, we manually truncate the matched string. This is unfortunate because without UTF-8 support, that we do not care to add as a dependency, the message easily ends up locally corrupted. *)
  | [] -> raise(Invalid_argument String.(
    "match_start: "^ sub string 0 (min (length string) (3 + max_list(List.map(length%fst) rules))) ^"... starts by none of patterns:\n"^ concat (make 1 split) (List.map fst rules)))
  | (p,act)::rest -> match if_start p act string with
    | Some ok -> raise(Result ok)
    | None -> apply_first rest
  in
  try apply_first rules with Result r -> r

let testterms = map (Term.const ~ty:Type.term % ID.make) ["b";"c";"f";"g";"q";"z"]

(* To generate test data. No multidigit numerals. Terms e.g. b,c,f,g and variables e.g. n,m,k,x,y (must be compatible with var_name). Examples:
  Xx+3x-2
  Mmg - nMg - mg - g
  Nng - mg + ng
  ₓₓf + ᵧᵧf - ∑ˣ Y∑ʸ yₓGᵧg
  -ₓXFg + Xₓf.Xg + Xf.ₓXg -1
  {ᵃₜb.Nb}{ᵉ2a}{ⁱ3e}{ʲ4i}{ᵏ5j}{ˡ1+k}{ᵐl+mn-2}
  {ⁿm-n}f + N{ˣm}∑ˣf
*)
[@@@warning "-14"](* Regular expressions depend on UTF-8 encoding—did not work?! *)
let rec poly_of_string s =
  (* Workaround to add substitutions ({ᵛᵃʳpoly} ≡ {var↦poly}) to unextensible setup: Replace {ᵛ...} by an encoding character c and add rule c→... for map_start_factor. *)
  let delim = regexp(String.of_seq(to_seq['{';'.';'.';'[';chr 131;'-';chr 191;']';'?';'\\';'|';'}'])) in
  let s', mapSubst = full_split delim s |> let rec packSubst = function
    | Delim"}" :: s' -> packSubst s'
    | Delim v :: Text p :: s' ->
      let r,m = packSubst s' in
      let codeSubst = String.make 1 (chr(length m)) in
      if codeSubst=" " then raise(Failure("packSubst: Too many substitutions in "^s));
      codeSubst ^ r,
      (codeSubst, fun i->String.[o[index_of' (sub v 1 (length v - 1)) (split_on_char ' ' "ʸ ˣ ᵛ ᵘ ᵗ ˢ ʳ ᵖ ᵒ ⁿ ᵐ ˡ ᵏ ʲ ⁱ ʰ ᵉ ᵃ"), poly_of_string p]]) :: m
    | Text p :: s' -> map_fst((^)p) (packSubst s')
    | [] -> "",[]
    | s' -> raise(Invalid_argument("packSubst: "^ to_string(function Text t | Delim t -> t) s' ^" is ill-formed in "^s))
  in packSubst in
  (* sanity check for operator vs. recurrence *)
  let check_mul_indet p = if equational p then mul_indet p else
    raise(Invalid_argument("Operator polynomial " ^ poly_to_string p ^ " cannot become a multiplier indeterminate in " ^ s)) in
  let replace c s = String.concat"" % map(fun a -> if a=c then s else String.make 1 a) % of_seq % String.to_seq in
  let split_map_fold split fold mapper = String.split_on_char split %> map mapper %> fold_left_1 fold in
  let p = replace '-' "+-" s' |>
    split_map_fold '+' (++) (
      split_map_fold '.' (fun n m -> check_mul_indet n >< m)
        (poly_of_mono_string mapSubst % replace ' ' "")) in
  (* Make argument A I explicit for C and X monomials if A _ appears in any monomial. *)
  let p_eq = filter((function A _ :: _ -> true | _ -> false) % rev) p in
  let p_CX = filter(for_all(function C _ | X _ -> true | _ -> false)) p in
  if p_eq=[] then p else
  if length p_eq + length p_CX = length p then p_eq ++ (p_CX><[[A I]])
  else raise(Invalid_argument(poly_to_string p ^ " from " ^ s ^ " mixes operator and applied monomials"))

and poly_of_mono_string base_rules s' = if s'="" then _0 (* neutral in splitting above *) else
  let map_start_factor rules = match_start (map (fun(p,f)-> p,
    fun i s -> f i >< if s="" then [[]] (* neutral in this recursion *) else poly_of_mono_string base_rules s
  ) rules) s' in
  map_start_factor(base_rules@[
    ("y x v u t s r p o n m l k j i h e a",fun i->[[mul_var i]]);
    ("Y X V U T S R P O N M L K J I H E A",fun i->[[shift i]]);
    ("∑ʸ ∑ˣ ∑ᵛ ∑ᵘ ∑ᵗ ∑ˢ ∑ʳ ∑ᵖ ∑ᵒ ∑ⁿ ∑ᵐ ∑ˡ ∑ᵏ ∑ʲ ∑ⁱ ∑ʰ ∑ᵉ ∑ᵃ",fun i->[[S i]]);
    ("ᵧ ₓ ᵥ ᵤ ₜ ₛ ᵣ ₚ ₒ ₙ ₘ ₗ ₖ ⱼ ᵢ ₕ ₑ ₐ",fun i->[[D i]]);
    ("∂ d",fun i->[[D 1]]);
    ("0 1 2 3 4 5 6 7 8 9",fun i->const_op_poly(Z.of_int i));
    ("-",fun i->const_op_poly(Z.minus_one));
    ("b c f g q z",fun i->of_term(get_at_idx_exn i testterms));
    ("B C F G Q Z",fun i->mul_indet(of_term(get_at_idx_exn i testterms)));
  ])