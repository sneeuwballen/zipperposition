open Logtk
open Interfaces
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
let sum_list = fold_left (+) 0
let sum_array = Array.fold_left (+) 0
let index_of p l = fst(get_exn(find_idx p l))
let index_of' = index_of % (=)
let with_cache_2 c f = curry(with_cache_rec c (uncurry % f % curry))
let with_cache_3 c f = curry(with_cache_2 c (uncurry % f % curry))

let to_formatter: ('x->string) -> 'x CCFormat.printer
  = fun to_string out x -> Format.fprintf out "%s" (to_string x)
let string_part_at ?(split=' ') = get % Array.of_list % String.split_on_char split
let subscript = String.to_seq %> List.of_seq %> concat_view "" (fun c -> match c with
  | '('->"₍" | ')'->"₎" | '+'->"₊" | '-'->"₋" | '='->"₌"
  | '0'..'9' -> string_part_at "₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉" Char.(code c - code '0')
  | 'a'..'y' -> string_part_at "ₐ ᵦ ᵪ ᵨ ₑ ᵩ ₔ ₕ ᵢ ⱼ ₖ ₗ ₘ ₙ ₒ ₚ ᵩ ᵣ ₛ ₜ ᵤ ᵥ ᵥᵥ ₓ ᵧ" Char.(code c - code 'a')
  | c -> raise(Invalid_argument("subscript: " ^ String.make 1 c ^ " has no subscript form assigned to it")))


type var = int
let compare_var = (-) (* variables are positive *)
type 't atom = I | V of var | T of ('t->'t->int) * 't
type 't poly = 't op list list
and 't op =
| C of Z.t (* never C 1 *)
| A of 't atom (* unique and last in a monomial (Perhaps this should be enforced by the type: have atom as type parameter and use unit when now missing, and let poly=(op list * A)list. Beware of unification that might not special case A.) *)
| D of var
| XD of var
| S of var
| X of 't op list (* never X(X p · q) or X(C p · q) or X(A I); always X(_@[A _]) *)
| O of (var * 't poly)list

let _0 = []
(* Use instead of C constructor with monomials. *)
let ( *.) c m = Z.(if equal c zero then [C zero] else match m with
  | C a ::n -> if equal (c*a) one then n else C(c*a)::n
  | m -> if equal c one then m else C c :: m)
(* Use instead of C constructor with polynomials. *)
let ( *:)c = map(( *.)c)
let const_op_poly n = Z.(if equal n zero then [] else if equal n one then [[]] else [[C n]])
let const_eq_poly n = Z.(if equal n zero then [] else if equal n one then [[A I]] else [[C n; A I]])
(* Other safe constructors but those for X are after ordering. *)
let shift i = O[i,[[A(V i)];[A I]]]
let mul_var i = X[A(V i)]
let eval_at ~var at = O[var, const_eq_poly at]
let embed comparison value = [[A(T(comparison, value))]]
let of_term = embed Term.compare

(* Distinguish operator polynomials and recurrence equations: is A present in monomials or not. *)
let equational i=i|> for_all(fun m -> m!=[] & match hd(rev m) with A _ -> true | C o -> is0 o | _ -> false)
let operational i=i|> for_all(fun m -> m=[] or match hd(rev m) with A _ -> false | C o -> is0 o | _ -> true)


(* Append padding elements to the end of the shorter list l1 or l2 to make their lengths equal. *)
let pad_tail fill l1 l2 = (fun l -> l @ repeat ((length%%>max) l1 l2 - length l) [fill]) @@ (l1,l2)
(* Append padding elements to the start of the shorter list l1 or l2 to make their lengths equal. *)
let pad_head fill l1 l2 = (fun l -> repeat ((length%%>max) l1 l2 - length l) [fill] @ l) @@ (l1,l2)

let rec trim_head delete = function o::l when delete o -> trim_head delete l | l->l
let trim_tail delete = rev % trim_head delete % rev

(* Lexicographic comparison after 0-padding and reversal. E.g. [a] ≈ [a,0] ≤ [a,b] < [_,B] when 0≤b<B. *)
let compare_later_dominant l1 l2 = uncurry(lex_list(-)) (rev @@ pad_tail 0 l1 l2)
let compare_later_dominant = rev %%> length_lex_list(-)


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

let equal' k x y = 0=k#compare x y
let min' k x y = if k#compare x y < 0 then x else y
let max' k x y = if k#compare x y > 0 then x else y
let sum' m = fold_left m#plus m#o
let neg' g = g#minus g#o
let is0' m = equal' m m#o

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
  module G = Group_of_object(struct
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

(* Monoid (R,·,1) from ring (R,+,·,0,1,...) *)
let multiplicative r : 'r monoid = object method plus = r#times method o = r#of_Z Z.one end


type simple_indeterminate = Indet: [`D of var |`O of var * 't poly |`S of var |`T of ('t->'t->int) * 't |`V of var] -> simple_indeterminate

let total_weight w weight = let rec recW i=i|>
  let w_of x = weight(Indet x) in
  fold_left(fun sum op -> w#plus sum (match op with
    | C _ | A I -> w#o
    | A(V i) -> w_of(`V i)
    | A(T(c,t)) -> w_of(`T(c,t))
    | D i -> w_of(`D i)
    | S i -> w_of(`S i)
    | O f -> sum' w (map(fun(n,fn)-> w_of(`O(n,fn))) f)
    | X f -> recW f
    | XD i -> w#plus (w_of(`V i)) (w_of(`D i))
  )) w#o in recW

let total_deg i=i|> total_weight theZ ~=Z.one

type indeterminate_shape = [`I|`C|`V|`O|`S|`D|`XD|`X|`T]
let indeterminate_order: indeterminate_shape list ref = ref [`I;`C;`V;`X;`O;`S;`D;`XD;`T]

let indeterminate_shape = function C _->`C | S _->`S | D _->`D | XD _->`XD | O _->`O | X _->`X | A(V _)->`V | A I->`I | A(T _)->`T

(* Monomial and other orders
What invariants the order must satisfy? At least the C(oefficient)s must contribute last because ++ changes them while linearly merging monomial lists instead of full resorting. *)

let rec compare_mono_by weights = dup %%>
  (total_weight(infinities int_alg) weights %%> compare_later_dominant)
  *** tiebreak_by weights

and compare_poly_by weights = lex_list(compare_mono_by weights)

and tiebreak_by weights i=i|> rev %%> lex_list(fun x y ->
  let xw,yw = flip index_of' !indeterminate_order % indeterminate_shape @@ (x,y) in
  if xw<yw then -1 else if xw>yw then 1 else match x,y with
    | A I, A I -> 0
    | C x, C y -> Z.((compare***compare) (abs x, x) (abs y, y))
    | D x, D y | S x, S y | XD x, XD y | A(V x), A(V y) -> compare x y
    | X x, X y -> compare_mono_by weights x y
    | O x, O y -> lex_list(compare_var *** compare_poly_by weights) x y
    | A(T(c,x)), A(T(_,y)) -> c x y
    | _ -> assert false (* If xw=yw, variant constructors of x,y must equal and they are covered above. *))

let indeterminate_weights: (simple_indeterminate -> int list) ref = ref~=[]
let get_indeterminate_weights: unit -> simple_indeterminate -> int list = fun()-> Obj.magic !indeterminate_weights
let rev_cmp_mono i=i|> flip(compare_mono_by(get_indeterminate_weights()))
let sort_poly: 't poly -> 't poly = fun p -> sort rev_cmp_mono p

(* Some indeterminate weights for common elimination operations *)

(* An elimination priority function. For example: elim_oper_args[t,2; s,1; r,1] to eliminate terms t,s,r with priority among them in t. *)
let elim_oper_args term_weight_list : simple_indeterminate -> int list = function
(* TODO The inconvenience here strongly indicates that argument type parameter in polynomials does not work! *)
| Indet(`T(c,t)) when Obj.magic(==) c Term.compare -> (match assq_opt (Obj.magic t) term_weight_list with
  | Some w -> [w]
  | None -> [])
| _ -> []

(* An elimination priority to indeterminates assigned by the given weight function. For example to eliminate 3ʳᵈ variable: elim_indeterminate(function((`Shift|`Mul),3,arg)->1|_->0). *)
let elim_indeterminate weight = todo"function"
(* |`O f -> weight(`Shift, *)


(* Equality for normalized polynomials. *)
let rec poly_eq p = CCList.equal mono_eq p
and mono_eq m n = m==n or CCList.equal indet_eq m n
and indet_eq x y = x==y or match x,y with
  | C a, C b -> Z.equal a b
  | A(T(c,t)), A(T(_,s)) -> c t s = 0
  | X f, X g -> mono_eq f g
  | O f, O g -> CCList.equal(CCPair.equal (=) poly_eq) f g
  | a, b -> a = b

let bit_rotate_right r b = (b lsr r) + (b lsl lnot r) (* for int's lacking 1 bit and r≥0 *)

(* Hash code for normalized polynomials. *)
let rec poly_hash ?(depth=2) p = fold_left (fun h m -> 127*h + mono_hash ~depth m) 0 (take 4 p)
and mono_hash ?(depth=2) = fold_left (fun h x -> 31*h + indet_hash ~depth x) 0
and indet_hash ?(depth=2) = if depth=0 then ~=7 else function
  | X f -> mono_hash ~depth:(depth-1) f
  | O f -> fold_left (fun h (i,fi) -> bit_rotate_right i (h + poly_hash ~depth:(depth-1) fi)) 0 f
  | A(T(c,t)) -> Hashtbl.hash c (* unprecise but avoids inconvenience of carrying hash in addition to c *)
  | x -> Hashtbl.hash x

let coef_view part_of_coef : 't poly -> ('t poly * 't op list) list =
  map(take_drop_while part_of_coef)
  %> group_by ~eq:(snd%%>mono_eq) ~hash:(snd%>mono_hash)
  %> map(fun coef_mono_list -> map fst coef_mono_list, snd(hd coef_mono_list))
  %> sort(snd %%> rev_cmp_mono)

let mul_coef_view = coef_view(function C _ | X _ -> true | _-> false)
let lead_coef = fst % hd % mul_coef_view
let lead_main_ops = snd % hd % mul_coef_view

(* Use this instead of X constructor. *)
let mul_indet i=i|> 
  let rec x = function
  | (X _ | C _ as m)::f -> m :: x f
  | [A I] -> []
  | m -> assert(equational[m]); [X m] in
  sort_poly % map x


(* TODO provide an association in an environment *)
let var_name = string_part_at "y x v u t s r p o n m l k j i h e a"
let term_name t = Term.to_string t

(* E.g. ["-2x";"3y";"-4z"] becomes "-2x﹢3y﹣4z". (In Ubuntu command line “﹢” has width 2 which seems nicer balance between 1 of “+” and 3 of “ + ”, some times.) *)
let concat_plus_minus view = function
| [] -> "0"
| m::p -> map view p
  |> flat_map String.(fun s -> if rcontains_from s 0 '-' then ["﹣"; sub s 1 (length s -1)] else ["﹢"; s])
  |> cons(view m)
  |> String.concat ""

let rec poly_to_string p = concat_plus_minus mono_to_string p

and poly_view_string p = mul_coef_view p |> concat_plus_minus(function
  | [n], m -> mono_to_string(n@m)
  | c, m -> "("^ poly_to_string c ^")"^ mono_to_string m)

and mono_to_string m = group_succ ~eq:indet_eq m
  |> concat_view "" (fun n -> indet_to_string(hd n) ^ match length n with 1->"" | l -> superscript(string_of_int l))
  |> function ""->"1" | "-"->"-1" | "͘"->"1͘" | s->s

and indet_to_string = function
| C a -> (match Z.to_string a with "-1"->"-" | s->s) (* The trivial a=1 does not occur. *)
| A I -> "͘"
| A(V i) -> var_name i
| A(T(_,t)) -> term_name t
| D i -> subscript(var_name i)
| XD i -> "ð"^subscript(var_name i)
| S i -> "∑"^superscript(var_name i)
| X m -> mono_to_string m
| O[i,[[A(V j)];[A I]]] when i=j -> String.uppercase(var_name i)
| O f -> "["^ concat_view "" (fun(i,fi)-> superscript(var_name i) ^ poly_view_string fi) f ^"]"

let pp_poly = to_formatter poly_to_string


(* Arithmetic operations ++, --, >< *)

(* a·m, b·m ↦ (a+b)·m where numbers a,b are implicitly 1 if absent. *)
let mono_add_coefficients i=i|> curry(function
| C a ::m, C b ::n -> if_~=(mono_eq m n) (Z.(a+b) *. m)
| C a ::m, n | m, C a ::n -> if_~=(mono_eq m n) (Z.(a+one) *. m)
| m, n -> if_~=(mono_eq m n) (Z.of_int 2 *. m))

let rec (++) p q : 't poly = match p,q with
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
| _,[] | [],_ -> []
| p, []::q -> p ++ (p><q)
| []::p, q -> q ++ (p><q)
| [[x]], [[y]] -> indeterminate_product x y
| nx::p, (y::m)::q ->
  let n,x = remove_at_idx(-1) nx, hd(rev nx) in
  let xy = [[x]]><[[y]] in
  (* Inputs nx and y::m are valid monomials, so if [[x;y]] is too, then simple concatenation puts indeterminates to the right order: *)
  (if poly_eq xy [[x;y]] then [nx@y::m]
    else [n]><xy><[m])
  ++ ([nx]><q) ++ (p><[y::m]) ++ (p><q)

and indeterminate_product x y =
  let ( *^) op f = mul_indet([[op]]><f) in
  match x,y with
  | C n, C k -> const_op_poly Z.(n*k)
  | _, C o | C o, _ when is0 o -> [[C o]] (* acceptably may eliminate A(rgument) *)
  | x, C k -> [[y;x]]
  | D i, X f -> [[X f; D i]] ++ D i *^[f]
  | XD i, X f -> [[X f; XD i]] ++ XD i *^[f]
  | D i, O f -> (if mem_assq i f then f else (i,[[A(V i)]])::f) (* identity ∘'s in f are implicit *)
    |> map(fun(n,fn)-> D i *^fn >< [[O f; D n]]) (* coordinatewise chain rule *)
    |> fold_left (++) _0 (* sum *)
  | XD i, O f -> fold_left(++)_0 (map(fun(n,fn)-> (todo"X fn⁻¹")><  XD i *^fn >< [[O f; XD n]]) (if mem_assq i f then f else (i,[[A(V i)]])::f)) (* like previous *)
  | D i, XD j when i=j -> [[XD i; D i]; [D i]]
  | O f, X g -> O f *^[g] >< [[O f]]
  | O[i,[[A(V j)];[A I]]], O[l,[[A(V k)];[A I]]] when i=j & l=k & i>l -> [[y;x]] (* TODO composite case *)
  | O[i,[[A(V j)];[A I]]], S l when i=j&i=l -> [[S i]; []]
  | S l, O[i,[[A(V j)];[A I]]] when i=j&i=l -> [[S i]; [C Z.minus_one; eval_at Z.zero ~var:i]; []]
  | O[i,[[C o]]], S l when i=l -> assert(is0 o); [[x]]
  | O[i,[[C c; A I]]], S l when i=l -> Z.(if c < zero
    then map(fun n -> [eval_at~var:i (of_int n + c)]) (range' 0 (Stdlib.abs(to_int c)))
    else map(fun n -> [eval_at~var:i (of_int n)]) (range' 0 (to_int c)))
  | X(S i ::f), S j when i=j -> let _Si_Xf = [[S i]]><mul_indet[f] in (_Si_Xf><[[S i]]) ++ [[S i; X(S i :: f)]] ++ _Si_Xf
  | D _, A I -> []
  | D i, A(V n) -> if i=n then [[]] else []
  | O f, A(V n) -> (match assq_opt n f with Some fn -> fn | _-> [[y]])
  | A _, A _ -> raise(Invalid_argument("indeterminate_product ("^ indet_to_string (Obj.magic x) ^") ("^ indet_to_string (Obj.magic y) ^")"))
  | A _, y -> [[y;x]] (* Delicate: I want to simplify also _,A but only when A is fully right which must be taken into account somewhere at some point! *)
  | x, y -> [[x;y]]

let join_normalizes x y = poly_eq (indeterminate_product x y) [[x;y]]

let poly_alg: 't poly ring = object
  method o = _0
  method plus = (++)
  method minus = (--)
  method times = (><)
  method of_Z = const_op_poly
end

let rec unifiers m' n' =
  let rec loop = function
  | [C a], [C b] -> Z.(let g = gcd a b in [C(b/g)], [C(a/g)])
  | m, ([] | [C _] as n) -> n,m
  | x::m, x'::n when indet_eq x x' -> loop(m,n)
  | x::m, y::n when join_normalizes x y ->
    (* This'd be faster if monomials were encoded in reverse. Only downside were unintuitivity. *)
    (match rev(hd([[y]]><[rev(x::m)])) with
    | y'::xm when indet_eq y y' -> let u,v = loop(xm,n) in u@[y], v
    | _ -> raise Exit)
  | m, y::n when m=[] or join_normalizes y (hd m) -> CCPair.swap(loop(y::n, m))
  | _ -> raise Exit
  in
  try Some(loop(rev m', rev n')) with Exit -> None

let lead_unifiers i=i|> curry(function x::_, y::_ -> unifiers x y | _ -> None)

let (|~>) general special = match lead_unifiers general special with
| Some(u, []) -> Some u
| Some(u, [C _1]) when Z.(equal _1 minus_one) -> Some(_1*.u)
| _ -> None


let superpose p p' = match lead_unifiers p p' with
  | Some(u,u') -> [([u]><p) -- ([u']><p')]
  | None -> []

let leadrewrite r p = match r |~> p with
  | Some u -> Some(p -- ([u]><r))
  | None -> None


module type View = sig type t type v val view: t -> v option end
(* Create index from mapping clause→polynomial, that can be instantiated by empty_with' default_features. *)
module LeadRewriteIndex(T: sig type t end)(P: View with type v= T.t poly) = FV_tree.FV_IDX(struct
  type t = P.t
  let compare = P.view %%> Stdlib.compare (* only used by sets *)
  type feature_func = T.t poly -> int
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


(* List of terms that the given recurrence relates, without duplicates. *)
let terms_in = sort_uniq ~cmp:Term.compare % flat_map(fun m -> match rev m with A(T(_,t))::_->[t] | _->[])

let term0 = Term.const ~ty:term (ID.make "𝟬")
exception RepresentingPolynomial of Term.t poly * Precedence.Weight.t * (simple_indeterminate -> int list)

let poly_as_lit_term_id ?name ?(weight=omega) p =
  let id = ID.make(get_lazy(fun()-> poly_to_string p) name) in
  ID.set_payload id (RepresentingPolynomial(p, weight, get_indeterminate_weights()));
  let term = Term.const ~ty:term id in
  Literal.mk_eq term0 term, term, id

(* Includes semantics of 0 unlike poly_as_lit_term_id. *)
let polyliteral p = if p = _0 then mk_tauto else (fun(l,_,_)->l) (poly_as_lit_term_id p)

let poly_of_id id = id |> ID.payload_find ~f:(fun data -> match data with
  | RepresentingPolynomial(p,w,ord) ->
    if ord != get_indeterminate_weights() then(
      (* If ordering by indeterminate_weights has changed, resort the polynomial. *)
      let p' = sort_poly p in
      ID.set_payload ~can_erase:((==)data) id (RepresentingPolynomial(p', w, get_indeterminate_weights()));
      Some p')
    else Some p
  | _ -> None)

let poly_of_term t = match view t with Const id -> poly_of_id id |_->None

let poly_of_lit = function Equation(o,p,true) when o==term0 -> poly_of_term p |_->None

let polyweight_of_id = ID.payload_find ~f:(function RepresentingPolynomial(_,w,_) -> Some w |_->None)


let map_terms c (f: 'cmp -> 't -> 's) = sort_poly % map(map(function
| A(T((cmp:'cmp),t)) -> A(T(c, f cmp t))
| x -> (Obj.magic: 't op -> 's op) x (*op(erator) x is independ of the type parameter in this branch. *)))

(* Applicable after (mul_)coef_view *)
let to_poly_poly: ('t poly * 't op list) list -> 't poly poly =fun i->i|>
  fold_left (++) _0 % map(fun(coef, arg)->
    let cp = compare_poly_by(get_indeterminate_weights()) in
    map_terms cp embed coef >< embed cp [arg])

let oper_coef_view: 't poly -> 't poly poly = fun i->i|> to_poly_poly % coef_view(function
  | C _ | X[A(V _)] -> true
  | O f -> let var_or_const = function [A(V _)] | [A I] | [C _; A I] -> true | _ -> false
    in for_all(for_all var_or_const) (map snd f)
  | _ -> false)


let if_start pre act string = String.(
  if length pre <= length string & sub string 0 (length pre) = pre
  then Some(act(sub string (length pre) (length string - length pre)))
  else None)

let match_start(type r) ?(split=' ') patterns string =
  let rules = flat_map (fun(pattern,act)-> mapi(fun i p -> p, act i) (String.split_on_char split pattern)) patterns in
  let exception Result of r in
  let rec apply_first = function
  | [] -> raise(Invalid_argument(
    "match_start: None of patterns `"^ String.concat (String.make 1 split) (map fst rules) ^"´ occurs in the beginning of the input\n"^string))
  | (p,act)::rest -> match if_start p act string with
    | Some ok -> raise(Result ok)
    | None -> apply_first rest
  in
  try apply_first rules with Result r -> r

let testterms = map (Term.const ~ty:Type.term % ID.make) ["f";"g"]

let rec poly_of_string s =
  let check_mul_indet p = if equational p then mul_indet p else
    raise(Invalid_argument("Operator polynomial " ^ poly_to_string p ^ " cannot become a multiplier indeterminate in " ^ s)) in
  let split_map_fold split fold mapper = String.split_on_char split %> map mapper %> fold_left_1 fold in
  let p = split_map_fold '-' (--) (
    split_map_fold '+' (++) (
      split_map_fold '.' (fun n m -> check_mul_indet n >< m)
        poly_of_mono_string)) s in
  (* Make argument A I explicit for C and X monomials when correctness demands it. *)
  let p_eq = filter(function A _ :: _ -> true | _ -> false) (map rev p) in
  let p_CX = filter(for_all(function C _ | X _ -> true | _ -> false)) p in
  if p_eq=[] then p else
  if length p_eq + length p_CX = length p then p_eq ++ (p_CX><[[A I]])
  else raise(Invalid_argument(poly_to_string p ^ " from " ^ s ^ " mixes operator and applied monomials"))

and poly_of_mono_string s' = if s'="" then _0 (* neutral in splitting above *) else
  let map_start_factor rules = match_start (map (fun(p,f)-> p,
    fun i s -> f i >< if s="" then [[]] (* neutral in this recursion *) else poly_of_mono_string s
  ) rules) s' in
  map_start_factor[
    ("y x v u t s r p o n m l k j i h e a",fun i->mul_indet[[A(V i)]]);
    ("Y X V U T S R P O N M L K J I H E A",fun i->[[shift i]]);
    ("∑ʸ ∑ˣ ∑ᵛ ∑ᵘ ∑ᵗ ∑ˢ ∑ʳ ∑ᵖ ∑ᵒ ∑ⁿ ∑ᵐ ∑ˡ ∑ᵏ ∑ʲ ∑ⁱ ∑ʰ ∑ᵉ ∑ᵃ",fun i->[[S i]]);
    ("ᵧ ₓ ᵥ ᵤ ₜ ₛ ᵣ ₚ ₒ ₙ ₘ ₗ ₖ ⱼ ᵢ ₕ ₑ ₐ",fun i->[[D i]]);
    ("∂ d",fun i->[[D 1]]);
    ("0 1 2 3 4 5 6 7 8 9",fun i->const_op_poly(Z.of_int i));
    ("f g",fun i->of_term(get_at_idx_exn i testterms));
    ("F G",fun i->mul_indet(of_term(get_at_idx_exn i testterms)));
  ]