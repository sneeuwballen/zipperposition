(* for repl
#mod_use "/home/vnn490/Documents/Zipo/zipperposition/src/prover_phases/RecurrencePolynomial.ml";;
open RecurrencePolynomial;;
#install_printer pp_poly;;
*)
open Logtk
open Interfaces
open Util
open UntypedPrint
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
(* Lexicographic product of comparison functions onto tuples. *)
let ( *** ) c1 c2 (x1,x2) (y1,y2) = match c1 x1 y1 with 0 -> c2 x2 y2 | r -> r
(* Example: lex_list(-) [1;4;2] [1;3;3;3] > 0 where (-) compares positive ints without overflow. *)
let rec lex_list c = curry(function
| [], [] -> 0
| [], _ -> 1
| _, [] -> -1
| x::xx, y::yy -> (c *** lex_list c) (x,xx) (y,yy))
let length_lex_list c = (fun l -> length l, l) %%> (-) *** lex_list c
let lex_array c = to_list %%> lex_list c
let length_lex_array c = (fun l -> Array.length l, l) %%> (-) *** lex_array c

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
  | c -> raise(Invalid_argument("RecurrencePolynomial.subscript: " ^ String.make 1 c ^ " has no subscript form assigned to it")))

module AlgZ: ORD_RING = struct
  type t = Z.t
  let compare = Z.compare
  let of_Z = id
  let zero = Z.zero
  let plus = Z.add
  let inverse = Z.neg
  let times = Z.mul
end

module type RingData = sig
  type t
  val compare: t -> t -> int
  val add: t -> t -> t
  val mul: t -> t -> t
  val of_Z: Z.t -> t
end
module Ring(R: RingData) = struct
  include R
  let o = of_Z Z.zero
  let j = of_Z Z.one
  let _j = of_Z Z.minus_one
  let (=~) a b = 0=compare a b
  let minus = mul _j
  let sub a b = add a (minus b)
end


type var = int
let compare_var = (-)
type atom = I | V of var | T of Term.t
type poly = op list list
and op =
| C of Z.t (* never C 1 *)
| A of atom (* unique and last in a monomial *)
| D of var
| XD of var
| S of var
| X of poly (* never X(p + q) or X(X p · q) *)
| O of (var*poly)list
(* I = Combo[], M = X[[I]], S = O[[I+Z1]] *)

let _0 = []
(* Use instead of C constructor with polynomials. *)
let const_op_poly n = Z.(if equal n zero then [] else if equal n one then [[]] else [[C n]])
let const_eq_poly n = Z.(if equal n zero then [] else if equal n one then [[A I]] else [[C n; A I]])
(* Use instead of C constructor with monomials. *)
let ( *:) c m = Z.(if equal c zero then [C zero] else match m with
  | C a ::n -> if equal (c*a) one then n else C(c*a)::n
  | m -> if equal c one then m else C c :: m)
(* Other safe constructors but those for X are after ordering. *)
let shift i = O[i,[[A(V i)];[A I]]]
let eval_at ~var at = O[var, const_eq_poly at]


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
let is0 m = equal' m m#o

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
  let map2_trim f = pad_tail r#o %>> uncurry(map2 f) %>> trim_tail(is0 r) in
  object
    method compare = rev%trim_head(is0 r) %%> length_lex_list r#compare
    method plus = map2_trim r#plus
    method minus = map2_trim r#minus
    method o = []
  end

(* Monoid (R,·,1) from ring (R,+,·,0,1,...) *)
let multiplicative r : 'r monoid = object method plus = r#times method o = r#of_Z Z.one end

let total_weight w weight = let rec recW i=i|>
  fold_left(fun sum op -> w#plus sum (match op with
    | C _ | A I -> w#o
    | A(V i) -> weight(`V i)
    | A(T t) -> weight(`T t)
    | D i -> weight(`D i)
    | S i -> weight(`S i)
    | O f -> sum' w (map(fun(n,fn)-> weight(`O(n,fn))) f)
    | X f -> fold_left (((%>)recW) % max' w) w#o f (* max weight in f *)
    | XD i -> w#plus (weight(`V i)) (weight(`D i))
  )) w#o in recW

let total_deg = total_weight theZ ~=Z.one

type simple_indeterminate = [`D of var |`O of var*poly |`S of var |`T of Term.t |`V of var]
type indeterminate_shape = [`I|`C|`V|`O|`S|`D|`XD|`X|`T]
let indeterminate_order: indeterminate_shape list ref = ref [`I;`C;`V;`O;`S;`D;`XD;`X;`T]

let indeterminate_shape = function C _->`C | S _->`S | D _->`D | XD _->`XD | O _->`O | X _->`X | A(V _)->`V | A I->`I | A(T _)->`T

let rec compare_mono_by weights = total_weight(infinities int_alg) weights %%> compare_later_dominant

and compare_poly_by weights = lex_list(compare_mono_by weights)

and tiebreak_by weights = lex_list(fun x y ->
  let xw,yw = flip index_of' !indeterminate_order % indeterminate_shape @@ (x,y) in
  if xw<yw then -1 else if xw>yw then 1 else match x,y with
    | A I, A I -> 0
    | C x, C y -> Z.((compare***compare) (abs x, x) (abs y, y))
    | D x, D y | S x, S y | XD x, XD y | A(V x), A(V y) -> compare x y
    | X x, X y -> compare_poly_by weights x y
    | O x, O y -> lex_list(compare_var *** compare_poly_by weights) x y
    | A(T x), A(T y) -> Term.compare x y
    | _ -> assert false (* If xw=yw, variant constructors of x,y must equal and they are covered above. *))

let indeterminate_weights: (simple_indeterminate -> int list) ref = ref~=[]
let rev_cmp_mono = flip(compare_mono_by !indeterminate_weights)


(* Equality for normalized polynomials. *)
let rec poly_eq p = CCList.equal mono_eq p
and mono_eq m n = m==n or CCList.equal indet_eq m n
and indet_eq x y = x==y or match x,y with
  | C a, C b -> Z.equal a b
  | A(T t), A(T s) -> Term.equal t s
  | X f, X g -> poly_eq f g
  | O f, O g -> CCList.equal(CCPair.equal (=) poly_eq) f g
  | a, b -> a = b

(* Hash code for normalized polynomials. *)
let rec poly_hash ?(depth=2) p = fold_left (fun h m -> 127*h + mono_hash ~depth m) 0 (take 4 p)
and mono_hash ?(depth=2) = fold_left (fun h x -> 31*h + indet_hash ~depth x) 0
and indet_hash ?(depth=2) = if depth=0 then ~=7 else function
  | X f | O((_,f)::_) -> poly_hash ~depth:(depth-1) f
  | A(T t) -> Term.hash t
  | x -> Hashtbl.hash x

let coef_view: poly -> (poly * op list) list =
  map(take_drop_while(function C _ | X _ -> true | _-> false))
  %> group_by ~eq:(snd%%>mono_eq) ~hash:(snd%>mono_hash)
  %> map(fun coef_mono_list -> map fst coef_mono_list, snd(hd coef_mono_list))
  %> sort(snd %%> rev_cmp_mono)

let lead_coef = fst % hd % coef_view
let lead_main_ops = snd % hd % coef_view

(* Use this instead of X constructor. *)
let mul_indet = 
  let rec x = function X f ::g -> X f :: x g | m -> [X[m]] in
  sort rev_cmp_mono % map x


(* a·m, b·m ↦ (a+b)·m where numbers a,b are implicitly 1 if absent. *)
let mono_add_coefficients = curry(function
| C a ::m, C b ::n -> if_~=(mono_eq m n) (Z.(a+b) *: m)
| C a ::m, n | m, C a ::n -> if_~=(mono_eq m n) (Z.(a+one) *: m)
| m, n -> if_~=(mono_eq m n) (Z.of_int 2 *: m))

let rec (++) p q : poly = match p,q with
| p, [] -> p
| m::p, n::q -> (match mono_add_coefficients m n with
  | Some mn -> mn :: p++q
  | _ when rev_cmp_mono m n < 0 -> m :: p++(n::q)
  | _ -> q++p)
| p, q -> q++p

let (--) p q =
  let negate = function C a ::m -> Z.neg a *: m | m -> Z.minus_one *: m in
  p ++ map negate q


let rec (><) p q = match p,q with
| _,[] | [],_ -> []
| p, []::q -> p ++ (p><q)
| []::p, q -> q ++ (p><q)
| [[x]], [[y]] -> str x^"·"^str y|<indeterminate_product x y
| nx::p, (y::m)::q ->
  let n,x = remove_at_idx(-1) nx, hd(rev nx) in
  let xy = [[x]]><[[y]] in
  (* Inputs nx and y::m are valid monomials, so if [[x;y]] is too, then simple concatenation puts indeterminates to the right order: *)
  str(n,x,y,m)|<(if poly_eq xy [[x;y]] then [nx@y::m]
    else [n]><xy><[m])
  ++ ([nx]><q) ++ (p><[y::m]) ++ (p><q)

and indeterminate_product x y =
  let ( *^) op f = mul_indet([[op]]><f) in
  match x,y with
  | C n, C k -> const_op_poly Z.(n*k)
  | _, C o | C o, _ when Z.(equal o zero) -> [[C o]]
  | x, C k -> [[y;x]]
  | D i, X f -> [[X f; D i]] ++ D i *^f
  | XD i, X f -> [[X f; XD i]] ++ XD i *^f
  | D i, O f -> (if mem_assq i f then f else (i,[[A(V i)]])::f) (* identity ∘'s in f are implicit *)
    |> map(fun(n,fn)-> D i *^fn >< [[O f; D n]]) (* coordinatewise chain rule *)
    |> fold_left (++) _0 (* sum *)
  | XD i, O f -> fold_left(++)_0 (map(fun(n,fn)-> (todo"X fn⁻¹")><  XD i *^fn >< [[O f; XD n]]) (if mem_assq i f then f else (i,[[A(V i)]])::f)) (* like previous *)
  | D i, XD j when i=j -> [[XD i; D i]; [D i]]
  | O f, X g -> O f *^g >< [[O f]]
  | O[i,[[A(V j)];[A I]]], S l when i=j&i=l -> [[S i]; []]
  | S l, O[i,[[A(V j)];[A I]]] when i=j&i=l -> [[S i]; [C Z.minus_one; eval_at Z.zero ~var:i]; []]
  | O[i,[[C o]]], S l when i=l -> assert Z.(equal o zero); [[x]]
  | O[i,[[C c; A I]]], S l when i=l -> Z.(if c < zero
    then map(fun n -> [eval_at~var:i (of_int n + c)]) (range' 0 (Stdlib.abs(to_int c)))
    else map(fun n -> [eval_at~var:i (of_int n)]) (range' 0 (to_int c)))
  | X[S i ::f], S j when i=j -> [[S i; X[f]; S i]; [S i; X[S i :: f]]; [S i; X[f]]] (* mul_indet not needed *)
  | D _, A I -> []
  | D i, A(V n) -> if i=n then [[]] else []
  | O f, A(V n) -> (match assq_opt n f with Some fn -> fn | _-> [[y]])
  | A _, A _ -> todo"illegal A·A"
  | A _, y -> [[y;x]] (* Delicate: I want to simplify also _,A but only when A is fully right which must be taken into account somewhere at some point! *)
  | x, y -> [[x;y]]


let poly_alg: poly ring = object
  method o = _0
  method plus = (++)
  method minus = (--)
  method times = (><)
  method of_Z = const_op_poly
end

(* Ordered lists representing sums g₁x₁+...+gₙxₙ. Algebraically the direct sum Gᕁ. *)
module Sums(Parameters: sig
  type g val group: g group
  type x val ord: x ord
end) = struct
  open Parameters
  (* type t = (g*x) list *)
  (* It'd be an error to define the type above because the final "include" also defines it. Is there a more idiomatic way—as with signatures one can "include...with t:=t"? *)
  
  let rec (++) p q = match p,q with
  | p, [] -> p
  | (c1,x1)::p1, (c2,x2)::p2 -> let ord = ord#compare x1 x2 in
    if ord = 0 then
      let c = group#plus c1 c2 in
      (if group#o=c then id else cons(c,x1)) (p1++p2)
    else if ord > 0 then (c1,x1) :: p1++q
    else q++p
  | p, q -> q++p
  
  let (--) p q = p ++ map(map_fst(group#minus group#o)) q
  
  let group = object method o=[] method plus=(++) method minus=(--) end
  include Group_of_object(struct
    type t = (g*x) list
    let group = group
  end)
end


module RecPoly(Parameters: sig
  type coef val ring: coef ring
end) = struct
  
  open Parameters
  type var = int
  let compare_var = (-)
  type atom = I | V of var | T of Term.t
  type poly = op list list
  and op = C of Z.t | A of atom | D of var | XD of var | S of var | X of poly | O of (var*poly)list

  let eval_at ~var at = O[var, [[C at; A I]]]

  include Sums(struct
    type g = coef
    let group = (ring :> g group)
    type x = op list
    let ord = todo"ord"
  end)

  let ring: t ring = object
    (* Note: classless “immediate” objects cannot be inherited like {record with ...}, see https://v2.ocaml.org/manual/objectexamples.html#s%3Aimmediate-objects *)
    method o=group#o
    method plus=group#plus
    method minus=group#minus
    
    method of_Z z = [ring#of_Z z, [A I]]
    method times = todo"times"
  end
end

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

and poly_view_string p = coef_view p |> concat_plus_minus(function
  | [n], m -> mono_to_string(n@m)
  | c, m -> "("^ poly_to_string c ^")"^ mono_to_string m)

and mono_to_string m = group_succ ~eq:indet_eq m
  |> concat_view "" (fun n -> indet_to_string(hd n) ^ match length n with 1->"" | l -> superscript(string_of_int l))
  |> function ""->"1" | "-"->"-1" | s->s

and indet_to_string = function
| C a -> (match Z.to_string a with "-1"->"-" | s->s) (* The trivial a=1 should not occur. *)
| A I -> ""
| A(V i) -> var_name i
| A(T t) -> term_name t
| D i -> subscript(var_name i)
| XD i -> "ð"^subscript(var_name i)
| S i -> "∑"^superscript(var_name i)
| X[m] -> mono_to_string m
| X f -> "("^ poly_to_string f ^")"
| O[i,[[A(V j)];[A I]]] when i=j -> String.uppercase(var_name i)
| O f -> "["^ concat_view "" (fun(i,fi)-> superscript(var_name i) ^ poly_view_string fi) f ^"]"

let pp_poly = to_formatter poly_to_string


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
  let product = sum'(object method plus=(><) method o=[[A I]] end) in
  String.split_on_char '-' s |> map(
    String.split_on_char '+' %> map(
      String.split_on_char '.' %> map(
        fun s -> [[X(poly_of_mono_string s)]]
      ) %> product
    ) %> sum' poly_alg
  ) |> function pos::neg -> fold_left (--) pos neg |_-> assert false

and poly_of_mono_string s' = if ~<s'="" then _0 (* neutral in splitting above *) else
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
    ("f g",fun i->[[A(T(get_at_idx_exn i testterms))]]);
    ("F G",fun i->mul_indet[[A(T(get_at_idx_exn i testterms))]]);
  ]