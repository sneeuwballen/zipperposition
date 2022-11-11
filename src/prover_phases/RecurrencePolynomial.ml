open Logtk
open Interfaces
open CCPair
open CCCache
open CCOpt
open CCVector
open CCArray
open CCList
open CCFun
open Stdlib
exception TODO

(* Utilities *)
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

module type PolyData = sig
  module R: RingData
  type indet
  type mono = indet list
  type poly = ( mono) list
  val compare: indet -> indet -> int
  (* Y·X = cof Y X · Y + ... *)
  val commutator_factor: indet -> indet -> poly
  val commutator_surplus: indet -> indet -> poly
end

module Poly(PD: PolyData) = struct
  include PD
  module RingR=Ring(R) open RingR

  let base_noncommuting_product b a = map(flip(@)[b]) (commutator_factor b a) @ commutator_surplus b a

  let rec (++) p q = match p,q with
  | p, [] -> p
  | m::p, n::q when m=n -> raise TODO
  | m::p, n::q when Stdlib.compare m n > 0 -> m :: p++(n::q)
  | p, q -> q++p

  [@@@warning "-8"]
  let rec (><) p q = match p,q with
  | _,[] | [],_ -> []
  | p, []::q -> p ++ (p><q)
  | []::p, q -> q ++ (p><q)
  | [[a]], [[b]] -> base_noncommuting_product a b
  | na::p, (b::m)::q ->
    let n,a = remove_at_idx(-1) na, hd(rev na) in
    ([n]><([[a]]><[[b]])><[m]) ++ ([na]><q) ++ (p><[b::m]) ++ (p><q)
  [@@@warning "+8"]
end


type var = int
let compare_var = (-)
type atom = I | V of var | T of Term.t
type poly = op list list
and op = C of Z.t | A of atom | D of var | XD of var | S of var | X of poly | O of (var*poly)list
(* and indet = On of var * op | Combo of indet list *)
(* I = Combo[], M = X[[I]], S = O[[I+Z1]] *)

let shift i = O[i,[[A(V i)];[A I]]]
let eval_at ~var at = O[var, [[C at; A I]]]

let simplify1 = function
| X[f;g] -> [[X[f]]; [X[g]]]
| X[X f ::g] -> [[X f; X[g]]]
| C i when Z.(equal i one) -> []
| O[] -> []
| a -> [[a]]

let commut a b =
  let (^) op = map(cons op) in
  match a,b with
| _, C o when Z.(equal o zero) -> [[C o]] (* must not merge below via or-pattern *)
| C o, _ when Z.(equal o zero) -> [[C o]]
| C n, C k -> [[C Z.(n*k)]]
| a, C k -> [[b;a]]
| D i, X f -> [[X f; D i]; [X(D i ^f)]]
| XD i, X f -> [[X f; XD i]; [X(XD i ^f)]]
| D i, O f -> map(fun(n,fn)-> [X(D i ^fn); O f; D n]) f |> if mem_assq i f then id else cons[O f; D i]
| XD i, O f -> map(fun(n,fn)-> [(raise TODO(* X fn⁻¹ *)); X(XD i ^fn); O f; XD n]) f
| D i, XD j when i=j -> [[XD i; D i]; [D i]]
| O f, X g -> [[X(O f ^g); O f]]
| O[i,[[A(V j)];[A I]]], S l when i=j&i=l -> [[S i]; []]
| S l, O[i,[[A(V j)];[A I]]] when i=j&i=l -> [[S i]; [C Z.minus_one; eval_at Z.zero ~var:i]; []]
| O[i,[[C o]]], S l when i=l -> assert Z.(equal o zero); [[a]]
| O[i,[[C c; A I]]], S l when i=l -> Z.(if c < zero
  then map(fun n -> [eval_at~var:i (of_int n + c)]) (range' 0 (Stdlib.abs(to_int c)))
  else map(fun n -> [eval_at~var:i (of_int n)]) (range' 0 (to_int c)))
| X[S i ::f], S j when i=j -> [[S i; X[f]; S i]; [S i; X[S i :: f]]; [S i; X[f]]]
| D _, A I -> []
| D i, A(V n) -> if i=n then [[]] else []
| O f, A(V n) -> (match assq_opt n f with Some fn -> fn | _-> [[b]])
| A _, A _ -> raise TODO (* illegal *)
| A _, b -> [[b;a]] (* Delicate: I want to simplify also _,A but only when A is fully right which must be taken into account somewhere at some point! *)
| a, b -> [[a;b]]

(* Equality for normalized polynomials. *)
let rec poly_eq p = CCList.equal mono_eq p
and mono_eq m n = m==n or CCList.equal(curry(function
  | C a, C b -> Z.equal a b
  | A(T t), A(T s) -> Term.equal t s
  | X f, X g -> poly_eq f g
  | O f, O g -> CCList.equal(CCPair.equal (=) poly_eq) f g
  | a, b -> a = b
)) m n

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

let lead_coef = fst % hd % coef_view
let lead_main_ops = snd % hd % coef_view


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
    let ord = raise TODO
  end)

  let ring: t ring = object
    (* Note: classless “immediate” objects cannot be inherited like {record with ...}, see https://v2.ocaml.org/manual/objectexamples.html#s%3Aimmediate-objects *)
    method o=group#o
    method plus=group#plus
    method minus=group#minus
    
    method of_Z z = [ring#of_Z z, [A I]]
    method times = raise TODO
  end
end