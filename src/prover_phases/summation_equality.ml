(* open Batteries opam install batteries + edit src/core/dune *)
open Logtk
open Logtk_parsers
open Logtk_proofs
open Libzipperposition
open Libzipperposition_calculi
open Phases_impl
open Util
open Util.UntypedPrint
open Precedence.Weight
open Comparison
open Monome
open Literals
open Literal.Conv
open Literal
open CCCache
open CCOpt
open CCArray
open CCVector
open CCList
open CCFun
module B = Builtin
open Type
open Term
open Stdlib
module H = Hashtbl
module HT = Hashtbl.Make(struct
  type t = term
  let equal = (==)
  let hash = Term.hash
end)
module HV = Hashtbl.Make(struct
  type t = Term.var
  let equal = (==)
  let hash = HVar.hash
end)

let (~=) x _ = x
let (@@) = CCPair.map_same
let (%%>) = compose_binop
let (%>>) f g x y = g(f x y)
let (<:>) r f = r:=f!r

(* type 't comparison = 't -> 't -> int *)
(* Lexicographic product of comparison functions onto tuples. *)
let ( *** ) c1 c2 (x1,x2) (y1,y2) = match c1 x1 y1 with 0 -> c2 x2 y2 | r -> r

(* Example: length_lex_list c = (fun l -> List.length l, l) %%> Stdlib.compare *** lex_list c *)
let rec lex_list c = curry(function
| [], [] -> 0
| [], _ -> 1
| _, [] -> -1
| x::xx, y::yy -> (c *** lex_list c) (x,xx) (y,yy))

let lex_array c = Array.to_list %%> lex_list c

let sum_list = fold_left (+) 0
let sum_array = Array.fold_left (+) 0

let (|->) i v a = a.(i)<-v; a
let (|=>) i v = (i|->v) % Array.copy

let index_of p l = fst(get_exn(find_idx p l))

(* Search hash table by value instead of by key. *)
let search_hash ?(eq=(=)) table value = H.fold (fun k v found -> if found=None & eq value v then Some k else found) table None

let with_cache_2 c f = curry(with_cache_rec c (uncurry % f % curry))
let with_cache_3 c f = curry(with_cache_2 c (uncurry % f % curry))


(* make constants for debugging *)
let constants = H.create 0
let have ?(infix=false) name par ty = match H.find_opt constants name with
| None ->
  let i = ID.make name in
  if infix then ID.set_payload i (ID.Attr_infix name);
  let c = const (arrow par ty) i in
  H.add constants name c; c
| Some c -> c


exception TODO


(* Given an inference L¹,L²⊢σC, create and put into use an inference  L¹∨D¹, L²∨D² ⊢ σ(C∨D¹∨D²) , where literals L¹ and L² must be eligible. Allow multiple conclusions. *)
let on_eligible_literals(type c)(module Env: Env.S with type C.t=c) name literal_inference =
  let module C = Env.C in
  let lifted_inference c1 c2 =
    let rename = Subst.Renaming.create() in
    let c1_lits, c2_lits = C.lits@@(c1,c2) in
    fold_lits ~eligible:(C.Eligible.res c1) c1_lits |> Iter.flat_map(fun(l1,pos1) ->
    fold_lits ~eligible:(C.Eligible.res c2) c2_lits |> Iter.flat_map_l(fun(l2,pos2) ->
      literal_inference l1 l2 |> map(fun(infered, subst) ->
        let c1_no_l1 = apply_subst_list rename subst (except_idx c1_lits pos1, 0) in
        let c2_no_l2 = apply_subst_list rename subst (except_idx c2_lits pos2, 1) in
        C.create (infered @ c1_no_l1 @ c2_no_l2)
          ~penalty:(max (C.penalty c1) (C.penalty c2))
          ~trail:(C.trail_l[c1;c2])
          Proof.(Step.inference ~tags:[] ~rule:(Rule.mk name) (map (fun c -> C.proof_parent_subst rename c subst) [c1,0; c2,1]))
    ))) |> Iter.to_rev_list
  in
  Env.add_binary_inf name (fun c ->
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
  (* Define an inference on clauses lifting the given literal_inference. *)
  let lifted_inference c1 c2 =
    let open SimplM.Infix in
    let c1_lits, c2_lits = C.lits@@(c1,c2) in
    (* Test partly alternative subsumption to early exit. *)
    if c1==c2 or not(Trail.subsumes (C.trail c1) (C.trail c2)) or Array.length c1_lits > Array.length c2_lits then c2,`Same else
    let rename = Subst.Renaming.create() in
    let exception Changed of C.t in try
    (* With the largest literal of c1 simplify any literal of c2. *)
    match maxlits_l ~ord:(Env.ord()) c1_lits with
    | [l1,pos1] -> fold_lits ~eligible:~= ~=true c2_lits (fun(l2,pos2) ->
      match literal_inference l1 l2 with
      | None -> ()
      | Some(simplified, subst) ->
        let c1_no_l1 = apply_subst_list rename subst (except_idx c1_lits pos1, 0) in
        let c2_no_l2 = except_idx c2_lits pos2 in
        let module S = Superposition.Make(Env) in
        (* Applying subst put c1 to the same scope as c2. *)
        match S.subsumes_with (Array.of_list c1_no_l1, 0) (Array.of_list c2_no_l2, 0) with
        | Some(subst', tags) when
          (* Subsumption test does not see variables in l2, which must not be specialized, and hence we check that they are not bound by subst'. *)
          Iter.for_all(fun(v,_) -> Literal.for_all(not % var_occurs ~var:(cast_var_unsafe v)) l2) (Subst.domain subst')
        ->
          let composed = Subst.(fold (fun s x t -> (if mem s x then update else bind) s x (apply rename subst' t, 0)) subst' subst) in
          raise(Changed(C.create (simplified @ c2_no_l2)
            ~penalty:(C.penalty c2)
            ~trail:(C.trail c2)
            Proof.(Step.simp ~tags ~rule:(Rule.mk name) (map (fun c -> C.proof_parent_subst rename c composed) [c2,1; c1,0]))))
        | _ -> ()
      ); c2,`Same
    | _ -> c2,`Same
    with Changed c2' -> c2',`New
  in
  (* Store index in flex state. *)
  let index_key = get_lazy Flex_state.create_key key_to_index in
  Env.flex_add index_key initial_index;
  let index act c = act (Env.flex_get index_key) c in
  (* Register the inference. *)
  Env.add_rw_simplify(fun c -> SimplM.app_list Iter.(to_rev_list(map lifted_inference (index Index.retrieve_generalizations c))) c);
  (* Register a filter to find active clauses that can be simplified by a “given clause”. It is then upto the inference above to access the “given clause” even if it got simplified. This is possible (only) by having an index that tracks the “given clause”. *)
  Env.add_backward_simplify(C.ClauseSet.of_iter % index Index.retrieve_specializations);
  (* Keep the index up to date. *)
  let open Env.ProofState in
  let update_index when' how' = Signal.on_every when' (Env.flex_add index_key % index how') in
  (* update_index ActiveSet.on_add_clause Index.add; *)
  (* update_index ActiveSet.on_remove_clause Index.remove; *)
  update_index SimplSet.on_add_clause Index.add;
  update_index SimplSet.on_remove_clause Index.remove;
    (* (fun i c -> if Iter.exists((==)c) (Env.get_active()) then i else Index.remove i c) *)




(* Mul | Move term | Moves term² ; c p d f | c p d₁ d₂ f₁ f₂ | c p | repara... *)
module RecurrencePolynomial = struct
type powers = int array
type shifts = One
| Shift of powers * term
| Product of powers * term * powers * term
| Diagonal of int * int * powers * term
type monomial = Z.t * powers * shifts
type poly = monomial list

(* This exception terminates computing or applying unifiers. Partial operations on monomials, such as product, may be considered as its cause. *)
exception MonomialIncompatibility

let is0 = Z.equal Z.zero
let _Q q = app_builtin ~ty:int (Rat q) []
let _Z = _Q % Q.of_bigint
let _z = _Z % Z.of_int

let coordinate_count = ref 0
let fold_coordinates o f = fold_left f o (init !coordinate_count id)

let make_powers f = Array.init !coordinate_count f
let (+^) = Array.map2(+)

(* Make normalized Product-shift of monomial. (Multiplication must commute.) *)
let product(s,f, z,g) = if compare f g < 0
  then Product(s,f, z,g)
  else Product(z,g, s,f)


let varstr name1 n = String.make 1 Char.(chr(code name1 - n))

let termstr t = (flip String.iter (to_string t)
  |> Iter.filter((!=)' ')
  |> Iter.to_string ~sep:"̲" (String.make 1)
)^"̲"

let powers_to_string name1 = String.concat "" % Array.to_list % Array.mapi(fun var -> function
| 0 -> ""
| 1 -> varstr name1 var
| exp -> varstr name1 var ^ superscript(string_of_int exp))

let shifts_to_string = function
| One -> ""
| Shift(s,f) -> powers_to_string 'Z' s ^ termstr f
| Product(s1,f1, s2,f2) -> powers_to_string 'Z' s1 ^ termstr f1 ^"⬝"^ powers_to_string 'Z' s2 ^ termstr f2
| Diagonal(sub, dup, s,f) -> "{"^ varstr 'z' sub ^"↦ "^ varstr 'z' dup ^"}"^ powers_to_string 'Z' s ^ termstr f

let mono_to_string(c,m,f) = match (match str c with "1"->"" | "-1"->"-" | c'->c') ^ powers_to_string 'z' m ^ shifts_to_string f with ""->"1" | "-"->"-1" | s->s

let poly_to_string p = if length p = 0 then "0" else concat_view " ＋ "  mono_to_string p

(* List of terms that the given recurrence relates, without duplicates. *)
let terms_in = sort_uniq ~cmp:Term.compare % flat_map(function
| _, _, One -> []
| _, _, (Shift(_,f) | Diagonal(_,_,_,f)) -> [f]
| _, _, Product(_,f,_,g) -> [f;g])


let mono_total_deg(_,m,s) = sum_array m + match s with
| One -> 0
| Shift(s,_) | Diagonal(_,_,s,_) -> sum_array s
| Product(s1,_, s2,_) -> sum_array s1 + sum_array s2

(* Use through redefine_elimination_priority. *)
let elimination_priority = ref ~= ~=0

(* Comparison functions for shifts, monomials *)

let compare_shifts s z = match s,z with
| One, One -> 0
| One, _ -> -1
| _, One -> 1
| Shift(s,f), Shift(z,g) -> (lex_array(-) *** Term.compare) (s,f) (z,g)
| Shift _, _ -> -1
| _, Shift _ -> 1
| Diagonal(i,i',s,f), Diagonal(j,j',z,g) -> (lex_array(-) *** Term.compare *** (-) *** (-)) (s,(f,(i,i'))) (z,(g,(j,j')))
| Diagonal _, _ -> -1
| _, Diagonal _ -> 1
| Product(s1,f1, s2,f2), Product(z1,g1, z2,g2) -> Term.(lex_array(-) *** lex_array(-) *** compare *** compare) (s1,(s2,(f1,f2))) (z1,(z2,(g1,g2)))

let compare_mono = (fun(c,m,s) -> !elimination_priority m s, (mono_total_deg(c,m,s), (s, (m, c)))) %%>
  (-) *** (-) *** compare_shifts *** lex_array(-) *** Z.compare

let rev_cmp_mono = flip compare_mono

(* TODO Update ... the given polynomials to follow the given new default elimination priority. Other existing polynomials become garbage and must not be used!
 The elimination priority overrides the default comparison order of monomials (which is a total degree reverse lexicographic one). This can be used to derive equations without certain indeterminate powers or operands by saturation. For example from the total degree saturated set {m²+n-1, n²+m+1} using instead n²≺m one rewrites m in first deriving (-n²-1)²+n-1 = n(n³+2n+1) which is independent of m.
 Parameters to priority are list of operator powers and operand term. The elimination preordering must extend divisibility: if monomial M=K⬝N then M has ≥ priority than N. Constant priority (default) leaves the tie-breaking total degree ordering unchanged. See elim_oper_args and elim_indeterminate for other basic options.
 Desing: Having a global switch like this has drawbacks. Namely saturations of polynomial equations cannot be (easily) nested, parallelised or paused-and-resumed. Scalable solution would be to substitute the module by a functor taking the elimination priority as parameter. However that'd be complicated to use because polynomials and/or their operations had to explicitely carry the ordering. *)
let redefine_elimination_priority = (:=)elimination_priority

(* An elimination priority function. For example: elim_oper_args[t,2; s,1; r,1] to eliminate terms t,s,r with priority among them in t. *)
let elim_oper_args weights = 
  let arg_weight = H.(get_or ~default:0 % find_opt(of_seq(List.to_seq weights))) in
  ~=(function
  | One -> 0
  | Shift(_,f) | Diagonal(_,_,_,f) -> arg_weight f
  | Product(_,f,_,g) -> (arg_weight %%> max) f g)

(* An elimination priority to indeterminates assigned by the given weight function. Example use: elim_indeterminate(function((`Shift|`Mul),arg,2)->1|_->0). *)
let elim_indeterminate weight m = let rec this = function
| One -> 0
| Shift(s,f) | Diagonal(_,_,s,f) -> sum_array Array.(map(fun p -> weight(`Mul,p,f)) m +^ map(fun p -> weight(`Shift,p,f)) s)
| Product(s1,f1, s2,f2) -> this(Shift(s1,f1)) + this(Shift(s2,f2))
in this


(* Arithmetic of polynomials etc. Main operations to superpose polynomials are: addition, multiplication by monomial, and lcm of monomials upto leading term. *)

let power0() = make_powers~=0
let _0 = []
let _1 t = [Z.one, power0(), Shift(power0(), t)]
let _1x1 t s = [Z.one, power0(), product(power0(), t, power0(), s)]

let equal_shifts s z = s==z or match s,z with
| Shift(s,f), Shift(z,g) -> s=z & f==g
| Product(s1,f1, s2,f2), Product(z1,g1, z2,g2) -> s1=z1 & f1==g1 & s2=z2 & f2==g2
| Diagonal(a,b,s,f), Diagonal(c,d,z,g) -> a=c & b=d & s=z & f==g
| _ -> false
let (=^) = equal_shifts

let sum_monomials =
  let rec sum_same = function
  | (o,m,s)::p when is0 o -> sum_same p
  | (c,m,s)::(a,n,z)::p when m=n & s=^z -> sum_same(Z.(c+a,m,s)::p)
  | cms::p -> cms :: sum_same p
  | [] -> []
  in
  sum_same % sort rev_cmp_mono

let map_monomials k = sum_monomials % flat_map k

let map_terms k = map_monomials(fun(c,m,s) -> [c, m, match s with
| One -> One
| Shift(s,f) -> Shift(s, k f)
| Product(s,f,z,g) -> product(s, k f, z, k g)
| Diagonal(sub,dup,s,f) -> Diagonal(sub, dup, s, k f)])

type polysubst = Z.t * powers * shifts

let shift_multiplier s m = (let rec loop out = function
  | 0::ss, m::mm -> loop (map (cons(Z.one,m)) out) (ss,mm)
  | s::ss, m::mm -> loop (concat(init(m+1) (fun n -> map (cons(Z.(bin (of_int m) n * of_int s**n) , m-n)) out))) (ss,mm)
  | _ -> out
  in loop [[]] Array.(to_list s, to_list m) |> map(fun l -> fold_left Z.(fun a (c,_) -> c*a) Z.one l, Array.of_list(rev_map snd l)))

let substiply(c,m,x) = map_monomials(fun(a,n,z) ->
  let s = match x with
  | Shift(s,_) | Product(_,_,s,_) | Diagonal(_,_,s,_) -> s
  | _ -> assert false
  in
  shift_multiplier s n |> map(fun(b,sn)->
  (match x with Diagonal(sub, dup, _,_) ->
    sn.(dup) <- sn.(dup)+sn.(sub);
    sn.(sub) <- 0;
  | _->());
  Z.(a*b*c),
  m+^sn,
  match z,x with
  | One, Product(z',f',_,_) -> Shift(z',f')
  | One, _ -> One
  | Shift(z,f), Product(z',f',_,_) -> product(z',f', s+^z,f)
  | Shift(z,f), Diagonal(sub, dup, _,_) -> Diagonal(sub, dup, s+^z, f)
  | Shift(z,f), _ -> Shift(s+^z,f)
  | Product(z1,f1, z2,f2), Shift _ -> Product(s+^z1, f1, s+^z2, f2)
  | Diagonal(sub, dup, z,f), Shift _ ->
    if s.(sub)>0 then raise MonomialIncompatibility; (* Note: this might signal bug? *)
    s.(sub) <- s.(dup);
    let result = Diagonal(sub, dup, s+^z, f) in
    s.(sub) <- 0;
    result
  | _ -> raise MonomialIncompatibility))


let mgu_coef a b = Z.(lcm a b / a, lcm a b / b)
let mgu_pow m m' = (fun n -> make_powers(fun v -> max m.(v) m'.(v) - n.(v))) @@ (m,m')

let rec mgu_shift s s' =
  let require condition = if not condition then raise MonomialIncompatibility in
  match s,s' with
  | Shift(s,f), Shift(s',f') ->
    require(f==f');
    (fun u -> Shift(u,f)) @@ mgu_pow s s'
  | Product(s1,f1, s2,f2), Product(z1,g1, z2,g2) ->
    require(f1==g1 & f2==g2 & s1+^z2 = s2+^z1);
    (fun u -> Shift(u,f1)) @@ mgu_pow s1 z1
  | Diagonal(i,j,s,f), Diagonal(i',j',s',f') ->
    require(i=i' & j=j' & f==f' & s.(i)-s.(j) = s'.(i)-s'.(j));
    (fun u -> Shift((i|->0)u,f)) @@ mgu_pow s s'
  | Product(s1,f1,s2,f2), Shift(s',f') ->
    if f'==f1 then(
      let u,u' = mgu_pow s1 s' in
      Shift(u',f2), Product(u'+^s2,f2, u,f2)
    )else((* copy of above with 1↔2 *)
      require(f'==f2);
      let u,u' = mgu_pow s2 s' in
      Shift(u',f1), Product(u'+^s1,f1, u,f1)
    )
  | Diagonal(sub, dup, s,f), Shift(s',f') ->
    require(f==f');
    let u,u' = mgu_pow s s' in
    let h = max u.(sub) u.(dup) in
    u'.(sub) <- u'.(sub) + h - u.(sub);
    u'.(dup) <- u'.(dup) + h - u.(dup);
    u.(sub) <- h; u.(dup) <- h;
    Shift(u,f), Diagonal(sub, dup, u',f)
  | Shift _, _ -> CCPair.swap(mgu_shift s' s)
  | _ -> raise MonomialIncompatibility

(* Most general lead unifier—a pair of operator monomials *)
let rec mgu (c,m,s) (c',m',s') =
  let uc, uc' = mgu_coef c c' in
  let um, um' = mgu_pow m m' in
  let us, us' = mgu_shift s s' in
  (uc,um,us), (uc',um',us')

let (|~>) general special = match mgu general special with
| (c,m,s), (i, o, Shift(o',_)) when Z.(equal one (abs i)) & sum_array(o+^o') = 0 -> Z.(i*c,m,s)
| _ -> raise MonomialIncompatibility


(* polynomial + polynomial *)
let (++) p r = sum_monomials(p@r)

(* constant × polynomial *)
let ( *:) a = if is0 a then ~=_0 else map Z.(fun(c,m,s) -> a*c,m,s)

(* polynomial - polynomial *)
let (--) p r = p ++ Z.of_int(-1)*:r


(* Calculate superposition between two polynomials pⱼ (j=1,2) that represent equations m⬝pⱼ=0 where m runs over all monomials. A result is m₁p₁ - m₂p₂ where mⱼ are least monomials such that leading terms in mⱼpⱼ equal. Currently result is unique, if superposition is possible. *)
let superpose p p' =
  if p=_0 or p'=_0 then [] else try
    let u,u' = mgu (hd p) (hd p') in
    [substiply u p -- substiply u' p']
  with MonomialIncompatibility -> []

(* Try rewrite leading monomial of p by r. *)
let leadrewrite r p =
  if r=_0 or p=_0 then None else try
    Some(p -- (substiply (hd r |~> hd p) r))
  with MonomialIncompatibility -> None


(* Express p = a*:n**:[c,m,f] ++ p' and then call act(a,n)(c,m,f)p', otherwise nomatch. If m=[x⁻¹], match it to any xᵏ with k>0 maximal in the term in question. *)
(* [@@@warning "-8"]
let separate (c,m,f) p nomatch act =
  let rec find_cmf r = function
  | [] -> nomatch
  | (c',m',f' as k)::p' -> (try if f != f' then find_cmf (k::r) p' else
    let m = match m with
    | [n] when n.exp<0 -> [get_exn(find_opt(fun n' -> n' =^ hd(set_exp n'.exp n)) m')]
    | _ -> m
    in
    let n = get_exn(div_factor m m') in
    let (c'',_,_)::_nm = n**:[c,m,f] in
    let a = get_exn(c' / c'') in
    act (a,n) (c,m,f) (rev_append r p' -- a*:_nm)
    with Invalid_argument _ -> find_cmf (k::r) p')
  in
  find_cmf [] p
[@@@warning "+8"] *)


let reparameterise structured_substitutes = map_monomials(fun(c,m,s)->
  structured_substitutes |> map(fun(var, base, scales) ->
    if sum_list scales = 0 then
      Z.(of_int base**m.(var) * c, (var|=>0)m, s)
    else
      (* Diagonal's cannot be nested which becomes a problem here! *)
      raise TODO))


(* Find r, q⬝arg s.t. p = (old - by)⬝q⬝arg + r when given indeterminate old, coefficient term by, target term arg, and polynomial p whose indeterminates commute with old. Note that r is "p evaluated at old=by". *)
let substitute_division var arg p =
  p |> map_monomials(fun(c,m,s)-> [c, m, match s with
    | One -> One
    | Shift(s,f) -> Shift((if f==arg then (var|=>0) s else s), f)
    | _ -> raise MonomialIncompatibility]),
  p |> map_monomials(function
    | _, _, One -> []
    | c, m, Shift(s,f) -> if f==arg then init s.(var) (fun j -> c, m, Shift((var|=>j) s, f)) else [c,m,Shift(s,f)]
    | _ -> raise MonomialIncompatibility)
  (* separate (_z 1, old, arg) p (p,_0) (fun (c,m) _ p' ->
    let r,q = substitute_division old by arg ((by><c)*:m**:_1 arg ++ p') in
    r, q ++ c*:m**:_1 arg) *)



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
  let sum value p = match hd p with
  | _, m, One -> value 0 m [|0|]
  | _, m, Shift(s,_) -> value 1 m s
  | _, m, Product(s1,_,s2,_) -> value 2 m (s1+^s2)
  | _, m, Diagonal(_,_,s,_) -> value 3 m s in
  [
    "monomial form", sum(fun f _ _ -> f);
    "total degree", sum(fun _ m s -> sum_array m + sum_array s);
    "shift degree", sum(fun _ _ s -> sum_array s);
    "coefficient degree", sum(fun _ m _ -> sum_array m);
    "1ˢᵗ var. degree", sum(fun _ m s -> m.(0) + s.(0));
    (* then: multiset of indeterminates... except that only ID multisets are supported *)
  ]


(* Embedding polynomials to terms and literals. Does not preserve equality. *)

let term0 = const ~ty:term (ID.make "𝟬")

exception RepresentingPolynomial of poly * Precedence.Weight.t * (powers->shifts->int)

let poly_as_lit_term_id ?name ?(weight=omega) p =
  let id = ID.make(get_lazy(fun()-> poly_to_string p) name) in
  ID.set_payload id (RepresentingPolynomial(p, weight, !elimination_priority));
  let term = const ~ty:term id in
  mk_eq term0 term, term, id

let poly_of_id id = id |> ID.payload_find ~f:(fun data -> match data with
  | RepresentingPolynomial(p,w,ord) ->
    if ord != !elimination_priority then(
      (* If the elimination_priority has changed, update the polynomial. *)
      let p' = sort rev_cmp_mono p in
      ID.set_payload ~can_erase:((==)data) id (RepresentingPolynomial(p', w, !elimination_priority));
      Some p')
    else Some p
  | _ -> None)

let poly_of_term t = match view t with Const id -> poly_of_id id |_->None

let poly_of_lit = function Equation(t,p,true) when t==term0 -> poly_of_term p |_->None

let polyweight_of_id = ID.payload_find ~f:(function RepresentingPolynomial(_,w,_) -> Some w |_->None)


(* quick manual data entry *)
(* let _p s = let is_low c = String.lowercase c = c in
  let rec go = function
  | v::e::rest -> go rest @ [{exp=int_of_string e; var=122-Char.(code(lowercase v.[0])); base=if is_low v then`Mono else`Move(_z 1)}]
  | _ -> []
  in
  go String.(split_on_char ' ' (trim(concat""[s;" 1"])))
let _P = map_monomials(fun(c,s,t)-> [_z c, _p s, have t [] int]) *)

end






module MakeSumSolver(MainEnv: Env.S) = struct
module C = MainEnv.C
(* module Ctx = MainEnv.Ctx *)
module R = RecurrencePolynomial

let parents_as_such = map (fun p -> C.proof_parent_subst (Subst.Renaming.create()) (p,0) Subst.empty)

let polyliteral p = if p = R._0 then mk_tauto else (fun(l,_,_)->l) (R.poly_as_lit_term_id p)

let definitional_poly_clause p = C.create [polyliteral p] ~penalty:1 ~trail:(C.trail_l[]) Proof.Step.trivial

let replace_lit_by_poly c old_lit_index p =
  C.create ~penalty:(C.penalty c) ~trail:(C.trail c)
    (polyliteral p :: except_idx (C.lits c) old_lit_index)
    Proof.(Step.inference ~rule:(Rule.mk "represent recurrence by polynomial") (parents_as_such [c]))

let polys_in = Iter.(concat % map(filter_map R.poly_of_lit % of_array % C.lits))

let polys_of_2_lits no yes l1 = match R.poly_of_lit l1 with
| None -> ~=no
| Some p1 ->
  fun l2 -> match R.poly_of_lit l2 with
  | None -> no
  | Some p2 -> yes p1 p2

let superpose_poly = polys_of_2_lits [] (R.superpose %>> map(fun p -> [polyliteral p], Subst.empty))

let rewrite_poly = polys_of_2_lits None (R.leadrewrite %>> CCOpt.map(fun p -> [polyliteral p], Subst.empty))


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
  
  let module LRI = R.LeadRewriteIndex(struct type t=C.t type v=R.poly
    let view c = match Literals.maxlits_l ~ord:(PolyEnv.ord()) (C.lits c) with
    | [largest,_] -> R.poly_of_lit largest
    | _ -> None
  end) in
  add_simplify_in_context env "lead-rewrite" rewrite_poly 
    (module LRI) (LRI.empty_with' R.default_features);
  on_eligible_literals env "sup. poly." superpose_poly;
  PolyEnv.add_is_trivial (Array.mem mk_tauto % C.lits);
  env,
  fun clauses -> 
    
    match saturate_in env (Iter.to_rev_list clauses) with
    | Error e -> raise(Failure e)
    | Ok(_, Unsat _) -> PolyEnv.C.ClauseSet.to_iter(PolyEnv.get_empty_clauses())
    | Ok _ -> PolyEnv.get_clauses()


let coords = ref []
(* TODO choose one of these to use *)
(* let var_term_table = H.create 6
let var_term = H.find var_term_table
let var_index term = match search_hash ~eq:(==) var_term_table term with
| Some v -> v
| None ->
  let new_index = H.length var_term_table in
  H.add var_term_table new_index term;
  new_index *)
let var_index term = index_of ((==)term) !coords
let var_term index = nth !coords index

let is_summation s = match view s with
| Const{name="sum"} -> true
| _ -> false

let to_Z t = match view t with
| AppBuiltin(B.Int z, []) -> z
| _ -> raise Exit
let is_Z t = try snd(to_Z t, true) with Exit -> false
let to_int = Z.to_int % to_Z

(* Explicit integer constant or simple constant suitable as a coordinate variable. *)
let is_atom t = is_Z t or ty t == int & match view t with
| Const _ -> true
| App(f,x) -> for_all is_Z x
| _ -> false

let is_free t = ty t == int & is_var t

let recurrence_table = HT.create 32
let add_recurrence t r = HT.add recurrence_table t
  (match HT.find_opt recurrence_table t with
  | None -> [r]
  | Some old_r -> r::old_r)


type encoding_phase_info = {coords: term list; symbols: term list; specialise: term HV.t}

let trivC = C.create ~penalty:1 ~trail:(C.trail_l[]) [Literal.mk_tauto] Proof.Step.trivial
let feed = MainEnv.FormRename.get_skolem ~parent:trivC ~mode:`SkolemRecycle

let no_plus = map(fun s -> match view s with
| AppBuiltin(B.Sum, [_;x;y]) when is_Z x -> y
| AppBuiltin(B.Sum, [_;x;y]) when is_Z y -> x
| _ -> s)

let rec term_to_poly info t =
  if ty t != int then raise Exit;
  match view t with
  | AppBuiltin(B.(Sum|Difference) as op, [_;x;y]) ->
    R.(if op=B.Sum then (++) else (--)) (term_to_poly info x) (term_to_poly info y)
  | AppBuiltin(B.Product, [_;c;x]) -> R.(try to_Z c *: term_to_poly info x
    with Exit -> raise TODO)
  | AppBuiltin(B.Int n, _) ->
    if R.is0 n then [] else R.[n, power0(), One]
  | _ when memq t info.coords ->
    R.[Z.one, Array.of_list(mapi(fun i s -> if s==t then 1 else 0) info.coords), One]
  | App(f,p) when exists (Unif.FO.matches ~pattern:(app f (no_plus p))) info.symbols ->
    to_iter info.symbols (fun s -> try
      Unif.FO.matching_same_scope ~scope:0 ~pattern:(app f (no_plus p)) s
      |> Subst.iter(fun (v,_) (b,_) -> HV.add info.specialise (cast_var_unsafe v) (of_term_unsafe b))
    with Unif.Fail -> ());
    R._1 t
  | App(f,p) when for_all(flip memq info.coords) p ->
    R._1 t
  | _ -> raise Exit

let equation_to_recpoly info literal = match to_form literal with SLiteral.Eq(t,s) ->
  (try Some R.(term_to_poly info t -- term_to_poly info s)
  with Exit -> None)
|_->None

(* Associate the clause as a recurrence polynomial to appropriate terms. To be used in initialization. *)
let register_if_clause_is_recurrence info clause =
  fold_lits ~eligible:(C.Eligible.res clause) (C.lits clause) Subst.(fun(lit,place)->
    equation_to_recpoly info lit |> CCOpt.iter List.(fun p ->
      let pair_with (v:Term.var) (x:Term.t) : var Scoped.t * term Scoped.t = ((v:>var),0), ((x:>term),0) in
      let substs = HV.to_seq_keys info.specialise
      |> of_seq_rev
      |> sort_uniq(HVar.compare Type.compare)
      |> map_product_l(fun v -> map(pair_with v) (sort_uniq Term.compare (HV.find_all info.specialise v)))
      |> map of_list in
      substs |> iter R.(fun s ->
        let s' t = of_term_unsafe(apply (Renaming.create()) s ((t:Term.t:>term),0)) in
        let sp = R.map_terms s' p
        |> map_monomials(fun(c,m,s)-> [c, m, match s with
        | Shift(z,g) ->
          let z = Array.copy z in
          let g' = match view(s' g) with
          | App(f,p) -> app f (p |> map(fun t -> match view t with
            | AppBuiltin(B.Sum, [_;x;y]) when is_Z x ->
              z.(index_of ((==)y) info.coords) <- to_int x; y
            (* symmetric copy *)
            | AppBuiltin(B.Sum, [_;y;x]) when is_Z x ->
              z.(index_of ((==)y) info.coords) <- to_int x; y
            | _ -> t))
          | _ -> raise TODO in
          Shift(z,g')
        | s -> s]) in
        terms_in sp |> iter(flip add_recurrence (replace_lit_by_poly clause place sp)))))


(* Try to separate non-trivial eligible recurrence and the rest of the literals from the given clause. *)
let view_poly_clause c = fold_lits ~eligible:(C.Eligible.res c) (C.lits c)
  |> Iter.find R.(fun(l,_)-> match poly_of_lit l with
    | Some p when p!=_0 -> Some(p, CCArray.filter((!=)l) (C.lits c))
    | _ -> None)


(* Filter eligible non-trivial polynomial recurrences by the given condition. *)
let filter_recurrences ok_poly = Iter.filter(fun c ->
  fold_lits ~eligible:(C.Eligible.res c) (C.lits c)
    |> Iter.exists R.(fun(l,_) -> match poly_of_lit l with
      | Some p -> p!=_0 & ok_poly p
      | _ -> false))

(* Filter those clauses that have eligible polynomial with leading term M t where some monomial M operates on an OK term t. *)
let filter_recurrences_of ok_term = filter_recurrences R.(hd %> function
  | _, _, Shift(_,t) -> ok_term t
  | _ -> false)


let rec recurrences_of t = match HT.find_opt recurrence_table t with
| Some r -> r
| None ->
  let r = Iter.to_rev_list(match view t with
  | AppBuiltin(B.Sum, [_;x;y]) -> propagate_affine x y t
  | AppBuiltin(B.Difference, [_;x;y]) -> propagate_affine x ~coef2:Z.minus_one y t
  (* TODO symmetric copy *)
  | AppBuiltin(B.Product, [_;x;y]) when is_Z x -> propagate_affine ~coef1:(to_Z x) x (R._z 0) t
  | AppBuiltin(B.Product, [_;x;y]) when is_atom x -> raise TODO
  | AppBuiltin(B.Product, [_;x;y]) -> propagate_times x y t
  | App(s,[m;f]) when is_summation s -> propagate_sum m (var_index(feed f)) (app f [feed f]) t
  | _ -> Iter.empty)
  in
  HT.add recurrence_table t r;
  r

and propagate_affine ?(coef1=Z.one) t ?(coef2=Z.one) s t_plus_s =
  let env, saturate = make_polynomial_environment() in
  R.redefine_elimination_priority(R.elim_oper_args[t,2; s,2; t_plus_s,1]);
  let poly x = R.(if is_Z x then [to_Z x, power0(), One]
    else if is_atom x then [Z.one, (var_index x |-> 1)(power0()), One]
    else _1 x) in
  Iter.of_list(recurrences_of t @ recurrences_of s)
  |> Iter.cons(definitional_poly_clause R.(_1 t_plus_s -- coef1*: poly t -- coef2*: poly s))
  |> saturate
  |> filter_recurrences_of((==)t_plus_s)

and propagate_times t s t_x_s =
  let env, saturate = make_polynomial_environment() in
  R.redefine_elimination_priority(R.elim_oper_args[t,2; s,2; t_x_s,1]);
  (* Computing dimension: Max size of a set S of indeterminates s.t. following. A leading element of basis is interpreted as a set of its indeterminates A. Demand A⊈S for all such A. *)
  let module S = Int_set in
  let to_set powers = CCArray.foldi (fun present var pow -> (if pow=0 then id else S.add var) present) S.empty powers in
  let shift_vars_on tl = sort_uniq ~cmp:(-) % flat_map(function
  | _, _, R.Shift(s,f) when memq f tl -> S.to_list(to_set s)
  | _ -> [])
  in
  let init_recurrences = Iter.of_list(recurrences_of t @ recurrences_of s)
    |> saturate
    |> filter_recurrences_of(fun x -> x==t or x==s)
  in
  let var_set = shift_vars_on[t;s] (concat(of_iter(polys_in init_recurrences))) in
  let total_dim = length var_set in
  let dimension_wrt x clauses =
    let heads = Iter.to_rev_list(polys_in clauses) |> flat_map(fun p ->
      match hd p with
      | _, _, R.Shift(s,x') when x'==x -> [to_set s]
      | _ -> [])
    in
    let rec size_nonsuperset s = function
    | [] -> S.cardinal s
    | v::vars ->
      if exists (flip S.subset s) heads then 0
      else max (size_nonsuperset (S.add v s) vars) (size_nonsuperset s vars)
    in
    size_nonsuperset S.empty var_set
  in
  let goal_dim = dimension_wrt t init_recurrences + dimension_wrt s init_recurrences - total_dim in
  let module SubEnv = (val env) in
  SubEnv.add_unary_inf "termination test" (fun _ ->
    if goal_dim >= dimension_wrt t_x_s (SubEnv.get_active())
    (* TODO reliably terminate—cannot timeout (see do_step in saturate.ml line 255), nor generate ⊥ (important becomes redundant and removable) *)
    then SubEnv.get_passive() C.mark_redundant;
  []);
  init_recurrences
  |> Iter.cons(definitional_poly_clause R.(_1 t_x_s --  _1x1 t s))
  |> saturate
  |> filter_recurrences_of((==)t_x_s)

and propagate_sum set sum_var s sum =
  let env, saturate = make_polynomial_environment() in
  let bad = ref[] in
  let nice = ref true in
  let sum_fobic = function
  | (_,_,t) when t!=s -> 0
  | (`Mul,v,_) when v=sum_var -> 1
  | (`Mul,v,_) -> 0
  | (_,v,_) -> try fst(0, (if v=sum_var then near_bijectivity_error else near_commutation_error v) set sum_var)
  with Exit -> 1
  in
  R.redefine_elimination_priority(R.elim_indeterminate sum_fobic);
  (* Note: Unfortunately post-saturation processing here reveals all the structural details of recurrence polynomials and so violates encapsulation. *)
  let rec_of_sum_with_bad_terms = Iter.of_list(recurrences_of s)
  |> saturate
  |> filter_recurrences R.(fun p -> fold_coordinates true (fun pass v ->
    pass & for_all(function
      | _, m, One -> true
      | _, m, Shift(s,f) -> 0 = m.(v)*sum_fobic(`Mul,v,f) + s.(v)*sum_fobic(`Shift,v,f)
      | _ -> false
    )p))
  |> Iter.map R.(fun c ->
    (* Above filtering ⟹ get_exn OK *)
    let p, lits = get_exn(view_poly_clause c) in
    let p' =
      (* Eliminate shifts w.r.t. summation variable. *)
      let e = near_bijectivity_error set sum_var in
      let r,q = substitute_division sum_var s p in
      e q ++ r
      (* Pull out shifts w.r.t. all remaining variables. Multipliers get pulled out implicitly. *)
      |> flip fold_coordinates (fun p v -> p |> map_monomials(function
        | c, m, Shift(z,t) as cmZt ->
          let e = near_commutation_error v set sum_var in
          (* Cut z into two at v. Important to get ∑ₙMK - MK∑ₙ = eᴹK + Meᴷ right. *)
          let outZ, inZ = power0(), power0() in
          blit z 0 outZ 0 v;
          blit z v inZ v (!coordinate_count - v);
          (* c m outZ Vᵃ inZ t ↦ ∑ᵢ₊ⱼ₌ₐ₋₁ c m outZ Vʲ e(Vⁱ inZ t) *)
          cmZt :: concat(init z.(v) (fun j ->
            substiply (c, m, Shift((v|->j) outZ, t)) (e [Z.one, power0(), Shift((v|->z.(v)-1-j) inZ, t)])))
        | other -> [other]
      ))
      (* Replace s by sum=∑s, and record excess terms to eliminate later. *)
      |> map_monomials(fun(c,m,z) -> [c, m, match z with
        | One -> raise TODO (* ∑m over the set *)
        | Shift(z,s') when s'==s -> Shift(z,sum)
        (* | Diagonal(_,_,_,s') when s'==s -> nice:=false; z *)
        | Shift(_,f) | Diagonal(_,_,_,f) -> bad:= add_nodup ~eq:(==) f !bad; z
        (* | _ -> z]) *)
        | _ -> assert false])
    in
    C.create_a ~penalty:(C.penalty c) ~trail:(C.trail c)
      (Array.append [|polyliteral p'|] lits)
      Proof.(Step.inference ~rule:(Rule.mk "pull recurrence out of sum") (parents_as_such [c]))
  )
  (* Force the iteration through to decide necessity of resaturation. *)
  |> Iter.persistent in
  if !bad=[] then rec_of_sum_with_bad_terms else(
    R.redefine_elimination_priority(R.elim_oper_args((sum,1)::map(fun f -> f,2)!bad));
    Iter.of_list(flat_map recurrences_of !bad)
    (* Iter.of_list(recurrences_of s) *)
    |> Iter.append rec_of_sum_with_bad_terms
    |> saturate
  )
  |> filter_recurrences R.(fun r ->
    exists(function _,_,Shift(_,sm) when sm==sum ->true | _->false) r
    & for_all(function _,_,Shift(_,t) when subterm ~sub:(var_term sum_var) t ->false | _->true) r)

(* For set S, sum_var n, defined by ∑ⁿ⁼ᔆ(N-1) and maps n↦(S Δ NS) *)
and near_bijectivity_error set var = near_either_error var set var
(* For var m, set S, sum_var n, defined by ∑ⁿ⁼ᔆM - M∑ⁿ⁼ᔆ and maps n↦(Sₘ Δ Sₘ₊₁) *)
and near_commutation_error v = near_either_error v

and near_either_error =
  let eq ((v,s),n) ((v',s'),n') = v=v' & s==s' & n=n' in
  with_cache_3 (lru 16 ~eq) ~=R.(fun op_var set sum_var ->
    (* TODO actually deduce end_points by superposition from op_var, set, sum_var *)
    let end_points =
      if op_var = sum_var then [1,1,`Dup(var_index set); -1,1,`None]
      else if op_var = var_index set then [-1,1,`Dup op_var]
      else []
    in
    map_monomials(fun(c,m,s) ->
      assert(m.(sum_var)=0); (* those must be eliminated before these errors are meaningful. *)
      match s with
      | One -> _0
      | Shift(s,f) -> end_points |> flat_map(fun(sign, base, scale) ->
        let c = Z.( * ) (Z.of_int sign) c in
        match scale with
        |`None -> [c, m, Shift((sum_var|=>0) s, replace ~old:(var_term sum_var) ~by:(_z base) f (*sum_var↦base*))]
        |`Dup var ->
          [c, m, if subterm ~sub:(var_term var) f
            then Diagonal(sum_var, var, (sum_var |=> s.(sum_var)+base) s, f)
            else Shift((var |-> s.(sum_var)+base) ((sum_var|=>0) s), replace ~old:(var_term sum_var) ~by:(var_term var) f)]
        | _ -> raise TODO)
      | _ -> assert false))


let try_prove_sum_equality clause = match
  fold_lits ~eligible:(C.Eligible.res clause) (C.lits clause) 
  |> Iter.find(fun(lit, place) ->
    (* Top-level ∑ must exist. *)
    if Literal.fold (fun ok t -> ok or match view t with App(s,_) -> is_summation s | _->ok) false lit
    then match to_form lit with SLiteral.Neq(lhs,rhs) ->
      let main_expr = app_builtin ~ty:int B.Difference [of_ty int; lhs; rhs] in
      let open Term.Set in
      let coord_set, sym_set = ref empty, ref empty in
      let rec walk t  = if not(is_Z t) & ty t == int then
        if is_atom t then coord_set <:> add t
        else match view t with
        | AppBuiltin(B.(Sum|Difference|Product), [_;x;y]) -> walk x; walk y
        | App(s, [m;f]) when is_summation s -> walk m; walk(app f [feed f])
        | App(f,x) -> List.iter walk x; if List.for_all is_atom x then sym_set <:> add t
        | _ -> ()
      in
      walk main_expr;
      (* Term.Seq.subterms ~include_builtin:true main_expr (fun t ->
        if not(is_Z t) then
          if is_atom t then coord_set <:> add t
          else if ty t == int then match view t with
          | App(f,x) -> if List.for_all is_atom x then sym_set <:> add t
          | _ -> ()); *)
      (* TODO better initialization *)
      R.coordinate_count := cardinal !coord_set;
      MainEnv.get_clauses() (register_if_clause_is_recurrence{
        coords = to_list !coord_set;
        symbols = to_list !sym_set;
        specialise = HV.create 4});
      coords := to_list !coord_set;
      to_iter !sym_set (fun s -> match view s with App(_,x) ->
        to_iter(diff !coord_set (of_list x)) (fun v -> add_recurrence s R.(
          let v_const = [Z.one, power0(), Shift((var_index v |-> 1)(power0()), s)] -- _1 s in
          C.create [polyliteral v_const] ~penalty:1 ~trail:(C.trail_l[])
            Proof.(Step.inference [] ~rule:(Rule.mk "constantness"))))
        |_-> assert false);
      let r = recurrences_of main_expr in
      ~<recurrence_table;
      print_endline("Refuting "^ str lit);
      print_endline "Final recurrences:";
      List.iter (print_endline%str) r;
      print_endline "————— summation_equality: Missing induction phase ⇒ terminated —————";
      exit 1
    |_->None
    else None)
with
| Some conclusions -> conclusions
| None -> []


let step text parents lits =
  C.create lits ~penalty:1 ~trail:(C.trail_l[]) (if parents=[]
  then (if text="goal" then Proof.Step.goal' else Proof.Step.assert') ~file:"" ~name:text ()
  else Proof.Step.inference ~tags:[] ~rule:(Proof.Rule.mk text) (map (fun p -> C.proof_parent_subst (Subst.Renaming.create()) (p,0) Subst.empty) parents))


(*
let test_hook clause =
  let subenv, saturate = make_polynomial_environment() in
  let eq0' p = C.create (List.map polyliteral p) ~penalty:1 ~trail:(C.trail_l[]) (Proof.Step.assert' ~file:"" ~name:"" ()) in
  let split_or k d s = match String.split_on_char k s with
  | [a;b] -> a,b | [a] -> if k='.' then d,a else a,d | _ -> raise Exit in
  let eq0 ss = eq0'[R._P(List.map(fun cmt ->
    let c,mt = split_or '.' "1" (String.trim cmt) in
    let m,t = split_or '\'' "" mt in
    int_of_string c, m, t
  ) (String.split_on_char '+' ss))] in

  R.redefine_elimination_priority String.(fun m f -> 
    match view f with
    | Const f when contains f.name '*' -> 1 
    | Const f when contains f.name '`' -> -1
    | _ -> 0
    (* let e = ref 0 in
    let _= List.map(function {R.base=`Mono; var=1; exp} -> e:=exp; () | _->()) m in
    !e *)
  );
  
  let _ = saturate_in subenv R.([
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
    (* eq0"x 1 X'(ˣᵧ) + -1.y 1 X'(ˣᵧ) + X'(ˣᵧ) + -1.x'(ˣᵧ) + -1.'(ˣᵧ)"; eq0"y 1 Y'(ˣᵧ) + Y'(ˣᵧ) + -1.x'(ˣᵧ) + y'(ˣᵧ)"; eq0"'f` + -1.y 2'(ˣᵧ)"; *)
    (* Change priority for ↓ *)
    (* eq0"x 1 X'f + -1.y 1 X'f + X'f + -1.x'f + -1.'f"; eq0"y 2 Y'f + -1.y 1 x'f + y 2'f + -1.x'f + y'f"; *)

    (* eq0"-1.x 2 X 2'∑f+-2.x 1 X 2'∑f+-1.X 2'∑f + 3.x 2 X'∑f+9.x 1 X'∑f+6.X'∑f + -2.x 2'∑f+-6.x'∑f+-4.'∑f"; eq0"x 1 X'g + -2.x'g + -4.'g"; eq0"'h` + -1.'∑f + 'g"; *)
    eq0'[R._0]] |> remove_at_idx(-1)) in
  let module SubEnv = (val subenv) in
  [step "" (Iter.to_rev_list(SubEnv.get_clauses())) []]
*)


(* Setup to do when MakeSumSolver(...) is called. *);;
(* MainEnv.add_unary_inf "test" test_hook; *)
MainEnv.add_unary_inf "recurrences for ∑" try_prove_sum_equality
end


(* Is this extension enabled? Set by a command line option. *)
let sum_by_recurrences = ref true

(* Define name and setup actions required to registration of this extension in libzipperposition_phases.ml *)
let extension = RecurrencePolynomial.{
  Extensions.default with
  name = "∑";
  env_actions = [fun env -> if !sum_by_recurrences then
    let module E = (val env) in
    let module I = MakeSumSolver(E) in()];
  
  prec_actions = Compute_prec.[fun state -> if not !sum_by_recurrences then id else
    add_constr 0 (* priority 0=high—we affect only specific symbols *)
    (Precedence.Constr.make(fun a b -> match poly_of_id a, poly_of_id b with
      | None, Some _ -> 1
      | Some _, None -> -1
      | _ -> 0
    ))%
    update_weight_rule(fun wf _ id -> get_lazy (fun()-> wf id) (polyweight_of_id id))];
};;
(* TODO Setting --int-inf-diff-to-lesseq is vital but Arith_int.ml hides it currently. *)
Options.add_opts[
  "--sum-by-recurrences", Arg.Bool((:=)sum_by_recurrences), " use holonomic sequence method to sums (∑) in algebras";
]