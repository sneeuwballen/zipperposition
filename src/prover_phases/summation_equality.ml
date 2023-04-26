(* open Batteries opam install batteries + edit src/core/dune *)
[@@@warning "-10-26"](* Before final cleanup, tolerate unused values and definitions. *)
open Logtk
open Logtk_parsers
open Logtk_proofs
open Libzipperposition
open Libzipperposition_calculi
open Phases_impl
open Util
open UntypedPrint
open Precedence.Weight
open Comparison
open Monome
open Literals
open Literal.Conv
open Literal
open CCCache
open CCOpt
open CCVector
open CCArray
open CCList
open CCFun
open Type
open Term
open Stdlib
module B = Builtin
module HS = Hashtbl (* H̲ashtable for S̲tructural equality *)
module HT = Hashtbl.Make(struct
  type t = term
  let equal = (==)
  let hash = Term.hash
end)
module HV = Hashtbl.Make(struct
  type t = Term.var
  let equal = HVar.equal(==)
  let hash = HVar.hash
end)

let todo = RecurrencePolynomial.todo
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

let lex_array c = to_list %%> lex_list c

let sum_list = fold_left (+) 0
let sum_array = Array.fold_left (+) 0

let index_of p l = fst(get_exn(find_idx p l))

(* Search hash table by value instead of by key. The equality relation of keys does not matter. *)
let search_hash ?(eq=(=)) table value = HS.fold (fun k v found -> if found=None & eq value v then Some k else found) table None

let with_cache_2 c f = curry(with_cache_rec c (uncurry % f % curry))
let with_cache_3 c f = curry(with_cache_2 c (uncurry % f % curry))


(* make constants for debugging *)
let constants = HS.create 0
let have ?(infix=false) name par ty = match HS.find_opt constants name with
| None ->
  let i = ID.make name in
  if infix then ID.set_payload i (ID.Attr_infix name);
  let c = const (arrow par ty) i in
  HS.add constants name c; c
| Some c -> c


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
        match S.subsumes_with (of_list c1_no_l1, 0) (of_list c2_no_l2, 0) with
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




(* Mul | Move term | Moves term² ; c p d f | c p d₁ d₂ f₁ f₂ | c p | repara...
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

let (|->) i v a = assert(v>=0); a.(i)<-v; a (* powers are non-negative *)
let (|=>) i v = (i|->v) % copy
let (|+>) i v a = (i |=> a.(i)+v) a

let is0 = Z.equal Z.zero
let _Q q = app_builtin ~ty:int (Rat q) []
let _Z = _Q % Q.of_bigint
let _z = _Z % Z.of_int

let coordinate_count = ref 0
let fold_coordinates o f = fold_left f o (init !coordinate_count id)

let make_powers f = Array.init !coordinate_count f
let (+^) = Array.map2(+)

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


let varstr name1 n = String.make 1 Char.(chr(code name1 - n))

let termstr t = (flip String.iter (to_string t)
  |> Iter.filter(fun c -> not(mem c [' '; '('; ')']))
  |> Iter.take 20
  |> Iter.to_string ~sep:"" (fun c ->
    String.make 1 c ^ if Char.code c < 0xC0 then "̲" else "")
)

let powers_to_string name1 = String.concat"" % to_list % Array.mapi(fun var -> function
| 0 -> ""
| 1 -> varstr name1 var
| exp -> varstr name1 var ^ superscript(string_of_int exp))

let shifts_to_string = function
| One -> ""
| Shift(s,f) -> powers_to_string 'Z' s ^ termstr f
| Diagonal(sub, dup, s,f) -> "{" ^ varstr 'z' sub ^ "↦ " ^ varstr 'z' dup ^ "}" ^ powers_to_string 'Z' s ^ termstr f
| Product(s1,f1, s2,f2) -> powers_to_string 'Z' s1 ^ termstr f1 ^ "⬝" ^ powers_to_string 'Z' s2 ^ termstr f2

let mono_to_string(c,m,f) = match (match Z.to_string c with "1"->"" | "-1"->"-" | c'->c') ^ powers_to_string 'z' m ^ shifts_to_string f with ""->"1" | "-"->"-1" | s->s

let poly_to_string p = if length p = 0 then "0" else concat_view " ＋ "  mono_to_string p

(* List of terms that the given recurrence relates, without duplicates. *)
let terms_in = sort_uniq ~cmp:Term.compare % flat_map(function
| _, _, One -> []
| _, _, (Shift(_,f) | Diagonal(_,_,_,f)) -> f
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

let compare_mono = (fun(c,m,s) -> !elimination_priority m s, (mono_total_deg(c,m,s), (s, (m, c))))
  %%> (-) *** (-) *** compare_shifts *** lex_array(-) *** Z.compare

let rev_cmp_mono = flip compare_mono

(* Update the global elimination priority used in normalization. Existing recurrence polynomials become garbage and must not be used! However clauses that encode recurrences remain usable.
 The elimination priority overrides the default comparison order of monomials (which is total degree based). This can be used to derive equations without certain indeterminates or operands by saturation. For example take the total degree saturated set {m²+n-1, n²+m+1}. Ordering n²≺m instead one rewrites m in first polynomial deriving (-n²-1)²+n-1 = n(n³+2n+1) which is independent of m.
 Parameters to priority are list of multiplier powers and shift factor. The elimination preordering must extend divisibility: priority of the monomial M⬝N must be at least that of N. Constant priority (default) leaves the tie-breaking total degree ordering unchanged. See elim_oper_args and elim_indeterminate for other basic options.
 Desing: Having a global switch like this has drawbacks. Namely saturations of polynomial equations cannot be (easily) nested, parallelised or paused-and-resumed. Scalable solution would be to substitute the module by a functor taking the elimination priority as parameter. However that'd be complicated to use because polynomials and/or their operations had to explicitely carry the ordering. *)
let redefine_elimination_priority = (:=)elimination_priority

(* An elimination priority function. For example: elim_oper_args[t,2; s,1; r,1] to eliminate terms t,s,r with priority among them in t. *)
let elim_oper_args weights =
  let arg_weight = get_or ~default:0 % flip assq_opt weights in
  ~=(function
  | One -> 0
  | Shift(_,f) | Diagonal(_,_,_,f) -> arg_weight f
  | Product(_,f,_,g) -> (arg_weight %%> max) f g)

(* An elimination priority to indeterminates assigned by the given weight function. For example to eliminate 3ʳᵈ variable: elim_indeterminate(function((`Shift|`Mul),3,arg)->1|_->0). *)
let elim_indeterminate weight m = let rec this = function
| One -> 0
| Shift(s,f) | Diagonal(_,_,s,f) -> sum_array Array.(map(fun p -> weight(`Mul,p,f)) m +^ map(fun p -> weight(`Shift,p,f)) s)
| Product(s1,f1, s2,f2) -> this(Shift(s1,f1)) + this(Shift(s2,f2))
in this


(* Make normalized Product-shift of monomial. (Multiplication must commute.) If f=g, reduce the problem of computing only one lead unifier in mgu_shift by ordering higher shift first.  *)
let product(s,f, z,g) = if (Term.compare *** lex_array(-)) (f,s) (g,z) > 0
  then Product(s,f, z,g)
  else Product(z,g, s,f)

(* Arithmetic of polynomials etc. Main operations to superpose polynomials are: addition, multiplication by monomial, and lcm of monomials upto leading term. *)

type polysubst = Z.t * powers * shifts

let shift_multiplier s m = (let rec loop out = function
  | 0::ss, m::mm -> loop (map (cons(Z.one,m)) out) (ss,mm)
  | s::ss, m::mm -> loop (concat(init(m+1) (fun n -> map (cons(Z.(bin (of_int m) n * of_int s**n) , m-n)) out))) (ss,mm)
  | _ -> out
  in loop [[]] (to_list s, to_list m) |> map(fun l -> fold_left Z.(fun a (c,_) -> c*a) Z.one l, of_list(rev_map snd l)))

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

(* Extend functionality of map_monomials by shifting whole recurrence forward if negative shift-powers occur. *)
let map_monomials f l =
  let p = map_monomials f l in
  let backshifts = fold_left Array.(fun bs (_,_,s) -> match s with
    | One -> bs
    | Shift(s,_) -> mapi (fun j -> min s.(j)) bs
    | Product(s,_,z,_) -> mapi (fun j -> min(min s.(j) z.(j))) bs
    | Diagonal(sub, dup, s,_) -> mapi (fun j -> min(
      if j=sub then 0 else
      if j=dup then min s.(j) s.(sub)
      else s.(j))) bs
  ) (power0()) p in
  if sum_array backshifts = 0 then p else
    substiply (Z.one, power0(), Shift(Array.map((-)0)backshifts, _z 0)) p

let mgu_coef a b = Z.(lcm a b / a, lcm a b / b)
let mgu_pow m m' = (fun n -> make_powers(fun v -> max m.(v) m'.(v) - n.(v))) @@ (m,m')
(* Note: the shift One always raises MonomialIncompatibility because it is never a leading monomial in normal circumstances. *)
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
  (* Correct multiplier by applying shift-part's coordinate substitution. Think e.g. unifier (n {k↦n}, 1) of knfₖₙ and n³fₙₙ. *)
  let fix n = function
  | Diagonal(sub, dup, _,_) -> (sub|->0) ((dup|+>n.(sub)) n)
  | _ -> n in
  let uc,uc' = mgu_coef c c' in
  let um,um' = mgu_pow (fix m s') (fix m' s) in
  let us,us' = mgu_shift s s' in
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

(* multiplier × polynomial *)
let ( **:) m = substiply(Z.one, m, Shift(power0 (), _z 0))
let mul_var ?(by=1) v = ( **:) ((v|->by) (power0 ()))

(* shift × monomial *)
let ( **>) s = substiply(Z.one, power0 (), Shift(s, _z 0))
let shift_var ?(by=1) v = ( **>) ((v|->by) (power0 ()))


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
  | _, m, Product(s1,_,s2,_) -> valuexm (s1+^s2)
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


(* Calculate superposition between two polynomials pⱼ (j=1,2) that represent equations m⬝pⱼ=0 where m runs over all monomials. A result is m₁p₁ - m₂p₂ where mⱼ are least monomials such that leading terms in mⱼpⱼ equal. Currently result is unique, if superposition is possible. *)
let superpose p p' =
  if p=_0 or p'=_0 then [] else try
    let u,u' = mgu (hd p) (hd p') in
    (* Debugging note: Untyped printing here changed nontermination into premature termination. Normal printing has no such effect ⇒ its about type tests. In case of termination I saw: dim-heads=0, then [0] derived, then dim-heads=[0] (eli [{}]) and stop. Nonterminating started instead to saturate ×-monomials at this point. It continued to get dim-heads=0. Before this deviation the derived recurrences appeared exactly the same. However I then changed something somewhere and now I cannot reproduce the termination anymore. *)
    (* [let aid = substiply u p -- substiply u' p' in print_endline(poly_to_string aid); aid] *)
    [substiply u p -- substiply u' p']
  with MonomialIncompatibility -> []

(* Try rewrite leading monomial of p by r. *)
let leadrewrite r p =
  if r=_0 or p=_0 then None else try
    Some(p -- (substiply (hd r |~> hd p) r))
  with MonomialIncompatibility -> None


(* Find r, q⬝arg s.t. p = (old - by)⬝q⬝arg + r when given indeterminate old, coefficient term by, target term arg, and polynomial p whose indeterminates commute with old. Note that r is "p evaluated at old=by". *)
let substitute_division var arg p =
(* Now ignoring the arg... *)
  p |> map_monomials(fun(c,m,s)-> [c, m, match s with
    | One -> One
    (* | Shift(s,f) -> Shift((if f==arg then (var|=>0) s else s), f) *)
    | Shift(s,f) -> Shift((var|=>0) s, f)
    | _ -> raise MonomialIncompatibility]),
  p |> map_monomials(function
    | _, _, One -> []
    (* | c, m, Shift(s,f) -> if f==arg then init s.(var) (fun j -> c, m, Shift((var|=>j) s, f)) else [c,m,Shift(s,f)] *)
    | c, m, Shift(s,f) -> init s.(var) (fun j -> c, m, Shift((var|=>j) s, f))
    | _ -> raise MonomialIncompatibility)


(* quick manual data entry *)
(* let _p s = let is_low c = String.lowercase c = c in
  let rec go = function
  | v::e::rest -> go rest @ [{exp=int_of_string e; var=122-Char.(code(lowercase v.[0])); base=if is_low v then`Mono else`Move(_z 1)}]
  | _ -> []
  in
  go String.(split_on_char ' ' (trim(concat""[s;" 1"])))
let _P = map_monomials(fun(c,s,t)-> [_z c, _p s, have t [] int]) *)

end
*)





module MakeSumSolver(MainEnv: Env.S) = struct
module C = MainEnv.C
module R = RecurrencePolynomial

(* In general parent clauses of an inference also track applied substitutions but with polynomials these substitutions are almost always identities. *)
let parents_as_such = map(fun p -> C.proof_parent_subst (Subst.Renaming.create()) (p,0) Subst.empty)

(* Make clause with standard penalty and trail. By default no substitution is applied to parents but supplied proof_step function can of course directly embed any parents. *)
let make_clause ?(parents=[]) literals proof_step = C.create literals
  ~penalty:(fold_left max 1 (map C.penalty parents))
  ~trail:(C.trail_l parents)
  (proof_step(parents_as_such parents))

let definitional_poly_clause p = make_clause [R.polyliteral p] ~=Proof.Step.trivial

let polys_of_2_lits no yes l1 =
  R.poly_of_lit l1 |> map_or ~default:~=no (fun p1 l2 ->
  R.poly_of_lit l2 |> map_or ~default:no (yes p1))

let superpose_poly = polys_of_2_lits [] (R.superpose %>> map(fun p -> [R.polyliteral p], Subst.empty))

let rewrite_poly = polys_of_2_lits None (R.leadrewrite %>> CCOpt.map(fun p -> [R.polyliteral p], Subst.empty))

let replace_lit_by_poly c old_lit_index p = make_clause ~parents:[c]
    (R.polyliteral p :: except_idx (C.lits c) old_lit_index)
    Proof.(Step.inference ~rule:(Rule.mk "represent recurrence by polynomial"))

let polys_in = Iter.(concat % map(filter_map R.poly_of_lit % of_array % C.lits))


let saturate_in env cc = Phases.(run(
  let (^) label thread = start_phase label >>= return_phase >>= ~=thread in
  Parse_CLI^LoadExtensions^Parse_prelude^Start_file^Parse_file^Typing^CNF^Compute_prec^Compute_ord_select^MakeCtx^MakeEnv^
  Phases_impl.refute_or_saturate env CCVector.{Clause.c_set= of_list cc; c_sos= of_list []}
  >>= fun result -> start_phase Exit >>= ~=(return_phase result)))


let make_polynomial_environment indeterminate_weights =
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
    (module LRI) (LRI.empty_with' (R.default_features()));
  on_eligible_literals env "sup. poly." superpose_poly;
  PolyEnv.add_is_trivial (Array.mem mk_tauto % C.lits);
  env,
  fun clauses -> 
    R.indeterminate_weights := indeterminate_weights;
    match saturate_in env (Iter.to_rev_list clauses) with
    | Error e -> raise(Failure e)
    | Ok(_, Unsat _) -> PolyEnv.C.ClauseSet.to_iter(PolyEnv.get_empty_clauses())
    | Ok _ -> PolyEnv.get_clauses()

(* note: maybe this is the only interface needed and can replace make_polynomial_environment *)
let saturate_with_order = snd % make_polynomial_environment


(* Filter eligible non-trivial polynomial recurrences by the given condition. *)
let filter_recurrences ok_poly = Iter.filter(fun c ->
  fold_lits ~eligible:(C.Eligible.res c) (C.lits c)
    |> Iter.exists R.(fun(l,_) -> match poly_of_lit l with
      | Some p -> p!=_0 & ok_poly p
      | _ -> false))

(* Filter those clauses that have eligible polynomial with leading term M f where some monomial M operates to a term f representing an expression in polylist. *)
let filter_recurrences_of polylist = filter_recurrences R.(fun p -> match terms_in[hd p] with
  | [t] -> mem ~eq:(CCOpt.equal poly_eq) (poly_of_term t) (map CCOpt.pure polylist)
  | _ -> false)

(* Transform recurrence polynomial(s) of the clause by the given function. *)
let map_poly_in_clause f ?(rule=Proof.Rule.mk"arithmetic on recurrence") c =
  make_clause ~parents:[c]
    (to_list(C.lits c) |> map(fun l -> map_or ~default:l (R.polyliteral % f) (R.poly_of_lit l)))
    (Proof.Step.inference ~rule)


(* Explicit cache for propagate_recurrences_to. We could also use with_cache_rec but its explicit recursive call functionparameter is slightly inconvenient in a long mutually recursive definition group. *)
module PolyMap = Hashtbl.Make(struct
  type t = R.poly
  let equal = R.poly_eq
  let hash p = R.poly_hash p
end)
let recurrence_table = PolyMap.create 16

(* Add clause c to the list of recurrences of p. *)
let add_new_rec p c = PolyMap.add recurrence_table p
  (CCList.add_nodup ~eq:C.equal c
    (get_or~default:[] (PolyMap.find_opt recurrence_table p)))

let rec propagate_recurrences_to f = match PolyMap.find_opt recurrence_table f with
| Some r -> r
| None ->
  let r = Iter.to_rev_list(match f with
  | [X[A(V _)]::_] | _::_::_ -> propagate_oper_affine f
  | [X p :: q] -> propagate_times [p] [q]
  | [S i :: p] -> propagate_sum i [p]
  | [O s :: p] -> propagate_subst s [p]
  | _ -> Iter.empty)
  in
  PolyMap.add recurrence_table f r;
  r

and recurrences_of_term t = propagate_recurrences_to R.(get_or~default:(of_term t) (poly_of_term t))

and propagate_oper_affine p's_on_f's =
  let _,result,_ = R.poly_as_lit_term_id p's_on_f's in
  let pf_sum = R.oper_coef_view p's_on_f's in
  let f's = R.terms_in pf_sum in
  Iter.of_list(flat_map recurrences_of_term f's)
  |> Iter.cons(definitional_poly_clause R.(of_term result -- pf_sum))
  |> saturate_with_order(R.elim_oper_args((result,1) :: map(fun f -> f,2) f's))
  |> filter_recurrences_of[p's_on_f's]

and propagate_times f g =
  (* 1. rename (ϱ) var.s of g with merge σ *)
  (* 2. create Rᶠ·ϱg and f·ϱRᵍ *)
  (* 3. propagate to substitution σ(f·ϱg) *)
  let rename, merge, _ = R.rename_apart ~taken:(R.free_variables f) (R.free_variables g) in
  let disjoint_recurrences = R.(
    map(map_poly_in_clause(fun r -> r><mul_indet([rename]><g))) (propagate_recurrences_to f) @
    map(map_poly_in_clause(fun r -> mul_indet f><[rename]><r)) (propagate_recurrences_to g)) in
  match merge with
  | [] -> Iter.of_list disjoint_recurrences
  | [O m] ->
    let f_x_g' = R.(mul_indet f><[rename]>< g) in
    PolyMap.add recurrence_table f_x_g' disjoint_recurrences;
    propagate_subst m f_x_g'
  | _ -> assert false (* impossible result from R.rename_apart *)

and propagate_sum i f =
  (* let bad = ref[] in *)
  let is_f t = R.(CCOpt.equal poly_eq (Some f) (poly_of_term t)) in
  let sum_blocker = function (* gives elimination priorities: [] is good, [1] is bad *)
  |`T t when not(is_f t) -> []
  |`V v when v!=i -> []
  |`O[u, R.[[A(V v)];[A I]]] when v=u -> []
  | _ -> [1]
  in
  (* let rec_of_sum_with_bad_terms =  *)
  Iter.of_list(propagate_recurrences_to f)
  |> saturate_with_order sum_blocker
  (* ∑ₙᑉⁿNf = N∑ₙᑉⁿf - f₀ is a simplification rule so no special action is required.
  |> Iter.map R.(fun c ->
    (* TODO remember to guarantee that get_exn succeeds *)
    let p, lits = get_exn(c|>todo"view_poly_clause") in
    let p' = p |> map_monomials(fun m -> match terms_in[m] with
      | [f] when is_f f -> m |> fold_indeterminates
        (function `T t -> [[S i]] >< of_term t |_-> assert false)
        (fun s g -> function
          | O[j,[[A I]]] when j=i -> g -- ([[eval_at ~var:i Z.zero]] >< g) ++ s
          | O[k,[[A I]]] -> todo"near commutativity"
          | x -> 
            assert(not(Int_set.mem i (free_variables[[x]]))); (* or if this cannot be guaranteed, propagation should be given up. *)
            [[x]]><s)
      | _ -> [[S i]]><[m])
    in
    make_clause (R.polyliteral p' :: to_list lits) ~parents:[c]
      Proof.(Step.inference ~rule:(Rule.mk "pull recurrence out of sum"))
  )
  Force the iteration through to decide necessity of resaturation. 
  Note: resaturation might not be useful anymore because the handling of excess terms is improved.
  |> Iter.persistent in
  if !bad=[] then rec_of_sum_with_bad_terms else(
    let _,sum,_ = R.(poly_as_lit_term_id([[S i]]><f)) in
    Iter.of_list(flat_map recurrences_of_term !bad)
    |> Iter.append rec_of_sum_with_bad_terms
    |> saturate_with_order(R.elim_oper_args((sum,1)::map(fun f -> f,2)!bad))
  )
  *)
  |> filter_recurrences_of[f]

and propagate_subst s f =
  (* 1. for each range coordinate create compound shift *)
  (* 2. create equations: compound shift = composition of 1-shifts ↭ 𝕊ⱼ = ∏ᵢ Sᵢ^mᵢⱼ ↭ 𝕊ᵃ = Sᵐᵃ *)
  (* 3. saturate *)
  (* 4. replace each compound shift by a 1-shift, and substitute multipliers *)
  let is1shift = function R.O[_,[[A I]]] -> true | _ -> false in
  let dom = R.free_variables f in
  let s = filter (fun(v,_)-> Int_set.mem v dom) s in (* ⇒ domain of s ⊆ dom ...necessary? *)
  match R.view_affine s with
  | None -> Iter.empty
  | Some(m,a) ->
    (* If 𝕊ⱼ=Sⱼ, we skip it, which makes saturation faster, but primarily this is to dodge the issue of telling 𝕊ⱼ and Sⱼ apart. *)
    let changedShift = filter(fun j -> exists R.(fun i -> m@.(i,j) != if i=j then 1 else 0) (Int_set.to_list dom)) in
    let multishifts_and_equations = changedShift(Int_set.to_list(R.rangeO s)) |> map R.(fun j ->
      let on_dom f = map f (Int_set.to_list dom) in
      (* 𝕊ⱼ = ∏ᵢ Sᵢ^mᵢⱼ = O[0,m₀ⱼ;...;i,mᵢⱼ;...] where j runs over range and i over domain *)
      let ss_j = O(on_dom(fun i -> i, const_eq_poly(Z.of_int(m@.(i,j))))) in
      (* TODO Recurrence polynomial data structure needs an additional marker to distinguish compound shifts from ordinary ones in all corner cases. *)
      if is1shift ss_j then failwith("Unimplemented: "^ string_of_int j^"ᵗʰ compound shift cannot be distinguished from 1-shift when propagating to substitution "^ poly_to_string[[O s]] ^" of "^ poly_to_string f);
      let eq_j = product(on_dom(fun i -> pow' poly_alg [[shift i]] (max 0 (m@.(i,j)))))
        -- product([[ss_j]] :: on_dom(fun i -> pow' poly_alg [[shift i]] (- min 0 (m@.(i,j))))) in
      ((ss_j, shift j), j), eq_j
    ) in
    (* We must eliminate all domain shifts except ones skipped above: dom\( rangeO s \ {j | ∃ ss_j} ) *)
    let elimIndices = Int_set.(diff dom (diff (R.rangeO s) (of_list(List.map(function((_,j),_)->j) multishifts_and_equations)))) in
    Iter.of_list(propagate_recurrences_to f)
    |> Iter.append(Iter.of_list(map (definitional_poly_clause % snd) multishifts_and_equations))
    |> saturate_with_order(R.elim_indeterminate(function
      |`O[i,R.[[A I]]] -> if Int_set.mem i elimIndices then 1 else 0
      | _ -> 0))
    |> Iter.map(map_poly_in_clause R.(map_indeterminates(function
      (* Replace Mᵢ=X[A(V i)] by ∑ⱼmᵢⱼMⱼ. *)
      | X[A(V i)] -> fold_left (++) _0 (map(fun j -> const_op_poly(Z.of_int(m@.(i,j))) >< [[mul_var j]]) (Int_set.to_list(R.rangeO s)))
      (* Replace 𝕊ⱼ by Sⱼ by looking it up from multishifts_and_equations. *)
      | x -> [[get_or~default:x (CCList.assoc_opt ~eq:indet_eq x (map(fst%fst) multishifts_and_equations))]])))


(* Make a polynomial into a new recurrence of its maximal term. Since polynomial is packed into a new clause, this function in this form is only suitable for testing. *)
let declare_recurrence r = add_new_rec
  R.(of_term(max_list ~ord:Term.compare (terms_in r)))
  (definitional_poly_clause r)

let sum_equality_inference clause =
  let (!) = R.poly_of_string in
  let eq0' p = make_clause (map R.polyliteral p) ~=(Proof.Step.assert' ~file:"" ~name:"" ()) in
  let split_or k d s = match String.split_on_char k s with
  | [a;b] -> a,b | [a] -> if k='.' then d,a else a,d | _ -> raise Exit in
  let eq0 ss = eq0' R.[fold_left (++) _0 (List.map(fun cmt ->
    let c,mt = split_or '.' "1" (String.trim cmt) in
    let m,t = split_or '\'' "" mt in
    Z.of_string c *: poly_of_string m >< of_term(have t [] int)
  ) (String.split_on_char '+' ss))] in
  
  declare_recurrence !"NNf-Nf-f";
  declare_recurrence !"Ng-ng-g";
(*
  propagate_recurrences_to !"nf"; (*OK*)
  propagate_recurrences_to !"g+f"; (*OK*)
*)
  propagate_recurrences_to !"g+nf"; (*diverge?*)

  (* eq0"zzzzz"; eq0"zz + -1."; *)
  (* eq0"3.zzzzzzzzzzzzzzzzzzz"; eq0"3.zzzzzzzzz + 1."; *)
  (* eq0"x + y + -1.z"; eq0"xx + yy + -1.zz"; *)
  (* eq0"yyx + -1.x + -1.y"; eq0"yxx + -1.x + -1."; *)
  (* eq0"xx + 3.x + 1."; eq0"yy + 3.y + 1."; eq0"xxxxx + yyyyy"; *)
  
  (* eq0"2.nN + -1.m + 3."; eq0"NM + 2.m";  *)
  (* eq0"2.nnNN + -1.n 1 N + 3."; eq0"NNN + 2.nN";   *)
  
  (* eq0"X'S* + -1.'S* + -1.X'f"; eq0"'h + -1.'S* + 'g*";  *)
  (* eq0"X'g* + -1.'g* + -1.X'f"; *)
  (* eq0"-1.XX'g* + 'g* + XX'f + X'f"; *)
  
  (* eq0"Y'⬝2ʸ + -2.'⬝2ʸ"; eq0"-4.'g` + y 2'⬝2ʸ + y'⬝2ʸ"; *)
  (* eq0"xX'(ˣᵧ) + -1.yX'(ˣᵧ) + X'(ˣᵧ) + -1.x'(ˣᵧ) + -1.'(ˣᵧ)"; eq0"yY'(ˣᵧ) + Y'(ˣᵧ) + -1.x'(ˣᵧ) + y'(ˣᵧ)"; eq0"'f` + -1.y 2'(ˣᵧ)"; *)
  (* Change priority for ↓ *)
  (* eq0"xX'f + -1.yX'f + X'f + -1.x'f + -1.'f"; eq0"yyY'f + -1.yx'f + y 2'f + -1.x'f + y'f"; *)

  (* eq0"-1.xxX 2'∑f+-2.xX 2'∑f+-1.X 2'∑f + 3.xxX'∑f+9.xX'∑f+6.X'∑f + -2.x 2'∑f+-6.x'∑f+-4.'∑f"; eq0"xX'g + -2.x'g + -4.'g"; eq0"'h` + -1.'∑f + 'g"; *)

  ~<recurrence_table;
  exit 1
  (* tl(tl[clause;clause]) *)

(************************************************************************)
let coords = ref []
let var_term index = nth !coords index
let var_index term = try index_of ((==)term) !coords
with Invalid_argument _ -> raise(Invalid_argument("var_index "^str term^" ∉  "^str!coords))

(* Try to separate non-trivial eligible recurrence and the rest of the literals from the given clause. *)
let view_poly_clause c = fold_lits ~eligible:(C.Eligible.res c) (C.lits c)
  |> Iter.find R.(fun(l,_)-> match poly_of_lit l with
    | Some p when p!=_0 -> Some(p, CCArray.filter((!=)l) (C.lits c))
    | _ -> None)

(* Filter those clauses that have eligible polynomial with leading term M t where some monomial M operates on an OK term t. *)
let filter_recurrences_of ok_term = filter_recurrences R.(fun p -> match terms_in[hd p] with [t] -> ok_term t |_->false)

let is_summation s = match view s with
| Const{name="sum"} -> true
| _ -> false

let to_Z t = match view t with
| AppBuiltin(B.Int z, []) -> z
| AppBuiltin(B.Rat q, []) when Z.one == Q.den q -> Q.num q
| _ -> raise Exit
let is_Z t = try snd(to_Z t, true) with Exit -> false
let to_int = Z.to_int % to_Z

type encoding_phase_info = {coords: term list; symbols: term list; specialise: term HV.t}

let _Z a = app_builtin ~ty:int (Int a) []
let _z = _Z % Z.of_int
let trivC = make_clause[Literal.mk_tauto] ~=Proof.Step.trivial
let feed = MainEnv.FormRename.get_skolem ~parent:trivC ~mode:`SkolemRecycle

let rec ev t = match view t with
| AppBuiltin(B.Sum, [_; x; y]) ->
  let x, y = ev x, ev y in
  (try _Z Z.(to_Z x + to_Z y)
  with Exit -> app_builtin ~ty:int B.Sum [of_ty int; x; y])
| AppBuiltin(B.Difference, [_; x; y]) ->
  let x, y = ev x, ev y in
  (try _Z Z.(to_Z x - to_Z y)
  with Exit -> app_builtin ~ty:int B.Difference [of_ty int; x; y])
| AppBuiltin(B.Product, [_; x; y]) ->
  let x, y = ev x, ev y in
  (try _Z Z.(to_Z x * to_Z y)
  with Exit -> app_builtin ~ty:int B.Product [of_ty int; x; y])
| App(s,[m;f]) when is_summation s & is_Z(ev m) ->
  ev R.(fold_left (fun adds j -> app_builtin ~ty:int B.Sum [of_ty int; adds; f@<[j]]) (_z 0) (init (1+to_int(ev m)) _z))
| App(f,p) -> app (ev f) (map ev p)
| Fun(ty,b) -> fun_ ty (ev b)
| _ -> t
(* The only two uses of ev. *)
and (@<) g = ev % Lambda.whnf_list g
let (=:) by old = ev % replace ~old ~by

let no_plus = map(fun s -> match view s with
| AppBuiltin(B.Sum, [_;x;y]) when is_Z x -> y
| AppBuiltin(B.Sum, [_;x;y]) when is_Z y -> x
| AppBuiltin(B.Difference, [_;x;y]) when is_Z y -> x
| _ -> s)

(* Explicit integer constant or simple constant suitable as a coordinate variable. *)
let is_atom t = is_Z t or ty t == int & match view t with
| Const _ -> true
| App(f,x) -> for_all is_Z x
| _ -> false

let is_free t = ty t ==int & is_var t

let recurrence_table = HT.create 32
let add_recurrence t r = HT.add recurrence_table t (r :: get_or ~default:[] (HT.find_opt recurrence_table t))

let rec term_to_poly info t =
  if ty t != int then raise Exit;
  match view t with
  | AppBuiltin(B.(Sum|Difference) as op, [_;x;y]) ->
    R.(if op=B.Sum then (++) else (--)) (term_to_poly info x) (term_to_poly info y)
  | AppBuiltin(B.Product, [_;c;x]) -> R.(try const_op_poly(to_Z c) >< term_to_poly info x
    with Exit -> if is_atom c
      then [[mul_var(var_index c)]] >< term_to_poly info x
      else R.of_term t)
  | AppBuiltin(B.Int n, _) -> R.const_eq_poly n (* or const_op_poly? *)
  | _ when memq t info.coords -> [[R.mul_var(var_index t)]]
  | App(f,p) when exists (Unif.FO.matches ~pattern:(f @< no_plus p)) info.symbols ->
    to_iter info.symbols (fun s -> try
      Unif.FO.matching_same_scope ~scope:0 ~pattern:(f @< no_plus p) s
      |> Subst.iter(fun (v,_) (b,_) -> HV.add info.specialise (cast_var_unsafe v) (of_term_unsafe b))
    with Unif.Fail -> ());
    R.of_term t
  | App(f,p) when for_all(flip memq info.coords) p ->
    R.of_term t
  | _ -> raise Exit

let equation_to_recpoly info literal = match to_form literal with SLiteral.Eq(t,s) ->
  (try Some R.(term_to_poly info t -- term_to_poly info s)
  with Exit -> None)
|_->None

(* Associate the clause as a recurrence polynomial to appropriate terms. To be used in initialization. *)
let register_if_clause_is_recurrence info clause =
  let v_id t = index_of ((==)t) info.coords in
  fold_lits ~eligible:(C.Eligible.res clause) (C.lits clause) Subst.(fun(lit,place)->
    equation_to_recpoly info lit |> CCOpt.iter List.(fun p ->
      let pair_with (v:Term.var) (x:Term.t) : var Scoped.t * term Scoped.t = ((v:>var),0), ((x:>term),0) in
      let substs = HV.to_seq_keys info.specialise
      |> of_seq_rev
      |> sort_uniq(HVar.compare Type.compare)
      |> map_product_l(fun v -> map(pair_with v) (sort_uniq Term.compare (HV.find_all info.specialise v)))
      (* when specialising, do not duplicate variables *)
      |> filter(fun l -> length l = length(sort_uniq(fst%snd %%> InnerTerm.compare) l))
      |> map of_list in
      substs |> iter R.(fun s ->
        let s' t = of_term_unsafe(apply (Renaming.create()) s ((t:Term.t:>term),0)) in
        ignore(v_id,s'); (* silence unusedness *)
        let sp = todo"substitution to shifts"(* R.map_terms s' p
        |> map_monomials(fun(c,m,s)-> let m=copy m in [c, m, match s with
        | Shift(z,g) ->
          let z = copy z in
          let rec correct_shift h = match view h with
          | App(f,p) -> f @< (p |> map(fun t -> match view t with
            | AppBuiltin(B.Sum, [_;x;y]) when is_Z x ->
              z.(v_id y) <- to_int x; y
            (* symmetric copy *)
            | AppBuiltin(B.Sum, [_;y;x]) when is_Z x ->
              z.(v_id y) <- to_int x; y
            | AppBuiltin(B.Difference, [_;y;x]) when is_Z x ->
              z.(v_id y) <- - to_int x; y
            | _ -> t))
          (* TODO h is a possibly shifted term in polynomial. Just wish here that shifts are not present! *)
          | AppBuiltin(B.Product, [_;c;x]) when is_Z c -> todo""
          | AppBuiltin(B.Product, [_;c;x]) when is_atom c ->
            m.(v_id c) <- 1+m.(v_id c);
            correct_shift x
          | _ -> h in
          Shift(z, correct_shift(s' g))
        | s -> s]) *)in
        terms_in sp |> iter(flip add_recurrence (replace_lit_by_poly clause place sp)))))


(* * The structure following propagation steps—large mutually recursive block. *  *)
let rec recurrences_of t = match HT.find_opt recurrence_table t with
| Some r -> r
| None ->
  let r = Iter.to_rev_list(match view t with
  | AppBuiltin(B.Sum, [_;x;y]) -> propagate_affine x y t
  | AppBuiltin(B.Difference, [_;x;y]) -> propagate_affine x ~coef2:Z.minus_one y t
  (* TODO symmetric copy *)
  | AppBuiltin(B.Product, [_;x;y]) when is_Z x -> propagate_affine ~coef1:(to_Z x) x (_z 0) t
  | AppBuiltin(B.Product, [_;x;y]) when is_atom x -> todo"atom"
  | AppBuiltin(B.Product, [_;x;y]) -> propagate_times x y t
  | App(s,[m;f]) when is_summation s ->
    if memq (feed f) !coords then
      propagate_sum m (var_index(feed f)) (f@<[feed f]) t
    else(
      print_endline("Out of coordinates with "^ str t);
      Iter.empty)
  | _ -> Iter.empty)
  in
  HT.add recurrence_table t r;
  r

and propagate_affine ?(coef1=Z.one) t ?(coef2=Z.one) s t_plus_s =
  let env, saturate = make_polynomial_environment(R.elim_oper_args[t,2; s,2; t_plus_s,1]) in
  let poly x = R.(if is_Z x then const_eq_poly(to_Z x)
    else if memq x !coords then [[mul_var(var_index x)]]>< const_eq_poly Z.one
    else of_term x) in
  Iter.of_list(recurrences_of t @ recurrences_of s)
  |> Iter.cons(definitional_poly_clause R.(of_term t_plus_s -- coef1*: poly t -- coef2*: poly s))
  |> saturate
  |> filter_recurrences_of((==)t_plus_s)

and propagate_times t s t_x_s =
  let env, saturate = make_polynomial_environment(R.elim_oper_args[t,2; s,2; t_x_s,1]) in
  let init_recurrences = Iter.of_list(recurrences_of t @ recurrences_of s)
    |> saturate
    |> filter_recurrences_of(fun x -> x==t or x==s)
  in
  init_recurrences
  |> Iter.cons(definitional_poly_clause R.(of_term t_x_s --  (mul_indet(of_term t) >< of_term s)))
  |> saturate
  |> filter_recurrences_of((==)t_x_s)

and propagate_sum set sum_var s sum =
  let bad = ref[] in
  (* let nice = ref true in *)
  let sum_blocker = function
  |`T t when t != s -> []
  (* |`V v when v=sum_var -> [1] *)
  |`V v when v!=sum_var -> []
  |`O[v, R.[[A(V w)];[A I]]] when v=w -> (try fst([], (if v=sum_var then near_bijectivity_error else near_commutation_error v) set sum_var)
  with Exit -> [1])
  | _ -> [1]
  in
  let env, saturate = make_polynomial_environment sum_blocker in
  (* Note: Unfortunately post-saturation processing here reveals all the structural details of recurrence polynomials and so violates encapsulation. *)
  let rec_of_sum_with_bad_terms = Iter.of_list(recurrences_of s)
  |> saturate
  (* Remove recurrences from which it was not possible to eliminate the blocking indeterminates. *)
  (* |> filter_recurrences R.(fun p -> fold_coordinates true (fun pass v ->
    pass & for_all(function
      | _, m, One -> true
      | _, m, Shift(s,f) -> 0 = m.(v)*sum_fobic(`Mul,v,f) + s.(v)*sum_fobic(`Shift,v,f)
      | _ -> false
    )p)) *)
  |> Iter.map R.(fun c ->
    (* Above filtering ⟹ get_exn OK *)
    let p, lits = get_exn(view_poly_clause c) in
    (* let p' =
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
        | One -> todo "∑m over the set"
        | Shift(z,s') when s'==s -> Shift(z,sum)
        (* TODO Variables may not escape. Can we some times eliminate f? If so, this is incomplete. Now try to copy summation from the given term to f. *)
        | Shift(z,f) when Term.subterm ~sub:(var_term sum_var) f ->
          (match view sum with App(sum,[set;_]) ->
            let new_var = HVar.fresh ~ty:(ty s) () in
            Shift(z, app sum [set; fun_of_fvars [new_var] ((var new_var =: var_term sum_var) f)])
          | _ -> assert false)
        (* | Diagonal(_,_,_,s') when s'==s -> nice:=false; z *)
        | Shift(_,f) | Diagonal(_,_,_,f) -> bad:= add_nodup ~eq:(==) f !bad; z
        (* | _ -> z]) *)
        | _ -> assert false])
    in *)
    make_clause (R.polyliteral p :: to_list lits) ~parents:[c]
      Proof.(Step.inference ~rule:(Rule.mk "pull recurrence out of sum"))
  )
  (* Force the iteration through to decide necessity of resaturation. *)
  |> Iter.persistent in
  if !bad=[] then rec_of_sum_with_bad_terms else(
    let _,resaturate = make_polynomial_environment(R.elim_oper_args((sum,1)::map(fun f -> f,2)!bad)) in
    Iter.of_list(flat_map recurrences_of !bad)
    (* Iter.of_list(recurrences_of s) *)
    |> Iter.append rec_of_sum_with_bad_terms
    |> resaturate
  )
  (* |> filter_recurrences R.(fun r ->
    exists(function _,_,Shift(_,sm) when sm==sum ->true | _->false) r
    & for_all(function _,_,Shift(_,t) when subterm ~sub:(var_term sum_var) t ->false | _->true) r) *)

(* TODO With only exclusive upper bound now: ∑ᑉᵐ(N-1) = @ᵐ-@⁰ *)
and near_bijectivity_error set var = near_either_error var set var
(* TODO With only exclusive upper bound: ∑ᑉᵐM-M∑ᑉᵐ = -@ᵐ *)
and near_commutation_error v = near_either_error v

and near_either_error =
  let eq ((v,s),n) ((v',s'),n') = v=v' & s==s' & n=n' in
  (* Note: this has hit assertion failure with lru cache. *)
  with_cache_3 (replacing 32 ~eq) ~=R.(fun op_var set sum_var ->
    (* TODO actually deduce end_points by superposition from op_var, set, sum_var *)
    let end_points =
      let set, base = match view set with
      | AppBuiltin(B.Sum, [_;y;x]) when is_Z y ->
        x, to_int y
      | AppBuiltin(B.(Sum|Difference) as op, [_;x;y]) when is_Z y ->
        x, to_int y * if op = B.Sum then 1 else -1
      | _ -> set, 0
      in
      if base < -1 then todo"base<-1";
      if op_var = sum_var then [1,1,`Dup(var_index set); -1,1,`None]
      else if op_var = var_index set then [-1,1+base,`Dup op_var]
      else []
    in end_points|>todo"near errors")
    (* map_monomials(fun(c,m,s) ->
      assert(m.(sum_var)=0); (* those must be eliminated before these errors are meaningful. *)
      match s with
      | One -> _0
      | Shift(s,f) -> end_points |> flat_map(fun(sign, base, scale) ->
        let c = Z.( * ) (Z.of_int sign) c in
        match scale with
        (* (±1,b,None) represents ±[sum_var:=b] *)
        |`None -> [c, m, Shift((sum_var|=>0) s, (_z base =: var_term sum_var) f (*sum_var↦base*))]
        (* (±1, b, Dup v) represents ±Vᵇ{sum_var↦v} where commuting Vᵇ duplicates it *)
        |`Dup var ->
          [c, m, if subterm ~sub:(var_term var) f
            then Diagonal(sum_var, var, (var|+>base) ((sum_var|+>base) s), f)
            else Shift((var |-> s.(sum_var)+base) ((sum_var|=>0) s), (var_term var =: var_term sum_var) f)]
        | _ -> todo"")
      | _ -> assert false)) *)


let step text parents lits =
  make_clause ~parents lits (fun parent_proofs -> if parents=[]
  then (if text="goal" then Proof.Step.goal' else Proof.Step.assert') ~file:"" ~name:text ()
  else Proof.Step.inference ~tags:[] ~rule:(Proof.Rule.mk text) parent_proofs)


let sum_equality_inference' clause =
  fold_lits ~eligible:(C.Eligible.res clause) (C.lits clause) 
  |> Iter.find(fun(lit, place) ->
    (* Top-level ∑ must exist. *)
    if Literal.fold (fun ok t -> ok or match view t with App(s,_) -> is_summation s | _->ok) false lit
    then match to_form lit with SLiteral.Neq(lhs,rhs) ->
      let main_expr = app_builtin ~ty:int B.Difference [of_ty int; lhs; rhs] in
      (* TODO better initialization *)
      let open Term.Set in
      let coord_set, sym_set = ref empty, ref empty in
      let rec walk t  = if not(is_Z t) & ty t == int then
        if is_atom t then coord_set <:> add t
        else match view t with
        | AppBuiltin(B.(Sum|Difference|Product), [_;x;y]) -> walk x; walk y
        | App(s, [m;f]) when is_summation s -> walk m; walk(f@<[feed f])
        | App(f,x) -> List.iter walk x; if List.for_all is_atom x then sym_set <:> add t
        | _ -> ()
      in
      walk main_expr;
      (* R.coordinate_count := cardinal !coord_set; *)
      MainEnv.get_clauses() (fun c -> c|>register_if_clause_is_recurrence{
        coords = to_list !coord_set;
        symbols = to_list !sym_set;
        specialise = HV.create 4});
      coords := to_list !coord_set;
      (* Add constant recurrences (form Vfₓ-fₓ=0) for all coordinates that atoms (form App(f,x)) lack. *)
      to_iter !sym_set (fun s -> match view s with App(_,x) ->
        to_iter(diff !coord_set (of_list x)) (fun v -> add_recurrence s R.(
          let v_const = ([[shift(var_index v)]] -- const_op_poly Z.one) >< of_term s in
          make_clause [R.polyliteral v_const] ~=Proof.(Step.inference [] ~rule:(Rule.mk "constantness"))))
        |_-> assert false);
      let r = recurrences_of main_expr in
      print_endline("Refuting "^ str lit);
      print_endline "Final recurrences:";
      List.iter (print_endline%str) r;
      (* Some[step "" (r) []] *)
      print_endline "————— summation_equality: Missing induction phase ⇒ terminated —————";
      exit 1
    |_->None
    else None)
  |> get_or ~default:[]


(*
let test_hook clause =
  let subenv, saturate = make_polynomial_environment() in
  let eq0' p = C.create (List.map R.polyliteral p) ~penalty:1 ~trail:(C.trail_l[]) (Proof.Step.assert' ~file:"" ~name:"" ()) in
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
    eq0'[R._0]] |> remove_at_idx(-1)) in
  let module SubEnv = (val subenv) in
  [step "" (Iter.to_rev_list(SubEnv.get_clauses())) []]
*)


(* Setup to do when MakeSumSolver(...) is called. *);;
(* MainEnv.add_unary_inf "test" test_hook; *)
MainEnv.add_unary_inf "recurrences for ∑" sum_equality_inference
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
    add_constr 0 (* priority 0=high—we coarsely define recurrence literals to be large, while tiebreaking must follow later *)
    (Precedence.Constr.make(fun a b -> match poly_of_id a, poly_of_id b with
      | None, Some _ -> 1
      | Some _, None -> -1
      | _ -> 0
    ))%
    (* We cannot simply define the KBO weights to be maximal too. Instead they are overriding parameters that we query by polyweight_of_id. *)
    update_weight_rule(fun wf _ id -> get_lazy (fun()-> wf id) (polyweight_of_id id))];
};;
(* TODO Setting --int-inf-diff-to-lesseq is vital but Arith_int.ml hides it currently. *)
Options.add_opts[
  "--sum-by-recurrences", Arg.Bool((:=)sum_by_recurrences), " use holonomic sequence method to sums (∑) in algebras";
]