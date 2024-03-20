[@@@warning "-10-20-21-26"](* Before final cleanup, tolerate unused values and definitions. *)
open Logtk
open Logtk_parsers
open Logtk_proofs
open Logtk_arith
open Libzipperposition
open Libzipperposition_calculi
open Phases_impl
open Util
open UntypedPrint
open String
open Precedence.Weight
open Comparison
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
module R = RecurrencePolynomial
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

let (=) = R.(=)
let todo = R.todo
let (~=) x _ = x
let (@@) = CCPair.map_same
let (%%>) = compose_binop
let (%>>) f g x y = g(f x y)
let (<:>) r f = r:=f!r

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

let index_of = R.index_of

(* Search hash table by value instead of by key. The equality relation of keys does not matter. *)
let search_hash ?(eq=(=)) table value = HS.fold (fun k v found -> if found=None & eq value v then Some k else found) table None

let with_cache_2 c f = curry(with_cache_rec c (uncurry % f % curry))
let with_cache_3 c f = curry(with_cache_2 c (uncurry % f % curry))

let ctrl_C_stoppable act =
  Sys.catch_break true;
  (try act() with
  | Sys.Break -> ()
  | Failure b when starts_with~prefix:"Stdlib.Sys.Break" b -> ());
  Sys.catch_break false

let get_clauses(type c)(module Env: Env.S with type C.t=c) = Iter.append (Env.get_active()) (Env.get_passive())


(* make constants for debugging *)
let constants = HS.create 0
let have ?(infix=false) name par ty = match HS.find_opt constants name with
| None ->
  let i = ID.make name in
  if infix then ID.set_payload i (ID.Attr_infix name);
  let c = const ~ty:(arrow par ty) i in
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




module MakeSumSolver(MainEnv: Env.S) = struct
module C = MainEnv.C

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
    | Ok _ -> get_clauses(module PolyEnv)

(* note: maybe this is the only interface needed and can replace make_polynomial_environment *)
let saturate_with_order = snd % make_polynomial_environment


(* Drop None's after mapping eligible non-trivial polynomial recurrences by the given transformation. *)
let filter_map_recurrences map_poly = Iter.filter_map(fun c ->
  try Some(make_clause~parents:[c] (fold_lits ~eligible:(C.Eligible.res c) (C.lits c)
    |> Iter.map R.(fun(l,_) -> match poly_of_lit l with
      | Some p when p!=_0 ->
        let p' = get_exn(map_poly p) (* exception catched *)
        in if poly_eq p' p then l else let l',_,_ = poly_as_lit_term_id p' in l'
      | _ -> l)
    |> Iter.to_rev_list) ~=Proof.Step.trivial)
  with Invalid_argument _ -> None)

(* Filter those clauses that have eligible polynomial whose every term M f, where term f represents an expression from polylist, has normal operator monomial M (that consist only of multipliers and 1-shifts). Moreover the leading term must have such form M f. After the filtering, unembed all other polynomials outside polylist.
 Notes: The ordering of the embedded polynomial term f can be problematic. The polynomial structure of f does not automatically contribute to the ordering. Hence requiring, that M f leads, silently discards recurrences that forgot to take f into account in the ordering. However at least propagation to addition and affine forms relies to the presence of the requirement that M f leads. Now try making leading requirement optional via parameter. *)
let filter_recurrences_of ?(lead=true) polylist = filter_map_recurrences R.(fun p -> if_~=
  (let wanted_terms = p |> map_monomials(fun m -> match terms_in[m] with
    | [t] when mem ~eq:(CCOpt.equal poly_eq) (poly_of_term t) (map CCOpt.pure polylist) -> [m]
    | _ -> _0)
  in not(poly_eq _0 wanted_terms)
  & (not lead or mono_eq (hd p) (hd wanted_terms))
  & poly_eq wanted_terms (oper_coef_view wanted_terms))
  (unembed(fun p' -> not(mem~eq:poly_eq p' polylist)) p))

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

(* Take polynomial expression and propagate recurrences to it based on its structure. The main branch point. *)
let rec propagate_recurrences_to f = match PolyMap.find_opt recurrence_table f with
| Some r -> r
| None ->
  let r = Iter.to_rev_list(match "propagating to "|<f with
  | [X[A(V _)]::_] | _::_::_ -> propagate_oper_affine f
  | [X q :: p] -> propagate_times [q] [p]
  | [S i :: p] -> propagate_sum i [p]
  | [O s :: p] -> propagate_subst s [p]
  | [C _ :: p] -> propagate_const f
  | _ -> Iter.empty)
  in
  PolyMap.add recurrence_table f r;
  r

and propagate_const f = match f with [R.C _ :: p] ->
  let _,ft,_ = R.poly_as_lit_term_id f in
  let _,pt,_ = R.poly_as_lit_term_id[p] (*essential to preserve equality here*) in
  propagate_recurrences_to[p]
  |> map(map_poly_in_clause R.(map_terms(fun t -> if t==pt then ft else t)))
  |> Iter.of_list
|_-> failwith("propagate_const: "^ R.poly_to_string f ^" should have been an explicit constant multiple of a monomial")

and propagate_oper_affine p's_on_f's =
  let _,result,_ = R.poly_as_lit_term_id p's_on_f's in
  let pf_sum = R.oper_coef_view p's_on_f's in
  R.arg_terms_in pf_sum
  |> map(fun a -> get_or~default:a R.(poly_of_term(term_of_arg a)))
  |> flat_map propagate_recurrences_to
  |> Iter.of_list
  |> Iter.cons(definitional_poly_clause R.(of_term result -- pf_sum))
  |> saturate_with_order R.(elim_oper_args((result,1) :: map(fun f -> f,2) (terms_in pf_sum)))
  |> filter_recurrences_of[p's_on_f's]

and propagate_times f g =
  (* 1. rename (ϱ) var.s of g with merge σ *)
  let rename, merge, _ = R.rename_apart ~taken:(R.free_variables f) (R.free_variables g) in
  (* 2. create Rᶠ·ϱg and f·ϱRᵍ *)
  let disjoint_recurrences = R.(
    map(map_poly_in_clause(fun r -> r><mul_indet([rename]><g))) (propagate_recurrences_to f) @
    map(map_poly_in_clause(fun r -> mul_indet f><[rename]><r)) (propagate_recurrences_to g)) in
  match merge with
  | [] -> Iter.of_list disjoint_recurrences
  (* 3. propagate to substitution σ(f·ϱg) when σ!=id *)
  | [O m] ->
    let f_x_g' = R.(mul_indet f><[rename]>< g) in
    PolyMap.add recurrence_table f_x_g' disjoint_recurrences;
    propagate_subst m f_x_g'
  | _ -> assert false (* impossible result from R.rename_apart *)

and propagate_sum i f =
  let is_f t = R.(CCOpt.equal poly_eq (Some f) (poly_of_term t)) in
  let sum_blocker = function (* gives elimination priorities: [] is good, [1] is bad *)
  |`T t when not(is_f t) -> []
  |`V v when v!=i -> []
  |`O[u, R.[[A(V v)];[A I]]] when v=u -> []
  | _ -> [1]
  in
  let sumf = R.(let _,sumf,_ = poly_as_lit_term_id([[S i]]><f) in of_term sumf) in
  let _,f_term,_ = R.poly_as_lit_term_id f in
  let embed_sumf_rewriter = definitional_poly_clause R.(([[S i]]>< of_term f_term) -- sumf) in
  Iter.of_list(propagate_recurrences_to f)
  |> saturate_with_order(R.elim_indeterminate' sum_blocker)
  |> Iter.map(map_poly_in_clause R.((><)[[S i]]))
  (* Replace each ∑ᵢf by the embedding sumf by rewriting with temporary polynomial ∑ᵢf-sumf. *)
  |> Iter.cons embed_sumf_rewriter
  (* We orient the rewriteter while keeping sumf leading for filtering. Hence we need W ∑ᵢ + W f > W sumf, but at the same time sumf must dominate arbitrarily deep ∑'s in excess terms as well as substitutions of f. *)
  |> saturate_with_order(R.elim_indeterminate'(function`S _->[2] |`T sum'f when sum'f==hd(R.terms_in sumf) -> [2;1] |`T f' when f'==f_term -> [1;1] | _->[]))
  |> Iter.filter((!=)embed_sumf_rewriter) (* perhaps filtered anyway in the end *)
  (* Add the definitional N∑ᑉⁿf = ∑ᑉⁿf + fₙ with sumf embedded while f is not. *)
  |> Iter.cons(definitional_poly_clause R.(([[shift i]]><sumf) -- sumf -- f))
  |> filter_recurrences_of R.[ [[S i]]><f ] (* recurrences of sumf, but the embedding level is weird *)

and propagate_subst s f =
  (* 1. for each range coordinate create compound shift *)
  (* 2. create equations: compound shift = composition of 1-shifts ↭ 𝕊ⱼ = ∏ᵢ Sᵢ^mᵢⱼ ↭ 𝕊ᵃ = Sᵐᵃ *)
  (* 3. saturate *)
  (* 4. replace each compound shift by a 1-shift, and substitute multipliers *)
  let _,f_term,_ = R.poly_as_lit_term_id f in
  let _,sf_term,_ = R.(poly_as_lit_term_id([o s]><f)) in
  (* special placeholder for f that depends on both range and domain variables to prevent simplification of shifts: *)
  let _,s'f_term,_ = R.(poly_as_lit_term_id(f++([o s]><f))) in
  let f_is_s'f = definitional_poly_clause R.(f -- of_term s'f_term) in
  let effect_dom = Int_set.of_list(map fst s) in
  let dom = R.free_variables f in
  let s'f = R.of_term~vars:Int_set.(to_list(union effect_dom (R.rangeO s))) s'f_term in
  assert(Int_set.subset effect_dom dom);
  match R.view_affine s with
  | None -> Iter.empty
  | Some(m,a) ->
    (* If 𝕊ⱼ=Sⱼ, we skip it, which makes saturation faster, but primarily this is to dodge the issue of telling 𝕊ⱼ and Sⱼ apart. *)
    let changedShift = filter(fun j -> exists R.(fun i -> m@.(i,j) != if i=j then 1 else 0) (Int_set.to_list dom)) in
    let multishifts_and_equations = changedShift(Int_set.to_list(R.rangeO s)) |> map R.(fun j ->
      let on_dom f = map f (Int_set.to_list dom) in
      (* 𝕊ⱼ = ∏ᵢ Sᵢ^mᵢⱼ = O[0,m₀ⱼ;...;i,mᵢⱼ;...] where j runs over range and i over domain *)
      let ss_j = hd(o(on_dom(fun i -> i, [var_poly i]++const_eq_poly(Z.of_int(m@.(i,j)))))) in
      (* TODO Recurrence polynomial data structure needs an additional marker to distinguish compound shifts from ordinary ones in all corner cases including permutations.
       We can tolerate a confusion between single and multi for a shift w.r.t. a new variable. This relaxation is good to keep in mind when transforming multishifts to 1-shifts. *)
      let ss_j = if not(is1shift ss_j) then ss_j
        else if not(Int_set.mem j dom) then shift j (* use final form immediately *)
        else failwith("Unimplemented: "^ string_of_int j^"ᵗʰ compound shift cannot be distinguished from 1-shift when propagating to substitution "^ poly_to_string[o s] ^" of "^ poly_to_string f) in
      let eq_j = product(on_dom(fun i -> pow' poly_alg [[shift i]] (max 0 (m@.(i,j)))))
        -- product([[ss_j]] :: on_dom(fun i -> pow' poly_alg [[shift i]] (- min 0 (m@.(i,j))))) in
      ((ss_j, shift j), j), eq_j
    ) in
    (* We must eliminate all domain shifts except ones skipped above: dom\( rangeO s \ {j | ∃ ss_j} ) *)
    let elimIndices = Int_set.(diff dom (diff (R.rangeO s) (of_list(List.map(snd%fst) multishifts_and_equations)))) in
    (* * * * Clause prosessing chain: turn recurrences of f to ones of “s'f”, add 𝕊=∏S, eliminate S, push ∘s i.e. map 𝕊ⱼ↦Sⱼ, and filter *)
    propagate_recurrences_to f
    |> map(map_poly_in_clause R.(map_submonomials(fun n -> CCOpt.if_~=(poly_eq f [n]) s'f)))
    |> Iter.of_list (* Above is undone via s'f_term ↦ sf_term at substitution push. *)
    |> Iter.append(Iter.of_list(map (definitional_poly_clause % snd) multishifts_and_equations))
    |> saturate_with_order(fun m' -> function
      |`O[i,R.[[A(V j)];[A I]]] when i=j & Int_set.mem i elimIndices ->
        (* Eliminate 1-shifts ...except that multishift could be simplified to 1-shift in case an excess argument term does not depend on some of the multishifted variables—a valid result of such simplification won't be penaliced. *)
        if multishifts_and_equations |> exists R.(function(((O ss,_),_),_)-> (* ∃𝕊 *)
          Int_set.(match assq_opt i ss with Some[[A(V j)];[A I]] when i=j -> (* 𝕊i = i+1 *)
            equal (of_list[i]) (inter (free_variables[m']) (of_list(List.map fst ss))) (* Sᵢ goes as well *)
          |_->false) |_->false)
        then [] else [1]
      | _ -> [])
    |> Iter.filter_map(fun c -> try Some(c |> map_poly_in_clause R.(fun p ->
      let cache_t_to_st = HT.create 4 in
      let p = if equational p then p else p>< of_term s'f_term in
      p |> map_monomials(fun m' -> [m']|>
        (* If the substitution does not change variables in the monomial m, then m can be kept as is, even if m contains shifts that were to be eliminated. This is not robust because generally the essence is that m is constant (or even satisfies more recurrences compared to f) w.r.t. some variables, while the effect of the substitution plays no rôle. *)
        if Int_set.inter effect_dom (free_variables[m']) = Int_set.empty then id
        else map_outer_indeterminates(let rec transf upcoming = function
          | C a -> `Go, const_op_poly a
          (* Apply substitution to terminal indeterminates. *)
          | A I -> `Go, const_eq_poly Z.one
          | A(V i) as x -> CCPair.map_snd((><)(const_eq_poly Z.one)) (transf upcoming (hd(hd(mul_indet[[x]]))))
          | A(T(s'f,vars)) when s'f==s'f_term -> `Go, of_term sf_term
          | A(T(t,vars)) when HT.mem cache_t_to_st t -> `Go, HT.find cache_t_to_st t
          | A(T(t,vars)) ->
            let _,st,_ = poly_as_lit_term_id([o s] >< get_or~default:(of_term~vars t) (poly_of_term t)) in
            let st = of_term st in
            HT.add cache_t_to_st t st; `Go,st
          (* Replace Mᵢ=X[A(V i)] by ∑ⱼmᵢⱼMⱼ. *)
          | X[A(V i)] -> `Go, fold_left (++) _0 (map(fun j -> const_op_poly(Z.of_int(m@.(i,j))) >< [[mul_var j]]) (Int_set.to_list(rangeO s)))
          (* Discard clauses still containing shifts that were to be eliminated. Since elimIndices ⊆ dom, we keep shifts of new variables that the below case interpretates as multishifts that need no further transforming. *)
          | O[i,[[A(V j)];[A I]]]as x when i=j & Int_set.mem i elimIndices -> raise Exit
          | O[i,[[A(V j)];[A I]]]as x when i=j -> `Go,[[x]]
          (* Replace 𝕊ⱼ by Sⱼ by looking it up from multishifts_and_equations. *)
          | O _ as x -> (match assoc_opt ~eq:indet_eq x (map(fst%fst) multishifts_and_equations) with
            | Some sx	-> `Go, [[sx]]
            | None	-> `End, [o s]><[upcoming])
          | _ -> `End, [o s]><[upcoming]
          in transf))))
      with Exit -> None)
    |> filter_recurrences_of~lead:false [get_exn(R.poly_of_term sf_term)(* safe by definition of sf_term *)]


(* Make a polynomial into a new recurrence of its maximal term. Since polynomial is packed into a new clause, this function in this form is only suitable for testing. *)
let declare_recurrence r = add_new_rec
  R.(max_list ~ord:(term_of_arg%%>Term.compare) (arg_terms_in r))
  (definitional_poly_clause r)

let sum_equality_inference clause = try
  (* for testing ↓ *)
  let (!) = R.poly_of_string in
  let eq0' p = make_clause (map R.polyliteral p) ~=Proof.Step.trivial in
  let split_or k d s = match String.split_on_char k s with
  | [a;b] -> a,b | [a] -> if k='.' then d,a else a,d | _ -> raise Exit in
  let eq0 ss = eq0' R.[fold_left (++) _0 (List.map(fun cmt ->
    let c,mt = split_or '.' "1" (String.trim cmt) in
    let m,t = split_or '\'' "" mt in
    Z.of_string c *: poly_of_string m >< of_term~vars:(todo"vars in eq0")(have t [] int)
  ) (String.split_on_char '+' ss))] in
  
(*
  declare_recurrence !"NNf-Nf-f";
  declare_recurrence !"Ng-ng-g";
  propagate_recurrences_to !"nf"; (* OK *)
  propagate_recurrences_to !"g+f"; (* OK *)
  propagate_recurrences_to !"g+nf"; (* diverge? *)
  propagate_recurrences_to !"∑ᵐb"; (* OK *)

  propagate_recurrences_to !"{ᵐm-n-1}b"; (* OK *)
  propagate_recurrences_to !"∑ⁿ{ᵐm-n-1}b"; (* OK *)
  propagate_recurrences_to !"{ⁿm}∑ⁿ{ᵐm-n-1}b"; (* OK *)
  propagate_recurrences_to !"{ⁿm}∑ⁿ{ᵐm-n-1}b - ∑ᵐb"; (* OK *)
*)
  ["z","xn"; "q","x"; "b","mn"; "c","mn"; "f","m"; "g","mn"; "γ","mn"]
    |> map(uncurry(HS.add R.variable_dependency));
  let init_rec_table() = PolyMap.clear recurrence_table;
    declare_recurrence !"Nz-xz	"; (* zₓₙ = xⁿ *)
    declare_recurrence !"MMf-Mf-f	"; (* fₘ = mᵗʰ Fibonacci *)
    declare_recurrence !"xXq+Xq-q	"; (* qₓ = x!⁻¹ *)
    declare_recurrence !"mMb-nMb+Mb-mb-b	"; (* binomial coefficient: *)
    declare_recurrence !"nNb+Nb-mb-nb	"; (* bₘₙ = (ₙͫ ) *)
    declare_recurrence !"MNc-nNc-Nc-c	"; (* Stirling cₘₙ = {ₙͫ } *)
    declare_recurrence !"MMγ-γ-Mg-g	"; (* double step γ=∑g *)
  in let tests = [|
    "{ᵐm+u}{ⁿh}b	- {ⁿh+1}∑ⁿB{ᵐu}{ⁿh-n}b	";(*0 convolution *)
    "∑ᵐ{ⁿm+1}∑ⁿg	- {ⁿm}∑ⁿ∑ᵐg + {ⁿm}∑ⁿ{ᵐn}∑ᵐg	";(*1 interchange *)
    "∑ᵐg	- {ˣm}∑ˣ{ᵐm-1-x}g	";(*2 reverse *)
    "∑ᵐg	- γ	";(*3 elementary *)
    "{ˣx+y}{ⁿm}z	- {ⁿm+1}∑ⁿBx{ˣx-n}{ⁿn-1}Z{ˣy+n+1}{ⁿm-n-1}z	";(*4 Abel *)
    "Q{ⁿm}z	- {ⁿx+1}∑ⁿC{ˣx-n}q	";(*5 Stirling *)
    "{ˣx+y}{ⁿm}z	- {ⁿm+1}∑ⁿBZ{ˣy}{ⁿm-n}z	";(*6 binomial *)
    "{ᵐn+1}f	- {ᵐn+1}∑ᵐ{ⁿn-m}b	";(*7 Fibonacci *)
    "γ	- {ˣm}∑ˣ{ᵐm-1-x}g	";(*8 presented *)
    "0	- {ᵐm-1-x}g	";(*9 debug *)
  |]in(*
    (p,)r,e,i: ✓
    c,b,A,F,S: leviää etenkin bₘₙ kanssa (? ja sijoituksille jäi aikoinaan kaavoja löytymättä ?)
  *)
  let rec go() =
    print_string"tutki: ";
    init_rec_table();
    let cmd = read_line() in
    if String.length cmd == 1 then(
      let p = (!)tests.(int_of_string cmd) in
      print_endline("Tutkitaan " ^ R.poly_to_string p);
      ctrl_C_stoppable(fun()->
        propagate_recurrences_to p;
        ~<recurrence_table;()))
    else R.(match String.split_on_char ',' cmd with [p;p'] ->
      let p,p' = poly_of_string@@(p,p') in
      print_endline(match lead_unifiers p p' with
      | None -> "Samastumattomat "^ poly_to_string p ^", "^ poly_to_string p'
      | Some(u,u') -> "Unify "^poly_to_string[u]^", "^poly_to_string[u']^"\n"
        ^"p̲p̲. "^poly_to_string(hd(superpose p p'))^"\n"
        ^match CCOpt.or_~else_:(leadrewrite p p')(leadrewrite p' p) with
        | Some r -> "rw. "^ poly_to_string r
        | None -> "")
    | x -> print_endline("Paloja jaettaessa pilkusta pitää tulla 2 eikä " ^ string_of_int(length x)));
    go()
  in go();
  exit 1;
  let seeSatur pp = ((~<) % Iter.to_list % saturate_with_order~= ~=[1] % Iter.of_list % map eq0) pp in
  seeSatur["vvvvv"; "vv-1"];
  seeSatur["yyx-x-y"; "yxx-x-1"];
  seeSatur[
  "3vvvvvvvvvvvvvvvvvvv"; "3vvvvvvvvv + 1";
  (* eq0"x + y-v"; eq0"xx + yy-vv"; *)
  (* eq0"xx + 3x + 1"; eq0"yy + 3y + 1"; eq0"xxxxx + yyyyy"; *)
  
  (* eq0"2nN-m + 3"; eq0"NM + 2m";  *)
  (* eq0"2nnNN-nN + 3"; eq0"NNN + 2nN";   *)
  
  (* eq0"Xb-b-Xf"; eq0"h-b + g";  *)
  (* eq0"Xg-g-Xf"; *)
  (* eq0"-XXg + g + XXf + Xf"; *)
  
  (* eq0"Yb-2b"; eq0"-4g + yyb + yb"; *)
  (* eq0"xXb-yXb + Xb-xb-b"; eq0"yYb + Yb-xb + yb"; eq0"f-yyb"; *)
  (* Change priority for ↓ *)
  (* eq0"xXf-yXf + Xf-xf-f"; eq0"yyYf-yxf + yyf-xf + yf"; *)

  (* eq0"-XxxX∑f+3xxX∑f+9xX∑f+6X∑f-2xx∑f-6x∑f-4∑f"; eq0"xXg-2xg-4g"; eq0"h-∑f+g"; *)
  
  ];
  exit 1
  (* tl(tl[clause;clause]) *)
with e -> print_endline Printexc.((*get_backtrace() ^"\n"^ *)match e with Failure m -> m | e -> to_string e); exit 1

(* Fibonacci ongelmatulosteotos:
summation_equality line 292 ≣̲̇26       propagating to  {ᵐn＋１͘}fₘ
CCOption.pp line 9 ≣̲̇49        rw.M－N
 NMfₘ－Mfₘ－fₘ
CCOption.pp line 9 ≣̲̇50        rw.M－N
 -Mfₘ＋N²fₘ－fₘ
CCOption.pp line 9 ≣̲̇50        rw.M－N
 N²fₘ－Nfₘ－fₘ
RecurrencePolynomial line 605 ≣̲̇48     p̲p̲. -NMfₘ－Mfₘ＋N³fₘ
CCOption.pp line 9 ≣̲̇48        rw.M－N
 -Mfₘ＋N³fₘ－N²fₘ
CCOption.pp line 9 ≣̲̇48        rw.M－N
 N³fₘ－N²fₘ－Nfₘ
CCOption.pp line 9 ≣̲̇49        rw.N²fₘ－Nfₘ－fₘ
	‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  ‽  -Nfₘ＋fₘ
CCOption.pp line 9 ≣̲̇49        rw.-Nfₘ＋fₘ
 -Nfₘ
CCOption.pp line 9 ≣̲̇49        rw.-Nfₘ＋fₘ
 -fₘ
RecurrencePolynomial line 605 ≣̲̇48     p̲p̲. Mfₘ－N²fₘ
CCOption.pp line 9 ≣̲̇47        rw.M－N
 -N²fₘ＋Nfₘ
CCOption.pp line 9 ≣̲̇48        rw.-Nfₘ＋fₘ
 Nfₘ－fₘ
CCOption.pp line 9 ≣̲̇49        rw.-fₘ
 -Nfₘ＋２fₘ
CCOption.pp line 9 ≣̲̇49        rw.-Nfₘ＋fₘ
 fₘ
CCOption.pp line 9 ≣̲̇50        rw.-fₘ
 0
RecurrencePolynomial line 605 ≣̲̇48     p̲p̲. -Nfₘ
CCOption.pp line 9 ≣̲̇47        rw.-fₘ
 -Nfₘ＋fₘ
CCOption.pp line 9 ≣̲̇48        rw.-fₘ
 -Nfₘ＋２fₘ
CCOption.pp line 9 ≣̲̇49        rw.-fₘ
 -Nfₘ＋３fₘ
CCOption.pp line 9 ≣̲̇50        rw.-fₘ
 -Nfₘ＋４fₘ
*)

(* Stirling ongelmatulosteotos:
summation_equality line 292 ≣̲̇28       propagating to  {ⁿx}cₙₘ
CCOption.pp line 9 ≣̲̇51        rw.N－X
 XMcₙₘ－nNcₙₘ－Ncₙₘ－cₙₘ
RecurrencePolynomial line 605 ≣̲̇50     p̲p̲. -nN²cₙₘ－２N²cₙₘ＋X²Mcₙₘ－Ncₙₘ
CCOption.pp line 9 ≣̲̇50        rw.N－X
 -２N²cₙₘ＋X²Mcₙₘ－nXNcₙₘ－Ncₙₘ
CCOption.pp line 9 ≣̲̇51        rw.N－X
 X²Mcₙₘ－nXNcₙₘ－２XNcₙₘ－Ncₙₘ
CCOption.pp line 9 ≣̲̇51        rw.XMcₙₘ－nNcₙₘ－Ncₙₘ－cₙₘ
 -XNcₙₘ－Ncₙₘ＋cₙₘ
CCOption.pp line 9 ≣̲̇52        rw.N－X
 -Ncₙₘ－X²cₙₘ＋cₙₘ
CCOption.pp line 9 ≣̲̇52        rw.N－X
 -X²cₙₘ－Xcₙₘ＋cₙₘ
*)

(* Setup to do when MakeSumSolver(...) is called. *);;
MainEnv.add_unary_inf "recurrences for ∑" sum_equality_inference
end

(* Is this extension enabled? Set by a command line option. *)
let sum_by_recurrences = ref false

(* Define name and setup actions required to registration of this extension in libzipperposition_phases.ml *)
let extension = R.{
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

Options.add_opts[
  "--sum-by-recurrences", Arg.Bool((:=)sum_by_recurrences), " use holonomic sequence method to sums (∑) in algebras";
]