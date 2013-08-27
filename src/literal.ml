
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Equational literals} *)

open Logtk
open Comparison.Infix

module T = Term
module S = Substs
module BV = Bitvector

type t =
 | Equation of    Term.t  (** lhs *)
                * Term.t  (** rhs *)
                * bool    (** sign (equality, ie true, or difference) *)
                * Comparison.t   (* TODO remove? or just orient equations? *)
  (** a literal, that is, a signed equation *)

let eq l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,ord1), Equation (l2,r2,sign2,ord2) ->
    sign1 = sign2 && l1 == l2 && r1 == r2 && ord1 = ord2

let eq_com l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
    sign1 = sign2 &&
    ((T.eq l1 l2 && T.eq r1 r2 && o1 = o2) ||
     (T.eq l1 r2 && T.eq r1 l2 && o1 = (Comparison.opp o2)))

let compare l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
      let c = Pervasives.compare o1 o2 in
      if c <> 0 then c else
        let c = T.compare l1 l2 in
        if c <> 0 then c else
          let c = T.compare r1 r2 in
          if c <> 0 then c else
            Pervasives.compare sign1 sign2

let to_multiset lit = match lit with
  | Equation (l, r, true, _) -> [l; r]
  | Equation (l, r, false, _) -> [l; l; r; r]

let compare_partial ~ord l1 l2 =
  (* Utils.multiset_partial ord#compare (lit_to_multiset l1) (lit_to_multiset l2) *)
  match l1, l2 with
  | Equation (s, t, sign_st, _), Equation (u, v, sign_uv, _) ->
    let s_u = Ordering.compare ord s u
    and s_v = Ordering.compare ord s v
    and t_u = Ordering.compare ord t u
    and t_v = Ordering.compare ord t v in
    match s_u, s_v, t_u, t_v, sign_st, sign_uv with
    | Eq, _, _, Eq, _, _
    | _, Eq, Eq, _, _, _ ->
      if sign_st = sign_uv then Eq
      else if sign_st then Lt
      else (assert sign_uv; Gt)
    | Gt, Gt, _, _, _, _        (* s dominates *)
    | _, _, Gt, Gt, _, _ -> Gt  (* t dominates *)
    | Gt, Eq, _, _, false, true (* s = v & s > u *)
    | Eq, Gt, _, _, false, true (* s = u & s > v *)
    | _, _, Gt, Eq, false, true (* t = v & t > u *)
    | _, _, Eq, Gt, false, true -> Gt (* t = u & t > v *)
    | Lt, _, Lt, _, _, _        (* u dominates *)
    | _, Lt, _, Lt, _, _ -> Lt  (* v dominates *)
    | Eq, _, Lt, _, true, false (* s = u, t < u *)
    | Lt, _, Eq, _, true, false (* t = u, s < u *)
    | _, Eq, _, Lt, true, false (* s = v, t < v *)
    | _, Lt, _, Eq, true, false -> Lt (* t = v, s < v *)
    | Eq, _, _, Gt, _, _        (* s = u, t > v *)
    | Gt, _, _, Eq, _, _        (* s > u, t = v *)
    | _, Eq, Gt, _, _, _        (* s = v, t > u *)
    | _, Gt, Eq, _, _, _        (* s > v, t = u *)
      when sign_uv = sign_st -> Gt
    | Eq, _, _, Lt, _, _        (* s = u, t < v *)
    | Lt, _, _, Eq, _, _        (* s < u, t = v *)
    | _, Eq, Lt, _, _, _        (* s = v, t < u *)
    | _, Lt, Eq, _, _, _        (* s < v, t = u *)
      when sign_uv = sign_st -> Lt
    | Eq, Eq, _, _, false, true (* s = u, s = v *)
    | _, _, Eq, Eq, false, true -> Gt (* t = u, t = v *)
    | _, Eq, _, Eq, true, false (* s = v, t = v *)
    | Eq, _, Eq, _, true, false -> Lt (* s = u, t = u *)
    | _ -> Incomparable

let hash lit = match lit with
  | Equation (l, r, sign, o) ->
    if sign
      then Hash.hash_int3 (Hash.hash_string o) l.T.tag r.T.tag
      else Hash.hash_int3 (Hash.hash_string o) r.T.tag l.T.tag

let weight = function
  | Equation (l, r, _ ,_) -> T.size l + T.size r

let depth = function
  | Equation (l, r, _ ,_) -> max (T.depth l) (T.depth r)

let is_pos lit = match lit with
  | Equation (_,_,sign,_) -> sign

let is_neg lit = match lit with
  | Equation (_,_,sign,_) -> not sign

let equational = function
  | Equation (l, r, _,_) -> l != T.true_term && r != T.true_term

let orientation_of = function
  | Equation (_, _, _, ord) -> ord

let check_type a b =
  let ok = not (T.has_type a) || not (T.has_type b) || T.compatible_type a b in
  assert ok

let mk_eq ~ord a b =
  check_type a b;
  Equation (a, b, true, Ordering.compare ord a b)

let mk_neq ~ord a b = 
  check_type a b;
  Equation (a, b, false, Ordering.compare ord a b)

let mk_lit ~ord a b sign =
  check_type a b;
  Equation (a, b, sign, Ordering.compare ord a b)

let apply_subst ?(recursive=true) ?renaming ~ord subst lit scope =
  match lit with
  | Equation (l,r,sign,_) ->
    let new_l = S.apply_subst ~recursive ?renaming subst l scope
    and new_r = S.apply_subst ~recursive ?renaming subst r scope in
    mk_lit ~ord new_l new_r sign

let reord ~ord (Equation (l,r,sign,_)) = mk_lit ~ord l r sign

let rec lit_of_fof ~ord ((Equation (l,r,sign,_)) as lit) =
  match l.T.term, r.T.term with
  (* deal with trivial literals *)
  | _ when T.eq l T.true_term && T.eq r T.false_term ->
    mk_lit ~ord T.true_term T.true_term (not sign)
  | _ when T.eq r T.true_term && T.eq l T.false_term ->
    mk_lit ~ord T.true_term T.true_term (not sign)
  | _ when T.eq l r ->
    mk_lit ~ord T.true_term T.true_term sign
    (* deal with false/true *)
  | _ when T.eq l T.false_term ->
    lit_of_fof ~ord (mk_lit ~ord r T.true_term (not sign))
  | _ when T.eq r T.false_term ->
    lit_of_fof ~ord (mk_lit ~ord l T.true_term (not sign))
    (* deal with negation *)
  | T.Node (s, [t]), _ when Symbol.eq s Symbol.not_symbol && T.eq r T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord t T.true_term (not sign))
  | _, T.Node (s, [t]) when Symbol.eq s Symbol.not_symbol && T.eq l T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord t T.true_term (not sign))
    (* deal with equality symbol *)
  | T.Node (s, [a; b]), _ when Symbol.eq s Symbol.eq_symbol && T.eq r T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord a b sign)
  | _, T.Node (s, [a; b]) when Symbol.eq s Symbol.eq_symbol && T.eq l T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord a b sign)
    (* default is just reordering *)
  | _ -> reord ~ord lit

let term_of_lit lit =
  match lit with
  | Equation (left, right, false, _) when T.eq right T.true_term ->
    T.mk_not left
  | Equation (left, right, true, _) when T.eq right T.true_term ->
    left
  | Equation (left, right, true, _) when T.eq left T.true_term ->
    right
  | Equation (left, right, false, _) when T.eq left T.true_term ->
    T.mk_not right
  | Equation (left, right, sign, ord) ->
    if sign then T.mk_eq left right else T.mk_not (T.mk_eq left right)

let negate (Equation (l,r,sign,ord)) = Equation (l,r,not sign,ord)

let fmap ~ord f = function
  | Equation (left, right, sign, _) ->
    let new_left = f left
    and new_right = f right in
    Equation (new_left, new_right, sign, Ordering.compare ord new_left new_right)

let add_vars set = function
  | Equation (l, r, _, _) ->
    T.add_vars set l;
    T.add_vars set r

let vars lit =
  let set = T.THashSet.create () in
  add_vars set lit;
  T.THashSet.to_list set

let infer_type ctx lit =
  match lit with
  | Equation (l,r,_,_) when T.eq r T.true_term ->
    TypeInference.constrain_term_type ctx l Type.o
  | Equation (l,r,_,_) when T.eq l T.true_term ->
    TypeInference.constrain_term_type ctx r Type.o
  | Equation (l,r,_,_) ->
    TypeInference.constrain_term_term ctx l r

let signature ?(signature=Signature.empty) lit =
  let ctx = TypeInference.Ctx.of_signature signature in
  let ctx = infer_type ctx lit in
  TypeInference.Ctx.to_signature ctx

let eq_lits lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then true else
    eq_com lits1.(i) lits2.(i) && check (i+1)
  in
  if Array.length lits1 <> Array.length lits2
    then false
    else check 0

let compare_lits lits1 lits2 = 
  let rec check i =
    if i = Array.length lits1 then 0 else
      let cmp = compare lits1.(i) lits2.(i) in
      if cmp = 0 then check (i+1) else cmp
  in
  if Array.length lits1 <> Array.length lits2
    then Array.length lits1 - Array.length lits2
    else check 0

let hash_lits lits =
  let h = ref 0 in
  Array.iter
    (fun (Equation (l, r, sign, _)) ->
      h := Hash.combine (Hash.combine !h l.T.tag) r.T.tag)
    lits;
  !h

let weight_lits lits =
  Array.fold_left (fun w lit -> w + weight lit) 0 lits

let depth_lits lits =
  Array.fold_left (fun d lit -> max d (depth lit)) 0 lits

let vars_lits lits =
  let set = T.THashSet.create () in
  for i = 0 to Array.length lits - 1 do
    add_vars set lits.(i);
  done;
  T.THashSet.to_list set

let ground_lits lits =
  let rec check i = if i = Array.length lits then true
    else match lits.(i) with
    | Equation (l, r, _, _) ->
      T.is_ground l && T.is_ground r && check (i+1)
  in check 0

let term_of_lits lits =
  match lits with
  | [||] -> T.false_term
  | _ -> Array.fold_left
    (fun t lit -> T.mk_or t (term_of_lit lit))
    (term_of_lit lits.(0)) (Array.sub lits 1 (Array.length lits - 1))

(** Apply the substitution to the array of literals, with scope *)
let apply_subst_lits ?(recursive=true) ?renaming ~ord subst lits scope =
  Array.map
    (fun lit -> apply_subst ~recursive ?renaming ~ord subst lit scope)
    lits

let apply_subst_list ?(recursive=true) ?renaming ~ord subst lits scope =
  List.map
    (fun lit -> apply_subst ~recursive ?renaming ~ord subst lit scope)
    lits

(** bitvector of literals that are positive *)
let pos_lits lits =
  let bv = ref BV.empty in
  for i = 0 to Array.length lits - 1 do
    if is_pos lits.(i) then bv := BV.set !bv i
  done;
  !bv

(** bitvector of literals that are positive *)
let neg_lits lits =
  let bv = ref BV.empty in
  for i = 0 to Array.length lits - 1 do
    if is_neg lits.(i) then bv := BV.set !bv i
  done;
  !bv

(** Bitvector that indicates which of the literals are maximal *)
let maxlits ~ord lits =
  let n = Array.length lits in
  (* at the beginning, all literals are potentially maximal *)
  let bv = ref (BV.make n) in
  for i = 0 to n-1 do
    (* i-th lit is already known not to be max? *)
    if not (BV.get !bv i) then () else
    for j = i+1 to n-1 do
      if not (BV.get !bv j) then () else
      match compare_partial ~ord lits.(i) lits.(j) with
      | Incomparable | Eq -> ()     (* no further information about i-th and j-th *)
      | Gt -> bv := BV.clear !bv j  (* j-th cannot be max *)
      | Lt -> bv := BV.clear !bv i  (* i-th cannot be max *)
    done;
  done;
  (* return bitvector *)
  !bv

(** Convert the lits into a sequence of equations *)
let lits_to_seq lits =
  Sequence.from_iter
    (fun k ->
      for i = 0 to Array.length lits - 1 do
        match lits.(i) with | Equation (l,r,sign,_) -> k (l,r,sign)
      done)

let lits_of_terms ~ord terms =
  let terms = Array.of_list terms in
  Array.map (fun t -> lit_of_fof ~ord (mk_eq ~ord t T.true_term)) terms

let lits_infer_type ctx lits =
  Array.fold_left infer_type ctx lits

let lits_signature ?(signature=Signature.empty) lits =
  let ctx = TypeInference.Ctx.of_signature signature in
  let ctx = lits_infer_type ctx lits in
  TypeInference.Ctx.to_signature ctx

(** {2 Special kinds of array} *)

(** Does t contains the symbol f? *)
let rec contains_symbol f t =
  match t.T.term with
  | T.Var _ | T.BoundVar _ -> false
  | T.Bind (s, t') -> s == f || contains_symbol f t'
  | T.Node (g, ts) -> g == f || List.exists (contains_symbol f) ts
  | T.At (t1, t2) -> contains_symbol f t1 || contains_symbol f t2

(** Recognized whether the clause is a Range-Restricted Horn clause *)
let is_RR_horn_clause lits = 
  let lit = ref None in
  (* find whether there is exactly one positive literal *)
  let rec find_uniq_pos n i =
    if i = Array.length lits
      then if n = 1 then !lit else None
      else begin
        match lits.(i) with
        | Equation (l,r,true,_) as lit' ->
          lit := Some lit';
          find_uniq_pos (n+1) (i+1)
        | _ -> find_uniq_pos n (i+1)
      end
  in
  match find_uniq_pos 0 0 with
  | None -> false
  | Some lit' ->
    (* check that all variables of the clause occur in the head *)
    let vars = vars lit' in
    List.length vars = List.length (vars_lits lits)

(** Recognizes Horn clauses (at most one positive literal) *)
let is_horn lits =
  (* Iterate on literals, counting the positive ones.
     [pos]: did we already meet a positive literal *)
  let rec iter_lits pos lits i =
    if i = Array.length lits then true else match lits.(i) with
      | Equation (_, _, true, _) ->
        if pos then false else iter_lits true lits (i+1)
      | Equation (_, _, false, _) -> iter_lits pos lits (i+1)
  in iter_lits false lits 0

(** Check whether the clause defines a symbol, e.g.
    subset(X,Y) = \forall Z(Z in X -> Z in Y). It means the LHS
    is a flat symbol with variables, and all variables in RHS
    are also in LHS *)
let is_definition lits =
  (* check that r is a definition of l=f(x1,...,xn) *)
  let check_def l r =
    match l.T.term with
    | T.Var _ | T.BoundVar _ | T.Bind _ | T.At _ -> false
    | T.Node (f, ts) ->
      (* l=f(x1,...,xn) where r contains no other var than x1,...,xn, and n > 0 *)
      T.atomic_rec l && ts <> [] && not (contains_symbol f r)
      && l != T.true_term && r != T.true_term
      && List.for_all T.is_var ts
      && List.for_all (fun x -> T.var_occurs x l) (T.vars r)
  in
  match lits with
  | [|Equation (({T.term=T.Node(_, _)} as l), r, true, _)|]
    when check_def l r -> Some (l, r)
  | [|Equation (l, ({T.term=T.Node(_, _)} as r), true, _)|]
    when check_def r l -> Some (r, l)
  | _ -> None

(** More general than definition. It means the clause is an
    equality where all variables in RHS are also in LHS. It
    can return two rewrite rules if the clause can be oriented
    in both ways, e.g. associativity axiom. *)
let is_rewrite_rule lits =
  (* check that l -> r is an acceptable rewrite rule *)
  let check_rule l r =
    match l.T.term with
    | T.Var _ | T.Bind _ | T.BoundVar _ | T.At _ -> false
    | T.Node (_, _) ->
      T.atomic_rec l && l != T.true_term && r != T.true_term &&
      List.for_all (fun x -> T.var_occurs x l) (T.vars r)
  in
  match lits with
  | [|Equation (l, r, true, _)|] ->
    (if check_rule l r then [l, r] else []) @ (if check_rule r l then [r, l] else [])
  | _ -> []

let is_pos_eq lits =
  match lits with
  | [|Equation (l,r,true,_)|] -> Some (l,r)
  | _ -> None

(** Checks whether the clause is "const = ground composite term", e.g.
    a clause "aIbUc = inter(a, union(b, c))". In this case it returns
    Some(constant, definition of constant) *)
let is_const_definition lits =
  match lits with
  | [|Equation (l,r,true,_)|] when T.is_const l && T.is_ground r
    && not (T.member_term l r) ->
    Some (l,r)
  | [|Equation (l,r,true,_)|] when T.is_const r && T.is_ground l
    && not (T.member_term r l) ->
    Some (r,l)
  | _ -> None

(** {2 IO} *)

let pp buf lit =
  match lit with
  | Equation (l, r, sign, _) when T.eq r T.true_term ->
    if sign
      then T.pp buf l
      else Printf.bprintf buf "¬%a" T.pp l
  | Equation (l, r, sign, _) when T.eq l T.true_term ->
    if sign
      then T.pp buf r
      else Printf.bprintf buf "¬%a" T.pp r
  | Equation (l, r, sign, _) when T.is_bool l ->
    if sign
      then Printf.bprintf buf "%a <=> %a" T.pp l T.pp r
      else Printf.bprintf buf "%a <~> %a" T.pp l T.pp r
  | Equation (l, r, sign, _) ->
    if sign
      then Printf.bprintf buf "%a = %a" T.pp l T.pp r
      else Printf.bprintf buf "%a != %a" T.pp l T.pp r

let pp_lits buf lits = 
  Util.pp_arrayi ~sep:" | "
    (fun buf i lit -> Printf.bprintf buf "%a" pp lit)
    buf lits

let to_string t = Util.on_buffer pp t

let lits_to_string a = Util.on_buffer pp_lits a

let fmt fmt lit =
  Format.pp_print_string fmt (to_string lit)

let fmt_lits fmt lits =
  Format.pp_print_string fmt (lits_to_string lits)

let bij ~ord =
  let open Bij in
  map
    ~inject:(fun (Equation (l,r,sign,_)) -> l,r,sign)
    ~extract:(fun (l,r,sign) -> mk_lit ~ord l r sign)
    (triple T.bij T.bij bool_)

let bij_lits ~ord =
  let open Bij in
  map
    ~inject:(fun a -> Array.to_list a)
    ~extract:(fun l -> Array.of_list l)
    (list_ (bij ~ord))
