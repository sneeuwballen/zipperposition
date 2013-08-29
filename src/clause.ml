
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

(** {1 Clauses} *)

open Logtk
open Comparison.Infix

module T = Term
module Lits = Literal
module S = Substs
module BV = Bitvector

let stat_fresh = Util.mk_stat "fresh_clause"
let stat_mk_hclause = Util.mk_stat "mk_hclause"
let stat_new_clause = Util.mk_stat "new_clause"
let prof_check_max_lit = Util.mk_profiler "check_max_lit"
let prof_mk_hclause = Util.mk_profiler "mk_hclause"
let prof_mk_hclause_raw = Util.mk_profiler "mk_hclause_raw"

(** {2 Type def} *)

type t = {
  hclits : Literal.t array;               (** the literals *)
  hcctx : Ctx.t;                          (** context of the clause *)
  mutable hctag : int;                    (** unique ID of the clause *)
  mutable hcflags : int;                  (** boolean flags for the clause *)
  mutable hcweight : int;                 (** weight of clause *)
  mutable hcselected : Bitvector.t;       (** bitvector for selected literals *)
  mutable hcvars : Term.t list;           (** the free variables *)
  mutable hcproof : Proof.t;              (** Proof of the clause *)
  mutable hcparents : t list;             (** parents of the clause *)
  mutable hcdescendants : int SmallSet.t ;(** the set of IDs of descendants of the clause *)
} 

type clause = t

let compact c = CompactClause.create c.hctag c.hclits

let to_seq c =
  let lits = Sequence.of_array c.hclits in
  Sequence.map (function | Lits.Equation (l,r,sign,_) -> l,r,sign) lits

let to_prec_clause c =
  let lits = Sequence.of_array c.hclits in
  let lits = Sequence.map
    (function Lits.Equation (l,r,sign,_) ->
      if sign then T.mk_eq l r else T.mk_neq l r)
    lits
  in
  lits

 (** {2 boolean flags} *)

let flag_ground = 1 lsl 0
let flag_lemma = 1 lsl 1
let flag_persistent = 1 lsl 2

let set_flag flag c truth =
  if truth
    then c.hcflags <- c.hcflags lor flag
    else c.hcflags <- c.hcflags land (lnot flag)

let get_flag flag c = (c.hcflags land flag) != 0

(** {2 Hashcons} *)

let eq hc1 hc2 = hc1.hctag = hc2.hctag

let compare hc1 hc2 = hc1.hctag - hc2.hctag

let hash c = Lits.hash_lits c.hclits

module CHashtbl = Hashtbl.Make(struct
  type t = clause
  let hash c = hash c
  let equal c1 c2 = eq c1 c2
end)

module CHashSet = struct
  type t = unit CHashtbl.t
  let create () = CHashtbl.create 13
  let is_empty t = CHashtbl.length t = 0
  let member t c = CHashtbl.mem t c
  let iter t f = CHashtbl.iter (fun c _ -> f c) t
  let add t c = CHashtbl.replace t c ()
  let to_list t =
    let l = ref [] in
    iter t (fun c -> l := c :: !l);
    !l
end

(** {2 Utils} *)

(** comparison of variables by index *)
let compare_vars a b =
  match a.T.term, b.T.term with
  | T.Var i, T.Var j -> i - j
  | _ -> assert false

(** check whether variables are from 0 to n *)
let check_normal vars =
  let rec check prev = function
  | [] -> true
  | {T.term=T.Var n}::l -> n = prev+1 && check n l
  | _ -> assert false
  in check (-1) vars

(** [is_child_of ~child c] is to be called to remember that [child] is a child
    of [c], is has been infered/simplified from [c] *)
let is_child_of ~child c =
  (* update the parent clauses' sets of descendants by adding [child] *)
  let descendants = SmallSet.add c.hcdescendants child.hctag in
  c.hcdescendants <- descendants

module CHashcons = Hashcons.Make(struct
  type t = clause
  let hash c = Lits.hash_lits c.hclits
  let equal c1 c2 = Lits.eq_lits c1.hclits c2.hclits && c1.hcctx == c2.hcctx
  let tag i c = (assert (c.hctag = (-1)); c.hctag <- i)
end)

(** the tautological empty clause *)
let true_clause ~ctx =
  let hcflags = flag_ground in
  let hclits = [| Lits.mk_eq ~ord:Ordering.none T.true_term T.true_term |] in
  let c = { hclits; hcproof = Obj.magic 0;
      hctag = -1; hcweight=2; hcselected=0; hcflags; hcctx=ctx;
      hcvars=[]; hcparents=[];
      hcdescendants=SmallSet.empty ~cmp:(fun i j -> i-j); }
  in
  let c = CHashcons.hashcons c in
  c.hcproof <- Proof.mk_infer (compact c) "trivial" [];
  c

(* TODO: use a (var, scope) -> int  hashtable to always produce
   normalized clauses, by construction (see renaming in logic-terms) *)

(** Build a new hclause from the given literals.
    If there are more than [BV.max_len] literals,
    the prover becomes incomplete by returning [true] instead. *)
let create_a ?parents ?selected ~ctx lits proof =
  Util.incr_stat stat_mk_hclause;
  Util.enter_prof prof_mk_hclause;
  if Array.length lits > BV.max_len && ctx.Ctx.complete
  then (Util.debug 0 "%% incompleteness: clause of %d lits -> $true" (Array.length lits);
        Ctx.lost_completeness ~ctx;
        Util.exit_prof prof_mk_hclause;
        true_clause ~ctx)
  else begin
  (* Set of variables. *)
  let all_vars = Lits.vars_lits lits in
  (*
  (* Renaming subst *)
  let subst, _ = List.fold_left
    (fun (subst, i) var ->
      (S.bind ~recursive:false subst (var, 0) (T.mk_var i var.sort, 0), i+1))
    (S.id_subst, 0) all_vars
  in
  (* Normalize literals *)
  let lits = Lits.apply_subst_lits ~recursive:false ~ord:ctx.ctx_ord subst (lits, 0) in
  let all_vars = vars_of_lits lits in
  *)
  (* create the structure *)
  let rec c = {
    hclits = lits;
    hcctx = ctx;
    hcflags = BV.empty;
    hctag = (-1);
    hcweight = 0;
    hcselected = 0;
    hcvars = all_vars;
    hcproof = Obj.magic 0;
    hcparents = [];
    hcdescendants = SmallSet.empty ~cmp:(fun i j -> i-j);
  } in
  let old_hc, c = c, CHashcons.hashcons c in
  if c == old_hc then begin
    (* update proof *)
    let proof' = proof (compact c) in
    c.hcproof <- proof';
    (* select literals, if not already done *)
    (c.hcselected <- match selected with
      | Some bv -> bv
      | None -> BV.from_list (Ctx.select ~ctx lits));
        (* compute weight *)
        c.hcweight <- Lits.weight_lits lits;
        (* compute flags *)
        (if Lits.ground_lits lits then set_flag flag_ground c true);
        (* parents *)
        (match parents with
        | None -> ()
        | Some parents ->
          c.hcparents <- parents;
          List.iter (fun parent -> is_child_of ~child:c parent) parents);
  end;
  (* return clause *)
  Util.incr_stat stat_new_clause;
  Util.exit_prof prof_mk_hclause;
  c
  end

(** Build clause from a list (delegating to create_a) *)
let create ?parents ?selected ~ctx lits proof =
  create_a ?parents ?selected ~ctx (Array.of_list lits) proof

let is_empty c = Array.length c.hclits = 0

(** Adapt a proof to a new clause *)
let adapt_proof proof c = match proof with
  | Proof.Axiom (_, f, a) -> Proof.mk_axiom c f a
  | Proof.Infer (_, r, l) -> Proof.mk_infer c r l

let stats () = CHashcons.stats ()

(** descendants of the clause *)
let descendants c = c.hcdescendants

(** simplify literals *)
let clause_of_fof c =
  let ctx = c.hcctx in
  let lits = Array.map (Lits.lit_of_fof ~ord:(Ctx.ord ~ctx)) c.hclits in
  if Lits.eq_lits lits c.hclits then c (* keep the same *)
  else begin
    let proof = adapt_proof c.hcproof in
    let new_hc = create_a ~parents:[c] ~ctx lits proof in
    new_hc.hcdescendants <- c.hcdescendants;
    new_hc
  end

(** Change the context of the clause *)
let update_ctx ~ctx c =
  let lits = Array.map (Lits.reord ~ord:(Ctx.ord ~ctx)) c.hclits in
  let proof = adapt_proof c.hcproof in
  let c' = create_a ~selected:c.hcselected ~ctx lits proof in
  c'

(** check the ordering relation of lits (always true after reord_clause ~ord) *)
let check_ord ~ord c =
  assert (
  Util.array_forall
    (function (Lits.Equation (l,r,sign,o)) as lit ->
      let ok = o = Ordering.compare ord l r in
      (if not ok then Util.printf "Ord problem: literal %a, ord %s is not %s\n"
                      Lits.pp lit (Comparison.to_string o)
                      (Comparison.to_string (Ordering.compare ord l r)));
      ok)
    c.hclits)

(** Apply substitution to the clause. Note that using the same renaming for all
    literals is important. *)
let rec apply_subst ?recursive ?renaming subst c scope =
  let ctx = c.hcctx in
  let ord = Ctx.ord ~ctx in
  let lits = Array.map
    (fun lit -> Lits.apply_subst ?recursive ?renaming ~ord subst lit scope)
    c.hclits in
  let descendants = c.hcdescendants in
  let proof = adapt_proof c.hcproof in
  let new_hc = create_a ~parents:[c] ~ctx lits proof in
  new_hc.hcdescendants <- descendants;
  new_hc

(** Bitvector that indicates which of the literals of [subst(clause)]
    are maximal under [ord] *)
let maxlits (c, scope) subst =
  let ord = Ctx.ord c.hcctx in
  let lits =
    Lits.apply_subst_lits ~recursive:true ~ord subst c.hclits scope
  in
  Lits.maxlits ~ord lits

(** Check whether the literal is maximal *)
let is_maxlit (c, scope) subst i =
  BV.get (maxlits (c, scope) subst) i

(** Bitvector that indicates which of the literals of [subst(clause)]
    are eligible for resolution. *)
let eligible_res (c, scope) subst =
  let ord = Ctx.ord c.hcctx in
  (* instantiate lits *)
  let lits = Lits.apply_subst_lits ~recursive:true ~ord subst c.hclits scope in
  let selected = c.hcselected in
  let n = Array.length lits in
  (* Literals that may be eligible: all of them if none is selected,
     selected ones otherwise. *)
  let check_sign = not (BV.is_empty selected) in
  let bv = ref (if BV.is_empty selected then BV.make n else selected) in
  (* Only keep literals that are maximal. If [check_sign] is true, comparisons
     are only done between same-sign literals. *)
  for i = 0 to n-1 do
    (* i-th lit is already known not to be max? *)
    if not (BV.get !bv i) then () else
    let lit = lits.(i) in
    for j = i+1 to n-1 do
      let lit' = lits.(j) in
      (* check if both lits are still potentially eligible, and have the same sign 
         if [check_sign] is true. *)
      if (check_sign && Lits.is_pos lit <> Lits.is_pos lit')
        || not (BV.get !bv j) then () else
      match Lits.compare_partial ~ord lits.(i) lits.(j) with
      | Incomparable | Eq -> ()     (* no further information about i-th and j-th *)
      | Gt -> bv := BV.clear !bv j  (* j-th cannot be max *)
      | Lt -> bv := BV.clear !bv i  (* i-th cannot be max *)
    done;
  done;
  !bv

(** Bitvector that indicates which of the literals of [subst(clause)]
    are eligible for paramodulation. *)
let eligible_param (c, scope) subst =
  let ord = Ctx.ord c.hcctx in
  if BV.is_empty c.hcselected then
    (* instantiate lits *)
    let lits = Lits.apply_subst_lits ~recursive:true ~ord subst c.hclits scope in
    (* only keep literals that are positive *)
    let bv = Lits.maxlits ~ord lits in
    BV.inter bv (Lits.pos_lits lits)
  else BV.empty  (* no eligible literal when some are selected *)

(** are there selected literals in the clause? *)
let has_selected_lits c = not (BV.is_empty c.hcselected)

(** Check whether the literal is selected *)
let is_selected c i =
  let bv = c.hcselected in
  BV.get bv i

(** Indexed list of selected literals *)
let selected_lits c = BV.select c.hcselected c.hclits

(** is the clause a unit clause? *)
let is_unit_clause c = match c.hclits with
  | [|_|] -> true
  | _ -> false

let is_oriented_rule c = match c.hclits with
  | [|Lits.Equation (_,_,true,Gt)|]
  | [|Lits.Equation (_,_,true,Lt)|] -> true (* oriented *)
  | _ -> false

let infer_type ctx clauses =
  Sequence.iter
    (fun c -> Lits.lits_infer_type ctx c.hclits)
    clauses

(** Compute signature of this set of clauses *)
let signature ?(signature=Signature.empty) clauses =
  let ctx = TypeInference.Ctx.of_signature signature in
  infer_type ctx clauses;
  TypeInference.Ctx.to_signature ctx

(** Conversion of a (boolean) term to a clause. *)
let rec from_term ~ctx (t, file, name) =
  let module S = Symbol in
  let ord = Ctx.ord ctx in
  let rec lits_from_term t = match t.T.term with
  | T.Node (n, [{T.term=T.Node (eq, [a;b])}])
  when S.eq n S.not_symbol && (S.eq eq S.eq_symbol || S.eq eq S.equiv_symbol) ->
    [Lits.mk_neq ~ord a b]
  | T.Node (eq, [a;b]) when (S.eq eq S.eq_symbol || S.eq eq S.equiv_symbol) ->
    [Lits.mk_eq ~ord a b]
  | T.Node (or_, l) when S.eq or_ S.or_symbol ->
    let l' = T.flatten_ac S.or_symbol l in
    (* flatten the or, and convert each element to a list of literals *)
    List.concat (List.map lits_from_term l')
  | T.Node (n, [f]) when S.eq n S.not_symbol ->
    [Lits.mk_neq ~ord f T.true_term]
  | T.Node _ ->
    [Lits.mk_eq ~ord t T.true_term]
  | T.Bind _ ->
    [Lits.mk_eq ~ord t T.true_term]
  | T.At _ ->
    failwith "curried clauses not handled"
  | T.Var _ | T.BoundVar _ ->
    failwith "variable should not occur at the formula level"
  in
  let proof c = Proof.mk_axiom c file name in
  let c = create ~ctx (lits_from_term t) proof in
  c

(** {2 Set of clauses} *)

(** Simple set *)
module ClauseSet = Set.Make(struct
  type t = clause
  let compare hc1 hc2 = hc1.hctag - hc2.hctag
end)

(** Set with access by ID, bookeeping of maximal var... *)
module CSet = struct
  module IntMap = Map.Make(struct
    type t = int
    let compare i j = i - j
  end)

  type t = clause IntMap.t
    (** Set of hashconsed clauses. Clauses are indexable by their ID. *)

  let empty = IntMap.empty

  let is_empty = IntMap.is_empty

  let size set = IntMap.fold (fun _ _ b -> b + 1) set 0

  let add set c = IntMap.add c.hctag c set

  let add_list set hcs =
    List.fold_left add set hcs

  let remove_id set i = IntMap.remove i set

  let remove set c = remove_id set c.hctag

  let remove_list set hcs =
    List.fold_left remove set hcs

  let get set i = IntMap.find i set

  let mem set c = IntMap.mem c.hctag set

  let mem_id set i = IntMap.mem i set

  let choose set =
    try Some (snd (IntMap.choose set))
    with Not_found -> None

  let iter set k = IntMap.iter (fun _ c -> k c) set

  let iteri set k = IntMap.iter k set

  let fold set acc f =
    let acc = ref acc in
    iteri set (fun i c -> acc := f !acc i c);
    !acc

  let to_list set =
    IntMap.fold (fun _ c acc -> c :: acc) set []

  let of_list l =
    add_list empty l

  let to_seq set =
    Sequence.from_iter
      (fun k -> iter set k)

  let of_seq set seq = Sequence.fold add set seq

  let infer_type ctx set = infer_type ctx (to_seq set)

  let remove_seq set seq = Sequence.fold remove set seq

  let remove_id_seq set seq = Sequence.fold remove_id set seq
end


(** {2 Positions in clauses} *)

type clause_pos = clause * Position.t * Term.t
let compare_clause_pos (c1, p1, t1) (c2, p2, t2) =
  let c = Pervasives.compare p1 p2 in
  if c <> 0 then c else
  let c = compare c1 c2 in
  if c <> 0 then c else
  (assert (T.eq t1 t2); 0)

(** {2 IO} *)

let pp_debug buf c =
  let pp_annot selected maxlits i =
    ""^(if BV.get selected i then "+" else "")
      ^(if BV.get maxlits i then "*" else "")
  in
  (* print literals with a '*' for maximal, and '+' for selected *)
  let selected = c.hcselected
  and max = maxlits (c, 0) S.empty in
  Buffer.add_char buf '[';
  Util.pp_arrayi ~sep:" | "
    (fun buf i lit ->
      let annot = pp_annot selected max i in
      Lits.pp buf lit;
      Buffer.add_string buf annot)
    buf c.hclits;
  Buffer.add_char buf ']';
  ()

let pp_tstp buf c =
  (* convert into a big term *)
  let t = Lits.term_of_lits c.hclits in
  (* quantify all free variables *)
  let t = T.close_forall t in
  T.pp buf t

let to_string = Util.on_buffer pp_debug

let fmt fmt c =
  Format.pp_print_string fmt (to_string c)

let pp_set_debug buf set =
  Sequence.iter
    (fun c -> pp_debug buf c; Buffer.add_char buf '\n')
    (CSet.to_seq set)

let pp_set_tstp buf set =
  Sequence.iter
    (fun c -> pp_tstp buf c; Buffer.add_char buf '\n')
    (CSet.to_seq set)

let bij ~ctx =
  Bij.(map
    ~inject:(fun c -> c.hclits)
    ~extract:(fun lits ->
      let proof c = Proof.mk_axiom c "bij" "bij" in
      create_a ~ctx lits proof)
    (Lits.bij_lits ~ord:(Ctx.ord ctx)))

let bij_set ~ctx =
  Bij.(map
    ~inject:(fun set ->
      let l = CSet.fold set [] (fun acc _ c -> c :: acc) in
      l)
    ~extract:(fun l -> CSet.add_list CSet.empty l)
    (list_ (bij ~ctx)))

