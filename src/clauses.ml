(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(* literals and clauses *)

open Types
open Symbols

module T = Terms
module Lits = Literals
module S = FoSubst
module Utils = FoUtils

let stat_fresh = mk_stat "fresh_clause"
let stat_mk_hclause = mk_stat "mk_hclause"
let stat_new_clause = mk_stat "new_clause"
let prof_check_max_lit = Utils.mk_profiler "check_max_lit"
let prof_mk_hclause = Utils.mk_profiler "mk_hclause"
let prof_mk_hclause_raw = Utils.mk_profiler "mk_hclause_raw"

(* ----------------------------------------------------------------------
 * boolean flags
 * ---------------------------------------------------------------------- *)

let flag_ground = 1 lsl 0
let flag_redundant = 1 lsl 1
let flag_transient = 1 lsl 2

let set_flag flag c truth =
  if truth
    then c.hcflags <- c.hcflags lor flag
    else c.hcflags <- c.hcflags land (lnot flag)

let get_flag flag c = (c.hcflags land flag) != 0

(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)

let eq_hclause hc1 hc2 = hc1.hctag = hc2.hctag

let compare_hclause hc1 hc2 = hc1.hctag - hc2.hctag

let hash_hclause hc = Lits.hash_lits hc.hclits

module CHashtbl = Hashtbl.Make(
  struct
    type t = hclause
    let hash hc = hash_hclause hc
    let equal c1 c2 = eq_hclause c1 c2
  end)

module CHashSet =
  struct
    type t = unit CHashtbl.t
    let create () = CHashtbl.create 13
    let is_empty t = CHashtbl.length t = 0
    let member t hc = CHashtbl.mem t hc
    let iter t f = CHashtbl.iter (fun hc _ -> f hc) t
    let add t hc = CHashtbl.replace t hc ()
    let to_list t =
      let l = ref [] in
      iter t (fun hc -> l := hc :: !l);
      !l
  end

(* ----------------------------------------------------------------------
 * useful functions to build and examine clauses
 * ---------------------------------------------------------------------- *)

module BV = Bitvector

(** comparison of variables by index *)
let compare_vars a b =
  match a.term, b.term with
  | Var i, Var j -> i - j
  | _ -> assert false

(** check whether variables are from 0 to n *)
let check_normal vars =
  let rec check prev = function
  | [] -> true
  | {term=Var n}::l -> n = prev+1 && check n l
  | _ -> assert false
  in check (-1) vars

(** Compute the set of variables for this array of literals *)
let vars_of_lits lits =
  let set = T.THashSet.create () in
  for i = 0 to Array.length lits - 1 do
    Lits.add_vars set lits.(i);
  done;
  T.THashSet.to_list set

(** Get a fresh ID for a clause *)
let get_next_tag =
  let current_tag = ref 1 in
  fun () ->
    let n = !current_tag in
    incr current_tag;
    n

(** the tautological empty clause *)
let true_clause ~ctx =
  let hcflags = flag_ground lor flag_redundant in
  let hc = { hclits = [| Equation (T.true_term, T.true_term, true, Eq) |];
      hctag = get_next_tag (); hcweight=2; hcselected=0; hcflags; hcctx=ctx;
      hcvars=[]; hcproof=Proof ("trivial", []); hcdescendants=[||]; }
  in
  hc

(** [is_child_of ~child c] is to be called to remember that [child] is a child
    of [c], is has been infered/simplified from [c] *)
let is_child_of ~child c =
  (* update the parent clauses' sets of descendants by adding [child] *)
  let descendants = Array.of_list (child.hctag :: Array.to_list c.hcdescendants) in
  c.hcdescendants <- descendants

(** Build a new hclause from the given literals. If there are more than 31 literals,
    the prover becomes incomplete by returning [true] instead. *)
let mk_hclause_a ?parents ?selected ~ctx lits proof =
  incr_stat stat_mk_hclause;
  Utils.enter_prof prof_mk_hclause;
  if Array.length lits > 31
  then (Utils.debug 0 (lazy (Utils.sprintf
            "%% incompleteness: clause of %d lits -> $true" (Array.length lits)));
        Utils.exit_prof prof_mk_hclause;
        true_clause ~ctx)
  else begin
  (* set of variables *)
  let all_vars = vars_of_lits lits in
  (* create the structure *)
  let hc = {
    hclits = lits;
    hcctx = ctx;
    hcflags = BV.empty;
    hctag = get_next_tag ();
    hcweight = Array.fold_left (fun acc lit -> acc + Lits.weight lit) 0 lits;
    hcselected = 0;
    hcvars = all_vars;
    hcproof = proof;
    hcdescendants = [||];
  } in
  (* select literals, if not already done *)
  (hc.hcselected <- match selected with
    | Some bv -> bv
    | None -> BV.from_list (ctx.ctx_select hc));
  (* compute flags *)
  (if Lits.ground_lits lits then set_flag flag_redundant hc true);
  (* parents *)
  (match parents with
  | None -> ()
  | Some parents ->
    List.iter (fun parent -> is_child_of ~child:hc parent) parents);
  (* return clause *)
  incr_stat stat_new_clause;
  Utils.exit_prof prof_mk_hclause;
  hc
  end

(** Build clause from a list (delegating to mk_hclause_a) *)
let mk_hclause ?parents ?selected ~ctx lits proof =
  mk_hclause_a ?parents ?selected ~ctx (Array.of_list lits) proof

(** descendants of the clause *)
let descendants hc = hc.hcdescendants

(** simplify literals *)
let clause_of_fof hc =
  let ctx = hc.hcctx in
  let lits = Array.map (Lits.lit_of_fof ~ord:ctx.ctx_ord) hc.hclits in
  mk_hclause_a ~ctx lits hc.hcproof

(** Change the context of the clause *)
let update_ctx ~ctx hc =
  let lits = Array.map (Lits.reord ~ord:ctx.ctx_ord) hc.hclits in
  mk_hclause_a ~selected:hc.hcselected ~ctx lits hc.hcproof

(** check the ordering relation of lits (always true after reord_clause ~ord) *)
let check_ord_hclause ~ord hc =
  assert (
  Utils.array_forall
    (function (Equation (l,r,_,o)) ->
      let ok = o = ord#compare l r in
      (if not ok then Format.printf "@[<h>Ord problem: literal %a %a"
                      !T.pp_term#pp l !T.pp_term#pp r);
      ok)
    hc.hclits)

(** Apply substitution to the clause *)
let rec apply_subst ?(recursive=true) subst hc =
  let ctx = hc.hcctx in
  if S.is_empty subst then hc
  else
    let ord = ctx.ctx_ord in
    let lits = Array.map (Lits.apply_subst ~recursive ~ord subst) hc.hclits in
    mk_hclause_a ~ctx lits hc.hcproof

(** bitvector of literals that are positive *)
let pos_lits lits =
  let bv = ref BV.empty in
  for i = 0 to Array.length lits - 1 do
    if Lits.is_pos lits.(i) then bv := BV.set !bv i
  done;
  !bv

(** bitvector of literals that are positive *)
let neg_lits lits =
  let bv = ref BV.empty in
  for i = 0 to Array.length lits - 1 do
    if Lits.is_neg lits.(i) then bv := BV.set !bv i
  done;
  !bv

(** Bitvector that indicates which of the literals are maximal *)
let maxlits_array ~ord lits =
  let n = Array.length lits in
  (* at the beginning, all literals are potentially maximal *)
  let bv = ref (BV.make n) in
  for i = 0 to n-1 do
    (* i-th lit is already known not to be max? *)
    if not (BV.get !bv i) then () else
    for j = i+1 to n-1 do
      if not (BV.get !bv j) then () else
      match Lits.compare_partial ~ord lits.(i) lits.(j) with
      | Incomparable | Eq -> ()     (* no further information about i-th and j-th *)
      | Gt -> bv := BV.clear !bv j  (* j-th cannot be max *)
      | Lt -> bv := BV.clear !bv i  (* i-th cannot be max *)
    done;
  done;
  (* return bitvector *)
  !bv

(** Bitvector that indicates which of the literals of [subst(clause)]
    are maximal under [ord] *)
let maxlits c subst =
  let ord = c.hcctx.ctx_ord in
  let lits = Array.map (Lits.apply_subst ~ord subst) c.hclits in
  maxlits_array ~ord lits

(** Check whether the literal is maximal *)
let is_maxlit c subst i = BV.get (maxlits c subst) i

(** Bitvector that indicates which of the literals of [subst(clause)]
    are eligible for resolution. *)
let eligible_res c subst =
  let ord = c.hcctx.ctx_ord in
  (* instantiate lits *)
  let lits = Array.map (Lits.apply_subst ~ord subst) c.hclits in
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
let eligible_param c subst =
  let ord = c.hcctx.ctx_ord in
  if BV.is_empty c.hcselected then
    (* instantiate lits *)
    let lits = Array.map (Lits.apply_subst ~ord subst) c.hclits in
    (* only keep literals that are positive *)
    let bv = maxlits_array ~ord lits in
    BV.inter bv (pos_lits lits)
  else BV.empty  (* no eligible literal when some are selected *)

(** Get a variant of the clause, in which variables are all > offset *)
let fresh_clause offset hc =
  let ctx = hc.hcctx in
  let ord = ctx.ctx_ord in
  incr_stat stat_fresh;
  let subst = S.relocate (offset + 1) hc.hcvars in
  let lits = Array.map (Lits.apply_subst ~recursive:false ~ord subst) hc.hclits in
  mk_hclause_a ~selected:hc.hcselected ~ctx lits hc.hcproof

(** Normalize clause by renaming variables from 0 *)
let normalize hc =
  let ctx = hc.hcctx in
  let ord = ctx.ctx_ord in
  let subst = S.relocate 0 hc.hcvars in
  let lits = Array.map (Lits.apply_subst ~recursive:false ~ord subst) hc.hclits in
  mk_hclause_a ~ctx lits hc.hcproof

(** are there selected literals in the clause? *)
let has_selected_lits hc = not (BV.is_empty hc.hcselected)

(** Check whether the literal is selected *)
let is_selected hc i =
  let bv = hc.hcselected in
  BV.get bv i

(** Indexed list of selected literals *)
let selected_lits c = BV.select c.hcselected c.hclits

(** is the clause a unit clause? *)
let is_unit_clause hc = match hc.hclits with
  | [|_|] -> true
  | _ -> false

(** check whether the clause is already in CNF *)
let is_cnf hc =
  Utils.array_forall
    (fun (Equation (l, r, sign, _)) ->
      T.atomic_rec l && T.atomic_rec r && (l.sort != bool_sort ||
                                          (l == T.true_term || r == T.true_term)))
    hc.hclits

(** Compute signature of this set of clauses *)
let signature clauses =
  (* explore a term *)
  let rec explore_term signature t = match t.term with
  | Var _ | BoundVar _ -> signature
  | Bind (s, t') ->
    let arity, sort = 1, t.sort in
    let signature' = update_sig signature s arity sort in
    explore_term signature' t'
  | Node (f, l) ->
    let arity, sort = List.length l, t.sort in
    let signature' = update_sig signature f arity sort in
    List.fold_left explore_term signature' l
  and explore_lit signature lit = match lit with
  | Equation (l,r,_,_) -> explore_term (explore_term signature l) r
  and explore_clause signature hc = Array.fold_left explore_lit signature hc.hclits
  (* Update signature with s -> (arity, sort).
     Checks consistency with current value, if any. *)
  and update_sig signature f arity sort =
    (try
      let arity', sort' = SMap.find f signature in
      assert (arity = arity' && sort == sort');
    with Not_found -> ());
    let signature' = SMap.add f (arity, sort) signature in
    signature'
  in
  List.fold_left explore_clause empty_signature clauses

let rec from_simple ~ctx (f, source) =
  let ord = ctx.ctx_ord in
  let open Literals in
  let rec lits_from_simple ~ord f = match f with
    | Simple.Not (Simple.Atom f) -> [mk_neq ~ord (T.from_simple f) T.true_term]
    | Simple.Atom f -> [mk_eq ~ord (T.from_simple f) T.true_term]
    | Simple.Not (Simple.Eq (t1,t2)) -> [mk_neq ~ord (T.from_simple t1) (T.from_simple t2)]
    | Simple.Eq (t1, t2) -> [mk_eq ~ord (T.from_simple t1) (T.from_simple t2)]
    | Simple.Not (Simple.Equiv (f1,f2)) ->
      [mk_neq ~ord (T.from_simple_formula f1) (T.from_simple_formula f2)]
    | Simple.Equiv (f1, f2) ->
      [mk_eq ~ord (T.from_simple_formula f1) (T.from_simple_formula f2)]
    | Simple.Or l -> List.concat (List.map (lits_from_simple ~ord) l)
    | _ -> [mk_eq ~ord (T.from_simple_formula f) T.true_term]
  in
  let proof = match source with
  | Simple.Axiom (a,b) -> Axiom (a,b)
  | Simple.Derived (name, fs) ->
    failwith "unable to convert non-axiom simple clause to hclause"
  in
  mk_hclause ~ctx (lits_from_simple ~ord f) proof

(* ----------------------------------------------------------------------
 * set of clauses, reachable by ID
 * ---------------------------------------------------------------------- *)

(** Simple set *)
module ClauseSet = Set.Make(
  struct
    type t = hclause
    let compare hc1 hc2 = hc1.hctag - hc2.hctag
  end)

(** Set with access by ID, bookeeping of maximal var... *)
module CSet =
  struct

    (** Set of hashconsed clauses. 'a is the fantom type for hclauses.
        It also contains a payload that is updated on every addition/
        removal of clauses. The additional payload is also updated upon
        addition/deletion. *)
    type t = {
      maxvar : int;                 (** index of maximum variable *)
      clauses : hclause Ptmap.t;    (** clause ID -> clause *)
    }

    let empty = { maxvar=0; clauses = Ptmap.empty; }

    let is_empty set = Ptmap.is_empty set.clauses

    let size set = Ptmap.fold (fun _ _ b -> b + 1) set.clauses 0

    let add set hc =
      let maxvar = max (T.max_var hc.hcvars) set.maxvar in
      { maxvar; clauses = Ptmap.add hc.hctag hc set.clauses; }

    let add_list set hcs =
      let maxvar, clauses =
        List.fold_left
          (fun (m,c) hc -> max m (T.max_var hc.hcvars), Ptmap.add hc.hctag hc c)
          (set.maxvar, set.clauses) hcs in
      {maxvar; clauses;}

    let remove_id set i =
      { set with clauses = Ptmap.remove i set.clauses }

    let remove set hc = remove_id set hc.hctag

    let remove_list set hcs =
      let clauses =
        List.fold_left
          (fun c hc -> Ptmap.remove hc.hctag c)
          set.clauses hcs in
      {set with clauses;}

    let remove_ids set ids =
      let clauses =
        Ptset.fold
          (fun i set -> Ptmap.remove i set)
          ids set.clauses in
      {set with clauses;}

    let get set i = Ptmap.find i set.clauses

    let mem set hc = Ptmap.mem hc.hctag set.clauses

    let mem_id set i = Ptmap.mem i set.clauses

    let iter set k = Ptmap.iter (fun _ hc -> k hc) set.clauses

    let iteri set k = Ptmap.iter k set.clauses

    let fold f acc set =
      let acc = ref acc in
      iteri set (fun i hc -> acc := f !acc i hc);
      !acc

    let to_list set =
      Ptmap.fold (fun _ hc acc -> hc :: acc) set.clauses []

    let of_list l =
      add_list empty l
  end

(* ----------------------------------------------------------------------
 * recognize some shapes of clauses
 * ---------------------------------------------------------------------- *)

(** Does t contains the symbol f? *)
let rec contains_symbol f t =
  match t.term with
  | Var _ | BoundVar _ -> false
  | Bind (s, t') -> s == f || contains_symbol f t'
  | Node (g, ts) -> g == f || List.exists (contains_symbol f) ts

(** Recognized whether the clause is a Range-Restricted Horn clause *)
let is_RR_horn_clause hc = 
  let lit = ref None in
  (* find whether there is exactly one positive literal *)
  let rec find_uniq_pos n i =
    if i = Array.length hc.hclits
      then if n = 1 then !lit else None
      else begin
        match hc.hclits.(i) with
        | Equation (l,r,true,_) as lit' ->
          lit := Some lit';
          find_uniq_pos (n+1) (i+1)
        | _ -> find_uniq_pos n (i+1)
      end
  in
  match find_uniq_pos 0 0 with
  | None -> false
  | Some lit' -> (* check that all variables of the clause occur in the head *)
    List.length (Lits.vars lit') = List.length hc.hcvars

(** Recognizes Horn clauses (at most one positive literal) *)
let is_horn hc =
  (* Iterate on literals, counting the positive ones.
     [pos]: did we already meet a positive literal *)
  let rec iter_lits pos lits i =
    if i = Array.length lits then true else match lits.(i) with
      | Equation (_, _, true, _) -> if pos then false else iter_lits true lits (i+1)
      | Equation (_, _, false, _) -> iter_lits pos lits (i+1)
  in iter_lits false hc.hclits 0

(** Check whether the clause defines a symbol, e.g.
    subset(X,Y) = \forall Z(Z in X -> Z in Y). It means the LHS
    is a flat symbol with variables, and all variables in RHS
    are also in LHS *)
let is_definition hc =
  (* check that r is a definition of l=f(x1,...,xn) *)
  let check_def l r =
    match l.term with
    | Var _ | BoundVar _ | Bind _ -> false
    | Node (f, ts) ->
      (* l=f(x1,...,xn) where r contains no other var than x1,...,xn, and n > 0 *)
      T.atomic_rec l && ts <> [] && not (contains_symbol f r) && l != T.true_term && r != T.true_term
      && List.for_all T.is_var ts
      && List.for_all (fun x -> T.var_occurs x l) (T.vars r)
  in
  match hc.hclits with
  | [|Equation (({term=Node(_, _)} as l), r, true, _)|] when check_def l r -> Some (l, r)
  | [|Equation (l, ({term=Node(_, _)} as r), true, _)|] when check_def r l -> Some (r, l)
  | _ -> None

(** More general than definition. It means the clause is an
    equality where all variables in RHS are also in LHS. It
    can return two rewrite rules if the clause can be oriented
    in both ways, e.g. associativity axiom. *)
let is_rewrite_rule hc =
  (* check that l -> r is an acceptable rewrite rule *)
  let check_rule l r =
    match l.term with
    | Var _ | Bind _ | BoundVar _ -> false
    | Node (_, _) ->
      T.atomic_rec l && l != T.true_term && r != T.true_term &&
      List.for_all (fun x -> T.var_occurs x l) (T.vars r)
  in
  match hc.hclits with
  | [|Equation (l, r, true, _)|] ->
    (if check_rule l r then [l, r] else []) @ (if check_rule r l then [r, l] else [])
  | _ -> []

let is_pos_eq hc =
  match hc.hclits with
  | [|Equation (l,r,true,_)|] -> Some (l,r)
  | _ -> None

(** Checks whether the clause is "const = ground composite term", e.g.
    a clause "aIbUc = inter(a, union(b, c))". In this case it returns
    Some(constant, definition of constant) *)
let is_const_definition hc =
  match hc.hclits with
  | [|Equation (l,r,true,_)|] when T.is_const l && T.is_ground_term r
    && not (T.member_term l r) ->
    Some (l,r)
  | [|Equation (l,r,true,_)|] when T.is_const r && T.is_ground_term l
    && not (T.member_term r l) ->
    Some (r,l)
  | _ -> None

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

(** pretty printer for clauses *)
class type pprinter_clause =
  object
    method pp_lits : Format.formatter -> literal array -> hclause -> unit
    method pp : Format.formatter -> clause -> unit      (** print clause *)
    method pp_h : Format.formatter -> hclause -> unit   (** print hclause *)
    method pp_pos : Format.formatter -> (clause * position) -> unit
    method pp_h_pos : Format.formatter -> (hclause * position * term) -> unit
    method pp_pos_subst : Format.formatter -> (clause * position * substitution) -> unit
    method horizontal : bool -> unit                    (** print in horizontal box? *)
  end

(** factor some code for classes *)
class virtual common_pp_clause =
  object (self)
    method virtual pp_lits : Format.formatter -> literal array -> hclause -> unit
    method pp formatter c = self#pp_lits formatter c.hclits c
    method pp_h formatter hc = self#pp_lits formatter hc.hclits hc
    method pp_pos formatter (c, pos) =
      Format.fprintf formatter "@[<h>[%a at %a]@]" self#pp c pp_pos pos
    method pp_h_pos formatter (hc, pos, t) =
      Format.fprintf formatter "@[<h>[%a at %a with %a]@]"
        self#pp_h hc pp_pos pos !T.pp_term#pp t
    method pp_pos_subst formatter (c, pos, subst) =
      Format.fprintf formatter "@[<h>[%a at %a with %a]@]"
        self#pp c pp_pos pos S.pp_substitution subst
  end

let pp_clause_debug =
  let _horizontal = ref true in
  let pp_annot selected maxlits i =
    ""^(if BV.get selected i then "+" else "")
      ^(if BV.get maxlits i then "*" else "") in
  object (self)
    (* print literals with a '*' for maximal, and '+' for selected *)
    method pp_lits formatter lits hc =
      let selected = hc.hcselected
      and maxlits = maxlits hc S.id_subst in
      (* how to print the list of literals *)
      let lits_printer formatter lits =
        Utils.pp_arrayi ~sep:" | "
          (fun formatter i lit ->
            let annot = pp_annot selected maxlits i in
            Format.fprintf formatter "%a%s" Lits.pp_literal_debug#pp lit annot)
          formatter lits
      in
      (* print in an horizontal box, or not *)
      if !_horizontal
        then Format.fprintf formatter "@[<h>[%a]@]" lits_printer lits
        else Format.fprintf formatter "[%a]" lits_printer lits
    (* regular printing is printing with no literal selected *)
    inherit common_pp_clause
    method horizontal s = _horizontal := s
  end

let pp_clause_tstp =
  let _horizontal = ref true in
  object (self)
    method pp_lits formatter lits hc =
      (* how to print the list of literals *)
      let lits_printer formatter lits =
        (* convert into a big term *)
        let t =
          match lits with
          | [||] -> T.false_term
          | _ -> Array.fold_left
            (fun t lit -> T.mk_or t (Lits.term_of_lit lit))
            (Lits.term_of_lit lits.(0)) (Array.sub lits 1 (Array.length lits - 1))
        in
        (* quantify all free variables *)
        let vars = T.vars t in
        let t = List.fold_left
          (fun t var -> T.mk_node forall_symbol bool_sort [var; t])
          t vars
        in
        T.pp_term_tstp#pp formatter t
      in
      (* print in an horizontal box, or not *)
      if !_horizontal
        then Format.fprintf formatter "@[<h>%a@]" lits_printer lits
        else Format.fprintf formatter "%a" lits_printer lits
    inherit common_pp_clause
    method horizontal s = _horizontal := s
  end

let pp_clause = ref pp_clause_debug

(** pretty printer for proofs *)
class type pprinter_proof =
  object
    method pp : Format.formatter -> hclause -> unit      (** pretty print proof from clause *)
  end

let pp_proof_debug =
  object (self)
    method pp formatter hc =
      assert (hc.hclits = [||]);
      (* already_printed is a set of clauses already printed. *)
      let already_printed = ref Ptset.empty
      and to_print = Queue.create () in
      (* initialize queue *)
      Queue.add hc to_print; 
      (* print every clause in the queue, if not already printed *)
      while not (Queue.is_empty to_print) do
        let hc = Queue.take to_print in
        if Ptset.mem hc.hctag !already_printed then ()
        else begin
          already_printed := Ptset.add hc.hctag !already_printed;
          match hc.hcproof with
          | Axiom (f, s) ->
            Format.fprintf formatter "@[<hov 4>@[<h>%a@]@ <--- @[<h>axiom %s in %s@]@]@;"
              !pp_clause#pp_h hc s f
          | Proof (rule, premises) ->
            (* print the proof step *)
            Format.fprintf formatter "@[<hov 4>@[<h>%a@]@ <--- @[<h>%s with @[<hv>%a@]@]@]@;"
              !pp_clause#pp_h hc rule
              (Utils.pp_list ~sep:", " !pp_clause#pp_pos_subst) premises;
            (* print premises recursively *)
            List.iter (fun (c, _, _) -> Queue.add c to_print) premises
        end
      done
  end

let pp_proof_tstp =
  object (self)
    method pp formatter hc =
      assert (hc.hclits = [||]);
      (* already_printed is a set of clauses already printed. *)
      let already_printed = ref Ptset.empty
      and clause_num = ref Ptmap.empty
      and counter = ref 1
      and to_print = Queue.create () in
      (* c -> hashconsed c, unique number for c *)
      let get_num hc = 
        try hc, Ptmap.find hc.hctag !clause_num
        with Not_found ->
          clause_num := Ptmap.add hc.hctag !counter !clause_num;
          incr counter;
          hc, Ptmap.find hc.hctag !clause_num
      in
      (* initialize queue *)
      let hc, num = get_num hc in
      Queue.add (hc, num) to_print; 
      (* print every clause in the queue, if not already printed *)
      while not (Queue.is_empty to_print) do
        let hc, num = Queue.take to_print in
        if Ptset.mem hc.hctag !already_printed then ()
        else begin
          already_printed := Ptset.add hc.hctag !already_printed;
          match hc.hcproof with
          | Axiom (f, ax_name) ->
            Format.fprintf formatter "@[<h>fof(%d, axiom, %a,@ @[<h>file('%s', %s)@]).@]@;"
              num pp_clause_tstp#pp_h hc f ax_name
          | Proof (name, premises) ->
            let premises = List.map (fun (c,_,_) -> get_num c) premises in
            let status = if name = "elim" || name = "to_cnf" then "esa" else "thm" in
            (* print the inference *)
            Format.fprintf formatter ("@[<h>fof(%d, plain, %a,@ " ^^
              "@[<h>inference('%s', [status(%s)], @[<h>[%a, theory(equality)]@])@]).@]@;")
              num pp_clause_tstp#pp_h hc name status
              (Utils.pp_list ~sep:"," Format.pp_print_int) (List.map snd premises);
            (* print every premise *)
            List.iter (fun (hc,num) -> Queue.add (hc, num) to_print) premises
        end
      done;
  end

let pp_proof = ref pp_proof_debug

(** print the content of a clause set *)
let pp_set formatter set =
  let clauses = CSet.to_list set in
  (* print as a list of clauses *)
  Format.fprintf formatter "@[<v>%a@]" (Utils.pp_list ~sep:"" !pp_clause#pp_h) clauses
