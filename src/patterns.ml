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

(** Representative patterns for terms and clauses, signature-independent *)

open Types
open Symbols

module T = Terms
module C = Clauses
module Utils = FoUtils

let prof_pclause_of_clause = Utils.mk_profiler "mk_pclause"

(* ----------------------------------------------------------------------
 * pattern symbol, variables, clauses
 * ---------------------------------------------------------------------- *)

type psymbol = int
type psort = int

(** Special symbols and their number *)
let special_symbols =
  [| true_symbol;   (* Constants *)
     false_symbol;
     eq_symbol;     (* Formula *)
     exists_symbol;
     forall_symbol;
     lambda_symbol;
     not_symbol;
     imply_symbol;
     and_symbol;
     or_symbol;
     db_symbol;     (* De Bruijn *)
     succ_db_symbol;
     bool_sort;     (* Sorts *)
     univ_sort;
     type_sort;
  |]

(* De Bruijn *)

(** Symbols are variable if above this threshold *)
let symbol_offset = Array.length special_symbols

(** A pattern term. Symbols, sorts and variables can all be bound. *)
type pterm =
  | PVar of int * psort
  | PNode of psymbol * psort * pterm list

let rec compare_pterm t1 t2 =
  match t1, t2 with
  | PVar _, PNode _ -> -1
  | PNode _, PVar _ -> 1
  | PVar (i1, s1), PVar (i2, s2) -> if i1 <> i2 then i1 - i2 else s1 - s2
  | PNode (f1, s1, l1), PNode (f2, s2, l2) ->
    if f1 <> f2 then f1 - f2
    else if s1 <> s2 then s1 - s2
    else lexico l1 l2
and lexico l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x1::l1', x2::l2' ->
    let cmp = compare_pterm x1 x2 in
    if cmp <> 0 then cmp else lexico l1' l2'

let eq_pterm t1 t2 = compare_pterm t1 t2 = 0

let hash_pterm t =
  let rec hash h t = match t with
  | PVar (i, sort) ->
    Hash.hash_int3 h i sort
  | PNode (f, sort, l) ->
    let h = Hash.hash_int3 h f sort in
    List.fold_left hash h l
  in hash 113 t

(** A pattern literal is a pair of pattern terms + the sign *)
type pliteral = {
  lterm: pterm;
  lweight: int;
  rterm : pterm;
  rweight: int;
  psign : bool;
}

let compare_pliteral lit1 lit2 =
  if lit1.psign <> lit2.psign then compare lit1.psign lit2.psign else
  let weight1 = lit1.lweight + lit1.rweight
  and weight2 = lit2.lweight + lit2.rweight in
  if weight1 <> weight2 then weight1 - weight2
  else let cmp_left = compare_pterm lit1.lterm lit2.lterm in
  if cmp_left <> 0 then cmp_left
  else compare_pterm lit1.rterm lit2.rterm

let eq_plit lit1 lit2 = compare_pliteral lit1 lit2 = 0

let hash_plit plit =
  let lh = hash_pterm plit.lterm
  and rh = hash_pterm plit.rterm in
  Hashcons.combine3 3167 lh rh (if plit.psign then 2 else 3)

(** A pattern clause is just a list of pliterals. We also keep the canonical
    pattern (starting from 0) of each literal.
    
    Exemple: the clause  p(X) | q(Y,x)  may have
    pc_lits = [f0(X0); f1(X1, X0)]
    pc_canonical = [f0(X0); f0(X0, X1)]
    *)
type pclause = {
  pc_lits : pliteral list;        (* literals that have consistent naming *)
  pc_canonical : pliteral list;   (* canonical pattern of each literal *)
  pc_vars : psymbol list;         (* list of free symbols of the pclause *)
}

let compare_pclause c1 c2 =
  let rec lexico l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | lit1::l1', lit2::l2' ->
    let cmp = compare_pliteral lit1 lit2 in
    if cmp <> 0 then cmp else lexico l1' l2'
  in lexico c1.pc_lits c2.pc_lits

let hash_pclause_seed = 2381 

(* subtlety: the hash must not depend of the order of the literals. Therefore,
   we compute a commutative hash of the canonical literals. *)
let hash_pclause pc =
  List.fold_left (fun h lit -> h lxor hash_plit lit) hash_pclause_seed pc.pc_canonical

(** List of non-special symbols/sort (index) that occur in the clause *)
let pclause_symbols lits =
  let rec pterm_symbols acc t = match t with
  | PVar (_, sort) -> if List.mem sort acc || sort < symbol_offset then acc else sort::acc
  | PNode (f, sort, l) ->
    let acc = if List.mem f acc || f < symbol_offset then acc else f :: acc in
    let acc = if List.mem sort acc || sort < symbol_offset then acc else sort :: acc in
    List.fold_left pterm_symbols acc l
  and plit_symbols acc lit =
    pterm_symbols (pterm_symbols acc lit.lterm) lit.rterm
  in
  List.rev (List.fold_left plit_symbols [] lits)

(* ----------------------------------------------------------------------
 * mapping between regular terms/clauses and pattern terms/clauses
 * ---------------------------------------------------------------------- *)

(** An abstract substitution maps abstract symbols to symbols, variables and sorts *)
type mapping = {
  m_var : int Ptmap.t;
  m_symbol : symbol Ptmap.t;
}

let empty_mapping =
  let map = ref {
    m_var = Ptmap.empty;
    m_symbol = Ptmap.empty;
  } in
  (* default associations *)
  Array.iteri
    (fun i symb -> map := { !map with m_symbol = Ptmap.add i symb !map.m_symbol; })
    special_symbols;
  !map

let bind_symbol mapping ps s =
  { mapping with m_symbol = Ptmap.add ps s mapping.m_symbol; }

(** Checks whether the mapping binds all symbols in the list *)
let binds_all mapping symbols =
  List.for_all (fun s -> Ptmap.mem s mapping.m_symbol) symbols

(** Reverse mapping, from concrete vars/symbols/sorts to abstract ones. *)
type rev_mapping = {
  mutable rm_var : int Ptmap.t;
  rm_symbol : psymbol SHashtbl.t;
  mutable rm_varnum : int;
  mutable rm_symbolnum : int;
}

let empty_rev_mapping () =
  let rev_map = {
    rm_var = Ptmap.empty;
    rm_symbol = SHashtbl.create 7;
    rm_varnum = 0;
    rm_symbolnum = symbol_offset;
  } in
  Array.iteri (fun i symb -> SHashtbl.add rev_map.rm_symbol symb i) special_symbols;
  rev_map

(* ----------------------------------------------------------------------
 * conversion to patterns, instantiation of patterns, match pattern
 * ---------------------------------------------------------------------- *)

(*s canonical patterns for terms, literals and clauses.
    The clause pattern is not necessarily unique. *)

(** Get a unique number for this symbol, using the rev_map *)
let rm_get_symbol ~rev_map symbol =
  try SHashtbl.find rev_map.rm_symbol symbol
  with Not_found ->
    (* the symbol is new *)
    let n = rev_map.rm_symbolnum in
    SHashtbl.replace rev_map.rm_symbol symbol n;
    rev_map.rm_symbolnum <- n + 1;
    n

(** Get a unique number for this var, using the rev_map *)
let rm_get_var ~rev_map i =
  try Ptmap.find i rev_map.rm_var
  with Not_found ->
    (* the variable is new *)
    let n = rev_map.rm_varnum in
    rev_map.rm_var <- Ptmap.add i n rev_map.rm_var;
    rev_map.rm_varnum <- n + 1;
    n

let pterm_of_term ?rev_map t =
  let rev_map = match rev_map with | None -> empty_rev_mapping () | Some m -> m in
  (* recursive conversion *)
  let rec convert t = match t.term with
  | Var i ->
    let psort = rm_get_symbol ~rev_map t.sort in
    let i' = rm_get_var ~rev_map i in
    PVar (i', psort)
  | Node (f, l) ->
    let psort = rm_get_symbol ~rev_map t.sort in
    let psymbol = rm_get_symbol ~rev_map f in
    let l' = List.map convert l in
    PNode (psymbol, psort, l')
  in
  convert t

let plit_of_lit ?rev_map lit =
  let rev_map = match rev_map with | None -> empty_rev_mapping () | Some m -> m in
  (* order the terms by decreasing weight, or by lexicographic order of their
     canonical form *)
  let l, r, psign = match lit with
  | Equation (l, r, sign, _) -> begin
    let pl, pr = pterm_of_term l, pterm_of_term r in
    if l.tsize > r.tsize then l, r, sign
    else if l.tsize < r.tsize then r, l, sign
    else match compare_pterm pl pr with
     | n when n < 0 -> r, l, sign
     | n when n >= 0 -> l, r, sign
     | _ -> assert false
     end
  in
  (* convert first l, then r *)
  let lterm = pterm_of_term ~rev_map l in
  let rterm = pterm_of_term ~rev_map r in
  { lterm; rterm; lweight = l.tsize; rweight = r.tsize; psign; }

let pclause_of_clause ?rev_map hc =
  Utils.enter_prof prof_pclause_of_clause;
  let rev_map = match rev_map with | None -> empty_rev_mapping () | Some m -> m in
  let lits = Array.map (fun lit -> plit_of_lit lit, C.weight_literal lit, lit) hc.hclits in
  let lits = Array.to_list lits in
  (* sort the literals *)
  let lits = List.sort (fun (plit1, w1, lit1) (plit2, w2, lit2) ->
    if w1 <> w2 then w1 - w2 else compare_pliteral plit1 plit2)
    lits in
  let plits = List.map (fun (plit, _, _) -> plit) lits in
  let lits = List.map (fun (_, _, lit) -> lit) lits in
  (* convert the literals to pliterals using the rev_map *)
  let lits = List.map (plit_of_lit ~rev_map) lits in
  (* free symbols *)
  let vars = pclause_symbols lits in
  Utils.exit_prof prof_pclause_of_clause;
  { pc_lits=lits; pc_canonical=plits; pc_vars=vars; }

(*s instantiate an abstract pattern *)

let rec instantiate_pterm ~map pterm =
  (* lookup the concrete symbol for this pattern symbol *)
  let get_symbol ~map f =
    try Ptmap.find f map.m_symbol
    with Not_found ->
      (Format.eprintf "could not find f%d@." f; assert false)
  in
  match pterm with
  | PVar (i, s) ->
    (* we may keep the variable unbounded, in which case it is preserved *)
    let i' = try Ptmap.find i map.m_var with Not_found -> i in
    let s' = get_symbol ~map s in
    T.mk_var i' s'
  | PNode (f, s, l) ->
    let f' = get_symbol ~map f in
    let s' = get_symbol ~map s in
    let l' = List.map (instantiate_pterm ~map) l in
    T.mk_node f' s' l'

let instantiate_plit ~map ~ord plit =
  let l = instantiate_pterm ~map plit.lterm
  and r = instantiate_pterm ~map plit.rterm in
  C.mk_lit ~ord l r plit.psign

let instantiate_pclause ~map ~ord pclause proof parents =
  let lits = List.map (instantiate_plit ~map ~ord) pclause.pc_lits in
  C.mk_hclause ~ord lits proof parents

(*s match an abstract pattern against a term of a clause. Failure is
    indicated by an empty list, but several mappings can exist for
    literals and clauses. *)

(** Bind [s] to [symbol], returning the new map, or check that the binding
    of [s] is already [symbol] (otherwise raise Exit). *)
let check_symbol ~map s symbol =
  try
    let symbol' = Ptmap.find s map.m_symbol in
    if symbol == symbol' then map else raise Exit
  with Not_found ->
    { map with m_symbol = Ptmap.add s symbol map.m_symbol; }

(** Matching of a pattern-term and a term. This maps pattern symbols
    and variables on symbols and variables *)
let rec match_pterm ~map pt t =
  match pt, t.term with
  | PVar (i, s), Var i' ->
    let map = check_symbol ~map s t.sort in
    (try
      let j = Ptmap.find i map.m_var in
      if j = i' then map else raise Exit
    with Not_found ->
      { map with m_var = Ptmap.add i i' map.m_var; })
  | PNode (f, s, l), Node (f', l') ->
    (if List.length l <> List.length l' then raise Exit);
    let map = check_symbol ~map f f' in
    let map = check_symbol ~map s t.sort in
    List.fold_left2 (fun map pt t -> match_pterm ~map pt t) map l l'
  | PVar _, Node _ | PNode _, Var _ -> raise Exit

let match_plit ~map plit lit =
  match plit, lit with
  | { lterm; lweight; rterm; rweight; psign; }, Equation (l, r, sign, _)
    when psign = sign && rweight + lweight = l.tsize + r.tsize ->
    (* try both matching, l=l&r=r or l=r&r=l, since both the weight
       and the sign are correct *)
    (if lweight = l.tsize && rweight = r.tsize
      then try let map = match_pterm ~map lterm l in
        [match_pterm ~map rterm r] with Exit -> []
      else []) @
    (if lweight = r.tsize && rweight = l.tsize
      then try let map = match_pterm ~map rterm l in
        [match_pterm ~map lterm r] with Exit -> []
      else [])
  | _ -> []

let match_pclause ?map pclause hc =
  let map = match map with | None -> empty_mapping | Some m -> m in
  (* do all permutations of literals to match together *)
  let open Bitvector in
  (* match plits[i...] with literals in lits that are not flag'd in [bv] *)
  let rec all_matches acc plits lits map i bv =
    if i < Array.length plits
      then for j = 0 to Array.length lits - 1 do
        if not (bv_get bv j) then
          let bv = bv_set bv j in
          (* try to match plits[i] with lits[j] *)
          let maps = match_plit ~map plits.(i) lits.(j) in
          List.iter (fun map -> all_matches acc plits lits map (i+1) bv) maps
      done
    else acc := map :: !acc
  in
  if List.length pclause.pc_lits <> Array.length hc.hclits
    then []  (* no matching if lengths are different *)
    else begin
      let plits = Array.of_list pclause.pc_lits in
      let bv = 0 in
      let acc = ref [] in
      (* find all matchings *)
      all_matches acc plits hc.hclits map 0 bv;
      !acc
    end

(* ----------------------------------------------------------------------
 * pretty printing
 * ---------------------------------------------------------------------- *)

let pp_symb formatter s = if s < Array.length special_symbols
  then !T.pp_symbol#pp formatter special_symbols.(s)
  else Format.fprintf formatter "f%d" (s - symbol_offset)

let rec pp_pterm formatter t =
  let pp_sort formatter s =
    if s >= symbol_offset then Format.fprintf formatter ":%a" pp_symb s else ()
  in
  match t with
  | PVar (i, s) -> Format.fprintf formatter "X%d%a" i pp_sort s
  | PNode (f, s, []) -> Format.fprintf formatter "%a%a" pp_symb f pp_sort s
  | PNode (f, s, l) ->
    Format.fprintf formatter "%a%a(%a)" pp_symb f pp_sort s
      (Utils.pp_list ~sep:", " pp_pterm) l

let pp_pclause formatter pclause =
  let pp_plit formatter lit =
    match lit.psign, lit.rterm with
    | true, PNode (0, _, []) -> (* = true *)
      Format.fprintf formatter "%a" pp_pterm lit.lterm
    | false, PNode (0, _, []) -> (* != true *)
      Format.fprintf formatter "~%a" pp_pterm lit.lterm
    | true, _ ->
      Format.fprintf formatter "%a = %a" pp_pterm lit.lterm pp_pterm lit.rterm
    | false, _ ->
      Format.fprintf formatter "%a != %a" pp_pterm lit.lterm pp_pterm lit.rterm
  in
  Utils.pp_list ~sep:" | " pp_plit formatter pclause.pc_lits

let pp_mapping formatter mapping =
  (* only print symbol binding *)
  Ptmap.iter
    (fun s symbol -> if s >= symbol_offset
      then Format.fprintf formatter "%a -> %s@;" pp_symb s (name_symbol symbol))
    mapping.m_symbol

(* ----------------------------------------------------------------------
 * map from patterns to data, with matching of clauses
 * ---------------------------------------------------------------------- *)

(* Compute a hash for a hclause. We guarantee that a hclause has the same hash
   as any of its pattern clauses. *)
let hash_hclause hc =
  Array.fold_left
    (fun h lit -> let plit = plit_of_lit lit in h lxor hash_plit plit)
    hash_pclause_seed hc.hclits

(** Match hclauses with sets of pattern clauses, with some associated values *)
module Map =
  struct
    module PMap = Map.Make(struct type t = pclause let compare = compare_pclause end)

    type 'a t = 'a PMap.t Ptmap.t ref
      (** the mapping. It first maps by (commutative) hash on pliterals,
          then it maps pclauses to values. *)

    let create () = ref Ptmap.empty

    (** add a mapping pattern clause -> value *)
    let add t pc value =
      let h = hash_pclause pc in
      (* map pclause -> value, for all pclauses that have the same hash *)
      let map =
        try Ptmap.find h !t
        with Not_found -> PMap.empty
      in
      let map = PMap.add pc value map in
      t := Ptmap.add h map !t

    (** fold on all stored key->value *)
    let fold t acc k =
      Ptmap.fold
        (fun _ map acc ->
          PMap.fold
            (fun pc value acc -> k acc pc value)
            map acc)
        !t acc

    (** match the hclause with pattern clauses. The callback, fold-like, is called
        on every match with both the pattern and the mapping. *)
    let retrieve t hc acc k =
      let h = hash_hclause hc in
      try
        let map = Ptmap.find h !t in
        (* fold on pclauses that have same hash *)
        PMap.fold
          (fun pc value acc ->
            (* match the pclause with the given hclause *)
            Utils.debug 3 (lazy (Utils.sprintf "%% @[<h>match %a with %a@]"
                          !C.pp_clause#pp_h hc pp_pclause pc));
            let mappings = match_pclause pc hc in
            List.fold_left
              (fun acc mapping -> k acc pc mapping value)
              acc mappings)
          map acc
      with Not_found ->
        acc  (* no pclause with such a hash *)

    (** Pretty print the map *)
    let pp pp_value formatter t =
      Format.fprintf formatter "@[<h>pattern clause mapping:@; @[<hov>";
      Ptmap.iter
        (fun _ map -> PMap.iter
          (fun pc value -> (* print this key->value *)
            Format.fprintf formatter "%a -> %a@;" pp_pclause pc pp_value value)
          map)
        !t;
      Format.fprintf formatter "@]@]"
  end

