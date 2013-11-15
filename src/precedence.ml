(*
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

(** {1 Precedence (total ordering) on symbols} *)

module T = FOTerm
module F = FOFormula
module PO = PartialOrder
module STbl = Symbol.Tbl
module SMap = Symbol.Map

type constr = Symbol.t -> Symbol.t -> int
  (** an ordering constraint (a possibly non-total ordering on symbols) *)

type  clause = FOFormula.t list
  (** Abstraction of a clause. It's only a list of terms. *)

type t = {
  prec_snapshot : Symbol.t list;  (** symbols in decreasing order *)
  prec_compare : Symbol.t -> Symbol.t -> int;       (** Compare symbols *)
  prec_weight : Symbol.t -> int;
  prec_set_weight : (Symbol.t -> int) -> t;
  prec_add_symbols : Symbol.t list -> t * int;
    (** add the given symbols to the precedenc (returns how many are new) *)
} (** A total ordering on symbols *)

let eq p1 p2 =
  List.length p1.prec_snapshot = List.length p2.prec_snapshot
  && List.for_all2 (==) p1.prec_snapshot p2.prec_snapshot

let snapshot p = p.prec_snapshot

let compare p s1 s2 = p.prec_compare s1 s2

let add_symbols p l =
  let p', _ = p.prec_add_symbols l in
  p'

let add_signature p signature =
  let symbols = Signature.to_symbols signature in
  add_symbols p symbols

let pp_snapshot buf s =
  Util.pp_list ~sep:" > " Symbol.pp buf s

let pp buf prec = pp_snapshot buf prec.prec_snapshot

let to_string p =
  let b = Buffer.create 32 in
  pp b p;
  Buffer.contents b

let fmt fmt p = Format.pp_print_string fmt (to_string p)

(* ----------------------------------------------------------------------
 * hard constraints on the ordering
 * ---------------------------------------------------------------------- *)

let cluster_constraint clusters =
  let table = STbl.create 5
  and cluster_num = ref 0 in
  (* for each cluster, assign it a (incremented) number, and
     remember symbol->number for every symbol of the cluster *)
  List.iter
    (fun cluster ->
      let num = !cluster_num in
      incr cluster_num;
      List.iter (fun symb -> STbl.add table symb num) cluster)
    clusters;
  (* compare symbols by their number, if they have. Smaller numbers are bigger symbols *)
  let compare s1 s2 =
    try
      let s1_num = STbl.find table s1
      and s2_num = STbl.find table s2 in
      s2_num - s1_num
    with Not_found -> 0 (* at least one is not in the table, we do not order *)
  in compare

(** build a hashtable from the given ordering *)
let mk_table symbols =
  let table = STbl.create 5 in
  let _ = List.fold_left (fun i s -> STbl.add table s i; i+1) 0 symbols
  in table

let list_constraint l =
  let table = mk_table l in
  (* compare symbols by number. Smaller symbols have bigger number *)
  fun a b ->
    try let na = STbl.find table a
        and nb = STbl.find table b in
        nb - na
    with Not_found -> 0

let arity_constraint s1 s2 =
  let _, a1 = Type.arity (Symbol.ty s1) in
  let _, a2 = Type.arity (Symbol.ty s2) in
  (* bigger arity means bigger symbol *)
  a1 - a2

let invfreq_constraint formulas =
  let freq_table = STbl.create 5 in
  (* frequency of symbols in clause *)
  let rec form_freq f = F.iter term_freq f
  and term_freq t = match t.T.term with
    | T.Var _ | T.BoundVar _ -> ()
    | T.Node (s, _, l) ->
      (let count = try STbl.find freq_table s with Not_found -> 0 in
      STbl.replace freq_table s (count+1);
      List.iter term_freq l)
  in
  Sequence.iter form_freq formulas;
  (* compare by inverse frequency (higher frequency => smaller) *)
  fun s1 s2 ->
    let freq1 = try STbl.find freq_table s1 with Not_found -> 0
    and freq2 = try STbl.find freq_table s2 with Not_found -> 0 in
    freq2 - freq1

let max_constraint symbols =
  let table = STbl.create 5
  and num = ref 0 in
  (* give number to symbols *)
  List.iter
    (fun symb -> let n = !num in
      incr num; STbl.add table symb n)
    symbols;
  let compare a b =
    (* not found implies the symbol is smaller than maximal symbols *)
    let a_n = try STbl.find table a with Not_found -> !num
    and b_n = try STbl.find table b with Not_found -> !num in
    b_n - a_n  (* if a > b then a_n < b_n *)
  in compare

let min_constraint symbols =
  let table = STbl.create 11
  and num = ref 0 in
  (* give number to symbols *)
  List.iter
    (fun symb -> let n = !num in
      incr num; STbl.add table symb n)
    symbols;
  fun a b ->
    (* not found implies the symbol is bigger than minimal symbols *)
    let a_n = try STbl.find table a with Not_found -> -1
    and b_n = try STbl.find table b with Not_found -> -1 in
    b_n - a_n  (* if a > b then a_n < b_n *)

(* regular string ordering *)
let alpha_constraint a b = Symbol.compare a b

(* ----------------------------------------------------------------------
 * Weight function
 * ---------------------------------------------------------------------- *)

(** weight of f = arity of f + 4 *)
let weight_modarity signature a =
  try fst (SMap.find a signature) + 4
  with Not_found -> 4

(** constant weight *)
let weight_constant a = 4

(** {2 Creation of a precedence from constraints} *)

(** Add the special symbols to the list *)
let complete_symbols symbols = 
  Util.list_union (==) symbols (Signature.to_symbols Signature.base)

(** Order the list of symbols using the constraints *)
let order_symbols constrs symbols =
  let po = PartialOrder.mk_partial_order symbols in
  (* complete the partial order using constraints, starting with the
     strongest ones *)
  List.iter (fun constr -> PartialOrder.complete po constr) constrs;
  assert (PartialOrder.is_total po);
  PartialOrder.symbols po

(** build a precedence on the [symbols] from a list of constraints *)
let create ?(complete=false) constrs symbols =
  let symbols = if complete then complete_symbols symbols else symbols in
  let symbols = order_symbols constrs symbols in
  let table = mk_table symbols in
  let weight = weight_constant in
  (* how to build a precedence *)
  let rec mk_prec symbols table weight =
    (** Add the given symbols to the precedence. Returns how many of them
        are new and have effectively been added *)
    let prec_add_symbols new_symbols = 
      let old_len = List.length symbols in
      let all_symbols = Util.list_union (==) new_symbols symbols in
      let new_len = List.length all_symbols in
      if new_len > old_len then begin
        (* some symbols have been added *)
        Util.debug 3 "add %a to the precedence"
                      (Util.pp_list ~sep:", " Symbol.pp) new_symbols;
        Util.debug 3 "old precedence %a" pp_snapshot symbols;

        (* build a partial order that respects the current ordering *)
        let po = PartialOrder.mk_partial_order all_symbols in
        PartialOrder.complete po (list_constraint symbols);
        (* complete it with the constraints *)
        List.iter (fun constr -> PartialOrder.complete po constr) constrs;
        assert (PartialOrder.is_total po);
        (* get the new precedence from the completed partial order *)
        let all_symbols = PartialOrder.symbols po in
        let table' = mk_table all_symbols in
        let prec' = mk_prec all_symbols table' weight in
        Util.debug 3 "new precedence %a" pp_snapshot all_symbols;
        (* return number of new symbols *)
        prec', new_len - old_len
      end else mk_prec symbols table weight, 0
    in
    let prec_compare a b = 
      (* some symbols are not explicitely in the signature. Instead, they
         are represented by 'generic' symbols *)
      let transform_symbol s = match s with
        | _ when Symbol.has_flag Symbol.flag_split s -> Symbol.split_symbol
        | _ when Symbol.has_flag Symbol.flag_fresh_const s -> Symbol.const_symbol
        | _ -> s
      in
      let a' = transform_symbol a
      and b' = transform_symbol b in
      try STbl.find table b' - STbl.find table a'
      with Not_found ->
        Symbol.compare a' b'
    in
    let prec_weight s = weight s in
    let prec_set_weight weight' = mk_prec symbols table weight' in
    { prec_snapshot = symbols;
      prec_add_symbols;
      prec_compare;
      prec_weight;
      prec_set_weight;
    }
  in
  (* initial precedence *)
  mk_prec symbols table weight

let default l =
  (* two constraints: false, true at end of precedence, and alpha constraint
    to be sure that the ordering is total *)
  let constrs =
    [min_constraint [Symbol.false_symbol; Symbol.true_symbol];
     alpha_constraint] in
  create constrs l

let default_of_set set =
  default (Symbol.Set.elements set)

let default_of_signature signature =
  default (Signature.to_symbols signature)
