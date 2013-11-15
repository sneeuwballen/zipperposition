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

(** {1 FOTerm Orderings} *)

module T = FOTerm

let prof_rpo = Util.mk_profiler "compare_rpo"
let prof_rpo6 = Util.mk_profiler "compare_rpo6"
let prof_kbo = Util.mk_profiler "compare_kbo"

open Comparison

(** {2 Type definitions} *)

type t = {
  ord_clear_cache : unit -> unit;                 (** Clear underlying cache *)
  ord_compare : FOTerm.t -> FOTerm.t -> Comparison.t; (** Compare two terms *)
  ord_precedence : Precedence.t;                  (** Current precedence *)
  ord_set_precedence : Precedence.t -> t;         (** Change the precedence *)
  ord_name : string;                              (** Name of the ordering *)
} (** A reduction ordering on terms *)

let compare ord t1 t2 = ord.ord_compare t1 t2

let precedence ord = ord.ord_precedence

let set_precedence ord prec = ord.ord_set_precedence prec

let add_symbols ord l =
  let prec = Precedence.add_symbols ord.ord_precedence l in
  set_precedence ord prec

let add_signature ord signature =
  let prec = Precedence.add_signature ord.ord_precedence signature in
  set_precedence ord prec

let name ord = ord.ord_name

let clear_cache ord = ord.ord_clear_cache ()

let pp buf ord =
  Printf.bprintf buf "%s(%a)" ord.ord_name Precedence.pp ord.ord_precedence

let to_string ord =
  let b = Buffer.create 20 in
  pp b ord;
  Buffer.contents b

let fmt fmt ord =
  Format.pp_print_string fmt (to_string ord)

(** {2 Common internal interface for orderings} *)

module type S = sig
  (* This order relation should be:
   * - stable for instantiation
   * - monotonic
   * - total on ground terms *)
  val compare_terms : prec:Precedence.t -> FOTerm.t -> FOTerm.t -> Comparison.t

  val name : string
end
  
(** {2 Ordering implementations} *)

module KBO = struct
  let name = "kbo"

  open Precedence

  (** used to keep track of the balance of variables *)
  type var_balance = {
    offset : int;
    mutable pos_counter : int;
    mutable neg_counter : int;
    balance : int array;
  }

  (** create a balance for the two terms *)
  let mk_balance t1 t2 =
    if T.is_ground t1 && T.is_ground t2
      then
        { offset = 0; pos_counter = 0; neg_counter = 0; balance = Obj.magic None }
      else begin
        let vars = T.vars_list [t1; t2] in
        let minvar, maxvar = T.min_var vars, T.max_var vars in
        assert (minvar <= maxvar);
        let width = maxvar - minvar + 1 in  (* width between min var and max var *)
        let vb = {
          offset = minvar; (* offset of variables to 0 *)
          pos_counter = 0;
          neg_counter = 0;
          balance = Array.make width 0;
        } in
        Obj.set_tag (Obj.repr vb.balance) Obj.no_scan_tag;  (* no GC scan *)
        vb
      end

  (** add a positive variable *)
  let add_pos_var balance idx =
    let idx = idx - balance.offset in
    let n = balance.balance.(idx) in
    (if n = 0
      then balance.pos_counter <- balance.pos_counter + 1
      else if n = -1 then balance.neg_counter <- balance.neg_counter - 1);
    balance.balance.(idx) <- n + 1

  (** add a negative variable *)
  let add_neg_var balance idx =
    let idx = idx - balance.offset in
    let n = balance.balance.(idx) in
    (if n = 0
      then balance.neg_counter <- balance.neg_counter + 1
      else if n = 1 then balance.pos_counter <- balance.pos_counter - 1);
    balance.balance.(idx) <- n - 1

  (** the KBO ordering itself. The implementation is borrowed from
      the kbo_5 version of "things to know when implementing KBO".
      It should be linear time. *)
  let rec kbo ~prec t1 t2 =
    let balance = mk_balance t1 t2 in
    (** variable balance, weight balance, t contains variable y. pos
        stands for positive (is t the left term) *)
    let rec balance_weight wb t y pos =
      match t.T.term with
      | T.Var x ->
        if pos
          then (add_pos_var balance x; (wb + 1, x = y))
          else (add_neg_var balance x; (wb - 1, x = y))
      | T.BoundVar _ -> (if pos then wb + 1 else wb - 1), false
      | T.Node (s, _, l) ->
        let wb' = if pos then wb + prec.prec_weight s else wb - prec.prec_weight s in
        balance_weight_rec wb' l y pos false
    (** list version of the previous one, threaded with the check result *)
    and balance_weight_rec wb terms y pos res = match terms with
      | [] -> (wb, res)
      | t::terms' ->
        let (wb', res') = balance_weight wb t y pos in
        balance_weight_rec wb' terms' y pos (res || res')
    (** lexicographic comparison *)
    and tckbolex wb terms1 terms2 =
      match terms1, terms2 with
      | [], [] -> wb, Eq
      | t1::terms1', t2::terms2' ->
        (match tckbo wb t1 t2 with
        | (wb', Eq) -> tckbolex wb' terms1' terms2'
        | (wb', res) -> (* just compute the weights and return result *)
          let wb'', _ = balance_weight_rec wb' terms1' 0 true false in
          let wb''', _ = balance_weight_rec wb'' terms2' 0 false false in
          wb''', res)
      | [], _ | _, [] -> failwith "different arities in lexicographic comparison"
    (** commutative comparison. Not linear, must call kbo to
        avoid breaking the weight computing invariants *)
    and tckbocommute wb ss ts =
      (* multiset comparison *)
      let res = Multiset.compare (kbo ~prec) (Multiset.create ss) (Multiset.create ts) in
      (* also compute weights of subterms *)
      let wb', _ = balance_weight_rec wb ss 0 true false in
      let wb'', _ = balance_weight_rec wb' ts 0 false false in
      wb'', res
    (** tupled version of kbo (kbo_5 of the paper) *)
    and tckbo wb t1 t2 =
      match t1.T.term, t2.T.term with
      | _ when T.eq t1 t2 -> (wb, Eq) (* do not update weight or var balance *)
      | T.Var x, T.Var y ->
        add_pos_var balance x;
        add_neg_var balance y;
        (wb, Incomparable)
      | T.Var x,  _ ->
        add_pos_var balance x;
        let wb', contains = balance_weight wb t2 x false in
        (wb' + 1, if contains then Lt else Incomparable)
      |  _, T.Var y -> 
        add_neg_var balance y;
        let wb', contains = balance_weight wb t1 y true in
        (wb' - 1, if contains then Gt else Incomparable)
      (* node/node, De Bruijn/De Bruijn *)
      | T.Node (f, _, ss), T.Node (g, _, ts) -> tckbo_composite wb f g ss ts
      | T.BoundVar i, T.BoundVar j ->
        (wb, if i = j then Eq else Incomparable)
      (* node and something else *)
      | T.Node (f, _, ss), T.BoundVar _ -> tckbo_composite wb f Symbol.db_symbol ss []
      | T.BoundVar _, T.Node (g, _, ts) -> tckbo_composite wb Symbol.db_symbol g [] ts
    (** tckbo, for composite terms (ie non variables). It takes a symbol
        and a list of subterms. *)
    and tckbo_composite wb f g ss ts =
      (* do the recursive computation of kbo *)
      let wb', recursive = tckbo_rec wb f g ss ts in
      let wb'' = wb' + prec.prec_weight f - prec.prec_weight g in
      (* check variable condition *)
      let g_or_n = if balance.neg_counter = 0 then Gt else Incomparable
      and l_or_n = if balance.pos_counter = 0 then Lt else Incomparable in
      (* lexicographic product of weight and precedence *)
      if wb'' > 0 then wb'', g_or_n
      else if wb'' < 0 then wb'', l_or_n
      else (match prec.prec_compare f g with
        | n when n > 0 -> wb'', g_or_n
        | n when n < 0 ->  wb'', l_or_n
        | _ ->
          assert (List.length ss = List.length ts);
          if recursive = Eq then wb'', Eq
          else if recursive = Lt then wb'', l_or_n
          else if recursive = Gt then wb'', g_or_n
          else wb'', Incomparable)
    (** recursive comparison *)
    and tckbo_rec wb f g ss ts =
      if f = g
        then if Symbol.has_flag Symbol.flag_multiset f
          (* use multiset or lexicographic comparison *)
          then tckbocommute wb ss ts
          else tckbolex wb ss ts
        else
          (* just compute variable and weight balances *)
          let wb', _ = balance_weight_rec wb ss 0 true false in
          let wb'', _ = balance_weight_rec wb' ts 0 false false in
          wb'', Incomparable
    in
    let _, res = tckbo 0 t1 t2 in res  (* ignore the weight *)

  let compare_terms ~prec x y =
    Util.enter_prof prof_kbo;
    let cmp = kbo ~prec x y in
    Util.exit_prof prof_kbo;
    cmp
end

(** hopefully more efficient (polynomial) implementation of LPO,
    following the paper "things to know when implementing LPO" by Löchner.
    We adapt here the implementation clpo6 with some multiset symbols (=) *)
module RPO6 = struct
  let name = "rpo6"

  open Precedence

  (** recursive path ordering *)
  let rec rpo6 ~prec s t =
    if T.eq s t then Eq else  (* equality test is cheap *)
    match s.T.term, t.T.term with
    | T.Var _, T.Var _ -> Incomparable
    | _, T.Var _ -> if T.var_occurs t s then Gt else Incomparable
    | T.Var _, _ -> if T.var_occurs s t then Lt else Incomparable
    (* node/node, De Bruijn/De Bruijn *)
    | T.Node (f, _, ss), T.Node (g, _, ts) -> rpo6_composite ~prec s t f g ss ts
    | T.BoundVar i, T.BoundVar j ->
      if i = j then Eq else Incomparable
    (* node and something else *)
    | T.Node (f, _, ss), T.BoundVar _ -> rpo6_composite ~prec s t f Symbol.db_symbol ss []
    | T.BoundVar _, T.Node (g, _, ts) -> rpo6_composite ~prec s t Symbol.db_symbol g [] ts
  (* handle the composite cases *)
  and rpo6_composite ~prec s t f g ss ts =
    match prec.prec_compare f g with
    | 0 when Symbol.has_flag Symbol.flag_multiset f ->
      cMultiset ~prec ss ts (* multiset subterm comparison *)
    | 0 ->
      cLMA ~prec s t ss ts  (* lexicographic subterm comparison *)
    | n when n > 0 -> cMA ~prec s ts
    | n when n < 0 -> Comparison.opp (cMA ~prec t ss)
    | _ -> assert false  (* match exhaustively *)
  (** try to dominate all the terms in ts by s; but by subterm property
      if some t' in ts is >= s then s < t=g(ts) *)
  and cMA ~prec s ts = match ts with
    | [] -> Gt
    | t::ts' ->
      (match rpo6 ~prec s t with
      | Gt -> cMA ~prec s ts'
      | Eq | Lt -> Lt
      | Incomparable -> Comparison.opp (alpha ~prec ts' s))
  (** lexicographic comparison of s=f(ss), and t=f(ts) *)
  and cLMA ~prec s t ss ts = match ss, ts with
    | si::ss', ti::ts' ->
      (match rpo6 ~prec si ti with
        | Eq -> cLMA ~prec s t ss' ts'
        | Gt -> cMA ~prec s ts' (* just need s to dominate the remaining elements *)
        | Lt -> Comparison.opp (cMA ~prec t ss')
        | Incomparable -> cAA ~prec s t ss' ts'
      )
    | [], [] -> Eq
    | _ -> assert false (* different length... *)
  (** multiset comparison of subterms (not optimized) *)
  and cMultiset ~prec ss ts =
    Multiset.compare (rpo6 ~prec) (Multiset.create ss) (Multiset.create ts)
  (** bidirectional comparison by subterm property (bidirectional alpha) *)
  and cAA ~prec s t ss ts =
    match alpha ~prec ss t with
    | Gt -> Gt
    | Incomparable -> Comparison.opp (alpha ~prec ts s)
    | _ -> assert false
  (** if some s in ss is >= t, then s > t by subterm property and transitivity *)
  and alpha ~prec ss t = match ss with
    | [] -> Incomparable
    | s::ss' ->
      (match rpo6 ~prec s t with
       | Eq | Gt -> Gt
       | Incomparable | Lt -> alpha ~prec ss' t)

  let compare_terms ~prec x y =
    Util.enter_prof prof_rpo6;
    let cmp = rpo6 ~prec x y in
    Util.exit_prof prof_rpo6;
    cmp
end

(** {2 Value interface} *)

(** Check that new_prec is a compatible superset of old_prec *)
let check_precedence old_prec new_prec =
  Util.debug 3 "check compatibility of %a with %a"
                Precedence.pp old_prec Precedence.pp new_prec;
  let rec check l = match l with
  | [] | [_] -> true
  | x::((y::_) as l') -> Precedence.compare new_prec x y > 0 && check l'
  in check (Precedence.snapshot old_prec)

let kbo prec =
  let cache = T.T2Cache.create 4096 in
  let rec mk_ord prec =
    let compare a b = KBO.compare_terms ~prec:prec a b in
    let ord_compare a b = T.T2Cache.with_cache cache compare a b in
    let ord_set_precedence prec' =
      assert (check_precedence prec prec');
      mk_ord prec'
    in
    { ord_name = "kbo";
      ord_compare;
      ord_clear_cache = (fun () -> T.T2Cache.clear cache);
      ord_precedence = prec;
      ord_set_precedence;
    }
  in mk_ord prec

let rpo6 prec =
  let cache = T.T2Cache.create 4096 in
  let rec mk_ord prec =
    let compare a b = RPO6.compare_terms ~prec:prec a b in
    let ord_compare a b = T.T2Cache.with_cache cache compare a b in
    let ord_set_precedence prec' =
      assert (check_precedence prec prec');
      mk_ord prec'
    in
    { ord_name = "rpo6";
      ord_compare;
      ord_clear_cache = (fun () -> T.T2Cache.clear cache);
      ord_precedence = prec;
      ord_set_precedence;
    }
  in mk_ord prec

let none =
  let rec ord prec =
    { ord_name = "none";
      ord_compare = (fun t1 t2 -> if T.eq t1 t2 then Eq else Incomparable);
      ord_set_precedence = (fun prec' -> ord prec');
      ord_precedence = prec;
      ord_clear_cache = (fun () -> ());
    } in
  ord (Precedence.default [])

let subterm =
  let ord_compare t1 t2 =
    if T.eq t1 t2 then Eq
    else if T.subterm ~sub:t1 t2 then Lt
    else if T.subterm ~sub:t2 t1 then Gt
    else Incomparable
  in
  let rec ord prec = {
    ord_name = "subterm";
    ord_compare;
    ord_set_precedence = (fun prec' -> ord prec');
    ord_precedence = prec;
    ord_clear_cache = (fun () -> ());
  } in
  ord (Precedence.default [])

(** {2 Globa table of orders} *)

let __table =
  let h = Hashtbl.create 5 in
  Hashtbl.add h "rpo6" rpo6;
  Hashtbl.add h "kbo" kbo;
  Hashtbl.add h "none" (set_precedence none);
  Hashtbl.add h "subterm" (set_precedence subterm);
  h

let default l =
  rpo6 (Precedence.default l)

let default_prec prec =
  rpo6 prec

let choose name prec =
  try
    (Hashtbl.find __table name) prec
  with Not_found ->
    failwith ("no such registered ordering: " ^ name)

let register name ord =
  if Hashtbl.mem __table name
    then raise (Invalid_argument ("ordering name already used: " ^ name))
    else Hashtbl.add __table name ord
