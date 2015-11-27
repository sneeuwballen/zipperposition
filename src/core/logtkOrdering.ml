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

(** {1 Term LogtkOrderings} *)

module MT = LogtkMultiset.Make(struct
  type t = LogtkFOTerm.t
  let compare = LogtkFOTerm.compare
end)

(** {2 LogtkType definitions} *)

module type S = LogtkOrdering_intf.S

(** {2 Functor} *)

let prof_rpo6 = LogtkUtil.mk_profiler "compare_rpo6"
let prof_kbo = LogtkUtil.mk_profiler "compare_kbo"


module Make(P : LogtkPrecedence.S with type symbol = LogtkSymbol.t) = struct
  module Prec = P

  type symbol = Prec.symbol

  module T = LogtkFOTerm
  module TC = LogtkFOTerm.Classic
  module LogtkCache = T.T2LogtkCache

  type term = T.t

  open LogtkComparison

  (** {2 LogtkType definitions} *)

  type t = {
    cache : LogtkComparison.t LogtkCache.t;
    compare : Prec.t -> term -> term -> LogtkComparison.t;
    prec : Prec.t;
    name : string;
  } (** Partial ordering on terms *)

  type ordering = t

  let compare ord t1 t2 = ord.compare ord.prec t1 t2

  let precedence ord = ord.prec

  (* Check that new_prec is a compatible superset of old_prec *)
  let _check_precedence old_prec new_prec =
    LogtkUtil.debug 3 "check compatibility of %a with %a"
      (fun k->k Prec.pp old_prec Prec.pp new_prec);
    let rec check l = match l with
    | [] | [_] -> true
    | x::((y::_) as l') -> Prec.compare new_prec x y > 0 && check l'
    in check (Prec.snapshot old_prec)

  let set_precedence ord prec' =
    if not (_check_precedence ord.prec prec')
      then raise (Invalid_argument "LogtkOrdering.set_precedence");
    LogtkCache.clear ord.cache;
    { ord with prec=prec'; }

  let update_precedence ord f =
    set_precedence ord (f ord.prec)

  let add_list ord l = update_precedence ord (fun p -> Prec.add_list p l)
  let add_seq ord seq = update_precedence ord (fun p -> Prec.add_seq p seq)

  let name ord = ord.name

  let clear_cache ord =
    LogtkCache.clear ord.cache

  let pp out ord =
    Format.fprintf out "%s(@[%a@])" ord.name Prec.pp ord.prec

  let to_string ord = CCFormat.to_string pp ord

  (** Common internal interface for orderings *)

  module type ORD = sig
    (* This order relation should be:
     * - stable for instantiation
     * - monotonic
     * - total on ground terms *)
    val compare_terms : prec:Prec.t -> term -> term -> LogtkComparison.t

    val name : string
  end

  (** {2 LogtkOrdering implementations} *)

  module KBO : ORD = struct
    let name = "kbo"

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
          let vars = Sequence.of_list [t1; t2] |> Sequence.flatMap T.Seq.vars in
          let minvar = T.Seq.min_var vars in
          let maxvar = T.Seq.max_var vars in
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

    let _weight prec s =
      let x = Prec.weight prec s in
      assert (x>0);
      x

    (** the KBO ordering itself. The implementation is borrowed from
        the kbo_5 version of "things to know when implementing KBO".
        It should be linear time. *)
    let rec kbo ~prec t1 t2 =
      let balance = mk_balance t1 t2 in
      (** variable balance, weight balance, t contains variable y. pos
          stands for positive (is t the left term) *)
      let rec balance_weight wb t y pos =
        match TC.view t with
        | TC.Var x ->
          if pos
            then (add_pos_var balance x; (wb + 1, x = y))
            else (add_neg_var balance x; (wb - 1, x = y))
        | TC.BVar _ -> (if pos then wb + 1 else wb - 1), false
        | TC.App (s, _, l) ->
          let wb' = if pos
            then wb + _weight prec s
            else wb - _weight prec s in
          balance_weight_rec wb' l y pos false
        | TC.NonFO -> assert false
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
        let res = MT.compare_partial_l (kbo ~prec) ss ts in
        (* also compute weights of subterms *)
        let wb', _ = balance_weight_rec wb ss 0 true false in
        let wb'', _ = balance_weight_rec wb' ts 0 false false in
        wb'', res
      (** tupled version of kbo (kbo_5 of the paper) *)
      and tckbo wb t1 t2 =
        match TC.view t1, TC.view t2 with
        | _ when T.equal t1 t2 -> (wb, Eq) (* do not update weight or var balance *)
        | TC.Var x, TC.Var y ->
          add_pos_var balance x;
          add_neg_var balance y;
          (wb, Incomparable)
        | TC.NonFO, _
        | _, TC.NonFO -> wb, Incomparable
        | TC.Var x,  _ ->
          add_pos_var balance x;
          let wb', contains = balance_weight wb t2 x false in
          (wb' + 1, if contains then Lt else Incomparable)
        |  _, TC.Var y ->
          add_neg_var balance y;
          let wb', contains = balance_weight wb t1 y true in
          (wb' - 1, if contains then Gt else Incomparable)
        (* node/node, De Bruijn/De Bruijn *)
        | TC.App (f, _, ss), TC.App (g, _, ts) -> tckbo_composite wb f g ss ts
        | TC.BVar i, TC.BVar j ->
          (wb, if i = j then Eq else Incomparable)
        (* node and something else *)
        | TC.App (_, _, _), TC.BVar _ ->
          let wb', _ = balance_weight wb t1 0 true in
          wb'-1, LogtkComparison.Gt
        | TC.BVar _, TC.App (_, _, _) ->
          let wb', _ = balance_weight wb t1 0 false in
          wb'+1, LogtkComparison.Lt
      (** tckbo, for composite terms (ie non variables). It takes a symbol
          and a list of subterms. *)
      and tckbo_composite wb f g ss ts =
        (* do the recursive computation of kbo *)
        let wb', recursive = tckbo_rec wb f g ss ts in
        let wb'' = wb' + _weight prec f - _weight prec g in
        (* check variable condition *)
        let g_or_n = if balance.neg_counter = 0 then Gt else Incomparable
        and l_or_n = if balance.pos_counter = 0 then Lt else Incomparable in
        (* lexicographic product of weight and precedence *)
        if wb'' > 0 then wb'', g_or_n
        else if wb'' < 0 then wb'', l_or_n
        else (match Prec.compare prec f g with
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
          then match Prec.status prec f with
          | LogtkPrecedence.Multiset ->
            (* use multiset or lexicographic comparison *)
            tckbocommute wb ss ts
          | LogtkPrecedence.Lexicographic ->
            tckbolex wb ss ts
          else
            (* just compute variable and weight balances *)
            let wb', _ = balance_weight_rec wb ss 0 true false in
            let wb'', _ = balance_weight_rec wb' ts 0 false false in
            wb'', Incomparable
      in
      let _, res = tckbo 0 t1 t2 in res  (* ignore the weight *)

    let compare_terms ~prec x y =
      LogtkUtil.enter_prof prof_kbo;
      let compare = kbo ~prec x y in
      LogtkUtil.exit_prof prof_kbo;
      compare
  end

  (** hopefully more efficient (polynomial) implementation of LPO,
      following the paper "things to know when implementing LPO" by Löchner.
      We adapt here the implementation clpo6 with some multiset symbols (=) *)
  module RPO6 : ORD = struct
    let name = "rpo6"

    (** recursive path ordering *)
    let rec rpo6 ~prec s t =
      if T.equal s t then Eq else  (* equality test is cheap *)
      match TC.view s, TC.view t with
      | TC.Var _, TC.Var _ -> Incomparable
      | _, TC.Var _ -> if T.var_occurs ~var:t s then Gt else Incomparable
      | TC.Var _, _ -> if T.var_occurs ~var:s t then Lt else Incomparable
      (* whatever *)
      | TC.NonFO, _
      | _, TC.NonFO -> LogtkComparison.Incomparable
      (* node/node, De Bruijn/De Bruijn *)
      | TC.App (f, _, ss), TC.App (g, _, ts) -> rpo6_composite ~prec s t f g ss ts
      | TC.BVar i, TC.BVar j ->
        if i = j && LogtkType.equal (T.ty s) (T.ty t) then Eq else Incomparable
      (* node and something else *)
      | TC.App (_, _, _), TC.BVar _ -> LogtkComparison.Incomparable
      | TC.BVar _, TC.App (_, _, _) -> LogtkComparison.Incomparable
    (* handle the composite cases *)
    and rpo6_composite ~prec s t f g ss ts =
      match Prec.compare prec f g with
      | 0 ->
        begin match Prec.status prec f with
        | LogtkPrecedence.Multiset ->
          cMultiset ~prec ss ts (* multiset subterm comparison *)
        | LogtkPrecedence.Lexicographic ->
          cLMA ~prec s t ss ts  (* lexicographic subterm comparison *)
        end
      | n when n > 0 -> cMA ~prec s ts
      | n when n < 0 -> LogtkComparison.opp (cMA ~prec t ss)
      | _ -> assert false  (* match exhaustively *)
    (** try to dominate all the terms in ts by s; but by subterm property
        if some t' in ts is >= s then s < t=g(ts) *)
    and cMA ~prec s ts = match ts with
      | [] -> Gt
      | t::ts' ->
        (match rpo6 ~prec s t with
        | Gt -> cMA ~prec s ts'
        | Eq | Lt -> Lt
        | Incomparable -> LogtkComparison.opp (alpha ~prec ts' s))
    (** lexicographic comparison of s=f(ss), and t=f(ts) *)
    and cLMA ~prec s t ss ts = match ss, ts with
      | si::ss', ti::ts' ->
        (match rpo6 ~prec si ti with
          | Eq -> cLMA ~prec s t ss' ts'
          | Gt -> cMA ~prec s ts' (* just need s to dominate the remaining elements *)
          | Lt -> LogtkComparison.opp (cMA ~prec t ss')
          | Incomparable -> cAA ~prec s t ss' ts'
        )
      | [], [] -> Eq
      | _ -> assert false (* different length... *)
    (** multiset comparison of subterms (not optimized) *)
    and cMultiset ~prec ss ts =
      MT.compare_partial_l (rpo6 ~prec) ss ts
    (** bidirectional comparison by subterm property (bidirectional alpha) *)
    and cAA ~prec s t ss ts =
      match alpha ~prec ss t with
      | Gt -> Gt
      | Incomparable -> LogtkComparison.opp (alpha ~prec ts s)
      | _ -> assert false
    (** if some s in ss is >= t, then s > t by subterm property and transitivity *)
    and alpha ~prec ss t = match ss with
      | [] -> Incomparable
      | s::ss' ->
        (match rpo6 ~prec s t with
         | Eq | Gt -> Gt
         | Incomparable | Lt -> alpha ~prec ss' t)

    let compare_terms ~prec x y =
      LogtkUtil.enter_prof prof_rpo6;
      let compare = rpo6 ~prec x y in
      LogtkUtil.exit_prof prof_rpo6;
      compare
  end

  (** {2 Value interface} *)

  let kbo prec =
    let cache = LogtkCache.create 4096 in
    let compare prec = LogtkCache.with_cache cache
      (fun a b -> KBO.compare_terms ~prec a b)
    in
    { cache; compare; name=KBO.name; prec; }

  let rpo6 prec =
    let cache = LogtkCache.create 4096 in
    let compare prec = LogtkCache.with_cache cache
      (fun a b -> RPO6.compare_terms ~prec a b)
    in
    { cache; compare; name=RPO6.name; prec; }

  let __cache = LogtkCache.create 5

  let none =
    let compare _ t1 t2 = if T.equal t1 t2 then Eq else Incomparable in
    { cache=__cache; compare; prec=Prec.default []; name="none"; }

  let subterm =
    let compare _ t1 t2 =
      if T.equal t1 t2 then Eq
      else if T.subterm ~sub:t1 t2 then Lt
      else if T.subterm ~sub:t2 t1 then Gt
      else Incomparable
    in
    { cache=__cache; compare; prec=Prec.default []; name="subterm"; }

  (** {2 Global table of orders} *)

  let __table =
    let h = Hashtbl.create 5 in
    Hashtbl.add h "rpo6" rpo6;
    Hashtbl.add h "kbo" kbo;
    Hashtbl.add h "none" (set_precedence none);
    Hashtbl.add h "subterm" (set_precedence subterm);
    h

  let default_of_list l =
    rpo6 (Prec.default l)

  let default_of_prec prec =
    default_of_list (Prec.snapshot prec)

  let by_name name prec =
    try
      (Hashtbl.find __table name) prec
    with Not_found ->
      raise (Invalid_argument ("no such registered ordering: " ^ name))

  let register name ord =
    if Hashtbl.mem __table name
      then raise (Invalid_argument ("ordering name already used: " ^ name))
      else Hashtbl.add __table name ord
end

module Default = Make(LogtkPrecedence.Default)

include Default
