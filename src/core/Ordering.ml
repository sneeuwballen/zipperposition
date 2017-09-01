
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Term Orderings} *)

module Prec = Precedence
module MT = Multiset.Make(Term)
module IOB = ID_or_builtin
module W = Precedence.Weight

open Comparison

let prof_rpo6 = Util.mk_profiler "compare_rpo6"
let prof_kbo = Util.mk_profiler "compare_kbo"

module T = Term
module TC = Term.Classic

let mk_cache n =
  let hash (a,b) = Hash.combine3 42 (T.hash a) (T.hash b) in
  CCCache.replacing
    ~eq:(fun (a1,b1)(a2,b2) -> T.equal a1 a2 && T.equal b1 b2)
    ~hash
    n

type term = T.t

(** {2 Type definitions} *)

type t = {
  cache : (T.t * T.t, Comparison.t) CCCache.t;
  compare : Prec.t -> term -> term -> Comparison.t;
  prec : Prec.t;
  name : string;
} (** Partial ordering on terms *)

type ordering = t

let compare ord t1 t2 = ord.compare ord.prec t1 t2

let precedence ord = ord.prec

let add_list ord l = Prec.add_list ord.prec l
let add_seq ord seq = Prec.add_seq ord.prec seq

let name ord = ord.name

let clear_cache ord = CCCache.clear ord.cache

let pp out ord =
  Format.fprintf out "%s(@[%a@])" ord.name Prec.pp ord.prec

let to_string ord = CCFormat.to_string pp ord

(** Common internal interface for orderings *)

module type ORD = sig
  (* This order relation should be:
   * - stable for instantiation
   * - monotonic
   * - total on ground terms *)
  val compare_terms : prec:Prec.t -> term -> term -> Comparison.t

  val name : string
end

(** {2 Ordering implementations} *)

(* compare the two symbols (ID or builtin) using the precedence *)
let prec_compare prec a b = match a, b with
  | IOB.I a, IOB.I b -> Prec.compare prec a b
  | IOB.I _, IOB.B _ -> 1
  | IOB.B _, IOB.I _ -> -1
  | IOB.B a, IOB.B b -> Builtin.compare a b

let prec_status prec = function
  | IOB.I s -> Prec.status prec s
  | IOB.B Builtin.Eq -> Prec.Multiset
  | IOB.B _ -> Prec.Lexicographic

module KBO : ORD = struct
  let name = "kbo"

  (* small cache for allocated arrays *)
  let alloc_cache = AllocCache.Arr.create ~buck_size:2 10

  (** used to keep track of the balance of variables *)
  type var_balance = {
    offset : int;
    mutable pos_counter : int;
    mutable neg_counter : int;
    mutable balance : int array;
  }

  (** create a balance for the two terms *)
  let mk_balance t1 t2: var_balance =
    if T.is_ground t1 && T.is_ground t2
    then (
      { offset = 0; pos_counter = 0; neg_counter = 0; balance = [||]; }
    ) else (
      let vars = Sequence.of_list [t1; t2] |> Sequence.flat_map T.Seq.vars in
      (* TODO: compute both at the same time *)
      let minvar = T.Seq.min_var vars in
      let maxvar = T.Seq.max_var vars in
      assert (minvar <= maxvar);
      (* width between min var and max var *)
      let width = maxvar - minvar + 1 in
      let vb = {
        offset = minvar; (* offset of variables to 0 *)
        pos_counter = 0;
        neg_counter = 0;
        balance = AllocCache.Arr.make alloc_cache width 0;
      } in
      Obj.set_tag (Obj.repr vb.balance) Obj.no_scan_tag;  (* no GC scan *)
      vb
    )

  (** add a positive variable *)
  let add_pos_var balance idx =
    let idx = idx - balance.offset in
    let n = balance.balance.(idx) in
    if n = 0
    then balance.pos_counter <- balance.pos_counter + 1
    else (
      if n = -1 then balance.neg_counter <- balance.neg_counter - 1
    );
    balance.balance.(idx) <- n + 1

  (** add a negative variable *)
  let add_neg_var balance idx =
    let idx = idx - balance.offset in
    let n = balance.balance.(idx) in
    if n = 0
    then balance.neg_counter <- balance.neg_counter + 1
    else (
      if n = 1 then balance.pos_counter <- balance.pos_counter - 1
    );
    balance.balance.(idx) <- n - 1

  let weight prec = function
    | IOB.B _ -> W.int 1
    | IOB.I s -> Prec.weight prec s

  exception Has_lambda

  (* TODO: ghc() that maps (in theory) variables to sets of symbols
     their instances can have as head (for a symbol [f] it's just [{f}] itself).
     Then [x > y] if [forall f∈ghc(x),y∈ghd(y), f>g] in precedence.
  *)

  (* FIXME: use [int ref HVar.Map] instead of an array, because there
     might be variable collisions?
     Check impact on perf *)

  (** the KBO ordering itself. The implementation is borrowed from
      the kbo_5 version of "things to know when implementing KBO".
      It should be linear time. *)
  let rec kbo ~prec t1 t2 =
    let balance = mk_balance t1 t2 in
    (** variable balance, weight balance, t contains variable y. pos
        stands for positive (is t the left term) *)
    let rec balance_weight (wb:W.t) t y ~pos : W.t * bool =
      match T.view t with
        | T.Var x ->
          let x = HVar.id x in
          if pos then (
            add_pos_var balance x;
            W.(wb + one), x = y
          ) else (
            add_neg_var balance x;
            W.(wb - one), x = y
          )
        | T.DB _ ->
          let w = if pos then W.(wb + one) else W.(wb - one) in
          w, false
        | T.Const s ->
          let open W.Infix in
          let wb' =
            if pos
            then wb + weight prec (IOB.I s)
            else wb - weight prec (IOB.I s)
          in wb', false
        | T.App (f, l) ->
          let wb', res = balance_weight wb f y ~pos in
          balance_weight_rec wb' l y ~pos res
        | T.AppBuiltin (b,l) ->
          let open W.Infix in
          let wb' = if pos
            then wb + weight prec (IOB.B b)
            else wb - weight prec (IOB.B b)
          in
          balance_weight_rec wb' l y ~pos false
        | T.Fun _ -> raise Has_lambda
    (** list version of the previous one, threaded with the check result *)
    and balance_weight_rec wb terms y ~pos res = match terms with
      | [] -> (wb, res)
      | t::terms' ->
        let wb', res' = balance_weight wb t y ~pos in
        balance_weight_rec wb' terms' y ~pos (res || res')
    (** lexicographic comparison *)
    and tckbolex wb terms1 terms2 =
      match terms1, terms2 with
        | [], [] -> wb, Eq
        | t1::terms1', t2::terms2' ->
          begin match tckbo wb t1 t2 with
            | (wb', Eq) -> tckbolex wb' terms1' terms2'
            | (wb', res) -> (* just compute the weights and return result *)
              let wb'', _ = balance_weight_rec wb' terms1' 0 ~pos:true false in
              let wb''', _ = balance_weight_rec wb'' terms2' 0 ~pos:false false in
              wb''', res
          end
        | [], _ ->
          let wb, _ = balance_weight_rec wb terms2 0 ~pos:false false in
          wb, Lt
        | _, [] ->
          let wb, _ = balance_weight_rec wb terms1 0 ~pos:true false in
          wb, Gt
    (** commutative comparison. Not linear, must call kbo to
        avoid breaking the weight computing invariants *)
    and tckbocommute wb ss ts =
      (* multiset comparison *)
      let res = MT.compare_partial_l (kbo ~prec) ss ts in
      (* also compute weights of subterms *)
      let wb', _ = balance_weight_rec wb ss 0 ~pos:true false in
      let wb'', _ = balance_weight_rec wb' ts 0 ~pos:false false in
      wb'', res
    (** tupled version of kbo (kbo_5 of the paper) *)
    and tckbo (wb:W.t) t1 t2 =
      match TC.view t1, TC.view t2 with
        | _ when T.equal t1 t2 -> (wb, Eq) (* do not update weight or var balance *)
        | TC.Var x, TC.Var y ->
          add_pos_var balance (HVar.id x);
          add_neg_var balance (HVar.id y);
          (wb, Incomparable)
        | TC.Var x,  _ ->
          add_pos_var balance (HVar.id x);
          let wb', contains = balance_weight wb t2 (HVar.id x) ~pos:false in
          (W.(wb' + one), if contains then Lt else Incomparable)
        |  _, TC.Var y ->
          add_neg_var balance (HVar.id y);
          let wb', contains = balance_weight wb t1 (HVar.id y) ~pos:true in
          (W.(wb' - one), if contains then Gt else Incomparable)
        (* node/node, De Bruijn/De Bruijn *)
        | TC.App (f, ss), TC.App (g, ts) -> tckbo_composite wb (IOB.I f) (IOB.I g) ss ts
        | TC.AppBuiltin (f, ss), TC.App (g, ts) -> tckbo_composite wb (IOB.B f) (IOB.I g) ss ts
        | TC.App (f, ss), TC.AppBuiltin (g, ts) -> tckbo_composite wb (IOB.I f) (IOB.B g) ss ts
        | TC.AppBuiltin (f, ss), TC.AppBuiltin (g, ts) -> tckbo_composite wb (IOB.B f) (IOB.B g) ss ts
        | TC.DB i, TC.DB j ->
          (wb, if i = j then Eq else Incomparable)
        | TC.NonFO, _
        | _, TC.NonFO ->
          (* compare partial applications *)
          let hd1, l1 = T.as_app t1 in
          let hd2, l2 = T.as_app t2 in
          if CCList.is_empty l1 && CCList.is_empty l2 then (
            let c = if T.equal t1 t2 then Eq else Incomparable in
            let wb, _ = balance_weight wb t1 0 ~pos:true in
            let wb, _ = balance_weight wb t2 0 ~pos:false in
            wb, c
          ) else (
            let wb, cmp_hd = tckbo wb hd1 hd2 in
            (* how to update the [wb] for [l1,l2] *)
            let update_wb() =
              let wb, _ = balance_weight_rec wb l1 0 ~pos:true false in
              let wb, _ = balance_weight_rec wb l2 0 ~pos:false false in
              wb
            in
            begin match cmp_hd with
              | Eq -> tckbolex wb l1 l2
              | Lt ->
                update_wb(), if balance.pos_counter = 0 then Lt else Incomparable
              | Gt ->
                update_wb(), if balance.neg_counter = 0 then Gt else Incomparable
              | Incomparable ->
                update_wb(), Incomparable
            end
          )
        (* node and something else *)
        | (TC.App (_, _) | TC.AppBuiltin _), TC.DB _ ->
          let wb', _ = balance_weight wb t1 0 ~pos:true in
          W.(wb'-one), Comparison.Gt
        | TC.DB _, (TC.App (_, _) | TC.AppBuiltin _) ->
          let wb', _ = balance_weight wb t1 0 ~pos:false in
          W.(wb'+one), Comparison.Lt
    (** tckbo, for composite terms (ie non variables). It takes a ID.t
        and a list of subterms. *)
    and tckbo_composite wb f g ss ts =
      (* do the recursive computation of kbo *)
      let wb', res = tckbo_rec wb f g ss ts in
      let wb'' = W.(wb' + weight prec f - weight prec g) in
      (* check variable condition *)
      let g_or_n = if balance.neg_counter = 0 then Gt else Incomparable
      and l_or_n = if balance.pos_counter = 0 then Lt else Incomparable in
      (* lexicographic product of weight and precedence *)
      if W.sign wb'' > 0 then wb'', g_or_n
      else if W.sign wb'' < 0 then wb'', l_or_n
      else match prec_compare prec f g with
        | n when n > 0 -> wb'', g_or_n
        | n when n < 0 ->  wb'', l_or_n
        | _ ->
          assert (List.length ss = List.length ts);
          if res = Eq then wb'', Eq
          else if res = Lt then wb'', l_or_n
          else if res = Gt then wb'', g_or_n
          else wb'', Incomparable
    (** recursive comparison *)
    and tckbo_rec wb f g ss ts =
      if IOB.equal f g
      then match prec_status prec f with
        | Prec.Multiset ->
          (* use multiset or lexicographic comparison *)
          tckbocommute wb ss ts
        | Prec.Lexicographic ->
          tckbolex wb ss ts
      else (
        (* just compute variable and weight balances *)
        let wb', _ = balance_weight_rec wb ss 0 ~pos:true false in
        let wb'', _ = balance_weight_rec wb' ts 0 ~pos:false false in
        wb'', Incomparable
      )
    in
    try
      let _, res = tckbo W.zero t1 t2 in
      AllocCache.Arr.free alloc_cache balance.balance;
      res
    with
      | Has_lambda ->
        (* lambda terms are not comparable, except trivial
           case when they are syntactically equal *)
        AllocCache.Arr.free alloc_cache balance.balance;
        Incomparable
      | e ->
        AllocCache.Arr.free alloc_cache balance.balance;
        raise e

  let compare_terms ~prec x y =
    Util.enter_prof prof_kbo;
    let compare = kbo ~prec x y in
    Util.exit_prof prof_kbo;
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
        | _, TC.Var var -> if T.var_occurs ~var s then Gt else Incomparable
        | TC.Var var, _ -> if T.var_occurs ~var t then Lt else Incomparable
        (* whatever *)
        | TC.NonFO, _
        | _, TC.NonFO -> Comparison.Incomparable
        (* node/node, De Bruijn/De Bruijn *)
        | TC.App (f, ss), TC.App (g, ts) -> rpo6_composite ~prec s t (IOB.I f) (IOB.I g) ss ts
        | TC.AppBuiltin (f, ss), TC.App (g, ts) -> rpo6_composite ~prec s t (IOB.B f) (IOB.I g) ss ts
        | TC.App (f, ss), TC.AppBuiltin (g, ts) -> rpo6_composite ~prec s t (IOB.I f) (IOB.B g) ss ts
        | TC.AppBuiltin (f, ss), TC.AppBuiltin (g, ts) -> rpo6_composite ~prec s t (IOB.B f) (IOB.B g) ss ts
        | TC.DB i, TC.DB j ->
          if i = j && Type.equal (T.ty s) (T.ty t) then Eq else Incomparable
        (* node and something else *)
        | (TC.App _ | TC.AppBuiltin _), TC.DB _ -> Comparison.Incomparable
        | TC.DB _, (TC.App _ | TC.AppBuiltin _) -> Comparison.Incomparable
  (* handle the composite cases *)
  and rpo6_composite ~prec s t f g ss ts =
    match prec_compare prec f g with
      | 0 ->
        begin match prec_status prec f with
          | Precedence.Multiset ->
            cMultiset ~prec ss ts (* multiset subterm comparison *)
          | Precedence.Lexicographic ->
            cLMA ~prec s t ss ts  (* lexicographic subterm comparison *)
        end
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
      begin match rpo6 ~prec si ti with
        | Eq -> cLMA ~prec s t ss' ts'
        | Gt -> cMA ~prec s ts' (* just need s to dominate the remaining elements *)
        | Lt -> Comparison.opp (cMA ~prec t ss')
        | Incomparable -> cAA ~prec s t ss' ts'
      end
    | [], [] -> Eq

    (* below are rules for extending to HO terms, likely
       incomplete and dangerous!
       TODO: instead compare types, which MUST be distinct in
       this case since the head symbol is the same *)
    | [], _::_ -> Lt
    | _::_, [] -> Gt
  (** multiset comparison of subterms (not optimized) *)
  and cMultiset ~prec ss ts =
    MT.compare_partial_l (rpo6 ~prec) ss ts
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
    let compare = rpo6 ~prec x y in
    Util.exit_prof prof_rpo6;
    compare
end

(** {2 Value interface} *)

let kbo prec =
  let cache = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache
      (fun (a, b) -> KBO.compare_terms ~prec a b) (a,b)
  in
  { cache; compare; name=KBO.name; prec; }

let rpo6 prec =
  let cache = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache
      (fun (a, b) -> RPO6.compare_terms ~prec a b) (a,b)
  in
  { cache; compare; name=RPO6.name; prec; }

let dummy_cache_ = CCCache.dummy

let none =
  let compare _ t1 t2 = if T.equal t1 t2 then Eq else Incomparable in
  { cache=dummy_cache_; compare; prec=Prec.default []; name="none"; }

let subterm =
  let compare _ t1 t2 =
    if T.equal t1 t2 then Eq
    else if T.subterm ~sub:t1 t2 then Lt
    else if T.subterm ~sub:t2 t1 then Gt
    else Incomparable
  in
  { cache=dummy_cache_; compare; prec=Prec.default []; name="subterm"; }

(** {2 Global table of orders} *)

let tbl_ =
  let h = Hashtbl.create 5 in
  Hashtbl.add h "rpo6" rpo6;
  Hashtbl.add h "kbo" kbo;
  Hashtbl.add h "none" (fun _ -> none);
  Hashtbl.add h "subterm" (fun _ -> subterm);
  h

let default_of_list l =
  rpo6 (Prec.default l)

let default_name = "kbo"
let names () = CCHashtbl.keys_list tbl_

let default_of_prec prec =
  default_of_list (Prec.snapshot prec)

let by_name name prec =
  try
    (Hashtbl.find tbl_ name) prec
  with Not_found ->
    invalid_arg ("no such registered ordering: " ^ name)

let register name ord =
  if Hashtbl.mem tbl_ name
  then invalid_arg ("ordering name already used: " ^ name)
  else Hashtbl.add tbl_ name ord
