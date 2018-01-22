
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Term Orderings} *)

module Prec = Precedence
module MT = Multiset.Make(Term)
module W = Precedence.Weight

open Comparison

let prof_lfhokbo_arg_coeff = Util.mk_profiler "compare_lfhokbo_arg_coeff"
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
  might_flip : Prec.t -> term -> term -> bool;
} (** Partial ordering on terms *)

type ordering = t

let compare ord t1 t2 = ord.compare ord.prec t1 t2

let might_flip ord t1 t2 = ord.might_flip ord.prec t1 t2

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
   * - compatible with function contexts
   * - total on ground terms *)
  val compare_terms : prec:Prec.t -> term -> term -> Comparison.t

  val might_flip : Prec.t -> term -> term -> bool

  val name : string
end

(** {2 Ordering implementations} *)

(* compare the two symbols (ID or builtin) using the precedence *)
let prec_compare prec a b = match a,b with
  | Head.I a, Head.I b ->
    begin match Prec.compare prec a b with
      | 0 -> Eq
      | n when n > 0 -> Gt
      | _ -> Lt
    end
  | Head.B a, Head.B b ->
    begin match Builtin.compare a b  with
      | 0 -> Eq
      | n when n > 0 -> Gt
      | _ -> Lt
    end
  | Head.I _, Head.B _ -> Gt (* id > builtin *)
  | Head.B _, Head.I _ -> Lt
  | Head.V x, Head.V y -> if x=y then Eq else Incomparable
  | Head.V _, _ -> Incomparable
  | _, Head.V _ -> Incomparable

let prec_status prec = function
  | Head.I s -> Prec.status prec s
  | Head.B Builtin.Eq -> Prec.Multiset
  | Head.B _ -> Prec.LengthLexicographic
  | Head.V _ -> Prec.LengthLexicographic
(* TODO: Variables should get a different status here. LengthLexicographic for variables
   will only work as long as all other symbols are LengthLexicographic as well. *)

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
    | Head.B _ -> W.int 1
    | Head.I s -> Prec.weight prec s
    | Head.V _ -> W.int 1  (* TODO: Maybe not a good value *)

  exception Has_lambda

  (** Blanchette's higher-order KBO *)
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
            then wb + weight prec (Head.I s)
            else wb - weight prec (Head.I s)
          in wb', false
        | T.App (f, l) ->
          let wb', res = balance_weight wb f y ~pos in
          balance_weight_rec wb' l y ~pos res
        | T.AppBuiltin (b,l) ->
          let open W.Infix in
          let wb' = if pos
            then wb + weight prec (Head.B b)
            else wb - weight prec (Head.B b)
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
    (** length-lexicographic comparison *)
    and tckbolenlex wb terms1 terms2 =
      if List.length terms1 = List.length terms2
      then tckbolex wb terms1 terms2
      else (
        (* just compute the weights and return result *)
        let wb', _ = balance_weight_rec wb terms1 0 ~pos:true false in
        let wb'', _ = balance_weight_rec wb' terms2 0 ~pos:false false in
        let res = if List.length terms1 > List.length terms2 then Gt else Lt in
        wb'', res
      )
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
      match T.view t1, T.view t2 with
        | _ when T.equal t1 t2 -> (wb, Eq) (* do not update weight or var balance *)
        | T.Var x, T.Var y ->
          add_pos_var balance (HVar.id x);
          add_neg_var balance (HVar.id y);
          (wb, Incomparable)
        | T.Var x,  _ ->
          add_pos_var balance (HVar.id x);
          let wb', contains = balance_weight wb t2 (HVar.id x) ~pos:false in
          (W.(wb' + one), if contains then Lt else Incomparable)
        |  _, T.Var y ->
          add_neg_var balance (HVar.id y);
          let wb', contains = balance_weight wb t1 (HVar.id y) ~pos:true in
          (W.(wb' - one), if contains then Gt else Incomparable)
        | T.DB i, T.DB j ->
          (wb, if i = j then Eq else Incomparable)
        | _ -> begin match Head.term_to_head t1, Head.term_to_head t2 with
            | Some f, Some g -> tckbo_composite wb f g (Head.term_to_args t1) (Head.term_to_args t2)
            | _ -> (wb, Incomparable)
          end
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
        | Gt -> wb'', g_or_n
        | Lt ->  wb'', l_or_n
        | Eq ->
          if res = Eq then wb'', Eq
          else if res = Lt then wb'', l_or_n
          else if res = Gt then wb'', g_or_n
          else wb'', Incomparable
        | Incomparable -> wb'', Incomparable
    (** recursive comparison *)
    and tckbo_rec wb f g ss ts =
      if prec_compare prec f g = Eq
      then match prec_status prec f with
        | Prec.Multiset ->
          tckbocommute wb ss ts
        | Prec.Lexicographic ->
          tckbolex wb ss ts
        | Prec.LengthLexicographic ->
          tckbolenlex wb ss ts
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

  (* KBO is monotonic *)
  let might_flip _ _ _ = false
end


module LFHOKBO_arg_coeff : ORD = struct
  let name = "lfhokbo_arg_coeff"


  module Weight_indet = struct
    type var = Type.t HVar.t

    type t =
      | Weight of var
      | Arg_coeff of var * int;;

    let compare x y = match (x, y) with
        Weight x', Weight y' -> HVar.compare Type.compare x' y'
      | Arg_coeff (x', i), Arg_coeff (y', j) ->
        let c = HVar.compare Type.compare x' y' in
        if c <> 0 then c else abs(i-j)
      | Weight _, Arg_coeff (_, _) -> 1
      | Arg_coeff (_, _), Weight _ -> -1

    let pp out (a:t): unit =
      begin match a with
          Weight x-> Format.fprintf out "w_%a" HVar.pp x
        | Arg_coeff (x, i) -> Format.fprintf out "k_%a_%d" HVar.pp x i
      end
    let to_string = CCFormat.to_string pp
  end

  module WI = Weight_indet
  module Weight_polynomial = Polynomial.Make(W)(WI)
  module WP = Weight_polynomial

  let rec weight prec t =
    (* Returns a function that is applied to the weight of argument i of a term
       headed by t before adding the weights of all arguments *)
    let arg_coeff_multiplier t i =
      begin match T.view t with
        | T.Const fid -> Some (WP.mult_const (Prec.arg_coeff prec fid i))
        | T.Var x ->     Some (WP.mult_indet (WI.Arg_coeff (x, i)))
        | _ -> None
      end
    in
    (* recursively calculates weights of args, applies coeff_multipliers, and
       adds all those weights plus the head_weight.*)
    let app_weight head_weight coeff_multipliers args =
      args
      |> List.mapi (fun i s ->
        begin match weight prec s, coeff_multipliers i with
          | Some w, Some c -> Some (c w)
          | _ -> None
        end )
      |> List.fold_left
        (fun w1 w2 ->
           begin match (w1, w2) with
             | Some w1', Some w2' -> Some (WP.add w1' w2')
             | _, _ -> None
           end )
        head_weight
    in
    begin match T.view t with
      | T.App (f,args) -> app_weight (weight prec f) (arg_coeff_multiplier f) args
      | T.AppBuiltin (_,args) -> app_weight (Some (WP.const (W.one))) (fun _ -> Some (fun x -> x)) args
      | T.Const fid -> Some (WP.const (Prec.weight prec fid))
      | T.Var x ->     Some (WP.indet (WI.Weight x))
      | _ -> None
    end

  let rec lfhokbo_arg_coeff ~prec t s =
    (* lexicographic comparison *)
    let rec lfhokbo_lex ts ss = match ts, ss with
      | [], [] -> Eq
      | _ :: _, [] -> Gt
      | [] , _ :: _ -> Lt
      | t0 :: t_rest , s0 :: s_rest ->
        begin match lfhokbo_arg_coeff ~prec t0 s0 with
          | Gt -> Gt
          | Lt -> Lt
          | Eq -> lfhokbo_lex t_rest s_rest
          | Incomparable -> Incomparable
        end
    in
    let lfhokbo_lenlex ts ss =
      if List.length ts = List.length ss then
        lfhokbo_lex ts ss
      else (
        if List.length ts > List.length ss
        then Gt
        else Lt
      )
    in
    (* compare t = g tt and s = f ss (assuming they have the same weight) *)
    let lfhokbo_composite g f ts ss =
      match prec_compare prec g f with
        | Incomparable -> (* try rule C2 *)
          let hd_ts_s = lfhokbo_arg_coeff ~prec (List.hd ts) s in
          let hd_ss_t = lfhokbo_arg_coeff ~prec (List.hd ss) t in
          if List.length ts = 1 && (hd_ts_s = Gt || hd_ts_s = Eq) then Gt else
          if List.length ss = 1 && (hd_ss_t = Gt || hd_ss_t = Eq) then Lt else
            Incomparable
        | Gt -> Gt (* by rule C3 *)
        | Lt -> Lt (* by rule C3 inversed *)
        | Eq -> (* try rule C4 *)
          begin match prec_status prec g with
            | Prec.Lexicographic -> lfhokbo_lex ts ss
            | Prec.LengthLexicographic -> lfhokbo_lenlex ts ss
            | _ -> assert false
          end
    in
    (* compare t and s assuming they have the same weight *)
    let lfhokbo_same_weight t s =
      match T.view t, T.view s with
        | _ -> begin match Head.term_to_head t, Head.term_to_head s with
            | Some g, Some f -> lfhokbo_composite g f (Head.term_to_args t) (Head.term_to_args s)
            | _ -> Incomparable
          end
    in
    (
      if t = s then Eq else
        match weight prec t, weight prec s with
          | Some wt, Some ws ->
            if WP.compare wt ws > 0 then Gt (* by rule C1 *)
            else if WP.compare wt ws < 0 then Lt (* by rule C1 *)
            else if wt = ws then lfhokbo_same_weight t s (* try rules C2 - C4 *)
            else Incomparable (* Our approximation of comparing polynomials cannot
                                 determine the greater polynomial *)
          | _ -> Incomparable
    )

  let compare_terms ~prec x y =
    Util.enter_prof prof_lfhokbo_arg_coeff;
    let compare = lfhokbo_arg_coeff ~prec x y in
    Util.exit_prof prof_lfhokbo_arg_coeff;
    compare

  let might_flip prec t s =
    (* Terms can flip if they have different argument coefficients for remaining arguments. *)
    assert (Term.ty t = Term.ty s);
    let term_arity =
      match Type.arity (Term.ty t) with
        | Type.NoArity ->
          failwith (CCFormat.sprintf "term %a has ill-formed type %a" Term.pp t Type.pp (Term.ty t))
        | Type.Arity (_,n) -> n in
    let id_arity s =
      match Type.arity (Type.const s) with
        | Type.NoArity ->
          failwith (CCFormat.sprintf "symbol %a has ill-formed type %a" ID.pp s Type.pp (Type.const s))
        | Type.Arity (_,n) -> n in
    match Head.term_to_head t, Head.term_to_head s with
      | Some (Head.I g), Some (Head.I f) ->
        List.exists
          (fun i ->
             Prec.arg_coeff prec g (id_arity g - i) != Prec.arg_coeff prec f (id_arity f - i)
          )
          CCList.(0 --^ term_arity)
      | Some (Head.V _), Some (Head.I _) | Some (Head.I _), Some (Head.V _) -> true
      | _ -> assert false
end


(** Blanchette's lambda-free higher-order RPO.
    hopefully more efficient (polynomial) implementation of LPO,
    following the paper "things to know when implementing LPO" by Löchner.
    We adapt here the implementation clpo6 with some multiset symbols (=) *)
module RPO6 : ORD = struct
  let name = "rpo6"

  (** recursive path ordering *)
  let rec rpo6 ~prec s t =
    if T.equal s t then Eq else  (* equality test is cheap *)
      match T.view s, T.view t with
        | T.Var _, T.Var _ -> Incomparable
        | _, T.Var var -> if T.var_occurs ~var s then Gt else Incomparable
        | T.Var var, _ -> if T.var_occurs ~var t then Lt else Incomparable
        | T.DB _, T.DB _ -> Incomparable
        | _ ->
          begin match Head.term_to_head s, Head.term_to_head t with
            | Some head1, Some head2 ->
              rpo6_composite ~prec s t head1 head2 (Head.term_to_args s) (Head.term_to_args t)
            | _ -> Incomparable
          end
  (* handle the composite cases *)
  and rpo6_composite ~prec s t f g ss ts =
    begin match prec_compare prec f g  with
      | Eq ->
        begin match prec_status prec f with
          | Prec.Multiset ->  cMultiset ~prec s t ss ts
          | Prec.Lexicographic ->  cLMA ~prec s t ss ts
          | Prec.LengthLexicographic ->  cLLMA ~prec s t ss ts
        end
      | Gt -> cMA ~prec s ts
      | Lt -> Comparison.opp (cMA ~prec t ss)
      | Incomparable -> cAA ~prec s t ss ts
    end
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
    | [], _::_ -> Lt
    | _::_, [] -> Gt
  (** length-lexicographic comparison of s=f(ss), and t=f(ts) *)
  and cLLMA ~prec s t ss ts =
    if List.length ss = List.length ts then
      cLMA ~prec s t ss ts
    else if List.length ss > List.length ts then
      cMA ~prec s ts
    else
      Comparison.opp (cMA ~prec t ss)
  (** multiset comparison of subterms (not optimized) *)
  and cMultiset ~prec s t ss ts =
    match MT.compare_partial_l (rpo6 ~prec) ss ts with
      | Eq | Incomparable -> Incomparable
      | Gt -> cMA ~prec s ts
      | Lt -> Comparison.opp (cMA ~prec t ss)
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

  (* The ordering might flip if it is established using the subterm rule *)
  let might_flip prec t s =
    let c = rpo6 ~prec t s in
    c = Incomparable ||
    c = Gt && alpha ~prec (Head.term_to_args t) s = Gt ||
    c = Lt && alpha ~prec (Head.term_to_args s) t = Gt
end



(** {2 Value interface} *)

let kbo prec =
  let cache = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache
      (fun (a, b) -> KBO.compare_terms ~prec a b) (a,b)
  in
  { cache; compare; name=KBO.name; prec; might_flip=KBO.might_flip }

let lfhokbo_arg_coeff prec =
  let cache = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache
      (fun (a, b) -> LFHOKBO_arg_coeff.compare_terms ~prec a b) (a,b)
  in
  { cache; compare; name=LFHOKBO_arg_coeff.name; prec; might_flip=LFHOKBO_arg_coeff.might_flip}

let rpo6 prec =
  let cache = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache
      (fun (a, b) -> RPO6.compare_terms ~prec a b) (a,b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> RPO6.might_flip prec a b) (a,b)
  in
  { cache; compare; name=RPO6.name; prec; might_flip}

let dummy_cache_ = CCCache.dummy

let none =
  let compare _ t1 t2 = if T.equal t1 t2 then Eq else Incomparable in
  let might_flip _ _ _ = false in
  { cache=dummy_cache_; compare; prec=Prec.default []; name="none"; might_flip}

let subterm =
  let compare _ t1 t2 =
    if T.equal t1 t2 then Eq
    else if T.subterm ~sub:t1 t2 then Lt
    else if T.subterm ~sub:t2 t1 then Gt
    else Incomparable
  in
  let might_flip _ _ _ = false in
  { cache=dummy_cache_; compare; prec=Prec.default []; name="subterm"; might_flip}

(** {2 Global table of orders} *)

let tbl_ =
  let h = Hashtbl.create 5 in
  Hashtbl.add h "rpo6" rpo6;
  Hashtbl.add h "lfhokbo_arg_coeff" lfhokbo_arg_coeff;
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
