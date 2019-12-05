(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Term Orderings} *)

module Prec = Precedence
module MT = Multiset.Make(Term)
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
  cache_compare : (T.t * T.t, Comparison.t) CCCache.t;
  compare : Prec.t -> term -> term -> Comparison.t;
  prec : Prec.t;
  name : string;
  cache_might_flip : (T.t * T.t, bool) CCCache.t;
  might_flip : Prec.t -> term -> term -> bool;
} (** Partial ordering on terms *)

type ordering = t

let normalize = ref (fun t -> Lambda.eta_reduce t |> Lambda.snf)

let compare ord t1 t2 = 
  ord.compare ord.prec (!normalize t1) (!normalize t2)

let might_flip ord t1 t2 = ord.might_flip ord.prec (!normalize t1) (!normalize t2)

let precedence ord = ord.prec

let add_list ord l = Prec.add_list ord.prec l
let add_seq ord seq = Prec.add_seq ord.prec seq

let name ord = ord.name

let clear_cache ord = CCCache.clear ord.cache_compare;  CCCache.clear ord.cache_might_flip

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

module Head = struct
  type var = Type.t HVar.t

  module T = Term

  type t =
    | I of ID.t
    | B of Builtin.t
    | V of var
    | DB of int
    | LAM

  let pp out = function
    | I id -> ID.pp out id
    | B b -> Builtin.pp out b
    | V x -> HVar.pp out x
    | DB i -> CCInt.pp out i
    | LAM -> CCString.pp out "LAM"

  let rec term_to_head s =
    match T.view s with
    | T.App (f,_) ->          term_to_head f
    | T.AppBuiltin (fid,_) -> B fid
    | T.Const fid ->          I fid
    | T.Var x ->              V x
    | T.DB i ->               DB i
    | T.Fun _ ->              LAM

  let term_to_args s =
    match T.view s with
    | T.App (_,ss) -> ss
    | T.AppBuiltin (_,ss) -> ss
    (* The orderings treat lambda-expressions like a "LAM" symbol applied to the body of the lambda-expression *)
    | T.Fun (_,t) -> [t]
    | _ -> []

  let to_string = CCFormat.to_string pp
end

(** {3 Ordering implementations} *)

(* compare the two heads (ID or builtin or variable) using the precedence *)
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
  | Head.DB a, Head.DB b ->
    begin match CCInt.compare a b  with
      | 0 -> Eq
      | n when n > 0 -> Gt
      | _ -> Lt
    end
  | Head.LAM,  Head.LAM  -> Eq
  | Head.I _,  Head.B _  -> Gt (* lam > db > id > builtin *)
  | Head.B _,  Head.I _  -> Lt
  | Head.DB _, Head.I _  -> Gt
  | Head.I _,  Head.DB _ -> Lt
  | Head.DB _, Head.B _  -> Gt
  | Head.B _,  Head.DB _ -> Lt
  | Head.LAM,  Head.DB _ -> Gt
  | Head.DB _, Head.LAM  -> Lt
  | Head.LAM,  Head.I _  -> Gt
  | Head.I _,  Head.LAM  -> Lt
  | Head.LAM,  Head.B _  -> Gt
  | Head.B _,  Head.LAM  -> Lt

  | Head.V x,  Head.V y -> if x=y then Eq else Incomparable
  | Head.V _, _ -> Incomparable
  | _, Head.V _ -> Incomparable

let prec_status prec = function
  | Head.I s -> Prec.status prec s
  | Head.B Builtin.Eq -> Prec.Multiset
  | _ -> Prec.LengthLexicographic

module KBO : ORD = struct
  let name = "kbo"

  (** used to keep track of the balance of variables *)
  type var_balance = {
    offset : int;
    mutable pos_counter : int;
    mutable neg_counter : int;
    mutable balance : CCInt.t Term.Tbl.t;
  }

  (** create a balance for the two terms *)
  let mk_balance t1 t2 =
    let numvars = Iter.length (T.Seq.vars t1) + Iter.length (T.Seq.vars t2) in
    { offset = 0; pos_counter = 0; neg_counter = 0; balance = Term.Tbl.create numvars; }

  (** add a positive variable *)
  let add_pos_var balance var =
    let n = Term.Tbl.get_or balance.balance var ~default:0 in
    if n = 0
    then balance.pos_counter <- balance.pos_counter + 1
    else (
      if n = -1 then balance.neg_counter <- balance.neg_counter - 1
    );
    Term.Tbl.add balance.balance var (n + 1)

  (** add a negative variable *)
  let add_neg_var balance var =
    let n = Term.Tbl.get_or balance.balance var ~default:0 in
    if n = 0
    then balance.neg_counter <- balance.neg_counter + 1
    else (
      if n = 1 then balance.pos_counter <- balance.pos_counter - 1
    );
    Term.Tbl.add balance.balance var (n - 1)

  let weight_var_headed = W.one

  let weight prec = function
    | Head.B _ -> W.one
    | Head.I s -> Prec.weight prec s
    | Head.V _ -> weight_var_headed
    | Head.DB _ -> Prec.db_weight prec
    | Head.LAM ->  Prec.lam_weight prec

  (** Higher-order KBO *)
  let rec kbo ~prec t1 t2 =
    let balance = mk_balance t1 t2 in
    (** Update variable balance, weight balance, and check whether the term contains the variable-headed term s.
        @param pos stands for positive (is t the left term?)
        @return weight balance, was `s` found?
    *)
    let rec balance_weight (wb:W.t) t s ~pos : W.t * bool =
      match T.view t with
      | T.Var _ ->
        balance_weight_var wb t s ~pos
      | T.App (f, _) when (T.is_var f) ->
        balance_weight_var wb t s ~pos
      | T.DB i ->
        let wb' =
          if pos
          then W.(wb + weight prec (Head.DB i))
          else W.(wb - weight prec (Head.DB i)) in
        wb', false
      | T.Const c ->
        let open W.Infix in
        let wb' =
          if pos
          then wb + weight prec (Head.I c)
          else wb - weight prec (Head.I c)
        in wb', false
      | T.App (f, l) ->
        let wb', res = balance_weight wb f s ~pos in
        balance_weight_rec wb' l s ~pos res
      | T.AppBuiltin (b,l) ->
        let open W.Infix in
        let wb' = if pos
          then wb + weight prec (Head.B b)
          else wb - weight prec (Head.B b)
        in
        balance_weight_rec wb' l s ~pos false
      | T.Fun (_, body) ->
        let open W.Infix in
        let wb' =
          if pos
          then wb + weight prec Head.LAM
          else wb - weight prec Head.LAM
        in
        balance_weight wb' body s ~pos
    (** balance_weight for the case where t is an applied variable *)
    and balance_weight_var (wb:W.t) t s ~pos : W.t * bool =
      if pos then (
        add_pos_var balance t;
        W.(wb + weight_var_headed), CCOpt.is_some s && ( Term.equal (CCOpt.get_exn s) t)
      ) else (
        add_neg_var balance t;
        W.(wb - weight_var_headed), CCOpt.is_some s && ( Term.equal (CCOpt.get_exn s) t)
      )
    (** list version of the previous one, threaded with the check result *)
    and balance_weight_rec wb terms s ~pos res = match terms with
      | [] -> (wb, res)
      | t::terms' ->
        let wb', res' = balance_weight wb t s ~pos in
        balance_weight_rec wb' terms' s ~pos (res || res')
    (** lexicographic comparison *)
    and tckbolex wb terms1 terms2 =
      match terms1, terms2 with
      | [], [] -> wb, Eq
      | t1::terms1', t2::terms2' ->
        begin match tckbo wb t1 t2 with
          | (wb', Eq) -> tckbolex wb' terms1' terms2'
          | (wb', res) -> (* just compute the weights and return result *)
            let wb'', _ = balance_weight_rec wb' terms1' None ~pos:true false in
            let wb''', _ = balance_weight_rec wb'' terms2' None ~pos:false false in
            wb''', res
        end
      | [], _ ->
        let wb, _ = balance_weight_rec wb terms2 None ~pos:false false in
        wb, Lt
      | _, [] ->
        let wb, _ = balance_weight_rec wb terms1 None ~pos:true false in
        wb, Gt
    (* length-lexicographic comparison *)
    and tckbolenlex wb terms1 terms2 =
      if List.length terms1 = List.length terms2
      then tckbolex wb terms1 terms2
      else (
        (* just compute the weights and return result *)
        let wb', _ = balance_weight_rec wb terms1 None ~pos:true false in
        let wb'', _ = balance_weight_rec wb' terms2 None ~pos:false false in
        let res = if List.length terms1 > List.length terms2 then Gt else Lt in
        wb'', res
      )
    (* commutative comparison. Not linear, must call kbo to
       avoid breaking the weight computing invariants *)
    and tckbocommute wb ss ts =
      (* multiset comparison *)
      let res = MT.compare_partial_l (kbo ~prec) ss ts in
      (* also compute weights of subterms *)
      let wb', _ = balance_weight_rec wb ss None ~pos:true false in
      let wb'', _ = balance_weight_rec wb' ts None ~pos:false false in
      wb'', res
    (* tupled version of kbo (kbo_5 of the paper) *)
    and tckbo (wb:W.t) t1 t2 =
      if T.equal t1 t2
      then (wb, Eq) (* do not update weight or var balance *)
      else
        match Head.term_to_head t1, Head.term_to_head t2 with
        | Head.V _, Head.V _ ->
          add_pos_var balance t1;
          add_neg_var balance t2;
          (wb, Incomparable)
        | Head.V _,  _ ->
          add_pos_var balance t1;
          let wb', contains = balance_weight wb t2 (Some t1) ~pos:false in
          (W.(wb' + weight_var_headed), if contains then Lt else Incomparable)
        |  _, Head.V _ ->
          add_neg_var balance t2;
          let wb', contains = balance_weight wb t1 (Some t2) ~pos:true in
          (W.(wb' - weight_var_headed), if contains then Gt else Incomparable)
        | h1, h2 -> tckbo_composite wb h1 h2 (Head.term_to_args t1) (Head.term_to_args t2)
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
    (* recursive comparison *)
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
        let wb', _ = balance_weight_rec wb ss None ~pos:true false in
        let wb'', _ = balance_weight_rec wb' ts None ~pos:false false in
        wb'', Incomparable
      )
    in
    let _, res = tckbo W.zero t1 t2 in
    res

  let compare_terms ~prec x y =
    Util.enter_prof prof_kbo;
    let compare = kbo ~prec x y in
    Util.exit_prof prof_kbo;
    compare

  (* The ordering might flip if one side is a lambda-expression *)
  let might_flip _ s t = T.is_fun s || T.is_fun t
end

(** Lambda-free higher-order RPO.
    hopefully more efficient (polynomial) implementation of LPO,
    following the paper "things to know when implementing LPO" by Löchner.
    We adapt here the implementation clpo6 with some multiset symbols (=) *)
module RPO6 : ORD = struct
  let name = "rpo6"

  (* recursive path ordering *)
  let rec rpo6 ~prec s t =
    if T.equal s t then Eq else  (* equality test is cheap *)
      match Head.term_to_head s, Head.term_to_head t with
      | Head.V _, Head.V _ -> Incomparable
      | _, Head.V _  -> if has_varheaded_subterm s t then Gt else Incomparable
      | Head.V _, _ -> if has_varheaded_subterm t s then Lt else Incomparable
      | h1, h2 -> rpo6_composite ~prec s t h1 h2 (Head.term_to_args s) (Head.term_to_args t)
  and has_varheaded_subterm t sub =
    T.equal t sub ||
    match T.view t with
    | T.Var _ | T.DB _ | T.Const _ -> false
    | T.App (f, _) when T.is_var f -> false
    | T.App (_, l) -> List.exists (fun t -> has_varheaded_subterm t sub) l
    | T.Fun (_, t') -> has_varheaded_subterm t' sub
    | T.AppBuiltin (_, l) -> List.exists (fun t -> has_varheaded_subterm t sub) l
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
  (* try to dominate all the terms in ts by s; but by subterm property
     if some t' in ts is >= s then s < t=g(ts) *)
  and cMA ~prec s ts = match ts with
    | [] -> Gt
    | t::ts' ->
      (match rpo6 ~prec s t with
       | Gt -> cMA ~prec s ts'
       | Eq | Lt -> Lt
       | Incomparable -> Comparison.opp (alpha ~prec ts' s))
  (* lexicographic comparison of s=f(ss), and t=f(ts) *)
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
  (* length-lexicographic comparison of s=f(ss), and t=f(ts) *)
  and cLLMA ~prec s t ss ts =
    if List.length ss = List.length ts then
      cLMA ~prec s t ss ts
    else if List.length ss > List.length ts then
      cMA ~prec s ts
    else
      Comparison.opp (cMA ~prec t ss)
  (* multiset comparison of subterms (not optimized) *)
  and cMultiset ~prec s t ss ts =
    match MT.compare_partial_l (rpo6 ~prec) ss ts with
    | Eq | Incomparable -> Incomparable
    | Gt -> cMA ~prec s ts
    | Lt -> Comparison.opp (cMA ~prec t ss)
  (* bidirectional comparison by subterm property (bidirectional alpha) *)
  and cAA ~prec s t ss ts =
    match alpha ~prec ss t with
    | Gt -> Gt
    | Incomparable -> Comparison.opp (alpha ~prec ts s)
    | _ -> assert false
  (* if some s in ss is >= t, then s > t by subterm property and transitivity *)
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

  (* The ordering might flip if one side is a lambda-expression or if the order is established using the subterm rule *)
  let might_flip prec t s =
    T.is_fun t || T.is_fun s ||
    let c = rpo6 ~prec t s in
    c = Incomparable ||
    c = Gt && alpha ~prec (Head.term_to_args t) s = Gt ||
    c = Lt && alpha ~prec (Head.term_to_args s) t = Gt
end



(** {2 Value interface} *)

let kbo prec =
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> KBO.compare_terms ~prec a b) (a,b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> KBO.might_flip prec a b) (a,b)
  in
  { cache_compare; compare; name=KBO.name; prec; might_flip; cache_might_flip}

let rpo6 prec =
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> RPO6.compare_terms ~prec a b) (a,b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> RPO6.might_flip prec a b) (a,b)
  in
  { cache_compare; compare; name=RPO6.name; prec; might_flip; cache_might_flip}

let dummy_cache_ = CCCache.dummy

let none =
  let compare _ t1 t2 = if T.equal t1 t2 then Eq else Incomparable in
  let might_flip _ _ _ = false in
  { cache_compare=dummy_cache_; compare; prec=Prec.default []; name="none"; might_flip; cache_might_flip=dummy_cache_}

let subterm =
  let compare _ t1 t2 =
    if T.equal t1 t2 then Eq
    else if T.subterm ~sub:t1 t2 then Lt
    else if T.subterm ~sub:t2 t1 then Gt
    else Incomparable
  in
  let might_flip _ _ _ = false in
  { cache_compare=dummy_cache_; compare; prec=Prec.default []; name="subterm"; might_flip; cache_might_flip=dummy_cache_ }

let map f { cache_compare=_; compare; prec; name; might_flip; cache_might_flip=_ } =
  let cache_compare = mk_cache 256 in
  let cache_might_flip = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare (fun (a, b) -> compare prec (f a) (f b)) (a,b) in
  let might_flip prec a b = CCCache.with_cache cache_might_flip (fun (a, b) -> might_flip prec (f a) (f b)) (a,b) in
  { cache_compare; compare; prec; name; might_flip; cache_might_flip }

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
