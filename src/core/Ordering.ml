(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Term Orderings} *)

module Prec = Precedence
module MT = Multiset.Make(Term)
module W = Precedence.Weight

open Comparison

let prof_rpo = ZProf.make "compare_rpo"
let prof_kbo = ZProf.make "compare_kbo"
let prof_epo = ZProf.make "compare_epo"
let prof_lambdafree_kbo_coeff = ZProf.make "compare_lambdafree_kbo_coeff"

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
  monotonic : bool;
} (** Partial ordering on terms *)

type ordering = t

let compare ord t1 t2 = 
  ord.compare ord.prec t1 t2

let might_flip ord t1 t2 = ord.might_flip ord.prec t1 t2

let monotonic ord = ord.monotonic

let precedence ord = ord.prec

let add_list ~signature ord l = Prec.add_list ~signature ord.prec l

let name ord = ord.name

let clear_cache ord = CCCache.clear ord.cache_compare;  CCCache.clear ord.cache_might_flip

let pp out ord =
  Format.fprintf out "%s(@[%a@])" ord.name Prec.pp ord.prec

let to_string ord = CCFormat.to_string pp ord

(* Type-1 combinator is a combinator that is not ground
    (see Ahmed's combinator KBO paper) *)
let ty1comb_to_var t balance =
  if T.is_comb t && not (T.is_ground t) then (
    match T.Tbl.find_opt balance t with
    | Some t' -> t'
    | None ->
        let fresh_var = T.var (HVar.fresh ~ty:(T.ty t) ()) in
        T.Tbl.add balance t fresh_var;
        fresh_var
  ) else t

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

  type quantifier = Forall | Exists

  type t =
    | I of ID.t
    | B of Builtin.t
    | Q of quantifier
    | V of var
    | DB of int
    | LAM

  let pp out = function
    | I id -> ID.pp out id
    | B b -> Builtin.pp out b
    | Q Forall -> CCString.pp out "FORALL"
    | Q Exists -> CCString.pp out "EXISTS"
    | V x -> HVar.pp out x
    | DB i -> CCInt.pp out i
    | LAM -> CCString.pp out "LAM"

  let rec term_to_head s =
    match T.view s with
    | T.App (f,_) ->          term_to_head f
    (* The head Q is only used if there are two arguments, the type argument and the lambda-expression *)
    | T.AppBuiltin (Builtin.ForallConst,[_;_]) -> Q Forall
    | T.AppBuiltin (Builtin.ExistsConst,[_;_]) -> Q Exists
    | T.AppBuiltin (fid,_) -> B fid
    | T.Const fid ->          I fid
    | T.Var x ->              V x
    | T.DB i ->               DB i
    | T.Fun _ ->              LAM

  let term_to_args s =
    match T.view s with
    | T.App (_,ss) -> ss
    | T.AppBuiltin ((Builtin.ForallConst|Builtin.ExistsConst),[ty;lam]) ->
      (* Under quantifiers, we ignore the lambda *)
      begin match T.view lam with 
      | T.Fun (ty', body) -> [ty; body]
      | _ -> [ty; lam]
      end
    | T.AppBuiltin (_,ss) -> ss
    (* The orderings treat lambda-expressions like a "LAM" symbol applied to the body of the lambda-expression *)
    | T.Fun (ty,t) -> [T.of_ty ty; t]
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
  | Head.Q Forall,  Head.Q Forall -> Eq
  | Head.Q Exists,  Head.Q Exists -> Eq
  | Head.Q Forall,  Head.Q Exists -> Gt
  | Head.Q Exists,  Head.Q Forall -> Lt

  (* lam > db > quantifier > symbols > bot > top *)
  | Head.I _,  Head.B _  -> Gt
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
  | Head.Q _,  Head.LAM  -> Lt
  | Head.Q _,  Head.DB _ -> Lt
  | Head.Q _,  Head.I _  -> Gt
  | Head.Q _,  Head.B _  -> Gt
  | Head.LAM,  Head.Q _  -> Gt
  | Head.DB _, Head.Q _  -> Gt
  | Head.I _,  Head.Q _  -> Lt
  | Head.B _,  Head.Q _  -> Lt

  | Head.V x,  Head.V y -> if HVar.equal Type.equal x y then Eq else Incomparable
  | Head.V _, _ -> Incomparable
  | _, Head.V _ -> Incomparable

let prec_status prec = function
  | Head.I s -> Prec.status prec s
  | _ -> Prec.LengthLexicographic

module type PARAMETERS =
sig
  val name : string
  val lambda_mode : bool
  val ignore_deep_quants : bool
end


module TermBoolAsKey = struct
  type t = term * bool
  let equal (t1, b1) (t2, b2) = T.equal t1 t2 && CCBool.equal b1 b2
  let hash (t, b) = Hash.combine2 (T.hash t) (Hash.bool b)
  let compare (t1, b1) (t2, b2) = if CCBool.equal b1 b2 then T.compare t1 t2 else CCBool.compare b1 b2
end

module TermBoolTbl = CCHashtbl.Make(TermBoolAsKey)

module MakeKBO (P : PARAMETERS) : ORD = struct
  let name = P.name

  (** used to keep track of the balance of variables *)
  type var_balance = {
    offset : int;
    mutable pos_counter : int;
    mutable neg_counter : int;
    mutable balance : CCInt.t TermBoolTbl.t;
    mutable comb2var : T.t Term.Tbl.t
  }

  (** create a balance for the two terms *)
  let mk_balance t1 t2 =
    let numvars = Iter.length (T.Seq.vars t1) + Iter.length (T.Seq.vars t2) in
    { offset = 0; pos_counter = 0; neg_counter = 0; balance = TermBoolTbl.create (2 * numvars);
      comb2var = Term.Tbl.create 16 }

  (** add a positive variable *)
  let add_pos_var balance var ~below_lam =
    let n = TermBoolTbl.get_or balance.balance (var, below_lam) ~default:0 in
    if n = 0
    then balance.pos_counter <- balance.pos_counter + 1
    else (
      if n = -1 then balance.neg_counter <- balance.neg_counter - 1
    );
    TermBoolTbl.add balance.balance (var, below_lam) (n + 1)

  (** add a negative variable *)
  let add_neg_var balance var ~below_lam =
    let n = TermBoolTbl.get_or balance.balance (var, below_lam) ~default:0 in
    if n = 0
    then balance.neg_counter <- balance.neg_counter + 1
    else (
      if n = 1 then balance.pos_counter <- balance.pos_counter - 1
    );
    TermBoolTbl.add balance.balance (var, below_lam) (n - 1)

  let weight_var_headed = W.one

  let weight prec ~below_lam = function
    | Head.Q _ -> if P.ignore_deep_quants && below_lam then W.one else  W.omega
    | Head.B _ -> W.one
    | Head.I s -> Prec.weight prec s
    | Head.V _ -> weight_var_headed
    | Head.DB _ -> Prec.db_weight prec
    | Head.LAM ->  Prec.lam_weight prec

  (* Overapproximation of being fluid *)
  let is_fluid t = match T.view t with
    | T.App (f, l) -> T.is_var f
    | T.Fun (ty, body) -> not (T.is_ground body)
    | _ -> false

  (** Higher-order KBO *)
  exception UnsupportedTerm
  let rec kbo ~prec t1 t2 =
    let balance = mk_balance t1 t2 in
    (** Update variable balance, weight balance, and check whether the term contains the fluid term s.
        @param pos stands for positive (is t the left term?)
        @return weight balance, was `s` found?
    *)
    let rec balance_weight (wb:W.t) t s ~pos ~below_lam : W.t * bool =
      let t = ty1comb_to_var t balance.comb2var in
      if T.is_var t || (P.lambda_mode && is_fluid t) then 
        balance_weight_var wb t s ~pos ~below_lam
      else (
        match Head.term_to_head t, Head.term_to_args t with
        | Head.V v, args -> 
          let wb', res = balance_weight_var wb (T.var v) s ~pos ~below_lam in
          balance_weight_rec wb' args s ~pos ~below_lam res
        | h, args ->
          let wb' =
            if pos
            then W.(wb + weight prec h ~below_lam)
            else W.(wb - weight prec h ~below_lam)
          in
          let below_lam = (h = Head.LAM) in
          if not P.lambda_mode && below_lam then raise UnsupportedTerm;
          balance_weight_rec wb' args s ~pos ~below_lam false
      )
    (** balance_weight for the case where t is an applied variable *)
    and balance_weight_var (wb:W.t) t s ~pos ~below_lam : W.t * bool =
    (* TODO: count below and above lam separately *)
      if pos then (
        add_pos_var balance t ~below_lam;
        W.(wb + weight_var_headed), CCOpt.is_some s && (Term.equal (CCOpt.get_exn s) t)
      ) else (
        add_neg_var balance t ~below_lam;
        W.(wb - weight_var_headed), CCOpt.is_some s && (Term.equal (CCOpt.get_exn s) t)
      )
    (** list version of the previous one, threaded with the check result *)
    and balance_weight_rec wb terms s ~pos ~below_lam res = match terms with
      | [] -> (wb, res)
      | t::terms' ->
        let wb', res' = balance_weight wb t s ~pos ~below_lam in
        balance_weight_rec wb' terms' s ~pos ~below_lam (res || res')
    (** lexicographic comparison *)
    and tckbolex wb terms1 terms2 ~below_lam =
      match terms1, terms2 with
      | [], [] -> wb, Eq
      | t1::terms1', t2::terms2' ->
        begin match tckbo wb t1 t2 ~below_lam with
          | (wb', Eq) -> tckbolex wb' terms1' terms2' ~below_lam
          | (wb', res) -> (* just compute the weights and return result *)
            let wb'', _ = balance_weight_rec wb' terms1' None ~pos:true ~below_lam false in
            let wb''', _ = balance_weight_rec wb'' terms2' None ~pos:false ~below_lam false in
            wb''', res
        end
      | [], _ ->
        let wb, _ = balance_weight_rec wb terms2 None ~pos:false ~below_lam false in
        wb, Lt
      | _, [] ->
        let wb, _ = balance_weight_rec wb terms1 None ~pos:true ~below_lam false in
        wb, Gt
    (* length-lexicographic comparison *)
    and tckbolenlex wb terms1 terms2 ~below_lam =
      if List.length terms1 = List.length terms2
      then tckbolex wb terms1 terms2 ~below_lam
      else (
        (* just compute the weights and return result *)
        let wb', _ = balance_weight_rec wb terms1 None ~pos:true ~below_lam false in
        let wb'', _ = balance_weight_rec wb' terms2 None ~pos:false ~below_lam false in
        let res = if List.length terms1 > List.length terms2 then Gt else Lt in
        wb'', res
      )
    (* tupled version of kbo (kbo_5 of the paper) *)
    and tckbo (wb:W.t) t1 t2 ~below_lam =
      if T.equal t1 t2
      then (wb, Eq) (* do not update weight or var balance *)
      else
      if P.lambda_mode 
      then (
        match Head.term_to_head t1, Head.term_to_head t2 with
        | Head.V _, Head.V _ ->
          add_pos_var balance t1 ~below_lam;
          add_neg_var balance t2 ~below_lam;
          (wb, Incomparable)
        | Head.V _,  _ ->
          add_pos_var balance t1 ~below_lam;
          let wb', contains = balance_weight wb t2 (Some t1) ~pos:false ~below_lam in
          (W.(wb' + weight_var_headed), if contains then Lt else Incomparable)
        |  _, Head.V _ ->
          add_neg_var balance t2 ~below_lam;
          let wb', contains = balance_weight wb t1 (Some t2) ~pos:true ~below_lam in
          (W.(wb' - weight_var_headed), if contains then Gt else Incomparable)
        | h1, h2 -> tckbo_composite wb h1 h2 (Head.term_to_args t1) (Head.term_to_args t2) ~below_lam
      ) 
      else (
        let t1 = ty1comb_to_var t1 balance.comb2var in 
        let t2 = ty1comb_to_var t2 balance.comb2var in
        match T.view t1, T.view t2 with
        | T.Var x, T.Var y ->
          add_pos_var balance t1 ~below_lam;
          add_neg_var balance t2 ~below_lam;
          (wb, Incomparable)
        | T.Var x,  _ ->
          add_pos_var balance t1 ~below_lam;
          let wb', contains = balance_weight wb t2 (Some t1) ~pos:false ~below_lam in
          (W.(wb' + one), if contains then Lt else Incomparable)
        |  _, T.Var y ->
          add_neg_var balance t2 ~below_lam;
          let wb', contains = balance_weight wb t1 (Some t2) ~pos:true ~below_lam in
          (W.(wb' - one), if contains then Gt else Incomparable)
        | _ -> let f, g = Head.term_to_head t1, Head.term_to_head t2 in
          tckbo_composite wb f g (Head.term_to_args t1) (Head.term_to_args t2) ~below_lam
      )
    (** tckbo, for non-variable-headed terms). *)
    and tckbo_composite wb f g ss ts ~below_lam =
      (* do the recursive computation of kbo *)
      let wb', res = tckbo_rec wb f g ss ts ~below_lam in
      let wb'' = W.(wb' + weight prec f ~below_lam - weight prec g ~below_lam) in
      if not P.lambda_mode then (
        begin match f with
          | Head.V x -> add_pos_var balance (T.var x) ~below_lam
          | _ -> ()
        end;
        begin match g with
          | Head.V x -> add_neg_var balance (T.var x) ~below_lam
          | _ -> ()
        end;
      );
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
    and tckbo_rec wb f g ss ts ~below_lam =
      let ss_below_lam = below_lam || f = Head.LAM in
      let ts_below_lam = below_lam || g = Head.LAM in
      if prec_compare prec f g = Eq
      then match prec_status prec f with
        | Prec.Multiset -> assert false
        | Prec.Lexicographic ->
          tckbolex wb ss ts ~below_lam:ss_below_lam
        | Prec.LengthLexicographic ->
          tckbolenlex wb ss ts ~below_lam:ss_below_lam
      else (
        (* just compute variable and weight balances *)
        let wb', _ = balance_weight_rec wb ss None ~pos:true ~below_lam:ss_below_lam false in
        let wb'', _ = balance_weight_rec wb' ts None ~pos:false ~below_lam:ts_below_lam false in
        wb'', Incomparable
      )
    in
    let _, res = tckbo W.zero t1 t2 ~below_lam:false in
    res

  let compare_terms ~prec x y =
    let _span = ZProf.enter_prof prof_kbo in
    let res = 
      (try 
        kbo ~prec x y
      with UnsupportedTerm -> Incomparable) in
    ZProf.exit_prof _span;
    res

  (* The ordering might flip if one side is a lambda-expression *)
  let might_flip _ s t = T.is_fun s || T.is_fun t
end

(** Lambda-free higher-order RPO.
    hopefully more efficient (polynomial) implementation of LPO,
    following the paper "things to know when implementing LPO" by Löchner.
    We adapt here the implementation clpo6 with some multiset symbols (=) *)
module MakeRPO (P : PARAMETERS) : ORD = struct
  let name = P.name

  (* recursive path ordering *)
  let rec rpo6 ~prec s t =
    if T.equal s t then Eq else (  (* equality test is cheap *)
      if P.lambda_mode then (
        match Head.term_to_head s, Head.term_to_head t with
        | Head.V _, Head.V _ -> Incomparable
        | _, Head.V _  -> if has_subterm s t then Gt else Incomparable
        | Head.V _, _ -> if has_subterm t s then Lt else Incomparable
        | h1, h2 -> rpo6_composite ~prec s t h1 h2 (Head.term_to_args s) (Head.term_to_args t)
      ) else (
        match T.view s, T.view t with
        | T.Var _, T.Var _ -> Incomparable
        | _, T.Var var -> if T.var_occurs ~var s then Gt else Incomparable
        | T.Var var, _ -> if T.var_occurs ~var t then Lt else Incomparable
        | _ ->
          let h1, h2 = Head.term_to_head s, Head.term_to_head t in
          rpo6_composite ~prec s t h1 h2 (Head.term_to_args s) (Head.term_to_args t)
      )
    )
  and has_subterm t sub = 
    T.Seq.subterms ~ignore_head:true ~include_builtin:true ~include_app_vars:false t 
    |> Iter.mem ~eq:T.equal sub
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
    let _span = ZProf.enter_prof prof_rpo in
    let compare = rpo6 ~prec x y in
    ZProf.exit_prof _span;
    compare

  (* The ordering might flip if one side is a lambda-expression or if the order is established using the subterm rule *)
  let might_flip prec t s =
    T.is_fun t || T.is_fun s ||
    let c = rpo6 ~prec t s in
    c = Incomparable ||
    c = Gt && alpha ~prec (Head.term_to_args t) s = Gt ||
    c = Lt && alpha ~prec (Head.term_to_args s) t = Gt
end

module EPO : ORD = struct
  let name = "epo"

  let rec epo ~prec (t,tt) (s,ss) = CCCache.with_cache _cache (fun ((t,tt), (s,ss)) -> epo_behind_cache ~prec (t,tt) (s,ss)) ((t,tt),(s,ss))
  and _cache = 
    let hash ((b,bb),(a,aa)) = Hash.combine4 (T.hash b) (T.hash a) (Hash.list T.hash bb) (Hash.list T.hash aa)in
    CCCache.replacing
      ~eq:(fun ((b1,bb1),(a1,aa1)) ((b2,bb2),(a2,aa2)) -> 
          T.equal b1 b2 && T.equal a1 a2 
          && CCList.equal T.equal bb1 bb2
          && CCList.equal T.equal aa1 aa2) 
      ~hash
      512
  and epo_behind_cache ~prec (t,tt) (s,ss) = 
    if T.equal t s && CCList.length tt = CCList.length ss && CCList.for_all2 T.equal tt ss 
    then Eq 
    else 
      begin match (T.view t,tt), (T.view s,ss) with
        | (T.Var _, []), (T.Var _, []) -> Incomparable
        | _, (T.Var var, []) -> 
          if T.var_occurs ~var t || CCList.exists (T.var_occurs ~var) tt then Gt else Incomparable
        | (T.Var var, []), _ -> 
          if T.var_occurs ~var s || CCList.exists (T.var_occurs ~var) ss then Lt else Incomparable
        | _ ->
          begin match Head.term_to_head t, Head.term_to_head s with
            | g, f ->
              epo_composite ~prec (t,tt) (s,ss) (g, Head.term_to_args t @ tt)  (f, Head.term_to_args s @ ss)
          end
      end
  and epo_composite ~prec (t,tt) (s,ss) (g,gg) (f,ff) =
    begin match prec_compare prec g f  with
      | Gt -> epo_check_e2_e3     ~prec (t,tt) (s,ss) (g,gg) (f,ff)
      | Lt -> epo_check_e2_e3_inv ~prec (t,tt) (s,ss) (g,gg) (f,ff)
      | Eq ->
        let c = match prec_status prec g with
          | Prec.Multiset -> assert false
          | Prec.Lexicographic -> epo_lex ~prec gg ff
          | Prec.LengthLexicographic ->  epo_llex ~prec gg ff
        in
        begin match g with
          | Head.V _ -> 
            if c = Gt then epo_check_e4     ~prec (t,tt) (s,ss) (g,gg) (f,ff) else
            if c = Lt then epo_check_e4_inv ~prec (t,tt) (s,ss) (g,gg) (f,ff) else
              epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
          | _ -> 
            if c = Gt then epo_check_e2_e3     ~prec (t,tt) (s,ss) (g,gg) (f,ff) else
            if c = Lt then epo_check_e2_e3_inv ~prec (t,tt) (s,ss) (g,gg) (f,ff) else
              epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
        end
      | Incomparable -> epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
    end
  and epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff) =
    if gg != [] && (let c = epo ~prec (s,ss) (chop (g,gg)) in c = Lt || c = Eq) then Gt else
    if ff != [] && (let c = epo ~prec (t,tt) (chop (f,ff)) in c = Lt || c = Eq) then Lt else
      Incomparable
  and epo_check_e2_e3 ~prec (t,tt) (s,ss) (g,gg) (f,ff) = 
    if ff = [] || epo ~prec (t,tt) (chop (f,ff)) = Gt  then Gt else 
      epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
  and epo_check_e2_e3_inv ~prec (t,tt) (s,ss) (g,gg) (f,ff) = 
    if gg = [] || epo ~prec (chop (g,gg)) (s, ss) = Lt then Lt else 
      epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
  and epo_check_e4 ~prec (t,tt) (s,ss) (g,gg) (f,ff) = 
    if ff = [] || epo ~prec (chop (g,gg)) (chop (f,ff)) = Gt then Gt else 
      epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
  and epo_check_e4_inv ~prec (t,tt) (s,ss) (g,gg) (f,ff) = 
    if gg = [] || epo ~prec (chop (g,gg)) (chop (f,ff)) = Lt then Lt else 
      epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
  and chop (f,ff) = (List.hd ff, List.tl ff)
  and epo_llex ~prec gg ff =
    let m, n = (List.length gg), (List.length ff) in
    if m < n then Lt else
    if m > n then Gt else
      epo_lex ~prec gg ff
  and epo_lex ~prec gg ff =
    match gg, ff with
    | [], [] -> Eq
    | (gg_hd :: gg_tl), (ff_hd :: ff_tl) -> 
      let c = epo ~prec (gg_hd,[]) (ff_hd,[]) in
      if c = Eq 
      then epo_lex ~prec gg_tl ff_tl
      else c
    | (_ :: _), [] -> Gt
    | [], (_ :: _) -> Lt

  let compare_terms ~prec x y = 
    let _span = ZProf.enter_prof prof_epo in
    let compare = epo ~prec (x,[]) (y,[]) in
    ZProf.exit_prof _span;
    compare

  let might_flip _ _ _ = false
end

(** Lambda-free KBO with argument coefficients (quite slow) *)
module LambdaFreeKBOCoeff : ORD = struct
  let name = "lambdafree_kbo_coeff"


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
        | _ -> let g, f = Head.term_to_head t, Head.term_to_head s in
          lfhokbo_composite g f (Head.term_to_args t) (Head.term_to_args s)
    in
    (
      if T.equal t s then Eq else
        match weight prec t, weight prec s with
          | Some wt, Some ws ->
            if WP.compare wt ws > 0 then Gt (* by rule C1 *)
            else if WP.compare wt ws < 0 then Lt (* by rule C1 *)
            else if WP.equal wt ws then lfhokbo_same_weight t s (* try rules C2 - C4 *)
            else Incomparable (* Our approximation of comparing polynomials cannot
                                 determine the greater polynomial *)
          | _ -> Incomparable
    )

  let compare_terms ~prec x y =
    let _span = ZProf.enter_prof prof_lambdafree_kbo_coeff in
    let compare = lfhokbo_arg_coeff ~prec x y in
    ZProf.exit_prof _span;
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
      | Head.I g, Head.I f ->
        List.exists
          (fun i ->
             Prec.arg_coeff prec g (id_arity g - i) != Prec.arg_coeff prec f (id_arity f - i)
          )
          CCList.(0 --^ term_arity)
      | Head.V _, Head.I _ | Head.I _, Head.V _ -> true
      | _ -> assert false
end

(** {2 Value interface} *)

let dummy_cache_ = CCCache.dummy

let map f { cache_compare=_; compare; prec; name; might_flip; cache_might_flip=_; monotonic } =
  let cache_compare = mk_cache 256 in
  let cache_might_flip = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare (fun (a, b) -> compare prec (f a) (f b)) (a,b) in
  let might_flip prec a b = CCCache.with_cache cache_might_flip (fun (a, b) -> might_flip prec (f a) (f b)) (a,b) in
  { cache_compare; compare; prec; name; might_flip; cache_might_flip; monotonic }

let lambda_kbo ~ignore_quans_under_lam prec =
  let module KBO = MakeKBO(struct 
      let name = "lambda_kbo"
      let lambda_mode = true
      let ignore_deep_quants = ignore_quans_under_lam
    end) in
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> 
        KBO.compare_terms ~prec a b) (a,b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> KBO.might_flip prec a b) (a,b)
  in
  let normalize t = Lambda.eta_reduce t |> Lambda.snf in
  let monotonic = false in
  map normalize { cache_compare; compare; name=KBO.name; prec; might_flip; cache_might_flip; monotonic }

let lambdafree_kbo prec =
  let module KBO = MakeKBO(struct 
      let name = "lambdafree_kbo"
      let lambda_mode = false
      let ignore_deep_quants = true
    end) in
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> KBO.compare_terms ~prec a b) (a,b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> KBO.might_flip prec a b) (a,b)
  in
  let monotonic = true in
  { cache_compare; compare; name=KBO.name; prec; might_flip; cache_might_flip; monotonic }

let lambdafree_rpo prec =
  let module RPO = MakeRPO(struct 
      let name = "lambdafree_rpo"
      let lambda_mode = false
      let ignore_deep_quants = true
    end) in
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> RPO.compare_terms ~prec a b) (a,b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> RPO.might_flip prec a b) (a,b)
  in
  let monotonic = false in
  { cache_compare; compare; name=RPO.name; prec; might_flip; cache_might_flip; monotonic }

let lambda_rpo prec =
  let module RPO = MakeRPO(struct 
      let name = "lambda_rpo"
      let lambda_mode = true
      let ignore_deep_quants = true
    end) in
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> RPO.compare_terms ~prec a b) (a,b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> RPO.might_flip prec a b) (a,b)
  in
  let monotonic = false in
  { cache_compare; compare; name=RPO.name; prec; might_flip; cache_might_flip; monotonic }

let compose f ord =
  {ord with 
    compare = 
      fun prec a b ->
        (* CCFormat.printf "kbo: @[%a@]<?>@[%a@]@." T.pp a T.pp b; *)
        let f_res,a',b' = f a b in
        (* CCFormat.printf "f_res: @[%a@]@." Comparison.pp f_res; *)
        if Comparison.equal Comparison.Eq f_res then (
          let res = ord.compare prec a' b' in
          (* CCFormat.printf "res: @[%a@]@." Comparison.pp res; *)
          res
        ) else f_res
      }

let dummy_cache_ = CCCache.dummy
let epo prec =
  let cache_compare = mk_cache 256 in
  (* TODO: make internal EPO cache accessible here ? **)
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> EPO.compare_terms ~prec a b) (a,b)
  in
  let cache_might_flip = dummy_cache_ in
  let might_flip = EPO.might_flip in
  { cache_compare; compare; name=EPO.name; prec; might_flip; cache_might_flip; monotonic=true }

let lambdafree_kbo_coeff prec =
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> LambdaFreeKBOCoeff.compare_terms ~prec a b) (a,b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> LambdaFreeKBOCoeff.might_flip prec a b) (a,b) in
  { cache_compare; compare; name=LambdaFreeKBOCoeff.name; prec; might_flip; cache_might_flip; monotonic=false }

let none =
  let compare _ t1 t2 = if T.equal t1 t2 then Eq else Incomparable in
  let might_flip _ _ _ = false in
  let monotonic = true in
  { cache_compare=dummy_cache_; compare; prec=Prec.default []; name="none"; might_flip; cache_might_flip=dummy_cache_; monotonic}

let subterm =
  let compare _ t1 t2 =
    if T.equal t1 t2 then Eq
    else if T.subterm ~sub:t1 t2 then Lt
    else if T.subterm ~sub:t2 t1 then Gt
    else Incomparable
  in
  let might_flip _ _ _ = false in
  let monotonic = true in
  { cache_compare=dummy_cache_; compare; prec=Prec.default []; name="subterm"; might_flip; cache_might_flip=dummy_cache_; monotonic}

(** {2 Global table of orders} *)

let tbl_ =
  let h = Hashtbl.create 5 in
  Hashtbl.add h "lambdafree_kbo" lambdafree_kbo;
  Hashtbl.add h "lambda_kbo" (lambda_kbo ~ignore_quans_under_lam:false);
  Hashtbl.add h "lambda_kbo_complete" (lambda_kbo ~ignore_quans_under_lam:true);
  Hashtbl.add h "lambdafree_rpo" lambdafree_rpo;
  Hashtbl.add h "lambda_rpo" lambda_rpo;
  Hashtbl.add h "epo" epo;
  Hashtbl.add h "lambdafree_kbo_coeff" lambdafree_kbo_coeff;
  Hashtbl.add h "none" (fun _ -> none);
  Hashtbl.add h "subterm" (fun _ -> subterm);
  h

let default_of_list l =
  lambda_rpo (Prec.default l)

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
