(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Term Orderings} *)

module Prec = Precedence
module MT = Multiset.Make(Term)
module W = Precedence.Weight

module C = Comparison

let prof_rpo = ZProf.make "compare_rpo"
let prof_kbo = ZProf.make "compare_kbo"
let prof_epo = ZProf.make "compare_epo"
let prof_lambdafree_kbo_coeff = ZProf.make "compare_lambdafree_kbo_coeff"
let prof_lambda_kbo = ZProf.make "compare_lambda_kbo"
let prof_lambda_lpo = ZProf.make "compare_lambda_lpo"

module T = Term
module TC = Term.Classic

let mk_cache n =
  let hash (a, b) = Hash.combine3 42 (T.hash a) (T.hash b) in
  CCCache.replacing
    ~eq:(fun (a1,b1)(a2,b2) -> T.equal a1 a2 && T.equal b1 b2)
    ~hash
    n

type term = T.t

(** {2 Type definitions} *)

type t = {
  cache_compare : (T.t * T.t, C.t) CCCache.t;
  compare : Prec.t -> term -> term -> C.t;
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

let clear_cache ord = CCCache.clear ord.cache_compare; CCCache.clear ord.cache_might_flip

let pp out ord =
  Format.fprintf out "%s(@[%a@])" ord.name Prec.pp ord.prec

let to_string ord = CCFormat.to_string pp ord

(* Type-1 combinator is a combinator that is not ground
    (see Bhayat and Reger's combinator KBO paper) *)
let ty1comb_to_var t balance =
  if T.is_comb t && not (T.is_ground t) then (
    match T.Tbl.find_opt balance t with
    | Some t' -> t'
    | None ->
        let fresh_var = T.var (HVar.fresh ~ty:(T.ty t) ()) in
        T.Tbl.add balance t fresh_var;
        fresh_var
  ) else t

let is_problematic_type ty =
  Type.is_var ty || Type.is_fun ty

let partition_leading p =
  let rec part acc xs =
    match xs with
    | x :: xs' when p x -> part (x :: acc) xs'
    | _ -> (List.rev acc, xs)
  in
  part []

let break_term_up u =
  match T.view u with
  | T.App (hd, all_args) -> (hd, partition_leading Term.is_type all_args)
  | _ -> (u, ([], []))

let is_eta_reduced_term_stable_wrt_flip t =
  match T.view (fst (break_term_up t)) with
  | T.AppBuiltin _
  | T.DB _
  | T.Const _ -> true
  | _ -> false

let is_term_stable_wrt_flip t =
  match T.view (fst (break_term_up t)) with
  | T.AppBuiltin _
  | T.DB _
  | T.Const _ -> true
  | T.Fun _ -> is_eta_reduced_term_stable_wrt_flip (Lambda.eta_reduce t)
  | _ -> false

(** Common internal interface for orderings *)

module type ORD = sig
  (* This order relation should be:
   * - stable for instantiation
   * - compatible with function contexts
   * - total on ground terms *)
  val compare_terms : prec:Prec.t -> term -> term -> C.t

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
let prec_compare prec a b = match a, b with
  | Head.I a, Head.I b ->
    begin match Prec.compare prec a b with
      | 0 -> C.Eq
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
    mutable pos_counter : int;
    mutable neg_counter : int;
    mutable balance : CCInt.t TermBoolTbl.t;
    mutable comb2var : T.t Term.Tbl.t
  }

  (** create a balance for the two terms *)
  let mk_balance t1 t2 =
    let numvars = Iter.length (T.Seq.vars t1) + Iter.length (T.Seq.vars t2) in
    { pos_counter = 0; neg_counter = 0; balance = TermBoolTbl.create (2 * numvars);
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
    | Head.Q _ -> if P.ignore_deep_quants && below_lam then W.one else W.omega
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
      | [], [] -> wb, C.Eq
      | t1::terms1', t2::terms2' ->
        begin match tckbo wb t1 t2 ~below_lam with
          | (wb', C.Eq) -> tckbolex wb' terms1' terms2' ~below_lam
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
        let res = if List.length terms1 > List.length terms2 then C.Gt else Lt in
        wb'', res
      )
    (* tupled version of kbo (kbo_5 of the paper) *)
    and tckbo (wb:W.t) t1 t2 ~below_lam =
      if T.equal t1 t2
      then (wb, C.Eq) (* do not update weight or var balance *)
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
          (wb, C.Incomparable)
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
      let g_or_n = if balance.neg_counter = 0 then C.Gt else Incomparable
      and l_or_n = if balance.pos_counter = 0 then C.Lt else Incomparable in
      (* lexicographic product of weight and precedence *)
      if W.sign wb'' > 0 then wb'', g_or_n
      else if W.sign wb'' < 0 then wb'', l_or_n
      else match prec_compare prec f g with
        | C.Gt -> wb'', g_or_n
        | Lt ->  wb'', l_or_n
        | Eq ->
          if res = C.Eq then wb'', Eq
          else if res = Lt then wb'', l_or_n
          else if res = Gt then wb'', g_or_n
          else wb'', Incomparable
        | _ -> wb'', Incomparable
    (* recursive comparison *)
    and tckbo_rec wb f g ss ts ~below_lam =
      let ss_below_lam = below_lam || f = Head.LAM in
      let ts_below_lam = below_lam || g = Head.LAM in
      if prec_compare prec f g = C.Eq
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
        wb'', C.Incomparable
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

  let cannot_flip s t =
    assert (Type.equal (Term.ty s) (Term.ty t));
    T.equal s t
    || (is_eta_reduced_term_stable_wrt_flip s
        && is_eta_reduced_term_stable_wrt_flip t)

  let might_flip _ s t =
    not (cannot_flip s t)
end

(** Hopefully more efficient (polynomial) implementation of LPO,
    following the paper "Things to Know when Implementing LPO" by Löchner.
    We adapt here the implementation clpo6 with some multiset symbols (=). *)
module MakeRPO (P : PARAMETERS) : ORD = struct
  let name = P.name

  (* recursive path ordering *)
  let rec rpo6 ~prec s t =
    if T.equal s t then C.Eq else (  (* equality test is cheap *)
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
      | Lt -> C.opp (cMA ~prec t ss)
      | _ -> cAA ~prec s t ss ts
    end
  (* try to dominate all the terms in ts by s; but by subterm property
     if some t' in ts is >= s then s < t=g(ts) *)
  and cMA ~prec s = function
    | [] -> Gt
    | t::ts' ->
      (match rpo6 ~prec s t with
       | C.Gt -> cMA ~prec s ts'
       | Eq | Lt -> Lt
       | _ -> C.opp (alpha ~prec ts' s))
  (* lexicographic comparison of s=f(ss), and t=f(ts) *)
  and cLMA ~prec s t ss ts = match ss, ts with
    | si::ss', ti::ts' ->
      begin match rpo6 ~prec si ti with
        | C.Eq -> cLMA ~prec s t ss' ts'
        | Gt -> cMA ~prec s ts' (* just need s to dominate the remaining elements *)
        | Lt -> C.opp (cMA ~prec t ss')
        | _ -> cAA ~prec s t ss' ts'
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
      C.opp (cMA ~prec t ss)
  (* multiset comparison of subterms (not optimized) *)
  and cMultiset ~prec s t ss ts =
    match MT.compare_partial_l (rpo6 ~prec) ss ts with
    | C.Gt -> cMA ~prec s ts
    | Lt -> C.opp (cMA ~prec t ss)
    | _ -> Incomparable
  (* bidirectional comparison by subterm property (bidirectional alpha) *)
  and cAA ~prec s t ss ts =
    match alpha ~prec ss t with
    | Gt -> Gt
    | Incomparable -> C.opp (alpha ~prec ts s)
    | _ -> assert false
  (* if some s in ss is >= t, then s > t by subterm property and transitivity *)
  and alpha ~prec ss t = match ss with
    | [] -> Incomparable
    | s::ss' ->
      (match rpo6 ~prec s t with
       | Eq | Gt -> Gt
       | _ -> alpha ~prec ss' t)

  let compare_terms ~prec x y =
    let _span = ZProf.enter_prof prof_rpo in
    let compare = rpo6 ~prec x y in
    ZProf.exit_prof _span;
    compare

  (* The ordering might flip if one side is a lambda-expression or
     variable-headed or if the orientation is established using the subterm
     rule. *)
  let cannot_flip ~prec s t =
    assert (Type.equal (Term.ty s) (Term.ty t));
    T.equal s t
    || (is_eta_reduced_term_stable_wrt_flip s
        && is_eta_reduced_term_stable_wrt_flip t
        && not (let c = rpo6 ~prec s t in
          c = Incomparable
          || c = Gt && alpha ~prec (Head.term_to_args s) t = Gt
          || c = Lt && alpha ~prec (Head.term_to_args t) s = Gt))

  let might_flip prec s t =
    not (cannot_flip ~prec s t)
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
    then C.Eq 
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
            if c = C.Gt then epo_check_e4     ~prec (t,tt) (s,ss) (g,gg) (f,ff) else
            if c = Lt then epo_check_e4_inv ~prec (t,tt) (s,ss) (g,gg) (f,ff) else
              epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
          | _ -> 
            if c = Gt then epo_check_e2_e3     ~prec (t,tt) (s,ss) (g,gg) (f,ff) else
            if c = Lt then epo_check_e2_e3_inv ~prec (t,tt) (s,ss) (g,gg) (f,ff) else
              epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
        end
      | _ -> epo_check_e1 ~prec (t,tt) (s,ss) (g,gg) (f,ff)
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
      | [], [] -> C.Eq
      | _ :: _, [] -> Gt
      | [] , _ :: _ -> Lt
      | t0 :: t_rest , s0 :: s_rest ->
        begin match lfhokbo_arg_coeff ~prec t0 s0 with
          | C.Gt -> Gt
          | Lt -> Lt
          | Eq -> lfhokbo_lex t_rest s_rest
          | _ -> Incomparable
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
        | Gt -> C.Gt (* by rule C3 *)
        | Lt -> Lt (* by rule C3 inversed *)
        | Eq -> (* try rule C4 *)
          begin match prec_status prec g with
            | Prec.Lexicographic -> lfhokbo_lex ts ss
            | Prec.LengthLexicographic -> lfhokbo_lenlex ts ss
            | _ -> assert false
          end
        | _ -> (* try rule C2 *)
          let hd_ts_s = lfhokbo_arg_coeff ~prec (List.hd ts) s in
          let hd_ss_t = lfhokbo_arg_coeff ~prec (List.hd ss) t in
          if List.length ts = 1 && (hd_ts_s = Gt || hd_ts_s = Eq) then Gt else
          if List.length ss = 1 && (hd_ss_t = Gt || hd_ss_t = Eq) then Lt else
            Incomparable
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
    assert (Type.equal (Term.ty t) (Term.ty s));
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
      | Head.V _, _ | _, Head.V _ -> true
      | _ -> assert false
end

(* This imperative polynomial data structure is designed to offer good
   performance. *)
module Polynomial = struct
  type unknown =
    | EtaUnknown of Type.t HVar.t  (* h *)
    | WeightUnknown of term list  (* w *)
    | CoeffUnknown of term list * int  (* k *)

  let compare unk2 unk1 = match unk2, unk1 with
  | EtaUnknown y, EtaUnknown x -> HVar.compare Type.compare y x
  | EtaUnknown _, _ -> +1
  | _, EtaUnknown _ -> -1
  | WeightUnknown ts, WeightUnknown ss -> CCList.compare T.compare ts ss
  | WeightUnknown _, _ -> +1
  | _, WeightUnknown _ -> -1
  | CoeffUnknown (ts, j), CoeffUnknown (ss, i) ->
    (match CCList.compare T.compare ts ss with
     | 0 -> CCInt.compare j i
     | n -> n)

  let equal unk1 (unk2 : unknown) = (compare unk2 unk1 = 0)

  let hash_unknown = function
    | EtaUnknown var -> HVar.hash var
    | WeightUnknown ts -> Hash.list Term.hash ts
    | CoeffUnknown (ts, i) -> Hash.list Term.hash ts + i + 1

  let pp_unknown out = function
    | EtaUnknown var ->
      Format.pp_print_string out "h_{";
      HVar.pp out var;
      Format.pp_print_string out "}"
    | WeightUnknown ts ->
      Format.pp_print_string out "w_{";
      CCList.pp Term.pp out ts;
      Format.pp_print_string out "}"
    | CoeffUnknown (ts, i) ->
      Format.pp_print_string out "k_{";
      CCList.pp Term.pp out ts;
      Format.pp_print_string out ",";
      Format.pp_print_int out i;
      Format.pp_print_string out "}"

  module ULH = CCHashtbl.Make(struct
      type t = unknown list
      let equal = CCList.equal equal
      let hash = Hash.list hash_unknown
    end)

  type polynomial = {
    mutable hashtbl : W.t ULH.t;
    mutable pos_counter : int;
    mutable neg_counter : int;
  }

  let mk_key = function
    | [] -> []
    | [unk] -> [unk]
    | unks -> List.sort compare unks

  let incr_counter poly sign k =
    if sign > 0 then poly.pos_counter <- poly.pos_counter + k
    else if sign < 0 then poly.neg_counter <- poly.neg_counter + k

  let create_zero () =
    {hashtbl = ULH.create 16;
     pos_counter = 0;
     neg_counter = 0}

  let is_zero poly =
    poly.pos_counter + poly.neg_counter = 0

  let all_coeffs_nonnegative poly =
    poly.neg_counter = 0

  let all_coeffs_nonpositive poly =
    poly.pos_counter = 0

  let add_monomial poly coeff unks =
    if coeff != W.zero then (
      let key = mk_key unks in
      match ULH.find_opt poly.hashtbl key with
      | None ->
        ULH.add poly.hashtbl key coeff;
        incr_counter poly (W.sign coeff) (+1)
      | Some old_coeff ->
        let sum = W.add old_coeff coeff in
        if sum = W.zero then
          ULH.remove poly.hashtbl key
        else
          ULH.replace poly.hashtbl key sum;
        incr_counter poly (W.sign old_coeff) (-1);
        incr_counter poly (W.sign sum) (+1)
    )

  let add poly1 poly2 =
    if not (is_zero poly2) then
      ULH.iter (fun key coeff -> add_monomial poly1 coeff key) poly2.hashtbl

  let multiply_unknowns poly unks =
    if not (CCList.is_empty unks) then
      let old_hashtbl = poly.hashtbl in
      poly.hashtbl <- ULH.create 16;
      ULH.iter (fun key coeff ->
          ULH.add poly.hashtbl (mk_key (List.rev_append unks key)) coeff)
        old_hashtbl

  let constant_monomial poly =
    match ULH.find_opt poly.hashtbl [] with
    | None -> W.zero
    | Some coeff -> coeff

  let pp out poly =
    let first = ref true in
    ULH.iter (fun key coeff ->
        if !first then first := false else Format.pp_print_string out " + ";
        Format.pp_print_string out "(";
        W.pp out coeff;
        Format.pp_print_string out ")";
        if not (CCList.is_empty key) then (
          Format.pp_print_string out "*";
          CCList.pp ~pp_sep:(fun out () -> Format.pp_print_string out "*")
            pp_unknown out key
        ))
      poly.hashtbl;
    if !first then Format.pp_print_string out "0";
    CCFormat.printf " [%d %d]" poly.pos_counter poly.neg_counter
end

let same_length_lex_ext f ys xs =
  let rec lex ys xs = match ys, xs with
    | [], [] -> C.Eq
    | y :: ys, x :: xs ->
      (match f y x with
      | C.Geq -> C.merge_with_Geq (lex ys xs)
      | Eq -> lex ys xs
      | Leq -> C.merge_with_Leq (lex ys xs)
      | cmp -> cmp)
    | _, _ -> assert false
  in
  match CCInt.compare (List.length ys) (List.length xs) with
  | 0 -> lex ys xs
  | n -> if n > 0 then C.Gt else Lt

let same_length_lex_ext_data f ys xs =
  let rec lex ys xs = match ys, xs with
    | [], [] -> ([], C.Eq)
    | y :: ys, x :: xs ->
      (match f y x with
      | (w, C.Geq) ->
        let (ws, cmp) = lex ys xs in
        (w :: ws, C.merge_with_Geq cmp)
      | (w, Eq) ->
        let (ws, cmp) = lex ys xs in
        (w :: ws, cmp)
      | (w, Leq) ->
        let (ws, cmp) = lex ys xs in
        (w :: ws, C.merge_with_Leq cmp)
      | (w, cmp) -> ([w], cmp))
    | _, _ -> assert false
  in
  match CCInt.compare (List.length ys) (List.length xs) with
  | 0 -> lex ys xs
  | n -> ([], if n > 0 then C.Gt else Lt)

let cw_ext f ys xs =
  if List.length ys = List.length xs then
    same_length_lex_ext (fun y x -> C.smooth (f y x)) ys xs
  else
    C.Incomparable

let cw_ext_data f ys xs =
  if List.length ys = List.length xs then
    same_length_lex_ext_data (fun y x ->
        let (w, cmp) = f y x in
        (w, C.smooth cmp))
      ys xs
  else
    ([], C.Incomparable)

let is_more_polymorphic t s =
  match Type.view (Term.ty t), Type.view (Term.ty s) with
  | Var y, Var x -> HVar.id y = HVar.id x
  | _, Var _ -> false
  | _, _ -> true

let consider_poly t s cmp =
  match cmp with
  | C.Gt | Geq -> if is_more_polymorphic t s then cmp else Incomparable
  | Lt | Leq -> if is_more_polymorphic s t then cmp else Incomparable
  | _ -> cmp

module LambdaKBO : ORD = struct
  let name = "lambda_kbo"

  (* We ignore [Prec.db_weight] to get better arithmetic for applied
     variables. *)
  let my_db_weight = W.one

  let add_monomial w sign coeff unks =
    Polynomial.add_monomial w (W.mult sign coeff) unks

  module Type_KBO = MakeKBO(struct
      let name = "type_kbo"
      let lambda_mode = false
      let ignore_deep_quants = true
    end)

  let compare_type_terms ~prec =
    Type_KBO.compare_terms ~prec

  let compare_types ~prec t_ty s_ty =
    compare_type_terms ~prec (Term.of_ty t_ty) (Term.of_ty s_ty)

  module WH = CCHashtbl.Make(struct
      type t = W.t
      let equal = CCShims_.Stdlib.(=)
      let hash = W.hash
    end)

  let weight_id_hashtbl = WH.create 16

  let id_of_weight w =
    match WH.find_opt weight_id_hashtbl w with
    | Some id -> id
    | None ->
      let id = ID.make (W.to_string w) in
      WH.add weight_id_hashtbl w id;
      id

  (* The ordering might flip if one eta-reduced side is a lambda-expression. *)
  let cannot_flip t s =
    assert (Type.equal (Term.ty t) (Term.ty s));
    T.equal t s || (is_term_stable_wrt_flip t && is_term_stable_wrt_flip s)

  let rec normalize_consts ~prec t =
    match T.view t with
    | AppBuiltin (b, bargs) ->
      Term.app_builtin ~ty:(Term.ty t) b
        (List.map (normalize_consts ~prec) bargs)
    | Const fid ->
      let fid' = id_of_weight (Prec.weight prec fid) in
      Term.const ~ty:(Term.ty t) fid'
    | Fun (ty, body) -> Term.fun_ ty (normalize_consts ~prec body)
    | App (s, ts) ->
      Term.app (normalize_consts ~prec s) (List.map (normalize_consts ~prec) ts)
    | _ -> t

  let categorize_var_arg var arg arg_ty (some_args, extra_args) =
    if is_problematic_type arg_ty then (arg :: some_args, extra_args)
    else (some_args, arg :: extra_args)

  let rec add_weight_of ~prec w sign t =
    let add_eta_extra_of w ty = match Type.view ty with
      | Var var ->
        add_monomial w sign (W.add (Prec.lam_weight prec) my_db_weight)
          [Polynomial.EtaUnknown var]
      | _ -> ()
    in
    let is_quantifier b =
      b = Builtin.ForallConst || b = Builtin.ExistsConst
    in
    let (hd, (_, args)) = break_term_up t in
    match T.view hd with
    | AppBuiltin (b, bargs) ->
      (* We give a weight of omega to quantifiers to partly satisfy the
         desideratum that a quantified formula should be larger than its
         instances. *)
      add_monomial w sign (if is_quantifier b then W.omega else W.one) [];
      List.iter (add_weight_of ~prec w sign) bargs;
      List.iter (add_weight_of ~prec w sign) args
    | DB i ->
      add_monomial w sign my_db_weight [];
      List.iter (add_weight_of ~prec w sign) args;
      add_eta_extra_of w (Term.ty t)
    | Var var ->
      if CCList.is_empty args then (
        add_monomial w sign W.one [];
        add_monomial w sign W.one [Polynomial.WeightUnknown [hd]]
      ) else (
        let (arg_tys, _) = Type.open_fun (Term.ty hd) in
        let (some_args, extra_args) =
          List.fold_right2 (categorize_var_arg var) args arg_tys ([], [])
        in
        let some_normal_args = List.map (normalize_consts ~prec) some_args in
        let add_weight_of_extra_arg i arg =
          let w' = Polynomial.create_zero () in
          add_weight_of ~prec w' sign arg;
          add_monomial w' (-1 * sign) my_db_weight [];
          Polynomial.multiply_unknowns w'
            [Polynomial.CoeffUnknown (hd :: some_normal_args, i + 1)];
          Polynomial.add w w'
        in
        add_monomial w sign W.one [];
        add_monomial w sign W.one
          [Polynomial.WeightUnknown (hd :: some_normal_args)];
        List.iteri add_weight_of_extra_arg extra_args;
        add_eta_extra_of w (Term.ty t)
      )
    | Const fid ->
      add_monomial w sign (Prec.weight prec fid) [];
      (* we abuse the meaning of the word "sign" below *)
      List.iteri (fun i arg ->
          add_weight_of ~prec w (sign * Prec.arg_coeff prec fid i) arg)
        args;
      add_eta_extra_of w (Term.ty t)
    | Fun (_, body) ->
      add_monomial w sign (Prec.lam_weight prec) [];
      add_weight_of ~prec w sign body
    | App _ -> ()  (* impossible *)

  let analyze_weight_diff w =
    match Polynomial.all_coeffs_nonnegative w,
      Polynomial.all_coeffs_nonpositive w with
    | false, false -> C.Incomparable
    | true, false ->
      if W.sign (Polynomial.constant_monomial w) > 0 then Gt else Geq
    | false, true ->
      if W.sign (Polynomial.constant_monomial w) < 0 then Lt else Leq
    | true, true -> Eq

  let consider_weight w cmp =
    (w,
     match analyze_weight_diff w with
     | Geq -> C.merge_with_Geq cmp
     | Eq -> cmp
     | Leq -> C.merge_with_Leq cmp
     | cmp' -> cmp')

  let rec process_args ~prec ts ss =
    let w = Polynomial.create_zero () in
    (* both comparisons are needed because connectives have variable arity *)
    if CCList.is_empty ts && CCList.is_empty ss then
      (w, C.Eq)
    else (
      let (ws, cmp) = same_length_lex_ext_data (process_terms ~prec) ts ss
      in
      let m = List.length ws in
      List.iter (Polynomial.add w) ws;
      List.iter (add_weight_of ~prec w (+1)) (CCList.drop m ts);
      List.iter (add_weight_of ~prec w (-1)) (CCList.drop m ss);
      consider_weight w cmp
    )
  and process_terms ~prec t s =
    let (t_hd, (t_tyargs, t_args)) = break_term_up t in
    let (s_hd, (s_tyargs, s_args)) = break_term_up s in
    match T.view t_hd, T.view s_hd with
    | Var y, Var x when HVar.id y = HVar.id x ->
      let w = Polynomial.create_zero () in
      (* only a single list needs to be checked thanks to eta-expansion *)
      if CCList.is_empty t_args then
        (w, C.Eq)
      else (
        let (arg_tys, _) = Type.open_fun (Term.ty t_hd) in
        let (some_t_args, extra_t_args) =
          List.fold_right2 (categorize_var_arg y) t_args arg_tys ([], [])
        in
        let (some_s_args, extra_s_args) =
          List.fold_right2 (categorize_var_arg y) s_args arg_tys ([], [])
        in
        let some_normal_t_args = List.map (normalize_consts ~prec) some_t_args
        and some_normal_s_args = List.map (normalize_consts ~prec) some_s_args
        in
        if CCList.equal T.equal some_normal_t_args some_normal_s_args
           && CCList.for_all2 cannot_flip some_t_args some_s_args then
          let (arg_ws, cmp) = cw_ext_data (process_terms ~prec) t_args s_args in
          let extra_arg_ws = CCList.drop (List.length some_t_args) arg_ws in
          let add_weight_of_extra_arg i arg_w =
            Polynomial.multiply_unknowns arg_w
              [Polynomial.CoeffUnknown (t_hd :: some_normal_t_args, i + 1)];
            Polynomial.add w arg_w
          in
          List.iteri add_weight_of_extra_arg extra_arg_ws;
          consider_weight w cmp
        else
          consider_weights_poly ~prec t s C.Incomparable
      )
    | Var _, AppBuiltin (b, _) ->
      consider_weights_poly ~prec t s
        (if Builtin.as_int b = 0 then C.Geq else C.Incomparable)
    | AppBuiltin (b, _), Var _ ->
      consider_weights_poly ~prec t s
        (if Builtin.as_int b = 0 then C.Leq else C.Incomparable)
    | Var _, _ | _, Var _ ->
      consider_weights_poly ~prec t s C.Incomparable
    | Fun (t_ty, t_body), Fun (s_ty, s_body) ->
      (match compare_types ~prec t_ty s_ty with
       | Eq -> process_terms ~prec t_body s_body
       | cmp -> consider_weights_poly ~prec t_body s_body cmp)
    | Fun _, (DB _|Const _|AppBuiltin _) -> consider_weights_poly ~prec t s Gt
    | DB _, Fun _ -> consider_weights_poly ~prec t s Lt
    | DB j, DB i ->
      if j > i then consider_weights_poly ~prec t s Gt
      else if j < i then consider_weights_poly ~prec t s Lt
      else process_args ~prec t_args s_args
    | DB _, (Const _|AppBuiltin _) -> consider_weights_poly ~prec t s Gt
    | Const _, (Fun _|DB _) -> consider_weights_poly ~prec t s Lt
    | Const gid, Const fid ->
      (match Prec.compare prec gid fid with
       | 0 ->
         (match ID.compare gid fid with
          | 0 ->
            (match same_length_lex_ext (compare_type_terms ~prec)
               t_tyargs s_tyargs with
             | Eq -> process_args ~prec t_args s_args
             | cmp -> consider_weights_poly ~prec t s cmp)
          | n -> consider_weights_poly ~prec t s (C.of_total n))
       | n when n > 0 -> consider_weights_poly ~prec t s Gt
       | _ -> consider_weights_poly ~prec t s Lt)
    | Const _, AppBuiltin _ -> consider_weights_poly ~prec t s Gt
    | AppBuiltin _, (Fun _|DB _|Const _) -> consider_weights_poly ~prec t s Lt
    | AppBuiltin (t_b, t_bargs), AppBuiltin (s_b, s_bargs) ->
      (match Builtin.compare t_b s_b with
       | 0 ->
         process_args ~prec (t_bargs @ t_tyargs @ t_args)
           (s_bargs @ s_tyargs @ s_args)
       | n when n > 0 -> consider_weights_poly ~prec t s Gt
       | _ -> consider_weights_poly ~prec t s Lt)
    | _, _ -> assert false
  and consider_weights_poly ~prec t s cmp =
    let w = Polynomial.create_zero () in
    add_weight_of ~prec w (+1) t;
    add_weight_of ~prec w (-1) s;
    consider_weight w (consider_poly t s cmp)

  let compare_terms ~prec t s =
    let _span = ZProf.enter_prof prof_lambda_kbo in
    let (_, cmp) = process_terms ~prec t s in
    ZProf.exit_prof _span;
    cmp

  let might_flip _ t s =
    not (cannot_flip t s)
end

module LambdaLPO : ORD = struct
  let name = "lambda_lpo"

  module Type_RPO = MakeRPO(struct
      let name = "type_rpo"
      let lambda_mode = false
      let ignore_deep_quants = true
    end)

  let compare_type_terms ~prec =
    Type_RPO.compare_terms ~prec

  let compare_types ~prec t_ty s_ty =
    compare_type_terms ~prec (Term.of_ty t_ty) (Term.of_ty s_ty)

  (* The ordering might flip if one side is a lambda-expression or
     variable-headed or if the orientation is established using the subterm
     rule. *)
  let rec cannot_flip ~prec t s =
    assert (Type.equal (Term.ty t) (Term.ty s));
    not (is_problematic_type (Term.ty t))
    || T.equal t s
    || (is_term_stable_wrt_flip t
        && is_term_stable_wrt_flip s
        && not (let cmp = do_compare_terms ~prec t s in
             cmp = C.Incomparable
             || (cmp = C.Gt && check_subs ~prec [t] s)
             || (cmp = C.Lt && check_subs ~prec [s] t)))
  and check_subs ~prec ts s =
    List.exists (fun ti -> C.is_Gt_or_Geq_or_Eq (do_compare_terms ~prec ti s))
      ts
  and compare_subs_both_ways ~prec t ts s ss =
    if check_subs ~prec ts s then C.Gt
    else if check_subs ~prec ss t then C.Lt
    else C.Incomparable
  and compare_rest ~prec t ss =
    match ss with
    | [] -> C.Gt
    | si :: ss' ->
      (match do_compare_terms ~prec t si with
       | C.Gt -> compare_rest ~prec t ss'
       | C.Eq | C.Leq | C.Lt -> C.Lt
       | C.Geq | C.Incomparable ->
         if check_subs ~prec ss' t then C.Lt else C.Incomparable)
  and compare_regular_args ~prec t ts s ss =
    match ts, ss with
    | [], [] -> C.Eq
    | ti :: ts', si :: ss' ->
      (match do_compare_terms ~prec ti si with
       | C.Gt -> compare_rest ~prec t ss'
       | C.Geq -> C.merge_with_Geq (compare_regular_args ~prec t ts' s ss')
       | C.Eq -> compare_regular_args ~prec t ts' s ss'
       | C.Leq -> C.merge_with_Leq (compare_regular_args ~prec t ts' s ss')
       | C.Lt -> C.opp (compare_rest ~prec s ts')
       | C.Incomparable -> compare_subs_both_ways ~prec t ts' s ss')
    | _, _ -> assert false
  and do_compare_terms ~prec t s =
    let (t_hd, (t_tyargs, t_args)) = break_term_up t in
    let (s_hd, (s_tyargs, s_args)) = break_term_up s in
    match T.view t_hd, T.view s_hd with
    | Var y, Var x ->
      if HVar.id y = HVar.id x
         && List.for_all2 (cannot_flip ~prec) t_args s_args then
        cw_ext (do_compare_terms ~prec) t_args s_args
      else
        C.Incomparable
    | Var _, Fun (_, s_body) ->
      if check_subs ~prec [s_body] t then C.Lt else C.Incomparable
    | Var _, (DB _|Const _) ->
      if check_subs ~prec s_args t then C.Lt else C.Incomparable
    | Var _, AppBuiltin (s_b, s_bargs) ->
      if check_subs ~prec (List.rev_append s_bargs s_args) t then C.Lt
      else C.Incomparable
    | (Const _|DB _), Var _ ->
      if check_subs ~prec t_args s then C.Gt else C.Incomparable
    | Const gid, Const fid ->
      (match Prec.compare prec gid fid with
       | 0 ->
         (match ID.compare gid fid with
          | 0 ->
           (match same_length_lex_ext (compare_type_terms ~prec)
              t_tyargs s_tyargs with
            | Gt -> compare_rest ~prec t s_args
            | Eq -> compare_regular_args ~prec t t_args s s_args
            | Lt -> C.opp (compare_rest ~prec s t_args)
            | _ -> compare_subs_both_ways ~prec t t_args s s_args)
          | n when n > 0 -> compare_rest ~prec t s_args
          | _ -> C.opp (compare_rest ~prec s t_args))
       | n when n > 0 -> compare_rest ~prec t s_args
       | _ -> C.opp (compare_rest ~prec s t_args))
    | Const _, AppBuiltin (_, s_bargs) ->
      compare_rest ~prec t (List.rev_append s_bargs s_args)
    | (Const _|AppBuiltin _), DB _ -> compare_rest ~prec t s_args
    | (Const _|AppBuiltin _|DB _), Fun (_, s_body) ->
      compare_rest ~prec t [s_body]
    | AppBuiltin (t_b, t_bargs), Var _ ->
      if check_subs ~prec (List.rev_append t_bargs t_args) s then C.Gt
      else C.Incomparable
    | AppBuiltin (_, t_bargs), Const _ ->
      C.opp (compare_rest ~prec s (List.rev_append t_bargs t_args))
    | AppBuiltin (t_b, t_bargs), AppBuiltin (s_b, s_bargs) ->
      let all_t_args = t_bargs @ t_tyargs @ t_args
      and all_s_args = s_bargs @ s_tyargs @ s_args in
      (match CCInt.compare (Builtin.as_int t_b) (Builtin.as_int s_b) with
       | 0 ->
        (match CCInt.compare (List.length all_t_args) (List.length all_s_args)
         with
         | 0 -> compare_regular_args ~prec t all_t_args s all_s_args
         | n when n > 0 -> compare_rest ~prec t all_s_args
         | _ -> C.opp (compare_rest ~prec s all_t_args))
       | n when n > 0 -> compare_rest ~prec t all_s_args
       | _ -> C.opp (compare_rest ~prec s all_t_args))
    | DB _, (Const _|AppBuiltin _) -> C.opp (compare_rest ~prec s t_args)
    | DB j, DB i ->
      if j > i then consider_poly t s (compare_rest ~prec t s_args)
      else if j = i then compare_regular_args ~prec t t_args s s_args
      else consider_poly t s (C.opp (compare_rest ~prec s t_args))
    | Fun (_, t_body), Var _ ->
      if check_subs ~prec [t_body] s then C.Gt else C.Incomparable
    | Fun (_, t_body), (Const _|AppBuiltin _|DB _) ->
      C.opp (compare_rest ~prec s [t_body])
    | Fun (t_ty, t_body), Fun (s_ty, s_body) ->
      (match compare_types ~prec t_ty s_ty with
       | Gt -> compare_rest ~prec t [s_body]
       | Eq -> do_compare_terms ~prec t_body s_body
       | Lt -> C.opp (compare_rest ~prec s [t_body])
       | _ -> compare_subs_both_ways ~prec t [t_body] s [s_body])
    | App _, _ | _, App _ -> assert false

  let compare_terms ~prec t s =
    let _span = ZProf.enter_prof prof_lambda_lpo in
    let cmp = do_compare_terms ~prec t s in
    ZProf.exit_prof _span;
    cmp

  let might_flip prec t s =
    not (cannot_flip ~prec t s)
end

(** {2 Value interface} *)

let dummy_cache_ = CCCache.dummy

let map2 f g { cache_compare=_; compare; prec; name; might_flip; cache_might_flip=_; monotonic } =
  let cache_compare = mk_cache 256 in
  let cache_might_flip = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare (fun (a, b) -> compare prec (f a) (f b)) (a, b) in
  let might_flip prec a b = CCCache.with_cache cache_might_flip (fun (a, b) -> might_flip prec (g a) (g b)) (a, b) in
  { cache_compare; compare; prec; name; might_flip; cache_might_flip; monotonic }

let map f ord = map2 f f ord

let derived_ho_kbo ~ignore_quans_under_lam prec =
  let module KBO = MakeKBO(struct 
      let name = "derived_ho_kbo"
      let lambda_mode = true
      let ignore_deep_quants = ignore_quans_under_lam
    end) in
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> 
        KBO.compare_terms ~prec a b) (a, b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> KBO.might_flip prec a b) (a, b)
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
      (fun (a, b) -> KBO.compare_terms ~prec a b) (a, b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> KBO.might_flip prec a b) (a, b)
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
      (fun (a, b) -> RPO.compare_terms ~prec a b) (a, b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> RPO.might_flip prec a b) (a, b)
  in
  let monotonic = false in
  { cache_compare; compare; name=RPO.name; prec; might_flip; cache_might_flip; monotonic }

let derived_ho_rpo prec =
  let module RPO = MakeRPO(struct 
      let name = "derived_ho_rpo"
      let lambda_mode = true
      let ignore_deep_quants = true
    end) in
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> RPO.compare_terms ~prec a b) (a, b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> RPO.might_flip prec a b) (a, b)
  in
  let monotonic = false in
  { cache_compare; compare; name=RPO.name; prec; might_flip; cache_might_flip; monotonic }

let compose f ord =
  {ord with 
    compare = 
      fun prec a b ->
        let f_res,a',b' = f a b in
        match f_res with
        | C.Geq -> C.merge_with_Geq (ord.compare prec a' b')
        | Eq -> ord.compare prec a' b'
        | Leq -> C.merge_with_Leq (ord.compare prec a' b')
        | _ -> f_res
      }

let dummy_cache_ = CCCache.dummy
let epo prec =
  let cache_compare = mk_cache 256 in
  (* TODO: make internal EPO cache accessible here ? **)
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> EPO.compare_terms ~prec a b) (a, b)
  in
  let cache_might_flip = dummy_cache_ in
  let might_flip = EPO.might_flip in
  { cache_compare; compare; name=EPO.name; prec; might_flip; cache_might_flip; monotonic=true }

let lambdafree_kbo_coeff prec =
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> LambdaFreeKBOCoeff.compare_terms ~prec a b) (a, b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> LambdaFreeKBOCoeff.might_flip prec a b) (a, b) in
  { cache_compare; compare; name=LambdaFreeKBOCoeff.name; prec; might_flip; cache_might_flip; monotonic=false }

let lambda_kbo prec =
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> LambdaKBO.compare_terms ~prec a b) (a, b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> LambdaKBO.might_flip prec a b) (a, b) in
  let normalize_cmp t =
    if Term.is_fo_term t then t else Lambda.eta_expand (Lambda.snf t)
  in
  let normalize_flip t =
    if Term.is_fo_term t then t else Lambda.snf t
  in
  let monotonic = false in
  map2 normalize_cmp normalize_flip
    { cache_compare; compare; name=LambdaKBO.name; prec; might_flip; cache_might_flip; monotonic }

let lambda_lpo prec =
  let cache_compare = mk_cache 256 in
  let compare prec a b = CCCache.with_cache cache_compare
      (fun (a, b) -> LambdaLPO.compare_terms ~prec a b) (a, b)
  in
  let cache_might_flip = mk_cache 256 in
  let might_flip prec a b = CCCache.with_cache cache_might_flip
      (fun (a, b) -> LambdaLPO.might_flip prec a b) (a, b) in
  let normalize_cmp t =
    if Term.is_fo_term t then t else Lambda.eta_expand (Lambda.snf t)
  in
  let normalize_flip t =
    if Term.is_fo_term t then t else Lambda.snf t
  in
  let monotonic = false in
  map2 normalize_cmp normalize_flip
    { cache_compare; compare; name=LambdaLPO.name; prec; might_flip; cache_might_flip; monotonic }

let none =
  let compare _ t1 t2 = if T.equal t1 t2 then C.Eq else Incomparable in
  let might_flip _ _ _ = false in
  let monotonic = true in
  { cache_compare=dummy_cache_; compare; prec=Prec.default []; name="none"; might_flip; cache_might_flip=dummy_cache_; monotonic}

let subterm =
  let compare _ t1 t2 =
    if T.equal t1 t2 then C.Eq
    else if T.subterm ~sub:t1 t2 then Lt
    else if T.subterm ~sub:t2 t1 then Gt
    else Incomparable
  in
  let might_flip _ _ _ = false in
  let monotonic = true in
  { cache_compare=dummy_cache_; compare; prec=Prec.default []; name="subterm"; might_flip; cache_might_flip=dummy_cache_; monotonic}

(** {2 Global table of orders} *)

let tbl_ =
  let h = Hashtbl.create 16 in
  Hashtbl.add h "lambdafree_kbo" lambdafree_kbo;
  Hashtbl.add h "derived_ho_kbo" (derived_ho_kbo ~ignore_quans_under_lam:false);
  Hashtbl.add h "derived_ho_kbo_complete" (derived_ho_kbo ~ignore_quans_under_lam:true);
  Hashtbl.add h "lambdafree_rpo" lambdafree_rpo;
  Hashtbl.add h "derived_ho_rpo" derived_ho_rpo;
  Hashtbl.add h "epo" epo;
  Hashtbl.add h "lambdafree_kbo_coeff" lambdafree_kbo_coeff;
  Hashtbl.add h "lambda_kbo" lambda_kbo;
  Hashtbl.add h "lambda_lpo" lambda_lpo;
  Hashtbl.add h "none" (fun _ -> none);
  Hashtbl.add h "subterm" (fun _ -> subterm);
  h

let default_of_list l =
  derived_ho_rpo (Prec.default l)

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
