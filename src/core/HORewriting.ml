
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Rewriting on HO terms} *)

module Hash = CCHash
module T = HOTerm

type term = T.t
type rule = term * term

exception IllFormedRule of rule

(** {2 Rewrite system} *)

(** Set of rules *)
module S = Sequence.Set.Make(struct
  type t = rule
  let compare (l1,r1) (l2,r2) =
    let c = T.compare l1 l2 in
    if c <> 0 then c else T.compare r1 r2
end)

type t = S.t

let empty = S.empty

(* check whether the rule is valid for rewriting *)
let _validate_rule (l,r) =
  let cond1 =
    T.Seq.vars r
    |> Sequence.for_all
      (fun v -> T.var_occurs ~var:v l)
  and cond2 =
    T.Seq.subterms_depth r
    |> Sequence.filter (fun (v,_) -> T.is_var v)
    |> Sequence.for_all (fun (_,d) -> d = 0)
  in
  if not (cond1 && cond2)
    then raise (IllFormedRule (l,r))

let add trs rule =
  _validate_rule rule;
  S.add rule trs

let merge t1 t2 = S.union t1 t2

let equal = S.equal
let compare = S.compare
let hash_fun s h = Hash.seq (Hash.pair T.hash_fun T.hash_fun) (S.to_seq s) h
let hash s = Hash.apply hash_fun s

module Seq = struct
  let to_seq = S.to_seq
  let of_seq init seq = S.union (S.of_seq seq) init
end

let to_list trs = Seq.to_seq trs |> Sequence.to_rev_list
let of_list l = Sequence.of_list l |> Seq.of_seq empty

let pp out t =
  Format.fprintf out
  "rewriting{@,@[%a@]@,}"
    (CCFormat.seq ~sep:"; "
      (fun out (t1, t2) -> Format.fprintf out "%a --> %a" T.pp t1 T.pp t2))
    (Seq.to_seq t)

let to_string = CCFormat.to_string pp

(** {2 Normalize} *)

exception RewrittenIn of term * Substs.t * rule

let normalize_collect trs t =
  (* reduce to normal form. *)
  let rec reduce ~trs ~rules t = match T.view t with
    | T.App (f, l) ->
        let f = reduce ~trs ~rules f in
        let l = List.map (reduce ~trs ~rules) l in
        rewrite_here ~trs ~rules (T.app f l)
    | T.Multiset (tau,l) ->
        let l' = List.map (reduce ~trs ~rules) l in
        rewrite_here ~trs ~rules (T.multiset ~ty:tau l')
    | T.Record (l, rest) ->
        let l' = List.map (fun (n,t) -> n, reduce ~trs ~rules t) l in
        rewrite_here ~trs ~rules (T.record l' ~rest)
    | T.Var _
    | T.DB _ -> t
    | T.Lambda (varty, t') ->
        let t' = reduce ~trs ~rules t' in
        T.lambda ~varty t'   (* no rules for lambda *)
    | T.Forall (varty, t') ->
        let t' = reduce ~trs ~rules t' in
        T.forall ~varty t'
    | T.Exists (varty, t') ->
        let t' = reduce ~trs ~rules t' in
        T.exists ~varty t'   (* no rules for lambda *)
    | T.AppBuiltin (_,[]) -> t
    | T.AppBuiltin (b,l) ->
        let l = List.map (reduce ~trs ~rules) l in
        T.app_builtin ~ty:(T.ty t) b l
    | T.Const _ ->
        rewrite_here ~trs ~rules t
  (* try to find a rewrite rules whose left-hand side matches [t]. In this
     case replace by the right-hand side of the rule. *)
  and rewrite_here ~trs ~rules t =
    try
      S.iter
        (fun (l,r) ->
          let substs = Unif.HO.matching
            ~pattern:(Scoped.make l 1) (Scoped.make t 0) in
          match substs |> Sequence.take 1 |> Sequence.to_list with
          | [subst] ->
              (* l\subst = t, rewrite into r\subst *)
              let r = Substs.HO.apply_no_renaming subst (Scoped.make r 1) in
              raise (RewrittenIn (r, subst, (l,r)))
          | _ -> ()   (* failure, try next rule *)
        ) trs;
      (* could not rewrite [t], just return it *)
      t
    with RewrittenIn (t', _subst, rule) ->
      rules := rule :: !rules;
      reduce ~trs ~rules t'
  in
  let rules = ref [] in
  let t' = reduce ~trs ~rules t in
  t', !rules

let normalize trs t =
  fst (normalize_collect trs t)

