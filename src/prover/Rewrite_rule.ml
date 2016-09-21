
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

(** {1 Rewrite Rules} *)
module T = FOTerm
module Stmt = Statement
module Su = Substs

let section = Util.Section.(make ~parent:zip "rewriting")

let stat_term_rw = Util.mk_stat "rw.term steps"
let stat_clause_rw = Util.mk_stat "rw.clause steps"

let prof_term_rw = Util.mk_profiler "rw.term"
let prof_clause_rw = Util.mk_profiler "rw.clause"

(* A rewrite rule, from [lhs] to [rhs]. *)
type rule_term = {
  lhs: T.t;
  rhs: T.t;
  lhs_db: T.t; (* lhs, after De Bruijn conversion *)
  rhs_db: T.t; (* rhs, same *)
}

(* constant rule [id := rhs] *)
let make_t_const id ty rhs =
  assert (T.vars_prefix_order rhs = []);
  let lhs = T.const ~ty id in
  { lhs; rhs; lhs_db=lhs; rhs_db=rhs; }

(* [id args := rhs] *)
let make_t id ty args rhs =
  let lhs = T.app (T.const ~ty id) args in
  assert (Type.equal (T.ty lhs) (T.ty rhs));
  let vars = T.vars_prefix_order lhs in
  assert (Sequence.for_all
      (fun v -> CCList.Set.mem ~eq:HVar.equal v vars) (T.Seq.vars rhs));
  { lhs; rhs; }

let rhs_term r = r.rhs

let pp_rule_term out r =
  Format.fprintf out "@[<2>@[%a@] -->@ @[%a@]@]" T.pp r.lhs T.pp r.rhs

type rule_clause = {
  c_lhs: Literal.t;
  c_rhs: Literal.t list list; (* list of clauses *)
}
(* invariant: all variables in [c_rhs] also occur in [c_lhs].

   For clauses we only rewrite step by step, so we can use the regular
   system of substitutions. *)

let make_c c_lhs c_rhs = {c_lhs; c_rhs}

let rhs_clause c = c.c_rhs

let pp_rule_clause out r =
  let pp_c = CCFormat.hvbox (Util.pp_list ~sep:" ∨ " Literal.pp) in
  Format.fprintf out "@[<2>@[%a@] ==>@ @[<v>%a@]@]"
    Literal.pp r.c_lhs (Util.pp_list pp_c) r.c_rhs

let compare_rule_term r1 r2 =
  CCOrd.(T.compare r1.lhs r2.lhs <?> (T.compare, r1.rhs, r2.rhs))

module Term_tree = struct
  type character =
    | Symbol of ID.t
    | BoundVariable of int
    | DB of int

  type iterator = {
    cur_char : character;
    cur_term : T.t;
    stack : T.t list list; (* skip: drop head, next: first of head *)
  }

  let char_to_int_ = function
    | Symbol _ -> 0
    | BoundVariable _ -> 1
    | DB _ -> 2

  let compare_char c1 c2 =
    match c1, c2 with
      | Symbol s1, Symbol s2 -> ID.compare s1 s2
      | BoundVariable i, BoundVariable j -> i - j
      | DB v1, DB v2 -> CCInt.compare v1 v2
      | _ -> char_to_int_ c1 - char_to_int_ c2

  let eq_char c1 c2 = compare_char c1 c2 = 0

  (** first symbol of t, or variable *)
  let term_to_char t =
    match T.Classic.view t with
      | T.Classic.Var v -> Variable v
      | T.Classic.DB i -> BoundVariable i
      | T.Classic.App (f, _) -> Symbol f
      | T.Classic.AppBuiltin _
      | T.Classic.NonFO -> Subterm t

  let open_term ~stack t =
    let cur_char = term_to_char t in
    match T.view t with
      | T.Var _
      | T.DB _
      | T.AppBuiltin _
      | T.Const _ ->
        {cur_char; cur_term=t; stack=[]::stack;}
      | T.App (_, l) ->
        {cur_char; cur_term=t; stack=l::stack;}

  let rec next_rec stack = match stack with
    | [] -> None
    | []::stack' -> next_rec stack'
    | (t::next')::stack' ->
      Some (open_term ~stack:(next'::stack') t)

  let skip iter = match iter.stack with
    | [] -> None
    | _next::stack' -> next_rec stack'

  let next iter = next_rec iter.stack

  (* convert term to list of var/symbol *)
  let to_list t =
    let rec getnext acc iter =
      let acc' = iter.cur_char :: acc in
      match next iter with
        | None -> List.rev acc'
        | Some iter' ->
          getnext acc' iter'
    in
    match iterate t with
      | None -> assert false
      | Some i -> getnext [] i


  type symbol =
    | S_app_builtin of Builtin.t
    | S_db of int (* bind/compare with given (bound) De bruijn *)
    | S_const of ID.t

  let compare_sym a b =
    let to_int = function
      | S_app_builtin _ -> 0
      | S_db _ -> 1 
      | S_const _ -> 2
    in
    match a, b with
      | S_app_builtin b1, S_app_builtin b2 -> Builtin.compare b1 b2
      | S_db i1, S_db i2 -> CCInt.compare i1 i2
      | S_const c1, S_const c2 -> ID.compare c1 c2
      | S_app_builtin _, _
      | S_db _, _
      | S_const _, _ -> CCInt.compare (to_int a) (to_int b)

  module Map_sym = CCMap.Make(struct
      type t = symbol
      let compare = compare_sym
    end)

  type t =
    | Empty
    | Leaf of rule_term (* first added *)
    | Branch of int * t Map_sym.t (* bind/match on given offset *)

  let empty = Empty

  let is_empty = function Empty ->  true | _ -> false

  module IntMap = CCMap.Make(CCInt)

  (* A pair [i, sym] means that we bind/match the De bruijn variable [i] with
     the symbol [i] *)
  type linearized = (int * symbol) list

  (* prefix traversal of the term, also returns a map
     from variables to De Bruijn indices. *)
  let linearize (t:T.t): linearized * int IntMap.t =
    let map = ref IntMap.empty in
    let offset = ref 0 in (* for real DB indices *)
    let rec aux i stack : linearized = match stack with
      | [] -> []
      | t :: stack ->
        begin match T.view t with
          | T.DB _ -> assert false
          | T.Const id -> (i, S_const id) :: aux (i+1) stack
          | T.Var v ->
            let v_id = HVar.id v in
            begin match IntMap.get v_id !map with
              | Some db ->
                assert (db <= i);
                (i, S_db db) :: aux (i+1) stack
              | None ->
                let db = !offset in
                incr offset;
                map := IntMap.add v_id db !map;
                (i, S_db db) :: aux (i+1) stack
            end
          | T.App (f, l) ->
            begin match T.view f with
              | T.Const id -> (i, S_const id) :: aux (i+1) (l @ stack)
              | _ -> assert false
            end
          | T.AppBuiltin (b,l) ->
            (i,S_app_builtin b) :: aux (i+1) (l @ stack)
        end
    in
    let l = aux 0 [t] in
    l, !map

  let replace_vars (subst:int IntMap.t)(t:T.t): T.t =
    let rec aux t = match T.view t with
      | T.Const _ -> t
      | T.App (f,l) -> T.app (aux f) (List.map aux l)
      | T.AppBuiltin (b,l) -> T.app_builtin ~ty:(T.ty t) b (List.map aux l)
      | T.Var v ->
        begin match IntMap.get (HVar.id v) subst with
          | Some db -> T.bvar ~ty:(T.ty t) db
          | None -> assert false
        end
      | T.DB _ -> assert false
    in aux t

  let add (r:rule_term) (t:t): t =
    let symbs, subst = linearize r.lhs in
    let r = { 
      lhs = replace_vars subst r.lhs;
      rhs = replace_vars subst r.rhs;
    } in
    (* db_env: is the DB already bound? *)
    let rec aux (db_env:bool DBEnv.t) (l:linearized) t: t =
      match l, t with
        | [], Leaf _ -> t (* already taken *)
        | [], Empty -> Leaf r
        | [], Branch _ -> assert false (* wrong arity *)
        | (i, s) :: tail, Empty ->
          let db_env = DBEnv.set db_env i true in
          Branch (i, Map_sym.singleton s (aux db_env tail Empty))
        | (i, s) :: tail, Branch (j, map) ->
          assert (i=j);
          let db_env = DBEnv.set db_env i true in
          let sub = Map_sym.get_or ~or_:Empty s map in
          let sub = aux db_env tail sub in
          Branch (j, Map_sym.add s sub map)
        | _ :: _, Leaf _ -> assert false (* wrong arity *)
    in
    aux DBEnv.empty symbs t

  (* find a matching occurrence for the given pattern.
     @return [Some (env, r)] if [env(r.lhs) = pat] *)
  let find (arg:T.t) (t:t): (T.t DBEnv.t * rule_term) option =
    (* env: bindings for De bruijn indices in the rule
       offsets: map offsets to subterms of the [arg]
       stack: prefix traversal of arg
       t: current subtree
    *)
    let rec aux
        (env:_ DBEnv.t)
        (offsets:T.t CCRAL.t)
        (subst:int IntMap.t) stack t = match stack, t with
      | [], Empty -> None
      | [], Leaf r -> Some (env, r)
      | [], Branch _ -> assert false
      | _::_, (Empty | Leaf _) -> assert false
      | t :: stack, Branch (i, map) ->
        let sym = match T.view t with
          | T.Const id -> S_const id
          | T.Var _
            begin

        end
    in
    aux DBEnv.empty pat [t]

  let to_seq t yield =
    let rec aux = function
      | Empty -> ()
      | Leaf rule -> yield rule
      | Branch (_, map) ->
        Map_sym.iter (fun _ t' -> aux t') map
    in
    aux t
end

module Set = struct

  type t = {
    terms: Term_tree.t;
    clauses: rule_clause list;
  }
  (* head symbol -> set of rules *)

  let empty = {
    terms=Term_tree.empty;
    clauses=[];
  }

  let is_empty t = Term_tree.is_empty t.terms && t.clauses=[]

  let add_term r s = {s with terms=Term_tree.add r s.terms}
  let add_clause r s = {s with clauses=r :: s.clauses}

  let add_stmt stmt t = match Stmt.view stmt with
    | Stmt.Def (id, ty, rhs) ->
      (* simple constant *)
      let r = make_t_const id ty rhs in
      Util.debugf ~section 5 "@[<2>add rewrite rule@ `@[%a@]`@]"
        (fun k->k pp_rule_term r);
      add_term r t
    | Stmt.RewriteTerm (id, ty, args, rhs) ->
      let r = make_t id ty args rhs in
      Util.debugf ~section 5 "@[<2>add rewrite rule@ `@[%a@]`@]"
        (fun k->k pp_rule_term r);
      add_term r t
    | Stmt.RewriteForm (lhs, rhs) ->
      let lhs = Literal.Conv.of_form lhs in
      let rhs = List.map (List.map Literal.Conv.of_form) rhs in
      let r = make_c lhs rhs in
      Util.debugf ~section 5 "@[<2>add rewrite rule@ `@[%a@]`@]"
        (fun k->k pp_rule_clause r);
      add_clause r t
    | Stmt.TyDecl _
    | Stmt.Data _
    | Stmt.Assert _
    | Stmt.Lemma _
    | Stmt.Goal _
    | Stmt.NegatedGoal _ -> t

  let to_seq_t t = Term_tree.to_seq t.terms
  let to_seq_c t = Sequence.of_list t.clauses

  let pp out t =
    Format.fprintf out "{@[<hv>%a@,%a@]}"
      (CCFormat.seq ~start:"" ~stop:"" pp_rule_term) (to_seq_t t)
      (CCFormat.seq ~start:"" ~stop:"" pp_rule_clause) (to_seq_c t)
end

(* TODO: {b long term}

   use De Bruijn  indices for rewrite rules, with RuleSet.t being a
   decision tree (similar to pattern-matching compilation) on head symbols
   + equality contraints for non-linear rules.

   Use the FOTerm.DB case extensively... *)

let normalize_term_ rules t =
  (* compute normal form of subterm
     @param k the continuation
     @return [t'] where [t'] is the normal form of [t] *)
  let rec reduce ~subst sc t k = match T.view t with
    | T.Const id ->
      (* pick a constant rule *)
      begin match Set.find_iter rules id |> Sequence.head with
        | None -> k t
        | Some r ->
          assert (T.is_const r.lhs);
          (* reduce [rhs], but no variable can be bound *)
          reduce ~subst:Substs.empty 0 r.rhs k
      end
    | T.App (f, l) ->
      (* first, reduce subterms *)
      reduce_l ~subst sc l
        (fun l' ->
           let t' = if T.same_l l l' then t else T.app f l' in
           match T.view f with
             | T.Const id ->
               let find_rule =
                 Set.find_iter rules id
                 |> Sequence.find
                   (fun r ->
                      try
                        let subst' =
                          Unif.FO.matching ~subst ~pattern:(r.lhs,1) (t',0)
                        in
                        Some (r, subst')
                      with Unif.Fail -> None)
               in
               begin match find_rule with
                 | None -> k t'
                 | Some (r, subst) ->
                   (* rewrite [t = r.lhs\sigma] into [rhs] (and normalize [rhs],
                      which contain variables bound by [subst]) *)
                   Util.debugf ~section 5
                     "@[<2>rewrite `@[%a@]`@ using `@[%a@]`@ with `@[%a@]`@]"
                     (fun k->k T.pp t' pp_rule_term r Su.pp subst);
                   Util.incr_stat stat_term_rw;
                   reduce ~subst 1 r.rhs k
               end
             | _ -> k t'
        )
    | T.DB _ -> k t
    | T.Var _ ->
      (* dereference, or return [t]. Careful not to traverse the already-
          evaluated value! *)
      k (fst (Su.FO.deref subst (t,sc)))
    | T.AppBuiltin (_,[]) -> k t
    | T.AppBuiltin (b,l) ->
      reduce_l ~subst sc l
        (fun l' ->
           let t' = if T.same_l l l' then t else T.app_builtin ~ty:(T.ty t) b l' in
           k t')
  (* reduce list *)
  and reduce_l ~subst sc l k = match l with
    | [] -> k []
    | t :: tail ->
      reduce_l ~subst sc tail
        (fun tail' -> reduce ~subst sc t
            (fun t' -> k (t' :: tail')))
  in
  reduce ~subst:Su.empty 0 t (fun t->t)

let normalize_term rules t =
  Util.with_prof prof_term_rw (normalize_term_ rules) t

let narrow_term ?(subst=Substs.empty) (rules,sc_r) (t,sc_t) = match T.view t with
  | T.Const _ -> Sequence.empty (* already normal form *)
  | T.App (f, _) ->
    begin match T.view f with
      | T.Const id ->
        Set.find_iter rules id
        |> Sequence.filter_map
          (fun r ->
             try Some (r, Unif.FO.unification ~subst (r.lhs,sc_r) (t,sc_t))
             with Unif.Fail -> None)
      | _ -> Sequence.empty
    end
  | T.Var _
  | T.DB _
  | T.AppBuiltin _ -> Sequence.empty

(* try to rewrite this literal, returning a list of list of lits instead *)
let step_lit rules lit =
  CCList.find_map
    (fun r ->
       let substs = Literal.matching ~pattern:(r.c_lhs,1) (lit,0) in
       match Sequence.head substs with
       | None -> None
       | Some subst -> Some (r.c_rhs, subst))
    rules

let normalize_clause_ rules lits =
  let eval_ll ~renaming subst (l,sc) =
    List.map
      (List.map
         (fun lit -> Literal.apply_subst ~renaming subst (lit,sc)))
      l
  in
  let step =
    CCList.find_mapi
      (fun i lit -> match step_lit rules.Set.clauses lit with
         | None -> None
         | Some (clauses,subst) ->
           Util.debugf ~section 5
             "@[<2>rewrite `@[%a@]`@ into `@[<v>%a@]`@ with @[%a@]@]"
             (fun k->k Literal.pp lit
                 CCFormat.(list (hvbox (Util.pp_list ~sep:" ∨ " Literal.pp))) clauses
                Substs.pp subst);
           Util.incr_stat stat_clause_rw;
           Some (i, clauses, subst))
      lits
  in
  match step with
    | None -> None
    | Some (i, clause_chunks, subst) ->
      let renaming = Substs.Renaming.create () in
      (* remove rewritten literal, replace by [clause_chunks], apply
         substitution (clause_chunks might contain other variables!),
         distribute to get a CNF again *)
      let lits = CCList.Idx.remove lits i in
      let lits = Literal.apply_subst_list ~renaming subst (lits,0) in
      let clause_chunks = eval_ll ~renaming subst (clause_chunks,1) in
      let clauses = List.map (fun new_lits -> new_lits @ lits) clause_chunks in
      Some clauses

let normalize_clause rules lits =
  Util.with_prof prof_clause_rw (normalize_clause_ rules) lits

let narrow_lit ?(subst=Substs.empty) (rules,sc_r) (lit,sc_lit) =
  Sequence.of_list rules.Set.clauses
  |> Sequence.flat_map
    (fun r ->
       Literal.unify ~subst (r.c_lhs,sc_r) (lit,sc_lit)
       |> Sequence.map (fun subst -> r, subst))
