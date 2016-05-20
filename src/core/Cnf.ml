
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Reduction to CNF and simplifications} *)

module Hash = CCHash
module T = TypedSTerm
module F = T.Form
module Stmt = Statement

let prof_estimate = Util.mk_profiler "cnf.estimate_num_clauses"
let prof_simplify_rename = Util.mk_profiler "cnf.simplify_rename"
let prof_to_cnf = Util.mk_profiler "cnf.distribute"
let prof_miniscope = Util.mk_profiler "cnf.miniscope"
let prof_skolemize = Util.mk_profiler "cnf.skolemize"

let section = Util.Section.make ~parent:Util.Section.zip "cnf"

type term = T.t
type type_ = T.t
type form = F.t
type lit = term SLiteral.t

exception Error of string

exception NotCNF of form

let () = Printexc.register_printer
  (function
    | Error msg -> Some (CCFormat.sprintf "@[<2>error in CNF:@ %s@]" msg)
    | NotCNF f -> Some (CCFormat.sprintf "@[<2>error:@ @[%a@]@ is not in CNF@]" T.pp f)
    | _ -> None)

let error_ msg = raise (Error msg)
let errorf_ msg = CCFormat.ksprintf msg ~f:error_
let not_cnf_ f = raise (NotCNF f)

type clause = lit list

let pp_clause out =
  Format.fprintf out "@[<2>%a@]" (Util.pp_list ~sep:" ∨ " (SLiteral.pp T.pp))

let clause_to_fo ?(ctx=FOTerm.Conv.create()) c =
  List.map (SLiteral.map ~f:(FOTerm.Conv.of_simple_term_exn ctx)) c

let as_lit = SLiteral.of_form

(* check whether the formula is already in CNF *)
let as_clause f = match F.view f with
  | F.Or l -> List.map as_lit l
  | F.Not _
  | F.Answer _
  | F.True
  | F.False
  | F.Atom _
  | F.Neq _
  | F.Eq _ -> [as_lit f]
  | F.And _
  | F.Equiv _
  | F.Xor _
  | F.Imply _
  | F.Forall _
  | F.Exists _ -> not_cnf_ f

let as_cnf f = match F.view f with
  | F.Or _ -> [as_clause f]
  | F.Not _
  | F.True
  | F.False
  | F.Answer _
  | F.Atom _
  | F.Neq _
  | F.Eq _ -> [[as_lit f]]
  | F.And l -> List.map as_clause l
  | F.Equiv _
  | F.Xor _
  | F.Imply _
  | F.Forall _
  | F.Exists _ -> not_cnf_ f

let is_clause f =
  try ignore (as_clause f); true
  with NotCNF _ | SLiteral.NotALit _ -> false

let is_cnf f =
  try ignore (as_cnf f); true
  with NotCNF _ | SLiteral.NotALit _ -> false

(* miniscoping (push quantifiers as deep as possible in the formula) *)
let miniscope ?(distribute_exists=false) f =
  Util.enter_prof prof_miniscope;
  (* recursive miniscoping *)
  let rec miniscope f = match F.view f with
    | F.Forall (var, f') ->
        begin match F.view f' with
          | F.And l ->
              (* forall x (and l) -> and (forall x f' \ f' in l) *)
              let l = List.map miniscope l in
              let with_v, without_v = List.partition (T.var_occurs ~var) l in
              F.and_ (List.map (F.forall var) with_v @ without_v)
          | F.Or l ->
              let l = List.map miniscope l in
              let with_v, without_v = List.partition (T.var_occurs ~var) l in
              F.or_ (F.forall var (F.or_ with_v) :: without_v)
          | _ -> F.forall var (miniscope f')
        end
    | F.Exists (var, f') ->
        begin match F.view f' with
          | F.And l ->
              let l = List.map miniscope l in
              let with_v, without_v = List.partition (T.var_occurs ~var) l in
              F.and_
                (F.exists var (F.and_ with_v) :: without_v)
          | F.Or l ->
              let l = List.map miniscope l in
              let with_v, without_v = List.partition (T.var_occurs ~var) l in
              (* see whether we push the existential into the formulas [with_v], in
                  which the variable occurs, or whether we keep it outside *)
              let with_v = if distribute_exists
                then List.map (F.exists var) with_v
                else [F.exists var (F.or_ with_v)]
              in
              F.or_ (with_v @ without_v)
          | _ -> F.exists var (miniscope f')
        end
    | F.And l -> F.and_ (List.map miniscope l)
    | F.Or l -> F.or_ (List.map miniscope l)
    | F.Imply (f1, f2) -> F.imply (miniscope f1) (miniscope f2)
    | F.Equiv (f1, f2) -> F.equiv (miniscope f1) (miniscope f2)
    | F.Xor (f1, f2) -> F.xor (miniscope f1) (miniscope f2)
    | F.Not f' -> F.not_ (miniscope f')
    | F.True
    | F.False
    | F.Neq _
    | F.Eq _
    | F.Answer _
    | F.Atom _ -> f
  in
  let res = miniscope f in
  Util.exit_prof prof_miniscope;
  res

(* negation normal form (also remove equivalence and implications). *)
let rec nnf f =
  Util.debugf ~section 5 "@[<2>nnf of@ `@[%a@]`@]" (fun k->k T.pp f);
  match F.view f with
  | F.Atom _
  | F.Neq _
  | F.Eq _
  | F.Answer _
  | F.True
  | F.False -> f
  | F.Not f' ->
      begin match F.view f' with
        | F.Not f'' -> nnf f''
        | F.Neq (a,b) -> F.eq a b
        | F.Eq (a,b) -> F.neq a b
        | F.And l ->
            F.or_ (List.map (fun f -> nnf (F.not_ f)) l)
        | F.Or l ->
            F.and_ (List.map (fun f -> nnf (F.not_ f)) l)
        | F.Xor (a,b) ->
            nnf (F.equiv a b)
        | F.Equiv (a,b) ->
            nnf (F.xor a b)
        | F.Imply (a,b) -> (* not (a=>b)  is a and not b *)
            nnf (F.and_ [a; F.not_ b])
        | F.Forall (var, f'') ->
            F.exists var (nnf (F.not_ f''))
        | F.Exists (var, f'') ->
            F.forall var (nnf (F.not_ f''))
        | F.True -> F.false_
        | F.False -> F.true_
        | F.Answer _
        | F.Atom _ -> f
      end
  | F.And l -> F.and_ (List.map nnf l)
  | F.Or l -> F.or_ (List.map nnf l)
  | F.Imply (f1, f2) ->
      nnf (F.or_ [ (F.not_ f1); f2 ])
  | F.Equiv(f1,f2) ->
      (* equivalence with positive polarity *)
      nnf (F.and_
             [ F.imply f1 f2; F.imply f2 f1 ])
  | F.Xor (f1,f2) ->
      (* equivalence with negative polarity *)
      nnf (F.and_
             [ F.or_ [f1; f2]; F.or_ [F.not_ f1; F.not_ f2] ])
  | F.Forall (var,f') -> F.forall var (nnf f')
  | F.Exists (var,f') -> F.exists var (nnf f')

let skolemize ~ctx f =
  Util.enter_prof prof_skolemize;
  let rec skolemize subst f = match F.view f with
    | F.And l -> F.and_ (List.map (skolemize subst) l)
    | F.Or l -> F.or_ (List.map (skolemize subst) l)
    | F.Not f' -> F.not_ (skolemize subst f')
    | F.Xor _
    | F.Imply _
    | F.Equiv _ -> error_ "can only skolemize a NNF formula"
    | F.Answer _
    | F.Atom _
    | F.Eq _
    | F.Neq _ -> T.Subst.eval subst f
    | F.True
    | F.False -> f
    | F.Exists (var,f') ->
        (* replace [v] by a fresh skolem term *)
        let t = Skolem.skolem_form ~ctx subst (Var.ty var) f in
        let subst = Var.Subst.add subst var t in
        Util.debugf 2 ~section "@[<2>bind %a to@ @[%a@]@]"
          (fun k->k Var.pp_full var T.pp t);
        skolemize subst f'
    | F.Forall (var,f') ->
        let var' = Var.copy var in
        let subst = Var.Subst.add subst var (T.var var') in
        skolemize subst f'
  in
  let res = skolemize f in
  Util.exit_prof prof_skolemize;
  res

(* For the following, we use "handbook of automated reasoning",
   chapter "compute small clause normal forms". We use a naive computation
   of clause sizes, but with a caching mechanism to block the exponential
   re-computation of sizes.
   The criterion for renaming is: if renaming makes less clauses, then
   always do it *)

(* estimation for a number of clauses *)
module Estimation = struct
  type t =
    | Exactly of int
    | TooBig

  let limit = max_int lsr 2

  (* lift "regular" operations on natural numbers *)
  let lift2 op a b = match a, b with
    | TooBig, _
    | _, TooBig -> TooBig
    | Exactly a, Exactly b ->
        let c = op a b in
        if (a>0 && b>0 && (c < a || c < b)) (* overflow *)
          || c > limit then TooBig else Exactly c

  let ( +/ ) = lift2 (+)
  let ( */ ) = lift2 ( * )

  (* comparison, but also assume that if both are too big, the first is bigger *)
  let geq_or_big e1 e2 = match e1, e2 with
    | TooBig, _ -> true
    | Exactly e', Exactly e'' -> e' >= e''
    | Exactly _, TooBig -> false

  let pp out = function
    | TooBig -> CCFormat.string out "<too big>"
    | Exactly n -> CCFormat.int out n
end

(* estimate the number of clauses needed by this formula. *)
let estimate_num_clauses ~pos f =
  Util.enter_prof prof_estimate;
  let module E = Estimation in
  (* recursive function.
     @param pos true if the formula is positive, false if it's negated *)
  let rec num pos f = match F.view f, pos with
    | F.Eq _, _
    | F.Neq _, _
    | F.Answer _, _
    | F.Atom _, _
    | F.True, _
    | F.False, _ -> E.Exactly 1
    | F.Not f', _ -> num (not pos) f'
    | F.And l, true -> sum_list pos l
    | F.And l, false -> prod_list pos l
    | F.Or l, true -> prod_list pos l
    | F.Or l, false -> sum_list pos l
    | F.Imply (a,b), true -> E.(num false a */ num true b)
    | F.Imply (a,b), false -> E.(num true a +/ num false b)
    | F.Equiv(a,b), true ->
        E.((num true a */ num false b) +/ (num false a */ num true b))
    | F.Equiv(a,b), false ->
        E.((num true a */ num true b) +/ (num false a */ num false b))
    | F.Xor(a,b), true ->
        (* a xor b is defined as  (not a) <=> b *)
        E.((num false a */ num false b) +/ (num true a */ num true b))
    | F.Xor(a,b), false ->
        E.((num false a */ num true b) +/ (num true a */ num false b))
    | F.Forall (_, f'), _
    | F.Exists (_, f'), _ -> num pos f'
  and sum_list pos l = match l with
    | [] -> E.Exactly 0
    | x :: tail -> E.(num pos x +/ sum_list pos tail)
  and prod_list pos l = match l with
    | [] -> E.Exactly 1
    | x :: tail -> E.(num pos x */ prod_list pos tail)
  in
  let n = num pos f in
  Util.exit_prof prof_estimate;
  n

(* atomic formula, or forall/exists/not an atomic formula (1 literal) *)
let rec will_yield_lit f = match F.view f with
  | F.Not f'
  | F.Exists (_, f')
  | F.Forall (_, f') -> will_yield_lit f'
  | F.Eq _
  | F.Neq _
  | F.Answer _
  | F.Atom _
  | F.True
  | F.False -> true
  | F.And _
  | F.Or _
  | F.Imply _
  | F.Equiv _
  | F.Xor _ -> false

(* introduce definitions for sub-formulas of [f], is needed. This might
   modify [ctx] by adding definitions to it, and it will {!NOT} introduce
   definitions in the definitions (that has to be done later).
   @param is_pos if true, polarity=pos, else polarity=false *)
let introduce_defs ~ctx ~is_pos f =
  let module E = Estimation in
  (* shortcut to compute the number of clauses *)
  let p pos f = estimate_num_clauses ~pos f in
  let _neg = function
    | `Pos -> `Neg
    | `Neg -> `Pos
    | `Both -> `Both
  (* rename formula *)
  and _rename ~polarity f =
    let p = Skolem.define ~ctx ~polarity f in
    Util.debugf ~section 4
      "@[<2>introduce@ def. @[%a@]@ for subformula `@[%a@]`@ with pol %a@]"
      (fun k->k T.pp p T.pp f Skolem.pp_polarity polarity);
    p
  in
  (* recurse in sub-formulas, renaming as needed.
     a is the multiplicative factor for the number of clauses of (cnf f)
     b is the multiplicative factor for the number of clauses of (cnf ~f)
     polarity is the polarity of f within the outermost formula *)
  let rec maybe_rename ~polarity a b f =
    let f = maybe_rename_subformulas ~polarity a b f in
    (* depending on polarity and subformulas, do renamings.
       The condition is (where p is the expected number of clauses):
       if pol=1,  a * p(F) >= a + p(F)
       if pol=-1, b * p(~F) >= b + p(~F)
       if pol=0, a * p(F) + b * p(~F) >= a + b + p(F) + p(~F)
    *)
    let c1, c2 = match polarity with
      | `Pos -> E.(a */ p true f, a +/ p true f)
      | `Neg -> E.(b */ p false f, b +/ p false f)
      | `Both ->
          E.( a */ p true f +/ b */ p false f
            , a +/ b +/ p true f +/ p false f)
    in
    let should_rename = E.geq_or_big c1 c2 in
    if not (will_yield_lit f) && should_rename
    then (
      let f' = _rename ~polarity f in
      Util.debugf ~section 5 "rename because pol=%a, a=%a, b=%a, c1=%a, c2=%a"
        (fun k->k Skolem.pp_polarity polarity E.pp a E.pp b E.pp c1 E.pp c2);
      f'
    ) else f
  (* introduce definitions for subterms *)
  and maybe_rename_subformulas ~polarity a b f = match F.view f with
    | F.True
    | F.False
    | F.Answer _
    | F.Atom _
    | F.Eq _
    | F.Neq _ -> f
    | F.And l ->
        let l' =
          List.mapi
            (fun i f' ->
               let a' = a in
               let b' = E.(b */ prod_p ~pos:false ~except:i l 0) in
               maybe_rename ~polarity a' b' f')
            l
        in
        F.and_ l'
    | F.Or l ->
        let l' =
          List.mapi
            (fun i f' ->
               let a' = E.(a */ prod_p ~pos:true ~except:i l 0) in
               let b' = b in
               maybe_rename ~polarity a' b' f')
            l
        in
        F.or_ l'
    | F.Not f' ->
        let a' = b and b' = a in
        F.not_ (maybe_rename ~polarity:(_neg polarity) a' b' f')
    | F.Imply (f1, f2) ->
        let f1' =
          let a' = b and b' = E.(a */ p true f2) in
          maybe_rename ~polarity:(_neg polarity) a' b' f1
        and f2' =
          let a' = E.(a */ p false f1) and b' = b in
          maybe_rename ~polarity a' b' f2
        in
        F.imply f1' f2'
    | F.Equiv (f1, f2) ->
        let f1' =
          let a' = E.(a */ p false f2 +/ b */ p true f2) in
          let b' = E.(a */ p true f2 +/ b */ p false f2) in
          maybe_rename ~polarity:`Both a' b' f1
        and f2' =
          let a' = E.(a */ p false f1 +/ b */ p true f1) in
          let b' = E.(a */ p true f1 +/ b */ p false f1) in
          maybe_rename ~polarity:`Both a' b' f2
        in
        F.equiv f1' f2'
    | F.Xor (f1, f2) ->
        (* we consider that f1 has reverted polarity *)
        let f1' =
          let b' = E.(a */ p false f2 +/ b */ p true f2) in
          let a' = E.(a */ p true f2 +/ b */ p false f2) in
          maybe_rename ~polarity:`Both a' b' f1
        and f2' =
          let a' = E.(a */ p true f1 +/ b */ p false f1) in
          let b' = E.(a */ p false f1 +/ b */ p true f1) in
          maybe_rename ~polarity:`Both a' b' f2
        in
        F.xor f1' f2'
    | F.Forall (var, f') ->
        F.forall var (maybe_rename ~polarity a b f')
    | F.Exists (var, f') ->
        F.exists var (maybe_rename ~polarity a b f')
  (* product of all (p ~pos x) for x in l if idx(x) != except *)
  and prod_p ~pos l i ~except = match l with
    | [] -> E.Exactly 1
    | x :: tail ->
        if i = except
        then prod_p ~pos tail (i+1) ~except
        else
          let p_x = estimate_num_clauses ~pos x in
          E.(p_x */ prod_p ~pos tail (i+1) ~except)
  in
  let polarity = if is_pos then `Pos else `Neg in
  maybe_rename ~polarity (E.Exactly 1) (E.Exactly 0) f

(* helper: reduction to cnf using De Morgan laws. Returns a list of list of
   atomic formulas *)
let rec to_cnf_rec f = match F.view f with
  | F.Eq _
  | F.Neq _
  | F.Answer _
  | F.Atom _
  | F.True
  | F.False
  | F.Not _ -> [[as_lit f]]
  | F.And l ->
      (* simply concat sub-CNF *)
      CCList.flat_map to_cnf_rec l
  | F.Or [] -> assert false
  | F.Or l ->
      Util.map_product ~f:to_cnf_rec l
  | F.Forall _
  | F.Exists _ ->
      errorf_
        "@[<2>Cnf.to_cnf_rec:@ can only clausify a skolemized formula,@ not @[%a@]@]"
        T.pp f
  | F.Xor _
  | F.Imply _
  | F.Equiv _ ->
      errorf_
        "@[<2>Cnf.to_cnf_rec:@ can only clausify a NNF formula,@ not @[%a@]@]"
        T.pp f

let to_cnf f =
  Util.enter_prof prof_to_cnf;
  let res = to_cnf_rec f in
  Util.exit_prof prof_to_cnf;
  res

type options =
  | DistributeExists
  | DisableRenaming
  | InitialProcessing of (form -> form) (** any processing, at the beginning *)
  | PostNNF of (form -> form)  (** any processing that keeps negation at leaves *)
  | PostSkolem of (form -> form) (** must not introduce variables nor negations *)

(* simplify formulas and rename them. May introduce new formulas *)
let simplify_and_rename ~ctx ~disable_renaming ~preprocess seq =
  Util.enter_prof prof_simplify_rename;
  (* add new declarations before [stmt] *)
  let add_decls src =
    let defs = Skolem.pop_new_definitions ~ctx in
    List.map
      (fun d ->
        (* introduce the required definition, with polarity as needed *)
        let f' = match d.Skolem.polarity with
           | `Pos -> F.imply d.Skolem.proxy d.Skolem.form
           | `Neg -> F.imply d.Skolem.form d.Skolem.proxy
           | `Both -> F.equiv d.Skolem.proxy d.Skolem.form
        in
        Stmt.assert_ ~src f')
      defs
  in
  (* convert a formula *)
  let conv_form ~is_goal f =
    Util.debugf ~section 4 "@[<2>simplify and rename@ `@[%a@]`@]" (fun k->k T.pp f);
    (* preprocessing *)
    let f = List.fold_left (|>) f preprocess in
    (* simplification *)
    if disable_renaming || is_clause f
     then f
     else introduce_defs ~is_pos:(not is_goal) ~ctx f
  in
  let res = seq
    |> Sequence.flat_map
      (fun st ->
        let src = st.Stmt.src in
        let new_st = match st.Stmt.view with
          | Stmt.Data _
          | Stmt.Def _
          | Stmt.RewriteTerm _
          | Stmt.TyDecl _ -> st
          | Stmt.RewriteForm (lhs, rhs) ->
            let rhs = List.map (conv_form ~is_goal:false) rhs in
            Stmt.rewrite_form ~src lhs rhs
          | Stmt.Assert f -> Stmt.assert_ ~src (conv_form ~is_goal:false f)
          | Stmt.Goal f -> Stmt.goal ~src (conv_form ~is_goal:true f)
          | Stmt.NegatedGoal _ -> assert false
        in
        match add_decls src with
        | [] -> Sequence.return new_st
        | l -> Sequence.of_list (new_st :: l)
      )
    |> CCVector.of_seq ?init:None
  in
  Util.exit_prof prof_simplify_rename;
  res

type 'a f_statement = (term, term, type_, 'a) Statement.t
(** A statement before CNF *)

type 'a c_statement = (clause, term, type_, 'a) Statement.t
(** A statement after CNF *)

let rec chop_exist_prefix_ f = match F.view f with
  | F.Exists (v, f') ->
    let vars, f' = chop_exist_prefix_ f' in
    v :: vars, f'
  | _ -> [], f

let id_ x = x

(* Transform the clauses into proper CNF; returns a list of clauses *)
let cnf_of_seq ?(opts=[]) ?(ctx=Skolem.create ()) ?(neg_src=id_) ?(cnf_src=id_) seq =
  (* read options *)
  let disable_renaming = List.mem DisableRenaming opts in
  let preprocess =
    CCList.filter_map
      (function InitialProcessing f -> Some f | _ -> None)
      opts
  and post_nnf =
    CCList.filter_map
      (function PostNNF f -> Some f | _ -> None)
      opts
  and post_skolem =
    CCList.filter_map
      (function PostSkolem f -> Some f | _ -> None)
      opts
  in
  (* simplify and introduce definitions *)
  let v = simplify_and_rename ~ctx ~disable_renaming ~preprocess seq in
  (* reduce the new formulas to CNF *)
  let res = CCVector.create () in
  let conv_form ~src f =
    Util.debugf ~section 4 "@[<2>reduce@ `@[%a@]`@ to CNF@]" (fun k->k T.pp f);
    let clauses =
      try as_cnf f
      with NotCNF _ | SLiteral.NotALit _ ->
        let f = nnf f in
        (* processing post-nnf *)
        let f = List.fold_left (|>) f post_nnf in
        Util.debugf ~section 4 "@[<2>... NNF:@ `@[%a@]`@]" (fun k->k T.pp f);
        let distribute_exists = List.mem DistributeExists opts in
        let f = miniscope ~distribute_exists f in
        Util.debugf ~section 4 "@[<2>... miniscoped:@ `@[%a@]`@]" (fun k->k T.pp f);
        (* adjust the variable counter to [f] before skolemizing *)
        let f = skolemize ~ctx Var.Subst.empty f in
        (* processing post-skolemization *)
        let f = List.fold_left (|>) f post_skolem in
        Util.debugf ~section 4 "@[<2>... skolemized:@ `@[%a@]`@]" (fun k->k T.pp f);
        let clauses = to_cnf f in
        Util.debugf ~section 4 "@[<2>... CNF:@ `@[%a@]`@]"
          (fun k->k (Util.pp_list ~sep:", " pp_clause) clauses);
        clauses
    in
    let new_ids = Skolem.pop_new_symbols ~ctx in
    List.iter
      (fun (id,ty) -> CCVector.push res (Stmt.ty_decl ~src id ty))
      new_ids;
    clauses
  in
  CCVector.iter
    (fun st ->
      let src = st.Stmt.src in
      match st.Stmt.view with
      | Stmt.Def (id,ty,t) ->
          CCVector.push res (Stmt.def ~src id ty t)
      | Stmt.RewriteTerm (id,ty,args,rhs) ->
          CCVector.push res (Stmt.rewrite_term ~src id ty args rhs)
      | Stmt.RewriteForm (lhs,rhs) ->
          (* polarized rewriting: make two rewrite rules, one positive,
             one negative.
             positive:   lhs <=> cnf(rhs)
             negative: ¬ lhs <=> cnf(¬ rhs) *)
          let src = cnf_src src in
          let c_pos = CCList.flat_map (conv_form ~src) rhs in
          CCVector.push res (Stmt.rewrite_form ~src lhs c_pos);
          let c_neg = conv_form ~src (F.not_ (F.and_ rhs)) in
          CCVector.push res (Stmt.rewrite_form ~src (SLiteral.negate lhs) c_neg);
      | Stmt.Data l ->
          CCVector.push res (Stmt.data ~src l)
      | Stmt.TyDecl (id,ty) ->
          CCVector.push res (Stmt.ty_decl ~src id ty)
      | Stmt.Assert f ->
          let src = cnf_src src in
          List.iter
            (fun c -> CCVector.push res (Stmt.assert_ ~src c))
            (conv_form ~src f)
      | Stmt.Goal f ->
          let vars, body = chop_exist_prefix_ f in
          (* add "answer" tuple if [f] has an existential prefix *)
          let f' = if vars=[]
            then F.not_ f
            else
              let ans = F.answer (List.map T.var vars) in
              F.forall_l vars (F.imply body ans)
          in
          let src = src |> neg_src |> cnf_src in
          let l = conv_form ~src f' in
          CCVector.push res (Stmt.neg_goal ~src l)
      | Stmt.NegatedGoal l ->
          let src = cnf_src src in
          let l = CCList.flat_map (conv_form ~src) l in
          CCVector.push res (Stmt.neg_goal ~src l)
    )
    v;
  (* return final vector of clauses *)
  CCVector.freeze res

let cnf_of ?opts ?ctx ?neg_src ?cnf_src st =
  cnf_of_seq ?opts ?ctx ?neg_src ?cnf_src (Sequence.return st)

let pp_f_statement out st = Statement.pp T.pp T.pp T.pp out st

let pp_c_statement out st =
  let pp_clause out c =
    Format.fprintf out "@[<hv>%a@]" (Util.pp_list ~sep:" ∨ " (SLiteral.pp T.pp)) c
  in
  Statement.pp pp_clause T.pp T.pp out st

let type_declarations seq =
  let open Statement in
  seq
  |> Sequence.flat_map Seq.ty_decls
  |> ID.Map.of_seq

let convert seq =
  let module A = UntypedAST in
  (* used for conversion *)
  let t_ctx = FOTerm.Conv.create() in
  let ty_ctx = Type.Conv.create() in
  let conv_t = FOTerm.Conv.of_simple_term_exn t_ctx in
  let conv_ty = Type.Conv.of_simple_term_exn ty_ctx in
  let conv_statement st =
    Util.debugf ~section 5
      "@[<2>@{<yellow>convert@}@ `@[%a@]`@]" (fun k->k pp_c_statement st);
    let attrs = Stmt.attrs st in
    let src = Stmt.src st in
    let res = match Stmt.view st with
      | Stmt.Goal c ->
          let c = clause_to_fo ~ctx:t_ctx c in
          Stmt.goal ~attrs ~src c
      | Stmt.NegatedGoal l ->
          let l = List.map (clause_to_fo ~ctx:t_ctx) l in
          Stmt.neg_goal ~attrs ~src l
      | Stmt.Assert c ->
          let c = clause_to_fo ~ctx:t_ctx c in
          Stmt.assert_ ~attrs ~src c
      | Stmt.Data l ->
          let l =
            List.map
              (Stmt.map_data ~ty:(Type.Conv.of_simple_term_exn ty_ctx))
              l
          in
          Stmt.data ~attrs ~src l
      | Stmt.Def (id, ty, t) ->
          let ty = Type.Conv.of_simple_term_exn ty_ctx ty in
          let t = conv_t t in
          Stmt.def ~attrs ~src id ty t
      | Stmt.RewriteTerm (id, ty, args, rhs) ->
        Stmt.rewrite_term ~attrs ~src id (conv_ty ty) (List.map conv_t args) (conv_t rhs)
      | Stmt.RewriteForm (lhs,rhs) ->
        Stmt.rewrite_form ~attrs ~src
          (SLiteral.map ~f:conv_t lhs)
          (List.map (clause_to_fo ~ctx:t_ctx) rhs)
      | Stmt.TyDecl (id, ty) ->
          let ty = conv_ty ty in
          Stmt.ty_decl ~attrs ~src id ty
    in
    Util.debugf ~section 3
      "@[@[<2>convert@ `@[%a@]`@]@ @[<2>into `@[%a@]`@]@]"
      (fun k->k pp_c_statement st Stmt.pp_clause res);
    res
  in
  Sequence.map conv_statement seq
  |> CCVector.of_seq
  |> CCVector.freeze

