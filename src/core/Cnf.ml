
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Reduction to CNF and simplifications} *)

module Hash = CCHash
module T = TypedSTerm
module F = T.Form

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

let clause_to_fo c =
  let ctx = FOTerm.Conv.create() in
  List.map (SLiteral.map ~f:(FOTerm.Conv.of_simple_term ctx)) c

let as_lit = SLiteral.of_form

(* check whether the formula is already in CNF *)
let as_clause f = match F.view f with
  | F.Or l -> List.map as_lit l
  | F.Not _
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
        if c < a || c < b || c > limit then TooBig else Exactly c

  let ( +/ ) = lift2 (+)
  let ( */ ) = lift2 ( * )

  (* comparison, but also assume that if both are too big, the first is bigger *)
  let geq_or_big e1 e2 = match e1, e2 with
    | TooBig, _ -> true
    | Exactly e', Exactly e'' -> e' >= e''
    | Exactly _, TooBig -> false
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
   definitions in the definitions (that has to be done later). *)
let introduce_defs ~ctx f =
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
      "@[<2>introduce@ def. @[%a@]@ for subformula @[%a@]@ with pol %a@]"
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
    let should_rename = match polarity with
      | `Pos ->
          E.(geq_or_big (a */ p true f) (a +/ p true f))
      | `Neg ->
          E.(geq_or_big (b */ p false f) (b +/ p false f))
      | `Both ->
          E.(geq_or_big
               (a */ p true f +/ b */ p false f)
               (a +/ b +/ p true f +/ p false f)
            )
    in
    if not (will_yield_lit f) && should_rename
    then _rename ~polarity f
    else f
  (* introduce definitions for subterms *)
  and maybe_rename_subformulas ~polarity a b f = match F.view f with
    | F.True
    | F.False
    | F.Atom _
    | F.Eq _
    | F.Neq _ -> f
    | F.And l ->
        let l' = List.mapi
            (fun i f' ->
               let a' = a in
               let b' = E.(b */ prod_p ~pos:false ~except:i l 0) in
               maybe_rename ~polarity a' b' f'
            ) l
        in
        F.and_ l'
    | F.Or l ->
        let l' = List.mapi
            (fun i f' ->
               let a' = E.(a */ prod_p ~pos:true ~except:i l 0) in
               let b' = b in
               maybe_rename ~polarity a' b' f'
            ) l
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
  maybe_rename ~polarity:`Pos (E.Exactly 1) (E.Exactly 0) f

(* helper: reduction to cnf using De Morgan laws. Returns a list of list of
   atomic formulas *)
let rec to_cnf_rec f = match F.view f with
  | F.Eq _
  | F.Neq _
  | F.Atom _
  | F.True
  | F.False
  | F.Not _ -> [[as_lit f]]
  | F.And l ->
      (* simply concat sub-CNF *)
      CCList.flat_map to_cnf_rec l
  | F.Or (f'::l) ->
      (* cartesian products of sub-CNF *)
      List.fold_left
        (fun cnf f' -> product (to_cnf_rec f') cnf)
        (to_cnf_rec f')
        l
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
  | F.Or [] -> assert false
(* cartesian product of lists of lists *)
and product a b =
  List.fold_left
    (fun acc litsa -> List.fold_left
        (fun acc' litsb -> (litsa @ litsb) :: acc')
        acc b)
    [] a

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
  let res = seq
    |> Sequence.flat_map
      (fun (f,annot) ->
         Util.debugf ~section 4 "@[<2>simplify and rename@ `@[%a@]`@]" (fun k->k T.pp f);
         (* preprocessing *)
         let f = List.fold_left (|>) f preprocess in
         (* simplification *)
         let f' =
           if disable_renaming || is_clause f
           then f
           else introduce_defs ~ctx f
         in
         let defs = Skolem.pop_new_definitions ~ctx in
         match defs with
         | [] -> Sequence.return (f',annot)
         | _::_ ->
            let defs = List.map
                (fun d ->
                  (* introduce the required definition, with polarity as needed *)
                  let f' = match d.Skolem.polarity with
                     | `Pos -> F.imply d.Skolem.proxy d.Skolem.form
                     | `Neg -> F.imply d.Skolem.form d.Skolem.proxy
                     | `Both -> F.equiv d.Skolem.proxy d.Skolem.form
                  in
                  f', annot
                )
                defs
            in
            Sequence.of_list ((f', annot) :: defs)
      )
    |> CCVector.of_seq ?init:None
  in
  Util.exit_prof prof_simplify_rename;
  res

type 'a statement = (clause, type_, 'a) Statement.t

(* Transform the clauses into proper CNF; returns a list of clauses *)
let cnf_of_seq ?(opts=[]) ?(ctx=Skolem.create ()) seq =
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
  CCVector.iter
    (fun (f, annot) ->
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
        (fun (id,ty) -> CCVector.push res (Statement.ty_decl ~src:annot id ty))
        new_ids;
      List.iter
        (fun c -> CCVector.push res (Statement.assert_ ~src:annot c))
        clauses;
    )
    v;
  (* return final vector of clauses *)
  CCVector.freeze res

let cnf_of ?opts ?ctx f annot =
  cnf_of_seq ?opts ?ctx (Sequence.return (f,annot))

let pp_statement out st =
  Statement.pp
    (Util.pp_list ~sep:" ∨ " (SLiteral.pp T.pp))
    T.pp out st

let type_declarations seq =
  Sequence.fold
    (fun acc st -> match Statement.view st with
      | Statement.TyDecl (id, ty) -> ID.Map.add id ty acc
      | Statement.Assert _ -> acc)
    ID.Map.empty seq

