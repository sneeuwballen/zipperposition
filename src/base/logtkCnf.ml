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

(** {1 Reduction to CNF and simplifications} *)

module Hash = CCHash
module T = LogtkFOTerm
module ST = LogtkScopedTerm
module S = LogtkSymbol
module F = LogtkFormula.FO

let prof_estimate = LogtkUtil.mk_profiler "cnf.estimate_num_clauses"
let prof_simplify_rename = LogtkUtil.mk_profiler "cnf.simplify_rename"
let prof_to_cnf = LogtkUtil.mk_profiler "cnf.distribute"
let prof_miniscope = LogtkUtil.mk_profiler "cnf.miniscope"
let prof_skolemize = LogtkUtil.mk_profiler "cnf.skolemize"

let section = LogtkUtil.Section.make ~parent:LogtkUtil.Section.logtk "cnf"

type form = LogtkFormula.FO.t
type symbol = LogtkSymbol.t

(* check whether the formula is already in CNF *)
let rec is_cnf f = match F.view f with
  | F.Or l -> List.for_all is_lit l
  | F.Not f' -> is_lit f
  | F.True
  | F.False
  | F.Atom _
  | F.Neq _
  | F.Eq _ -> true
  | F.And _
  | F.Equiv _
  | F.Xor _
  | F.Imply _
  | F.Forall _
  | F.Exists _
  | F.ForallTy _ -> false

and is_lit f = match F.view f with
  | F.Not f' -> F.is_atomic f'
  | F.Eq _
  | F.Neq _
  | F.Atom _
  | F.True
  | F.False -> true
  | F.Or _
  | F.And _
  | F.Equiv _
  | F.Xor _
  | F.Imply _
  | F.Forall _
  | F.Exists _
  | F.ForallTy _ -> false

let is_clause l = List.for_all is_lit l

let __reconvert t =
  match F.of_term t with
  | Some f -> f
  | None ->
      assert (T.is_term t);
      F.Base.atom (T.of_term_exn t)

let __unshift n f =
  __reconvert (ST.DB.unshift n (f : F.t :> ST.t))

let __contains_db n f =
  ST.DB.contains (f : F.t :> ST.t) n

(* miniscoping (push quantifiers as deep as possible in the formula) *)
let miniscope ?(distribute_exists=false) f =
  LogtkUtil.enter_prof prof_miniscope;
  (* recursive miniscoping *)
  let rec miniscope f = match F.view f with
  | F.Forall (varty, f') ->
    begin match F.view f' with
    | F.And l ->
      (* forall x (and l) -> and (forall x f' \ f' in l) *)
      let l = List.map miniscope l in
      let with_v, without_v = List.partition (__contains_db 0) l in
      F.Base.and_
        ( List.map (F.Base.__mk_forall ~varty) with_v
        @ List.map (__unshift 1) without_v)
    | F.Or l ->
      let l = List.map miniscope l in
      let with_v, without_v = List.partition (__contains_db 0) l in
      F.Base.or_
        (  F.Base.__mk_forall ~varty (F.Base.or_ with_v)
        :: List.map (__unshift 1) without_v)
    | _ -> F.Base.__mk_forall ~varty (miniscope f')
    end
  | F.Exists (varty, f') ->
    begin match F.view f' with
    | F.And l ->
      let l = List.map miniscope l in
      let with_v, without_v = List.partition (fun f -> __contains_db 0 f) l in
      F.Base.and_
        (  F.Base.__mk_exists ~varty (F.Base.and_ with_v)
        :: List.map (__unshift 1) without_v)
    | F.Or l ->
      let l = List.map miniscope l in
      let with_v, without_v = List.partition (fun f -> __contains_db 0 f) l in
      (* see whether we push the existential into the formulas [with_v], in
          which the variable occurs, or whether we keep it outside *)
      let with_v = if distribute_exists
        then List.map (F.Base.__mk_exists ~varty) with_v
        else [F.Base.__mk_exists ~varty (F.Base.or_ with_v)]
      in
      let without_v = List.map (__unshift 1) without_v in
      F.Base.or_ (with_v @ without_v)
    | _ -> F.Base.__mk_exists ~varty (miniscope f')
    end
  | F.ForallTy f' ->
    F.Base.__mk_forall_ty (miniscope f')  (* do not bother *)
  | F.And l -> F.Base.and_ (List.map miniscope l)
  | F.Or l -> F.Base.or_ (List.map miniscope l)
  | F.Imply (f1, f2) -> F.Base.imply (miniscope f1) (miniscope f2)
  | F.Equiv (f1, f2) -> F.Base.equiv (miniscope f1) (miniscope f2)
  | F.Xor (f1, f2) -> F.Base.xor (miniscope f1) (miniscope f2)
  | F.Not f' -> F.Base.not_ (miniscope f')
  | F.True
  | F.False
  | F.Neq _
  | F.Eq _
  | F.Atom _ -> f
  in
  let res = miniscope (F.simplify f) in
  LogtkUtil.exit_prof prof_miniscope;
  res

(* negation normal form (also remove equivalence and implications). *)
let rec nnf f =
  LogtkUtil.debug ~section 5 "nnf of %a..." F.pp f;
  match F.view f with
  | F.Atom _
  | F.Neq _
  | F.Eq _
  | F.True
  | F.False -> f
  | F.Not f' ->
      begin match F.view f' with
      | F.Not f'' -> nnf f''
      | F.Neq (a,b) -> F.Base.eq a b
      | F.Eq (a,b) -> F.Base.neq a b
      | F.And l ->
        F.Base.or_ (List.map (fun f -> nnf (F.Base.not_ f)) l)
      | F.Or l ->
        F.Base.and_ (List.map (fun f -> nnf (F.Base.not_ f)) l)
      | F.Xor (a,b) ->
        nnf (F.Base.equiv a b)
      | F.Equiv (a,b) ->
        nnf (F.Base.xor a b)
      | F.Imply (a,b) -> (* not (a=>b)  is a and not b *)
        nnf (F.Base.and_ [a; F.Base.not_ b])
      | F.Forall (varty, f'') ->
        F.Base.__mk_exists ~varty (nnf (F.Base.not_ f''))
      | F.Exists (varty, f'') ->
        F.Base.__mk_forall ~varty (nnf (F.Base.not_ f''))
      | F.ForallTy f' ->
        failwith "quantification on type variables in contravariant position"
      | F.True -> F.Base.false_
      | F.False -> F.Base.true_
      | F.Atom _ -> f
      end
  | F.And l -> F.Base.and_ (List.map nnf l)
  | F.Or l -> F.Base.or_ (List.map nnf l)
  | F.Imply (f1, f2) ->
    nnf (F.Base.or_ [ (F.Base.not_ f1); f2 ])
  | F.Equiv(f1,f2) ->
    (* equivalence with positive polarity *)
    nnf (F.Base.and_
      [ F.Base.imply f1 f2; F.Base.imply f2 f1 ])
  | F.Xor (f1,f2) ->
    (* equivalence with negative polarity *)
    nnf (F.Base.and_
      [ F.Base.or_ [f1; f2]; F.Base.or_ [F.Base.not_ f1; F.Base.not_ f2] ])
  | F.Forall (varty,f') -> F.Base.__mk_forall ~varty (nnf f')
  | F.Exists (varty,f') -> F.Base.__mk_exists ~varty (nnf f')
  | F.ForallTy f' -> F.Base.__mk_forall_ty (nnf f')

(* evaluate [f] in the given [env], and then unshift remaining free DB vars *)
let __eval_and_unshift env f =
  __reconvert (ST.DB.unshift 1 (ST.DB.eval env (f : F.t :> ST.t)))

(* TODO: replace universally quantified vars by free vars
   only *after* the skolemization of the subformula is done.
   LogtkSkolemization should proceed from the leaves (alpha-equiv checking
   is trivial with De Bruijn indices) up to the root. *)
let skolemize ~ctx f =
  LogtkUtil.enter_prof prof_skolemize;
  let rec skolemize f = match F.view f with
  | F.And l -> F.Base.and_ (List.map skolemize l)
  | F.Or l -> F.Base.or_ (List.map skolemize l)
  | F.Not f' -> F.Base.not_ (skolemize f')
  | F.Xor _
  | F.Imply _
  | F.Equiv _ -> failwith "can only skolemize a NNF formula"
  | F.Atom _
  | F.Eq _
  | F.Neq _
  | F.True
  | F.False -> f
  | F.Exists (ty,f') ->
    (* replace [v] by a fresh skolem term *)
    let new_f' = LogtkSkolem.skolem_form ~ctx ~ty f' in
    skolemize new_f'
  | F.Forall (ty,f') ->
    (* remove quantifier, replace by fresh variable *)
    F.iter (LogtkSkolem.update_var ~ctx) f';
    LogtkUtil.debug ~section 5 "type of variable: %a" LogtkType.pp ty;
    let v = T.var ~ty (LogtkSkolem.fresh_var ~ctx) in
    let env = LogtkDBEnv.singleton (v:T.t:>ST.t) in
    let new_f' = __eval_and_unshift env f' in
    skolemize new_f'
  | F.ForallTy f' ->
    F.iter (LogtkSkolem.update_var ~ctx) f';
    let v = LogtkType.var (LogtkSkolem.fresh_var ~ctx) in
    let env = LogtkDBEnv.singleton (v:LogtkType.t:> ST.t) in
    let new_f' = __eval_and_unshift env f' in
    skolemize new_f'
  in
  let res = skolemize f in
  LogtkUtil.exit_prof prof_skolemize;
  res

(** For the following, we use "handbook of automated reasoning",
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

  let pp buf n = match n with
    | TooBig -> Buffer.add_string buf "<too_big>"
    | Exactly n -> Printf.bprintf buf "%d" n
end

(* table (formula, polarity) -> int *)
module FPolTbl = Hashtbl.Make(struct
  type t = F.t * bool
  let hash_fun (f,x) h = Hash.bool_ x (F.hash_fun f h)
  let hash it = Hash.apply hash_fun it
  let equal (f1,x1)(f2,x2) = x1=x2 && F.eq f1 f2
end)

(* estimate the number of clauses needed by this formula. *)
let estimate_num_clauses ~cache ~pos f =
  LogtkUtil.enter_prof prof_estimate;
  let module E = Estimation in
  (* recursive function.
     @param pos true if the formula is positive, false if it's negated *)
  let rec num pos f =
    try
      FPolTbl.find cache (f,pos)
    with Not_found ->
      (* compute *)
      let n = match F.view f, pos with
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
        | F.Exists (_, f'), _
        | F.ForallTy f', _ -> num pos f'
      in
      (* memoize *)
      LogtkUtil.debug ~section 5 "estimated %a clauses (sign %B) for %a" E.pp n pos F.pp f;
      FPolTbl.add cache (f,pos) n;
      n
  and sum_list pos l = match l with
    | [] -> E.Exactly 0
    | x :: tail -> E.(num pos x +/ sum_list pos tail)
  and prod_list pos l = match l with
    | [] -> E.Exactly 1
    | x :: tail -> E.(num pos x */ prod_list pos tail)
  in
  let n = num pos f in
  LogtkUtil.exit_prof prof_estimate;
  n

(* atomic formula, or forall/exists/not an atomic formula (1 literal) *)
let rec will_yield_lit f = match F.view f with
  | F.Not f'
  | F.ForallTy f'
  | F.Exists (_, f')
  | F.Forall (_, f') -> will_yield_lit f'
  | _ -> F.is_atomic f

(* introduce definitions for sub-formulas of [f], is needed. This might
   modify [ctx] by adding definitions to it, and it will {!NOT} introduce
   definitions in the definitions (that has to be done later). *)
let introduce_defs ~ctx ~cache f =
  let module E = Estimation in
  (* shortcut to compute the number of clauses *)
  let p pos f = estimate_num_clauses ~cache ~pos f in
  let _neg = function
    | `Pos -> `Neg
    | `Neg -> `Pos
    | `Both -> `Both
  (* rename formula *)
  and _rename ~polarity f =
    let p = LogtkSkolem.get_definition ~ctx ~polarity f in
    LogtkUtil.debug ~section 4 "introduce def. %a for subformula %a" F.pp p F.pp f;
    p
  in
  (* recurse in sub-formulas, renaming as needed.
     a is the multiplicative factor for the number of clauses of (cnf f)
     b is the multiplicative factor for the number of clauses of (cnf ~f)
     polarity is the polarity of f within the outermost formula *)
  let rec maybe_rename ~polarity a b f =
    let f = maybe_rename_subformulas ~polarity a b f in
    (* check whether the formula is already defined! In which case, it's for free *)
    if LogtkSkolem.has_definition ~ctx f
    then (
      let atom = LogtkSkolem.get_definition ~ctx ~polarity f in
      LogtkUtil.debug ~section 5 "use definition %a for %a" F.pp atom F.pp f;
      atom
    )
    (* depending on polarity and subformulas, do renamings.
      The condition is (where p is the expected number of clauses):
      if pol=1,  a * p(F) >= a + p(F)
      if pol=-1, b * p(~F) >= b + p(~F)
      if pol=0, a * p(F) + b * p(~F) >= a + b + p(F) + p(~F)
    *)
    else
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
        F.Base.and_ l'
    | F.Or l ->
        let l' = List.mapi
          (fun i f' ->
            let a' = E.(a */ prod_p ~pos:true ~except:i l 0) in
            let b' = b in
            maybe_rename ~polarity a' b' f'
          ) l
        in
        F.Base.or_ l'
    | F.Not f' ->
        let a' = b and b' = a in
        F.Base.not_ (maybe_rename ~polarity:(_neg polarity) a' b' f')
    | F.Imply (f1, f2) ->
        let f1' =
          let a' = b and b' = E.(a */ p true f2) in
          maybe_rename ~polarity:(_neg polarity) a' b' f1
        and f2' =
          let a' = E.(a */ p false f1) and b' = b in
          maybe_rename ~polarity a' b' f2
        in
        F.Base.imply f1' f2'
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
        F.Base.equiv f1' f2'
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
        F.Base.xor f1' f2'
    | F.Forall (varty, f') ->
        F.Base.__mk_forall ~varty (maybe_rename ~polarity a b f')
    | F.Exists (varty, f') ->
        F.Base.__mk_exists ~varty (maybe_rename ~polarity a b f')
    | F.ForallTy f' ->
        F.Base.__mk_forall_ty (maybe_rename ~polarity a b f')
  (* product of all (p ~pos x) for x in l if idx(x) != except *)
  and prod_p ~pos l i ~except = match l with
    | [] -> E.Exactly 1
    | x :: tail ->
        if i = except
        then prod_p ~pos tail (i+1) ~except
        else
          let p_x = estimate_num_clauses ~cache ~pos x in
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
  | F.False -> [[f]]
  | F.Not f' ->
      begin match F.view f' with
      | F.Eq (a,b) -> [[F.Base.neq a b]]
      | F.Neq (a,b) -> [[F.Base.eq a b]]
      | _ when F.is_atomic f' ->[[f]]
      | _ -> failwith (LogtkUtil.sprintf "should be atomic: %a" F.pp f')
      end
  | F.And l ->
    (* simply concat sub-CNF *)
    LogtkUtil.list_flatmap to_cnf_rec l
  | F.Or (f'::l) ->
    (* cartesian products of sub-CNF *)
    List.fold_left
      (fun cnf f' -> product (to_cnf_rec f') cnf)
      (to_cnf_rec f')
      l
  | F.Forall _
  | F.Exists _
  | F.ForallTy _ -> failwith "Cnf.to_cnf_rec: can only clausify a skolemized formula"
  | F.Xor _
  | F.Imply _
  | F.Equiv _ -> failwith "Cnf.to_cnf_rec: can only clausify a NNF formula"
  | F.Or [] -> assert false
(* cartesian product of lists of lists *)
and product a b =
  List.fold_left
    (fun acc litsa -> List.fold_left
      (fun acc' litsb -> (litsa @ litsb) :: acc')
      acc b)
    [] a

let to_cnf l =
  LogtkUtil.enter_prof prof_to_cnf;
  let res = to_cnf_rec l in
  LogtkUtil.exit_prof prof_to_cnf;
  res

type clause = F.t list
  (** Basic clause representation, as list of literals *)

type options =
  | DistributeExists
  | DisableRenaming
  | InitialProcessing of (form -> form) (** any processing, at the beginning *)
  | PostNNF of (form -> form)  (** any processing that keeps negation at leaves *)
  | PostSkolem of (form -> form) (** must not introduce variables nor negations *)

(* simplify formulas and rename them. May introduce new formulas *)
let simplify_and_rename ~ctx ~cache ~disable_renaming ~preprocess l =
  LogtkUtil.enter_prof prof_simplify_rename;
  let l' = List.map
    (fun f ->
      let f = F.flatten f in
      (* preprocessing *)
      let f = List.fold_left (|>) f preprocess in
      (* simplification *)
      let f = F.simplify f in
      if disable_renaming || is_cnf f
        then f
        else introduce_defs ~ctx ~cache f)
    l
  in
  (* add the new definitions to the list of formulas to reduce to CNF *)
  let defs = LogtkSkolem.pop_new_definitions ~ctx in
  let defs = List.map
    (fun d ->
      (* introduce the required definition, with polarity as needed *)
      let f = match !(d.LogtkSkolem.polarity) with
        | `Pos -> F.Base.imply d.LogtkSkolem.proxy d.LogtkSkolem.form
        | `Neg -> F.Base.imply d.LogtkSkolem.form d.LogtkSkolem.proxy
        | `Both -> F.Base.equiv d.LogtkSkolem.proxy d.LogtkSkolem.form
      in
      F.simplify f)
    defs
  in
  let res = List.rev_append defs l' in
  LogtkUtil.exit_prof prof_simplify_rename;
  res

(* LogtkTransform the clauses into proper CNF; returns a list of clauses *)
let cnf_of_list ?(opts=[]) ?(ctx=LogtkSkolem.create LogtkSignature.empty) l =
  let acc = ref [] in
  (* read options *)
  let disable_renaming = List.mem DisableRenaming opts in
  let preprocess = LogtkUtil.list_fmap
    (function InitialProcessing f -> Some f | _ -> None)
    opts
  and post_nnf = LogtkUtil.list_fmap
    (function PostNNF f -> Some f | _ -> None)
    opts
  and post_skolem = LogtkUtil.list_fmap
    (function PostSkolem f -> Some f | _ -> None)
    opts
  in
  let cache = FPolTbl.create 128 in
  (* simplify and introduce definitions *)
  let l = simplify_and_rename ~ctx ~cache ~disable_renaming
    ~preprocess l in
  (* reduce the new formulas to CNF *)
  List.iter (fun f ->
    LogtkUtil.debug ~section 4 "reduce %a to CNF..." F.pp f;
    let clauses = if is_cnf f
      then
        match F.view f with
        | F.Or l -> [l]  (* works because [f] flattened before *)
        | F.False
        | F.True
        | F.Eq _
        | F.Neq _
        | F.Atom _ -> [[f]]
        | F.Not f'  ->
          begin match F.view f' with
          | F.Eq (a,b) -> [[F.Base.neq a b]]
          | F.Neq (a,b) -> [[F.Base.eq a b]]
          | _ when F.is_atomic f' ->[[f]]
          | _ -> failwith (LogtkUtil.sprintf "should be atomic: %a" F.pp f')
          end
        | F.Equiv _
        | F.Imply _
        | F.Xor _
        | F.And _
        | F.Forall _
        | F.Exists _
        | F.ForallTy _ -> assert false
      else begin
        let f = F.simplify f in
        LogtkUtil.debug ~section 4 "... simplified: %a" F.pp f;
        let f = nnf f in
        (* processing post-nnf *)
        let f = List.fold_left (|>) f post_nnf in
        LogtkUtil.debug ~section 4 "... NNF: %a" F.pp f;
        let distribute_exists = List.mem DistributeExists opts in
        let f = miniscope ~distribute_exists f in
        LogtkUtil.debug ~section 4 "... miniscoped: %a" F.pp f;
        (* adjust the variable counter to [f] before skolemizing *)
        LogtkSkolem.clear_var ~ctx;
        F.iter (LogtkSkolem.update_var ~ctx) f;
        let f = skolemize ~ctx f in
        (* processing post-skolemization *)
        let f = List.fold_left (|>) f post_skolem in
        LogtkUtil.debug ~section 4 "... skolemized: %a" F.pp f;
        let clauses = to_cnf f in
        LogtkUtil.debug ~section 4 "... CNF: %a"
          (LogtkUtil.pp_list ~sep:", " (LogtkUtil.pp_list ~sep:" | " F.pp)) clauses;
        assert (List.for_all (List.for_all F.is_closed) clauses);
        clauses
      end
    in
    assert (List.for_all is_clause clauses);
    acc := List.rev_append clauses !acc
  ) l;
  !acc

let cnf_of ?opts ?ctx f = cnf_of_list ?opts ?ctx [f]
