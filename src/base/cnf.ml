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

module T = FOTerm
module ST = ScopedTerm
module S = Symbol
module F = Formula.FO

type form = Formula.FO.t

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
  miniscope (F.simplify f)

(* negation normal form (also remove equivalence and implications). *)
let rec nnf f =
  Util.debug 5 "nnf of %a..." F.pp f;
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
 * only *after* the skolemization of the subformula is done.
 * Skolemization should proceed from the leaves (alpha-equiv checking
 * is trivial with De Bruijn indices) up to the root. *)
let skolemize ~ctx f =
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
    let new_f' = Skolem.skolem_form ~ctx ~ty f' in
    skolemize new_f'
  | F.Forall (ty,f') ->
    (* remove quantifier, replace by fresh variable *)
    F.iter (Skolem.update_var ~ctx) f';
    Util.debug 5 "type of variable: %a" Type.pp ty;
    let v = T.var ~ty (Skolem.fresh_var ~ctx) in
    let env = DBEnv.singleton (v:T.t:>ST.t) in
    let new_f' = __eval_and_unshift env f' in
    skolemize new_f'
  | F.ForallTy f' ->
    F.iter (Skolem.update_var ~ctx) f';
    let v = Type.var (Skolem.fresh_var ~ctx) in
    let env = DBEnv.singleton (v:Type.t:> ST.t) in
    let new_f' = __eval_and_unshift env f' in
    skolemize new_f'
  in
  skolemize f

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

  let sum l = List.fold_left (+/) (Exactly 0) l
  let prod l = List.fold_left ( */) (Exactly 1) l

  (* comparison: is e > n? *)
  let gt e n = match e with
    | TooBig -> true
    | Exactly e' -> e' > n

  let pp buf n = match n with
    | TooBig -> Buffer.add_string buf "<too_big>"
    | Exactly n -> Printf.bprintf buf "%d" n
end

(* table (formula, polarity) -> int *)
module FPolTbl = Hashtbl.Make(struct
  type t = F.t * bool
  let hash (f, x) =
    if x then F.hash f else Hash.combine 13 (F.hash f)
  let equal (f1,x1)(f2,x2) = x1=x2 && F.eq f1 f2
end)

(* estimate the number of clauses needed by this formula. *)
let estimate_num_clauses ~cache ~pos f =
  let module E = Estimation in
  (* recursive function.
     @param pos true if the formula is positive, false if it's negated *)
  let rec num pos f =
    try
      FPolTbl.find cache (f,pos)
    with Not_found ->
      (* compute *)
      let n = match F.view f with
        | F.Eq _
        | F.Neq _
        | F.Atom _
        | F.True
        | F.False -> E.Exactly 1
        | F.Not f' -> num (not pos) f'
        | F.And l ->
            let l' = List.rev_map (num pos) l in
            if pos then E.sum l' else E.prod l'
        | F.Or l ->
            let l' = List.rev_map (num pos) l in
            if pos then E.prod l' else E.sum l'
        | F.Imply (a,b) ->
            if pos
              then E.(num false a */ num true b)
              else E.(num true a +/ num false b)
        | F.Equiv(a,b) ->
            if pos
              then E.((num true a */ num false b) +/ (num false a */ num true b))
              else E.((num true a */ num true b) +/ (num false a */ num false b))
        | F.Xor(a,b) ->
            (* a xor b is defined as  (not a) <=> b *)
            if pos
              then E.((num false a */ num false b) +/ (num true a */ num true b))
              else E.((num false a */ num true b) +/ (num true a */ num false b))
        | F.Forall (_, f')
        | F.Exists (_, f')
        | F.ForallTy f' -> num pos f'
      in
      (* memoize *)
      Util.debug 5 "estimated %a clauses (sign %B) for %a" E.pp n pos F.pp f;
      FPolTbl.add cache (f,pos) n;
      n
  in
  num pos f

(* introduce definitions for sub-formulas of [f], is needed. This might
 * modify [ctx] by adding definitions to it, and it will {!NOT} introduce
 * definitions in the definitions (that has to be done later). *)
let introduce_defs ~ctx ~cache ~limit f =
  let _neg = function
    | `Pos -> `Neg
    | `Neg -> `Pos
    | `Both -> `Both
  and should_rename ~polarity ~limit f =
    match polarity with
    | `Pos ->
      Estimation.gt (estimate_num_clauses ~cache ~pos:true f) limit
    | `Neg ->
      Estimation.gt (estimate_num_clauses ~cache ~pos:false f) limit
    | `Both ->
        Estimation.(gt
          (estimate_num_clauses ~cache ~pos:true f +/
           estimate_num_clauses ~cache ~pos:false f)
        limit)
  in
  (* rename [f] if its count of clauses is above the limit *)
  let rename_if_above_limit ~polarity ~limit f =
    if should_rename ~polarity ~limit f
      then begin
        let p = Skolem.get_definition ~ctx ~polarity f in
        Util.debug 4 "introduce def. %a for subformula %a" F.pp p F.pp f;
        p
      end
      else f
  in
  let rec recurse ~polarity f =
    (* first introduce definitions for subterms *)
    let f = match F.view f with
      | F.True
      | F.False
      | F.Atom _
      | F.Eq _
      | F.Neq _ -> f
      | F.And l -> F.Base.and_ (List.map (recurse ~polarity) l)
      | F.Or l -> F.Base.or_ (List.map (recurse ~polarity) l)
      | F.Not f' -> F.Base.not_ (recurse ~polarity:(_neg polarity) f')
      | F.Imply (f1, f2) ->
          F.Base.imply
            (recurse ~polarity:(_neg polarity) f1)
            (recurse ~polarity f2)
      | F.Equiv (f1, f2) ->
          F.Base.equiv
            (recurse ~polarity:`Both f1)
            (recurse ~polarity:`Both f2)
      | F.Xor (f1, f2) ->
          F.Base.xor
            (recurse ~polarity:`Both f1)
            (recurse ~polarity:`Both f2)
      | F.Forall (varty, f') ->
          F.Base.__mk_forall ~varty (recurse ~polarity f')
      | F.Exists (varty, f') ->
          F.Base.__mk_exists ~varty (recurse ~polarity f')
      | F.ForallTy f' ->
          F.Base.__mk_forall_ty (recurse ~polarity f')
    in
    (* check whether the formula is already defined! *)
    if Skolem.has_definition ~ctx f
    then begin
      let p = Skolem.get_definition ~ctx ~polarity f in
      Util.debug 5 "use definition %a for %a" F.pp p F.pp f;
      p
    end
    else
      (* depending on polarity and subformulas, do renamings *)
      match F.view f, polarity with
      | F.And l, (`Neg | `Both) ->
          let l' = List.map (rename_if_above_limit ~polarity ~limit) l in
          let f = F.Base.and_ l' in
          (* maybe there are 15 formulas that give 2 clauses each! In that
             case we need to rename them all *)
          if should_rename ~polarity ~limit:1 f
          then F.Base.and_
            (List.map (rename_if_above_limit ~polarity ~limit:1) l')
          else f
      | F.Or l, (`Pos | `Both) ->
          let l' = List.map (rename_if_above_limit ~polarity ~limit) l in
          let f = F.Base.or_ l' in
          (* maybe there are 15 formulas that give 2 clauses each! In that
             case we need to rename them all *)
          if should_rename ~polarity ~limit:1 f
            then F.Base.or_
              (List.map (rename_if_above_limit ~polarity ~limit:1) l')
            else f
      | F.Equiv(f1, f2), _ ->
          F.Base.equiv
            (rename_if_above_limit ~polarity:`Both ~limit f1)
            (rename_if_above_limit ~polarity:`Both ~limit f2)
      | F.Xor(f1, f2), _ ->
          F.Base.xor
            (rename_if_above_limit ~polarity:`Both ~limit f1)
            (rename_if_above_limit ~polarity:`Both ~limit f2)
      | F.Imply(f1, f2), `Pos ->
          F.Base.imply
            (rename_if_above_limit ~polarity:`Neg ~limit f1)
            (rename_if_above_limit ~polarity:`Pos ~limit f2)
      | F.Imply(f1, f2), `Both ->
          F.Base.imply
            (rename_if_above_limit ~polarity:`Pos ~limit f1)
            (rename_if_above_limit ~polarity:`Neg ~limit f2)
      | F.Not f', _ ->
          let f' = rename_if_above_limit ~polarity:(_neg polarity) ~limit f' in
          F.Base.not_ f'
      | _ -> f (* do not rename *)
  in
  recurse ~polarity:`Pos f

(* helper: reduction to cnf using De Morgan laws. Returns a list of list of
  atomic formulas *)
let rec to_cnf f = match F.view f with
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
      | _ -> failwith (Util.sprintf "should be atomic: %a" F.pp f')
      end
  | F.And l ->
    (* simply concat sub-CNF *)
    Util.list_flatmap to_cnf l
  | F.Or (f'::l) ->
    (* cartesian products of sub-CNF *)
    List.fold_left
      (fun cnf f' -> product (to_cnf f') cnf)
      (to_cnf f')
      l
  | F.Forall _
  | F.Exists _
  | F.ForallTy _ -> failwith "Cnf.to_cnf: can only clausify a skolemized formula"
  | F.Xor _
  | F.Imply _
  | F.Equiv _ -> failwith "Cnf.to_cnf: can only clausify a NNF formula"
  | F.Or [] -> assert false
(* cartesian product of lists of lists *)
and product a b =
  List.fold_left
    (fun acc litsa -> List.fold_left
      (fun acc' litsb -> (litsa @ litsb) :: acc')
      acc b)
    [] a

type clause = F.t list
  (** Basic clause representation, as list of literals *)

type options =
  | DistributeExists
  | DisableRenaming
  | InitialProcessing of (form -> form) (** any processing, at the beginning *)
  | PostNNF of (form -> form)  (** any processing that keeps negation at leaves *)
  | PostSkolem of (form -> form) (** must not introduce variables nor negations *)
  | DefLimit of int  (* limit size above which names are used *)

let default_def_limit = 24

(* simplify formulas and rename them. May introduce new formulas *)
let simplify_and_rename ~ctx ~cache ~disable_renaming ~preprocess ~limit l =
  let l' = List.map
    (fun f ->
      let f = F.flatten f in
      (* preprocessing *)
      let f = List.fold_left (|>) f preprocess in
      (* simplification *)
      let f = F.simplify f in
      if disable_renaming || is_cnf f
        then f
        else introduce_defs ~ctx ~cache ~limit f)
    l
  in
  (* add the new definitions to the list of formulas to reduce to CNF *)
  let defs = Skolem.pop_new_definitions ~ctx in
  let defs = List.map
    (fun d ->
      (* introduce the required definition, with polarity as needed *)
      let f = match !(d.Skolem.polarity) with
        | `Pos -> F.Base.imply d.Skolem.proxy d.Skolem.form
        | `Neg -> F.Base.imply d.Skolem.form d.Skolem.proxy
        | `Both -> F.Base.equiv d.Skolem.proxy d.Skolem.form
      in
      F.simplify f)
    defs
  in
  List.rev_append defs l'

(* Transform the clauses into proper CNF; returns a list of clauses *)
let cnf_of_list ?(opts=[]) ?(ctx=Skolem.create Signature.empty) l =
  let acc = ref [] in
  (* read options *)
  let disable_renaming = List.mem DisableRenaming opts in
  let preprocess = Util.list_fmap
    (function InitialProcessing f -> Some f | _ -> None)
    opts
  and post_nnf = Util.list_fmap
    (function PostNNF f -> Some f | _ -> None)
    opts
  and post_skolem = Util.list_fmap
    (function PostSkolem f -> Some f | _ -> None)
    opts
  in
  (* definition limit *)
  let def_limit = List.fold_left
    (fun acc opt -> match opt with
      | DefLimit i -> i
      | _ -> acc)
    default_def_limit opts
  in
  let cache = FPolTbl.create 128 in
  (* simplify and introduce definitions *)
  let l = simplify_and_rename ~ctx ~cache ~disable_renaming
    ~preprocess ~limit:def_limit l in
  (* reduce the new formulas to CNF *)
  List.iter (fun f ->
    Util.debug 4 "reduce %a to CNF..." F.pp f;
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
          | _ -> failwith (Util.sprintf "should be atomic: %a" F.pp f')
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
        Util.debug 4 "... simplified: %a" F.pp f;
        let f = nnf f in
        (* processing post-nnf *)
        let f = List.fold_left (|>) f post_nnf in
        Util.debug 4 "... NNF: %a" F.pp f;
        let distribute_exists = List.mem DistributeExists opts in
        let f = miniscope ~distribute_exists f in
        Util.debug 4 "... miniscoped: %a" F.pp f;
        (* adjust the variable counter to [f] before skolemizing *)
        Skolem.clear_var ~ctx;
        F.iter (Skolem.update_var ~ctx) f;
        let f = skolemize ~ctx f in
        (* processing post-skolemization *)
        let f = List.fold_left (|>) f post_skolem in
        Util.debug 4 "... skolemized: %a" F.pp f;
        let clauses = to_cnf f in
        Util.debug 4 "... CNF: %a"
          (Util.pp_list ~sep:", " (Util.pp_list ~sep:" | " F.pp)) clauses;
        clauses
      end
    in
    assert (List.for_all is_clause clauses);
    acc := List.rev_append clauses !acc
  ) l;
  !acc

let cnf_of ?opts ?ctx f = cnf_of_list ?opts ?ctx [f]
