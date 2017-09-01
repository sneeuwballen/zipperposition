
(*
Copyright (c) 2013-2014, Simon Cruanes
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

(** {1 Simple Resolution Prover} *)

open Logtk

module E = CCResult
module T = Term
module TS = TypedSTerm
module F = TypedSTerm.Form
module P = Logtk_parsers

(** Global signature (maps symbols such as "f", "parent_of" or "greater"
    to their type). Every symbol has exactly one type.
    The initial signature is the TPTP signature (logic connectives) *)
let _signature = ref Logtk.Signature.empty

(** We do not have to do anything about terms, because they are already
    defined in {! Logtk.Term}. Terms are either variables or
    applications of a constant (symbol) to a list of sub-terms.

    Examples (capitalized letter are variables):
      - f(X, g(X,a))
      - age_of(grandmother_of(frida))
      - Y
      - the_universe
*)

let conv_ty =
  let conv_ = Type.Conv.create () in
  fun t -> Type.Conv.of_simple_term_exn conv_ t

(** {2 Literals and Clauses} *)

(** A literal is an atomic proposition (term of type [$o], i.e. the type of
    propositions), or its negation. We represent this as a pair of [(term, sign)].

    Examples:
      - [(older_than(obama, bieber), true)]
      - [(lives_in(paris, poutine), false)]
*)
module Lit = struct
  type t = T.t * bool

  (** We also define a few basic comparison and printing functions.
      Comparison functions are used by many data structures;
      Printing is useful for informing the user of results or
      for debugfging. *)

  let compare = CCOrd.pair T.compare CCOrd.bool
  let equal a b = compare a b=0

  let pp out (t,b) = Format.fprintf out "%s%a" (if b then "" else "¬") T.pp t
end

(** A clause is a disjunction ("or") of literals. We simply use a list
    of literals.

    Example:
      [ (lives_in(paris, X), false); (eats_baguette(X), true) ]
      (means "forall X, if X lives in paris then X eats baguette")
*)
module Clause = struct
  type t = Lit.t list

  let make l = CCList.uniq ~eq:Lit.equal l
  let compare = CCOrd.list Lit.compare
  let equal a b = compare a b = 0

  (** A clause is trivial if it contains both a literal and its opposite.
      It means the clause is tautological, that is, always true; we can
      dispose of it because resolution is about {b refutation} (proving false) *)
  let is_trivial c =
    List.exists
      (fun (t,b) ->
         b &&
         List.exists (fun (t',b') -> not b' && T.equal t t') c
      ) c

  (** A substitution maps some variables to terms. Here this function will
      be used to {i apply} the substitution to a clause - replace
      variables of the clause by their image in the substitution (or keep
      them unchanged if they do not appear in the substitution.

      Substitutions are pre-defined in Logtk, and applying a substitution
      to a term is defined too (the function {!Subst.FO.apply} that
      applies a substitution to a first-order term)
  *)
  let apply_subst ~renaming subst (c, s_c) =
    make (List.map (fun (t,b) -> Subst.FO.apply ~renaming subst (t, s_c), b) c)

  (** printing a clause: print literals separated with "|" *)
  let pp out c = Util.pp_list ~sep:" | " Lit.pp out c

  (** Conversion from list of {!SLiteral.t}
      type: [Formula.t list -> clause] *)
  let _of_forms (c:T.t SLiteral.t list): t =
    let _atom f = match f with
      | SLiteral.Atom (t, sign) -> t, sign
      | _ ->
        failwith (CCFormat.sprintf "unsupported formula %a" (SLiteral.pp T.pp) f)
    in
    make (List.map _atom c)
end

(** A pair (clause, index) where the index refers to some literal
    in the clause. *)
module ClauseWithPos = struct
  type t = Clause.t * int
  let compare = CCOrd.pair Clause.compare CCInt.compare
end

(** {2 Saturation Algorithm} *)

(** An Index is a multimap from terms to (clause,index). Basically
    when we process a clause [c], for each literal [(term,sign)]
    at position [i] in the clause [c], we add
    [term -> (c, i)] into the index. Later we will be able to retrieve
    the pair [(c,i)] using any term that {i unifies} with [term]. *)
module Index = Logtk.NPDtree.MakeTerm(ClauseWithPos)

(** Set of clauses. Easy to define thanks to {!Clause.compare} *)
module ClauseSet = Set.Make(Clause)

(** State for one proof:
    - One term index (maps terms to the pairs [(clause, index of literal)]
      they appear in);
    - One set of processed ("active") clauses;
    - One queue of clauses yet to be processed. We process those clauses
      one by one.
*)
let _idx = ref (Index.empty())
let _active_set = ref ClauseSet.empty
let _passive_set = Queue.create()

exception Unsat
(** Raise this exception when the empty clause (i.e., "false")
    is found. This means the original clause set implies false
    and therefore that it's absurd. *)

(** add [c] to the passive set, if not already present in
    the active set nor it is trivial. *)
let _add_passive c =
  if c = [] then raise Unsat
  else if Clause.is_trivial c
  then (
    Util.debugf 4 "clause %a is trivial" (fun k->k Clause.pp c);
  )
  else if not (ClauseSet.mem c !_active_set)
  then (
    Util.debugf 4 "new passive clause %a" (fun k->k Clause.pp c);
    Queue.push c _passive_set
  )

(** When we process a clause [c], we put it into the active set
    (set of processed clauses). That also means every literal [(term,sign)]
    at index [i] will go into the index, so we can retrieve [c]
    by its literals later.
*)
let _add_active c =
  _active_set := ClauseSet.add c !_active_set;
  List.iteri
    (fun i (t,_) -> _idx := Index.add !_idx t (c,i))
    c

(** Factoring inference (takes one clause and deduce other clauses):

    {[
      A or A' or C
      ---------------
        sigma (A' or C)
          if sigma(A) = sigma(A')
    ]}

    means that if the clause has two positive literals (A,true) and (A',true)
    with some substitution sigma such that sigma(A)=sigma(A'), then
    we can {i factor} those literals into (sigma(A),true) provided
    we also apply sigma to the rest of the clause.
*)
let _factoring c =
  List.iteri
    (fun i (t,b) ->
       if b then List.iteri
           (fun j (t',b') ->
              (** Only try the inference if the two literals have positive sign.
                  The restriction [i < j] is used not to do the same inference
                  twice (symmetry).
              *)
              if i<j && b'
              then try
                  let subst = Unif.FO.unification (t, 0) (t', 0) in
                  (** Now we have subst(t)=subst(t'), the inference can proceed *)
                  let c' = CCList.remove_at_idx i c in
                  let renaming = Subst.Renaming.create() in
                  (** Build the conclusion of the inference (removing one
                      of the factored literals *)
                  let c' = Clause.apply_subst ~renaming subst (c',0) in
                  Util.debugf 3 "factoring of %a ----> %a" (fun k->k Clause.pp c Clause.pp c');
                  (** New clauses go into the passive set *)
                  _add_passive c'
                with Unif.Fail -> ()
           ) c
    ) c

(** Resolution inference rule, between the clause [C] and any clause [D]
    from the active set.

    {[
      A or C    ¬A' or D
      ------------------
        sigma(C or D)
          if sigma(A) = sigma(A')
    ]}

    This rule is called "resolution" and it's one of the first automated proof
    technique ever. If "resolves" together two complementary literals in
    two clauses (assuming those clauses do not share variables).

    Let us explain in the propositional case (ignoring variables), assuming
    [A = A']. The idea is, roughly:
      - we know that either [A] or either [not A] is true
        (excluded middle)
      - if [A] is true, it means that [not A' or D]
        can only be true if [D] is true (since [A=A'=true]). Therefore
        [D] must be true.
      - if [A] is false, then [A or C] can only be true if [C] is true;
        therefore [C] holds.
      - by excluded middle one of those must be true, so in any
        case [C or D] is true. Hence the conclusion.

    For the first-order case, we compute the {b most general unifier} or
    [A] and [A'] (if it exists), call this unifier substitution [sigma].
    Then, the reasoning is the same as in the propositional case since
    the literals are actually equal.

    Note: the "0" and "1" are {b scopes}, a trick I use to avoid actually
    renaming variables in one of the clauses. More details can be found
    in the documentation for {!Subst} or in the talk I (Simon) gave at PAAR
    2014.
*)
let _resolve_with c =
  List.iteri
    (fun i (t,b) ->
       (** Retrieve within the index, mappings [term -> (clause,index)]
           such that [term] unifies with [t]. 0 and 1 are again scopes. *)
       Index.retrieve_unifiables (!_idx,0) (t,1)
       |> Sequence.iter
         (fun (_t', (d,j), subst) ->
            let (_,b') = List.nth d j in
            (** We have found [_t'], and a pair [(d, j)] such
                that [d] is another clause, and the [j]-th literal of [d]
                begin [_t', b']).
                If [b] and [b'] are complementary we are in the case where
                resolution applies.
            *)
            if b<>b'
            then (
              let renaming = Subst.Renaming.create() in
              (** Build the conclusion clause, merging the remainders [c']
                  and [d'] (which live respectively in scope 1 and 0)
                  of the clauses together after applying the substitution. *)
              let concl =
                (let c' = CCList.remove_at_idx i c in
                 Clause.apply_subst ~renaming subst (c',1))
                @
                  (let d' = CCList.remove_at_idx j d in
                   Clause.apply_subst ~renaming subst (d',0))
              in
              (** Simplify the resulting clause (remove duplicate literals)
                  and add it into the passive set, to be processed later *)
              let concl = Clause.make concl in
              Util.debugf 3 "resolution of %a and %a ---> %a"
                (fun k->k Clause.pp c Clause.pp d Clause.pp concl);
              _add_passive concl
            )
         )
    ) c

(** Main saturation algorithm, a simple "given clause" loop. This is
    the outer loop of the resolution procedure: given an initial
    set of [clauses], the algorithm does:

    - add all the clauses into the passive set
    - while some passive clauses remain unprocessed, pick one of them,
        call it [c], and then do the following:
        - add [c] into the active set
        - perform inferences between [c] and the active set (including [c] itself)
        - add the resulting new clauses to the passive set.
    - if at any point the empty clause [[]] is found, then
      the initial set of clauses is unsatisfiable (absurd).
    - otherwise, if the loop stops, we have computed a fixpoint of the
      initial clauses with respect to inferences without finding [false],
      which means the original set of clauses is satisfiable (admits a model) *)
let _saturate clauses =
  List.iter _add_passive clauses;
  try
    while not (Queue.is_empty _passive_set) do
      let c = Queue.pop _passive_set in
      (** Is the clause [c] suitable for processing? It must not be processed
          yet and must not be trivial either. *)
      if not (Clause.is_trivial c) && not (ClauseSet.mem c !_active_set)
      then (
        Util.debugf 2 "given clause: %a" (fun k->k Clause.pp c);
        _add_active c;
        _resolve_with c;
        _factoring c;
      )
    done;
    `Sat
  with
    | Unsat -> `Unsat

(** {2 Main} *)

(** Read the problem to solve from the file [f], (try to) solve it
    and return the result. We use an error monad to make error
    handling easier (the function [>>=] is a {i monadic bind}). *)
let process_file f =
  Util.debugf 2 "process file %s..." (fun k->k f);
  let res = E.(
      (** parse the file in the format *)
      P.Parsing_utils.parse_tptp f
      (** Perform type inference and type checking (possibly updating
          the signature) *)
      >>= TypeInference.infer_statements ?ctx:None
      (** CNF ("clausal normal form"). We transform arbitrary first order
          formulas into a set of clauses (see the {!Clause} module)
          because resolution only works on clauses.

          This algorithm is already implemented in {!Logtk}. *)
      >>= fun st ->
      let decls = Cnf.cnf_of_seq ?ctx:None (CCVector.to_seq st) in
      _signature :=
        CCVector.to_seq decls
        |> Cnf.type_declarations
        |> ID.Map.map conv_ty;
      let clauses =
        CCVector.to_seq decls
        |> Cnf.convert
        |> CCVector.to_seq
        |> Sequence.flat_map Statement.Seq.forms
        |> Sequence.map Clause._of_forms
        |> Sequence.to_rev_list
      in
      (** Perform saturation (solve the problem) *)
      E.return (_saturate clauses)
    ) in
  match res with
    | E.Error msg ->
      print_endline msg;
      exit 1
    | E.Ok `Sat -> print_endline "sat"
    | E.Ok `Unsat -> print_endline "unsat"

(** Parse command-line arguments, including the file to process *)

let _options = ref (Options.make ())
let _help = "usage: resolution file.p"
let _file = ref None

let _set_file f = match !_file with
  | None -> _file := Some f
  | Some _ -> failwith "can only deal with one file"

let main () =
  Arg.parse !_options _set_file _help;
  match !_file with
    | None -> print_endline _help; exit 0
    | Some f -> process_file f

let () = main()
