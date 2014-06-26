
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

(** {1 Meta-Level reasoner} *)

open Logtk

module HOT = HOTerm

(** {2 Meta-level property}
A meta-level statement is just a higher-order term. *)

type term = HOTerm.t
type property = term
type fact = term

let property_ty = Type.const (Symbol.of_string "property")

(** {2 Meta-Level clause}
A Horn clause about meta-level properties *)

module Clause = struct
  type t = {
    head : term;
    body : term list;
  }
  type clause = t

  let safe head body =
    let vars_body = Sequence.flatMap HOT.Seq.vars (Sequence.of_list body) in
    HOT.Seq.vars head
      |> Sequence.for_all (fun v -> Sequence.exists (HOT.eq v) vars_body)

  let rule head body =
    if not (safe head body) then
      let msg = Util.sprintf "unsafe Horn clause: %a <- %a"
        HOT.pp head (Util.pp_list HOT.pp) body in
      raise (Invalid_argument msg)
    else {head; body; }

  let fact head = rule head []

  let is_fact t = t.body = []

  let eq c1 c2 =
    HOT.eq c1.head c2.head &&
    (try List.for_all2 HOT.eq c1.body c2.body with Invalid_argument _ -> false)

  let pop_body c = match c.body with
    | [] -> failwith "Clause.pop_body"
    | _::body' -> {c with body=body'; }

  let apply_subst subst ~renaming c s_c =
    let head = Substs.HO.apply subst ~renaming c.head s_c in
    let body = List.map (fun t -> Substs.HO.apply subst ~renaming t s_c) c.body in
    { head; body; }

  module Seq = struct
    let terms c k =
      k c.head; List.iter k c.body
    let vars c = terms c |> Sequence.flatMap HOT.Seq.vars
  end

  let hash_fun c h = Hash.seq HOT.hash_fun (Seq.terms c) h
  let hash c = Hash.apply hash_fun c

  let cmp c1 c2 =
    let c = HOT.cmp c1.head c2.head in
    if c = 0
      then Util.lexicograph HOT.cmp c1.body c2.body
      else c

  let pp buf c = match c.body with
    | [] -> Printf.bprintf buf "%a." HOT.pp c.head
    | _::_ ->
        Printf.bprintf buf "%a <- %a." HOT.pp c.head (Util.pp_list HOT.pp) c.body
  let to_string = Util.on_buffer pp
  let fmt fmt c = Format.pp_print_string fmt (to_string c)

  module Set = Sequence.Set.Make(struct
    type t = clause
    let compare = cmp
  end)

  module Map = Sequence.Map.Make(struct
    type t = clause
    let compare = cmp
  end)
end

type clause = Clause.t

(** {2 Proofs}

Unit-resolution proofs *)

module Proof = struct
  type t =
    | Axiom
    | Resolved of fact with_proof * clause with_proof

  and 'a with_proof = {
    conclusion : 'a;
    proof : t;
  }

  let rec facts t k = match t with
    | Axiom -> ()
    | Resolved (fact, clause) ->
        k fact.conclusion;
        facts fact.proof k;
        facts clause.proof k

  let rec rules t k = match t with
    | Axiom -> ()
    | Resolved (fact, clause) ->
        k clause.conclusion;
        rules fact.proof k;
        rules clause.proof k

  let make fact proof clause proof' =
    Resolved ({conclusion=fact; proof;}, {conclusion=clause; proof=proof';})
end

type proof = Proof.t


(** {2 Consequences}
What can be deduced when the Database is updated with new rules
and facts. *)

type consequence = fact * proof

(** Index.

For now we don't use indexing, but still use an Index module so that
the change to a proper indexing structure is easier later *)

module Index = struct
  module M = HOT.Map
  module S = Clause.Set

  type t = S.t M.t

  let empty = M.empty

  let add idx t c =
    let set = try M.find t idx with Not_found -> S.empty in
    let set = S.add c set in
    M.add t set idx

  (* fold on unifiable terms and their associated clause *)
  let retrieve_unify ?(subst=Substs.empty) idx s_idx t s_t acc k =
    M.fold
      (fun t' set acc ->
        let res = Unif.HO.unification ~subst t' s_idx t s_t in
        KList.fold
          (fun acc subst ->
            S.fold (fun clause acc -> k acc clause subst) set acc)
          acc res)
      idx acc
end

(** {2 Fact and Rules Database}

A database contains a set of Horn-clauses about properties, that allow
to reason about them by forward-chaining. *)

type t = {
  facts : Index.t;
  rules : Index.t;    (* indexed by first lit of body *)
  all : proof Clause.Map.t; (* map clause to proofs *)
} (** A DB that holds a saturated set of Horn clauses and facts *)

let empty = {
  facts = Index.empty;
  rules = Index.empty;
  all = Clause.Map.empty;
}

(* Used for the fixpoint computation *)
type add_state = {
  mutable db : t;
  consequences : consequence Queue.t;
  to_process : (Clause.t * Proof.t) Queue.t;
  renaming : Substs.Renaming.t;
}

(* new state *)
let new_state db = {
  db;
  consequences = Queue.create ();
  to_process = Queue.create ();
  renaming = Substs.Renaming.create ();
}

let __consequences state =
  Sequence.of_queue state.consequences

(* add a fact to the DB *)
let __add_fact ~state ~proof fact =
  Index.retrieve_unify state.db.rules 0 fact 1 ()
    (fun () clause subst ->
      (* compute resolvent *)
      Substs.Renaming.clear state.renaming;
      let clause' = Clause.apply_subst subst ~renaming:state.renaming
        (Clause.pop_body clause) 0 in
      (* build proof *)
      let proof_clause = Clause.Map.find clause state.db.all in
      let proof' = Proof.make fact proof clause proof_clause in
      Queue.push (clause', proof') state.to_process)

(* add a rule (non unit Horn clause) to the DB *)
let __add_rule ~state ~proof clause =
  match clause.Clause.body with
  | [] -> assert false
  | lit::_ ->
    Index.retrieve_unify state.db.facts 1 lit 0 ()
      (fun () fact_clause subst ->
        assert (Clause.is_fact fact_clause);
        let fact = fact_clause.Clause.head in
        (* compute resolvent *)
        Substs.Renaming.clear state.renaming;
        let clause' = Clause.apply_subst subst ~renaming:state.renaming
          (Clause.pop_body clause) 0 in
        (* build proof *)
        let proof_fact = Clause.Map.find fact_clause state.db.all in
        let proof' = Proof.make fact proof_fact clause proof in
        Queue.push (clause', proof') state.to_process)

(* deal with state.to_process until no clause is to be processed *)
let __process state =
  while not (Queue.is_empty state.to_process) do
    let c, proof = Queue.pop state.to_process in
    if not (Clause.Map.mem c state.db.all)
    then begin
      Util.debug 5 "meta-reasoner: add clause %a" Clause.pp c;
      (* new clause: insert its proof in state.db.all, then update fixpoint *)
      state.db <- {state.db with all=Clause.Map.add c proof state.db.all;};
      match c.Clause.body with
      | [] ->
          let fact = c.Clause.head in
          (* add fact to consequence if it's not an axiom *)
          if proof <> Proof.Axiom
            then Queue.push (fact,proof) state.consequences;
          (* update fixpoint *)
          __add_fact ~state ~proof c.Clause.head;
          (* add to index *)
          state.db <- {state.db with facts=Index.add state.db.facts fact c; }
      | body1::_ ->
          (* update fixpoint *)
          __add_rule ~state ~proof c;
          (* add [c] to index, by its first literal *)
          state.db <- {state.db with rules=Index.add state.db.rules body1 c; }
    end
  done

(* add a clause to the state *)
let __add state ~proof c =
  Queue.push (c, proof) state.to_process;
  __process state

let add db clause =
  let state = new_state db in
  Queue.push (clause, Proof.Axiom) state.to_process;
  __process state;
  state.db, __consequences state

let add_fact db fact =
  add db (Clause.fact fact)

module Seq = struct
  let to_seq db k =
    Clause.Map.iter
      (fun c proof -> match proof with
        | Proof.Axiom -> k c
        | Proof.Resolved _ -> ())
      db.all

  (* efficient *)
  let of_seq db seq =
    let state = new_state db in
    Sequence.iter (__add state ~proof:Proof.Axiom) seq;
    assert (Queue.is_empty state.to_process);
    state.db, __consequences state

  let facts db =
    to_seq db
    |> Sequence.fmap
        (fun c ->
          match c.Clause.body with
          | [] -> Some c.Clause.head
          | _::_ -> None)
end
