
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

  let rule head body = {head; body; }
  let fact head = rule head []

  let is_fact t = t.body = []

  let eq c1 c2 =
    HOT.eq c1.head c2.head &&
    (try List.for_all2 HOT.eq c1.body c2.body with Invalid_argument _ -> false)

  module Seq = struct
    let terms c k =
      k c.head; List.iter k c.body
    let vars c = terms c |> Sequence.flatMap HOT.Seq.vars
  end

  let hash c =
    Hash.hash_seq HOT.hash 13 (Seq.terms c)

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
end

type clause = Clause.t

(** {2 Explanation for Facts} *)

module Explanation = struct
  type derivation = {
    rules : clause list;
    facts : fact list;
  } 

  (** An explanation of why some property holds. *)
  type t =
    | Axiom
    | Deduced of derivation
end

type explanation = Explanation.t

(** {2 Consequences}
What can be deduced when the Database is updated with new rules
and facts. *)

module Consequence = struct
  type t = private {
    fact : fact;
    explanation : explanation Lazy.t;
  } (** Consequence of adding a fact or clause to the DB. The second
        value is a (lazy) explanation of why the first property is true. *)

  let fact t = t.fact
  let explanation t = Lazy.force t.explanation
end
type consequence = Consequence.t

(** Index.

For now we don't use indexing, but still use an Index module so that
the change to a proper indexing structure is easier later *)

module Index = struct
  module S = Clause.Set
  module M = HOT.Map

  type t = S.t M.t

  let empty = M.empty

  let add idx t c =
    let set = try M.find t idx with Not_found -> S.empty in
    let set = S.add c set in
    M.add t set idx

  let iter idx k =
    M.iter (fun t set -> S.iter (fun clause -> k (t,clause)) set) idx

  (* fold on unifiable terms and their associated clause *)
  let retrieve_unify ?(subst=Substs.empty) idx s_idx t s_t acc k =
    M.fold
      (fun t' set acc ->
        let res = Unif.HO.unification ~subst t' s_idx t s_t in
        Unif.Res.fold
          (fun acc subst ->
            S.fold (fun clause acc -> k acc clause subst) set acc)
          acc res)
      idx acc
end

(** {2 Fact and Rules Database}

A database contains a set of Horn-clauses about properties, that allow
to reason about them by forward-chaining. *)

type t = {
  idx : Index.t;
} (** A DB that holds a saturated set of Horn clauses and facts *)

let empty = {
  idx=Index.empty;
}

let add db clause = assert false
  
let add_fact db fact = add db (Clause.fact fact)

module Seq = struct
  let to_seq db k = Index.iter db.idx (fun (_,c) -> k c)

  let of_seq db seq =
    let q = Queue.create () in
    let db = Sequence.fold
      (fun db c ->
        let db, consequences = add db c in
        Queue.push consequences q;
        db)
      db seq
    in
    db, Sequence.concat (Sequence.of_queue q)

  let facts db =
    to_seq db
    |> Sequence.fmap
        (fun c ->
          match c.Clause.body with
          | [] -> Some c.Clause.head
          | _::_ -> None)
end
