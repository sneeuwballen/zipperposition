
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

(** {1 Specialized plugins}

A plugin is a bridge between OCaml code and some meta-level property
of the prover. For instance, a default plugin is bound to the predicate
"holds", which states that some (encoded) clause holds in the current
problem. The plugin for "holds" provides a view centered on the "holds"
predicate, and allows to add "holds" facts and filter on them.

Plugins can be extended to handle some inner type that can be encoded/decoded
to meta-prover-compatible types. *)

open Logtk

module T = HOTerm

type term = T.t

class type t = object
  method symbol : Symbol.t
    (** Symbol used to represent properties of this plugin *)

  method ty : Type.t
    (** Type of the symbol *)

  method owns : term -> bool
    (** Does this term belong to the plugin? *)

  method clauses : Reasoner.clause list
    (** Initial clauses to add *)
end

class type ['inner] extended = object
  inherit t

  method to_fact : 'inner -> term
    (** Encode an 'inner value to a fact *)

  method of_fact : term -> 'inner option
    (** Decode a fact into an 'inner value, if it actually belongs
        to the plugin *)
end

type foclause = Encoding.foclause

type set = t Symbol.Map.t
  (** Set of plugins, by their symbols *)

let signature_of_set (s:set) : Signature.t =
  Symbol.Map.to_seq s
    |> Sequence.map (fun (s, p) -> s, p#ty)
    |> Signature.Seq.of_seq

(** {2 Builtin plugins} *)

let __sym_holds = Symbol.of_string "holds"
let __sym_lemma = Symbol.of_string "lemma"
let __ty_wrap = Type.(Reasoner.property_ty <=. Type.multiset Type.TPTP.o)
let __encoding_wrap =
  Encoding.(currying >>> rigidifying >>> clause_prop)

(* clause that implies (holds c) whenever (lemma c) is true *)
let __clause_lemma_imply_holds =
  let var = T.var ~ty:Type.(multiset TPTP.o) 0 in
  Reasoner.Clause.rule
    (T.at (T.const ~ty:__ty_wrap __sym_holds) var)
    [ T.at (T.const ~ty:__ty_wrap __sym_lemma) var ]

let wrap_fo_clause pred clauses : foclause extended =
  let hd = T.const ~ty:__ty_wrap pred in
  object
    method symbol = pred
    method ty = __ty_wrap
    method owns t =
      try Symbol.eq (T.head t) pred
      with _ -> false

    method to_fact c =
      Util.debug 5 "encode clause %a" (Encoding.pp_clause FOTerm.pp) c;
      let c' = (__encoding_wrap#encode c : Encoding.EncodedClause.t :> T.t) in
      Util.debug 5 "... into %a" T.pp c';
      T.at hd c'

    method of_fact t =
      match T.view t with
      | T.At (hd', c) when T.eq hd hd' ->
          __encoding_wrap#decode (Encoding.EncodedClause.__magic c)
      | _ -> None

    method clauses = clauses
  end

let holds = wrap_fo_clause __sym_holds []
let lemma = wrap_fo_clause __sym_lemma [__clause_lemma_imply_holds]

let axiom_or_theory which : (Symbol.t * term) extended  =
  let s = Symbol.of_string which in
  let ty = Type.(Reasoner.property_ty <=. const s) in
  let hd = T.const ~ty s in
  object
    method symbol = s
    method ty = ty
    method owns t =
      match T.open_at t with
      | hd', _, _ -> T.eq hd hd'
    method to_fact (s,t) =
      T.at hd t
    method of_fact t =
      Util.debug 5 "%s.of_fact %a?" which T.pp t;
      match T.open_at t with
      | hd', _, [f] when T.eq hd hd' ->
          begin match T.open_at f with
          | name, _, [arg] ->
              begin match T.view name with
              | T.Const s -> Some (s, arg)
              | _ -> None
              end
          | _ -> None
          end
      | _ -> None
    method clauses = []
  end

let axiom = axiom_or_theory "axiom"
let theory = axiom_or_theory "theory"

module Base = struct
  let __list = [ (holds :> t) ; (lemma :> t); (axiom :> t) ; (theory :> t) ]

  let set =
    List.fold_left
      (fun set p -> Symbol.Map.add p#symbol (p:>t) set)
      Symbol.Map.empty __list

  let signature = signature_of_set set
end

(** {2 Interaction with Reasoner} *)

let facts r plugin =
  Reasoner.Seq.facts r |> Sequence.fmap plugin#of_fact

let of_consequence (fact,_) plugin = plugin#of_fact fact

let of_consequences seq plugin =
  Sequence.fmap
    (fun c -> of_consequence c plugin)
    seq
