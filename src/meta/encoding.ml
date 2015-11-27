
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

(** {1 Encoding of clauses} *)

open Logtk

module T = ScopedTerm
module FOT = FOTerm
module HOT = HOTerm

type 'a printer = Format.formatter -> 'a -> unit

let section = Util.Section.make ~parent:Util.Section.logtk "meta"

(** {2 Base definitions} *)

type 'a lit =
  | Eq of 'a * 'a * bool
  | Prop of 'a * bool
  | Bool of bool

let fmap_lit f = function
  | Eq (a,b, truth) -> Eq (f a, f b, truth)
  | Prop (a, truth) -> Prop (f a, truth)
  | Bool b -> Bool b

let opt_seq_lit = function
  | Eq (Some a, Some b, truth) -> Some (Eq (a, b, truth))
  | Prop (Some a, truth) -> Some (Prop (a, truth))
  | Eq _ | Prop _ -> None
  | Bool b -> Some (Bool b)

type 'a clause = 'a lit list

let fmap_clause f c = List.map (fmap_lit f) c

type foterm = FOTerm.t
type hoterm = HOT.t

type foclause = foterm clause
type hoclause = hoterm clause

(* convert a list of formulas into a clause *)
let foclause_of_clause l =
  let module F = Formula.FO in
  Util.debugf ~section 5 "foclause_of_clause @[%a@]"
    (fun k->k (CCFormat.list F.pp) l);
  let term_of_form f = match F.view f with
    | F.Atom t -> t
    | _ -> invalid_arg (CCFormat.sprintf "expected term, got formula %a" F.pp f)
  in
  List.map
    (fun f -> match F.view f with
      | F.Not f' -> Prop (term_of_form f', false)
      | F.Eq (a,b) -> Eq (a, b, true)
      | F.Neq (a,b) -> Eq (a, b, false)
      | F.True -> Bool true
      | F.False -> Bool false
      | _ -> Prop (term_of_form f, true)
    ) l

let clause_of_foclause l =
  let module F = Formula.FO in
  List.map
    (function
      | Eq (a, b, sign) -> F.Base.mk_eq sign a b
      | Prop (a, sign) -> F.Base.mk_atom sign a
      | Bool true -> F.Base.true_
      | Bool false -> F.Base.false_
    ) l

let pp_clause pp_t out c =
  CCList.print ~start:"" ~stop:"" ~sep:" | "
    (fun buf lit -> match lit with
     | Eq (a, b, true) -> Format.fprintf buf "@[%a = %a@]" pp_t a pp_t b
     | Eq (a, b, false) -> Format.fprintf buf "@[%a != %a@]" pp_t a pp_t b
     | Prop (a, true) -> pp_t buf a
     | Prop (a, false) -> Format.fprintf buf "@[~ %a@]" pp_t a
     | Bool b -> Format.fprintf buf "%B" b
    ) out c

(** {6 Encoding abstraction} *)

class type ['a, 'b] t = object
  method encode : 'a -> 'b
  method decode : 'b -> 'a option
end

let id = object
  method encode x = x
  method decode x = Some x
end

let compose a b = object
  method encode x = b#encode (a#encode x)
  method decode y =
    match b#decode y with
    | Some x -> a#decode x
    | None -> None
end

let (>>>) a b = compose a b

(** {6 Currying} *)

let currying =
  let module ListOpt = CCList.Traverse(CCOpt) in
  object
  method encode c = fmap_clause HOT.curry c
  method decode c =
    fmap_clause HOT.uncurry c
      |> List.map opt_seq_lit
      |> ListOpt.sequence_m
  end

(** {6 Clause encoding}

Encode the whole clause into a {!Reasoner.Property.t}, ie a higher-order term
that represents a meta-level property. *)

module EncodedClause = struct
  type t = Reasoner.term

  let equal = HOT.equal
  let hash = HOT.hash
  let hash_fun = HOT.hash_fun
  let compare = HOT.compare
  let pp = HOT.pp
  let to_string = HOT.to_string

  let __magic t = t
end

(** Encode/Decode clauses into terms:
    terms are already curried and rigidified, so we only need to replace
    connectives by their multiset versions. *)

let __ty_or = Type.(TPTP.o <=. multiset TPTP.o)
let __ty_eq = Type.(forall [var 0] (TPTP.o <=. multiset (var 0)))
let __ty_not = Type.(TPTP.o <=. TPTP.o)

let __or_conn =
  HOT.const ~ty:__ty_or Symbol.Base.or_
let __and__conn =
  HOT.const ~ty:__ty_or Symbol.Base.and_
let __xor_conn =
  HOT.const ~ty:__ty_or Symbol.Base.xor
let __equiv_conn =
  HOT.const ~ty:__ty_or Symbol.Base.equiv
let __eq_conn =
  HOT.const ~ty:__ty_eq Symbol.Base.eq
let __neq_conn =
  HOT.const ~ty:__ty_eq Symbol.Base.neq
let __not_conn =
  HOT.const ~ty:__ty_not Symbol.Base.not_

let signature = Signature.of_list
  [ Symbol.Base.or_, __ty_or
  ; Symbol.Base.and_, __ty_or
  ; Symbol.Base.xor, __ty_or
  ; Symbol.Base.equiv, __ty_or
  ; Symbol.Base.eq, __ty_eq
  ; Symbol.Base.neq, __ty_eq
  ; Symbol.Base.not_, __ty_not
  ]

let __encode_lit = function
  | Eq (a, b, truth) ->
      let ty = HOT.ty a in
      if truth
        then HOT.at (HOT.tyat __eq_conn ty) (HOT.multiset ~ty [a; b])
        else HOT.at (HOT.tyat __neq_conn ty) (HOT.multiset ~ty [a; b])
  | Prop (p, true) -> p
  | Prop (p, false) -> HOT.at __not_conn p
  | Bool true -> HOT.TPTP.true_
  | Bool false -> HOT.TPTP.false_

let __decode_lit t = match HOT.open_at t with
  | hd, _, [r] when HOT.equal hd __not_conn -> Prop (r, false)
  | hd, _, [r] ->
      begin match HOT.view r with
      | HOT.Multiset (_,[a;b]) when HOT.equal hd __eq_conn -> Eq (a, b, true)
      | HOT.Multiset (_,[a;b]) when HOT.equal hd __neq_conn -> Eq (a, b, false)
      | _ -> Prop (t, true)
      end
  | _ -> Prop (t, true)

let clause_prop = object
  method encode c =
    let lits = List.map __encode_lit c in
    HOT.close_forall (HOT.multiset ~ty:Type.TPTP.o lits)

  method decode c =
    let c = HOT.open_forall c in
    match HOT.view c with
    | HOT.Multiset (_,l) -> Some (List.map __decode_lit l)
    | _ -> None
end

