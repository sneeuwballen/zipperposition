
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Encoding of clauses} *)

open Libzipperposition

module T = InnerTerm
module FOT = FOTerm
module HOT = TypedSTerm

type 'a printer = Format.formatter -> 'a -> unit

let section = Util.Section.(make ~parent:zip "meta")

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

(* convert a list of literals into a clause *)
let foclause_of_clause l =
  Util.debugf ~section 5 "@[<2>foclause_of_clause@ `@[%a@]`@]"
    (fun k->k (Util.pp_list (SLiteral.pp FOT.pp)) l);
  List.map
    (function
      | SLiteral.Atom (t, b) -> Prop (t, b)
      | SLiteral.True -> Bool true
      | SLiteral.False -> Bool false
      | SLiteral.Eq (a,b) -> Eq (a,b,true)
      | SLiteral.Neq (a,b) -> Eq (a,b,false))
    l

let clause_of_foclause l =
  List.map
    (function
      | Eq (a, b, true) -> SLiteral.eq a b
      | Eq (a, b, false) -> SLiteral.neq a b
      | Prop (a, sign) -> SLiteral.atom a sign
      | Bool true -> SLiteral.true_
      | Bool false -> SLiteral.false_)
    l

let pp_clause pp_t out c =
  CCList.print ~start:"" ~stop:"" ~sep:" | "
    (fun buf lit -> match lit with
       | Eq (a, b, true) -> Format.fprintf buf "@[<1>%a@ = %a@]" pp_t a pp_t b
       | Eq (a, b, false) -> Format.fprintf buf "@[<1>%a@ != %a@]" pp_t a pp_t b
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
  let ctx = FOTerm.Conv.create () in
  object
    method encode c =
      fmap_clause FOTerm.Conv.to_simple_term c
    method decode c =
    fmap_clause (FOTerm.Conv.of_simple_term ctx) c
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

let ty_ms =
  let a = Var.of_string ~ty:HOT.Ty.tType "a" in
  HOT.Ty.(forall a ([multiset (HOT.Ty.var a)] ==> prop))
let eq_conn = HOT.const ~ty:ty_ms (ID.make "=")
let neq_conn = HOT.const ~ty:ty_ms (ID.make "≠")
let not_conn = HOT.const ~ty:HOT.Ty.([prop] ==> prop) (ID.make "¬")

let __encode_lit = function
  | Eq (a, b, truth) ->
      let ty = HOT.ty_exn a in
      let ty_mul = HOT.Ty.multiset ty in
      if truth
      then HOT.app_infer eq_conn [ty; HOT.multiset ~ty:ty_mul [a; b]]
      else HOT.app_infer neq_conn [ty; HOT.multiset ~ty:ty_mul [a; b]]
  | Prop (p, true) -> p
  | Prop (p, false) -> HOT.app_infer not_conn [p]
  | Bool true -> HOT.Form.true_
  | Bool false -> HOT.Form.false_

let __decode_lit t = match HOT.view t with
  | HOT.App (hd, [r]) when HOT.equal hd not_conn -> Prop (r, false)
  | HOT.App (hd, [r]) ->
      begin match HOT.view r with
        | HOT.Multiset [a;b] when HOT.equal hd eq_conn -> Eq (a, b, true)
        | HOT.Multiset [a;b] when HOT.equal hd neq_conn -> Eq (a, b, false)
        | _ -> Prop (t, true)
      end
  | _ -> Prop (t, true)

let clause_prop = object
  method encode c =
    let lits = List.map __encode_lit c in
    HOT.Form.close_forall (HOT.multiset ~ty:HOT.Ty.(multiset prop) lits)

  method decode c =
    let _, c = HOT.open_binder Binder.Forall c in
    match HOT.view c with
    | HOT.Multiset l -> Some (List.map __decode_lit l)
    | _ -> None
end

