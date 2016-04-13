
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Specialized plugins}

    A plugin is a bridge between OCaml code and some meta-level property
    of the prover. For instance, a default plugin is bound to the predicate
    "holds", which states that some (encoded) clause holds in the current
    problem. The plugin for "holds" provides a view centered on the "holds"
    predicate, and allows to add "holds" facts and filter on them.

    Plugins can be extended to handle some inner type that can be encoded/decoded
    to meta-prover-compatible types. *)

open Libzipperposition

let section = Util.Section.(make ~parent:zip "meta")

module T = TypedSTerm

type term = T.t
type ty = Reasoner.ty

(** Core features of a plugin, that don't depend on its type parameter *)
class type core = object
  method signature : ty ID.Map.t
  (** Signature of symbols used *)

  method owns : term -> bool
  (** Does this term belong to the plugin? *)
end

class type ['a] t = object
  inherit core

  method clauses : Reasoner.clause list
  (** Initial clauses to add *)

  method to_fact : 'a -> term
  (** Encode an 'a value to a fact *)

  method of_fact : term -> 'a option
  (** Decode a fact into an 'a value, if it actually belongs
      to the plugin *)
end

type foclause = Encoding.foclause

module Set = struct
  module S = CCSet.Make(struct
      type t = core
      let compare a b = Oo.id a - Oo.id b
    end)

  type t = S.t
  let empty = S.empty
  let add set x = S.add x set
  let of_seq = S.of_seq

  let signature set =
    S.fold
      (fun core signature ->
         ID.Map.merge_safe core#signature signature
           ~f:(fun _ pair -> match pair with
             | `Left ty | `Right ty -> Some ty
             | `Both (a,b) ->
               if T.equal a b
               then Some a else failwith "merge: incompatible types"))
      set ID.Map.empty
end

(** {2 Builtin plugins} *)

(* TODO parse those correctly *)

let __sym_holds = ID.make "holds"
let __sym_lemma = ID.make "lemma"
let __ty_wrap = T.Ty.([multiset prop] ==> Reasoner.property_ty)
let __encoding_wrap =
  Encoding.(currying >>> clause_prop)

(* clause that implies (holds c) whenever (lemma c) is true *)
let __clause_lemma_imply_holds =
  let var = T.var (Var.of_string ~ty:T.Ty.(multiset prop) "a") in
  Reasoner.Clause.rule
    (T.app_infer (T.const ~ty:__ty_wrap __sym_holds) [var])
    [ T.app_infer (T.const ~ty:__ty_wrap __sym_lemma) [var]]

let wrap_fo_clause pred clauses : foclause t =
  let hd = T.const ~ty:__ty_wrap pred in
  object
    method signature = ID.Map.singleton pred __ty_wrap
    method clauses = clauses
    method owns t =
      try ID.equal (T.head_exn t) pred
      with _ -> false

    method to_fact c =
      Util.debugf ~section 5 "@[<2>encode clause@ `@[%a@]`@]"
        (fun k->k (Encoding.pp_clause FOTerm.pp) c);
      let c' = (__encoding_wrap#encode c : Encoding.EncodedClause.t :> T.t) in
      Util.debugf ~section 5 "@[<4>... into@ `@[%a@]`@]" (fun k->k T.pp c');
      T.app_infer hd [c']

    method of_fact t =
      match T.view t with
      | T.App (hd', [c]) when T.equal hd hd' ->
          __encoding_wrap#decode (Encoding.EncodedClause.__magic c)
      | _ -> None
  end

let holds = wrap_fo_clause __sym_holds []
let lemma = wrap_fo_clause __sym_lemma [__clause_lemma_imply_holds]

let axiom_or_theory which : (ID.t * term list) t  =
  let s = ID.make which in
  let ty_s = T.Ty.const s in
  let ty = T.Ty.([ty_s] ==> Reasoner.property_ty) in
  let hd = T.const ~ty s in
  object
    method signature = ID.Map.singleton s ty
    method owns t =
      match T.view t with
      | T.App (hd', _) -> T.equal hd hd'
      | _ -> assert false
    method to_fact (name,args) =
      T.app_infer hd
        [T.app_infer
           (T.const ~ty:T.Ty.(List.map T.ty_exn args ==> ty_s) name)
           args]
    method of_fact t =
      Util.debugf ~section 5 "%s.of_fact %a?" (fun k->k which T.pp t);
      match T.view t with
      | T.App (hd', [f]) when T.equal hd hd' ->
          begin match T.view f with
            | T.App (name, args) ->
                begin match T.view name with
                  | T.Const name' -> Some (name', args)
                  | _ -> None
                end
            | _ -> None
          end
      | _ -> None
    method clauses = []
  end

let axiom = axiom_or_theory "axiom"
let theory = axiom_or_theory "theory"

(* TODO: parse this properly (declare it before type inference) *)

let __sym_rule_arrow = ID.make "-->"
let __sym_pre_rewrite = ID.make "pre_rewrite"
let __sym_rewrite = ID.make "rewrite"

let __ty_rule = T.Ty.const (ID.make "rule")
let __ty_rule_arrow =
  let a = Var.of_string ~ty:T.Ty.tType "a" in
  T.Ty.(forall a ([T.Ty.var a; T.Ty.var a] ==> __ty_rule))
let __ty_rewrite = T.Ty.([multiset __ty_rule] ==> Reasoner.property_ty)

let pre_rewrite : (term * term) list t =
  let const_pre_rewrite = T.const ~ty:__ty_rewrite __sym_pre_rewrite in
  let const_rule = T.const ~ty:__ty_rule_arrow __sym_rule_arrow in
  let make_rule l r = T.app_infer const_rule [l;r] in
  object
    method signature = ID.Map.of_list
        [ __sym_rule_arrow, __ty_rule_arrow
        ; __sym_pre_rewrite, __ty_rewrite ]
    method clauses = []
    method owns t =
      try ID.equal (T.head_exn t) __sym_pre_rewrite
      with _ -> false

    method to_fact rules =
      rules
      |> List.map (fun (l,r) -> make_rule l r)
      |> T.multiset ~ty:(T.Ty.multiset __ty_rule)
      |> CCList.return
      |> T.app_infer const_pre_rewrite

    method of_fact t = match T.view t with
      | T.App (l, [r]) when T.equal l const_pre_rewrite ->
          begin match T.view r with
            | T.Multiset l ->
                begin try
                    let rules = List.map
                        (fun pair ->
                           match
                             T.open_binder Binder.Forall pair |> snd |> T.view
                           with
                           | T.App (hd, [l;r]) when T.equal hd const_rule -> l, r
                           | _ -> raise Exit) l
                    in
                    Some rules
                  with Exit -> None
                end
            | _ -> None
          end
      | _ -> None
  end

let rewrite : (FOTerm.t * FOTerm.t) list t =
  let const_rewrite = T.const ~ty:__ty_rewrite __sym_rewrite in
  let const_rule = T.const ~ty:__ty_rule_arrow __sym_rule_arrow in
  let make_rule l r = T.app_infer const_rule [l;r] in
  object
    method signature = ID.Map.of_list
        [ __sym_rule_arrow, __ty_rule_arrow
        ; __sym_rewrite, __ty_rewrite ]
    method clauses = []
    method owns t =
      try ID.equal (T.head_exn t) __sym_rewrite
      with _ -> false

    method to_fact rules =
      rules
      |> List.map
        (fun (l,r) ->
          let l' = FOTerm.Conv.to_simple_term l in
          let r' = FOTerm.Conv.to_simple_term r in
          make_rule l' r')
      |> T.multiset ~ty:(T.Ty.multiset __ty_rule)
      |> CCList.return
      |> T.app_infer const_rewrite

    method of_fact t =
      let ctx = FOTerm.Conv.create() in
      match T.view t with
      | T.App (l, [r]) when T.equal l const_rewrite ->
          begin match T.view r with
            | T.Multiset l ->
                begin try
                    let rules = List.map
                        (fun pair ->
                           match
                             T.open_binder Binder.Forall pair |> snd |> T.view
                           with
                           | T.App (hd, [l;r]) when T.equal hd const_rule ->
                             begin match
                                 FOTerm.Conv.of_simple_term ctx l,
                                 FOTerm.Conv.of_simple_term ctx r
                               with
                                 | Some l', Some r' -> l', r'
                                 | _ -> raise Exit
                             end
                           | _ -> raise Exit) l
                    in
                    Some rules
                  with Exit -> None
                end
            | _ -> None
          end
      | _ -> None
  end


module Base = struct
  let __list =
    [ (holds :> core); (lemma :> core); (axiom :> core);
      (theory :> core); (rewrite :> core); (pre_rewrite :> core) ]

  let set =
    Set.of_seq (Sequence.of_list __list)

  let signature = Set.signature set
end

(** {2 Interaction with Reasoner} *)

let facts r plugin =
  Reasoner.Seq.facts r |> Sequence.fmap plugin#of_fact

let of_consequence (fact,_) plugin = plugin#of_fact fact

let of_consequences seq plugin =
  Sequence.fmap
    (fun c -> of_consequence c plugin)
    seq
