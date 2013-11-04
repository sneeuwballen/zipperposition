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

(** {1 Clause Patterns} *)

open Logtk

module T = FOTerm
module S = Substs.HO
module F = FOFormula
module HOT = HOTerm

let prof_matching = Util.mk_profiler "meta.pattern.matching"
let prof_encode = Util.mk_profiler "meta.pattern.encode"
let prof_decode = Util.mk_profiler "meta.pattern.decode"

let __var_symbol = Symbol.mk_const "V"
let __fun_symbol = Symbol.mk_const "S"

let __var ty = HOT.mk_const ~ty:Type.(ty <=. ty) __var_symbol
let __fun ty = HOT.mk_const ~ty:Type.(ty <=. ty) __fun_symbol

(** {2 Encoding as terms} *)

(** This module is used to handle the encoding of formulas and terms into
    patterns. Terms are supposed to be curried. *)

module EncodedForm = struct
  type t = HOT.t

  (* Encoding of term, in a form that allows to distinguish variables from
      constants even after abstracting symbols *)
  let rec encode_t t = match t.HOT.term with
    | HOT.Var _ ->
      (* First order variable. We add a special constant before it, so
          that we won't match an abstracted symbol with a variable, e.g.
          (p a) with (p X) once a is abstracted to A. (p X) will become
          (p (__var X)), and the substitution will be rejected. *)
      HOT.mk_at (__var t.HOT.ty) [t]
    | HOT.BoundVar _ -> t
    | HOT.Lambda t' ->
      let varty = HOT.lambda_var_ty t in
      HOT.mk_lambda ~varty (encode_t t')
    | HOT.Const s when not (Symbol.is_connective s) -> 
      (** Similarly to the Var case, here we need to protect constants
          from being bound to variables once abstracted into variables *)
      HOT.mk_at (__fun t.HOT.ty) [t]
    | HOT.Const _ -> t
    | HOT.At (t, l) ->
      HOT.mk_at (encode_t t) (List.map encode_t l)

  (* Inverse operation of {! encode_t} *)
  let rec decode_t t = match t.HOT.term with
    | HOT.Var _
    | HOT.BoundVar _ -> t
    | HOT.Lambda t' ->
      let varty = HOT.lambda_var_ty t in
      HOT.mk_lambda ~varty (decode_t t')
    | HOT.Const _ -> t
    | HOT.At ({HOT.term=HOT.Const s}, [t'])
      when (Symbol.eq s __var_symbol || Symbol.eq s __fun_symbol) -> decode_t t'
    | HOT.At (t, l) -> HOT.mk_at (decode_t t) (List.map decode_t l)

  let encode f =
    Util.enter_prof prof_encode;
    let t = F.to_term f in
    let t = encode_t t in
    Util.exit_prof prof_encode;
    t

  let decode t =
    Util.enter_prof prof_decode;
    let t = decode_t t in
    let f = F.of_term t in
    Util.exit_prof prof_decode;
    f

  let eq = HOT.eq
  let compare = HOT.compare
  let hash = HOT.hash
  let bij = HOT.bij
  let mapping = MetaReasoner.Translate.term
  let pp = HOT.pp
  let fmt = HOT.fmt
end

(** {2 Main type}

We encode formulas as terms, because it allows us to lambda-abstract
over them, which makes the order of variables pretty clear.

TODO: remove the list of types, the EncodedForm already contains a function
type
*)

type t =
  | Pattern of EncodedForm.t * Type.t list

let compare (Pattern (t1, types1)) (Pattern (t2, types2)) =
  let c = EncodedForm.compare t1 t2 in
  if c <> 0
    then c
    else Util.lexicograph Type.cmp types1 types2

let eq p1 p2 = compare p1 p2 = 0

let hash (Pattern (t, types)) =
  Hash.hash_list Type.hash (EncodedForm.hash t) types

let pp buf (Pattern (p, _)) =
  EncodedForm.pp buf p

let to_string p = Util.on_buffer pp p

let pp_apply buf (p, args) =
  Printf.bprintf buf "%a(%a)" pp p (Util.pp_list EncodedForm.pp) args

let fmt fmt (Pattern (p, _)) =
  Format.pp_print_string fmt (Util.on_buffer HOT.pp p)

let debug fmt (Pattern (p, types)) =
  Format.fprintf fmt "pat(%a, [%a])" HOT.debug p
    (Sequence.pp_seq Type.fmt) (Sequence.of_list types)

let bij =
  Bij.(map
    ~inject:(fun (Pattern (t, types)) -> t, types)
    ~extract:(fun (t, types) -> Pattern (t, types))
    (pair EncodedForm.bij (list_ Type.bij)))

(** {2 Basic Operations} *)

(* list of constants, in prefix traversal order *)
let rec functions_in_order acc t = match t.HOT.term with
  | HOT.Lambda t' -> functions_in_order acc t'
  | HOT.Const s when not (Symbol.is_connective s)
    && not (Symbol.eq s __var_symbol)
    && not (Symbol.eq s __fun_symbol) -> (* constant, add it *)
    if List.memq t acc then acc else t :: acc
  | HOT.Const _
  | HOT.Var _
  | HOT.BoundVar _ -> acc
  | HOT.At (t, l) ->
    let acc = functions_in_order acc t in
    List.fold_left functions_in_order acc l

let create f =
  (* gather interesting symbols to abstract *)
  let symbols = List.rev (functions_in_order [] f) in
  (* create pattern by lambda abstraction *)
  let pat = Lambda.lambda_abstract_list f symbols in
  let types = match pat.HOT.ty with
    | Type.Fun (_, l) -> l
    | _ -> []
  in
  Pattern (pat, types), symbols

let arity = function
  | Pattern (_, l) -> List.length l

let can_apply (pat,args) =
  match pat with
  | Pattern (t, types) ->
    (* type checking for compatibility of [args] and [types] *)
    Lambda.can_apply t.HOT.ty (List.map HOT.get_type args)

let apply (pat, args) =
  match pat with
  | Pattern (t, types) -> Lambda.lambda_apply_list t args

(** Maps a pattern, parametrized by some of its variables, into datalog terms *)
let mapping =
  let module MT = MetaReasoner.Translate in
  let m = MT.triple MT.term (MT.list_ MT.type_) (MT.list_ MT.term) in
  MT.map
    ~inject:(fun (Pattern (p,types), args) ->
      assert (List.length args = List.length types);
      p, types, args)
    ~extract:(fun (t, types, args) ->
      Pattern (t, types), args)
    m

(** Matches the first pattern (curryfied term) against the second one. Only
    head variables can be bound to any term. It may return several solutions. *)
let matching_terms p1 o_1 p2 o_2 =
  (* is a substitution acceptable? *)
  let ok_subst subst =
    Util.debug 5 "examine matching subst %a" Substs.HO.pp subst;
    Sequence.for_all
      (fun (_, _, t, _) -> not (HOT.contains_symbol  __var_symbol t))
      (S.to_seq subst)
  in
  let substs = HOUnif.matching_ac p1 o_1 p2 o_2 in
  Sequence.filter ok_subst substs

(* assuming term is encoded, match the pattern against it, yielding
    a sequence of (pattern, term list) *)
let matching pat right =
  Util.enter_prof prof_matching;
  match pat with
  | Pattern (t', types) ->
    (* instantiate with variables *)
    let offset = HOT.max_var (HOT.vars t') + 1 in
    let vars = List.mapi (fun i ty -> HOT.mk_var ~ty (i+offset)) types in
    let left = Lambda.lambda_apply_list t' vars in
    (* match left and right *)
    Util.debug 5 "MetaPattern: match %a with %a" HOT.pp left HOT.pp right;
    let substs = matching_terms left 1 right 0 in
    let substs = Sequence.map
      (fun subst ->
        let args = List.map (fun v -> S.apply_no_renaming subst v 1) vars in
        pat, args)
      substs
    in
    let substs = Sequence.persistent substs in
    Util.exit_prof prof_matching;
    substs

(** {2 Set of patterns} *)

type pattern = t

module Set = struct
  module PSet = Set.Make(struct
    type t = pattern
    let compare = compare
  end)

  type t = PSet.t
    (** A proper set of patterns (for now) *)

  (* TODO: use some kind of indexing for make matching faster, e.g. by
      using an AC-compatible hash. *)

  let empty = PSet.empty

  let is_empty = PSet.is_empty
  
  let eq s1 s2 = PSet.equal s1 s2

  (** Add a pattern to the set *)
  let add set pat = PSet.add pat set

  (** Match the given formula (curried) against the patterns of the set. Returns
      a list of instances of the pattern. E.g. if the pattern is
      commutativity, matching against "f(X,Y)=f(Y,X)" will return
      [\f. f @ X @ Y = f @ Y @X, [f]]. *)
  let matching set f =
    PSet.fold
      (fun pat' acc ->
        let substs = matching pat' f in
        List.rev_append
          (Sequence.to_rev_list substs)
          acc)
      set []

  let to_seq set =
    Sequence.from_iter (fun k -> PSet.iter k set)

  let of_seq ?(init=empty) pats = Sequence.fold add init pats

  let pp buf set =
    Buffer.add_string buf "patterns {";
    Util.pp_seq pp buf (to_seq set);
    Buffer.add_string buf "}"

  let fmt formatter set =
    Format.fprintf formatter "@[<hov2>patterns {@;%a}@]"
      (Sequence.pp_seq fmt) (to_seq set)

  let bij =
    let open Bij in
    map
      ~inject:(fun set -> Sequence.to_list (to_seq set))
      ~extract:(fun l -> of_seq (Sequence.of_list l))
      (list_ bij)
end

