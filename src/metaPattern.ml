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

module T = Term
module S = Substs

let prof_matching = Util.mk_profiler "meta.pattern.matching"

let __var_symbol = Symbol.mk_const "$$var"

(** {2 Basics} *)

type t =
  | Pattern of Term.t * int

(** Encoding of term, in a form that allows to distinguish variables from
    constants even after abstracting symbols *)
let rec encode t = match t.T.term with
  | T.Var _ ->
    (* First order variable. We add a special constant before it, so
        that we won't match an abstracted symbol with a variable, e.g.
        (p a) with (p X) once a is abstracted to A. (p X) will become
        (p (__var X)), and the substitution will be rejected. *)
    T.mk_node __var_symbol [t]
  | T.BoundVar _ -> t
  | T.Bind (s, t') -> T.mk_bind s (encode t')
  | T.Node (s, []) -> t
  | T.Node (s, l) -> T.mk_node s (List.map encode l)
  | T.At (t1, t2) -> T.mk_at (encode t1) (encode t2)

(** Inverse operation for [encode] *)
let rec decode t = match t.T.term with
  | T.Var _
  | T.BoundVar _ -> t
  | T.Bind (s, t') -> T.mk_bind s (decode t')
  | T.Node (s, [t']) when Symbol.eq s __var_symbol -> decode t'
  | T.Node (s, l) -> T.mk_node s (List.map decode l)
  | T.At (t1, t2) -> T.mk_at (decode t1) (decode t2)

let compare (Pattern (t1, i1)) (Pattern (t2, i2)) = Term.compare t1 t2

let eq (Pattern (t1, _)) (Pattern (t2, _)) = Term.eq t1 t2

let hash (Pattern (t, _)) = Term.hash t

let pp buf (Pattern (p, _)) =
  Term.pp buf p

let pp_apply buf (p, args) =
  Printf.bprintf buf "%a(%a)" pp p (Util.pp_list T.pp) args

let fmt fmt (Pattern (p, _)) =
  Term.fmt fmt p

let bij =
  Bij.map
    ~inject:(fun (Pattern (t, i)) -> t, i)
    ~extract:(fun (t, i) -> Pattern (t, i))
    (Bij.pair T.bij Bij.int_)

(** {2 Fundamental operations} *)

(** list of constants, in prefix traversal order *)
let functions_in_order t =
  let rec recurse acc t = match t.T.term with
  | T.Var _
  | T.BoundVar _ -> acc
  | T.Bind (s, t') ->
    recurse acc t'
  | T.Node (_, []) -> (* constant, add it *)
    if List.memq t acc then acc else t :: acc
  | T.Node (_, l) ->  (* subterms *)
    List.fold_left recurse acc l
  | T.At (t1, t2) ->
    let acc = recurse acc t1 in
    recurse acc t2
  in
  List.rev (recurse [] t)

let create t =
  let t = encode (T.curry t) in
  let funs = functions_in_order t in
  let pat = T.lambda_abstract_list t funs in
  Pattern (pat, List.length funs), funs

let arity = function
  | Pattern (_, i) -> i

let apply (pat, args) =
  match pat with
  | Pattern (t, arity) ->
    assert (arity = List.length args);
    let t = T.lambda_apply_list t args in
    decode t

let to_term = function
  | Pattern (p, _) -> p
let of_term t i =
  Pattern (t, i)

(** Maps a pattern, parametrized by some of its variables, into datalog terms *)
let mapping =
  MetaReasoner.Translate.map
    ~inject:(fun (p, args) -> assert (List.length args = arity p); to_term p, args)
    ~extract:(fun (t, args) -> of_term t (List.length args), args)
    (MetaReasoner.Translate.parametrize MetaReasoner.Translate.term)

(** Matches the first pattern (curryfied term) against the second one. Only
    head variables can be bound to any term. It may return several solutions. *)
let matching_terms p1 o_1 p2 o_2 =
  (** Does [symb] occur in [t]? *)
  let rec symb_occurs symb t = match t.T.term with
    | T.Var _
    | T.BoundVar _ -> false
    | T.Bind (s, t') -> Symbol.eq s symb || symb_occurs symb t'
    | T.Node (s, l) -> Symbol.eq s symb || List.exists (symb_occurs symb) l
    | T.At (t1, t2) -> symb_occurs symb t1 || symb_occurs symb t2
  in
  (* is a substitution acceptable? *)
  let ok_subst subst =
    Sequence.for_all
      (fun (_, _, t, _) -> not (symb_occurs __var_symbol t))
      (Substs.to_seq subst)
  in
  let substs = Unif.matching_ac p1 o_1 p2 o_2 in
  Sequence.filter ok_subst substs

(* assuming term is encoded, match the pattern against it, yielding
    a sequence of (pattern, term list) *)
let matching pat right =
  let rec _mk_vars offset i =
    if i = 0
      then []
      else T.mk_var (offset+i) :: _mk_vars offset (i-1)
  in
  match pat with
  | Pattern (t', i) ->
    (* instantiate with variables *)
    let vars = _mk_vars (T.max_var (T.vars t') + 1) i in
    let left = T.lambda_apply_list t' vars in
    (* match pat and pat' *)
    let substs = matching_terms left 1 right 0 in
    Sequence.map
      (fun subst ->
        let args = List.map (fun v -> Substs.apply_subst subst v 1) vars in
        pat, args)
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

  (** Match the given term (raw) against the patterns of the set. Returns
      a list of instances of the pattern. E.g. if the pattern is
      commutativity, matching against "f(X,Y)=f(Y,X)" will return
      [\f. f @ X @ Y = f @ Y @X, [f]]. *)
  let matching set t =
    let right = encode t in
    PSet.fold
      (fun pat' acc ->
        let substs = matching pat' right in
        (Sequence.to_rev_list substs) @ acc)
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

