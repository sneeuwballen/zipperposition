
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

(** {1 Rewriting on HO terms} *)

module Hash = CCHash
module T = HOTerm

type term = T.t
type rule = term * term

(** {2 Rewrite system} *)

(** Set of rules *)
module S = Sequence.Set.Make(struct
  type t = rule
  let compare (l1,r1) (l2,r2) =
    let c = T.cmp l1 l2 in
    if c <> 0 then c else T.cmp r1 r2
end)

type t = S.t

let empty = S.empty

let add trs rule = S.add rule trs

let merge t1 t2 = S.union t1 t2

let eq = S.equal
let cmp = S.compare
let hash_fun s h = Hash.seq (Hash.pair T.hash_fun T.hash_fun) (S.to_seq s) h
let hash s = Hash.apply hash_fun s

module Seq = struct
  let to_seq = S.to_seq
  let of_seq init seq = S.union (S.of_seq seq) init
end

let to_list trs = Seq.to_seq trs |> Sequence.to_rev_list
let of_list l = Sequence.of_list l |> Seq.of_seq empty

let pp buf t =
  Printf.bprintf buf
  "rewriting{%a}"
    (Util.pp_seq ~sep:"; " (Util.pp_pair ~sep:" --> " T.pp T.pp))
    (Seq.to_seq t)

let to_string = Util.on_buffer pp
let fmt fmt s = Format.pp_print_string fmt (to_string s)

(** {2 Normalize} *)

exception RewrittenIn of term * Substs.t * rule

let normalize_collect trs t =
  (* reduce to normal form. *)
  let rec reduce ~trs ~rules t = match T.view t with
    | T.At (l, r) ->
        let l' = reduce ~trs ~rules l in
        let r' = reduce ~trs ~rules r in
        rewrite_here ~trs ~rules (T.at l' r')
    | T.TyAt (t, ty) ->
        let t' = reduce ~trs ~rules t in
        rewrite_here ~trs ~rules (T.tyat t' ty)
    | T.Multiset (tau,l) ->
        let l' = List.map (reduce ~trs ~rules) l in
        rewrite_here ~trs ~rules (T.multiset ~ty:tau l')
    | T.Record (l, rest) ->
        let l' = List.map (fun (n,t) -> n, reduce ~trs ~rules t) l in
        let rest = CCOpt.map (reduce ~trs ~rules) rest in
        rewrite_here ~trs ~rules (T.record l' ~rest)
    | T.RigidVar _
    | T.Var _
    | T.BVar _ -> t
    | T.Lambda (varty, t') ->
        let t' = reduce ~trs ~rules t' in
        T.__mk_lambda ~varty t'   (* no rules for lambda *)
    | T.Const _ ->
        rewrite_here ~trs ~rules t
  (* try to find a rewrite rules whose left-hand side matches [t]. In this
     case replace by the right-hand side of the rule. *)
  and rewrite_here ~trs ~rules t =
    try
      S.iter
        (fun (l,r) ->
          let substs = Unif.HO.matching ~pattern:l 1 t 0 in
          match substs |> Sequence.take 1 |> Sequence.to_list with
          | [subst] ->
              (* l\subst = t, rewrite into r\subst *)
              let r = Substs.HO.apply_no_renaming subst r 1 in
              raise (RewrittenIn (r, subst, (l,r)))
          | _ -> ()   (* failure, try next rule *)
        ) trs;
      (* could not rewrite [t], just return it *)
      t
    with RewrittenIn (t', _subst, rule) ->
      rules := rule :: !rules;
      reduce ~trs ~rules t'
  in
  let rules = ref [] in
  let t' = reduce ~trs ~rules t in
  t', !rules

let normalize trs t =
  fst (normalize_collect trs t)

