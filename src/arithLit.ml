
(*
Zipperposition: a functional superposition prover for prototyping
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

open Logtk

type term = FOTerm.t
type scope = Substs.scope

module T = FOTerm
module S = Substs
module F = Formula.FO
module P = Position
module M = Monome
module MF = Monome.Focus

(** {2 Type Decls} *)

type op =
  | Equal
  | Different
  | Less
  | Lesseq

type 'm divides = {
  num : Z.t;
  power : int;
  monome : 'm;
  sign : bool;
} (** [num^power divides monome] or not. *)

type t =
  | Binary of op * Z.t Monome.t * Z.t Monome.t
  | Divides of Z.t Monome.t divides
(** Arithmetic literal (on integers) *)

type lit = t

(** {2 Basics} *)

let eq lit1 lit2 = match lit1, lit2 with
  | Binary (op1, x1, y1), Binary (op2, x2, y2) ->
      op1 = op2 && M.eq x1 x2 && M.eq y1 y2
  | Divides d1, Divides d2 ->
      d1.sign = d2.sign && d1.power = d2.power &&
      Z.equal d1.num d2.num && M.eq d1.monome d2.monome
  | _, _ -> false

let eq_com lit1 lit2 = match lit1, lit2 with
  | Binary (op1, x1, y1), Binary (op2, x2, y2)
  when op1 = op2 && (op1 = Equal || op1 = Different) ->
      (M.eq x1 x2 && M.eq y1 y2) || (M.eq x1 y2 && M.eq x2 y1)
  | _ -> eq lit1 lit2

let cmp lit1 lit2 = match lit1, lit2 with
  | Binary (op1, x1, y1), Binary (op2, x2, y2) ->
      let c = compare op1 op2 in
      if c <> 0 then c
      else let c = M.compare x1 x2 in
      if c <> 0 then c else M.compare y1 y2
  | Divides d1, Divides d2 ->
      Util.lexicograph_combine
        [ compare d1.sign d2.sign
        ; compare d1.power d2.power
        ; Z.compare d1.num d2.num
        ; M.compare d1.monome d2.monome ]
  | Binary _,  Divides _ -> 1
  | Divides _, Binary _ -> -1

let hash = function
  | Binary (op, m1, m2) -> Hash.hash_int3 (Hashtbl.hash op) (M.hash m1) (M.hash m2)
  | Divides d ->
      Hash.hash_int3 (Z.hash d.num) (M.hash d.monome) d.power

let sign = function
  | Binary ((Equal | Lesseq | Less), _, _) -> true
  | Binary (Different, _, _) -> false
  | Divides d -> d.sign

let is_pos = sign
let is_neg l = not (is_pos l)

let _is_bin p = function
  | Binary (op, _, _) -> p op
  | Divides _ -> false

let is_eq = _is_bin ((=) Equal)
let is_neq = _is_bin ((=) Different)
let is_eqn = _is_bin (function Equal | Different -> true | _ -> false)
let is_less = _is_bin ((=) Less)
let is_lesseq = _is_bin ((=) Lesseq)
let is_ineq = _is_bin (function Less | Lesseq -> true | _ -> false)
let is_divides = function
  | Divides _ -> true
  | Binary _ -> false

(* main constructor *)
let make op m1 m2 =
  let m = Monome.difference m1 m2 in
  let m1, m2 = Monome.split m in
  Binary (op, m1, m2)

let mk_eq = make Equal
let mk_neq = make Different
let mk_less = make Less
let mk_lesseq = make Lesseq

(* TODO: normalize coeffs by n^power *)
let mk_divides ?(sign=true) n ~power m =
  Divides { sign; num=n; power; monome=m; }

let mk_not_divides = mk_divides ~sign:false

let negate = function
  | Binary (op, m1, m2) ->
      begin match op with
      | Equal -> Binary (Different, m1, m2)
      | Different -> Binary (Equal, m1, m2)
      | Less -> Binary (Lesseq, m2, m1)
      | Lesseq -> Binary (Less, m2, m1)
      end
  | Divides d -> Divides { d with sign=not d.sign; }

let pp buf = function
  | Binary (op, l, r) ->
    Printf.bprintf buf "%a %s %a"
      M.pp l
      (match op with Equal -> "=" | Different -> "≠"
        | Less -> "<" | Lesseq -> "≤")
      M.pp r
  | Divides d when d.sign ->
    let nk = Z.pow d.num d.power in
    Printf.bprintf buf "%s | %a" (Z.to_string nk) M.pp d.monome
  | Divides d ->
    let nk = Z.pow d.num d.power in
    Printf.bprintf buf "¬(%s | %a)" (Z.to_string nk) M.pp d.monome

let pp_tstp buf = function
  | Binary (Equal, l, r) ->
    Printf.bprintf buf "%a = %a" M.pp_tstp l M.pp_tstp r
  | Binary (Different, l, r) ->
    Printf.bprintf buf "%a != %a" M.pp_tstp l M.pp_tstp r
  | Binary (Less, l, r) ->
    Printf.bprintf buf "$less(%a, %a)" M.pp_tstp l M.pp_tstp r
  | Binary (Lesseq, l, r) ->
    Printf.bprintf buf "$lesseq(%a, %a)" M.pp_tstp l M.pp_tstp r
  | Divides d when d.sign ->
    let nk = Z.pow d.num d.power in
    Printf.bprintf buf "$remainder_e(%a, %s) = 0" M.pp_tstp d.monome (Z.to_string nk)
  | Divides d ->
    let nk = Z.pow d.num d.power in
    Printf.bprintf buf "$remainder_e(%a, %s) != 0" M.pp_tstp d.monome (Z.to_string nk)

let to_string = Util.on_buffer pp
let fmt fmt lit = Format.pp_print_string fmt (to_string lit)

(** {2 Operators} *)

let map f = function
  | Binary (op, m1, m2) -> Binary (op, M.map f m1, M.map f m2)
  | Divides d -> Divides { d with monome=M.map f d.monome; }

let fold f acc = function
  | Binary (_, m1, m2) ->
      let acc = Sequence.fold f acc (Monome.Seq.terms m1) in
      Sequence.fold f acc (Monome.Seq.terms m2)
  | Divides d ->
      Sequence.fold f acc (Monome.Seq.terms d.monome)

type 'a unif = subst:Substs.t -> 'a -> scope -> 'a -> scope -> Substs.t Sequence.t

(* match {x1,y1} in scope 1, with {x2,y2} with scope2 *)
let unif4 op ~subst x1 y1 sc1 x2 y2 sc2 k =
  op ~subst x1 sc1 x2 sc2
    (fun subst -> op ~subst y1 sc1 y2 sc2 k);
  op ~subst y1 sc1 x2 sc2
    (fun subst -> op ~subst x1 sc1 y2 sc2 k);
  ()

let generic_unif m_unif ~subst lit1 sc1 lit2 sc2 k = match lit1, lit2 with
  | Binary (((Equal | Different) as op1), x1, y1),
    Binary (((Equal | Different) as op2), x2, y2) when op1 = op2 ->
    (* try both ways *)
    unif4 m_unif ~subst x1 y1 sc1 x2 y2 sc2 k
  | Binary (op1, x1, y1), Binary (op2, x2, y2) ->
    if op1 = op2
      then m_unif ~subst x1 sc1 x2 sc2
        (fun subst -> m_unif ~subst y1 sc1 y2 sc2 k)
  | Divides d1, Divides d2 ->
    if  Z.equal d1.num d2.num && d1.power = d2.power && d1.sign = d2.sign
      then m_unif ~subst d1.monome sc1 d2.monome sc2 k
  | Binary _, Divides _
  | Divides _, Binary _ -> ()

let unify ?(subst=Substs.empty) lit1 sc1 lit2 sc2 =
  generic_unif (fun ~subst -> M.unify ~subst) ~subst lit1 sc1 lit2 sc2

let matching ?(subst=Substs.empty) lit1 sc1 lit2 sc2 =
  generic_unif (fun ~subst -> M.matching ~subst) ~subst lit1 sc1 lit2 sc2

let variant ?(subst=Substs.empty) lit1 sc1 lit2 sc2 =
  generic_unif (fun ~subst -> M.variant ~subst) ~subst lit1 sc1 lit2 sc2

let are_variant lit1 lit2 =
  not (Sequence.is_empty (variant lit1 0 lit2 1))

let apply_subst ~renaming subst lit scope = match lit with
  | Binary (op, m1, m2) ->
    Binary (op,
      M.apply_subst ~renaming subst m1 scope,
      M.apply_subst ~renaming subst m1 scope)
  | Divides d ->
      Divides { d with monome=M.apply_subst ~renaming subst d.monome scope; }

let is_trivial = function
  | Divides d when d.sign -> M.is_const d.monome && Z.sign (M.const d.monome) = 0
  | Divides d -> M.is_const d.monome && Z.sign (M.const d.monome) <> 0
  | Binary (Equal, m1, m2) -> M.eq m1 m2
  | Binary (Less, m1, m2) -> M.dominates ~strict:true m2 m1
  | Binary (Lesseq, m1, m2) -> M.dominates ~strict:false m2 m1
  | Binary (Different, m1, m2) ->
      let m = M.difference m1 m2 in
      M.is_const m && Z.sign (M.const m) <> 0

let is_absurd = function
  | Binary (Equal, m1, m2) ->
      let m = M.difference m1 m2 in
      M.is_const m && M.sign m <> 0
  | Binary (Different, m1, m2) -> M.eq m1 m2
  | Binary (Less, m1, m2) ->
      let m = M.difference m1 m2 in
      M.is_const m && M.sign m >= 0
  | Binary (Lesseq, m1, m2) ->
      let m = M.difference m1 m2 in
      M.is_const m && M.sign m > 0
  | _ -> false   (* TODO *)

let fold_terms ?(pos=P.stop) ?(vars=false) ~which ~ord ~subterms lit acc f =
  (* function to call at terms *)
  let at_term ~pos acc t =
    if subterms
      then T.all_positions ~vars ~pos t acc f
      else f acc t pos
  and fold_monome = match which with
    | `All -> M.fold
    | `Max -> M.fold_max ~ord
  in
  match lit with
  | Binary (op, m1, m2) ->
      let acc = fold_monome
        (fun acc i _ t -> at_term ~pos:P.(append pos (left (arg i stop))) acc t)
        acc m1
      in
      fold_monome
        (fun acc i _ t -> at_term ~pos:P.(append pos (right (arg i stop))) acc t)
        acc m2
  | Divides d ->
      fold_monome
        (fun acc i _ t -> at_term ~pos:P.(append pos (arg i stop)) acc t)
        acc d.monome

let max_terms ~ord = function
  | Binary (_, m1, m2) ->
      let l = M.terms m1 @ M.terms m2 in
      Multiset.max_l (Ordering.compare ord) l
  | Divides d ->
      Multiset.max_l (Ordering.compare ord) (M.terms d.monome)

let to_form = function
  | Binary (op, m1, m2) ->
    let t1 = M.Int.to_term m1 in
    let t2 = M.Int.to_term m2 in
    begin match op with
      | Equal -> F.Base.eq t1 t2
      | Different -> F.Base.neq t1 t2
      | Less ->
        let sym = Symbol.TPTP.Arith.less in
        let ty = Signature.find_exn Signature.TPTP.Arith.base sym in
        let cst = T.const ~ty sym in
        F.Base.atom (T.app_full cst [Type.TPTP.int] [t1; t2])
      | Lesseq ->
        let sym = Symbol.TPTP.Arith.lesseq in
        let ty = Signature.find_exn Signature.TPTP.Arith.base sym in
        let cst = T.const ~ty sym in
        F.Base.atom (T.app_full cst [Type.TPTP.int] [t1; t2])
    end
  | Divides d ->
    let nk = Z.pow d.num d.power in
    let t = M.Int.to_term d.monome in
    let sym = Symbol.TPTP.Arith.remainder_e in
    let ty = Signature.find_exn Signature.TPTP.Arith.base sym in
    let cst = T.const ~ty sym in
    (* $remainder_e(t, nk) = 0 *)
    let f = F.Base.eq
      (T.const ~ty:Type.TPTP.int (Symbol.of_int 0))
      (T.app cst [t; T.const ~ty:Type.TPTP.int (Symbol.mk_int nk)])
    in
    if d.sign then f else F.Base.not_ f

(** {2 Iterators} *)

module Seq = struct
  let terms lit k = match lit with
    | Binary (_, m1, m2) -> M.Seq.terms m1 k; M.Seq.terms m2 k
    | Divides d -> M.Seq.terms d.monome k

  let vars lit = terms lit |> Sequence.flatMap T.Seq.vars
end

(** {2 Focus on a Term} *)

module Focus = struct
  (** focus on a term in one of the two monomes *)
  type t =
    | Left of op * Z.t Monome.Focus.t * Z.t Monome.t
    | Right of op * Z.t Monome.t * Z.t Monome.Focus.t
    | Div of Z.t Monome.Focus.t divides

  let get lit pos =
    match lit, pos with
    | Binary (op, m1, m2), P.Left (P.Arg (i, _)) ->
        Some (Left (op, M.Focus.get m1 i, m2))
    | Binary (op, m1, m2), P.Right (P.Arg (i, _)) ->
        Some (Right (op, m1, M.Focus.get m2 i))
    | Divides d, P.Arg (i, _) ->
        let d' = {
          sign=d.sign; power=d.power; num=d.num;
          monome=M.Focus.get d.monome i;
        } in
        Some (Div d')
    | _ -> None

  let get_exn lit pos = match get lit pos with
    | None ->
      invalid_arg
        (Util.sprintf "wrong position %a for focused arith lit %a"
          P.pp pos pp lit)
    | Some x -> x

  let focused_monome = function
    | Left (_, mf, _)
    | Right (_, _, mf) -> mf
    | Div d -> d.monome

  let term lit = MF.term (focused_monome lit)

  let fold_terms ?(pos=P.stop) lit acc f =
    match lit with
    | Binary (op, m1, m2) ->
      let acc = MF.fold_m ~pos:P.(append pos (left stop)) m1 acc
        (fun acc mf pos -> f acc (Left (op, mf, m2)) pos)
      in
      let acc = MF.fold_m ~pos:P.(append pos (right stop)) m2 acc
        (fun acc mf pos -> f acc (Right (op, m1, mf)) pos)
      in acc
    | Divides d ->
      MF.fold_m ~pos d.monome acc
        (fun acc mf pos -> f acc (Div {d with monome=mf}) pos)

  (* is the focused term maximal in the arithmetic literal? *)
  let is_max ~ord = function
    | Left (_, mf, m)
    | Right (_, m, mf) ->
        let t = MF.term mf in
        let terms = Sequence.append (M.Seq.terms m) (MF.rest mf |> M.Seq.terms) in
        Sequence.for_all
          (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
          terms
    | Div d ->
        let t = MF.term d.monome in
        Sequence.for_all
          (fun t' -> Ordering.compare ord t t' <> Comparison.Lt)
          (MF.rest d.monome |> M.Seq.terms)

  let map_lit ~f_m ~f_mf lit = match lit with
    | Left (op, mf, m) ->
        Left (op, f_mf mf, f_m m)
    | Right (op, m, mf) ->
        Right (op, f_m m, f_mf mf)
    | Div d ->
        Div { d with monome=f_mf d.monome; }

  let product lit z =
    map_lit
      ~f_mf:(fun mf -> MF.product mf z)
      ~f_m:(fun m -> M.product m z)
      lit

  let apply_subst ~renaming subst lit scope =
    map_lit
      ~f_mf:(fun mf -> MF.apply_subst ~renaming subst mf scope)
      ~f_m:(fun m -> M.apply_subst ~renaming subst m scope)
      lit

  let unify ?(subst=Substs.empty) lit1 sc1 lit2 sc2 k =
    let _set_mf lit mf = match lit with
    | Left (op, _, m) -> Left (op, mf, m)
    | Right (op, m, _) -> Right (op, m, mf)
    | Div d ->
        Div { d with monome=mf; }
    in
    MF.unify_ff ~subst (focused_monome lit1) sc1 (focused_monome lit2) sc2
      (fun (mf1, mf2, subst) ->
        k (_set_mf lit1 mf1, _set_mf lit2 mf2, subst))

  (* scale focused literals to have the same coefficient *)
  let scale l1 l2 =
    let z1 = MF.coeff (focused_monome l1)
    and z2 = MF.coeff (focused_monome l2) in
    let gcd = Z.gcd z1 z2 in
    product l1 (Z.divexact z2 gcd), product l2 (Z.divexact z1 gcd)

  let op = function
    | Left (op, _, _)
    | Right (op, _, _) -> `Binary op
    | Div _ -> `Divides

  let unfocus = function
    | Left (op, m1_f, m2) -> Binary (op, MF.to_monome m1_f, m2)
    | Right (op, m1, m2_f) -> Binary (op, m1, MF.to_monome m2_f)
    | Div d ->
        let d' = {
          num=d.num; power=d.power; sign=d.sign;
          monome=MF.to_monome d.monome;
        } in
        Divides d'

  let pp buf lit =
    let op2str = function
      | Equal -> "="
      | Different -> "≠"
      | Less -> "<"
      | Lesseq -> "≤"
    in
    match lit with
    | Left (op, mf, m) ->
        Printf.bprintf buf "%a %s %a" MF.pp mf (op2str op) M.pp m
    | Right (op, m, mf) ->
        Printf.bprintf buf "%a %s %a" M.pp m (op2str op) MF.pp mf
    | Div d when d.sign ->
      let nk = Z.pow d.num d.power in
      Printf.bprintf buf "%s | %a" (Z.to_string nk) MF.pp d.monome
    | Div d ->
      let nk = Z.pow d.num d.power in
      Printf.bprintf buf "¬(%s | %a)" (Z.to_string nk) MF.pp d.monome

  let to_string = Util.on_buffer pp
  let fmt fmt lit = Format.pp_print_string fmt (to_string lit)
end
