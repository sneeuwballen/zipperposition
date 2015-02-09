
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013-2015, Simon Cruanes
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

(** {1 Common stuff for Induction} *)

module T = Logtk.FOTerm
module F = Logtk.Formula.FO
module Ty = Logtk.Type
module Sym = Logtk.Symbol
module Util = Logtk.Util
module Lits = Literals

type term = Logtk.FOTerm.t
type sym = Logtk.Symbol.t
type formula = F.t

let section = Const.section

let ind_types_ = ref []
let cover_set_depth_ = ref 1

let ind_types () = !ind_types_
let cover_set_depth () = !cover_set_depth_

(* is [s] a constructor symbol for some inductive type? *)
let is_constructor s = match s with
  | Sym.Cst info ->
      let name = info.Sym.cs_name in
      List.exists (fun (_, cstors) -> List.mem name cstors) !ind_types_
  | _ -> false

let on_enable = Signal.create()

type kind = [`Full | `Simple]

let show_kind : kind -> string
  = function
  | `Full -> "qbf"
  | `Simple -> "sat"

let kind_ : kind ref = ref `Full

let enabled_ = ref false
let enable_ () =
  if not !enabled_ then (
    enabled_ := true;
    Util.debugf ~section 1
      "Induction(%s): requires ord=rpo6; select=NoSelection" (show_kind !kind_);
    Params.ord := "rpo6";   (* new default! RPO is necessary*)
    Params.dot_all_roots := true;  (* print proofs more clearly *)
    Params.select := "NoSelection";
    Signal.send on_enable !kind_;
  )

let declare_ ty cstors =
  (* remember to declare this type as inductive *)
  Util.debug ~section 1 "user declares inductive type %s = %a"
    ty (CCList.pp CCString.pp) cstors;
  ind_types_ := (ty, cstors) :: !ind_types_;
  enable_();
  ()

let constr_cstors =
  let module C = Logtk.Comparison in
  fun s1 s2 -> match is_constructor s1, is_constructor s2 with
    | true, true
    | false, false -> if Sym.eq s1 s2 then C.Eq else C.Incomparable
    | true, false -> C.Lt
    | false, true -> C.Gt

module Make(Ctx : Ctx.S) = struct
  (* declare a list of inductive types *)
  let declare_types () =
    List.iter
      (fun (ty,cstors) ->
        (* TODO: support polymorphic types? *)
        let pattern = Ty.const (Sym.of_string ty) in
        let constructors = List.map
          (fun str ->
            let s = Sym.of_string str in
            match Ctx.find_signature s with
              | None ->
                  let msg = Util.sprintf
                    "cannot find the type of inductive constructor %s" str
                  in failwith msg
              | Some ty ->
                  s, ty
          ) cstors
        in
        (* declare type. *)
        ignore (Ctx.Induction.declare_ty pattern constructors);
        Util.debug ~section 1 "declare inductive type %a" Ty.pp pattern;
        ()
      ) !ind_types_

  (* true if [t = c] where [c] is some inductive constructor
      such as "cons" or "node" *)
  let is_a_constructor t = match T.Classic.view t with
    | T.Classic.App (s, _, _) ->
        Sequence.exists (Sym.eq s) Ctx.Induction.Seq.constructors
    | _ -> false

  (* find inductive constants in clauses of [seq] *)
  let find_inductive_cst lits : T.t Sequence.t =
    Lits.Seq.terms lits
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.filter
      (fun t ->
        T.is_ground t
        && T.is_const t
        && not (Ctx.Induction.is_blocked t)
        && Ctx.Induction.is_inductive_type (T.ty t)
        && not (is_a_constructor t)   (* 0 and nil: not inductive const *)
          )

  (* ensure s1 > s2 if s1 is an inductive constant
      and s2 is a sub-case of s1 *)
  let constr_sub_cst s1 s2 =
    let module C = Logtk.Comparison in
    let res =
      if Ctx.Induction.is_inductive_symbol s1 && Ctx.Induction.dominates s1 s2
        then C.Gt
      else if Ctx.Induction.is_inductive_symbol s2 && Ctx.Induction.dominates s2 s1
        then C.Lt
      else C.Incomparable
    in res
end

module MakeAvatar(A : Avatar.S) = struct
  module Env = A.E
  module Ctx = Env.Ctx
  module BoolLit = Ctx.BoolLit
  module C = Env.C
  module CI = Ctx.Induction

  (* XXX Clear definitions introduced for Skolemization. This is necessary
      to avoid the following case:

        prove a+b != b+a
        introduce lemma !X: !Y: X+Y = Y+X
        to prove lemma, cnf(~ !X: !Y: X+Y = Y+X)
        obtain a+b != b+a instead of fresh variables! *)
  let clear_skolem_ctx () =
    let module S = Logtk.Skolem in
    Util.debug ~section 1 "forget Skolem definitions";
    S.clear_skolem_cache ~ctx:Ctx.skolem;
    S.all_definitions ~ctx:Ctx.skolem
      |> Sequence.iter (S.remove_def ~ctx:Ctx.skolem)

  (* generic mechanism for adding a formula
      and make a lemma out of it, including Skolemization, etc. *)
  let introduce_cut f proof : C.t list * BoolLit.t =
    let f = F.close_forall f in
    let box = BoolLit.inject_form f in
    (* positive clauses *)
    let c_pos =
      PFormula.create f (Proof.mk_f_trivial f) (* proof will be ignored *)
      |> PFormula.Set.singleton
      |> Env.cnf
      |> C.CSet.to_list
      |> List.map
        (fun c ->
          let trail = C.Trail.singleton box in
          C.create_a ~trail (C.lits c) proof
        )
    in
    let c_neg =
      PFormula.create (F.Base.not_ f) (Proof.mk_f_trivial f)
      |> PFormula.Set.singleton
      |> Env.cnf
      |> C.CSet.to_list
      |> List.map
        (fun c ->
          let trail = C.Trail.singleton (BoolLit.neg box) in
          C.create_a ~trail (C.lits c) proof
        )
    in
    c_pos @ c_neg, box

  (* terms that are either inductive constants or sub-constants *)
  let constants_or_sub c : T.Set.t =
    C.Seq.terms c
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.filter
      (fun t -> CI.is_inductive t || CI.is_sub_constant t)
    |> T.Set.of_seq

  (* apply the list of replacements [l] to the term [t] *)
  let replace_many l t =
    List.fold_left
      (fun t (old,by) -> T.replace t ~old ~by)
      t l

  let flag_cut_introduced = C.new_flag()

  let is_ind_conjecture_ c =
    match C.distance_to_conjecture c with
    | Some (0 | 1) -> true
    | Some _
    | None -> false

  let has_pos_lit_ c =
    CCArray.exists Literal.is_pos (C.lits c)

  (* when a clause has inductive constants, take its negation
      and add it as a lemma *)
  let inf_introduce_lemmas c =
    if C.is_ground c
    && not (is_ind_conjecture_ c)
    && not (C.get_flag flag_cut_introduced c)
    && not (has_pos_lit_ c) (* XXX: only positive lemmas *)
    then
      let set = constants_or_sub c in
      if T.Set.is_empty set (* XXX ? || T.Set.for_all CI.is_inductive set *)
      then [] (* no inductive, or already ongoing induction *)
      else (
        (* fresh var generator *)
        let mk_fvar =
          let r = ref 0 in
          fun ty ->
            let v = T.var ~ty !r in
            incr r;
            v
        in
        (* abstract w.r.t all those constants *)
        let replacements = T.Set.fold
          (fun cst acc ->
            (cst, mk_fvar (T.ty cst)) :: acc
          ) set []
        in
        (* replace constants by variables in [c], then
            let [f] be [forall... bigAnd_{l in c} not l] *)
        let f = C.lits c
          |> Literals.map (replace_many replacements)
          |> Array.to_list
          |> List.map (fun l -> Ctx.Lit.to_form (Literal.negate l))
          |> F.Base.and_
          |> F.close_forall
        in
        (* if [box f] already exists, no need to re-do inference *)
        if BoolLit.exists_form f
        then []
        else (
          (* introduce cut now *)
          let proof cc = Proof.mk_c_trivial ~theories:["ind"] ~info:["cut"] cc in
          let clauses, _ = introduce_cut f proof in
          List.iter (fun c -> C.set_flag flag_cut_introduced c true) clauses;
          Util.debugf ~section 2 "@[<2>introduce cut@ from %a@ @[<hv0>%a@]@]"
            C.fmt c (CCList.print ~start:"" ~stop:"" C.fmt) clauses;
          clauses
        )
      )
    else []

  (* TODO: copy redundancy criteria from QBF ? *)

  let show_lemmas () =
    let forms = BoolLit.iter_injected
      |> Sequence.filter_map
        (function
          | BoolLit.Form f -> Some f
          | _ -> None
        )
      |> Sequence.to_rev_list
    in
    Util.debugf ~section 1 "@[<2>lemmas:@ @[<hv0>%a@]@]"
      (CCList.print ~start:"" ~stop:"" F.fmt) forms
end

module A = Logtk_parsers.Ast_tptp

let init_from_decls pairs =
  let get_str = function
    | A.GNode (s, []) | A.GString s -> s
    | _ -> raise Exit
  in
  (* search for "inductive(c1, c2, ...)" *)
  let rec scan_for_constructors = function
    | A.GNode ("inductive", l) :: tail when List.length l >= 2 ->
        begin try
          let constructors = List.map get_str l in
          Some constructors
        with Exit ->
          scan_for_constructors tail
        end
    | _ :: tail -> scan_for_constructors tail
    | []  -> None
  in
  Sequence.iter
    (fun (ty, info) -> match scan_for_constructors info with
      | None -> ()
      | Some l -> declare_ ty l
    ) pairs

let setup_induction_ str = match str with
  | "qbf" ->
      kind_ := `Full;
      enable_ ()
  | "sat" ->
      kind_ := `Simple;
      enable_ ()
  | _ ->
      failwith ("unknown QBF kind: " ^ str ^ " (known: 'sat', 'qbf')")

let () =
  Params.add_opts
    [ "-induction", Arg.String setup_induction_,
      " enable induction with the given style ('qbf' | 'sat')"
    ; "-induction-depth", Arg.Set_int cover_set_depth_,
      " set default induction depth"
    ]
