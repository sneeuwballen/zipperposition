
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


module T = Logtk.FOTerm
module Util = Logtk.Util
module Form = Logtk.Formula.FO
module Lits = Literals

module StringTbl = CCHashtbl.Make(struct
  type t = string
  let hash = CCString.hash
  let equal = CCString.equal
end)

module type S = BBox_intf.S

let section = Util.Section.make ~parent:Const.section "bbox"

module Make(I : BBox_intf.TERM)
           (Case : BBox_intf.TERM)
           (Sub : BBox_intf.TERM) =
struct
  module BLit = Qbf.Lit
  type t = BLit.t

  module I = I
  module Case = Case
  module Sub = Sub

  type inductive_cst = I.t
  type inductive_sub_cst = Sub.t
  type inductive_case = Case.t

  let neg = BLit.neg
  let sign = BLit.sign
  let abs = BLit.abs
  let set_sign = BLit.set_sign

  [@@@warning "-39"]

  type ctx_predicate =
    | InLoop (** lits = ctx[i], where ctx in loop(i) *)
    | InitOk (** Ctx is initialized *or* it's not in loop *)
    | ExpressesMinimality of Sub.t
    [@@deriving ord]

  type injected =
    | Clause_component of Literals.t
    | Form of Form.t [@compare Form.cmp]
    | Ctx of
      ClauseContext.t
      * (inductive_cst [@compare I.compare])
      * ctx_predicate
    | Name of string  (* name for CNF *)
    | Input (** input marker *)
    [@@deriving ord]

  let string_of_ctx_pred ctx i = function
    | InLoop ->
        CCFormat.sprintf "⟦%a ∈ loop(%a)⟧"
          ClauseContext.print ctx I.print i
    | InitOk ->
        CCFormat.sprintf "⟦init(%a, %a)⟧"
          ClauseContext.print ctx I.print i
    | ExpressesMinimality t ->
        CCFormat.sprintf "⟦min(%a,%a,%a)⟧"
          ClauseContext.print ctx I.print i Sub.print t

  let pp_injected buf = function
    | Clause_component lits ->
        Printf.bprintf buf "⟦%a⟧" (Util.pp_array ~sep:" ∨ " Literal.pp) lits
    | Form  f -> Printf.bprintf buf "⟦%a⟧" Form.pp f
    | Ctx (lits, ind, pred) ->
        let s = string_of_ctx_pred lits ind pred in
        Buffer.add_string buf s
    | Input -> Printf.bprintf buf "input"
    | Name s ->
        Printf.bprintf buf "⟦%s⟧" s

  module FV = Logtk.FeatureVector.Make(struct
    type t = Lits.t * injected * BLit.t
    let cmp (l1,i1,j1)(l2,i2,j2) =
      CCOrd.(Lits.compare l1 l2
        <?> (compare_injected, i1, i2)
        <?> (BLit.compare, j1, j2))
    let to_lits (l,_,_) = Lits.Seq.abstract l
  end)
  module ITbl = CCHashtbl.Make(BLit)

  let _clause_set = ref (FV.empty())
  let _form_set = ref Form.Map.empty
  let _lit2inj = ITbl.create 56
  let _names = StringTbl.create 15

  let dummy_t = BLit.fresh()

  let _retrieve_alpha_equiv lits =
    let dummy_injected = Clause_component lits in
    FV.retrieve_alpha_equiv_c !_clause_set (lits,dummy_injected,dummy_t) ()
      |> Sequence.map2 (fun _ l -> l)

  let _input =
    let i = BLit.fresh () in
    ITbl.add _lit2inj i Input;
    i

  let _save injected t =
    ITbl.add _lit2inj t injected;
    Util.debug ~section 4 "save bool_lit %d = %a"
      (t:t:>int) pp_injected injected;
    match injected with
    | Clause_component lits ->
        (* also be able to retrieve by lits *)
        _clause_set := FV.add !_clause_set (lits, injected, t)
    | Form f ->
        _form_set := Form.Map.add f (injected, t) !_form_set
    | Ctx (cc, cst, _) ->
        _clause_set := FV.add !_clause_set
          (ClauseContext.apply cc (I.to_term cst), injected, t)
    | Input -> ()
    | Name s ->
        StringTbl.add _names s (injected, t)

  (* clause -> boolean lit *)
  let inject_lits lits  =
    (* special case: one negative literal. *)
    let lits, sign =
      if Array.length lits = 1 && Literal.is_neq lits.(0)
        then [| Literal.negate lits.(0) |], false
        else lits, true
    in
    (* retrieve clause. the index doesn't matter for retrieval *)
    _retrieve_alpha_equiv lits
      |> Sequence.filter_map
        (function
          | lits', Clause_component _, blit
            when Lits.are_variant lits lits' ->
              Some blit
          | _ -> None
        )
      |> Sequence.head
      |> (function
          | Some t -> BLit.apply_sign sign t
          | None ->
              let i = BLit.fresh () in
              (* maintain mapping *)
              let lits_copy = Array.copy lits in
              _save (Clause_component lits_copy) i;
              BLit.apply_sign sign i
          )

  let exists_form f = Form.Map.mem f !_form_set

  let inject_form f =
    try
      let _, t = Form.Map.find f !_form_set in
      t
    with Not_found ->
      let inj = Form f in
      let i = BLit.fresh () in
      _save inj i;
      i

  let inject_ctx ctx (t:I.t) pred =
    let lits = ClauseContext.apply ctx (I.to_term t) in
    _retrieve_alpha_equiv lits
      |> Sequence.filter_map
        (function
          | lits', Ctx (_, t', pred'), blit
            when Lits.are_variant lits lits'
            && I.equal t t' && compare_ctx_predicate pred pred' = 0
            -> Some blit
          | _ -> None
        )
      |> Sequence.head
      |> (function
          | Some blit -> blit
          | None ->
              let i = BLit.fresh() in
              (* maintain mapping *)
              _save (Ctx (ctx, t, pred)) i;
              i
          )

  let inject_input = _input

  let inject_name s =
    try snd (StringTbl.find _names s)
    with Not_found ->
      let i = BLit.fresh () in
      _save (Name s) i;
      i

  let inject_name' fmt =
    let buf = Buffer.create 16 in
    Printf.kbprintf
      (fun _ -> inject_name (Buffer.contents buf))
      buf fmt

  (* boolean lit -> injected *)
  let extract i =
    if not (BLit.sign i)
      then failwith "BBox.extract: require absolute bool lit";
    try Some (ITbl.find _lit2inj i)
    with Not_found -> None

  let extract_exn i =
    if not (BLit.sign i)
      then failwith "BBox.extract: require absolute bool lit";
    try ITbl.find _lit2inj i
    with Not_found -> failwith "BBox.extact: not a proper injected lit"

  let keep_in_splitting l = match extract_exn (BLit.abs l) with
    | Form _
    | Clause_component _ -> false
    | Ctx _
    | Input
    | Name _ -> true

  let inductive_cst b = match extract_exn b with
    | Name _
    | Form _
    | Input
    | Clause_component _ -> None
    | Ctx (_, t, _) -> Some t

  let iter_injected k = ITbl.values _lit2inj k

  let pp buf i =
    if not (BLit.sign i) then Buffer.add_string buf "¬";
    let i = BLit.abs i in
    match extract i with
    | None -> Printf.bprintf buf "L%d" (i:t:>int)
    | Some inj -> pp_injected buf inj

  let print fmt i =
    if not (BLit.sign i) then Format.pp_print_string fmt "¬";
    let i = BLit.abs i in
    match extract i with
    | None -> Format.fprintf fmt "L%d" (i:t:>int)
    | Some (Clause_component lits) ->
        Format.fprintf fmt "@[⟦%a⟧@]"
          (CCArray.print ~sep:" ∨ " Literal.fmt) lits
    | Some (Form f) ->
        Format.fprintf fmt "@[⟦%a⟧@]" Form.fmt f
    | Some (Ctx (lits, ind, pred)) ->
        let s = string_of_ctx_pred lits ind pred in
        Format.pp_print_string fmt s
    | Some Input -> Format.pp_print_string fmt "input"
    | Some (Name s) ->
        Format.fprintf fmt "⟦%s⟧" s
end
