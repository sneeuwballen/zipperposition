
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

(** {1 Inference and simplification rules for Algebraic types} *)

open Logtk

module T = FOTerm
module S = Substs
module Lit = Literal
module Lits = Literals

type term = T.t

let stat_declare = Util.mk_stat "enum_type.declare"
let stat_simplify = Util.mk_stat "enum_type.simplify"

(** {2 Inference rules} *)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  val declare_type : ty:Type.t -> var:term -> term list -> unit
  (** Declare that the given type's domain is the given list of cases
      for the given variable [var] (whose type must be [ty].
      Will be ignored if the type already has a enum declaration. *)

  val simplify : Env.multi_simpl_rule

  (** {6 Registration} *)

  val register : unit -> unit
  (** Register rules in the environment *)
end

let _enable = ref true

module Make(E : Env.S) = struct
  module Env = E
  module C = Env.C
  module PS = Env.ProofState

  type decl = {
    decl_ty : Type.t;
    decl_var : term;
    decl_cases : term list;
  }

  (* set of enumerated types *)
  let _decls = ref []

  (* find whether some declaration matches this type, and return it *)
  let _find ?(subst=S.empty) s_decl ty s_ty =
    CCList.find
      (fun decl ->
        try
          let subst = Unif.Ty.matching ~subst ~pattern:decl.decl_ty s_decl ty s_ty in
          Some (decl, subst)
        with Unif.Fail -> None
      ) !_decls

  (* declare an enumerated type *)
  let _declare ~ty ~var cases =
    if List.exists (fun t -> not (Type.eq ty (T.ty t))) cases
      then failwith "EnumTypes: invalid declaration (type mismatch)";
    if Type.is_var ty
      then failwith "EnumTypes: cannot declare enum for type variable";
    match _find 1 ty 0 with
      | Some _ ->
          Util.debug 2 "EnumTypes: type %a already declared" Type.pp ty
      | None ->
          Util.debug 2 "EnumTypes: declare new enum type %a (cases %a = %a)"
            Type.pp ty T.pp var (CCList.pp ~sep:"|" T.pp) cases;
          Util.incr_stat stat_declare;
          let decl = {
            decl_ty=ty;
            decl_var=var;
            decl_cases=cases;
          } in
          _decls := decl :: !_decls

  let declare_type ~ty ~var enum =
    _declare ~ty ~var enum

  (* detect whether the clause [c] is a declaration of enum type *)
  let _detect_declaration c =
    (* loop over literals checking whether they are all of the form
      [var = t] for some [t] *)
    let rec _check_all_vars ~ty ~var acc lits = match lits with
      | [] -> Some (ty, var, acc)
      | Lit.Equation (l, r, true) :: lits' when T.eq l var ->
          _check_all_vars ~ty ~var (r::acc) lits'
      | Lit.Equation (l, r, true) :: lits' when T.eq r var ->
          _check_all_vars ~ty ~var (l::acc) lits'
      | _ -> None
    in
    match Array.to_list (C.lits c) with
    | Lit.Equation (l,r,true) :: lits when T.is_var l && not (Type.is_var (T.ty l))->
        _check_all_vars ~ty:(T.ty l) ~var:l [r] lits
    | Lit.Equation (l,r,true) :: lits when T.is_var r && not (Type.is_var (T.ty r))->
        _check_all_vars ~ty:(T.ty r) ~var:r [l] lits
    | _ -> None

  (* variables occurring under some function symbol (at non-0 depth) *)
  let _shielded_vars lits =
    Sequence.of_array lits
      |> Sequence.flatMap Lit.Seq.terms
      |> Sequence.flatMap T.Seq.subterms_depth
      |> Sequence.fmap
        (fun (v,depth) ->
          if depth>0 && T.is_var v then Some v else None
        )
      |> T.Seq.add_set T.Set.empty

  let _naked_vars lits =
    let v = Sequence.of_array lits
      |> Sequence.flatMap Lit.Seq.vars
      |> T.Seq.add_set T.Set.empty
    in
    T.Set.diff v (_shielded_vars lits)
      |> T.Set.elements

  let simplify c =
    let naked = _naked_vars (C.lits c) in
    let s_c = 0 and s_decl = 1 in
    match _detect_declaration c with
    | Some _ -> None (* do not eliminate enum declarations themselves *)
    | None ->
      CCList.find
        (fun v ->
          match _find s_decl (T.ty v) s_c with
          | None -> None
          | Some (decl, subst) ->
              (* we found an enum type declaration for [v], replace it
                with each case for the enum type *)
              Util.incr_stat stat_simplify;
              Some (
                List.map
                  (fun case ->
                    (* replace [v] with [case] now *)
                    let subst = Substs.FO.bind subst v s_c case s_decl in
                    let renaming = S.Renaming.create () in
                    let lits' = Lits.apply_subst ~renaming subst (C.lits c) s_c in
                    let proof cc = Proof.mk_c_inference ~info:[S.to_string subst]
                      ~rule:"enum_type_case_switch" cc [C.proof c]
                    in
                    let c' = C.create_a ~parents:[c] lits' proof in
                    Util.debug 4 "deduce %a from %a (enum_type switch on %a)"
                      C.pp c C.pp c' Type.pp decl.decl_ty;
                    c'
                  ) decl.decl_cases
              )
        ) naked

  let register () =
    if !_enable then begin
      Util.debug 1 "register handling of enumerated types";
      Env.add_multi_simpl_rule simplify;
      (* detect whether the clause is a declaration of enum type, and if it
          is, declare the type! *)
      let _detect_and_declare c =
        begin match _detect_declaration c with
        | None -> ()
        | Some (ty,var,cases) -> _declare ~ty ~var cases
        end; Signal.ContinueListening
      in
      Signal.on PS.PassiveSet.on_add_clause _detect_and_declare;
      Signal.on PS.ActiveSet.on_add_clause _detect_and_declare;
    end
end

(* TODO: during preprocessing, scan clauses to find declarations asap *)

(** {2 As Extension} *)

let extension =
  let module DOIT(Env : Env.S) = struct
    include Extensions.MakeAction(Env)
    module ET = Make(Env)
    let actions =
      [ Ext_general ET.register
      ]
  end in
  { Extensions.default with
    Extensions.name = "enum_types";
    Extensions.make=(module DOIT : Extensions.ENV_TO_S);
  }

let () =
  Extensions.register extension;
  Params.add_opts
    [ "-enum-types"
      , Arg.Bool (fun b -> _enable := b)
      , "enable/disable special handling for enumerated types"
    ]
