
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

  val declare_type : proof:Proof.t -> ty:Type.t -> var:term -> term list -> unit
  (** Declare that the given type's domain is the given list of cases
      for the given variable [var] (whose type must be [ty].
      Will be ignored if the type already has a enum declaration. *)

  val instantiate_vars : Env.multi_simpl_rule
  (** Instantiate variables whose type is a known enumerated type,
      with all variables of this type. *)

  val add_symbol_declaration : Env.unary_inf_rule
  (** Add an axiom for symbols whose return type is an enumeration *)

  (** {6 Registration} *)

  val register : unit -> unit
  (** Register rules in the environment *)
end

let _enable = ref true

module Make(E : Env.S) = struct
  module Env = E
  module C = Env.C
  module PS = Env.ProofState
  module Ctx = Env.Ctx

  type decl = {
    decl_ty : Type.t;
    decl_var : term;
    decl_cases : term list;
    decl_proof : Proof.t;
    mutable decl_symbols : Symbol.Set.t; (* set of declared symbols *)
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
  let _declare ~ty ~var ~proof cases =
    if List.exists (fun t -> not (Type.eq ty (T.ty t))) cases
      then failwith "EnumTypes: invalid declaration (type mismatch)";
    if Type.is_var ty
      then failwith "EnumTypes: cannot declare enum for type variable";
    match _find 1 ty 0 with
      | Some _ ->
          Util.debug 2 "EnumTypes: type %a already declared" Type.pp ty
      | None ->
          Util.debug 1 "EnumTypes: declare new enum type %a (cases %a = %a)"
            Type.pp ty T.pp var (CCList.pp ~sep:"|" T.pp) cases;
          Util.incr_stat stat_declare;
          (* set of already declared symbols *)
          let decl_symbols = List.fold_left
            (fun set t -> match T.head t with
              | None -> failwith "EnumTypes: non-symbolic case?"
              | Some s -> Symbol.Set.add s set
            ) Symbol.Set.empty cases
          in
          let decl = {
            decl_ty=ty;
            decl_var=var;
            decl_cases=cases;
            decl_symbols;
            decl_proof=proof;
          } in
          _decls := decl :: !_decls

  let declare_type ~proof ~ty ~var enum =
    _declare ~ty ~var ~proof enum

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

  (* retrieve variables that are directly under a positive equation *)
  let _vars_under_eq lits =
    Sequence.of_array lits
      |> Sequence.flatMap Lit.Seq.terms
      |> Sequence.filter T.is_var
      |> T.Seq.add_set T.Set.empty
      |> T.Set.elements

  let instantiate_vars c =
    let vars = _vars_under_eq (C.lits c) in
    let s_c = 0 and s_decl = 1 in
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
                  Util.debug 4 "deduce %a\n   from %a (enum_type switch on %a)"
                    C.pp c' C.pp c Type.pp decl.decl_ty;
                  c'
                ) decl.decl_cases
            )
      ) vars

  (* traverse term sequence and yield symbols whose type is a specialized
      version of some enum type *)
  let _find_symbols_of_enum_type s_decl terms s_t =
    let set = Symbol.Set.of_seq (terms |> Sequence.flatMap T.Seq.symbols) in
    Symbol.Set.to_seq set
      |> Sequence.fmap
        (fun s ->
          (* return type of [s] *)
          let ty = Ctx.find_signature_exn s in  (* FIXME: can fail *)
          let ty_ret = snd (Type.open_fun ty) in
          match _find s_decl ty_ret s_t with
          | Some (decl,subst) when not (Symbol.Set.mem s decl.decl_symbols) ->
              (* undeclared symbol *)
              Some (s, ty, decl, subst)
          | Some _
          | None -> None
        )

  let _make_term_of_sym s ty =
    match Type.arity ty with
    | Type.Arity(0,0)
    | Type.NoArity ->
        T.const ~ty s
    | Type.Arity (i,j) ->
        let ty_vars = if i>0 then CCList.range 1 i |> List.map Type.var else [] in
        let ty' = Type.apply_list ty ty_vars in
        let ty_args = Type.expected_args ty' in
        let vars = List.mapi (fun i ty -> T.var ~ty i) ty_args in
        T.app_full (T.const ~ty s) ty_vars vars

  let add_symbol_declaration c =
    let symbols = C.lits c
      |> Sequence.of_array
      |> Sequence.flatMap Lit.Seq.terms
      |> (fun seq -> _find_symbols_of_enum_type 1 seq 0)
      |> Sequence.to_list
    in
    List.map
      (fun (s,ty,decl,subst) ->
        (* declare symbol! *)
        let t = _make_term_of_sym s ty in
        let subst = Unif.FO.matching ~subst ~pattern:decl.decl_var 1 t 0 in
        let renaming = S.Renaming.create () in
        let lits = List.map
          (fun case -> Lit.mk_eq
            (S.FO.apply ~renaming subst t 0)
            (S.FO.apply ~renaming subst case 1)
          ) decl.decl_cases
        in
        let proof cc = Proof.mk_c_inference ~rule:"axiom_enum_types" cc [decl.decl_proof] in
        let c' = C.create lits proof in
        Util.debug 1 "declare enum type for %a: clause %a" Symbol.pp s C.pp c';
        c'
      ) symbols

  let register () =
    if !_enable then begin
      Util.debug 1 "register handling of enumerated types";
      Env.add_multi_simpl_rule instantiate_vars;
      Env.add_unary_inf "add_enum_declaration" add_symbol_declaration;
      (* detect whether the clause is a declaration of enum type, and if it
          is, declare the type! *)
      let _detect_and_declare c =
        begin match _detect_declaration c with
        | None -> ()
        | Some (ty,var,cases) -> _declare ~ty ~var ~proof:(C.proof c) cases
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
