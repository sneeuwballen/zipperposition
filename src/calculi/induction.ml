
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

(** {1 Induction Through QBF} *)

module Sym = Logtk.Symbol
module T = Logtk.FOTerm
module Ty = Logtk.Type
module Util = Logtk.Util
module Lits = Literals

module type S = sig
  module Env : Env.S
  module Ctx : module type of Env.Ctx

  val scan : Env.C.t Sequence.t -> unit
  val register : unit -> unit
end

let _ind_types = ref []

module Make(E : Env.S)(Solver : BoolSolver.QBF) = struct
  module Env = E
  module Ctx = Env.Ctx

  module CI = Ctx.Induction
  module BoolLit = Ctx.BoolLit
  module Avatar = Avatar.Make(E)(Solver)  (* will use some inferences *)

  let _level0 = Solver.level0
  let _level1 = Solver.push Qbf.Forall []
  let _level2 = Solver.push Qbf.Exists []

  (* maps each inductive constant to
      set(clause contexts that are candidate for induction) *)
  let _candidates : ClauseContext.Set.t T.Tbl.t =
    T.Tbl.create 15

  (* scan clauses for ground terms of an inductive type, and declare those terms *)
  let scan seq =
    Sequence.iter
      (fun c ->
        Lits.Seq.terms (Env.C.lits c)
        |> Sequence.filter
          (fun t ->
            T.is_ground t && CI.is_inductive_type (T.ty t)
          )
        |> Sequence.iter (fun t -> CI.declare t)
      ) seq

  (* detect ground terms of an inductive type, and perform a special
      case split with Xor on them. *)
  let case_split_ind c =
    None (* TODO *)

  (* declare a list of inductive types *)
  let _declare_types l =
    List.iter
      (fun (ty,cstors) ->
        (* TODO: support polymorphic types? *)
        let pattern = Ty.const (Sym.of_string ty) in
        let constructors = List.map
          (fun str ->
            let s = Sym.of_string str in
            match Ctx.find_signature s with
              | None ->
                  failwith
                    (Util.sprintf
                      "cannot find the type of inductive constructor %s" str)
              | Some ty ->
                  s, ty
          ) cstors
        in
        (* declare type. *)
        ignore (CI.declare_ty pattern constructors);
        ()
      ) l

  (* TODO: use inference rules from Avatar, but requiring a QBF solver.
     TODO: Do not use its check, use the QBF check instead,
        probably less often, but with provability relation.

        XXX: when shall we encode the provability relation?
        XXX: manage set of clause contexts to subsume/be subsumed
        XXX: manager S_candidates (contexts for which C[n]<-trail provable) *)

  let register() =
    Util.debug 1 "register induction calculus";
    _declare_types !_ind_types;
    (* avatar rules *)
    Env.add_multi_simpl_rule Avatar.split;
    Env.add_unary_inf "avatar.check_empty" Avatar.check_empty;
    Env.add_generate "avatar.check_sat" Avatar.check_satisfiability;
    (* induction rules TODO *)
    Env.add_multi_simpl_rule case_split_ind;
    ()
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module Solver = (val BoolSolver.get_qbf() : BoolSolver.QBF) in
    Util.debug 1 "created QBF solver \"%s\"" Solver.name;
    let module A = Make(E)(Solver) in
    A.register()
  in
  Extensions.({default with name="induction"; actions=[Do action]})

let _enabled = ref false
let _enable () =
  if not !_enabled then (
    _enabled := true;
    Extensions.register extension
  )

(* [str] describes an inductive type, under the form "foo:c1|c2|c3" where
    "foo" is the type name and "c1", "c2", "c3" are the type constructors. *)
let _add_ind_type str =
  _enable();
  let _fail() =
    failwith "expected \"type:c1|c2|c3\" where c1,... are constructors"
  in
  match Util.str_split ~by:":" str with
  | [ty; cstors] ->
      let cstors = Util.str_split ~by:"|" str in
      if List.length cstors < 2 then _fail();
      (* remember to declare this type as inductive *)
      _ind_types := (ty, cstors) :: !_ind_types
  | _ -> _fail()

let () =
  Params.add_opts
    [ "-induction", Arg.String _add_ind_type, "enable Induction on the given type"
    ]
