
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
module Ty = Logtk.Type
module Sym = Logtk.Symbol
module Util = Logtk.Util
module Lits = Literals

type term = Logtk.FOTerm.t
type sym = Logtk.Symbol.t

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

let enabled_ = ref false
let enable_ () =
  if not !enabled_ then (
    enabled_ := true;
    Util.debug ~section 1 "Induction: requires ord=rpo6; select=NoSelection";
    Params.ord := "rpo6";   (* new default! RPO is necessary*)
    Params.dot_all_roots := true;  (* print proofs more clearly *)
    Params.select := "NoSelection";
    Signal.send on_enable ();
  )

let declare_ ty cstors =
  (* remember to declare this type as inductive *)
  Util.debug ~section 1 "user declares inductive type %s = %a"
    ty (CCList.pp CCString.pp) cstors;
  ind_types_ := (ty, cstors) :: !ind_types_;
  enable_();
  ()

(* [str] describes an inductive type, under the form "foo:c1|c2|c3" where
    "foo" is the type name and "c1", "c2", "c3" are the type constructors. *)
let add_ind_type_ str =
  let _fail() =
    failwith "expected \"type:c1|c2|c3\" where c1,... are constructors"
  in
  match Util.str_split ~by:":" str with
  | [ty; cstors] ->
      let cstors = Util.str_split ~by:"|" cstors in
      if List.length cstors < 2 then _fail();
      declare_ ty cstors
  | _ -> _fail()

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

let () =
  Params.add_opts
    [ "-induction", Arg.String add_ind_type_,
      " enable Induction on the given type"
    ; "-induction-depth", Arg.Set_int cover_set_depth_,
      " set default induction depth"
    ]
