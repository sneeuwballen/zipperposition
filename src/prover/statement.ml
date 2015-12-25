
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Statement} *)

open Libzipperposition
open Libzipperposition_parsers

type clause = FOTerm.t SLiteral.t list

type view =
  | TyDecl of ID.t * Type.t
  | Assert of clause

type t = {
  view: view;
  src: StatementSrc.t;
}

let view t = t.view
let src t = t.src

let ty_decl ~src id ty = {src; view=TyDecl (id,ty); }
let assert_ ~src c = {src; view=Assert c; }

let role_is_conj_ = function
  | Ast_tptp.R_conjecture
  | Ast_tptp.R_negated_conjecture -> true
  | _ -> false

(* conversion from a TPTP AST *)
let of_cnf_tptp ~file st =
  let mk_src (role, name) =
    let is_conjecture = role_is_conj_ role in
    StatementSrc.make ~is_conjecture ~name file
  in
  match st with
  | Cnf.Assert (c, data) ->
      let c = Cnf.clause_to_fo c in
      assert_ ~src:(mk_src data) c
  | Cnf.TyDecl (id, ty, data) ->
      let ctx = Type.Conv.create() in
      let ty = Type.Conv.of_simple_term_exn ctx ty in
      ty_decl ~src:(mk_src data) id ty

let signature ?(init=Signature.empty) seq =
  Sequence.fold
    (fun sigma st -> match view st with
      | TyDecl (id, ty) -> Signature.declare sigma id ty
      | Assert _ -> sigma)
    init seq

(** {2 Iterators} *)

module Seq = struct
  let ty_decls st k = match view st with
    | TyDecl (id, ty) -> k (id,ty)
    | Assert _ -> ()

  let clauses st k = match view st with
    | TyDecl _ -> ()
    | Assert c -> k c

  let lits st = clauses st |> Sequence.flat_map Sequence.of_list

  let terms st = lits st |> Sequence.flat_map SLiteral.to_seq
end

(** {2 IO} *)

let fpf = Format.fprintf

let pp out st = match st.view with
  | TyDecl (id,ty) ->
      fpf out "@[<2>val %a :@ %a@]." ID.pp id Type.pp ty
  | Assert c ->
      fpf out "@[<2>assert@ (%a)@]."
        (Util.pp_list ~sep:" ∨ " (SLiteral.pp FOTerm.pp)) c

let to_string = CCFormat.to_string pp

module TPTP = struct
  let pp _ _ = assert false (* TODO *)

  let to_string = CCFormat.to_string pp
end

