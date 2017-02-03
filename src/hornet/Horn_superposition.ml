
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(* {1 Superposition on Horn Clauses} *)

open Libzipperposition

module T = FOTerm
module C = Clause

let section = Util.Section.make "horn_sup"


module Make : State.THEORY_FUN = functor(Ctx : State.CONTEXT) -> struct
  module Ctx = Ctx
  module B_lit = Ctx.B_lit

  (* index term->clause *)
  module CP_idx = NPDtree.MakeTerm(C.With_pos)

  let name = "horn_superposition"

  (** {2 Unit and Horn Clauses} *)

  (* Horn clauses in the input problem *)
  let horn0 : C.Horn.t list =
    CCVector.to_seq Ctx.statements
    |> Sequence.flat_map Statement.Seq.forms
    |> Sequence.filter_map
      (fun c -> match C.classify c with
         | C.Horn c -> Some c
         | _ -> None)
    |> Sequence.to_rev_list

  (* unit clauses in the input problem *)
  let unit0 : C.Unit.t list =
    CCVector.to_seq Ctx.statements
    |> Sequence.flat_map Statement.Seq.forms
    |> Sequence.filter_map
      (fun c -> match C.classify c with
         | C.Unit_atom c -> Some c
         | _ -> None)
    |> Sequence.to_rev_list

  (* index on the head terms (active paramodulation/resolution) *)
  let idx_heads0 : CP_idx.t =
    assert false (* TODO
    (Sequence.of_list horn0
    |> Sequence.map C.Horn.concl_pos
                 *)

  let idx_heads : CP_idx.t ref = ref idx_heads0

  let on_assumption (_:B_lit.t): unit =
    () (* TODO *)
end

let theory : State.theory_fun = (module Make)

