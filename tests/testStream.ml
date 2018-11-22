
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test stream data structure *)

open Logtk

let a_ = ID.make "a"

module Params = struct
  let signature = Signature.singleton a_ Type.term
  let ord = (Ordering.default_of_prec (Precedence.default [a_]))
  let select = Selection.default ord
end

module MyCtx = Ctx.Make(Params)

module MyClause = Clause.Make(MyCtx)

module Stm = Stream.Make(MyClause)

let t_test = (module Stm : Alcotest.TESTABLE with type t = Stm.t)

let check_empty_stm = "empty_stm", `Quick, fun () ->
  let st1 : Stm.t = {id=0;penalty=1; stm = Oseq.empty} in
  let st2 = Stm.make(OSeq.empty) in
  Alcotest.(check t_test) "empty_stm" st1 st2;
  ()

let check_drip_stm = ()

let check_default_queue = ()

let check_empty_queue = ()

let check_add_stm_to_queue = ()

let check_take_first_when_available = ()

let check_take_first_anyway = ()

let suite : unit Alcotest.test_case list =
  [  (*check_empty_stm;*)
  ]
