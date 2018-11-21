
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

module Stm = Stream.Make()

let t_test = (module Stm : Alcotest.TESTABLE with type t = Stm.t)

let check_empty_stm = "empty_stm", `Quick, fun () ->
  ()

let check_drip_stm = ()

let check_default_queue = ()

let check_empty_queue = ()

let check_add_stm_to_queue = ()

let check_take_first_when_available = ()

let check_take_first_anyway = ()
