
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Quasipure Literal Elimination} *)
open Logtk
open Libzipperposition

module type S = sig
  module Env : Env.S
  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module CS = C.ClauseSet
  module L = Literal
  module T = Term
  module SAT = Sat_solver.Make ()

  let do_qle cs =
    CCFormat.printf "%a" (CS.pp C.pp) cs

  let setup () =
    Signal.once Env.on_start
      (fun () -> do_qle (CS.of_iter (Env.get_passive ())))
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module QLE = Make(E) in

    QLE.setup ()
  in
  { Extensions.default with Extensions.
                         name = "qle";
                         prio = 50;
                         env_actions = [action];
  }
