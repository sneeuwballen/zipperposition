
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Blocked Clause Elimination} *)

open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()

let section = Util.Section.make ~parent:Const.section "pred-elim"

module type S = sig
  module Env : Env.S

  (** {6 Registration} *)
  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module L = Literal
  module T = Term
  
  
  let setup () = ()
end

let _enabled = ref false

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module PredElim = Make(E) in

    E.flex_add k_enabled !_enabled;
    
    PredElim.setup ()
  in
  { Extensions.default with Extensions.
                         name="bce";
                         env_actions=[action];
  }

let () =
  Options.add_opts [
    "--pred-elim", Arg.Bool ((:=) _enabled), " scan clauses for AC definitions";
  ]