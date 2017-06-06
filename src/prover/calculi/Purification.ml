
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Purification} *)

(** See {!Purify} *)

open Logtk

module type S = sig
  module Env : Env.S

  val setup : unit -> unit
  (** Register rules in the environment *)
end

let section = Util.Section.make "purification"

let stat_purify_calls = Util.mk_stat "purify.calls"
let stat_purify_steps = Util.mk_stat "purify.steps"
let prof_purify = Util.mk_profiler "purify"

(* flag to be used to know when a clause cannot be purified *)
let flag_no_purify = SClause.new_flag ()

module Make(E : Env.S) = struct
  module Env = E
  module C = Env.C

  let purify (c:C.t): C.t SimplM.t =
    Util.incr_stat stat_purify_calls;
    if C.get_flag flag_no_purify c
    then SimplM.return_same c
    else begin match Purify.purify (C.lits c) with
      | None -> SimplM.return_same c (* no chang *)
      | Some new_lits ->
        let proof =
          Proof.Step.inference ~rule:(Proof.Rule.mk "purify") [C.proof_parent c] in
        let new_c =
          C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof
        in
        Util.debugf ~section 5 "(@[<2>purify@ :old @[%a@]@ :into @[%a@]@])"
          (fun k->k C.pp c C.pp new_c);
        Util.incr_stat stat_purify_steps;
        SimplM.return_new new_c
    end

  let setup () =
    Util.debug ~section 1 "setup purification";
    (* force rules *)
    Env.add_unary_simplify purify;
    ()
end

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    ET.setup ()
  in
  { Extensions.default with
      Extensions.name = "purification";
      env_actions=[register];
  }

let () = Extensions.register extension
