
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk

module T = FOTerm

type term = FOTerm.t
let section = Util.Section.make ~parent:Const.section "ho"

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C

  (* TODO *)

  let setup () =
    Util.debug ~section 1 "setup HO rules";
    Env.Ctx.lost_completeness();
    ()
end

let k_some_ho : bool Flex_state.key = Flex_state.create_key()

let st_contains_ho (st:(_,_,_) Statement.t): bool =
  Statement.Seq.terms st
  |> Sequence.flat_map T.Seq.vars
  |> Sequence.exists
    (fun v ->
       let n_ty_vars, args, _ = Type.open_poly_fun (HVar.ty v) in
       n_ty_vars > 0 || args<>[])

let extension =
  let register env =
    let module E = (val env : Env.S) in
    if E.flex_get k_some_ho then (
      let module ET = Make(E) in
      ET.setup ()
    )
  (* check if there are HO variables *)
  and check_ho vec state =
    let is_ho =
      CCVector.to_seq vec
      |> Sequence.exists st_contains_ho
    in
    if is_ho then (
      Util.debug ~section 2 "problem is HO"
    );
    Flex_state.add k_some_ho is_ho state
  in
  { Extensions.default with
      Extensions.name = "ho";
      post_cnf_actions=[check_ho];
      env_actions=[register];
  }

let () = Extensions.register extension
