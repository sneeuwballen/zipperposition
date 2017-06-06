
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Heuristics} *)

open Logtk

module T = Term
module Lit = Literal

let depth_limit_ = ref None
let max_vars = ref 10

let enable_depth_limit i =
  if i <= 0 then invalid_arg "Heuristics.enable_depth_limit";
  depth_limit_ := Some i

let section = Util.Section.make ~parent:Const.section "heuristics"

let stat_depth_limit = Util.mk_stat "trivial.too_deep"
let stat_vars = Util.mk_stat "trivial.too_many_vars"

(** {2 Rules} *)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  val register : unit -> unit
end

module Make(E : Env.S) = struct
  module Env = E
  module PS = Env.ProofState
  module C = Env.C
  module Ctx = Env.Ctx

  let _depth_types lits =
    Literals.Seq.terms lits
    |> Sequence.map T.ty
    |> Sequence.map (fun t -> InnerTerm.depth (t : Type.t :> InnerTerm.t))
    |> Sequence.max ?lt:None
    |> CCOpt.map_or ~default:0 CCFun.id

  let is_too_deep c =
    match !depth_limit_ with
      | None -> false
      | Some d ->
        let lits = C.lits c in
        let depth = max (_depth_types lits) (Literals.depth lits) in
        if depth > d
        then (
          Ctx.lost_completeness();
          Util.incr_stat stat_depth_limit;
          Util.debugf ~section 5 "@[<2>clause dismissed (too deep at %d):@ @[%a@]@]"
            (fun k->k depth C.pp c);
          true
        ) else false

  let has_too_many_vars c =
    let lits = C.lits c in
    (* number of distinct first-order term variables *)
    let n_vars =
      Literals.vars lits
      |> List.filter
        (fun v ->
           let ty = HVar.ty v in
           not (Type.is_tType ty) && not (Type.is_fun ty))
      |> List.length
    in
    if n_vars > !max_vars then (
      Ctx.lost_completeness();
      Util.incr_stat stat_vars;
      Util.debugf ~section 5 "@[<2>clause dismissed (%d vars is too much):@ @[%a@]@]"
        (fun k->k n_vars C.pp c);
      true
    ) else false

  let register () =
    Util.debug ~section 2 "register heuristics...";
    Env.add_is_trivial is_too_deep;
    Env.add_is_trivial has_too_many_vars;
    ()
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module H = Make(E) in
    H.register ()
  in
  Extensions.(
    { default with name="heuristics"; env_actions=[action]; }
  )

let () =
  Params.add_opts
    [ "--depth-limit", Arg.Int enable_depth_limit, " set maximal term depth";
      "--max-vars", Arg.Set_int max_vars, " maximum number of variables per clause";
    ];
  ()

