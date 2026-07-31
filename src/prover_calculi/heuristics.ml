(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Heuristics} *)

open Logtk
open Libzipperposition
module T = Term
module Lit = Literal

let k_depth_limit : int option Flex_state.key = Flex_state.create_key ()
let k_max_vars : int Flex_state.key = Flex_state.create_key ()
let k_no_max_vars : bool Flex_state.key = Flex_state.create_key ()

let section = Util.Section.make ~parent:Const.section "heuristics"
let stat_depth_limit = Util.mk_stat "trivial.too_deep"
let stat_vars = Util.mk_stat "trivial.too_many_vars"

(** {2 Rules} *)

module C = Clause
module Ctx = Ctx

let _depth_types lits =
  Literals.Seq.terms lits |> Iter.map T.ty
  |> Iter.map (fun t -> InnerTerm.depth (t : Type.t :> InnerTerm.t))
  |> Iter.max ?lt:None
  |> CCOpt.map_or ~default:0 CCFun.id

let is_too_deep env c =
  match Option.join (Flex_state.get k_depth_limit (Env.flex_state_of env)) with
  | None -> false
  | Some d ->
    let lits = C.lits c in
    let depth = max (_depth_types lits) (Literals.depth lits) in
    if depth > d then (
      Ctx.lost_completeness (Env.get_ctx env);
      Util.incr_stat stat_depth_limit;
      Util.debugf ~section 5 "@[<2>clause dismissed (too deep at %d):@ @[%a@]@]"
        (fun k -> k depth C.pp c);
      true
    ) else
      false

let has_too_many_vars env c =
  if
    CCOpt.get_or ~default:false
      (Flex_state.get k_no_max_vars (Env.flex_state_of env))
  then
    false
  else (
    let lits = C.lits c in
    (* number of distinct term variables *)
    let n_vars =
      Literals.vars lits
      |> List.filter (fun v -> not (Type.is_tType (HVar.ty v)))
      |> List.length
    in
    if
      n_vars
      > CCOpt.get_or ~default:10 (Flex_state.get k_max_vars (Env.flex_state_of env))
    then (
      Ctx.lost_completeness (Env.get_ctx env);
      Util.incr_stat stat_vars;
      Util.debugf ~section 5
        "@[<2>clause dismissed (%d vars is too much):@ @[%a@]@]" (fun k ->
          k n_vars C.pp c);
      true
    ) else
      false
  )

let register env =
  Util.debug ~section 2 "register heuristics...";
  Env.add_is_trivial env is_too_deep;
  Env.add_is_trivial env has_too_many_vars;
  ()

let extension =
  let action (env : Env.t) = register env in
  Extensions.{ default with name = "heuristics"; env_actions = [ action ] }

let () =
  Params.add_flex_opts (fun ~flex_ref ->
    [
      ( "--depth-limit",
        Arg.Int
          (fun i -> flex_ref := Flex_state.add k_depth_limit (Some i) !flex_ref),
        " set maximal term depth" );
      ( "--max-vars",
        Arg.Int (fun n -> flex_ref := Flex_state.add k_max_vars n !flex_ref),
        " maximum number of variables per clause" );
      ( "--no-max-vars",
        Arg.Bool
          (fun v -> flex_ref := Flex_state.add k_no_max_vars v !flex_ref),
        " disable/enable maximum number of variables per clause" );
    ]);
  Params.add_to_mode "best" (fun flex_ref ->
    flex_ref := Flex_state.add k_no_max_vars true !flex_ref);
  Params.add_to_mode "ho-pragmatic" (fun flex_ref ->
    flex_ref := Flex_state.add k_no_max_vars true !flex_ref);
  Params.add_to_mode "ho-competitive" (fun flex_ref ->
    flex_ref := Flex_state.add k_no_max_vars true !flex_ref);
  Params.add_to_modes
    [
      "ho-complete-basic";
      "fo-complete-basic";
      "lambda-free-intensional";
      "lambda-free-extensional";
      "ho-comb-complete";
      "lambda-free-purify-intensional";
      "lambda-free-purify-extensional";
    ] (fun flex_ref -> flex_ref := Flex_state.add k_no_max_vars true !flex_ref);
  ()
