(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Check LLProof — Clause-Based} *)

open Logtk
module T = TypedSTerm
module F = T.Form
module Fmt = CCFormat
module P = LLProof

type proof = LLProof.t
type form = F.t

(** {2 Types} *)

type res =
  | R_ok
  | R_fail

type stats = {
  n_ok: int;
  n_fail: int;
  n_skip_esa: int;
  n_skip_tags: int;
  n_skip_trivial: int;
  n_skip: int;
}

let section = LLProof.section
let stat_check = Util.mk_stat "llproof.check.step"

let pp_res out = function
  | R_ok -> Fmt.fprintf out "@{<Green>OK@}"
  | R_fail -> Fmt.fprintf out "@{<Red>FAIL@}"

let pp_stats out (s : stats) =
  Fmt.fprintf out "(@[<hv>:ok %d@ :fail %d@ :skip %d (:esa %d@ :tags %d)@])"
    s.n_ok s.n_fail s.n_skip s.n_skip_esa s.n_skip_tags

exception Error of string

let error msg = raise (Error msg)
let errorf msg = Fmt.ksprintf ~f:error msg

let () =
  Printexc.register_printer (function
    | Error msg -> Some (Util.err_spf "llproof_check: %s" msg)
    | _ -> None)

(** {2 Applying Instantiation} *)

(** One-shot variable replacement: replace each occurrence of [v] with [t] in
    the term, without recursively substituting inside [t]. *)
let rec replace_vars (subst : (Type.t HVar.t * Term.t) list) (t : Term.t) :
    Term.t =
  if Term.is_ground t then
    t
  else (
    match Term.view t with
    | Term.Var v ->
      (match List.assq_opt v subst with
      | Some t' -> t'
      | None -> t)
    | Term.DB _ -> t
    | Term.Const _ -> t
    | Term.App (hd, args) ->
      let hd' = replace_vars subst hd in
      let args' = List.map (replace_vars subst) args in
      Term.app hd' args'
    | Term.Fun (ty, body) -> Term.fun_ ty (replace_vars subst body)
    | Term.AppBuiltin (b, args) ->
      Term.app_builtin ~ty:(Term.ty t) b (List.map (replace_vars subst) args)
  )

let replace_vars_lit (subst : (Type.t HVar.t * Term.t) list) (lit : Literal.t) :
    Literal.t =
  match Literal.View.as_eqn lit with
  | Some (l, r, sign) ->
    Literal.mk_lit (replace_vars subst l) (replace_vars subst r) sign
  | None -> lit

let apply_inst (inst : LLProof.inst) (clause : LLProof.clause) : LLProof.clause
    =
  if inst = [] then
    clause
  else
    Array.map (replace_vars_lit inst) clause

let concl_of_parent (p : LLProof.parent) : LLProof.clause =
  apply_inst p.LLProof.p_inst (P.concl p.LLProof.p_proof)

(** {2 Conversion to LLTerm (ground)} *)

(** Replace free [Var.t] nodes in [f] with fresh constants. Returns the ground
    formula and the variable-to-constant mapping. *)
let ground_form ~ctx (f : form) : form =
  let vars = T.free_vars f in
  if vars = [] then
    f
  else (
    let subst =
      vars
      |> List.mapi (fun i v ->
             v, T.const ~ty:(Var.ty v) (Name.makef "$$sk_%d" i))
      |> Var.Subst.of_list
    in
    T.Subst.eval subst f
  )

let clause_to_form ~ctx (cl : LLProof.clause) : form =
  Literals.Conv.to_s_form ~allow_free_db:true ~ctx cl

(** {2 Checking Proofs} *)

type check_step_res =
  | CS_check of res
  | CS_skip of [ `ESA | `Other | `Tags | `Trivial ]

let pp_csr out = function
  | CS_check r -> pp_res out r
  | CS_skip r ->
    let s =
      match r with
      | `ESA -> "esa"
      | `Tags -> "tags"
      | `Other -> "other"
      | `Trivial -> "trivial"
    in
    Fmt.fprintf out "@{<Yellow>SKIP@} (%s)" s

let conv_res = function
  | LLProver.R_ok -> R_ok
  | LLProver.R_fail -> R_fail

let n_proof = ref 0

let prove ~dot_prefix (premises : form list) (concl : form) =
  let module TT = LLTerm in
  let ctx = TT.Conv.create () in
  let a = List.map (TT.Conv.of_term ctx) premises in
  let b = TT.Conv.of_term ctx concl in
  let res, final_state = LLProver.prove a b in
  Util.debugf ~section 5 "(@[proof-stats@ %a@])" (fun k ->
      k LLProver.pp_stats final_state);
  (match dot_prefix, res with
  | Some prefix, LLProver.R_fail ->
    let p_id = CCRef.incr_then_get n_proof in
    let file = Printf.sprintf "%s_%d.dot" prefix p_id in
    Util.debugf ~section 2 "print proof %d@ into `%s`" (fun k -> k p_id file);
    CCIO.with_out file (fun oc ->
        let out = Format.formatter_of_out_channel oc in
        Fmt.fprintf out "%a@." LLProver.pp_dot final_state)
  | _ -> ());
  conv_res res

let check_step ?dot_prefix (p : proof) : check_step_res =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "llproof.check-step" in
  let ctx = Term.Conv.create () in
  match P.step p with
  | P.Goal | P.Assert | P.By_def _ | P.Define _ -> CS_check R_ok
  | P.Trivial -> CS_skip `Other
  | P.Esa (_, _) -> CS_skip `ESA
  | P.Inference { parents; tags; name; _ } ->
    if name = "simplify_reflect-" || name = "simplify_reflect+" then
      CS_skip `Other
    else if LLProver.can_check tags then (
      (* Apply instantiations and build forms *)
      let prem_forms =
        List.map
          (fun par ->
            let cl = concl_of_parent par in
            clause_to_form ~ctx cl)
          parents
      in
      let concl_form = clause_to_form ~ctx (P.concl p) in
      (* Ground all forms consistently: use the same Skolem constants
         for variables that appear in multiple clauses (conclusion + premises). *)
      let all_forms = concl_form :: prem_forms in
      let all_vars = T.free_vars_l all_forms in
      let subst =
        all_vars
        |> List.mapi (fun i v ->
               v, T.const ~ty:(Var.ty v) (Name.makef "$$sk_%d" i))
        |> Var.Subst.of_list
      in
      let ground_concl = T.Subst.eval subst concl_form in
      let ground_prems = List.map (T.Subst.eval subst) prem_forms in
      CS_check (prove ~dot_prefix ground_prems ground_concl)
    ) else
      CS_skip `Tags

let check ?dot_prefix ?(before_check = fun _ -> ()) ?(on_check = fun _ _ -> ())
    (p : proof) : res * stats =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "llproof.check" in
  let tbl = P.Tbl.create 64 in
  let stats =
    ref
      {
        n_ok = 0;
        n_fail = 0;
        n_skip_esa = 0;
        n_skip_tags = 0;
        n_skip_trivial = 0;
        n_skip = 0;
      }
  in
  let upd_stats f = stats := f !stats in
  let to_check = Queue.create () in
  Queue.push p to_check;

  while not (Queue.is_empty to_check) do
    let p = Queue.pop to_check in
    if not (P.Tbl.mem tbl p) then (
      let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "llproof.check.step" in
      before_check p;
      Util.debugf ~section 3 "(@[@{<Yellow>start_checking_proof@}@ %a@])"
        (fun k -> k P.pp p);
      let res = check_step ?dot_prefix p in
      P.Tbl.add tbl p res;
      Util.debugf ~section 3
        "(@[<hv>@{<Yellow>done_checking_proof@}@ :of %a@ :res %a@])" (fun k ->
          k P.pp p pp_csr res);
      on_check p res;
      (match res with
      | CS_check R_ok ->
        P.set_check_res p P.R_ok;
        upd_stats (fun s -> { s with n_ok = s.n_ok + 1 })
      | CS_check R_fail ->
        P.set_check_res p P.R_fail;
        upd_stats (fun s -> { s with n_fail = s.n_fail + 1 })
      | CS_skip r ->
        P.set_check_res p P.R_skip;
        upd_stats (fun s ->
            {
              s with
              n_skip = s.n_skip + 1;
              n_skip_esa =
                (if r = `ESA then
                   s.n_skip_esa + 1
                 else
                   s.n_skip_esa);
              n_skip_tags =
                (if r = `Tags then
                   s.n_skip_tags + 1
                 else
                   s.n_skip_tags);
              n_skip_trivial =
                (if r = `Trivial then
                   s.n_skip_trivial + 1
                 else
                   s.n_skip_trivial);
            }));
      List.iter (fun p -> Queue.push p to_check) (P.premises p)
    )
  done;
  if !stats.n_fail = 0 then
    R_ok, !stats
  else
    R_fail, !stats
