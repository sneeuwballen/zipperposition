
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Check LLProof} *)

open Logtk

module T = TypedSTerm
module F = T.Form
module Ty = T.Ty
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
  n_skip_esa: int; (** steps skipped because ESA *)
  n_skip_tags: int; (** steps skipped because of theory tags *)
  n_skip_trivial: int; (** steps skipped because they are trivial *)
  n_skip: int;
}

let section = LLProof.section
let prof_check = Util.mk_profiler "llproof.check.step"
let stat_check = Util.mk_stat "llproof.check.step"

let pp_res out = function
  | R_ok -> Fmt.fprintf out "@{<Green>OK@}"
  | R_fail -> Fmt.fprintf out "@{<Red>FAIL@}"

let pp_stats out (s:stats) =
  Fmt.fprintf out "(@[<hv>:ok %d@ :fail %d@ :skip %d (:esa %d@ :tags %d)@])"
    s.n_ok s.n_fail s.n_skip s.n_skip_esa s.n_skip_tags

exception Error of string

let error msg = raise (Error msg)
let errorf msg = Fmt.ksprintf ~f:error msg

let () = Printexc.register_printer
    (function
      | Error msg -> Some (Util.err_spf "llproof_check: %s" msg)
      | _ -> None)

(** {2 Checking Proofs} *)

let instantiate (f:form) (inst:LLProof.inst) : form =
  let f = T.rename_all_vars f in
  let vars, body = T.unfold_binder Binder.Forall f in
  if List.length vars <> List.length inst then (
    errorf "mismatched arities in instantiate `%a`@ :with %a"
      T.pp f LLProof.pp_inst inst
  );
  let subst =
    List.fold_left2 Var.Subst.add Var.Subst.empty vars inst
  in
  let f' = T.Subst.eval subst body in
  Util.debugf ~section 5
    "(@[<hv>instantiate@ :inst %a@ :from %a@ :into %a@ :subst {%a}@])"
    (fun k->k LLProof.pp_inst inst T.pp f T.pp f' (Var.Subst.pp T.pp_with_ty) subst);
  f'

let concl_of_parent (p:LLProof.parent) : form = match p.LLProof.p_inst with
  | [] -> P.concl p.LLProof.p_proof
  | r ->
    let f = P.concl p.LLProof.p_proof in
    instantiate f r

let open_forall = T.unfold_binder Binder.Forall

type check_step_res =
  | CS_check of res
  | CS_skip of [`ESA | `Other | `Tags | `Trivial]

let pp_csr out = function
  | CS_check r -> pp_res out r
  | CS_skip r ->
    let s = match r with
      | `ESA -> "esa" | `Tags -> "tags"
      | `Other -> "other" | `Trivial -> "trivial" in
    Fmt.fprintf out "@{<Yellow>SKIP@} (%s)" s

let conv_res = function
  | LLProver.R_ok -> R_ok
  | LLProver.R_fail -> R_fail

let n_proof = ref 0 (* proof counter *)

let prove ~dot_prefix (a:form list) (b:form) =
  let module TT = LLTerm in
  (* convert into {!LLTerm.t} *)
  let ctx = TT.Conv.create() in
  let a = List.map (TT.Conv.of_term ctx) a in
  let b = TT.Conv.of_term ctx b in
  (* prove [a ∧ -b ⇒ ⊥] *)
  let res, final_state = LLProver.prove a b in
  Util.debugf ~section 5 "(@[proof-stats@ %a@])"(fun k->k LLProver.pp_stats final_state);
  (* print state, maybe *)
  begin match dot_prefix with
    | None -> ()
    | Some prefix ->
      let p_id = CCRef.incr_then_get n_proof in
      let file = Printf.sprintf "%s_%d.dot" prefix p_id in
      Util.debugf ~section 2 "print proof %d@ into `%s`" (fun k->k p_id file);
      CCIO.with_out file
        (fun oc ->
           let out = Format.formatter_of_out_channel oc in
           Fmt.fprintf out "%a@." LLProver.pp_dot final_state);
  end;
  conv_res res

let check_step_ ?dot_prefix (p:proof): check_step_res =
  let concl = P.concl p in
  Util.incr_stat stat_check;
  begin match P.step p with
    | P.Goal
    | P.Assert
    | P.By_def _
    | P.Define _
      -> CS_check R_ok
    | P.Negated_goal p' ->
      (* [p'] should prove [not concl] *)
      CS_check (prove ~dot_prefix [P.concl p'] (F.not_ concl))
    | P.Trivial -> CS_skip `Other (* axiom of the theory *)
    | P.Instantiate {tags;_} when not (LLProver.can_check tags) -> CS_skip `Tags
    | P.Instantiate {form=p';inst;_} ->
      (* re-instantiate and check we get the same thing *)
      let p'_inst = instantiate (LLProof.concl p') inst in
      let vars, body_concl = open_forall concl in
      (* now remove free variables by using fresh constants *)
      let subst =
        vars
        |> List.mapi (fun i v -> v, T.const ~ty:(Var.ty v) (ID.makef "sk_%d" i))
        |> Var.Subst.of_list
      in
      CS_check (prove ~dot_prefix [T.Subst.eval subst p'_inst] (T.Subst.eval subst body_concl))
    | P.Esa (_,_) -> CS_skip `ESA (* TODO *)
    | P.Inference {parents;tags;intros;_} ->
      if LLProver.can_check tags then (
        (* within the fragment of {!Tab.prove} *)
        let all_premises =
          List.map concl_of_parent parents
        and concl =
          instantiate concl intros
        in
        CS_check (prove ~dot_prefix all_premises concl)
      ) else CS_skip `Tags
  end

let check_step ?dot_prefix p = Util.with_prof prof_check (check_step_ ?dot_prefix) p

let check
    ?dot_prefix
    ?(before_check=fun _ -> ())
    ?(on_check=fun _ _ -> ())
    (p:proof) : res * stats
  =
  let tbl = P.Tbl.create 64 in
  let stats = ref {
      n_ok=0; n_fail=0; n_skip_esa=0; n_skip_tags=0;
      n_skip_trivial=0; n_skip=0;
    } in
  let upd_stats f = stats := f !stats in
  let rec check (p:proof): unit =
    if not (P.Tbl.mem tbl p) then (
      before_check p;
      Util.debugf ~section 3 "(@[@{<Yellow>start_checking_proof@}@ %a@])"
        (fun k->k P.pp p);
      let res = check_step ?dot_prefix p in
      P.Tbl.add tbl p res;
      Util.debugf ~section 3
        "(@[<hv>@{<Yellow>done_checking_proof@}@ :of %a@ :res %a@])"
        (fun k->k P.pp p pp_csr res);
      on_check p res;
      begin match res with
        | CS_check R_ok ->
          P.set_check_res p P.R_ok;
          upd_stats (fun s -> {s with n_ok = s.n_ok+1})
        | CS_check R_fail ->
          P.set_check_res p P.R_fail;
          upd_stats (fun s -> {s with n_fail = s.n_fail+1})
        | CS_skip r ->
          P.set_check_res p P.R_skip;
          upd_stats
            (fun s ->
               {s with
                  n_skip = s.n_skip+1;
                  n_skip_esa=if r=`ESA then s.n_skip_esa+1 else s.n_skip_esa;
                  n_skip_tags=if r=`Tags then s.n_skip_tags+1 else s.n_skip_tags;
                  n_skip_trivial=if r=`Trivial then s.n_skip_trivial+1 else s.n_skip_trivial;
               })
      end;
      (* now check premises *)
      List.iter check (P.premises p)
    );
  in
  check p;
  if !stats.n_fail = 0 then R_ok, !stats else R_fail, !stats

