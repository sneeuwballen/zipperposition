
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Check LLProof} *)

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
  n_nocheck: int;
}

let section = LLProof.section
let prof_check = Util.mk_profiler "llproof_check.step"

let pp_res out = function
  | R_ok -> Fmt.fprintf out "@{<Green>ok@}"
  | R_fail -> Fmt.fprintf out "@{<Red>fail@}"

let pp_stats out (s:stats) =
  Fmt.fprintf out "(@[<hv>:ok %d@ :fail %d@ :nocheck %d@])"
    s.n_ok s.n_fail s.n_nocheck

(** {2 Congruence Closure} *)

module CC = Congruence.Make(struct
    include T

    let subterms t = match T.view t with
      | T.App (f, l) -> f :: l
      | T.AppBuiltin (_,l) -> l
      | T.Ite (a,b,c) -> [a;b;c]
      | T.Const _ | T.Var _
        -> []
      | T.Match _
      | T.Bind _
      | T.Let _
      | T.Multiset _
      | T.Record _
      | T.Meta _
        -> assert false (* TODO *)

    let update_subterms t l = match T.view t, l with
      | T.App (_, l1), f :: l1' when List.length l1 = List.length l1' ->
        T.app ~ty:(T.ty_exn t) f l1'
      | T.AppBuiltin (b, l1), l1' when List.length l1 = List.length l1' ->
        T.app_builtin ~ty:(T.ty_exn t) b l1'
      | (T.Const _ | T.Var _), [] -> t
      | T.Ite (_,_,_), [a;b;c] -> T.ite a b c
      | T.App _, _
      | T.AppBuiltin _, _
      | T.Const _, _
      | T.Var _, _
      | T.Ite _, _
      | T.Match _, _
      | T.Bind _, _
      | T.Let _, _
      | T.Multiset _, _
      | T.Record _, _
      | T.Meta _, _ -> assert false
  end)

(** {2 Tableau Prover} *)

(** A simple tableau prover for discharging every proof obligation.
    It does not do instantiation (we assume every inference step is
    preceded by relevant instantiations) but can handle renamings.
*)

module Tab : sig
  val prove : form list -> form list -> res
  (** [prove a b] returns [R_ok] if [a => b] is a tautology. *)
end = struct
  type branch = {
    expansed: form list;
    open_ : form list;
  }

  (** main state *)
  type t = {
    mutable branches: branch list;
    cc: CC.t;
  }

  let prove _a _b =
    let _t = {branches=[]; cc=CC.create(); } in
    assert false (* TODO *)
end

(** {2 Checking Proofs} *)

let check_step_ (p:proof): res option =
  let concl = P.concl p in
  begin match P.step p with
    | P.Goal
    | P.Assert
    | P.By_def _
    | P.Define _
      -> Some R_ok
    | P.Negated_goal p' ->
      (* [p'] should prove [not concl] *)
      Some (Tab.prove [P.concl p'] [F.not_ concl])
    | P.Trivial ->
      (* should be able to prove the conclusion directly *)
      Some (Tab.prove [] [concl])
    | P.Instantiate (_,_) -> None (* TODO *)
    | P.Esa (_,_,_) -> None (* TODO *)
    | P.Inference (_,_, P.C_other) -> Some R_ok
    | P.Inference (_,_, P.C_no_check) -> None
    | P.Inference (_, premises, P.C_check axioms) ->
      let all_premises =
        axioms @ List.map P.concl premises
      in
      Some (Tab.prove all_premises [concl])
  end

let check_step p = Util.with_prof prof_check check_step_ p

let check
    ?(before_check=fun _ -> ())
    ?(on_check=fun _ _ -> ())
    (p:proof) : res * stats
  =
  let tbl = P.Tbl.create 64 in
  let stats = ref {n_ok=0; n_fail=0; n_nocheck=0} in
  let upd_stats f = stats := f !stats in
  let rec check (p:proof): unit =
    if not (P.Tbl.mem tbl p) then (
      before_check p;
      Util.debugf ~section 3 "(@[@{<Yellow>start_checking_proof@}@ %a@])" (fun k->k P.pp p);
      let res = check_step p in
      P.Tbl.add tbl p res;
      Util.debugf ~section 3
        "(@[<hv>@{<Yellow>done_checking_proof@}@ :of %a@ :res %a@])"
        (fun k->k P.pp p (Fmt.Dump.option pp_res) res);
      on_check p res;
      begin match res with
        | Some R_ok -> upd_stats (fun s -> {s with n_ok = s.n_ok+1})
        | Some R_fail -> upd_stats (fun s -> {s with n_fail = s.n_fail+1})
        | None -> upd_stats (fun s -> {s with n_nocheck = s.n_nocheck+1})
      end;
      (* now check premises *)
      List.iter check (P.premises p)
    );
  in
  check p;
  if !stats.n_fail = 0 then R_ok, !stats else R_fail, !stats
