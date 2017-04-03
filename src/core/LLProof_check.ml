
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Check LLProof} *)

module T = TypedSTerm
module F = T.Form
module Ty = T.Ty
module Fmt = CCFormat
module P = LLProof

type proof = LLProof.t

type res =
  | R_ok
  | R_fail

type stats = {
  n_ok: int;
  n_fail: int;
  n_nocheck: int;
}

let section = Util.Section.make "llproof_check"

let pp_res out = function
  | R_ok -> Fmt.fprintf out "@{<green>ok@}"
  | R_fail -> Fmt.fprintf out "@{<red>fail@}"

let pp_stats out (s:stats) =
  Fmt.fprintf out "(@[:ok %d@ :fail %d@ :nocheck %d@])"
    s.n_ok s.n_fail s.n_nocheck

let check_step (_p:proof): res option = None (* TODO: tableau prover, etc. *)

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
      Util.debugf ~section 3 "(@[start check_proof@ %a@])" (fun k->k P.pp p);
      let res = check_step p in
      P.Tbl.add tbl p res;
      Util.debugf ~section 3 "(@[check_proof_res@ :of %a@ :res %a@])"
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
