
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Clause} *)

open Logtk

type bool_lit = BBox.Lit.t

type t = bool_lit list

let compare = CCOrd.list BBox.Lit.compare

let pp = BBox.pp_bclause

let pp_zf = Util.pp_list ~sep:" || " BBox.pp_zf

let pp_tstp = Util.pp_list ~sep:" | " BBox.pp_tstp

let pp_in = function
  | Output_format.O_zf -> pp_zf
  | Output_format.O_tptp -> pp_tstp
  | Output_format.O_normal -> pp
  | Output_format.O_none -> CCFormat.silent

let to_form ~ctx:_ c = List.map BBox.to_s_form c |> TypedSTerm.Form.or_

exception E_proof of t

let proof_tc =
  Proof.Result.make_tc
    ~of_exn:(function | E_proof c -> Some c | _ -> None)
    ~to_exn:(fun c -> E_proof c)
    ~compare:compare
    ~flavor:(fun _ -> `Pure_bool)
    ~to_form
    ~pp_in
    ()

let mk_proof_res = Proof.Result.make proof_tc

let proof_res_as_bc r =
  let module P = Proof in
  begin match r with
    | P.Res (_, E_proof p) -> Some p
    | _ -> None
  end
