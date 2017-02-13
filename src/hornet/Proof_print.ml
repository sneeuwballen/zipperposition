
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Print Proofs} *)

open Hornet_types

module HC = Horn_clause
module P_res = Proof_res
module U = Hornet_types_util
module Fmt = CCFormat

type t = proof_with_res

(* traverse recursively, with a table to preserve DAG *)
let pp_dag out (p:t): unit =
  let tbl : int P_res.Tbl.t = P_res.Tbl.create 32 in
  let count = ref 0 in
  (* recursive traversal. Returns the unique ID of this step *)
  let rec aux (x:t): int =
    let p, res = x in
    begin match P_res.Tbl.get tbl res with
      | Some id -> id
      | None ->
        let id = CCRef.incr_then_get count in
        P_res.Tbl.add tbl res id;
        (* print parents, get their ID *)
        let l =
          Proof.parents p |> List.map aux
        in
        Format.fprintf out "@[<hv4>@[@{<Green>* [%d]@} %a@]@ :from %a@ :dep %a@]@,"
          id Proof_res.pp res U.pp_proof p Fmt.Dump.(list int) l;
        id
    end
  in
  let aux' _ () = ignore (aux p) in
  Format.fprintf out "@[<v>%a@]@." aux' ()

(* TODO: DOT output *)

let pp_dot _ _ = assert false

let pp_dot_file file p =
  CCIO.with_out file
    (fun oc ->
       let out = Format.formatter_of_out_channel oc in
       pp_dot out p;
       Format.pp_print_flush out ();
       flush oc)
