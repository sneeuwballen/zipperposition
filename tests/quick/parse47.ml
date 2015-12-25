#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

module E = CCError

let () =
  let res = E.(
    Util_tptp.parse_file ~recursive:true "pelletier_problems/pb47.p"
    >>= fun s ->
    Util_tptp.infer_types (`sign Signature.empty) s
    >>= fun (signature, s') ->
    let forms = Util_tptp.Typed.formulas s' in
    E.return (Sequence.for_all F.FO.is_closed forms)
  ) in
  match res with
  | `Ok true -> ok ()
  | `Ok false -> print_endline "assertion failure, ERROR"
  | `Error msg -> print_endline msg
;;
