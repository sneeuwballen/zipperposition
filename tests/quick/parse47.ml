#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

module E = CCError

let () =
  let res = E.(
    Util_tptp.parse_file ~recursive:true "examples/pelletier_problems/pb47.p"
    >>= fun s ->
    Util_tptp.infer_types s
    >>= fun s' ->
    let forms = Util_tptp.formulas s' in
    E.return (Sequence.for_all TT.closed forms)
  ) in
  match res with
  | `Ok true -> ok ()
  | `Ok false -> fail "test failed"
  | `Error msg -> fail msg
;;
