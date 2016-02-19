#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

module E = CCError

let () =
  let res = E.(
    Parsing_utils.parse "examples/pelletier_problems/pb47.p"
    >>= TypeInference.infer_statements ?ctx:None
    >|= CCVector.to_seq
    >|= fun s' ->
    let forms = Sequence.flat_map Statement.Seq.forms s' in
    Sequence.for_all TT.closed forms
  ) in
  match res with
  | `Ok true -> ok ()
  | `Ok false -> fail "test failed"
  | `Error msg -> fail msg
;;
