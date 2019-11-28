#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

module E = CCResult

let () =
  let res = E.(
    Parsing_utils.parse "examples/pelletier_problems/pb47.p"
    >>= TypeInference.infer_statements ?ctx:None
    >|= CCVector.to_seq
    >|= fun s' ->
    let forms = Iter.flat_map Statement.Seq.forms s' in
    Iter.for_all TT.closed forms
  ) in
  match res with
  | E.Ok true -> ok ()
  | E.Ok false -> fail "test failed"
  | E.Error msg -> fail msg
;;
