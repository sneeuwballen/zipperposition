
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open OUnit2

let props =
  List.flatten
  [ TestTerm.props
  ; TestUnif.props
  ; TestCNF.props
  ; TestIndex.props
  ; TestType.props
  ; TestOrdering.props
  ; TestCongruence.props
  ; TestMultiset.props
  ]

let suite =
  "all_tests" >:::
    ( TestSubsts.suite
      :: TestMultiset.suite
      :: List.map QCheck_runner.to_ounit2_test props
    )

let _ =
  OUnit2.run_test_tt_main ~exit suite
