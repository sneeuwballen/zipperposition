
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open OUnit

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
    (  TestSubsts.suite
      :: TestMultiset.suite
      :: []
    )

let () =
  ignore (OUnit.run_test_tt suite);
  QCheck_runner.run_tests_main props
