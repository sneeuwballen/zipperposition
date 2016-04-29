
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

let flag_debug = OUnit2.Conf.make_int "debug" 0 " set debug level"

let test_init ctxt =
  Libzipperposition.Util.set_debug (flag_debug ctxt)

let suite =
  "all_tests" >:::
    ( ("init" >:: test_init)
      :: TestSubsts.suite
      :: TestMultiset.suite
      :: List.map QCheck_runner.to_ounit2_test props
    )

let () =
  OUnit2.run_test_tt_main ~exit suite
