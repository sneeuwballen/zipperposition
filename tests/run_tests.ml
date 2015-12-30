
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition
open OUnit

let suite =
  "all_tests" >:::
    [ TestTerm.suite
    ; TestSubsts.suite
    ; TestOrdering.suite
    ; TestMultiset.suite
    ]

let props = QCheck.flatten
  [ TestTerm.props
  ; TestUnif.props
  ; TestCNF.props
  ; TestIndex.props
  ; TestType.props
  ; TestOrdering.props
  ; TestCongruence.props
  ; TestMultiset.props
  ]

let specs = Arg.align (Options.make ())

let _ =
  ignore (run_test_tt_main ~arg_specs:specs suite);
  ignore (QCheck.run_tests props);
  ()
