
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition
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
    ( TestSubsts.suite
      :: TestMultiset.suite
      :: List.map QCheck_runner.to_ounit_test props
    )

let specs = Arg.align (Options.make ())

let _ =
  let res = run_test_tt_main ~arg_specs:specs suite in
  if List.exists (function (RFailure _) -> true | _ -> false) res
  then exit 1;
  ()
