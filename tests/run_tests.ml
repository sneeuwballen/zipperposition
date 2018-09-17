
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open OUnit2

let props =
  List.flatten
    [ TestOrdinal.props;
      TestTerm.props;
      TestUnif.props;
      TestCNF.props;
      TestCongruence.props;
      TestIndex.props;
      TestType.props;
      TestOrdering.props;
      TestMultiset.props;
    ]

let suite =
  let props = QCheck_runner.to_ounit2_test_list props in
  test_list
    ["unit" >:::
      [TestSubsts.suite;
       TestMultiset.suite;
       TestOrdering.suite;
       TestTerm.suite;
       TestUnif.suite;
      ];
      "qcheck" >::: props;
    ]

let () =
  CCFormat.set_color_default true;
  OUnit2.run_test_tt_main suite
