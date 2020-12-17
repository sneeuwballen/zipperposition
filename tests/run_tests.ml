
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

let props =
  List.flatten
    [ 
      TestTerm.props;
      TestUnif.props;
      TestCNF.props;
      TestCongruence.props;
      TestIndex.props;
      TestType.props;
      TestOrdering.props;
      TestMultiset.props;
    ]

let props = "qcheck", List.map QCheck_alcotest.to_alcotest props

let units : unit Alcotest.test_case list =
  [TestSubsts.suite;
   TestMultiset.suite;
   TestOrdering.suite;
   TestTerm.suite;
   TestUnif.suite;
   ] |> List.flatten

let units = "units", units

let () =
  CCFormat.set_color_default true;
  Alcotest.run ~and_exit:true "all" [
    units;
    props;
  ]
