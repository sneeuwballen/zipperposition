(* run all tests *)

module Utils = FoUtils

let _ =
  TestTerms.run ();
  TestOrderings.run ();
  TestRewriting.run ();
  TestClauses.run ();
  TestPartialOrder.run ();
  ()
