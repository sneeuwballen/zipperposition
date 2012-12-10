(* run all tests *)

module Utils = FoUtils

let _ =
  TestPartialOrder.run ();
  TestClauses.run ();
  TestRewriting.run ();
  TestTerms.run ();
  TestOrderings.run ();
  ()
