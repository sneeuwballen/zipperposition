(* run all tests *)

module Utils = FoUtils

let _ =
  TestClauses.run ();
  TestRewriting.run ();
  TestTerms.run ();
  TestOrderings.run ();
  ()
