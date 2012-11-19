(* run all tests *)

module Utils = FoUtils

let _ =
  Utils.set_debug 3;
  TestRewriting.run ();
  TestTerms.run ();
  TestOrderings.run ();
  TestClauses.run ();
  ()
