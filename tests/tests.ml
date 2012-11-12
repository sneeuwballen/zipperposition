(* run all tests *)

module Utils = FoUtils

let _ =
  Utils.set_debug 3

let _ =
  TestTerms.run ();
  TestOrderings.run ();
  TestRewriting.run ();
  TestClauses.run ();
  ()
