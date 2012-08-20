(* run all tests *)

let _ =
  TestTerms.run ();
  TestClauses.run ();
  TestOrderings.run ()
