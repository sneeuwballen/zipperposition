(* run all tests *)

let _ =
  TestTerms.run ();
  TestEUnif.run ();
  TestClauses.run ();
  TestOrderings.run ();
  ()
