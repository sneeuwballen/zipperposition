
open OUnit

let suite = 
  "all_tests" >:::
    [ TestTerm.suite
    ; TestOrderings.suite
    ; TestRewriting.suite
    ]

let _ =
  run_test_tt_main suite
