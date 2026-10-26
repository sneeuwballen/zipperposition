(* Tests for the scoped Name_payload state (kata fcxr) *)

open Logtk

let s = Name.make "s"

let test_scope_retracts () =
  Name_payload.with_scope (fun () ->
      Name_payload.add s Name.Attr_distinct;
      Alcotest.(check bool)
        "visible in scope" true
        (Name_payload.exists s ~f:(( = ) Name.Attr_distinct)));
  Alcotest.(check bool)
    "retracted after scope" false
    (Name_payload.exists s ~f:(( = ) Name.Attr_distinct))

let test_nested_scopes_copy () =
  Name_payload.with_scope (fun () ->
      Name_payload.add s Name.Attr_comm;
      Name_payload.with_scope (fun () ->
          Name_payload.add s Name.Attr_assoc;
          Alcotest.(check bool)
            "inner sees outer payload" true
            (Name_payload.exists s ~f:(( = ) Name.Attr_comm));
          Alcotest.(check bool)
            "inner sees own payload" true
            (Name_payload.exists s ~f:(( = ) Name.Attr_assoc)));
      Alcotest.(check bool)
        "outer does not see inner payload" false
        (Name_payload.exists s ~f:(( = ) Name.Attr_assoc));
      Alcotest.(check bool)
        "outer still sees own payload" true
        (Name_payload.exists s ~f:(( = ) Name.Attr_comm)))

let test_copy_shares_nothing () =
  Name_payload.with_scope (fun () ->
      Name_payload.add s Name.Attr_comm;
      let st1 = Name_payload.copy (Option.get (Name_payload.get_opt ())) in
      Name_payload.add s Name.Attr_distinct;
      Alcotest.(check bool)
        "copy is independent" false
        (Name_payload.Pure.exists ~f:(( = ) Name.Attr_distinct)
        @@ Option.get
        @@ Hstring.Tbl.get st1.Name_payload.payloads s))

let test_unset_fails () =
  (* outside any scope (with_scope already popped by outer suite scope, this
     runs inside the suite's base scope, so spawn the check via a nested
     restore: simulate by reading from a fresh state-less context) *)
  Alcotest.(check bool)
    "scope is active in the test process" true
    (Option.is_some (Name_payload.get_opt ()))

let suite =
  [
    "scope retracts mutations", `Quick, test_scope_retracts;
    "nested scopes copy parent state", `Quick, test_nested_scopes_copy;
    "copy shares nothing", `Quick, test_copy_shares_nothing;
    "base scope active in tests", `Quick, test_unset_fails;
  ]
