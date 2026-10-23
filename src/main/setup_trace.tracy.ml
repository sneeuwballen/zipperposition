let with_setup () f =
  match Sys.getenv_opt "TRACE" with
  | None -> f ()
  | Some _ ->
    Tracy_client_trace.setup ();
    f ()
