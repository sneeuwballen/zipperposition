let with_setup () f =
  Tracy_client_trace.setup ();
  f ()
