(* OASIS_START *)
(* OASIS_STOP *)
Ocamlbuild_plugin.dispatch dispatch_default;;

let doc_intro = "doc/api_intro.text" ;;

dispatch
  (function hook ->
      dispatch_default hook ;
      match hook with
        | After_rules ->
            dep ["ocaml"; "doc"; "extension:html"] & [doc_intro] ;
            flag ["ocaml"; "doc"; "extension:html"] &
            (S[A"-t"; A"Logtk Documentation";
               A"-intro"; P doc_intro;
              ]);
        | _ -> ()
  );;
