
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Global CLI options}

    Those options can be used by any program that parses command
    line arguments using the standard module {!Arg}. It may modify some
    global parameters, and return a parameter type for other options.
*)

let stats = ref false

type print_format =
  | Print_normal
  | Print_tptp

let output = ref Print_normal

let print_format_of_string s =
  match s |> String.trim |> String.lowercase with
  | "tptp" | "tstp" -> Print_tptp
  | "default" | "normal" -> Print_normal
  | _ -> failwith ("unknown print format " ^ s)

let _print_types () =
  HOTerm.print_all_types := true;
  FOTerm.print_all_types := true;
  ()

let make_other_opts () =
  (* debugf level for every section *)
  Util.Section.iter
  |> Sequence.filter_map
    (fun (name,sec) ->
       if name="" then None
       else Some
           ("--debug." ^ name, Arg.Int (Util.Section.set_debug sec),
            " debug level for section " ^ name))
  |> Sequence.to_list

let make () =
  List.rev_append
    [ "--debug", Arg.Int Util.set_debug, " debug level (int)"
    ; "--profile", Arg.Set Util.enable_profiling, " enable profiling"
    ; "--print-types", Arg.Unit _print_types , " print type annotations everywhere"
    ; "--bt", Arg.Bool Printexc.record_backtrace, " enable backtraces"
    ; "--mem-limit", Arg.Int Util.set_memory_limit, " memory limit (in MB)"
    ; "--stats", Arg.Set stats, " gather and print statistics"
    ; "--output"
      , Arg.String
        (fun s -> output := print_format_of_string s; )
      , " choose printing format for terms and formulas (default \"default\")"
    ]
    (make_other_opts ())
