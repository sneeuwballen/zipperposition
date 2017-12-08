
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Global CLI options}

    Those options can be used by any program that parses command
    line arguments using the standard module {!Arg}. It may modify some
    global parameters, and return a parameter type for other options.
*)

let stats = ref false

type input_format =
  | I_tptp
  | I_zf
  | I_tip
  | I_dk
  | I_guess

let input_format_of_string s =
  match s |> String.trim |> CCString.lowercase_ascii with
    | "tptp" | "tstp" -> I_tptp
    | "zf" -> I_zf
    | "tip" -> I_tip
    | "dk" -> I_dk
    | s -> failwith ("unknown input format " ^ s)

type print_format = Output_format.t =
  | O_none
  | O_normal
  | O_tptp
  | O_zf

let print_format_of_string s =
  match s |> String.trim |> CCString.lowercase_ascii with
    | "none" -> O_none
    | "tptp" | "tstp" -> O_tptp
    | "default" | "normal" -> O_normal
    | "dk"
    | "zf" -> O_zf
    | _ -> failwith ("unknown print format " ^ s)

let input = ref I_guess
let output = ref O_normal
let set_in s = input := input_format_of_string s
let set_out s = output := print_format_of_string s
let comment() = Output_format.comment_prefix !output

let _print_types () =
  Term.print_all_types := true;
  ()

let switch_opt b f = Arg.Unit (fun () -> f b)
let switch_set b r = Arg.Unit (fun () -> r := b)

let mk_debug_opts () =
  (* debugf level for every section *)
  Util.Section.iter
  |> Sequence.filter_map
    (fun (name,sec) ->
       if name="" then None
       else Some
           ("--debug." ^ name, Arg.Int (Util.Section.set_debug sec),
            " debug level for section " ^ name))
  |> Sequence.to_list
  |> CCList.cons
    ("-d", Arg.Int (Util.Section.set_debug Util.Section.root), " alias to --debug")

(* Options that can be added by plugins *)
let other_opts = ref []

let add_opt o = other_opts := o :: !other_opts
let add_opts l = other_opts := l @ !other_opts

let make () =
  List.rev_append
    [ "--debug", Arg.Int Util.set_debug, " debug level (int)"
    ; "--profile", Arg.Set Util.enable_profiling, " enable profiling"
    ; "--print-types", Arg.Unit _print_types , " print type annotations everywhere"
    ; "--print-hashconsing-id",
      Arg.Set InnerTerm.print_hashconsing_ids,
      " print each term's unique hashconsing ID"
    ; "--backtrace", switch_opt true Printexc.record_backtrace, " enable backtraces"
    ; "-bt", switch_opt true Printexc.record_backtrace, " enable backtraces (alias to --backtrace)"
    ; "--no-backtrace", switch_opt false Printexc.record_backtrace, " disable backtraces"
    ; "--color", switch_opt true CCFormat.set_color_default, " enable colors"
    ; "--no-color", switch_opt false CCFormat.set_color_default, " disable colors"
    ; "-nc", switch_opt false CCFormat.set_color_default, " alias to --no-color"
    ; "--mem-limit", Arg.Int Util.set_memory_limit, " memory limit (in MB)"
    ; "--stats", Arg.Set stats, " gather and print statistics"
    ; "--input", Arg.String set_in, " set input format (zf or tptp)"
    ; "-i", Arg.String set_in, " alias for --input"
    ; "--output" , Arg.String set_out , " choose printing format (zf, tptp, default, none)"
    ; "-o", Arg.String set_out, " alias for --output"
    ; "--break", Arg.Set Util.break_on_debug, " wait for user input after each debug message"
    ; "--show-ty-args", Arg.Set InnerTerm.show_type_arguments, " show type arguments in terms"
    ; "--hide-ty-args", Arg.Clear InnerTerm.show_type_arguments, " hide type arguments in terms"
    ]
    (List.rev_append !other_opts (mk_debug_opts ()))
