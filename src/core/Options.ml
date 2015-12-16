
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Global CLI options}

    Those options can be used by any program that parses command
    line arguments using the standard module {!Arg}. It may modify some
    global parameters, and return a parameter type for other options.
*)


type t = {
  stats : bool;  (** statistics *)
  print_format : [`Debug | `Normal | `TPTP]; (** Printing format *)
}

let default = {
  stats = false;
  print_format = `Normal;
}

let print_format_of_string s =
  match String.lowercase (String.trim s) with
  | "tptp" | "tstp" -> `TPTP
  | "default" | "normal" -> `Normal
  | "debug" -> `Debug
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
            " debug level for section " ^ name
           )
    )
  |> Sequence.to_list

let make opts =
  let mod_opt f = opts := f !opts in
  let l =
    [ "--debug", Arg.Int Util.set_debug, " logtk: debug level"
    ; "--profile", Arg.Set Util.enable_profiling, " logtk: enable profiling"
    ; "--print-types", Arg.Unit _print_types , " print type annotations everywhere"
    ; "--bt", Arg.Bool Printexc.record_backtrace, " enable backtraces"
    ; "--mem-limit", Arg.Int Util.set_memory_limit, " memory limit (in MB)"
    ; "--stats", Arg.Unit (fun () -> mod_opt (fun o -> {o with stats=true;})),
      " gather and print statistics"
    ; "--print",
      Arg.String
        (fun s ->
           mod_opt (fun o -> {o with print_format=print_format_of_string s;} )),
      " choose printing format for terms and formulas (default \"debugf\")"
    ] @ make_other_opts ()
  in
  List.sort (fun (x,_,_)(y,_,_) -> String.compare x y) l

let global = ref default

let global_opts = make global

let mk_global_opts () = make global
