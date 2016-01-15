
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Global CLI options}

    Those options can be used by any program that parses command
    line arguments using the standard module {!Arg}. It may modify some
    global parameters, and return a parameter type for other options.
*)

val stats : bool ref
(** Enable printing of statistics? *)

type print_format =
  | Print_none
  | Print_normal
  | Print_tptp

val print_format_of_string : string -> print_format
(** @raise Failure if it could not parse *)

val output : print_format ref
(** Output format *)

val make : unit -> (string * Arg.spec * string) list
(** Produce of list of options suitable for {!Arg.parse}, that may
    modify global parameters and the given option reference. *)

