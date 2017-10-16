
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Global CLI options} *)

(** Those options can be used by any program that parses command
    line arguments using the standard module {!Arg}. It may modify some
    global parameters, and return a parameter type for other options.
*)

val stats : bool ref
(** Enable printing of statistics? *)

type input_format =
  | I_tptp
  | I_zf
  | I_tip
  | I_dk
  | I_guess (* based on the file extension *)

val input_format_of_string : string -> input_format

type print_format = Output_format.t =
  | O_none
  | O_normal
  | O_tptp
  | O_zf

val print_format_of_string : string -> print_format
(** @raise Failure if it could not parse *)

val input : input_format ref

val output : print_format ref
(** Output format *)

val comment : unit -> string
(** Current comment format *)

val switch_opt : 'a -> ('a -> unit) -> Arg.spec
(** [switch_opt b f] is an option that, when parsed, will call [f b]. Useful
    for
    {[ [ ("--foo", switch_opt true set_foo, " enable foo"
         ; ("--no-foo", switch_opt false set_foo, " disable foo"]
    ]} *)

val switch_set : 'a -> 'a ref -> Arg.spec
(** [switch_set x r] is the option that, enabled, calls [r := x] *)

val add_opt : (string * Arg.spec * string) -> unit
(** Add a new command line option *)

val add_opts : (string * Arg.spec * string) list -> unit
(** Add new command line options *)

val make : unit -> (string * Arg.spec * string) list
(** Produce of list of options suitable for {!Arg.parse}, that may
    modify global parameters and the given option reference. *)

