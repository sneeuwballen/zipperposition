
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Global CLI options}

    Those options can be used by any program that parses command
    line arguments using the standard module {!Arg}. It may modify some
    global parameters, and return a parameter type for other options.
*)

type t = {
  stats : bool;  (** statistics *)
  print_format : [`Debug | `Normal | `TPTP]; (** Printing format *)
} (** Options that can be set by the user *)

val default : t
(** Default options *)

val make : t ref -> (string * Arg.spec * string) list
(** Produce of list of options suitable for {!Arg.parse}, that may
    modify global parameters and the given option reference.
    After parsing, the reference content will reflect CLI options *)

val global : t ref
(** Global parameters, can be used as a mutable default *)

val global_opts : (string * Arg.spec * string) list
(** Options that modify {!global}.
    Caution, this might miss some options from modules that aren't registered yet.
    @deprecated since 0.6.1 , use {!mk_global_opts} instead*)

val mk_global_opts : unit -> (string * Arg.spec * string) list
