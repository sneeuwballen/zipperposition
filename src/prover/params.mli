
open Logtk

type t = {
  ord : string;
  seed : int;
  steps : int;
  version : bool;
  timeout : float;
  prelude : (string, CCVector.ro) CCVector.t;
  files : (string, CCVector.ro) CCVector.t;
  select : string; (** name of the selection function *)
  dot_file : string option; (** file to print the final state in *)
  dot_llproof: string option; (** file to print llproof *)
  dot_sat : bool; (** Print saturated set into DOT? *)
  dot_all_roots : bool;
  dot_check: string option; (** prefix for printing checker proofs *)
  def_as_rewrite: bool;
  expand_def : bool; (** expand definitions *)
  stats : bool;
  presaturate : bool; (** initial interreduction of proof state? *)
  unary_depth : int; (** Maximum successive levels of unary inferences *)
  check: bool; (** check proof *)
}

val parse_args : unit -> t

val default : t

val add_opt : (string * Arg.spec * string) -> unit
val add_opts : (string * Arg.spec * string) list -> unit

val key : t Flex_state.key

(**/**)
val select : string ref
(**/**)
