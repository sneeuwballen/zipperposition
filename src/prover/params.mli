open Logtk

type t = {
  ord: string ref;
  seed: int;
  steps: int;
  version: bool;
  timeout: float;
  prelude: (string, CCVector.ro) CCVector.t;
  files: (string, CCVector.ro) CCVector.t;
  select: string;  (** name of the selection function *)
  bool_select: string;  (** name of the boolean selection function *)
  dot_file: string option;  (** file to print the final state in *)
  dot_llproof: string option;  (** file to print llproof *)
  dot_sat: bool;  (** Print saturated set into DOT? *)
  dot_all_roots: bool;
  dot_check: string option;  (** prefix for printing checker proofs *)
  proof_trace: string option;  (** file to write proof trace in mdag format *)
  def_as_rewrite: bool;
  expand_def: bool;  (** expand definitions *)
  stats: bool;
  presaturate: bool;  (** initial interreduction of proof state? *)
  unary_depth: int;  (** Maximum successive levels of unary inferences *)
  check: bool;  (** check proof *)
  e_path: string option;  (** path to E binary *)
  progress: bool;  (** progress bar *)
  check_types: bool;  (** check types in new clauses *)
  max_multi_simpl: int;  (** max multi-simplification depth. -1 = unlimited *)
}

val parse_args : unit -> t
val default : t
val add_opt : string * Arg.spec * string -> unit
val add_opts : (string * Arg.spec * string) list -> unit
val add_to_mode : string -> (unit -> unit) -> unit
val add_to_modes : string list -> (unit -> unit) -> unit
val key : t Flex_state.key

(**/**)

module Cli : sig
  val set_select : string -> unit
  val set_bool_select : string -> unit
  val set_e_path : string option -> unit
  val set_progress : bool -> unit
  val set_check_types : bool -> unit
  val set_max_multi_simpl : int -> unit
end

(**/**)
