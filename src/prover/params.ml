
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parameters for the prover, etc.} *)

open Libzipperposition

(* TODO: params to limit depth of preprocessing *)
(* TODO: params to enable/disable some preprocessing *)

type t = {
  param_ord : string;
  param_seed : int;
  param_steps : int;
  param_version : bool;
  param_timeout : float;
  param_files : (string, CCVector.ro) CCVector.t;
  param_select : string; (** name of the selection function *)
  param_dot_file : string option; (** file to print the final state in *)
  param_dot_sat : bool; (** Print saturated set into DOT? *)
  param_dot_all_roots : bool;
  param_expand_def : bool; (** expand definitions *)
  param_stats : bool;
  param_presaturate : bool; (** initial interreduction of proof state? *)
  param_unary_depth : int; (** Maximum successive levels of unary inferences *)
}

(** Options that can be added by plugins *)
let other_opts = ref []

let add_opt o = other_opts := o :: !other_opts
let add_opts l = other_opts := l @ !other_opts

let ord = ref "kbo"
and seed = ref 1928575
and steps = ref 0
and version = ref false
and timeout = ref 0.
and proof = ref Options.Print_normal
and presaturate = ref false
and dot_file = ref None
and dot_sat = ref false
and dot_all_roots = ref false
and expand_def = ref false
and select = ref "SelectComplex"
and unary_depth = ref 1
and files = CCVector.create ()

let set_proof s =
  proof := Options.print_format_of_string s

(** parse_args returns parameters *)
let parse_args () =
  let help_select = CCFormat.sprintf " selection function (@[<h>%a@])"
    (Util.pp_list ~sep:"," CCFormat.string)
    (Selection.available_selections ())
  in
  (* special handlers *)
  let add_file s = CCVector.push files s in
  (* options list *)
  let options = (
    [ "--ord", Arg.Set_string ord, " choose ordering (rpo,kbo)"
    ; "--version", Arg.Set version, " print version"
    ; "--steps", Arg.Set_int steps, " maximal number of steps of given clause loop"
    ; "--timeout", Arg.Set_float timeout, " timeout (in seconds)"
    ; "-t", Arg.Set_float timeout, " short for --timeout"
    ; "--select", Arg.Set_string select, help_select
    ; "--expand-def", Arg.Set expand_def, " expand definitions"
    ; "--proof", Arg.String set_proof, " choose proof printing (none, debug, or tstp)"
    ; "--presaturate", Arg.Set presaturate,
        " pre-saturate (interreduction of) the initial clause set"
    ; "--dot", Arg.String (fun s -> dot_file := Some s) , " print final state to file in DOT"
    ; "--dot-sat", Arg.Set dot_sat, " print saturated set into DOT"
    ; "--dot-all-roots", Arg.Set dot_all_roots, " print all empty clauses into DOT"
    ; "--color", Arg.Bool CCFormat.set_color_default, " enable/disable ANSI color codes"
    ; "--seed", Arg.Set_int seed, " set random seed"
    ; "--unary-depth", Arg.Set_int unary_depth, " maximum depth for successive unary inferences"
    ] @ !other_opts @ Options.make ()
  ) |> List.sort (fun (s1,_,_)(s2,_,_) -> String.compare s1 s2)
    |> Arg.align
  in
  Arg.parse options add_file "solve problems in files";
  if CCVector.is_empty files
    then CCVector.push files "stdin";
  let files = CCVector.freeze files in (* from now on, immutable *)
  (* return parameter structure *)
  { param_ord= !ord; param_seed = !seed; param_steps = !steps;
    param_version= !version; param_timeout = !timeout;
    param_files = files; param_select = !select;
    param_stats= ! Options.stats;
    param_presaturate = !presaturate; param_dot_all_roots= !dot_all_roots;
    param_dot_file = !dot_file;
    param_unary_depth= !unary_depth; param_dot_sat= !dot_sat;
    param_expand_def= !expand_def; }

(* key used to store the parameters in Flex_state *)
let key : t Flex_state.key = Flex_state.create_key()

