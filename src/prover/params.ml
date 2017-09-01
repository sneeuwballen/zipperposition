
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parameters for the prover, etc.} *)

open Logtk

(* TODO: params to limit depth of preprocessing *)
(* TODO: params to enable/disable some preprocessing *)

type t = {
  param_ord : string;
  param_seed : int;
  param_steps : int;
  param_version : bool;
  param_timeout : float;
  param_prelude : (string, CCVector.ro) CCVector.t;
  param_files : (string, CCVector.ro) CCVector.t;
  param_select : string; (** name of the selection function *)
  param_dot_file : string option; (** file to print the final state in *)
  param_dot_sat : bool; (** Print saturated set into DOT? *)
  param_dot_all_roots : bool;
  param_def_as_rewrite: bool;
  param_expand_def : bool; (** expand definitions *)
  param_stats : bool;
  param_presaturate : bool; (** initial interreduction of proof state? *)
  param_unary_depth : int; (** Maximum successive levels of unary inferences *)
  param_check: bool; (** check proof *)
}

let ord = ref "kbo"
and seed = ref 1928575
and steps = ref ~-1
and version = ref false
and timeout = ref 0.
and presaturate = ref false
and dot_file = ref None
and dot_sat = ref false
and dot_all_roots = ref false
and expand_def = ref false
and select = ref "default"
and unary_depth = ref 1
and def_as_rewrite = ref true
and prelude = CCVector.create()
and files = CCVector.create ()
and check = ref false

(** parse_args returns parameters *)
let parse_args () =
  (* special handlers *)
  let add_file s = CCVector.push files s in
  (* options list *)
  let options = (
    [ "--ord", Arg.Set_string ord, " choose ordering (rpo,kbo)"
    ; "--version", Arg.Set version, " print version"
    ; "--steps", Arg.Set_int steps,
      " maximal number of steps of given clause loop (no limit if negative)"
    ; "--timeout", Arg.Set_float timeout, " timeout (in seconds)"
    ; "-t", Arg.Set_float timeout, " short for --timeout"
    ; "--expand-def", Arg.Set expand_def, " expand definitions"
    ; "--presaturate", Arg.Set presaturate,
      " pre-saturate (interreduction of) the initial clause set"
    ; "--dot", Arg.String (fun s -> dot_file := Some s) , " print final state to file in DOT"
    ; "--dot-sat", Arg.Set dot_sat, " print saturated set into DOT"
    ; "--dot-all-roots", Arg.Set dot_all_roots, " print all empty clauses into DOT"
    ; "--color", Arg.Bool CCFormat.set_color_default, " enable/disable ANSI color codes"
    ; "--seed", Arg.Set_int seed, " set random seed"
    ; "--unary-depth", Arg.Set_int unary_depth, " maximum depth for successive unary inferences"
    ; "--def-as-rewrite", Arg.Set def_as_rewrite, " treat definitions as rewrite rules"
    ; "--def-as-assert", Arg.Clear def_as_rewrite, " treat definitions as axioms"
    ; "--check", Arg.Set check, " check proof"
    ; "--prelude", Arg.String (CCVector.push prelude), " parse prelude file"
    ; "--no-check", Arg.Clear check, " do not check proof"
    ] @ Options.make ()
  ) |> List.sort (fun (s1,_,_)(s2,_,_) -> String.compare s1 s2)
                |> Arg.align
  in
  Arg.parse options add_file "solve problems in files";
  if CCVector.is_empty files then (
    CCVector.push files "stdin";
    if !Options.input = Options.I_guess then Options.input := Options.I_zf;
  );
  (* freeze arrays of files, so that from now on they are immutable *)
  let prelude = CCVector.freeze prelude in
  let files = CCVector.freeze files in
  (* return parameter structure *)
  { param_ord= !ord; param_seed = !seed; param_steps = !steps;
    param_version= !version; param_timeout = !timeout; param_prelude= prelude;
    param_files = files; param_select = !select;
    param_stats= ! Options.stats; param_def_as_rewrite= !def_as_rewrite;
    param_presaturate = !presaturate; param_dot_all_roots= !dot_all_roots;
    param_dot_file = !dot_file;
    param_unary_depth= !unary_depth; param_dot_sat= !dot_sat;
    param_expand_def= !expand_def; param_check= !check; }

let add_opt = Options.add_opt
let add_opts = Options.add_opts

(* key used to store the parameters in Flex_state *)
let key : t Flex_state.key = Flex_state.create_key()

