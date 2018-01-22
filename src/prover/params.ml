
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Parameters for the prover, etc.} *)

open Logtk

(* TODO: params to limit depth of preprocessing *)
(* TODO: params to enable/disable some preprocessing *)

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

let default : t = {
  ord= "kbo";
  seed = 1928575;
  steps = -1;
  version= false;
  timeout = 0.;
  prelude= CCVector.create() |> CCVector.freeze;
  files = CCVector.create() |> CCVector.freeze;
  select = "default";
  stats= !Options.stats;
  def_as_rewrite= true;
  presaturate = false;
  dot_all_roots= false;
  dot_file = None;
  dot_llproof= None;
  dot_check=None;
  unary_depth= 1;
  dot_sat= false;
  expand_def= false;
  check= false;
}

let select = ref default.select

(** parse_args returns parameters *)
let parse_args () =
  let ord = ref default.ord
  and seed = ref default.seed
  and steps = ref default.steps
  and version = ref default.version
  and timeout = ref default.timeout
  and presaturate = ref default.presaturate
  and dot_file = ref default.dot_file
  and dot_llproof = ref default.dot_llproof
  and dot_sat = ref default.dot_sat
  and dot_all_roots = ref default.dot_all_roots
  and dot_check = ref default.dot_check
  and expand_def = ref default.expand_def
  and unary_depth = ref default.unary_depth
  and def_as_rewrite = ref default.def_as_rewrite
  and prelude = CCVector.create()
  and files = CCVector.create ()
  and check = ref default.check
  in
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
    ; "--dot-llproof", Arg.String (fun s -> dot_llproof := Some s) , " print LLProof to file in DOT"
    ; "--dot-sat", Arg.Set dot_sat, " print saturated set into DOT"
    ; "--dot-all-roots", Arg.Set dot_all_roots, " print all empty clauses into DOT"
    ; "--dot-check-prefix", Arg.String (fun s-> dot_check :=Some s), " prefix for printing checker proofs in DOT"
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
  { ord= !ord; seed = !seed; steps = !steps;
    version= !version; timeout = !timeout; prelude= prelude;
    files = files; select = !select;
    stats= ! Options.stats; def_as_rewrite= !def_as_rewrite;
    presaturate = !presaturate; dot_all_roots= !dot_all_roots;
    dot_file = !dot_file; dot_llproof= !dot_llproof;
    dot_check= !dot_check;
    unary_depth= !unary_depth; dot_sat= !dot_sat;
    expand_def= !expand_def; check= !check; }

let add_opt = Options.add_opt
let add_opts = Options.add_opts

(* key used to store the parameters in Flex_state *)
let key : t Flex_state.key = Flex_state.create_key()

