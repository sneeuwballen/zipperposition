(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** Parameters for the prover, the calculus, etc. *)

(** parameters for the main procedure *)
type parameters = {
  param_ord : Types.precedence -> Types.ordering;
  param_role : string option;     (** Role of the process *)
  param_seed : int;
  param_steps : int;
  param_version : bool;
  param_calculus : string;
  param_timeout : float;
  param_files : string list;
  param_theories : bool;          (** detect theories *)
  param_precedence : bool;        (** use heuristic for precedence? *)
  param_select : string;          (** name of the selection function *)
  param_progress : bool;          (** print progress during search *)
  param_pipeline : bool;          (** use the pipelined given clause *)
  param_pipeline_capacity : int;  (** capacity (in clauses) of the pipeline *)
  param_parallel : bool;          (** is the pipeline parallel, ie multicore? *)
  param_proof : bool;             (** print proof *)
  param_dot_file : string option; (** file to print the final state in *)
  param_kb : string;              (** file to use for KB *)
  param_kb_load : string list;    (** theory files to read *)
  param_kb_clear : bool;          (** do we need to clear the KB? *)
  param_kb_print : bool;          (** print knowledge base and exit *)
  param_learn : bool;             (** learn lemmas? *)
  param_presaturate : bool;       (** initial interreduction of proof state? *)
  param_output_syntax : string;   (** syntax for output *)
  param_index : string;           (** indexing structure *)
  param_print_sort : bool;        (** print sorts of terms *)
  param_print_all : bool;         (** print desugarized lambda / DB symbols *)
}

(** parse_args returns parameters *)
let parse_args () =
  let help_select = FoUtils.sprintf "selection function (@[<h>%a@])"
    (FoUtils.pp_list ~sep:"," Format.pp_print_string)
    (Selection.available_selections ()) in
  let unamed_skolem () = Terms.skolem := Terms.unamed_skolem in
  (* parameters *)
  let ord = ref "rpo6"
  and role = ref None
  and seed = ref 1928575
  and steps = ref 0
  and version = ref false
  and timeout = ref 0.
  and proof = ref true
  and output = ref "debug"
  and index = ref "fp"
  and theories = ref true
  and calculus = ref "superposition"
  and presaturate = ref false
  and heuristic_precedence = ref true
  and dot_file = ref None
  and kb = ref "kb"
  and kb_load = ref []
  and kb_clear = ref false
  and kb_print = ref false
  and learn = ref true
  and select = ref "SelectComplex"
  and progress = ref false
  and pipeline = ref false
  and pipeline_capacity = ref 10
  and parallel = ref false
  and print_sort = ref false
  and print_all = ref false
  and files = ref [] in
  (* some handlers *)
  let set_parallel () = parallel := true; pipeline := true in
  (* options list *) 
  let options =
    [ ("-ord", Arg.Set_string ord, "choose ordering (rpo,kbo)");
      ("-role", Arg.String (fun r -> role := Some r), "role of the process (do not use)");
      ("-debug", Arg.Int FoUtils.set_debug, "debug level");
      ("-version", Arg.Set version, "print version");
      ("-steps", Arg.Set_int steps, "maximal number of steps of given clause loop");
      ("-unamed-skolem", Arg.Unit unamed_skolem, "unamed skolem symbols");
      ("-calculus", Arg.Set_string calculus, "set calculus ('superposition' or 'delayed')");
      ("-timeout", Arg.Set_float timeout, "verbose mode");
      ("-select", Arg.Set_string select, help_select);
      ("-kb", Arg.Set_string kb, "Knowledge Base (KB) file");
      ("-kb-load", Arg.String (fun f -> kb_load := f :: !kb_load), "load theory file into KB");
      ("-kb-clear", Arg.Set kb_clear, "clear content of KB and exit");
      ("-kb-print", Arg.Set kb_print, "print content of KB and exit");
      ("-no-learning", Arg.Clear learn, "disable lemma learning");
      ("-learning-limit", Arg.Set_int LemmaLearning.max_lemmas, "maximum number of lemma learnt at once");
      ("-progress", Arg.Set progress, "print progress");
      ("-profile", Arg.Set FoUtils.enable_profiling, "enable profiling of code");
      ("-pipeline", Arg.Set pipeline, "use pipelined given clause");
      ("-pipeline-capacity", Arg.Set_int pipeline_capacity, "set capacity of pipeline");
      ("-parallel", Arg.Unit set_parallel, "fork processes to parallelize the pipeline");
      ("-no-theories", Arg.Clear theories, "do not detect theories in input");
      ("-no-heuristic-precedence", Arg.Clear heuristic_precedence, "do not use heuristic to choose precedence");
      ("-no-proof", Arg.Clear proof, "disable proof printing");
      ("-presaturate", Arg.Set presaturate, "pre-saturate (interreduction of) the initial clause set");
      ("-dot", Arg.String (fun s -> dot_file := Some s) , "print final state to file in DOT");
      ("-output", Arg.Set_string output, "output syntax ('debug', 'tstp')");
      ("-index", Arg.Set_string index, "index structure (fp or discr_tree)");
      ("-print-sort", Arg.Set print_sort, "print sorts of terms");
      ("-print-all", Arg.Set print_all, "print desugarized terms (lambdas, De Bruijn terms)");
      ("-print-ord", Arg.Unit (fun () -> Clauses.pp_literal_debug#ord true), "print order of sides of literals");
    ]
  in
  Arg.parse options (fun f -> files := f :: !files) "solve problems in files";
  (if !files = [] then files := ["stdin"]);
  let param_ord = match !ord with
    | "rpo" -> Orderings.rpo
    | "rpo6" -> Orderings.rpo6
    | "kbo" -> Orderings.kbo
    | x -> failwith ("unknown ordering " ^ x) in
  (* return parameter structure *)
  { param_ord; param_role = !role; param_seed = !seed; param_steps = !steps;
    param_version= !version; param_calculus= !calculus; param_timeout = !timeout;
    param_files = !files; param_select = !select; param_theories = !theories;
    param_progress = !progress; param_pipeline = !pipeline;
    param_pipeline_capacity = !pipeline_capacity; param_parallel = !parallel;
    param_proof = !proof;
    param_presaturate = !presaturate; param_output_syntax = !output;
    param_index= !index; param_dot_file = !dot_file;
    param_kb = !kb; param_kb_load = !kb_load;
    param_kb_clear = !kb_clear;
    param_kb_print = !kb_print; param_learn = !learn;
    param_print_sort = !print_sort; param_print_all = !print_all;
    param_precedence= !heuristic_precedence;}
