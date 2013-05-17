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

open Basic

(** parse_args returns parameters *)
let parse_args () =
  let help_select = FoUtils.sprintf "selection function (@[<h>%a@])"
    (FoUtils.pp_list ~sep:"," Format.pp_print_string)
    (Selection.available_selections ()) in
  let unamed_skolem () = Terms.skolem := Terms.unamed_skolem in
  (* parameters *)
  let ord = ref "rpo6"
  and seed = ref 1928575
  and steps = ref 0
  and version = ref false
  and timeout = ref 0.
  and proof = ref "debug"
  and index = ref "fp"
  and split = ref false
  and theories = ref true
  and calculus = ref "delayed"
  and presaturate = ref false
  and heuristic_precedence = ref true
  and kb = ref (Filename.concat Const.home "kb")
  and kb_load = ref []
  and kb_clear = ref false
  and kb_print = ref false
  and kb_where = ref false
  and dot_file = ref None
  and select = ref "SelectComplex"
  and progress = ref false
  and files = ref [] in
  (* special handlers *)
  let set_progress () =
    FoUtils.need_cleanup := true;
    progress := true
  in
  (* options list *) 
  let options =
    [ ("-ord", Arg.Set_string ord, "choose ordering (rpo,kbo)");
      ("-debug", Arg.Int FoUtils.set_debug, "debug level");
      ("-version", Arg.Set version, "print version");
      ("-steps", Arg.Set_int steps, "maximal number of steps of given clause loop");
      ("-unamed-skolem", Arg.Unit unamed_skolem, "unamed skolem symbols");
      ("-calculus", Arg.Set_string calculus, "set calculus ('superposition' or 'delayed' (default))");
      ("-timeout", Arg.Set_float timeout, "timeout (in seconds)");
      ("-select", Arg.Set_string select, help_select);
      ("-split", Arg.Set split, "enable splitting");
      ("-kb", Arg.Set_string kb, "Knowledge Base (KB) file");
      ("-kb-load", Arg.String (fun f -> kb_load := f :: !kb_load), "load theory file into KB");
      ("-kb-clear", Arg.Set kb_clear, "clear content of KB and exit");
      ("-kb-print", Arg.Set kb_print, "print content of KB and exit");
      ("-kb-where", Arg.Set kb_where, "print default dir that is search for KB");
      ("-print-sort", Arg.Unit (fun () -> Terms.pp_term_debug#sort true), "print sorts");
      ("-progress", Arg.Unit set_progress, "print progress");
      ("-profile", Arg.Set FoUtils.enable_profiling, "enable profiling of code");
      ("-no-theories", Arg.Clear theories, "do not detect theories in input");
      ("-no-heuristic-precedence", Arg.Clear heuristic_precedence, "do not use heuristic to choose precedence");
      ("-proof", Arg.Set_string proof, "choose proof printing (none, debug, or tstp)");
      ("-presaturate", Arg.Set presaturate, "pre-saturate (interreduction of) the initial clause set");
      ("-dot", Arg.String (fun s -> dot_file := Some s) , "print final state to file in DOT");
      ("-seed", Arg.Set_int seed, "set random seed");
      ("-index", Arg.Set_string index, "index structure (fp or discr_tree)");
    ]
  in
  Arg.parse options (fun f -> files := f :: !files) "solve problems in files";
  (if !files = [] then files := ["stdin"]);
  let param_ord = Orderings.choose !ord in
  (* return parameter structure *)
  { param_ord; param_seed = !seed; param_steps = !steps;
    param_version= !version; param_calculus= !calculus; param_timeout = !timeout;
    param_files = !files; param_select = !select; param_theories = !theories;
    param_progress = !progress;
    param_proof = !proof; param_split = !split;
    param_presaturate = !presaturate;
    param_index= !index; param_dot_file = !dot_file;
    param_kb = !kb; param_kb_load = !kb_load; param_kb_where = !kb_where;
    param_kb_clear = !kb_clear;
    param_kb_print = !kb_print; 
    param_precedence= !heuristic_precedence;}
