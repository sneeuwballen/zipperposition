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
  param_ord : string;
  param_steps : int;
  param_version : bool;
  param_calculus : string;
  param_timeout : float;
  param_files : string list;
  param_theories : bool;          (** detect theories *)
  param_precedence : bool;        (** use heuristic for precedence? *)
  param_select : string;          (** name of the selection function *)
  param_progress : bool;          (** print progress during search *)
  param_proof : bool;             (** print proof *)
  param_dot_file : string option; (** file to print the final state in *)
  param_presimplify : bool;       (** do we simplify the initial set? *)
  param_output_syntax : string;   (** syntax for output *)
  param_index : string;           (** indexing structure *)
  param_print_sort : bool;        (** print sorts of terms *)
  param_print_all : bool;         (** print desugarized lambda / DB symbols *)
}
