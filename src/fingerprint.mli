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

(** fingerprint term indexing *)

open Types

type fingerprint_fun

val fp3d : fingerprint_fun
val fp3w : fingerprint_fun
val fp4d : fingerprint_fun
val fp4w : fingerprint_fun
val fp5m : fingerprint_fun
val fp6m : fingerprint_fun
val fp7 : fingerprint_fun
val fp7m : fingerprint_fun
val fp16 : fingerprint_fun

val mk_index : fingerprint_fun -> Index.index
