
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Selection functions} *)

open Libzipperposition

(** See "E: a brainiac theorem prover". A selection function
    returns a bitvector of selected literals. *)

type t = Literal.t array -> CCBV.t

val no_select : t

val select_max_goal : strict:bool -> ord:Ordering.t -> t
  (** Select a maximal negative literal, if any, or nothing *)

val select_diff_neg_lit : strict:bool -> ord:Ordering.t -> t
  (** arbitrary negative literal with maximal weight difference between sides *)

val select_complex : strict:bool -> ord:Ordering.t -> t
  (** x!=y, or ground negative lit, or like select_diff_neg_lit *)

val select_complex_except_RR_horn : strict:bool -> ord:Ordering.t -> t
  (** if clause is a restricted range horn clause, then select nothing;
      otherwise, like select_complex *)

(** {2 Global selection Functions} *)

val default_selection : ord:Ordering.t -> t
  (** Default selection function *)

val selection_from_string : ord:Ordering.t -> string -> t
  (** selection function from string (may fail) *)

val available_selections : unit -> string list
  (** available names for selection functions *)

val register : string -> (ord:Ordering.t -> t) -> unit
  (** Register new selection function
      @raise Failure if the name is already used *)
