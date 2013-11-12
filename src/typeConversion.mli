
(*
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

(** {1 Conversion between type representations} *)

(** {2 Basic -> Type} *)

type ctx
  (** Context used to map names to ints *)

val create_ctx : unit -> ctx

val clear : ctx -> unit

val of_basic : ?ctx:ctx -> Basic.Ty.t -> Type.t
  (** Conversion from a {!Basic.Ty.t} term representation.
      An optional {!ctx} can be used to map named variables
      to {!Type.Var}s. *)

val of_quantified : ?ctx:ctx -> Basic.Ty.quantified -> Type.t
  (** Same as {!of_basic}, ignoring the quantifiers *)

(** {2 Type -> Basic} *)

val to_basic : Type.t -> Basic.Ty.t
  (** Convert back to "parsed" raw type *)

val to_quantified : Type.t -> Basic.Ty.quantified
  (** Convert to basic type representation and quantify
      all variables universally *)
