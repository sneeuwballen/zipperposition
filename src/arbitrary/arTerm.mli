
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

(** {1 Arbitrary Typed Terms and Formulas} *)

open Logtk

type 'a arbitrary = 'a QCheck.Arbitrary.t

val default : FOTerm.t arbitrary
  (** Default polymorphic term *)

val ground : FOTerm.t arbitrary
  (** Default ground monomorphic term *)

val pred : FOTerm.t arbitrary
  (** predicates (type "o") *)

val pos : FOTerm.t -> Position.t arbitrary
  (** Random valid position in the term *)

module HO : sig
  val ground : HOTerm.t arbitrary
    (** Ground HO terms *)

  val default : HOTerm.t arbitrary
    (** HO polymorphic term *)
end

(** {2 Prolog Terms} *)

module PT : sig
  val default : PrologTerm.t arbitrary
    (** Default polymorphic prolog term *)

  val ground : PrologTerm.t arbitrary
    (** Default ground monomorphic term *)

  val pred : PrologTerm.t arbitrary
    (** predicates (type "o") *)
end
