
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

(** {1 First-order Formulas}
Hashconsed formulas for first-order logic. This provides many useful
functions, and smart constructors that perform some basic
simplifications

TODO: attributes, to speed up simplification/flattening/closeness
checking
*)

type symbol = Symbol.t
type type_ = Type.t

module type S = Formula_intf.S
module type TERM = Formula_intf.TERM

module Make(MyT : TERM) : S
  with type term = MyT.t
  and type term_set = MyT.Set.t

module FO : sig
  include S with type term = FOTerm.t and type term_set = FOTerm.Set.t

  (** {2 Conversion to higher-order term} *)

  val to_hoterm : t -> HOTerm.t
  val of_hoterm : HOTerm.t -> t option
end

(** Functor to translate terms *)
module Map(From:S)(To:S) : sig
  val map : (From.term -> To.term) -> From.t -> To.t
end
