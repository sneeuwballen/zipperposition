
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

(** {1 Lambda-Calculus} *)

type term = HOTerm.t

val beta_reduce : ?depth:int -> term -> term
  (** Beta-reduce the term *)

val eta_reduce : term -> term
  (** Eta-reduce the term *)

val lambda_abstract : signature:Signature.t -> term -> term -> term
  (** [lambda_abstract term sub_t], applied to a curried term [term], and a
      subterm [sub_t] of [term], gives [term'] such that
      [beta_reduce (term' @ sub_t) == term] holds.
      It basically abstracts out [sub_t] with a lambda. If [sub_t] is not
      a subterm of [term], then [term' == ^[X]: term].

      For instance (@ are omitted), [lambda_abstract f(a,g @ b,c) g] will return
      the term [^[X]: f(a, X @ b, c)].

      The signature is needed to infer the types of terms, and therefore
      to infer the type of the lambda-bound variable
  *)

val lambda_abstract_list : signature:Signature.t -> term -> term list -> term
  (** Abstract successively the given subterms, starting from the
      left ones (the closer from the left, the deeper the lambda) *)

val lambda_apply_list : term -> term list -> term
