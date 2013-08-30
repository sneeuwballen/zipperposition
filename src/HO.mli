
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

(** {1 Higher Order operations} *)

(** Higher order formulas and terms are represented by terms. *)

val curry : Term.t -> Term.t                    (** Curry all subterms *)
val uncurry : Term.t -> Term.t                  (** Un-curry all subterms *)
val curried : Term.t -> bool                    (** Is the term already curried? *)

val is_fo : Term.t -> bool                 (** Check that the (curried) term is first-order *)

val beta_reduce : Term.t -> Term.t              (** Beta-reduce the (curried) term *)

val eta_reduce : Term.t -> Term.t               (** Eta-reduce the (curried) term *)

val lambda_abstract : signature:Signature.t -> Term.t -> Term.t -> Term.t
  (** [lambda_abstract t sub_t], applied to a curried term [t], and a
      subterm [sub_t] of [t], gives [t'] such that
      [beta_reduce (t' @ sub_t) == t] holds.
      It basically abstracts out [sub_t] with a lambda. If [sub_t] is not
      a subterm of [t], then [t' == ^[X]: t].

      For instance (@ are omitted), [lambda_abstract f(a,g @ b,c) g] will return
      the term [^[X]: f(a, X @ b, c)].

      The signature is needed to infer the types of terms, and therefore
      to infer the type of the lambda-bound variable
  *)

val lambda_abstract_list : signature:Signature.t -> Term.t -> Term.t list -> Term.t
  (** Abstract successively the given subterms, starting from the
      left ones (the closer from the left, the deeper the lambda) *)

val lambda_apply_list : Term.t -> Term.t list -> Term.t
