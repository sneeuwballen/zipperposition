
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
type scope = Substs.scope

val beta_reduce : ?depth:int -> term -> term
  (** Beta-reduce the term *)

val eta_reduce : term -> term
  (** Eta-reduce the term *)

val lambda_abstract : term -> sub:term -> term
  (** [lambda_abstract term ~sub], applied to a curried term [term], and a
      subterm [sub] of [term], gives [term'] such that
      [beta_reduce (term' @ sub_t) == term] holds.
      It basically abstracts out [sub] with a lambda. If [sub] is not
      a subterm of [term], then [term' == ^[X]: term].

      For instance (@ are omitted), [lambda_abstract f(a,g @ b,c) ~sub:g] will return
      the term [^[X]: f(a, X @ b, c)].
  *)

val lambda_abstract_list : term -> term list -> term
  (** Abstract successively the given subterms, starting from the
      right ones. The converse operation is {!lambda_apply_list},
      that is, [lambda_apply_list (lambda_abstract_list t args) args = t]
      should hold. *)

val match_types : ?subst:Substs.Ty.t ->
                  Type.t -> scope -> Type.t list -> scope ->
                  Substs.Ty.t
  (** Match the first type's arguments with the list.
      @raise TypeUnif.Error if types are not compatible *)

val can_apply : Type.t -> Type.t list -> bool
  (** Can we apply a term with the given type to terms with
      the corresponding list of types? *)

val lambda_apply_list : term -> term list -> term
  (** Apply a lambda to a list of arguments.
      The type of the lambda must be a generalization of a function
      that takes the list's types as arguments.
      
      @raise TypeUnif.Error if the first term doesn't have a function type or
        if the types are not compatible
  *)
