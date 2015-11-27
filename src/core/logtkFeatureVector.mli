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

(** {1 Feature Vector indexing} *)

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption *)

module Make(C : LogtkIndex.CLAUSE) : sig
  type feature_vector = int list
    (** a vector of feature *)

  (** {2 Features} *)

  module Feature : sig
    type t = {
      name : string;
      f : LogtkIndex.lits -> int;
    } (** a function that computes a given feature on clauses *)

    val name : t -> string
    val compute : t -> LogtkIndex.lits -> int
    include LogtkInterfaces.PRINT with type t := t

    val sum_of_depths : t                 (** sum of depths of symbols *)
    val size_plus : t                     (** size of positive clause *)
    val size_minus : t                    (** size of negative clause *)
    val count_symb_plus : LogtkSymbol.t -> t   (** occurrences of symbol in positive clause *)
    val count_symb_minus : LogtkSymbol.t -> t  (** occurrences of symbol in negative clause *)
    val max_depth_plus : LogtkSymbol.t -> t    (** maximal depth of symb in positive clause *)
    val max_depth_minus : LogtkSymbol.t -> t   (** maximal depth of symb in negative clause *)
  end

  val compute_fv : Feature.t list -> LogtkIndex.lits -> feature_vector

  (** {2 LogtkIndex} *)

  include LogtkIndex.SUBSUMPTION_IDX with module C = C

  val retrieve_alpha_equiv : t -> LogtkIndex.lits -> 'a -> ('a -> C.t -> 'a) -> 'a
  (** Retrieve clauses that are potentially alpha-equivalent to the given clause
      @since 0.6 *)

  val retrieve_alpha_equiv_c : t -> C.t -> 'a -> ('a -> C.t -> 'a) -> 'a
  (** @since 0.6 *)

  val empty_with : Feature.t list -> t

  val default_features : Feature.t list

  val features_of_signature : ?ignore:(LogtkSymbol.t -> bool) ->
                              LogtkSignature.t -> Feature.t list
    (** Build a set of features from the given signature. LogtkSymbols
        that satisfy [ignore] are not considered (default ignores
        connectives) *)

  val of_signature : LogtkSignature.t -> t

  val features : t -> Feature.t list
end
