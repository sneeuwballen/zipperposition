open Logtk

module T = Term

module type S = sig
  module Ctx : Ctx.S
  module C : Clause.S with module Ctx = Ctx

  val on_pred_skolem_introduction : (C.t * Term.t) Signal.t
  (** this signal is raised when a predicate Skolem is introduced  *)

  val is_renaming_clause : C.t -> bool
  (** This clause has the shape of the renaming clause   *)

  val rename_form : 
    ?should_rename:(T.t -> bool) -> 
    ?polarity_aware:bool ->
    c:C.t ->
    T.t -> bool -> (T.t * C.t list * C.t list) option
  (**
    `rename_form ~should_rename ~c f polarity` tries to find
    a definition for formula f with the given polarity.

    The result is of the form 
      `Some (renamer term, new_defs, proof_parents)`

    If the definition for a generalization of f is already found in 
    the store, but with different polarity new_defs will contain definition
    for the missing polarity. If the definition of f is found for
    the right polarity, new_defs will be empty.

    If f is not found an `should_rename f` holds, then definition of f
    is introduced for f with the given polarity.

    In each case, proof_parents contains the clauses that introduce
    definition for f (either or both of polarities, that is).

    For details and examples, consult our PAAR 2020 paper
    \url{http://matryoshka.gforge.inria.fr/pubs/ho_bools_paper.pdf}
    
  *)
  val get_skolem : parent:C.t -> mode:[< `Choice | `SkolemRecycle | `SkolemAlwaysFresh ] -> T.t -> T.t
  (** `get_skolem ~parent ~mode f` computes a ``Skolem'' term 
     for a formula f. This is either real Skolem term of Choice symbol applied to f,
     depending on the mode  *)

end

module Make(C:Clause.S) : S with module Ctx = C.Ctx and module C = C
