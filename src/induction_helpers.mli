
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Common stuff for Induction}

    Also registers some CLI options *)

(* TODO merge with induction module *)

open Logtk

type term = FOTerm.t
type formula = TypedSTerm.t

val ind_types : unit -> (string * string list) list
(** List of [ty, constructors] *)

val cover_set_depth : unit -> int
(** Depth for generating cover sets *)

val is_constructor : ID.t -> bool
(** Is this ID.tbol a registered inductive constructor? *)

val on_enable : unit Signal.t
(** Triggered if induction is enabled  *)

val constr_cstors : ID.t -> ID.t -> Logtk.Comparison.t
(** Partial order on ID.tbols, where constructors are smaller than other
    ID.tbols *)

val init_from_decls :
  (string * Logtk_parsers.Ast_tptp.optional_info) Sequence.t -> unit
(** Initialize from a bunch of declarations' optional info, if one takes
    only pairs [(some_type_name : $tType, info)] *)

module Make(Ctx : Ctx_intf.S) : sig
  val declare_types : unit -> unit
  (** Declare the list of [ind_types ()] to the given context's *)

  val is_a_constructor : term -> bool
  (** Is the term the application of a constructor? *)

  val find_inductive_cst : Literals.t -> term Sequence.t
  (** Potential inductive constants in those literals *)

  val constr_sub_cst : ID.t -> ID.t -> Logtk.Comparison.t
  (** constants are bigger than their sub-constants *)
end

module MakeAvatar(A : Avatar.S) : sig
  val clear_skolem_ctx : unit -> unit

  val inf_introduce_lemmas : A.E.C.t -> A.E.C.t list
  (** Introduce lemmas heuristically based on the given clause *)

  val show_lemmas : unit -> unit
  (** Print all lemmas that were added in debug mode *)
end
