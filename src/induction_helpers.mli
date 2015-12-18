
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Common stuff for Induction}

    Also registers some CLI options *)

(* TODO
   - merge with induction module
   - fix initialization routine (use a regular type def!) *)

open Logtk

type term = FOTerm.t
type formula = TypedSTerm.t

(* FIXME: this should be global only in a functor, e.g. in Induction *)
val ind_types : unit -> (ID.t * ID.t list) list
(** List of [ty, constructors] *)

val cover_set_depth : unit -> int
(** Depth for generating cover sets *)

val is_constructor : ID.t -> bool
(** Is this ID.tbol a registered inductive constructor? *)

val on_enable : unit Signal.t
(** Triggered if induction is enabled  *)

val constr_cstors : [`partial] Precedence.Constr.t
(** Partial order on ID.tbols, where constructors are smaller than other
    ID.tbols *)

val init_from_decls :
  (string * Logtk_parsers.Ast_tptp.optional_info) Sequence.t -> unit
(** Initialize from a bunch of declarations' optional info, if one takes
    only pairs [(some_type_name : $tType, info)] *)

(* FIXME: use a dedicated Context that can be created BEFORE
   the precedence is created (so that constr_sub_cst is created in time)

   OR:
     use ID.payload to recognize inductive constants and sub-constants,
     so that no context is needed. *)

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
  val inf_introduce_lemmas : A.E.C.t -> A.E.C.t list
  (** Introduce lemmas heuristically based on the given clause *)

  val show_lemmas : unit -> unit
  (** Print all lemmas that were added in debug mode *)
end
