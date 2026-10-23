(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Classification of Constants} *)

open Logtk

(* NOTE:
   Some sub-constants can ALSO be constants (e.g. #nat0 is sub-constant
   of #list0 in case cons, but it can also be a constant {z, s #nat1}).
   This explains the {!Inductive_cst} case.
*)

type res =
  | Ty of Ind_ty.t
  | Cstor of Ind_ty.constructor * Ind_ty.t
  | Inductive_cst of Ind_cst.t option
  | Projector of Name.t  (** projector of some constructor (id: type) *)
  | DefinedCst of int * Statement.definition
      (** (recursive) definition of given stratification level + definition *)
  | Parameter of int
  | Skolem
  | Other

val classify : Ind_cst.state -> Name.t -> res
(** [classify id] returns the role [id] plays in inductive reasoning *)

val id_is_cstor : Ind_cst.state -> Name.t -> bool
val id_is_projector : Ind_cst.state -> Name.t -> bool
val id_is_defined : Name.t -> bool
val pp_res : res CCFormat.printer

val pp_signature : Ind_cst.state -> Signature.t CCFormat.printer
(** Print classification of signature *)

val prec_constr : Ind_cst.state -> [ `partial ] Precedence.Constr.t
(** Partial order on [Name.t], with: regular > constant > sub_constant > cstor
*)

val weight_fun : Ind_cst.state -> Precedence.weight_fun
