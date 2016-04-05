
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Classification of Constants} *)

open Libzipperposition

(* NOTE:
   Some sub-constants can ALSO be constants (e.g. #nat0 is sub-constant
   of #list0 in case cons, but it can also be a constant {z, s #nat1}).
   This explains the {!Inductive_cst} case.
*)

type res =
  | Ty of Ind_ty.t
  | Cstor of Ind_ty.constructor * Ind_ty.t
  | Inductive_cst of (Ind_cst.cst option * Ind_cst.sub_cst option)
  | Other

val classify : ID.t -> res
(** [classify id] returns the role [id] plays in inductive reasoning *)

val prec_constr : [`partial] Precedence.Constr.t
(** Partial order on [ID.t], with:
    regular > constant > sub_constant > cstor *)

