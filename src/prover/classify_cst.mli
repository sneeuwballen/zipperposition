
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Classification of Constants} *)

open Libzipperposition

type res =
  | Ty of Ind_ty.t
  | Cstor of Ind_ty.constructor * Ind_ty.t
  | Cst of Ind_cst.cst
  | Sub of Ind_cst.sub_cst
  | Other

val classify : ID.t -> res
(** [classify id] returns the role [id] plays in inductive reasoning *)

val prec_constr : [`partial] Precedence.Constr.t
(** Partial order on [ID.t], with:
    regular > constant > sub_constant > cstor *)

