
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Classification of Constants} *)

(* TODO check this before all declarations?
   to avoid a constructor to be also declared as sub-constant, etc. *)

open Libzipperposition

type res =
  | Ty of Ind_ty.t
  | Cstor of Ind_ty.constructor * Ind_ty.t
  | Cst of Ind_cst.cst
  | Sub of Ind_cst.sub_cst
  | Other

let classify id =
  let res =
    CCList.find
      (function
        | Ind_ty.Payload_ind_cstor (c,t) -> Some (Cstor (c,t))
        | Ind_ty.Payload_ind_type x -> Some (Ty x)
        | Ind_cst.Payload_cst c -> Some (Cst c)
        | Ind_cst.Payload_sub_cst s -> Some (Sub s)
        | _ -> None)
      (ID.payload id)
  in
  match res with
  | None -> Other
  | Some x -> x

(* FIXME: Sub and Cst should have same precedence, because some
   sub-constants can ALSO be constants (e.g. #nat0 is sub-constant
   of #list0 in case cons, but it can also be a constant {z, s #nat1})

   FIXME: also, it means multiple classification is required
   --> `| Inductive_skolem of [cst | sub] list` where the list can grow,
     for every non-defined constant that has an inductive type?

   FIXME: also, Inductive_skolem cases should be compared by "sub-case"
     order (i.e. `x sub-cst-of y` means `x < y`); this is stable
*)

let prec_constr_ a b =
  let to_int_ = function
    | Ty _ -> 0
    | Cstor _ -> 1
    | Sub _ -> 2
    | Cst _ -> 3  (* larger than Sub *)
    | Other -> 4
  in
  let c_a = classify a in
  let c_b = classify b in
  match c_a, c_b with
  | Ty _, Ty _
  | Cstor _, Cstor _
  | Cst _, Cst _
  | Sub _, Sub _
  | Other, Other -> 0
  | Ty _, _
  | Cstor _, _
  | Cst _, _
  | Sub _, _
  | Other, _ -> CCInt.compare (to_int_ c_a) (to_int_ c_b)

let prec_constr = Precedence.Constr.make prec_constr_
