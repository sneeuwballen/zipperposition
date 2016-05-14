
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Classification of Constants} *)

open Libzipperposition

type res =
  | Ty of Ind_ty.t
  | Cstor of Ind_ty.constructor * Ind_ty.t
  | Inductive_cst of Ind_cst.cst option
  | Projector of ID.t (** projector of some constructor (id: type) *)
  | DefinedCst of int (** (recursive) definition of given stratification level *)
  | Other

let classify id =
  let res =
    CCList.find
      (function
        | Ind_ty.Payload_ind_cstor (c,t) -> Some (Cstor (c,t))
        | Ind_ty.Payload_ind_type x -> Some (Ty x)
        | Ind_ty.Payload_ind_constant ->
          let as_cst = Ind_cst.as_cst id in
          Some (Inductive_cst as_cst)
        | Ind_ty.Payload_ind_projector id -> Some (Projector id)
        | Statement.Payload_defined_cst l -> Some (DefinedCst l)
        | _ -> None)
      (ID.payload id)
  in
  match res with
  | None -> Other
  | Some x -> x

let dominates_ opt_c opt_sub =
  CCOpt.(get false (map2 Ind_cst.dominates opt_c opt_sub))

let prec_constr_ a b =
  let to_int_ = function
    | Ty _ -> 0
    | Projector _ -> 1
    | Cstor _ -> 2
    | Inductive_cst _ -> 3
    | Other -> 4
    | DefinedCst _ -> 5 (* defined: biggest *)
  in
  let c_a = classify a in
  let c_b = classify b in
  match c_a, c_b with
  | Ty _, Ty _
  | Cstor _, Cstor _
  | Projector _, Projector _
  | Other, Other -> 0
  | Inductive_cst c1, Inductive_cst c2 ->
    (* Inductive_cst cases should be compared by "sub-case" order (i.e. `x
       sub-cst-of y` means `x < y`); this is a stable ordering. *)
    if dominates_ c1 c2 then 1
    else if dominates_ c2 c1 then -1
    else 0
  | DefinedCst l1, DefinedCst l2 ->
    (* bigger level means defined later *)
    CCInt.compare l1 l2
  | Ty _, _
  | Cstor _, _
  | Inductive_cst _, _
  | Projector _, _
  | DefinedCst _, _
  | Other, _ -> CCInt.compare (to_int_ c_a) (to_int_ c_b)

let prec_constr = Precedence.Constr.make prec_constr_
