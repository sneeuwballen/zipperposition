
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Classification of Constants} *)

open Libzipperposition

type res =
  | Ty of Ind_ty.t
  | Cstor of Ind_ty.constructor * Ind_ty.t
  (* | Inductive_cst of Ind_cst.cst option *)
  | Projector of ID.t (** projector of some constructor (id: type) *)
  | DefinedCst of int (** (recursive) definition of given stratification level *)
  | Other

let classify id = match ID.payload id with
  | Ind_ty.Payload_ind_cstor (c,t) -> Cstor (c,t)
  | Ind_ty.Payload_ind_type x -> Ty x
  | Ind_ty.Payload_ind_projector id -> Projector id
  | Statement.Payload_defined_cst l -> DefinedCst l
  | _ -> Other

let pp_res out = function
  | Ty _ -> Format.fprintf out "ind_ty"
  | Cstor (_, ity) -> Format.fprintf out "cstor of %a" Ind_ty.pp ity
  | Projector id -> Format.fprintf out "projector_%a" ID.pp id
  | DefinedCst lev -> Format.fprintf out "defined (level %d)" lev
  | Other -> CCFormat.string out "other"

let pp_signature out sigma =
  let pp_pair out (id,ty) =
    Format.fprintf out "(@[%a : %a (%a)@])" ID.pp id Type.pp ty pp_res (classify id)
  in
  Format.fprintf out
    "{@[<hv>%a@]}" (Util.pp_list ~sep:"," pp_pair) (Signature.to_list sigma)

let prec_constr_ a b =
  let to_int_ = function
    | Ty _ -> 0
    | DefinedCst _ -> 5 (* try to make defined smaller, so that constraints are pure *)
    | Projector _ -> 10
    | Cstor _ -> 20
    | Other -> 40
  in
  let c_a = classify a in
  let c_b = classify b in
  match c_a, c_b with
    | Ty _, Ty _
    | Cstor _, Cstor _
    | Projector _, Projector _
    | Other, Other -> 0
    | DefinedCst l1, DefinedCst l2 ->
      (* bigger level means defined later *)
      CCInt.compare l1 l2
    | Ty _, _
    | Cstor _, _
    | Projector _, _
    | DefinedCst _, _
    | Other, _ -> CCInt.compare (to_int_ c_a) (to_int_ c_b)

let prec_constr = Precedence.Constr.make prec_constr_
