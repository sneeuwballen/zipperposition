
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Classification of Constants} *)

open Logtk

type res =
  | Ty of Ind_ty.t
  | Cstor of Ind_ty.constructor * Ind_ty.t
  | Inductive_cst of Ind_cst.t option
  | Projector of ID.t (** projector of some constructor (id: type) *)
  | DefinedCst of int * Statement.definition
  | Parameter of int
  | Skolem
  | Other

let classify id =
  let rec aux = function
    | [] -> Other
    | p :: tail ->
      begin match p id with
        | None -> aux tail
        | Some x -> x
      end
  in
  let (|>>) p f id = match p id with | None -> None | Some x -> Some (f x) in
  aux
    [ (Ind_ty.as_constructor |>> fun (c,t) -> Cstor (c,t));
      (Ind_ty.as_inductive_ty |>> fun x -> Ty x);
      (ID.as_parameter |>> fun x->Parameter x);
      (Ind_cst.id_as_cst |>> fun c -> Inductive_cst (Some c));
      (fun id ->
         let open CCOpt.Infix in
         ID.as_skolem id >>= function
         | ID.K_ind -> Some (Inductive_cst None)
         | ID.K_normal -> Some Skolem);
      (Ind_ty.as_projector |>> fun p -> Projector (Ind_ty.projector_id p));
      (Rewrite.as_defined_cst |>> fun cst ->
       DefinedCst (Rewrite.Defined_cst.level cst, Rewrite.Defined_cst.rules cst));
    ]

let id_is_cstor id = match classify id with Cstor _ -> true | _ -> false
let id_is_projector id = match classify id with Projector _ -> true | _ -> false
let id_is_defined id = match classify id with DefinedCst _ -> true | _ -> false

let pp_res out = function
  | Ty _ -> Format.fprintf out "ind_ty"
  | Cstor (_, ity) -> Format.fprintf out "cstor of %a" Ind_ty.pp ity
  | Inductive_cst _ -> Format.fprintf out "ind_cst"
  | Projector id -> Format.fprintf out "projector_%a" ID.pp id
  | DefinedCst (lev,_) -> Format.fprintf out "defined (level %d)" lev
  | Parameter i -> Format.fprintf out "parameter %d" i
  | Skolem -> CCFormat.string out "skolem"
  | Other -> CCFormat.string out "other"

let pp_signature out sigma =
  let pp_pair out (id,ty) =
    Format.fprintf out "(@[%a : %a (%a)@])" ID.pp id Type.pp ty pp_res (classify id)
  in
  Format.fprintf out
    "{@[<hv>%a@]}" (Util.pp_list ~sep:"," pp_pair) (Signature.to_list sigma)

let dominates_ opt_c opt_sub =
  CCOpt.(get_or ~default:false (map2 Ind_cst.dominates opt_c opt_sub))

let prec_constr_ a b =
  let to_int_ = function
    | Ty _ -> 0
    | Projector _ -> 1
    | Cstor _ -> 2
    | Parameter _ -> 3
    | Inductive_cst _ -> 4
    | Skolem
    | Other -> 10 (* skolem and other constants, at the same level *)
    | DefinedCst _ -> 20 (* defined: biggest *)
  in
  let c_a = classify a in
  let c_b = classify b in
  match c_a, c_b with
    | Ty _, Ty _
    | Cstor _, Cstor _
    | Projector _, Projector _
    | Skolem, Skolem
    | Other, Other -> 0
    | Parameter i, Parameter j -> CCOrd.int i j (* by mere index *)
    | Inductive_cst c1, Inductive_cst c2 ->
      (* Inductive_cst cases should be compared by "sub-case" order (i.e. `x
         sub-cst-of y` means `x < y`); this is a stable ordering. *)
      if dominates_ c1 c2 then 1
      else if dominates_ c2 c1 then -1
      else 0
    | DefinedCst (l1,_), DefinedCst (l2,_) ->
      (* bigger level means defined later *)
      CCInt.compare l1 l2
    | Ty _, _
    | Cstor _, _
    | Inductive_cst _, _
    | Parameter _, _
    | Projector _, _
    | DefinedCst _, _
    | Skolem, _
    | Other, _
      -> CCInt.compare (to_int_ c_a) (to_int_ c_b)

let prec_constr = Precedence.Constr.make prec_constr_

let weight_fun (id:ID.t): Precedence.Weight.t =
  let module W = Precedence.Weight in
  begin match classify id with
    | Ty _ -> W.int 1
    | Projector _ -> W.int 1
    | Parameter _ -> W.int 1
    | Cstor _ -> W.int 1
    | Inductive_cst _ -> W.int 2
    | Skolem
    | Other -> W.omega_plus 4
    | DefinedCst _ -> W.omega_plus 5 (* defined: biggest *)
  end
