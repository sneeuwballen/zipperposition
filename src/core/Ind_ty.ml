
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inductive Types} *)

module T = Term

let section = Util.Section.make "ind_ty"

(** Constructor for an inductive type *)
type constructor = {
  cstor_name: ID.t;
  cstor_ty: Type.t;
  cstor_args: (Type.t * projector) list;
}

(** A projector for a given constructor and argument position *)
and projector = {
  p_id: ID.t;
  p_ty: Type.t;
  p_index: int; (* index of projected argument *)
  p_cstor: constructor lazy_t;
}

(** {6 Inductive Types} *)

(** An inductive type, along with its set of constructors *)
type t = {
  ty_id: ID.t; (* name *)
  ty_vars: Type.t HVar.t list; (* list of variables *)
  ty_pattern: Type.t; (* equal to  [id ty_vars] *)
  ty_constructors : constructor list;
  (* constructors, all returning [pattern] and containing
     no other type variables than [ty_vars] *)
  ty_is_rec: bool lazy_t;
  (* true iff the type is (mutually) recursive *)
  ty_proof: Proof.t;
}

let equal a b = ID.equal a.ty_id b.ty_id

type id_or_tybuiltin =
  | I of ID.t
  | B of Type.builtin

exception InvalidDecl of string

exception NotAnInductiveConstructor of ID.t

exception NotAnInductiveType of ID.t

let () =
  let spf = CCFormat.sprintf in
  Printexc.register_printer
    (function
      | InvalidDecl msg ->
        Some (spf "@[<2>invalid declaration:@ %s@]" msg)
      | NotAnInductiveType id ->
        Some (spf "%a is not an inductive type" ID.pp id)
      | NotAnInductiveConstructor id ->
        Some (spf "%a is not an inductive constructor" ID.pp id)
      | _ -> None)

exception Payload_ind_type of t
exception Payload_ind_cstor of constructor * t
exception Payload_ind_projector of projector

let invalid_decl_ msg = raise (InvalidDecl msg)
let invalid_declf_ fmt = CCFormat.ksprintf fmt ~f:invalid_decl_

let pp out ty =
  let ppvars out =
    function [] -> () | l -> Format.fprintf out " %a" (Util.pp_list HVar.pp) l
  in
  Format.fprintf out "@[%a%a@]" ID.pp ty.ty_id ppvars ty.ty_vars

let type_hd ty =
  let _, _, ret = Type.open_poly_fun ty in
  match Type.view ret with
    | Type.Builtin b -> Some (B b)
    | Type.App (s, _) -> Some (I s)
    | _ -> None

let type_hd_exn ty = match type_hd ty with
  | Some res -> res
  | None ->
    invalid_declf_ "expected function type,@ got `@[%a@]`" Type.pp ty

let as_inductive_ty id =
  ID.payload_find id
    ~f:(function
      | Payload_ind_type ty -> Some ty
      | _ -> None)

let as_inductive_ty_exn id =
  match as_inductive_ty id with
    | Some ty -> ty
    | None -> invalid_declf_ "%a is not an inductive type" ID.pp id

let is_inductive_ty id =
  match as_inductive_ty id with Some _ -> true | None -> false

let is_inductive_type ty =
  match type_hd ty with
    | Some (I id) -> is_inductive_ty id
    | Some (B _)
    | None -> false

let is_inductive_simple_type ty =
  try is_inductive_ty (TypedSTerm.head_exn ty)
  with Not_found -> false

let as_inductive_type ty = match Type.view ty with
  | Type.App (id, l) ->
    begin match as_inductive_ty id with
      | None -> None
      | Some ity -> Some (ity, l)
    end
  | Type.Fun _ | Type.Forall _ | Type.Builtin _ | Type.DB _ | Type.Var _
    -> None

let as_inductive_type_exn ty = as_inductive_type ty |> CCOpt.get_exn

let is_recursive (t:t) =
  let new_ = Lazy.is_val t.ty_is_rec in
  let res = Lazy.force t.ty_is_rec in
  if new_ then (
    Util.debugf ~section 3 "(@[is_recursive@ :ty %a@ :res %B@])"
      (fun k->k pp t res);
  );
  res

let proof (t:t) : Proof.t = t.ty_proof

(* is [top] recursive? *)
let is_rec_ (top:t): bool =
  let rec find_in_ity (seen:t list) (ity:t): bool =
    if CCList.mem ~eq:equal ity seen then false (* loop *)
    else (
      let seen = ity :: seen in
      List.exists
        (fun cstor -> find_in_ty_args seen cstor.cstor_ty)
        ity.ty_constructors
    )
  and find_in_ty_args seen ty = match Type.view ty with
    | Type.Forall ty' -> find_in_ty_args seen ty'
    | Type.Fun (args,_) -> List.exists (find_in_ty seen) args
    | Type.App _ | Type.Builtin _ | Type.Var _ | Type.DB _ -> false
  and find_in_ty (seen:t list) (ty:Type.t) = match Type.view ty with
    | Type.Forall ty' -> find_in_ty seen ty'
    | Type.App (id,l) ->
      ID.equal id top.ty_id || List.exists (find_in_ty seen) l
    | Type.Fun (args,ret) ->
      find_in_ty seen ret || List.exists (find_in_ty seen) args
    | Type.Builtin _ | Type.Var _ | Type.DB _ -> false
  in
  find_in_ity [] top

(* declare that the given type is inductive *)
let declare_ty id ~ty_vars constructors ~proof =
  Util.debugf ~section 1 "declare inductive type %a" (fun k->k ID.pp id);
  if constructors = [] then (
    invalid_declf_ "Ind_types.declare_ty %a: no constructors provided" ID.pp id;
  );
  (* check that [ty] is not declared already *)
  begin match as_inductive_ty id with
    | Some _ -> invalid_declf_ "inductive type %a already declared" ID.pp id;
    | None -> ()
  end;
  let rec ity = {
    ty_id=id;
    ty_vars;
    ty_pattern=Type.app id (List.map Type.var ty_vars);
    ty_constructors=constructors;
    ty_is_rec=lazy (is_rec_ ity);
    ty_proof=proof;
  } in
  (* map the constructors to [ity] too *)
  List.iter
    (fun c ->
       ID.set_payload c.cstor_name (Payload_ind_cstor (c, ity));
       (* declare projectors *)
       List.iter
         (fun (_,p) ->
            ID.set_payload p.p_id (Payload_ind_projector p)
              ~can_erase:(function Payload_ind_projector _ -> true | _ ->false))
         c.cstor_args;
       ()
    )
    constructors;
  (* map [id] to [ity] *)
  ID.set_payload id (Payload_ind_type ity);
  ity

(** {6 Constructors} *)

let mk_constructor id ty args =
  let rec c = lazy (
    let args =
      List.mapi
        (fun i (ty_arg,(p_id,p_ty)) ->
           let p = {p_id; p_ty; p_cstor=c; p_index=i} in
           ty_arg, p)
        args
    in
    { cstor_name=id; cstor_ty=ty; cstor_args=args }
  ) in
  Lazy.force c

let as_constructor id =
  ID.payload_find id
    ~f:(function
      | Payload_ind_cstor (cstor,ity) -> Some (cstor,ity)
      | _ -> None)

let as_constructor_exn id = match as_constructor id with
  | None -> raise (NotAnInductiveConstructor id)
  | Some x -> x

let is_constructor s =
  match as_constructor s with Some _ -> true | None -> false

let contains_inductive_types t =
  T.Seq.subterms t
  |> Sequence.exists (fun t -> is_inductive_type (T.ty t))

(** {6 Projectors} *)

let projector_id p = p.p_id
let projector_ty p = p.p_ty
let projector_idx p = p.p_index
let projector_cstor p = Lazy.force p.p_cstor

let as_projector id =
  ID.payload_find id
    ~f:(function
      | Payload_ind_projector p -> Some p
      | _ -> None)
