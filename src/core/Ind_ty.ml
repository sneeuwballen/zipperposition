
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inductive Types} *)

module T = FOTerm
module Stmt = Statement

let section = Util.Section.(make ~parent:zip "ind")

type constructor = {
  cstor_name: ID.t;
  cstor_ty: Type.t;
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
}

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

let invalid_decl_ msg = raise (InvalidDecl msg)
let invalid_declf_ fmt = CCFormat.ksprintf fmt ~f:invalid_decl_

let type_hd_exn ty =
  let _, ret = Type.open_fun ty in
  match Type.view ret with
  | Type.Builtin b -> B b
  | Type.App (s, _) -> I s
  | _ ->
      invalid_declf_ "expected function type,@ got `@[%a@]`" Type.pp ty

let as_inductive_ty id =
  CCList.find
    (function
      | Payload_ind_type ty -> Some ty
      | _ -> None)
    (ID.payload id)

let as_inductive_ty_exn id =
  match as_inductive_ty id with
  | Some ty -> ty
  | None -> invalid_declf_ "%a is not an inductive type" ID.pp id

let is_inductive_ty id =
  match as_inductive_ty id with Some _ -> true | None -> false

let is_inductive_type ty =
  match type_hd_exn ty with
  | B _ -> false
  | I id -> is_inductive_ty id

let as_inductive_type ty =
  match type_hd_exn ty with
  | B _ -> None
  | I id -> as_inductive_ty id

(* declare that the given type is inductive *)
let declare_ty id ~ty_vars constructors =
  Util.debugf ~section 1 "declare inductive type %a" (fun k->k ID.pp id);
  if constructors = []
  then invalid_declf_ "Ind_types.declare_ty %a: no constructors provided" ID.pp id;
  (* check that [ty] is not declared already *)
  List.iter
    (function
      | Payload_ind_type _ -> invalid_declf_ "inductive type %a already declared" ID.pp id;
      | _ -> ())
    (ID.payload id);
  let ity = {
    ty_id=id;
    ty_vars;
    ty_pattern=Type.app id (List.map Type.var ty_vars);
    ty_constructors=constructors;
  } in
  (* map the constructors to [ity] too *)
  List.iter
    (fun c ->
      ID.add_payload c.cstor_name (Payload_ind_cstor (c, ity)))
    constructors;
  (* map [id] to [ity] *)
  ID.add_payload id (Payload_ind_type ity);
  ity

let declare_stmt st = match Stmt.view st with
  | Stmt.Data l ->
      List.iter
        (fun d ->
          let ty_vars =
            List.mapi (fun i v -> HVar.make ~ty:(Var.ty v) i) d.Stmt.data_args
          and cstors =
            List.map (fun (c,ty) -> {cstor_name=c; cstor_ty=ty;}) d.Stmt.data_cstors
          in
          let _ = declare_ty d.Stmt.data_id ~ty_vars cstors in
          ())
        l
  | _ -> ()

(** {6 Constructors} *)

let as_constructor id =
  CCList.find
    (function
      | Payload_ind_cstor (cstor,ity) -> Some (cstor,ity)
      | _ -> None)
    (ID.payload id)

let as_constructor_exn id = match as_constructor id with
  | None -> raise (NotAnInductiveConstructor id)
  | Some x -> x

let is_constructor s =
  match as_constructor s with Some _ -> true | None -> false

let contains_inductive_types t =
  T.Seq.subterms t
  |> Sequence.exists (fun t -> is_inductive_type (T.ty t))
