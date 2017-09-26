
(** {1 Inductive Constants and Cases}

    Skolem constants of an inductive type, coversets, etc. required for
    inductive reasoning. *)

open Logtk

module T = Term

exception InvalidDecl of string
exception NotAnInductiveConstant of ID.t

let () =
  let spf = CCFormat.sprintf in
  Printexc.register_printer
    (function
      | InvalidDecl msg ->
        Some (spf "@[<2>invalid declaration:@ %s@]" msg)
      | NotAnInductiveConstant id ->
        Some (spf "%a is not an inductive constant" ID.pp id)
      | _ -> None)

let invalid_decl m = raise (InvalidDecl m)
let invalid_declf m = CCFormat.ksprintf m ~f:invalid_decl

type t = {
  cst_id: ID.t;
  cst_args: Type.t list;
  cst_ty: Type.t; (* [cst_ty = cst_id cst_args] *)
  cst_ity: Ind_ty.t; (* the corresponding inductive type *)
  cst_is_sub: bool; (* sub-constant? *)
  cst_depth: int; (* how many induction lead to this one? *)
}

exception Payload_cst of t

(** {6 Inductive Constants} *)

let to_term c = Term.const ~ty:c.cst_ty c.cst_id
let id c = c.cst_id
let ty c = c.cst_ty

let equal a b = ID.equal a.cst_id b.cst_id
let compare a b = ID.compare a.cst_id b.cst_id
let hash a = ID.hash a.cst_id

module Cst_set = CCSet.Make(struct
    type t_ = t
    type t = t_
    let compare = compare
  end)

let depth c = c.cst_depth

let same_type c1 c2 = Type.equal c1.cst_ty c2.cst_ty

let pp out c = ID.pp out c.cst_id

let on_new_cst = Signal.create()

let id_as_cst id =
  ID.payload_find id
    ~f:(function
      | Payload_cst c -> Some c
      | _ -> None)

let id_as_cst_exn id = match id_as_cst id with
  | None -> raise (NotAnInductiveConstant id)
  | Some c -> c

let id_is_cst id = match id_as_cst id with Some _ -> true | _ -> false

let is_sub c = c.cst_is_sub

let id_is_sub id = id_as_cst id |> CCOpt.map_or ~default:false is_sub

(** {6 Creation of Coverset and Cst} *)

let n_ = ref 0

let make_skolem ty : ID.t =
  let c = ID.makef "#%s_%d" (Type.mangle ty) !n_ in
  incr n_;
  (* declare as a skolem *)
  let k = if Ind_ty.is_inductive_type ty then ID.K_ind else ID.K_normal in
  let _, args, _ = Type.open_poly_fun ty in
  ID.set_payload c (ID.Attr_skolem (k, List.length args));
  c

(* declare new constant *)
let declare ~depth ~is_sub id ty =
  Util.debugf ~section:Ind_ty.section 2
    "@[<2>declare new inductive symbol@ `@[%a : %a@]`@ :depth %d :is_sub %B@]"
    (fun k->k ID.pp id Type.pp ty depth is_sub);
  assert (not (id_is_cst id));
  assert (Type.is_ground ty); (* constant --> not polymorphic *)
  let ity, args = match Ind_ty.as_inductive_type ty with
    | Some (t,l) -> t,l
    | None -> invalid_declf "cannot declare a constant of type %a" Type.pp ty
  in
  (* build coverset and constant, mutually recursive *)
  let cst = {
    cst_id=id;
    cst_ty=ty;
    cst_depth=depth;
    cst_ity=ity;
    cst_args=args;
    cst_is_sub=is_sub;
  }
  in
  ID.set_payload id (Payload_cst cst)
    ~can_erase:(function
      | ID.Attr_skolem (ID.K_ind, _) ->
        true (* special case: promotion from skolem to inductive const *)
      | _ -> false);
  (* return *)
  Signal.send on_new_cst cst;
  cst

let make ?(depth=0) ~is_sub (ty:Type.t): t =
  let id = make_skolem ty in
  declare ~depth ~is_sub id ty

let dominates (c1:t)(c2:t): bool =
  c1.cst_depth < c2.cst_depth

(** {2 Inductive Skolems} *)

type ind_skolem = ID.t * Type.t

let ind_skolem_compare = CCOrd.pair ID.compare Type.compare
let ind_skolem_equal a b = ind_skolem_compare a b = 0

let id_is_ind_skolem (id:ID.t) (ty:Type.t): bool =
  let n_tyvars, ty_args, ty_ret = Type.open_poly_fun ty in
  n_tyvars=0
  && ty_args=[] (* constant *)
  && Ind_ty.is_inductive_type ty_ret
  && Ind_ty.is_recursive (Ind_ty.as_inductive_type_exn ty_ret |> fst)
  && Type.is_ground ty
  && (id_is_cst id || (not (Ind_ty.is_constructor id) && not (Rewrite.is_defined_cst id)))

let ind_skolem_depth (id:ID.t): int = match id_as_cst id with
  | None -> 0
  | Some c -> depth c

(* find inductive constant candidates in terms *)
let find_ind_skolems t : ind_skolem Sequence.t =
  T.Seq.subterms t
  |> Sequence.filter_map
    (fun t -> match T.view t with
       | T.Const id ->
         let ty = T.ty t in
         if id_is_ind_skolem id ty
         then (
           let n_tyvars, ty_args, _ = Type.open_poly_fun ty in
           assert (n_tyvars=0 && ty_args=[]);
           Some (id, ty)
         ) else None
       | _ -> None)
