(** {1 Inductive Constants and Cases}

    Skolem constants of an inductive type, coversets, etc. required for
    inductive reasoning. *)

open Logtk
module T = Term

exception InvalidDecl of string
exception NotAnInductiveConstant of Name.t

let () =
  let spf = CCFormat.sprintf in
  Printexc.register_printer (function
    | InvalidDecl msg -> Some (spf "@[<2>invalid declaration:@ %s@]" msg)
    | NotAnInductiveConstant id ->
      Some (spf "%a is not an inductive constant" Name.pp id)
    | _ -> None)

let invalid_decl m = raise (InvalidDecl m)
let invalid_declf m = CCFormat.ksprintf m ~f:invalid_decl

type t = {
  cst_id: Name.t;
  cst_args: Type.t list;
  cst_ty: Type.t; (* [cst_ty = cst_id cst_args] *)
  cst_ity: Ind_ty.t; (* the corresponding inductive type *)
  cst_is_sub: bool; (* sub-constant? *)
  cst_depth: int; (* how many induction lead to this one? *)
}
[@@warning "-69"]

type state = {
  mutable counter: int;
  payload: t Name.Tbl.t;
}

let k_state : state Flex_state.key = Flex_state.create_key ()

let init flex_state =
  match Flex_state.get k_state flex_state with
  | Some _ -> flex_state
  | None ->
    Flex_state.add k_state
      { counter = 0; payload = Name.Tbl.create 128 }
      flex_state

let get_state flex_state =
  match Flex_state.get k_state flex_state with
  | Some st -> st
  | None -> invalid_arg "Ind_cst state is not initialized"

(** {5 Inductive Constants} *)

let to_term c = Term.const ~ty:c.cst_ty c.cst_id
let id c = c.cst_id
let ty c = c.cst_ty
let equal a b = Name.equal a.cst_id b.cst_id
let compare a b = Name.compare a.cst_id b.cst_id
let hash a = Name.hash a.cst_id

module Cst_set = CCSet.Make (struct
  type t_ = t
  type t = t_

  let compare = compare
end)

let depth c = c.cst_depth
let same_type c1 c2 = Type.equal c1.cst_ty c2.cst_ty
let pp out c = Name.pp out c.cst_id
let id_as_cst st id = Name.Tbl.find_opt st.payload id

let id_as_cst_exn st id =
  match id_as_cst st id with
  | None -> raise (NotAnInductiveConstant id)
  | Some c -> c

let id_is_cst st id =
  match id_as_cst st id with
  | Some _ -> true
  | _ -> false

let is_sub c = c.cst_is_sub
let id_is_sub st id = id_as_cst st id |> CCOpt.map_or ~default:false is_sub

(** {5 Creation of Coverset and Cst} *)

let make_skolem st ty : Name.t =
  let c = Name.makef "#%s_%d" (Type.mangle ty) st.counter in
  st.counter <- st.counter + 1;
  (* declare as a skolem *)
  let k =
    if Ind_ty.is_inductive_type ty then
      Name.K_ind
    else
      Name.K_normal
  in
  Name_payload.add c (Name.Attr_skolem k);
  c

(* declare new constant *)
let declare st ~depth ~is_sub id ty =
  Util.debugf ~section:Ind_ty.section 2
    "@[<2>declare new inductive symbol@ `@[%a : %a@]`@ :depth %d :is_sub %B@]"
    (fun k -> k Name.pp id Type.pp ty depth is_sub);
  assert (not (id_is_cst st id));
  assert (Type.is_ground ty);
  (* constant --> not polymorphic *)
  let ity, args =
    match Ind_ty.as_inductive_type ty with
    | Some (t, l) -> t, l
    | None -> invalid_declf "cannot declare a constant of type %a" Type.pp ty
  in
  (* build coverset and constant, mutually recursive *)
  let cst =
    {
      cst_id = id;
      cst_ty = ty;
      cst_depth = depth;
      cst_ity = ity;
      cst_args = args;
      cst_is_sub = is_sub;
    }
  in
  Name.Tbl.replace st.payload id cst;
  cst

let make st ?(depth = 0) ~is_sub (ty : Type.t) : t =
  let id = make_skolem st ty in
  declare st ~depth ~is_sub id ty

let dominates (c1 : t) (c2 : t) : bool = c1.cst_depth < c2.cst_depth

(** {2 Inductive Skolems} *)

type ind_skolem = Name.t * Type.t

let ind_skolem_compare = CCOrd.pair Name.compare Type.compare
let ind_skolem_equal a b = ind_skolem_compare a b = 0

let id_is_ind_skolem st (id : Name.t) (ty : Type.t) : bool =
  let n_tyvars, ty_args, ty_ret = Type.open_poly_fun ty in
  n_tyvars = 0
  && ty_args = [] (* constant *)
  && Ind_ty.is_inductive_type ty_ret
  && Ind_ty.is_recursive (Ind_ty.as_inductive_type_exn ty_ret |> fst)
  && Type.is_ground ty
  && (id_is_cst st id
     || ((not (Ind_ty.is_constructor id)) && not (Rewrite.is_defined_cst id)))

let ind_skolem_depth st (id : Name.t) : int =
  match id_as_cst st id with
  | None -> 0
  | Some c -> depth c

(* find inductive constant candidates in terms *)
let find_ind_skolems st t : ind_skolem Iter.t =
  T.Seq.subterms t
  |> Iter.filter_map (fun t ->
      match T.view t with
      | T.Const id ->
        let ty = T.ty t in
        if id_is_ind_skolem st id ty then (
          let n_tyvars, ty_args, _ = Type.open_poly_fun ty in
          assert (n_tyvars = 0 && ty_args = []);
          Some (id, ty)
        ) else
          None
      | _ -> None)
