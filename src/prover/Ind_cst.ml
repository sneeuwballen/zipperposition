
(** {1 Inductive Constants and Cases}

    Skolem constants of an inductive type, coversets, etc. required for
    inductive reasoning. *)

open Libzipperposition

module T = FOTerm

exception InvalidDecl of string
exception AlreadyDeclaredConstant of ID.t
exception NotAnInductiveConstant of ID.t
exception NotAnInductiveSubConstant of ID.t

let () =
  let spf = CCFormat.sprintf in
  Printexc.register_printer
  (function
    | InvalidDecl msg ->
        Some (spf "@[<2>invalid declaration:@ %s@]" msg)
    | AlreadyDeclaredConstant id ->
        Some (spf "%a already declared as an inductive constant" ID.pp id)
    | NotAnInductiveConstant id ->
        Some (spf "%a is not an inductive constant" ID.pp id)
    | NotAnInductiveSubConstant id ->
        Some (spf "%a is not an inductive sub-constant" ID.pp id)
    | _ -> None)

let invalid_decl m = raise (InvalidDecl m)
let invalid_declf m = CCFormat.ksprintf m ~f:invalid_decl

(** {6 Inductive Case} *)

type case = {
  case_term : FOTerm.t;
  case_kind: [`Base | `Rec];
  case_sub: ID.Set.t; (* set of sub-constants *)
}

type cover_set = case list

let case_equal a b = FOTerm.equal a.case_term b.case_term
let case_compare a b = FOTerm.compare a.case_term b.case_term
let case_hash a = FOTerm.hash a.case_term

let pp_case out c = CCFormat.hovbox FOTerm.pp out c.case_term

let case_is_rec c = c.case_kind = `Rec
let case_is_base c = c.case_kind = `Base

(** {6 Inductive Constants} *)

type cst = {
  cst_id: ID.t;
  cst_ty: Type.t;
  cst_ity: Ind_ty.t; (* the corresponding inductive type *)
  cst_coverset: cover_set; (* the coverset for this constant *)
}

exception Payload_cst of cst

let cst_to_term c = FOTerm.const ~ty:c.cst_ty c.cst_id

let cst_equal a b = ID.equal a.cst_id b.cst_id
let cst_compare a b = ID.compare a.cst_id b.cst_id
let cst_hash a = ID.hash a.cst_id

let pp_cst out c = ID.pp out c.cst_id

let on_new_cst = Signal.create()

let as_cst id =
  CCList.find
    (function
      | Payload_cst c -> Some c
      | _ -> None)
    (ID.payload id)

let as_cst_exn id = match as_cst id with
  | None -> raise (NotAnInductiveConstant id)
  | Some c -> c

let is_cst id = match as_cst id with Some _ -> true | _ -> false

let cover_set c = c.cst_coverset

let cases ?(which=`All) set =
  let seq = Sequence.of_list set in
  match which with
  | `All -> seq
  | `Base -> Sequence.filter case_is_base seq
  | `Rec -> Sequence.filter case_is_rec seq

(** {6 Sub-Constants} *)

type sub_cst = {
  sub_cst_id: ID.t;
  sub_cst_ty: Type.t;
  sub_cst_case: case;
  sub_cst_cst: cst;
}

exception Payload_sub_cst of sub_cst

let as_sub_cst id =
  CCList.find
    (function
      | Payload_sub_cst sub -> Some sub
      | _ -> None)
    (ID.payload id)

let as_sub_cst_exn id = match as_sub_cst id with
  | None -> raise (NotAnInductiveSubConstant id)
  | Some x -> x

let case_sub c =
  ID.Set.to_seq c.case_sub
  |> Sequence.map as_sub_cst_exn

let is_sub_cst id = match as_sub_cst id with Some _ -> true | _ -> false

let is_sub_cst_of id cst =
  match as_sub_cst id with
  | None -> false
  | Some sub -> cst_equal sub.sub_cst_cst cst

let as_sub_cst_of id cst =
  match as_sub_cst id with
  | Some sub when cst_equal sub.sub_cst_cst cst -> Some sub
  | _ -> None

let inductive_cst_of_sub_cst sub =
  sub.sub_cst_cst, sub.sub_cst_case

let sub_constants_case c =
  ID.Set.to_seq c.case_sub
  |> Sequence.map as_sub_cst_exn

let sub_constants set =
  Sequence.of_list set
  |> Sequence.flat_map sub_constants_case

let term_of_sub_cst s = T.const ~ty:s.sub_cst_ty s.sub_cst_id

let rec dominates c sub =
  Sequence.exists
    (fun sub_c ->
       ID.equal sub_c.sub_cst_id sub.sub_cst_id
       ||
       match as_cst sub_c.sub_cst_id with
         | None -> false
         | Some c' -> dominates c' sub)
    (sub_constants c.cst_coverset)

(* either:
   - id is not a sub-constant, or
   - id is a sub-constant, but with a type different from its parent *)
let sub_cst_criterion id =
  match as_sub_cst id with
    | None -> true
    | Some sub ->
      not (Type.equal sub.sub_cst_ty (T.ty sub.sub_cst_case.case_term))

(* find new inductive constant candidates in terms *)
let find_cst_in_term t =
  T.Seq.subterms t
  |> Sequence.filter_map
    (fun t -> match T.view t with
      | T.Const id ->
          let n_tyvars, ty_args, ty_ret = Type.open_poly_fun (T.ty t) in
          (* must be a constant *)
          if n_tyvars=0 && ty_args=[]
          then match Ind_ty.as_inductive_type ty_ret with
            | Some ity ->
              if not (is_cst id)
              && not (Ind_ty.is_constructor id)
              && sub_cst_criterion id
              && Skolem.is_skolem id
                then Some (id, ity, ty_ret) (* bingo *)
                else None
            | _ -> None
          else None
      | _ -> None)

(** {6 Creation of Coverset and Cst} *)

type id_or_ty_builtin =
  | I of ID.t
  | B of Type.builtin

let type_hd_exn ty =
  let _, ret = Type.open_fun ty in
  match Type.view ret with
  | Type.Builtin b -> B b
  | Type.App (s, _) -> I s
  | _ ->
      invalid_declf "expected function type, got %a" Type.pp ty

(* type declarations required by [c] *)
let declarations_of_cst c =
  Sequence.of_list c.cst_coverset
  |> Sequence.flat_map
    (fun c -> ID.Set.to_seq c.case_sub)
  |> Sequence.map
    (fun id ->
      let sub = as_sub_cst_exn id in
      id, sub.sub_cst_ty)

module CoversetState = struct
  (* state for creating coverset *)
  type t = {
    sub_csts: ID.Set.t; (* raw set of constants *)
    to_declare: (ID.t * Type.t) list;
      (* those sub-constants need to be declared *)
  }

  let empty = {
    sub_csts=ID.Set.empty;
    to_declare=[];
  }

  (* state monad *)
  type 'a m = t -> t * 'a

  (* state monad inside a backtracking monad *)
  type 'a mm = t -> (t * 'a) list
  let fail : _ mm = fun _ -> []
  let return : 'a -> 'a m = fun x st -> st, x
  let yield : 'a -> 'a mm = fun x st -> [st, x]
  let yield_l : 'a list -> 'a mm = fun l st -> List.map (fun x -> st,x) l
  let (>>=) : 'a mm -> ('a -> 'b m) -> 'b mm
  = fun x_mm f st ->
    let xs = x_mm st in
    List.map (fun (st,x) -> f x st) xs
  let (>>>=) : 'a mm -> ('a -> 'b mm) -> 'b mm
  = fun x_mm f st ->
    let xs = x_mm st in
    CCList.flat_map
      (fun (st,x) -> f x st)
      xs
  let (>>|=) : 'a mm -> ('a -> 'b) -> 'b mm
  = fun x_mm f st ->
    let xs = x_mm st in
    List.map (fun (st,x) -> st, f x) xs
  let get : t mm = fun st -> [st, st]
  let set : t -> unit m = fun st _ -> st, ()

  (* modify the state: add [c] to the set of cases *)
  let add_sub_case c ty : unit mm =
    get >>= fun st ->
    let st = {
      sub_csts=ID.Set.add c st.sub_csts;
      to_declare=(c,ty) :: st.to_declare;
    } in
    set st

  let rec map_l : ('a -> 'b mm) -> 'a list -> 'b list mm
  = fun f l -> match l with
    | [] -> yield []
    | x :: tl ->
        f x >>>= fun x' ->
        map_l f tl >>>= fun tl' ->
        yield (x'::tl')

  let run : 'a mm -> t -> 'a list
  = fun m st -> List.map snd (m st)
end

let n_ = ref 0

let mk_skolem_ pp x =
  let name = CCFormat.sprintf "#%a_%d" pp x !n_ in
  incr n_;
  let c = ID.make name in
  ID.add_payload c Skolem.Attr_skolem;
  c

(* coverset of given depth for this type under given substitution of
   ity.ty_vars *)
let make_coverset_ ~depth:initial_depth ~subst ity : (case * (ID.t * Type.t) list) list =
  let open CoversetState in
  (* list of generators of:
      - member of the coverset (one of the t such that cst=t)
      - set of sub-constants of this term *)
  let rec make depth : T.t mm =
    (* leaves: fresh constants *)
    if depth=0
    then (
      let c = mk_skolem_ ID.pp ity.Ind_ty.ty_id in
      let ty = Substs.Ty.apply_no_renaming subst (ity.Ind_ty.ty_pattern,0) in
      let t = T.const ~ty c in
      add_sub_case c ty >>= fun () ->
      return t
    )
    (* inner nodes or base cases: constructors *)
    else (
      yield_l ity.Ind_ty.ty_constructors
      >>>= fun cstor ->
      let f = cstor.Ind_ty.cstor_name in
      let ty_f = cstor.Ind_ty.cstor_ty in
      (* apply to ground type parameters *)
      let ty_params =
        List.map
          (fun v ->
             let v = Type.var v in
             Substs.Ty.apply_no_renaming subst (v,0))
          ity.Ind_ty.ty_vars
      in
      let ty_f_applied = Type.apply ty_f ty_params in
      let ty_params = List.map T.of_ty ty_params in
      let n_ty_params, ty_args_f, _ = Type.open_poly_fun ty_f_applied in
      assert (n_ty_params=0);
      if ty_args_f=[]
      then
        if depth > 0
        then yield (T.app (T.const ~ty:ty_f f) ty_params)  (* only one answer : f *)
        else fail
      else (
        (* make fresh type variables and apply *)
        map_l (make_of_ty (depth-1)) ty_args_f
        >>|= fun args ->
        T.app (T.const ~ty:ty_f f) (ty_params @ args)
      )
    )
  (* return a new term of type [ty] *)
  and make_of_ty depth ty : T.t mm =
    if Unif.Ty.matches ~pattern:ity.Ind_ty.ty_pattern ty
    then make depth (* previous case *)
    else (
      (* not an inductive sub-case, just create a skolem symbol *)
      let c = match type_hd_exn ty with
        | B b -> mk_skolem_ Type.pp_builtin b
        | I id -> mk_skolem_ ID.pp id
      in
      let t = T.const ~ty c in
      add_sub_case c ty >>|= fun () -> t
    )
  in
  (* build the toplevel values, along with a list of sub-constants
     to declare *)
  let make_top =
    let l = make initial_depth in
    l >>>= fun t ->
    (* obtain the current set of sub-constants *)
    get >>>= fun state ->
    let case = {
      case_term=t;
      case_kind=if ID.Set.is_empty state.sub_csts then `Base else `Rec;
      case_sub=state.sub_csts;
    } in
    yield (case, state.to_declare)
  in
  run make_top empty

let declare_cst ?(cover_set_depth=1) id ~ty =
  if is_cst id then raise (AlreadyDeclaredConstant id);
  assert (Type.is_ground ty); (* constant --> not polymorphic *)
  let ity = match type_hd_exn ty with
    | B b -> invalid_declf "cannot declare a constant of type %a" Type.pp_builtin b
    | I id -> Ind_ty.as_inductive_ty_exn id
  in
  (* map variables from [ity] to this concrete type *)
  let subst = Unif.Ty.matching_same_scope ~pattern:ity.Ind_ty.ty_pattern ty ~scope:0 in
  let l = make_coverset_ ~depth:cover_set_depth ~subst ity in
  let cover_set = List.map fst l in
  Util.debugf ~section:Ind_ty.section 2
    "@[<2>declare new inductive symbol `@[%a : %a@]`@ with coverset {@[%a@]}@]"
    (fun k->k ID.pp id Type.pp ty (Util.pp_list pp_case) cover_set);
  (* build the constant itself *)
  let cst = {
    cst_id=id;
    cst_ty=ty;
    cst_ity=ity;
    cst_coverset=cover_set;
  } in
  ID.add_payload id (Payload_cst cst);
  (* declare the sub-constants *)
  List.iter
    (fun (case,to_declare) ->
      List.iter
        (fun (id,ty) ->
          ID.add_payload id
            (Payload_sub_cst {
              sub_cst_id=id;
              sub_cst_ty=ty;
              sub_cst_case=case;
              sub_cst_cst=cst;
            }))
        to_declare)
    l;
  Util.debugf ~section:Ind_ty.section 5
    "@[<2>sub-constants:@ @[<v>%a@]@]"
    (fun k ->
       let pp_case out case =
         Format.fprintf out "@[<h>case %a: sub {@[<hv>%a@]}@]"
            pp_case case (Util.pp_list ID.pp)
            (sub_constants_case case
             |> Sequence.map (fun c -> c.sub_cst_id) |> Sequence.to_list)
       in
       k (CCFormat.seq pp_case) (cases cover_set));
  (* return *)
  Signal.send on_new_cst cst;
  cst
