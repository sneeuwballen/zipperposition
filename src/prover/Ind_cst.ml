
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

let pp_case out c = FOTerm.pp out c.case_term

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

(* find inductive constants in terms *)
let find_cst_in_term t =
  T.Seq.subterms t
  |> Sequence.filter_map
    (fun t -> match T.view t with
      | T.Const id ->
          let ty_args, ty_ret = Type.open_fun (T.ty t) in
          (* must be a constant *)
          if ty_args=[]
          then match Ind_ty.as_inductive_type ty_ret with
            | Some ity ->
              if not (is_cst id)
              && not (Ind_ty.is_constructor id)
              && Skolem.is_skolem id
                then Some (id, ity, ty_ret) (* bingo *)
                else None
            | _ -> None
          else None
      | _ -> None)

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

  let return : 'a -> 'a m = fun x st -> st, x
  let (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  = fun x f st ->
    let st, x' = x st in
    f x' st
  let get : t m = fun st -> st, st
  let set : t -> unit m = fun st _ -> st, ()

  (* modify the state: add [c] to the set of cases *)
  let add_sub_case c ty : unit m =
    get >>= fun st ->
    let st = {
      sub_csts=ID.Set.add c st.sub_csts;
      to_declare=(c,ty) :: st.to_declare;
    } in
    set st

  (* backtracking monad inside the state monad *)
  type 'a mm = 'a list m
  let fail : _ mm = fun st -> st, []
  let yield : 'a -> 'a mm = fun x st -> st, [x]
  let (>>>=) : 'a mm -> ('a -> 'b mm) -> 'b mm
  = fun x f st ->
    let st, xs = x st in
    CCList.fold_flat_map (fun st x -> f x st) st xs
  let (>>|=) : 'a mm -> ('a -> 'b) -> 'b mm
  = fun x f st ->
    let st, xs = x st in
    st, List.map f xs

  let rec map_l : ('a -> 'b mm) -> 'a list -> 'b list mm
  = fun f l -> match l with
    | [] -> yield []
    | x :: tl ->
        f x >>>= fun x' ->
        map_l f tl >>>= fun tl' ->
        yield (x'::tl')
end

let n_ = ref 0

let mk_skolem_ pp x =
  let name = CCFormat.sprintf "#%a_%d" pp x !n_ in
  incr n_;
  let c = ID.make name in
  ID.add_payload c Skolem.Attr_skolem;
  c

(* coverset of given depth for this type and constant *)
let make_coverset_ ~depth:initial_depth ity : (case * (ID.t * Type.t) list) list =
  let open CoversetState in
  (* list of generators of:
      - member of the coverset (one of the t such that cst=t)
      - set of sub-constants of this term *)
  let rec make depth : T.t mm =
    (* leaves: fresh constants *)
    if depth=0
    then (
      let c = mk_skolem_ ID.pp ity.Ind_ty.ty_id in
      (* FIXME: what if there are type parameters?! *)
      let ty = ity.Ind_ty.ty_pattern  in
      let t = T.const ~ty c in
      add_sub_case c ty >>= fun () ->
      yield t
    )
    (* inner nodes or base cases: constructors *)
    else (
      return ity.Ind_ty.ty_constructors
      >>>= fun cstor ->
      let f = cstor.Ind_ty.cstor_name in
      let ty_f = cstor.Ind_ty.cstor_ty in
      match Type.arity ty_f with
       | Type.NoArity ->
           invalid_declf "invalid constructor %a for inductive type %a"
             ID.pp f Type.pp ity.Ind_ty.ty_pattern
       | Type.Arity (0, 0) ->
           if depth > 0
           then yield (T.const ~ty:ty_f f)  (* only one answer : f *)
           else fail
       | Type.Arity (0, _) ->
           let ty_args = Type.expected_args ty_f in
           map_l (make_of_ty (depth-1)) ty_args
           >>|= fun args ->
           T.app (T.const ~ty:ty_f f) args
       | Type.Arity (m,_) ->
           invalid_declf
             "inductive constructor %a requires %d type \
               parameters, expected 0"
             ID.pp f m
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
      add_sub_case c ty >>= fun () ->
      yield t
    )
  in
  (* build the toplevel values, along with a list of sub-constants
     to declare *)
  let make_top =
    let l = make initial_depth in
    l >>>= fun t ->
    (* obtain the current set of sub-constants, and reset it *)
    get >>= fun state ->
    set empty >>= fun () ->
    let case = {
      case_term=t;
      case_kind=if ID.Set.is_empty state.sub_csts then `Base else `Rec;
      case_sub=state.sub_csts;
    } in
    yield (case, state.to_declare)
  in
  let _st, l = make_top empty in
  l

let declare_cst ?(cover_set_depth=1) id ~ty =
  if is_cst id then raise (AlreadyDeclaredConstant id);
  let ity = match type_hd_exn ty with
    | B b -> invalid_declf "cannot declare a constant of type %a" Type.pp_builtin b
    | I id -> Ind_ty.as_inductive_ty_exn id
  in
  let l = make_coverset_ ~depth:cover_set_depth ity in
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
         Format.fprintf out "@[<h>case %a: sub @[<hv>%a@]@]@."
            pp_case case (Util.pp_list ID.pp)
            (sub_constants_case case
             |> Sequence.map (fun c -> c.sub_cst_id) |> Sequence.to_list)
       in
       k (CCFormat.seq pp_case) (cases cover_set));
  (* return *)
  Signal.send on_new_cst cst;
  cst

(* TODO check this before all declarations?
   to avoid a constructor to be also declared as sub-constant, etc. *)

type classify_res =
  | Ty of Ind_ty.t
  | Cstor of Ind_ty.constructor * Ind_ty.t
  | Cst of cst
  | Sub of sub_cst
  | Other

let classify id =
  let res =
    CCList.find
      (function
        | Payload_cst c -> Some (Cst c)
        | Ind_ty.Payload_ind_cstor (c,t) -> Some (Cstor (c,t))
        | Ind_ty.Payload_ind_type x -> Some (Ty x)
        | Payload_sub_cst s -> Some (Sub s)
        | _ -> None)
      (ID.payload id)
  in
  match res with
  | None -> Other
  | Some x -> x

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
