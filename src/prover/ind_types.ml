
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Inductive Types} *)

open Libzipperposition

module T = FOTerm

let section = Util.Section.make ~parent:Const.section "ind"

type constructor = {
  cstor_name: ID.t;
  cstor_ty: Type.t;
}

(** {6 Inductive Types} *)

(** An inductive type, along with its set of constructors *)
type t = {
  id: ID.t; (* name *)
  ty_vars: Type.t HVar.t list; (* list of variables *)
  ty_pattern: Type.t; (* equal to  [id ty_vars] *)
  constructors : constructor list;
    (* constructors, all returning [pattern] and containing
       no other type variables than [ty_vars] *)
}

let fail_ fmt = CCFormat.ksprintf fmt ~f:failwith
let invalid_argf_ fmt = CCFormat.ksprintf fmt ~f:invalid_arg

exception AlreadyDeclaredType of ID.t
exception AlreadyDeclaredConstant of ID.t
exception NotAnInductiveType of ID.t
exception NotAnInductiveConstructor of ID.t
exception NotAnInductiveConstant of ID.t
exception NotAnInductiveCase of FOTerm.t
exception NotAnInductiveSubConstant of ID.t

let () =
  let spf = CCFormat.sprintf in
  Printexc.register_printer
  (function
    | AlreadyDeclaredType id ->
        Some (spf "%a already declared as an inductive type" ID.pp id)
    | AlreadyDeclaredConstant id ->
        Some (spf "%a already declared as an inductive constant" ID.pp id)
    | NotAnInductiveType id ->
        Some (spf "%a is not an inductive type" ID.pp id)
    | NotAnInductiveConstructor id ->
        Some (spf "%a is not an inductive constructor" ID.pp id)
    | NotAnInductiveConstant id ->
        Some (spf "%a is not an inductive constant" ID.pp id)
    | NotAnInductiveCase t ->
        Some (spf "@[@[%a@]@ is not an inductive case@]" T.pp t)
    | NotAnInductiveSubConstant id ->
        Some (spf "%a is not an inductive sub-constant" ID.pp id)
    | _ -> None)

exception Payload_ind_type of t
exception Payload_ind_cstor of constructor * t

let type_hd_exn ty =
  let _, ret = Type.open_fun ty in
  match Type.view ret with
  | Type.App (s, _) -> s
  | _ ->
      invalid_argf_ "expected function type, got %a" Type.pp ty

let on_new_inductive_ty = Signal.create()

let as_inductive_ty id =
  CCList.find
    (function
      | Payload_ind_type ty -> Some ty
      | _ -> None)
    (ID.payload id)

let as_inductive_ty_exn id = match as_inductive_ty id with
  | Some ty -> ty
  | None -> raise (NotAnInductiveType id)

let is_inductive_ty id =
  match as_inductive_ty id with Some _ -> true | None -> false

let is_inductive_type ty =
  let id = type_hd_exn ty in
  is_inductive_ty id

let as_inductive_type ty =
  let id = type_hd_exn ty in
  as_inductive_ty id

(* declare that the given type is inductive *)
let declare_ty id ~ty_vars constructors =
  Util.debugf ~section 1 "declare inductive type %a" (fun k->k ID.pp id);
  if constructors = []
  then invalid_argf_ "Ind_types.declare_ty %a: no constructors provided" ID.pp id;
  (* check that [ty] is not declared already *)
  List.iter
    (function
      | Payload_ind_type _ -> fail_ "inductive type %a already declared" ID.pp id;
      | _ -> ())
    (ID.payload id);
  let ity = {
    id;
    ty_vars;
    ty_pattern=Type.app id (List.map Type.var ty_vars);
    constructors;
  } in
  (* map the constructors to [ity] too *)
  List.iter
    (fun c ->
      ID.add_payload c.cstor_name (Payload_ind_cstor (c, ity)))
    constructors;
  (* map [id] to [ity] *)
  ID.add_payload id (Payload_ind_type ity);
  Signal.send on_new_inductive_ty ity;
  ity

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
  cst_ity: t; (* the corresponding inductive type *)
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
          let ty = T.ty t in
          begin match as_inductive_type ty with
          | Some ity ->
              if not (is_cst id) && not (is_constructor id)
              then Some (id, ity, ty) (* bingo *)
              else None
          | _ -> None
          end
      | _ -> None)

(** {6 Sub-Constants} *)

type sub_cst = {
  sub_cst_term: ID.t;
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

(** {6 Creation of Coverset and Cst} *)

(* monad over "lazy" values *)
module FunM = CCFun.Monad(struct type t = unit end)
module FunT = CCList.Traverse(FunM)

(* coverset of given depth for this type and constant *)
let make_coverset_ ~depth:_ _ity _cst : cover_set =
  assert false
  (* FIXME
  let cst_data = T.Tbl.find _tbl cst in
  (* list of generators of:
      - member of the coverset (one of the t such that cst=t)
      - set of sub-constants of this term *)
  let rec make depth =
    (* leaves: fresh constants *)
    if depth=0
    then
      [fun () ->
        let ty = ity.pattern in
        let name = CCFormat.sprintf "#%a" ID.pp (type_hd_exn ty) in
        let c = ID.make name in
        let t = T.const ~ty c in
        ID.Tbl.replace cst_data.dominates c ();
        Ctx.declare c ty;
        set_blocked t;
        t, T.Set.singleton t
      ]
    (* inner nodes or base cases: constructors *)
    else CCList.flat_map
        (fun (f, ty_f) ->
           match Type.arity ty_f with
           | Type.NoArity ->
               fail_ "invalid constructor %a for inductive type %a"
                 ID.pp f Type.pp ity.pattern
           | Type.Arity (0, 0) ->
               if depth > 0
               then  (* only one answer : f *)
                 [fun () -> T.const ~ty:ty_f f, T.Set.empty]
               else []
           | Type.Arity (0, _) ->
               let ty_args = Type.expected_args ty_f in
               CCList.(
                 make_list (depth-1) ty_args >>= fun mk_args ->
                 return (fun () ->
                     let args, set = mk_args () in
                     T.app (T.const f ~ty:ty_f) args, set)
               )
           | Type.Arity (m,_) ->
               fail_
                 ("inductive constructor %a requires %d type " ^^
                  "parameters, expected 0")
                 ID.pp f m
        ) ity.constructors
  (* given a list of types [l], yield all lists of cover terms
      that have types [l] *)
  and make_list depth l
    : (T.t list * T.Set.t) FunM.t list
    = match l with
    | [] -> [FunM.return ([], T.Set.empty)]
    | ty :: tail ->
        let t_builders = if Unif.Ty.matches ~pattern:ity.pattern ty
          then make depth
          else [fun () ->
              (* not an inductive sub-case, just create a skolem symbol *)
              let name = CCFormat.sprintf "#%a" ID.pp (type_hd_exn ty) in
              let c = ID.make name in
              let t = T.const ~ty c in
              ID.Tbl.replace cst_data.dominates c ();
              Ctx.declare c ty;
              t, T.Set.empty
            ] in
        let tail_builders = make_list depth tail in
        CCList.(
          t_builders >>= fun mk_t ->
          tail_builders >>= fun mk_tail ->
          [FunM.(mk_t >>= fun (t,set) ->
                 mk_tail >>= fun (tail,set') ->
                 return (t::tail, T.Set.union set set'))]
        )
  in
  assert (depth>0);
  (* make the cover set's cases, tagged with `Base or `Rec depending
     on whether they contain sub-cases *)
  let cases_and_subs =
    List.map
      (fun gen ->
         let t, set = gen() in
         (* remember whether [t] is base or recursive case *)
         if T.Set.is_empty set
         then (t, `Base), set
         else (t, `Rec), set)
      (make depth)
  in
  let cases, sub_constants = List.split cases_and_subs in
  let cases, rec_cases, base_cases =
    List.fold_left
      (fun (c,r,b) (t,is_base) -> match is_base with
         | `Base -> t::c, r, t::b
         | `Rec -> t::c, t::r, b)
      ([],[],[]) cases
  in
  let sub_constants =
    List.fold_left T.Set.union T.Set.empty sub_constants in
  let coverset = {
    cases = List.map Case.of_term_unsafe cases;
    rec_cases = List.map Case.of_term_unsafe cases;
    base_cases = List.map Case.of_term_unsafe cases;
    sub_constants;
  } in
  (* declare sub-constants as such. They won't be candidate for induction
     and will be smaller than [t] *)
  List.iter
    (fun ((t, _), set) ->
       T.Tbl.add _tbl_case t (cst, coverset);
       T.Set.iter
         (fun sub_cst -> T.Tbl.replace _tbl_sub_cst sub_cst (cst, coverset, t))
         set)
    cases_and_subs;
  coverset
  *)

let declare_cst ?(cover_set_depth=1) id ~ty =
  if is_cst id then raise (AlreadyDeclaredConstant id);
  let ity = as_inductive_ty_exn (type_hd_exn ty) in
  let cst_coverset = make_coverset_ ~depth:cover_set_depth ity id in
  Util.debugf 2 "declare new inductive constant %a" (fun k->k ID.pp id);
  let cst = {
    cst_id=id;
    cst_ty=ty;
    cst_ity=ity;
    cst_coverset;
  } in
  Signal.send on_new_cst cst;
  cst

(* TODO check this before all declarations?
   to avoid a constructor to be also declared as sub-constant, etc. *)

type classify_res =
  | Ty of t
  | Cstor of constructor * t
  | Cst of cst
  | Sub of sub_cst
  | Other

let classify id =
  let res =
    CCList.find
      (function
        | Payload_cst c -> Some (Cst c)
        | Payload_ind_cstor (c,t) -> Some (Cstor (c,t))
        | Payload_ind_type x -> Some (Ty x)
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
