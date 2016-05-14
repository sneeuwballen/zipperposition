
(** {1 Inductive Constants and Cases}

    Skolem constants of an inductive type, coversets, etc. required for
    inductive reasoning. *)

open Libzipperposition

module T = FOTerm

let max_depth_ = ref 4

exception InvalidDecl of string
exception AlreadyDeclaredConstant of ID.t
exception NotAnInductiveConstant of ID.t

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
    | _ -> None)

let invalid_decl m = raise (InvalidDecl m)
let invalid_declf m = CCFormat.ksprintf m ~f:invalid_decl

type cst = {
  cst_id: ID.t;
  cst_ty: Type.t;
  cst_ity: Ind_ty.t; (* the corresponding inductive type *)
  cst_depth: int;
  cst_parent: cst lazy_t option;
  cst_coverset: cover_set option; (* the coverset for this constant *)
}

and case = {
  case_term : FOTerm.t;
  case_kind: [`Base | `Rec]; (* at least one sub-constant? *)
  case_sub: cst list; (* set of sub-constants *)
}

and path =
  | P_nil
  | P_cons of cst * case * ClauseContext.t list * path

and cover_set = case list

exception Payload_cst of cst

(** {6 Inductive Case} *)

let case_equal a b = FOTerm.equal a.case_term b.case_term
let case_compare a b = FOTerm.compare a.case_term b.case_term
let case_hash a = FOTerm.hash a.case_term

let pp_case out c = CCFormat.hovbox FOTerm.pp out c.case_term

let case_to_term c = c.case_term

let case_is_rec c = c.case_kind = `Rec
let case_is_base c = c.case_kind = `Base

let case_sub_constants c = Sequence.of_list c.case_sub

(** {6 Inductive Constants} *)

let cst_to_term c = FOTerm.const ~ty:c.cst_ty c.cst_id
let cst_id c = c.cst_id

let cst_equal a b = ID.equal a.cst_id b.cst_id
let cst_compare a b = ID.compare a.cst_id b.cst_id
let cst_hash a = ID.hash a.cst_id

module CstSet = CCSet.Make(struct
    type t = cst
    let compare = cst_compare
  end)

let cst_depth c = c.cst_depth

let cst_same_type c1 c2 = Type.equal c1.cst_ty c2.cst_ty

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

let cst_cover_set c = c.cst_coverset

let cst_cases ?(which=`All) set =
  let seq = Sequence.of_list set in
  match which with
  | `All -> seq
  | `Base -> Sequence.filter case_is_base seq
  | `Rec -> Sequence.filter case_is_rec seq

let cover_set_sub_constants set =
  Sequence.of_list set
  |> Sequence.flat_map case_sub_constants

let is_sub_cst id = match as_cst id with
  | None -> false
  | Some cst -> cst.cst_parent <> None

let is_sub_cst_of id c1 =
  match as_cst id with
    | None -> false
    | Some c2 ->
      match c2.cst_parent with
        | Some (lazy c1') -> cst_equal c1' c1
        | None -> false

let as_sub_cst_of id c1 =
  match as_cst id with
    | None -> None
    | Some c2 ->
      match c2.cst_parent with
        | Some (lazy c1') when cst_equal c1' c1 -> Some c2
        | _ -> None

let term_of_cst s = T.const ~ty:s.cst_ty s.cst_id

(* [dominates c1 c2] if [c2] is a sub-constant of [c1], or if
   some sub-constant of [c1] dominates [c2].
   In any case it means [c1.level < c2.level] *)
let rec dominates c1 c2 =
  c1.cst_depth < c2.cst_depth
  &&
  match c1.cst_coverset with
  | None -> false
  | Some set ->
    Sequence.exists
      (fun sub_c -> cst_equal sub_c c2 || dominates sub_c c2)
      (cover_set_sub_constants set)

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
  cover_set_sub_constants c
  |> Sequence.map (fun c' -> c'.cst_id, c'.cst_ty)

module CoversetState = struct
  (* state for creating coverset *)
  type t = CstSet.t (* raw set of constants *)

  let empty = CstSet.empty

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
  let add_sub_case c : unit mm =
    get >>= fun st ->
    let st = CstSet.add c st in
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
let rec make_coverset_
    ~cover_set_depth
    ~subst
    ~parent
    ity
  : cover_set
=
  let open CoversetState in
  (* list of generators of:
      - member of the coverset (one of the t such that cst=t)
      - set of sub-constants of this term *)
  let rec make depth : T.t mm =
    (* leaves: fresh constants *)
    if depth=0
    then (
      let id = mk_skolem_ ID.pp ity.Ind_ty.ty_id in
      let ty = Substs.Ty.apply_no_renaming subst (ity.Ind_ty.ty_pattern,0) in
      let sub = declare_cst_ ~cover_set_depth ~parent id ty in
      add_sub_case sub >>= fun () ->
      return (term_of_cst sub)
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
        map_l (make_of_ty ~parent (depth-1)) ty_args_f
        >>|= fun args ->
        T.app (T.const ~ty:ty_f f) (ty_params @ args)
      )
    )
  (* return a new term of type [ty] *)
  and make_of_ty ~parent depth ty : T.t mm =
    if Unif.Ty.matches ~pattern:ity.Ind_ty.ty_pattern ty
    then make depth (* previous case *)
    else (
      (* not an inductive sub-case, just create a skolem symbol *)
      let id = match type_hd_exn ty with
        | B b -> mk_skolem_ Type.pp_builtin b
        | I id -> mk_skolem_ ID.pp id
      in
      let sub = declare_cst_ ~cover_set_depth ~parent id ty in
      let t = term_of_cst sub in
      add_sub_case sub >>|= fun () -> t
    )
  in
  (* build the toplevel values, along with a list of sub-constants
     to declare *)
  let make_top =
    let l = make cover_set_depth in
    l >>>= fun t ->
    (* obtain the current set of sub-constants *)
    get >>>= fun state ->
    let case = {
      case_term=t;
      case_kind=if CstSet.is_empty state then `Base else `Rec;
      case_sub=CstSet.elements state;
    } in
    yield case
  in
  run make_top empty

(* declare new constant *)
and declare_cst_ ~cover_set_depth ~parent id ty =
  if is_cst id then raise (AlreadyDeclaredConstant id);
  assert (Type.is_ground ty); (* constant --> not polymorphic *)
  let ity = match type_hd_exn ty with
    | B b -> invalid_declf "cannot declare a constant of type %a" Type.pp_builtin b
    | I id -> Ind_ty.as_inductive_ty_exn id
  in
  (* depth of the constant *)
  let depth = match parent with
    | None -> 0
    | Some (lazy p) -> p.cst_depth + 1
  in
  (* map variables from [ity] to this concrete type *)
  let subst =
    Unif.Ty.matching_same_scope ~pattern:ity.Ind_ty.ty_pattern ty ~scope:0
  in
  (* build coverset and constant, mutually recursive *)
  let rec cst = lazy {
    cst_id=id;
    cst_ty=ty;
    cst_depth=depth;
    cst_parent=parent;
    cst_ity=ity;
    cst_coverset=cover_set;
  }
  and cover_set =
    if depth >= !max_depth_
    then None
    else Some (make_coverset_ ~cover_set_depth ~subst ~parent:(Some cst) ity)
  in
  Util.debugf ~section:Ind_ty.section 2
    "@[<2>declare new inductive symbol `@[%a : %a@]`\
     @ with coverset {@[%a@]}@]"
    (fun k->k ID.pp id Type.pp ty (CCFormat.opt (Util.pp_list pp_case)) cover_set);
  let cst = Lazy.force cst in
  ID.add_payload id (Payload_cst cst);
  Util.debugf ~section:Ind_ty.section 5
    "@[<2>sub-constants:@ @[<v>%a@]@]"
    (fun k ->
       let pp_case out case =
         Format.fprintf out "@[<h>case %a: sub {@[<hv>%a@]}@]"
            pp_case case (Util.pp_list ID.pp)
            (case_sub_constants case
             |> Sequence.map (fun c -> c.cst_id) |> Sequence.to_list)
       in
       k CCFormat.(opt (list pp_case)) cover_set);
  (* return *)
  Signal.send on_new_cst cst;
  cst

(* declare toplevel constant *)
let declare_cst ?(cover_set_depth=1) id ~ty =
  declare_cst_ ~cover_set_depth ~parent:None id ty

(** {6 Path} *)

(* lexico ordering on paths *)
let rec path_compare p1 p2 = match p1, p2 with
  | P_nil, P_nil -> 0
  | P_nil, P_cons _ -> -1
  | P_cons _, P_nil -> 1
  | P_cons (cst1, case1, l1, p1'), P_cons (cst2, case2, l2, p2') ->
    let open CCOrd in
    cst_compare cst1 cst2
    <?> (case_compare, case1, case2)
    <?> (list_ ClauseContext.compare, l1, l2)
    <?> (path_compare, p1', p2')

let path_equal p1 p2 = path_compare p1 p2 = 0

let rec path_hash_fun p h = match p with
  | P_nil -> CCHash.int 42 h
  | P_cons (c,case,l,p') ->
    h
    |> CCHash.int (cst_hash c)
    |> CCHash.int (case_hash case)
    |> CCHash.list ClauseContext.hash_fun l
    |> path_hash_fun p'

let path_hash = CCHash.apply path_hash_fun

let rec path_length = function
  | P_nil -> 0
  | P_cons (_, _, _, p') -> 1 + path_length p'

let rec pp_path out = function
  | P_nil -> ()
  | P_cons (cst, case, l, p') ->
    Format.fprintf out "@[<hv2>[@[%a = %a@]@ for @[<v>%a@]]"
      pp_cst cst pp_case case (Util.pp_list ClauseContext.pp) l;
    if p' <> P_nil then (
      Format.fprintf out "·@[%a@]" pp_path p'
    );
    Format.fprintf out "@]"

let lits_of_path p =
  let n = path_length p in
  let a = Array.make n Literal.mk_tauto in
  let rec aux a i = function
    | P_nil -> assert (i=n); ()
    | P_cons (cst, case, _, p') ->
      let l = cst_to_term cst in
      let r = case_to_term case in
      a.(i) <- Literal.mk_eq l r;
      aux a (i+1) p'
  in
  aux a 0 p;
  a
