
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
  cst_parent: cst option;
  mutable cst_coverset: cover_set option; (* the coverset for this constant *)
}

and case = {
  case_term : FOTerm.t;
  case_kind: [`Base | `Rec]; (* at least one sub-constant? *)
  case_sub: cst list; (* set of sub-constants *)
  case_skolems: (ID.t * Type.t) list; (* set of other skolems *)
}

and path_cell = {
  path_cst: cst;
  path_case: case;
  path_clauses: ClauseContext.t list;
}

and path = path_cell list

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

let cover_set_cases ?(which=`All) set =
  let seq = Sequence.of_list set in
  match which with
  | `All -> seq
  | `Base -> Sequence.filter case_is_base seq
  | `Rec -> Sequence.filter case_is_rec seq

let cover_set_skolems c =
  Sequence.of_list c
  |> Sequence.flat_map (fun c -> Sequence.of_list c.case_skolems)

(** {6 Inductive Constants} *)

let cst_to_term c = FOTerm.const ~ty:c.cst_ty c.cst_id
let cst_id c = c.cst_id
let cst_ty c = c.cst_ty

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
        | Some c1' -> cst_equal c1' c1
        | None -> false

let as_sub_cst_of id c1 =
  match as_cst id with
    | None -> None
    | Some c2 ->
      match c2.cst_parent with
        | Some c1' when cst_equal c1' c1 -> Some c2
        | _ -> None

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

(** {6 Creation of Coverset and Cst} *)

type id_or_ty_builtin =
  | I of ID.t
  | B of Type.builtin

let type_hd_exn ty =
  let _, _, ret = Type.open_poly_fun ty in
  match Type.view ret with
  | Type.Builtin b -> B b
  | Type.App (s, _) -> I s
  | _ ->
      invalid_declf "expected function type, got %a" Type.pp ty

(* type declarations required by [c] *)
let declarations_of_cst c = match c.cst_coverset with
  | None -> Sequence.empty
  | Some set ->
    let seq1 =
      cover_set_sub_constants set
      |> Sequence.map (fun c' -> c'.cst_id, c'.cst_ty)
    and seq2 = cover_set_skolems set in
    Sequence.append seq1 seq2

module CoversetState = struct
  (* state for creating coverset *)
  type t = {
    cst: CstSet.t; (* raw set of constants *)
    others: (ID.t * Type.t) list; (* non-inductive terms *)
  }

  let empty = {
    cst=CstSet.empty;
    others=[];
  }

  (* state monad *)
  type 'a m = t -> t * 'a

  (* state monad inside a backtracking monad *)
  type 'a mm = t -> (t * 'a) list
  let fail : _ mm = fun _ -> []
  let yield : 'a -> 'a mm = fun x st -> [st, x]
  let yield_l : 'a list -> 'a mm = fun l st -> List.map (fun x -> st,x) l
  let (>>=) : 'a mm -> ('a -> 'b m) -> 'b mm
  = fun x_mm f st ->
    let xs = x_mm st in
    List.map (fun (st,x) -> f x st) xs
  let (>|=) : 'a mm -> ('a -> 'b) -> 'b mm
  = fun x_mm f st ->
    let xs = x_mm st in
    List.map (fun (st,x) -> st, f x) xs
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
    let st = { st with cst=CstSet.add c st.cst} in
    set st

  let add_constant id ty : unit mm =
    get >>= fun st ->
    let st = { st with others=(id,ty)::st.others} in
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

(* declare new constant *)
let declare_cst_ ~parent id ty =
  if is_cst id then raise (AlreadyDeclaredConstant id);
  assert (Type.is_ground ty); (* constant --> not polymorphic *)
  let ity = match Ind_ty.as_inductive_type ty with
    | Some t -> t
    | None -> invalid_declf "cannot declare a constant of type %a" Type.pp ty
  in
  (* depth of the constant *)
  let depth = match parent with
    | None -> 0
    | Some p -> p.cst_depth + 1
  in
  (* build coverset and constant, mutually recursive *)
  let cst = {
    cst_id=id;
    cst_ty=ty;
    cst_depth=depth;
    cst_parent=parent;
    cst_ity=ity;
    cst_coverset=None;
  }
  in
  Util.debugf ~section:Ind_ty.section 2
    "@[<2>declare new inductive symbol `@[%a : %a@]`@]"
    (fun k->k ID.pp id Type.pp ty);
  ID.add_payload id (Payload_cst cst);
  (* return *)
  Signal.send on_new_cst cst;
  cst

let make_coverset_ ~cover_set_depth cst : cover_set =
  let open CoversetState in
  (* map variables from [ity] to this concrete type *)
  let ity = cst.cst_ity in
  let subst =
    Unif.Ty.matching_same_scope
      ~pattern:ity.Ind_ty.ty_pattern cst.cst_ty ~scope:0
  in
  (* we declare [id:ty], a new sub-constant of [cst] *)
  let decl_sub id ty =
    if Ind_ty.is_inductive_type ty
    then
      let sub = declare_cst_ ~parent:(Some cst) id ty in
      add_sub_case sub >|= fun () ->
      cst_to_term sub
    else
      add_constant id ty >|= fun () ->
      T.const ~ty id
  in
  (* list of generators of:
      - member of the coverset (one of the t such that cst=t)
      - set of sub-constants of this term *)
  let rec make depth : T.t mm =
    (* leaves: fresh constants *)
    if depth=0
    then (
      let id = mk_skolem_ ID.pp ity.Ind_ty.ty_id in
      let ty = Substs.Ty.apply_no_renaming subst (ity.Ind_ty.ty_pattern,0) in
      decl_sub id ty
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
      let id = match type_hd_exn ty with
        | B b -> mk_skolem_ Type.pp_builtin b
        | I id -> mk_skolem_ ID.pp id
      in
      decl_sub id ty
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
      case_kind=if CstSet.is_empty state.cst then `Base else `Rec;
      case_sub=CstSet.elements state.cst;
      case_skolems=state.others;
    } in
    yield case
  in
  run make_top empty

(* compute coverset on the fly, if need be *)
let cst_cover_set c = match c.cst_coverset with
  | Some _ as res -> res
  | None ->
    if c.cst_depth >= !max_depth_
    then None
    else (
      let set = make_coverset_ ~cover_set_depth:1 c in
      c.cst_coverset <- Some set;
      Util.debugf ~section:Ind_ty.section 2
        "@[<2>coverset of `@[%a@]`@ is @[%a@]@]"
        (fun k->k pp_cst c (Util.pp_list pp_case) set);
      Util.debugf ~section:Ind_ty.section 5
        "@[<2>sub-constants:@ @[<v>%a@]@]"
        (fun k ->
           let pp_case out case =
             Format.fprintf out "@[<h>case %a: sub {@[<hv>%a@]}@]"
               pp_case case (Util.pp_list ID.pp)
               (case_sub_constants case
                |> Sequence.map (fun c -> c.cst_id) |> Sequence.to_list)
           in
           k CCFormat.(list pp_case) set);
      Some set
    )

(* declare toplevel constant, evaluate its coverset immediately *)
let declare_cst ?(cover_set_depth=1) id ~ty =
  let cst = declare_cst_ ~parent:None id ty in
  cst.cst_coverset <- Some (make_coverset_ ~cover_set_depth cst);
  cst

let cst_of_id id ty =
  if Ind_ty.is_inductive_type ty
  (* check if already a constant *)
  then match as_cst id with
    | Some c -> c
    | None -> declare_cst id ~ty
  else
    invalid_declf "@[cst_of_id: @[%a:%a@]@ is not of an inductive type@]"
      ID.pp id Type.pp ty

let cst_of_term t =
  let ty = T.ty t in
  match T.view t with
  | T.Const id ->
    if Ind_ty.is_inductive_type ty
    then match as_cst id with
      | Some _ as res -> res
      | None -> Some (declare_cst id ~ty)
    else None
  | _ -> None (* TODO: allow function, if not a constructor *)

(* TODO: generalize to ground terms *)
(* find inductive constant candidates in terms *)
let find_cst_in_term t =
  T.Seq.subterms t
  |> Sequence.filter_map
    (fun t -> match T.view t with
      | T.Const id ->
          let n_tyvars, ty_args, ty_ret = Type.open_poly_fun (T.ty t) in
          (* must be a constant *)
          if n_tyvars=0 && ty_args=[]
          then
            if Ind_ty.is_inductive_type ty_ret then
              if is_cst id
              || not (Ind_ty.is_constructor id)
                then Some (cst_of_id id ty_ret) (* bingo *)
                else None
            else
              None
          else None
      | _ -> None)

(** {6 Path} *)

let path_empty = []
let path_cons path_cst path_case path_clauses tail =
  { path_cst; path_case; path_clauses } :: tail

(* lexico ordering on paths *)
let rec path_compare p1 p2 = match p1, p2 with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | c1 :: p1', c2 :: p2' ->
    let open CCOrd in
    cst_compare c1.path_cst c2.path_cst
    <?> (case_compare, c1.path_case, c2.path_case)
    <?> (list_ ClauseContext.compare, c1.path_clauses, c2.path_clauses)
    <?> (path_compare, p1', p2')

let path_equal p1 p2 = path_compare p1 p2 = 0

let rec path_hash_fun p h = match p with
  | [] -> CCHash.int 42 h
  | c :: p' ->
    h
    |> CCHash.int (cst_hash c.path_cst)
    |> CCHash.int (case_hash c.path_case)
    |> CCHash.list ClauseContext.hash_fun c.path_clauses
    |> path_hash_fun p'

let path_hash = CCHash.apply path_hash_fun

let path_length = List.length

let path_dominates p1 p2 =
  let n1 = List.length p1 in
  let n2 = List.length p2 in
  n1 > n2
  && path_equal (CCList.drop (n1-n2) p1) p2

let path_contains_cst p c =
  List.exists (fun c' -> cst_equal c c'.path_cst) p

let pp_path out p =
  let rec aux out = function
  | [] -> ()
  | c :: p' ->
    Format.fprintf out "@[<hv>[@[%a = %a@]@ for @[<v>%a@]]@]"
      pp_cst c.path_cst pp_case c.path_case
      (Util.pp_list ClauseContext.pp) c.path_clauses;
    if p' <> [] then (
      Format.fprintf out "@,@<1>·%a" aux p'
    );
  in
  Format.fprintf out "@[<hv>%a@]" aux p

let lits_of_path p =
  Array.of_list p
  |> Array.map
    (fun c ->
      let l = cst_to_term c.path_cst in
      let r = case_to_term c.path_case in
      Literal.mk_eq l r)
