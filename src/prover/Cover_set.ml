
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Cover Set} *)

open Logtk

module T = Term
module Fmt = CCFormat

(** Use for reasoning by case during induction *)

type cst = Ind_cst.t
type term = Term.t

type case = {
  case_top: cst; (* copy of the coverset's top constant *)
  case_term : Term.t; (* rhs *)
  case_kind: [`Base | `Rec]; (* at least one sub-constant? *)
  case_sub: cst list; (* set of sub-constants *)
  case_skolems: (ID.t * Type.t) list; (* set of other skolems *)
}

type t = {
  cs_top: cst; (* inductive constant that is unique to that set *)
  cs_cases: case list; (* cases  that also are unique *)
}

(** {6 Inductive Case} *)

module Case = struct
  type t = case
  let equal a b =
    Ind_cst.equal a.case_top b.case_top && T.equal a.case_term b.case_term
  let compare a b =
    CCOrd.Infix.(
      Ind_cst.compare a.case_top b.case_top
      <?> (Term.compare, a.case_term, b.case_term))
  let hash a =
    Hash.combine2 (Ind_cst.hash a.case_top) (T.hash a.case_term)

  let pp out c = Fmt.hovbox T.pp out c.case_term

  let to_term c = c.case_term
  let to_lit c =
    let lhs = Ind_cst.to_term c.case_top in
    Literal.mk_eq lhs (to_term c)

  let same_cst c1 c2 = Ind_cst.equal c1.case_top c2.case_top

  let is_rec c = c.case_kind = `Rec
  let is_base c = c.case_kind = `Base

  let sub_constants c = c.case_sub
  let skolems c = c.case_skolems
end

let top t = t.cs_top
let ty t = Ind_cst.ty (top t)

let cases ?(which=`All) (set:t): case Sequence.t =
  let seq = Sequence.of_list set.cs_cases in
  begin match which with
    | `All -> seq
    | `Base -> Sequence.filter Case.is_base seq
    | `Rec -> Sequence.filter Case.is_rec seq
  end

let pp out (set:t): unit =
  Format.fprintf out
    "(@[<hv2>coverset of type `@[%a@]`@ :top `%a`@ :cases [@[<hv>%a@]]@])"
    Type.pp (ty set) Ind_cst.pp (top set)
    (Util.pp_list Case.pp) set.cs_cases

let skolems (c:t) =
  Sequence.of_list c.cs_cases
  |> Sequence.flat_map_l Case.skolems

let sub_constants (set:t) =
  Sequence.of_list set.cs_cases
  |> Sequence.flat_map_l Case.sub_constants

(* type declarations required by [c] *)
let declarations (set:t) =
  let decl_of_cst c = Ind_cst.id c, Ind_cst.ty c in
  let seq1 =
    sub_constants set |> Sequence.map decl_of_cst
  and seq2 = skolems set
  and top = decl_of_cst set.cs_top in
  Sequence.cons top (Sequence.append seq1 seq2)

module State_ = struct
  (* state for creating coverset *)
  type t = {
    cst: Ind_cst.Cst_set.t; (* raw set of constants *)
    others: (ID.t * Type.t) list; (* non-inductive terms *)
  }

  let empty = {
    cst=Ind_cst.Cst_set.empty;
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
    let st = { st with cst=Ind_cst.Cst_set.add c st.cst} in
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

let make_coverset_ ~cover_set_depth ~depth (ty:Type.t)(ity:Ind_ty.t) : t =
  let open State_ in
  (* map variables from [ity] to this concrete type *)
  let scope = 0 in
  let cs_top = Ind_cst.make ~depth ~is_sub:false ty in
  (* how to make a simple skolem *)
  let mk_skolem ty : T.t mm =
    let id = Ind_cst.make_skolem ty in
    add_constant id ty >|= fun () ->
    T.const ~ty id
  in
  (* how to make a sub-constant of type [ty] and register it in [cs_top] *)
  let decl_sub ty : T.t mm =
    if Ind_ty.is_inductive_type ty
    then (
      let sub = Ind_cst.make ~depth:(depth+1) ~is_sub:true ty in
      add_sub_case sub >|= fun () ->
      Ind_cst.to_term sub
    ) else mk_skolem ty
  in
  (* list of generators of:
      - member of the coverset (one of the t such that cst=t)
      - set of sub-constants of this term *)
  let rec make (cs_depth:int)(subst:Subst.t) : T.t mm =
    Util.debugf ~section:Ind_ty.section 5
      "(@[make_cover_set@ :ty %a@ :depth %d@ :subst %a@])"
      (fun k->k Ind_ty.pp ity cs_depth Subst.pp subst);
    (* leaves: fresh constants *)
    if cs_depth=0 then (
      let ty = Subst.Ty.apply Subst.Renaming.none subst (ity.Ind_ty.ty_pattern,scope) in
      decl_sub ty
    ) else (
      (* inner nodes or base cases: constructors *)
      yield_l ity.Ind_ty.ty_constructors
      >>>= fun cstor ->
      let f = cstor.Ind_ty.cstor_name in
      let ty_f = cstor.Ind_ty.cstor_ty in
      (* apply to ground type parameters *)
      let ty_params =
        List.map
          (fun v ->
             let v = Type.var v in
             Subst.Ty.apply Subst.Renaming.none subst (v,scope))
          ity.Ind_ty.ty_vars
      in
      let ty_f_applied = Type.apply ty_f ty_params in
      let ty_params = List.map T.of_ty ty_params in
      let n_ty_params, ty_args_f, _ = Type.open_poly_fun ty_f_applied in
      assert (n_ty_params=0);
      if ty_args_f=[]
      then (
        if cs_depth > 0
        then yield (T.app (T.const ~ty:ty_f f) ty_params)  (* only one answer : f *)
        else fail
      ) else (
        (* make fresh type variables and apply *)
        map_l (make_of_ty (cs_depth-1)) ty_args_f
        >>|= fun args ->
        T.app (T.const ~ty:ty_f f) (ty_params @ args)
      )
    )
  (* return a new term of type [ty] *)
  and make_of_ty depth ty : T.t mm =
    let subst =
      try
        Some
          (Unif.Ty.matching_same_scope
             ~pattern:ity.Ind_ty.ty_pattern ty ~scope)
      with Unif.Fail -> None
    in
    begin match subst with
      | Some subst -> make depth subst (* previous case *)
      | None ->
        decl_sub ty (* not an inductive sub-case, just create a skolem symbol *)
    end
  in
  let subst =
    Unif.Ty.matching_same_scope
      ~pattern:ity.Ind_ty.ty_pattern ty ~scope
  in
  (* build the toplevel values, along with a list of sub-constants
     to declare *)
  let make_cases =
    let l = make cover_set_depth subst in
    l >>>= fun t ->
    (* obtain the current set of sub-constants *)
    get >>>= fun state ->
    let case = {
      case_top=cs_top;
      case_term=t;
      case_kind=if Ind_cst.Cst_set.is_empty state.cst then `Base else `Rec;
      case_sub=Ind_cst.Cst_set.elements state.cst;
      case_skolems=state.others;
    } in
    yield case
  in
  let cs_cases = run make_cases empty in
  {cs_top; cs_cases}

(* compute coverset on the fly, if need be *)
let make ?(cover_set_depth=1) ~depth (ty:Type.t): t =
  if cover_set_depth <= 0 then (
    Util.invalid_argf "Cover_set.make: cover_set_depth=%d must be > 0"
      cover_set_depth;
  );
  begin match Ind_ty.as_inductive_type ty with
    | Some (ity,_) ->
      let set = make_coverset_ ~cover_set_depth ~depth ty ity in
      Util.debugf ~section:Ind_ty.section 2
        "@[<2>new coverset:@ %a@]" (fun k->k pp set);
      Util.debugf ~section:Ind_ty.section 5
        "@[<2>sub-constants:@ @[<v>%a@]@]"
        (fun k ->
           let pp_case out case =
             Format.fprintf out "@[<h>case %a: sub {@[<hv>%a@]}@]"
               Case.pp case (Util.pp_list ID.pp)
               (Case.sub_constants case |> List.rev_map Ind_cst.id)
           in
           k (Util.pp_list pp_case) set.cs_cases);
      set
    | None ->
      Util.errorf ~where:"cover_set.make"
        "type `@[%a@]`@ is not an inductive type"
        Type.pp ty
  end
