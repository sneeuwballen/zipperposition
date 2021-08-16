
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Substitutions} *)

module T = InnerTerm

type term = T.t
type var = T.t HVar.t

module VarInt = struct
  type t = var Scoped.t
  let compare = Scoped.compare (fun a b -> CCOrd.int (HVar.id a) (HVar.id b))
  let equal = Scoped.equal (fun a b -> HVar.id a = HVar.id b)
  let hash = Scoped.hash HVar.hash
end

module VarWTypeInt = struct
  type t = var Scoped.t
  let compare = Scoped.compare (HVar.compare T.compare)
  let equal = Scoped.equal (HVar.equal T.equal)
  let hash = Scoped.hash HVar.hash
end


module H = Hashtbl.Make(VarInt)
module M = CCMap.Make(VarInt)
(* module MWTy = CCMap.Make(VarWTypeInt) *)
module IntMap = Map.Make(CCInt)


(** {2 Renaming} *)

module Renaming = struct
  type t =
    | R_none
    | R_some of {
        mutable map: var M.t;
        mutable n: int;
      }

  let none = R_none

  let[@inline] is_none = function R_none -> true | R_some _ -> false

  let[@inline] create () = R_some {map=M.empty; n=0}

  (* rename variable *)
  let rename r ((v,_) as var) = match r with
    | R_none -> v
    | R_some r ->
      try
        M.find var r.map
      with Not_found ->
        let v' = HVar.make ~ty:(HVar.ty v) r.n in
        r.n <- r.n + 1;
        r.map <- M.add var v' r.map;
        v'

  (* rename variable (after specializing its type if needed) *)
  let rename_with_type renaming (v,sc_v) new_ty =
    let v' = rename renaming (v,sc_v) in
    HVar.cast v' ~ty:new_ty

end

(* map from scoped variables, to scoped terms *)
type t = T.t Scoped.t M.t

type subst = t

let empty = M.empty

let[@inline] is_empty s = M.is_empty s

let[@inline] find_exn subst v = M.find v subst
let[@inline] find subst v = try Some (M.find v subst) with Not_found -> None

let[@inline] mem subst v = M.mem v subst

let[@unroll 2] rec deref subst ((t,sc_t) as term) =
  match T.view t with
  | T.Var v ->
    begin match find subst (v,sc_t) with
      | Some t' -> deref subst t'
      | None -> term
    end
  | _ -> term

(** Recursively lookup a variable in the substitution, until we get a value
    that is not a variable or that is not bound *)
let get_var subst v =
  match find subst v with
  | None -> None
  | Some t -> Some (deref subst t)

exception InconsistentBinding of var Scoped.t * term Scoped.t * term Scoped.t

let () = Printexc.register_printer
    (function
      | InconsistentBinding (v, t1, t2) ->
        let msg = CCFormat.sprintf
            "@[<2>inconsistent binding@ for %a: %a@ and %a@]"
            (Scoped.pp T.pp_var) v (Scoped.pp T.pp) t1 (Scoped.pp T.pp) t2
        in
        Some msg
      | _ -> None)

let bind
  : t -> var Scoped.t -> T.t Scoped.t -> t
  = fun subst v t ->
    assert (not (M.mem v subst));
    M.add v t subst

let update
  : t -> var Scoped.t -> T.t Scoped.t -> t
  = fun subst v t ->
    assert (M.mem v subst);
    M.add v t subst

let[@inline] remove subst v = M.remove v subst

let filter_scope subst sc = M.filter (fun (_,sc_v) _ -> sc=sc_v) subst

let merge s1 s2 =
  M.merge
    (fun v b1 b2 -> match b1, b2 with
       | None, _ -> b2
       | _, None -> b1
       | Some t1, Some t2 ->
         if Scoped.equal T.equal t1 t2
         then Some t1
         else raise (InconsistentBinding (v, t1, t2)))
    s1 s2

(*
let compose s1 s2 = failwith "Subst.compose: not implemented"
*)

let fold f acc subst =
  M.fold (fun v t acc -> f acc v t) subst acc

let iter f subst = M.iter (fun v t -> f v t) subst

(* set of variables bound by subst, with their scope *)
let domain s k = M.iter (fun v _ -> k v) s

(* set of terms that some variables are bound to by the substitution *)
let codomain s k = M.iter (fun _ t -> k t) s

(* is the substitution a renaming? *)
let is_renaming subst =
  let rev =
    codomain subst
    |> Iter.filter_map
      (fun (t,sc_t) -> match T.view (Lambda.Inner.eta_reduce t) with
         | T.Var v -> Some ((v,sc_t),())
         | _ -> None)
    |> M.of_iter
  in
  (* as many variables in codomain as variables in domain *)
  M.cardinal rev = M.cardinal subst

(* variables introduced by the subst *)
let introduced subst k =
  M.iter
    (fun _ (t,sc_t) ->
       T.Seq.vars t (fun v -> k (v,sc_t)))
    subst

let normalize subst : t =
  let rec aux sc t =
    if T.equal t T.tType then t
    else (
      let ty = aux sc (T.ty_exn t) in
      match T.view t with
      | T.Var v ->
        (* follow binding if it stays in the same domain *)
        begin match find subst (v,sc) with
          | Some (u, sc') when sc=sc' -> aux sc u
          | _ -> T.var (HVar.cast ~ty v)
        end
      | T.DB i -> T.bvar ~ty i
      | T.Const id -> T.const ~ty id
      | T.App (f, l) -> T.app ~ty (aux sc f) (List.map (aux sc) l)
      | T.AppBuiltin (b, l) -> T.app_builtin b ~ty (List.map (aux sc) l)
      | T.Bind (b,varty,body) ->
        let varty = aux sc varty in
        T.bind b ~ty ~varty (aux sc body)
    )
  in
  M.map (fun (t,sc) -> aux sc t, sc) subst

let[@inline] map f subst = M.map (fun (t,sc) -> f t, sc) subst

let[@inline] filter f subst = M.filter f subst

let[@inline] to_iter subst k = M.iter (fun v t -> k (v,t)) subst

let[@inline] to_list subst = M.fold (fun v t acc -> (v,t)::acc) subst []

let of_iter ?(init=empty) seq =
  Iter.fold (fun subst (v,t) -> bind subst v t) init seq

let of_list ?(init=empty) l = match l with
  | [] -> init
  | _::_ ->
    List.fold_left (fun subst (v,t) -> bind subst v t) init l

let[@inline] equal (s1:t) s2 : bool = M.equal (Scoped.equal T.equal) s1 s2
let[@inline] compare s1 s2 = M.compare (Scoped.compare T.compare) s1 s2

let[@inline] hash (s:t): int =
  CCHash.(iter (pair (Scoped.hash HVar.hash) (Scoped.hash T.hash))) (M.to_iter s)

let pp_bindings out subst =
  let pp_binding out (v,t) =
    Format.fprintf out "@[<2>@[%a@] @<1>→@ @[%a@]@]"
      (Scoped.pp T.pp_var) v 
      (Scoped.pp T.pp) t 
  in
  Util.pp_iter ~sep:", " pp_binding out (to_iter subst)

let pp out subst = Format.fprintf out "{@[<hv>%a@]}" pp_bindings subst

let to_string = CCFormat.to_string pp

(** {2 Applying a substitution} *)

let apply_aux ~sv subst ~f_rename t sc =
  let rec aux t sc_t depth =
    if T.is_ground t then t
    else (
      match T.ty t with
      | T.NoType ->
        assert(T.equal T.tType t);
        t
      | T.HasType ty ->
        let ty' = aux ty sc_t depth in
        let res =
          begin match T.view t with
            | T.Const id ->
              (* regular constant *)
              if T.equal ty ty'
              then t
              else T.const ~ty:ty' id
            | T.DB i ->
              if T.equal ty ty'
              then t
              else T.bvar ~ty:ty' i
            | T.Var v ->
              (* the most interesting cases!
                 switch depending on whether [t] is bound by [subst] or not *)
              begin match find_exn subst (v,sc_t) with
                | (t',sc') ->
                  (* NOTE: if [t'] is not closed, we assume that it
                     is always replaced in a context where variables
                     are properly bound. Typically, that means only
                     in rewriting. *)
                  (* also apply [subst] to [t'] *)
                  let shifted = if sv != -1 then T.DB.shift depth t' else t' in
                  aux shifted sc' depth
                | exception Not_found ->
                  (* rename the variable using [f_rename] *)
                  let v' = f_rename (v,sc_t) ty' in
                  T.var v'
              end
            | T.Bind (s, varty, sub_t) ->
              let varty' = aux varty sc_t (depth+1) in
              let sub_t' = aux sub_t sc_t (depth+1) in
              let res = T.bind ~varty:varty' ~ty:ty' s sub_t' in
              (* Util.debugf 1 ("Before body: %a, after body: %a") *)
              (* (fun k -> k T.pp t T.pp res); res *)
              res
            | T.App (hd, l) ->
              let hd' = aux hd sc_t depth in
              let l' = aux_list l sc_t depth in
              if T.equal ty ty' && T.equal hd hd' && T.same_l l l'
              then t
              else T.app ~ty:ty' hd' l'
            | T.AppBuiltin (s, l) ->
              let l' = aux_list l sc_t depth in
              if T.equal ty ty' && T.same_l l l'
              then t
              else T.app_builtin ~ty:ty' s l'
          end in
        res)
  and aux_list l sc depth = match l with
    | [] -> []
    | [t1] -> [aux t1 sc depth]
    | t1::t2::l' ->
      aux t1 sc depth :: aux t2 sc depth :: aux_list l' sc depth
  in
  aux t sc sv

(* Apply substitution to a term and rename variables not bound by [subst]*)
let apply ?(shift_vars=(-1)) renaming subst (t,sc) =
  if is_empty subst && Renaming.is_none renaming then t
  else (
    apply_aux ~sv:shift_vars subst ~f_rename:(Renaming.rename_with_type renaming) t sc
  )

(** {2 Specializations} *)

module type SPECIALIZED = sig
  type term
  type t = subst

  val find_exn : t -> var Scoped.t -> term Scoped.t

  val get_var : t -> var Scoped.t -> term Scoped.t option

  val deref : t -> term Scoped.t -> term Scoped.t

  val apply : ?shift_vars:int -> Renaming.t -> t -> term Scoped.t -> term
  (** Apply the substitution to the given term/type.
      @param renaming used to desambiguate free variables from distinct scopes *)

  val bind : t -> var Scoped.t -> term Scoped.t -> t
  (** Add [v] -> [t] to the substitution. Both terms have a context.
      @raise InconsistentBinding if [v] is already bound in
        the same context, to another term. *)

  val update : t -> var Scoped.t -> term Scoped.t -> t
  (** Replaces [v] -> ? by [v] -> [t] in the substitution. Both terms have a context.
      @raise InconsistentBinding if [v] is not yet bound in the same context. *)

  val of_list : ?init:t -> (var Scoped.t * term Scoped.t) list -> t
end

module Ty : SPECIALIZED with type term = Type.t = struct
  type term = Type.t
  type t = subst

  let deref subst t =
    let t, sc = deref subst (t : term Scoped.t :> T.t Scoped.t) in
    Type.of_term_unsafe t, sc

  let get_var subst v =
    let o = get_var subst v in
    CCOpt.map (Scoped.map Type.of_term_unsafe) o

  let find_exn subst v =
    let t = find_exn subst v in
    Scoped.map Type.of_term_unsafe t

  let apply ?(shift_vars=(-1)) renaming subst t =
    Type.of_term_unsafe (apply ~shift_vars renaming subst (t : term Scoped.t :> T.t Scoped.t))

  let bind = (bind :> t -> var Scoped.t -> term Scoped.t -> t)
  let update = (update :> t -> var Scoped.t -> term Scoped.t -> t)
  let of_list = (of_list :> ?init:t -> (var Scoped.t * term Scoped.t) list -> t)
end


module FO = struct
  type term = Term.t
  type t = subst

  let deref subst t =
    let t, sc = deref subst (t : term Scoped.t :> T.t Scoped.t) in
    Term.of_term_unsafe t, sc

  let get_var subst v =
    let o = get_var subst v in
    CCOpt.map (Scoped.map Term.of_term_unsafe) o

  let find_exn subst v =
    let t = find_exn subst v in
    Scoped.map Term.of_term_unsafe t


  let apply ?(shift_vars=(-1))  renaming subst t =
    Term.of_term_unsafe (apply ~shift_vars renaming subst (t : term Scoped.t :> T.t Scoped.t))

  let apply_l ?(shift_vars=(-1))  renaming subst (l,sc) =
    List.map (fun t -> apply ~shift_vars renaming subst (t,sc)) l


  let compose ~scope s1 s2 =
    (* Format.printf "Composing: @[ %a = %a @]\n" pp s1 pp s2; *)
    let subs_l1 = to_list s1 in
    let subs_as_map =
      (List.map (fun ((v,sc_v), (t,sc_t)) ->
           ((v,sc_v), (( (Lambda.snf (apply Renaming.none s2 (Term.of_term_unsafe t,sc_t))) : term :> T.t), scope)))
          subs_l1) @ (to_list s2) in
    (of_list subs_as_map)


  let canonize_neg_vars ~var_set = 
    let max_id   = T.VarSet.max_elt_opt var_set in
    match max_id with 
    | Some id ->
      let max_id = ref (CCInt.max (HVar.id id) (-1)) in
      T.VarSet.fold (fun v subst -> 
          let v_id = HVar.id v in
          if v_id < 0 then (
            match get_var subst ((v :> InnerTerm.t HVar.t),0) with
            | Some _ -> subst 
            | None -> (
                incr max_id;
                let renamed_var = T.var (HVar.make ~ty:(HVar.ty v) !max_id) in
                bind subst ((v :> InnerTerm.t HVar.t), 0) (renamed_var, 0)))
          else subst) 
        var_set empty 
    | None -> empty

  let canonize_all_vars t =
    apply (Renaming.create()) empty (t,0)


  let bind = (bind :> t -> var Scoped.t -> term Scoped.t -> t)
  let update = (update :> t -> var Scoped.t -> term Scoped.t -> t)
  let of_list = (of_list :> ?init:t -> (var Scoped.t * term Scoped.t) list -> t)


  let bind' = (bind :> t -> Type.t HVar.t Scoped.t -> term Scoped.t -> t)
  let update' = (update :> t -> Type.t HVar.t Scoped.t -> term Scoped.t -> t)
  let of_list' = (of_list :> ?init:t -> (Type.t HVar.t Scoped.t * term Scoped.t) list -> t)
  (* let to_list = (to_list :>  t -> (Type.t HVar.t Scoped.t * term Scoped.t) list ) *)


  let map f s = map (fun t -> (f (Term.of_term_unsafe t) : term :> T.t)) s

  let filter f s =
    filter
      (fun (v,sc_v) (t,sc_t) ->
         f
           (HVar.update_ty ~f:Type.of_term_unsafe v,sc_v)
           (Term.of_term_unsafe t,sc_t))
      s


  let iter f s =
    iter (fun (v, sc_v) (t,sc_t) ->
        let v = HVar.update_ty ~f:Type.of_term_unsafe v in
        let t = Term.of_term_unsafe t in
        f (v, sc_v) (t, sc_t)
      ) s

  let unleak_variables subs =
    let subs_l = to_list subs in
    let unleaked_l, new_sk = List.fold_right
        (fun ((v,sc_v), (t,sc_t)) (l, sk_map) ->
           let t = Term.of_term_unsafe t in
           Util.debugf 1 " unleaking in unleak_vars : %a" (fun k -> k Term.pp t);
           let t', sk_map = Term.DB.skolemize_loosely_bound ~already_sk:sk_map t in
           let v' = (HVar.update_ty ~f:Type.of_term_unsafe v,sc_v) in
           (v', (t',sc_t))::l, sk_map) subs_l ([],Term.IntMap.empty) in
    of_list' unleaked_l, List.map snd (Term.IntMap.bindings new_sk)

  let subset_is_renaming ~subset ~res_scope subst =
    try 
      let subset = List.filter (fun v ->
          let der_t, der_sc = deref subst v in
          if der_sc != snd v then (
            der_sc = res_scope
          ) else (
            der_sc = res_scope && not (Term.equal (fst v) der_t)
          )
        ) subset in
      let derefed_vars = CCList.map (fun v ->
          let derefed = deref subst v in
          if not (Term.is_var (fst derefed)) then (
            raise (invalid_arg "found a non-variable")
          ) else derefed
        ) subset 
                         |> CCList.sort_uniq ~cmp:(Scoped.compare Term.compare) in
      List.length derefed_vars = List.length subset
    with Invalid_argument _ -> false



end

(** {2 Projections for proofs} *)

module Projection = struct
  type t = {
    scope: Scoped.scope;
    subst: subst;
    renaming: Renaming.t;
  }

  let[@inline] scope t = t.scope
  let[@inline] subst t = t.subst
  let[@inline] renaming t = t.renaming

  (* actual constructor from a substitution *)
  let bindings (p:t) : (var * term) list =
    fold
      (fun l (v,sc_v) (t,sc_t) ->
         if sc_v = p.scope then (
           let t = apply p.renaming p.subst (t,sc_t) in
           (v,t) :: l
         ) else l)
      [] p.subst

  let as_inst ?allow_free_db ~ctx (sp:t) (vars:_ HVar.t list) : (_,_) Var.Subst.t =
    List.map
      (fun v ->
         let t_v = Term.var v in
         let t =
           FO.apply (renaming sp) (subst sp) ((t_v,scope sp))
         in
         Term.Conv.var_to_simple_var ctx v, Term.Conv.to_simple_term ?allow_free_db ctx t)
      vars
    |> Var.Subst.of_list

  let[@inline] make renaming (subst,sc) : t = { scope=sc; subst; renaming; }

  let[@inline] is_empty (p:t) : bool = is_empty p.subst && Renaming.is_none p.renaming

  let pp out (p:t) : unit = Format.fprintf out "%a[%d]" pp p.subst p.scope
end
