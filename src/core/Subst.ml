
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Substitutions} *)

module T = InnerTerm

type term = T.t
type var = T.t HVar.t

module VarInt = struct
  type t = var Scoped.t
  let compare = Scoped.compare (HVar.compare T.compare)
  let equal = Scoped.equal (HVar.equal T.equal)
  let hash = Scoped.hash HVar.hash
end

module H = Hashtbl.Make(VarInt)
module M = CCMap.Make(VarInt)

(** {2 Renaming} *)

module Renaming = struct
  type t =
    | Dummy
    | Tbl of T.t HVar.t H.t

  let create () = Tbl (H.create 8)

  let clear r = match r with
    | Dummy -> ()
    | Tbl r ->
      H.clear r;
      ()

  (* special renaming that does nothing *)
  let dummy = Dummy

  (* rename variable *)
  let rename r ((v,_) as var) = match r with
    | Dummy -> v  (* do not rename *)
    | Tbl tbl ->
      begin try
          H.find tbl var
        with Not_found ->
          let v' = HVar.make ~ty:(HVar.ty v) (H.length tbl) in
          H.add tbl var v';
          v'
      end
end

(* map from scoped variables, to scoped terms *)
type t = T.t Scoped.t M.t

type subst = t

let empty = M.empty

let is_empty = M.is_empty

let find_exn subst v = M.find v subst
let find subst v = try Some (M.find v subst) with Not_found -> None

let mem subst v = M.mem v subst

let rec deref subst ((t,sc_t) as term) =
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
            (Scoped.pp HVar.pp) v (Scoped.pp T.pp) t1 (Scoped.pp T.pp) t2
        in
        Some msg
      | _ -> None)

let bind
  : t -> var Scoped.t -> T.t Scoped.t -> t
  = fun subst v t ->
    assert (not (M.mem v subst));
    M.add v t subst

let remove subst v = M.remove v subst

let append s1 s2 =
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
    |> Sequence.filter_map
      (fun (t,sc_t) -> match T.view t with
         | T.Var v -> Some ((v,sc_t),())
         | _ -> None)
    |> M.of_seq
  in
  (* as many variables in codomain as variables in domain *)
  M.cardinal rev = M.cardinal subst

(* variables introduced by the subst *)
let introduced subst k =
  M.iter
    (fun _ (t,sc_t) ->
       T.Seq.vars t (fun v -> k (v,sc_t)))
    subst

let to_seq subst k = M.iter (fun v t -> k (v,t)) subst

let to_list subst = M.fold (fun v t acc -> (v,t)::acc) subst []

let of_seq ?(init=empty) seq =
  Sequence.fold (fun subst (v,t) -> bind subst v t) init seq

let of_list ?(init=empty) l = match l with
  | [] -> init
  | _::_ ->
    List.fold_left (fun subst (v,t) -> bind subst v t) init l

let equal (s1:t) s2 : bool = M.equal (Scoped.equal T.equal) s1 s2

let hash (s:t): int =
  CCHash.(seq (pair (Scoped.hash HVar.hash) (Scoped.hash T.hash))) (M.to_seq s)

let pp out subst =
  let pp_binding out (v,t) =
    Format.fprintf out "@[<2>@[%a@] →@ @[%a@]@]"
      (Scoped.pp HVar.pp) v (Scoped.pp T.pp) t
  in
  Format.fprintf out "{@[<hv>%a@]}"
    (Util.pp_seq ~sep:", " pp_binding)
    (to_seq subst)

let to_string = CCFormat.to_string pp

(** {2 Applying a substitution} *)

let apply subst ~renaming t =
  let rec aux (t,sc_t) =
    match T.ty t with
      | T.NoType ->
        assert(T.equal T.tType t);
        t
      | T.HasType ty ->
        let ty' = aux (ty,sc_t) in
        match T.view t with
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
            begin try
                let (t', _) as term'  = find_exn subst (v,sc_t) in
                (* NOTE: we used to shift [t'], in case it contained free De
                   Bruijn indices, but that shouldn't happen because only
                   closed terms should appear in substitutions. *)
                assert (T.DB.closed t');
                (* also apply [subst] to [t'] *)
                aux term'
              with Not_found ->
                (* variable not bound by [subst], rename it
                    (after specializing its type if needed) *)
                let v = HVar.cast v ~ty:ty' in
                let v' = Renaming.rename renaming (v,sc_t) in
                if T.equal ty ty' && HVar.equal T.equal v v'
                then t
                else T.var v'
            end
          | T.Bind (s, varty, sub_t) ->
            let varty' = aux (varty,sc_t) in
            let sub_t' = aux (sub_t,sc_t) in
            T.bind ~varty:varty' ~ty:ty' s sub_t'
          | T.App (hd, l) ->
            let hd' = aux (hd,sc_t) in
            let l' = aux_list l sc_t in
            if T.equal ty ty' && T.equal hd hd' && T.same_l l l'
            then t
            else T.app ~ty:ty' hd' l'
          | T.AppBuiltin (s, l) ->
            let l' = aux_list l sc_t in
            if T.equal ty ty' && T.same_l l l'
            then t
            else T.app_builtin ~ty:ty' s l'
  and aux_list l sc = match l with
    | [] -> []
    | t::l' ->
      aux (t,sc) :: aux_list l' sc
  in
  aux t

let apply_no_renaming subst t =
  apply subst ~renaming:Renaming.dummy t

(** {2 Specializations} *)

module type SPECIALIZED = sig
  type term
  type t = subst

  val find_exn : t -> var Scoped.t -> term Scoped.t

  val get_var : t -> var Scoped.t -> term Scoped.t option

  val deref : t -> term Scoped.t -> term Scoped.t

  val apply : t -> renaming:Renaming.t -> term Scoped.t -> term
  (** Apply the substitution to the given term/type.
      @param renaming used to desambiguate free variables from distinct scopes *)

  val apply_no_renaming : t -> term Scoped.t -> term
  (** Same as {!apply}, but performs no renaming of free variables.
      {b Caution}, can entail collisions between scopes! *)

  val bind : t -> var Scoped.t -> term Scoped.t -> t
  (** Add [v] -> [t] to the substitution. Both terms have a context.
      @raise InconsistentBinding if [v] is already bound in
        the same context, to another term. *)
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

  let apply subst ~renaming t =
    Type.of_term_unsafe (apply subst ~renaming (t : term Scoped.t :> T.t Scoped.t))

  let apply_no_renaming subst t =
    Type.of_term_unsafe (apply_no_renaming subst (t : term Scoped.t :> T.t Scoped.t))

  let bind = (bind :> t -> var Scoped.t -> term Scoped.t -> t)
end

module FO : SPECIALIZED with type term = FOTerm.t = struct
  type term = FOTerm.t
  type t = subst

  let deref subst t =
    let t, sc = deref subst (t : term Scoped.t :> T.t Scoped.t) in
    FOTerm.of_term_unsafe t, sc

  let get_var subst v =
    let o = get_var subst v in
    CCOpt.map (Scoped.map FOTerm.of_term_unsafe) o

  let find_exn subst v =
    let t = find_exn subst v in
    Scoped.map FOTerm.of_term_unsafe t

  let apply subst ~renaming t =
    FOTerm.of_term_unsafe (apply subst ~renaming (t : term Scoped.t :> T.t Scoped.t))

  let apply_no_renaming subst t =
    FOTerm.of_term_unsafe (apply_no_renaming  subst (t : term Scoped.t :> T.t Scoped.t))

  let bind = (bind :> t -> var Scoped.t -> term Scoped.t -> t)
end
