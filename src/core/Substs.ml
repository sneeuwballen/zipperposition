
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Substitutions} *)

module T = InnerTerm
module Hash = CCHash

type term = T.t
type var = T.t HVar.t

module VarInt = struct
  type t = var Scoped.t
  let compare = Scoped.compare HVar.compare
  let equal = Scoped.equal HVar.equal
  let hash = Scoped.hash HVar.hash_fun
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
  let rename r v = match r with
  | Dummy -> Scoped.get v  (* do not rename *)
  | Tbl tbl ->
      begin try
        H.find tbl v
      with Not_found ->
        let v' = HVar.make ~ty:(HVar.ty v.Scoped.value) (H.length tbl) in
        H.add tbl v v';
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

let rec deref subst t =
  match T.view t.Scoped.value with
    | T.Var v ->
        begin try
          let t' = find_exn subst (Scoped.set t v) in
          deref subst t'
        with Not_found -> t
        end
    | _ -> t

(** Recursively lookup a variable in the substitution, until we get a value
    that is not a variable or that is not bound *)
let get_var subst v =
  match find subst v with
  | None -> None
  | Some t -> Some (deref subst t)

exception KindError

let bind subst v t =
  try
    let t' = M.find v subst in
    let msg = CCFormat.sprintf
      "@[<2>Subst.bind:@ inconsistent binding@ for %a: %a@ and %a@]"
        (Scoped.pp HVar.pp) v (Scoped.pp T.pp) t (Scoped.pp T.pp) t'
    in
    invalid_arg msg
  with Not_found ->
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
          else
            let msg = CCFormat.sprintf
              "@[<2>Subst.append:@ inconsistent bindings for @[%a@]:@ @[%a@]@ and @[%a@]@]"
                (Scoped.pp HVar.pp) v (Scoped.pp T.pp) t1 (Scoped.pp T.pp) t2
            in
            invalid_arg msg)
    s1 s2

(*
let compose s1 s2 = failwith "Subst.compose: not implemented"
*)

let fold f acc subst =
  M.fold (fun v t acc -> f acc v t) subst acc

let iter f subst = M.iter (fun v t -> f v t) subst

(* is the substitution a renaming? *)
let is_renaming subst =
  try
    let codomain = H.create 8 in
    M.iter
      (fun _ t ->
        match T.view t.Scoped.value with
        | T.Var v -> H.replace codomain (Scoped.set t v) ()
        | _ ->
            raise Exit (* some var bound to a non-var term *)
      )
      subst;
    (* as many variables in codomain as variables in domain *)
    H.length codomain = M.cardinal subst
  with Exit -> false

(* set of variables bound by subst, with their scope *)
let domain s k = M.iter (fun v _ -> k v) s

(* set of terms that some variables are bound to by the substitution *)
let codomain s k = M.iter (fun _ (t,s_t) -> k (t,s_t)) s

(* variables introduced by the subst *)
let introduced subst k =
  M.iter
    (fun _ (t,s_t) ->
      T.Seq.vars t (fun v -> k (v,s_t)))
    subst

let to_seq subst k = M.iter (fun v t -> k (v,t)) subst

let to_list subst = M.fold (fun v t acc -> (v,t)::acc) subst []

let of_seq ?(init=empty) seq =
  Sequence.fold (fun subst (v,t) -> bind subst v t) init seq

let of_list ?(init=empty) l = match l with
  | [] -> init
  | _::_ ->
    List.fold_left (fun subst (v,t) -> bind subst v t) init l

let pp out subst =
  let pp_binding out (v,t) =
    Format.fprintf out "@[<2>@[%a@] →@ @[%a@]a@]"
      (Scoped.pp HVar.pp) v (Scoped.pp T.pp) t
  in
  Format.fprintf out "{@[<hv>%a@]}"
    (CCFormat.seq ~start:"" ~stop:"" ~sep:", " pp_binding)
    (to_seq subst)

let to_string = CCFormat.to_string pp

(** {2 Applying a substitution} *)

let apply subst ~renaming t =
  let rec aux t =
    match T.ty t.Scoped.value with
    | T.NoType ->
      assert(T.is_ground t.Scoped.value);
      t.Scoped.value
    | T.HasType ty ->
      let ty = aux (Scoped.set t ty) in
      match T.view t.Scoped.value with
      | T.Const s ->
        (* regular constant *)
        T.const ~ty s
      | T.DB i -> T.bvar ~ty i
      | T.Var v ->
        (* the most interesting cases!
           switch depending on whether [t] is bound by [subst] or not *)
        begin try
          let t'  = find_exn subst (Scoped.set t v) in
          (* NOTE: we used to shift [t'], in case it contained free De
             Bruijn indices, but that shouldn't happen because only
             closed terms should appear in substitutions. *)
          assert (T.DB.closed t'.Scoped.value);
          (* also apply [subst] to [t'] *)
          aux t'
        with Not_found ->
          (* variable not bound by [subst], rename it
              (after specializing its type if needed) *)
          let t = Scoped.set t v in
          T.var (Renaming.rename renaming t)
        end
      | T.Bind (s, varty, sub_t) ->
          let varty' = aux (Scoped.set t varty) in
          let sub_t' = aux (Scoped.set t sub_t) in
          T.bind ~varty:varty' ~ty s sub_t'
      | T.App (hd, l) ->
          let hd' = aux (Scoped.set t hd) in
          let l' = aux_list t l in
          T.app ~ty hd' l'
      | T.Record (l, rest) ->
          let rest = match rest with
            | None -> None
            | Some v ->
                begin try
                  let t' = find_exn subst (Scoped.set t v) in
                  Some (aux t')
                with Not_found ->
                  Some (T.var v)
                end
          in
          let l' = List.map (fun (s,t') -> s, aux (Scoped.set t t')) l in
          T.record_flatten ~ty l' ~rest
      | T.SimpleApp (s, l) ->
          let l' = aux_list t l in
          T.simple_app ~ty s l'
      | T.AppBuiltin (s, l) ->
          let l' = aux_list t l in
          T.app_builtin ~ty s l'
      | T.Multiset l ->
          let l' = aux_list t l in
          T.multiset ~ty l'
  and aux_list s l = match l with
    | [] -> []
    | t::l' ->
        aux (Scoped.set s t) :: aux_list s l'
  in
  aux t

let apply_no_renaming subst t =
  apply subst ~renaming:Renaming.dummy t

(** {2 Specializations} *)

module type SPECIALIZED = sig
  type term
  type t = subst

  val apply : t -> renaming:Renaming.t -> term Scoped.t -> term
  (** Apply the substitution to the given term/type.
      @param renaming used to desambiguate free variables from distinct scopes *)

  val apply_no_renaming : t -> term Scoped.t -> term
  (** Same as {!apply}, but performs no renaming of free variables.
      {b Caution}, can entail collisions between scopes! *)

  val bind : t -> var Scoped.t -> term Scoped.t -> t
  (** Add [v] -> [t] to the substitution. Both terms have a context.
      @raise Invalid_argument if [v] is already bound in
        the same context, to another term. *)
end

module Ty = struct
  type term = Type.t
  type t = subst

  let apply subst ~renaming t =
    Type.of_term_unsafe (apply subst ~renaming (t : term Scoped.t :> T.t Scoped.t))

  let apply_no_renaming subst t =
    Type.of_term_unsafe (apply_no_renaming subst (t : term Scoped.t :> T.t Scoped.t))

  let bind = (bind :> t -> var Scoped.t -> term Scoped.t -> t)
end

module FO = struct
  type term = FOTerm.t
  type t = subst

  let apply subst ~renaming t =
    FOTerm.of_term_unsafe (apply subst ~renaming (t : term Scoped.t :> T.t Scoped.t))

  let apply_no_renaming  subst t =
    FOTerm.of_term_unsafe (apply_no_renaming  subst (t : term Scoped.t :> T.t Scoped.t))

  let bind = (bind :> t -> var Scoped.t -> term Scoped.t -> t)
end

module HO = struct
  type term = HOTerm.t
  type t = subst

  let apply  subst ~renaming t =
    HOTerm.of_term_unsafe (apply  subst ~renaming (t : term Scoped.t :> T.t Scoped.t))

  let apply_no_renaming  subst t =
    HOTerm.of_term_unsafe (apply_no_renaming  subst (t : term Scoped.t :> T.t Scoped.t))

  let bind = (bind :> t -> var Scoped.t -> term Scoped.t -> t)
end
