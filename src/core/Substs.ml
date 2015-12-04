
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Substitutions} *)

module T = ScopedTerm
module Hash = CCHash

type scope = int
  (** A scope is an integer. Variables can only be bound in one scope,
      and variables from distinct scopes are distinct too. *)

type 'a scoped = 'a * scope

let pp_scoped p out (x,sc) =
  Format.fprintf out "@[%a[%d]@]" p x sc

type term = T.t
type var = term HVar.t

module VarInt = struct
  type t = (var * int)
  let compare (t1,i1)(t2,i2) =
    if i1=i2 then HVar.compare t1 t2 else CCOrd.int_ i1 i2
  let equal (t1,i1)(t2,i2) = i1 = i2 && HVar.equal t1 t2
  let hash = Hash.apply (fun (t,i) h -> Hash.int_ i (HVar.hash_fun t h))
end

module H = Hashtbl.Make(VarInt)
module M = CCMap.Make(VarInt)

(** {2 Renaming} *)

module Renaming = struct
  type t =
    | Dummy
    | Tbl of T.t H.t

  let create () = Tbl (H.create 8)

  let clear r = match r with
  | Dummy -> ()
  | Tbl r ->
    H.clear r;
    ()

  (* special renaming that does nothing *)
  let dummy = Dummy

  (* rename variable *)
  let rename r v s_v = match r, T.view v with
  | Dummy, _ -> v  (* do not rename *)
  | Tbl tbl, T.Var v ->
      let key = v, s_v in
      begin try
        H.find tbl key
      with Not_found ->
        let v' = T.var (HVar.make ~ty:(HVar.ty v) (H.length tbl)) in
        H.add tbl key v';
        v'
      end
  | _ -> assert false
end

type t = (term * int) M.t

type subst = t

let empty = M.empty

let is_empty = M.is_empty

let find_exn subst v = M.find v subst
let find subst v = try Some (M.find v subst) with Not_found -> None

let mem subst v = M.mem v subst

let rec deref subst ((t, sc_t) as pair) =
  match T.view t with
    | T.Var v ->
        begin try
          let t' = find_exn subst (v, sc_t) in
          deref subst t'
        with Not_found -> pair
        end
    | _ -> pair

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
        (pp_scoped HVar.pp) v (pp_scoped T.pp) t (pp_scoped T.pp) t'
    in
    invalid_arg msg
  with Not_found ->
    M.add v t subst

let remove subst v = M.remove v subst

let append s1 s2 =
  M.merge
    (fun (v,s_v) b1 b2 -> match b1, b2 with
      | None, _ -> b2
      | _, None -> b1
      | Some (t1, s1), Some (t2, s2) ->
        if T.equal t1 t2 && s1 = s2
          then Some (t1, s1)
          else
            let msg = CCFormat.sprintf
              "@[<2>Subst.append:@ inconsistent binding@ for %a[%d]: %a[%d]@ and %a[%d]@]"
                HVar.pp v s_v T.pp t1 s1 T.pp t2 s2
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
      (fun _ (t,s_t) ->
        match T.view t with
        | T.Var v -> H.replace codomain (v,s_t) ()
        | _ ->
            raise Exit (* some var bound to a non-var term *)
      )
      subst;
    (* as many variables in codomain as variables in domain *)
    H.length codomain = M.cardinal subst
  with Exit -> false

(* set of variables bound by subst, with their scope *)
let domain s k = M.iter (fun (v,s_v) _ -> k (v,s_v)) s

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
    Format.fprintf out "@[<2>%a →@ %a@]"
      (pp_scoped HVar.pp) v (pp_scoped T.pp) t
  in
  Format.fprintf out "{@[<hv>%a@]}"
    (CCFormat.seq ~start:"" ~stop:"" ~sep:", " pp_binding)
    (to_seq subst)

let to_string = CCFormat.to_string pp

(** {2 Applying a substitution} *)

let apply subst ~renaming (t, s_t) =
  let rec aux t s_t =
    match T.ty t with
    | T.NoType ->
      assert(T.is_ground t);
      t
    | T.HasType ty ->
      let ty = aux ty s_t in
      match T.view t with
      | T.Const s ->
        (* regular constant *)
        T.const ~ty s
      | T.DB i -> T.bvar ~ty i
      | T.Var v ->
        (* the most interesting cases!
           switch depending on whether [t] is bound by [subst] or not *)
        begin try
          let t', s_t' = find_exn subst (v, s_t) in
          (* NOTE: we used to shift [t'], in case it contained free De
             Bruijn indices, but that shouldn't happen because only
             closed terms should appear in substitutions. *)
          assert (T.DB.closed t');
          (* also apply [subst] to [t'] *)
          aux t' s_t'
        with Not_found ->
          (* variable not bound by [subst], rename it
              (after specializing its type if needed) *)
          let t = T.var v in
          Renaming.rename renaming t s_t
        end
      | T.Bind (s, varty, sub_t) ->
          let varty' = aux varty s_t in
          let sub_t' = aux sub_t s_t in
          T.bind ~varty:varty' ~ty s sub_t'
      | T.App (hd, l) ->
          let hd' = aux hd s_t in
          let l' = aux_list l s_t in
          T.app ~ty hd' l'
      | T.Record (l, rest) ->
          let rest = match rest with
            | None -> None
            | Some v ->
                begin try
                  let t', s_t' = find_exn subst (v, s_t) in
                  Some (aux t' s_t')
                with Not_found ->
                  Some (T.var v)
                end
          in
          let l' = List.map (fun (s,t') -> s, aux t' s_t) l in
          T.record_flatten ~ty l' ~rest
      | T.SimpleApp (s, l) ->
          let l' = aux_list l s_t in
          T.simple_app ~ty s l'
      | T.AppBuiltin (s, l) ->
          let l' = aux_list l s_t in
          T.app_builtin ~ty s l'
      | T.Multiset l ->
          let l' = aux_list l s_t in
          T.multiset ~ty l'
  and aux_list l s_l = match l with
    | [] -> []
    | t::l' -> aux t s_l :: aux_list l' s_l
  in
  aux t s_t

let apply_no_renaming subst t =
  apply subst ~renaming:Renaming.dummy t

(** {2 Specializations} *)

module type SPECIALIZED = sig
  type term
  type t = subst

  val apply : t -> renaming:Renaming.t -> term scoped -> term
  (** Apply the substitution to the given term/type.
      @param renaming used to desambiguate free variables from distinct scopes *)

  val apply_no_renaming : t -> term scoped -> term
  (** Same as {!apply}, but performs no renaming of free variables.
      {b Caution}, can entail collisions between scopes! *)

  val bind : t -> var scoped -> term scoped -> t
  (** Add [v] -> [t] to the substitution. Both terms have a context.
      @raise Invalid_argument if [v] is already bound in
        the same context, to another term. *)
end

module Ty = struct
  type term = Type.t
  type t = subst

  let apply subst ~renaming t =
    Type.of_term_unsafe (apply subst ~renaming (t : term scoped :> T.t scoped))

  let apply_no_renaming subst t =
    Type.of_term_unsafe (apply_no_renaming subst (t : term scoped :> T.t scoped))

  let bind = (bind :> t -> var scoped -> term scoped -> t)
end

module FO = struct
  type term = FOTerm.t
  type t = subst

  let apply subst ~renaming t =
    FOTerm.of_term_unsafe (apply subst ~renaming (t : term scoped :> T.t scoped))

  let apply_no_renaming  subst t =
    FOTerm.of_term_unsafe (apply_no_renaming  subst (t : term scoped :> T.t scoped))

  let bind = (bind :> t -> var scoped -> term scoped -> t)
end

module HO = struct
  type term = HOTerm.t
  type t = subst

  let apply  subst ~renaming t =
    HOTerm.of_term_unsafe (apply  subst ~renaming (t : term scoped :> T.t scoped))

  let apply_no_renaming  subst t =
    HOTerm.of_term_unsafe (apply_no_renaming  subst (t : term scoped :> T.t scoped))

  let bind = (bind :> t -> var scoped -> term scoped -> t)
end
