(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Substitutions} *)

module T = ScopedTerm

type scope = int
  (** A scope is an integer. Variables can only be bound in one scope,
      and variables from distinct scopes are distinct too. *)

type 'a scoped = 'a * scope

type term = T.t

module TermInt = struct
  type t = (T.t * int)
  let equal (t1,i1)(t2,i2) = i1 = i2 && T.eq t1 t2
  let hash (t,i) = Hash.combine i (T.hash t)
end

module H = Hashtbl.Make(TermInt)

module M = PersistentHashtbl.Make(TermInt)

(** {2 Renaming} *)

module Renaming = struct
  type t =
    | Dummy
    | Tbl of T.t H.t

  let create () = Tbl (H.create 5)

  let clear r = match r with
  | Dummy -> ()
  | Tbl r ->
    H.clear r;
    ()

  (* special renaming that does nothing *)
  let dummy = Dummy

  (* rename variable *)
  let rename r v s_v = match r, T.view v with
  | Dummy, T.Var _ -> v  (* do not rename *)
  | Tbl tbl, T.Var i ->
      begin try
        H.find tbl (v, s_v)
      with Not_found ->
        let v' = T.var ~kind:(T.kind v) ~ty:(T.ty_exn v) (H.length tbl) in
        H.add tbl (v, s_v) v';
        v'
      end
  | _ -> assert false
end

type t =
  | E
  | M of (term * int) M.t

type subst = t

let empty = E

let is_empty = function
  | E -> true
  | M m -> M.is_empty m

let lookup subst v s_v = match subst with
  | E -> raise Not_found
  | M m ->
    if T.is_var v
      then M.find m (v, s_v)
      else raise Not_found

let mem subst v s_v = match subst with
  | E -> false
  | M m -> M.mem m (v, s_v)

(** Recursively lookup a variable in the substitution, until we get a value
    that is not a variable or that is not bound *)
let rec get_var subst v sc_v = match subst with
  | E -> v, sc_v
  | M m ->
    try let t, sc_t = lookup subst v sc_v in
        if T.is_var t && (sc_t <> sc_v || not (T.eq t v))
          then get_var subst t sc_t
          else t, sc_t (* fixpoint of lookup *)
    with Not_found -> v, sc_v

exception KindError

let bind subst v s_v t s_t =
  if T.kind v <> T.kind t then raise KindError;
  let t', s_t' = get_var subst v s_v in
  if s_t' = s_t && T.eq t' t
    then subst (* compatible (absence of) bindings *)
    else if T.is_var t'
      then
        let m = match subst with
          | E -> M.create 11
          | M m -> m
        in
        let m' = M.replace m (t', s_t') (t, s_t) in
        M m'
      else
        let msg = Util.sprintf
          "Subst.bind: inconsistent binding for %a[%d]: %a[%d] and %a[%d]"
            T.pp v s_v T.pp t s_t T.pp t' s_t'
        in
        raise (Invalid_argument msg)

let remove subst v s_v = match subst with
  | E -> E
  | M m -> M (M.remove m (v, s_v))

let append s1 s2 = match s1, s2 with
  | E, _ -> s2
  | _, E -> s1
  | M m1, M m2 ->
    let m = M.merge
      (fun (v,s_v) b1 b2 -> match b1, b2 with
        | None, _ -> b2
        | _, None -> b1
        | Some (t1, s1), Some (t2, s2) ->
          if T.eq t1 t2 && s1 = s2
            then Some (t1, s1)
            else
              let msg = Util.sprintf
                "Subst.bind: inconsistent binding for %a[%d]: %a[%d] and %a[%d]"
                  T.pp v s_v T.pp t1 s1 T.pp t2 s2
              in
              raise (Invalid_argument msg))
      m1 m2
    in
    M m

(*
let compose s1 s2 = failwith "Subst.compose: not implemented"
*)

let fold subst acc f = match subst with
  | E -> acc
  | M m -> M.fold (fun acc (v,s_v) (t,s_t) -> f acc v s_v t s_t) acc m

let iter subst k = match subst with
  | E -> ()
  | M m -> M.iter m (fun (v,s_v) (t,s_t) -> k v s_v t s_t)

(* is the substitution a renaming? *)
let is_renaming subst = match subst with
| E -> true
| M m ->
  begin try
    let codomain = H.create 5 in
    M.iter m
      (fun _ (t,s_t) ->
        (* is some var bound to a non-var term? *)
        if not (T.is_var t) then raise Exit;
        H.replace codomain (t,s_t) ());
    (* as many variables in codomain as variables in domain *)
    H.length codomain = M.length m
  with Exit -> false
  end

(* set of variables bound by subst, with their scope *)
let domain s k = match s with
| E -> ()
| M m ->
  M.iter m (fun (v,s_v) _ -> k (v,s_v));
  ()

(* set of terms that some variables are bound to by the substitution *)
let codomain s k = match s with
| E -> ()
| M m ->
  M.iter m (fun _ (t,s_t) -> k (t,s_t));
  ()

(* variables introduced by the subst *)
let introduced subst k = match subst with
| E -> ()
| M m ->
  M.iter m
    (fun _ (t,s_t) ->
      T.Seq.vars t (fun v -> k (v,s_t)));
  ()

let to_seq subst = match subst with
| E -> Sequence.empty
| M m ->
  let seq = M.to_seq m in
  Sequence.map (fun ((v, s_v), (t, s_t)) -> v, s_v, t, s_t) seq

let to_list subst =
  let seq = to_seq subst in
  Sequence.to_rev_list seq

let of_seq ?(init=empty) seq =
  Sequence.fold (fun subst (v,s_v,t,s_t) -> bind subst v s_v t s_t) init seq

let of_list ?(init=empty) l = match l with
  | [] -> init
  | _::_ ->
    List.fold_left (fun subst (v,s_v,t,s_t) -> bind subst v s_v t s_t) init l

let pp buf subst =
  let pp_binding buf (v,s_v,t,s_t) =
    Printf.bprintf buf "%a[%d] → %a[%d]" T.pp v s_v T.pp t s_t
  in
  match to_list subst with
  | [] -> Buffer.add_string buf "{}"
  | l -> Printf.bprintf buf "{%a}" (Util.pp_list ~sep:", " pp_binding) l

let to_string = Util.on_buffer pp

let fmt fmt subst =
  Format.pp_print_string fmt (to_string subst)

(** {2 Applying a substitution} *)

let apply ?(depth=0) subst ~renaming t s_t =
  let rec _apply depth t s_t =
    match T.ty t with
    | T.NoType -> t
    | _ when T.ground t -> t
    | T.HasType ty ->
      let kind = T.kind t in
      let ty = _apply depth ty s_t in
      match T.view t with
      | T.Const s -> T.const ~kind ~ty s
      | T.BVar i -> T.bvar ~kind ~ty i
      | T.Var i ->
        (* the most interesting cases!
           switch depending on whether [t] is bound by [subst] or not *)
        begin try
          let t', s_t' = lookup subst t s_t in
          (* if t' contains free De Bruijn symbols, lift them by [depth] *)
          let t' = T.DB.shift ~depth:0 depth t' in
          (* also apply [subst] to [t'] *)
          _apply depth t' s_t'
        with Not_found ->
          (* variable not bound by [subst], rename it
              (after specializing its type if needed) *)
          let t = T.var ~kind ~ty i in
          Renaming.rename renaming t s_t
        end
      | T.Bind (s, sub_t) ->
          let sub_t' = _apply (depth+1) sub_t s_t in
          T.bind ~kind ~ty s sub_t'
      | T.App (hd, l) ->
          let hd' = _apply depth hd s_t in
          let l' = _apply_list depth l s_t in
          T.app ~kind ~ty hd' l'
      | T.Record l ->
          let l' = List.map (fun (s,t') -> s, _apply depth t' s_t) l in
          T.record ~kind ~ty l'
  and _apply_list depth l s_l = match l with
    | [] -> []
    | t::l' -> _apply depth t s_l :: _apply_list depth l' s_l
  in
  _apply depth t s_t

let apply_no_renaming ?depth subst t s_t =
  apply subst ?depth ~renaming:Renaming.dummy t s_t

  (*
let bij =
  Bij.(map
    ~inject:(fun s -> Sequence.to_list (to_seq s))
    ~extract:(fun seq -> of_seq (Sequence.of_list seq))
    (list_ (quad T.bij int_ T.bij int_)))
*)

(** {2 Specializations} *)

module type SPECIALIZED = sig
  type term
  type t = subst

  val apply : ?depth:int -> t -> renaming:Renaming.t -> term -> scope -> term
    (** Apply the substitution to the given term/type.
        @param renaming used to desambiguate free variables from distinct scopes *)

  val apply_no_renaming : ?depth:int -> t -> term -> scope -> term
    (** Same as {!apply}, but performs no renaming of free variables.
      {b Caution}, can entail collisions between scopes! *)

  val bind : t -> term -> scope -> term -> scope -> t
    (** Add [v] -> [t] to the substitution. Both terms have a context.
        @raise Invalid_argument if [v] is already bound in
          the same context, to another term. *)
end

module Ty = struct
  type term = Type.t
  type t = subst

  let apply ?depth subst ~renaming t s_t =
    Type.of_term_exn (apply ?depth subst ~renaming (t : Type.t :> T.t) s_t)

  let apply_no_renaming ?depth subst t s_t =
    Type.of_term_exn (apply_no_renaming ?depth subst (t : Type.t :> T.t) s_t)

  let bind = (bind :> t -> term -> scope -> term -> scope -> t)
end

module FO = struct
  type term = FOTerm.t
  type t = subst

  let apply ?depth subst ~renaming t s_t =
    FOTerm.of_term_exn (apply ?depth subst ~renaming (t : FOTerm.t :> T.t) s_t)

  let apply_no_renaming ?depth subst t s_t =
    FOTerm.of_term_exn (apply_no_renaming ?depth subst (t : FOTerm.t :> T.t) s_t)

  let bind = (bind :> t -> term -> scope -> term -> scope -> t)
end

module HO = struct
  type term = HOTerm.t
  type t = subst

  let apply ?depth subst ~renaming t s_t =
    HOTerm.of_term_exn (apply ?depth subst ~renaming (t : HOTerm.t :> T.t) s_t)

  let apply_no_renaming ?depth subst t s_t =
    HOTerm.of_term_exn (apply_no_renaming ?depth subst (t : HOTerm.t :> T.t) s_t)

  let bind = (bind :> t -> term -> scope -> term -> scope -> t)
end
