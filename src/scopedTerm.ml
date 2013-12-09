
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

(** {1 Scoped Terms} *)

module type SYMBOL = sig
  type t

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t
  include Interfaces.PRINT with type t := t
  include Interfaces.SERIALIZABLE with type t := t
end

module type DATA = sig
  type t

  val create : unit -> t
  val copy : t -> t
end

(** {2 Signature of a Term Representation}

Terms are optionally typed (in which case types are terms) *)

module type S = sig
  module Sym : SYMBOL

  module Data : DATA

  type t
    (** Abstract type of term *)

  type term = t

  type view = private
    | Var of int
    | BVar of int
    | Bind of Sym.t * t
    | Const of Sym.t
    | App of t * t list

  val view : t -> view
    (** View on the term's head form *)

  val ty : t -> t option
    (** Type of the term, if present *)

  val data : t -> Data.t
    (** Additional data for the term (unique to the term) *)

  val set_data : t -> Data.t -> unit
    (** Change data associated with the term *)

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  (** {3 Bool flags} *)

  val flags : t -> int
  val new_flag : unit -> int
  val set_flag : t -> int -> bool -> unit
  val get_flag : t -> int -> bool

  (** {3 Constructors} *)

  val const : ?ty:t -> Sym.t -> t
  val app : ?ty:t -> t -> t list -> t
  val bind : ?ty:t -> Sym.t -> t -> t
  val var : ?ty:t -> int -> t
  val bvar : ?ty:t -> int -> t

  val is_var : t -> bool
  val is_bvar : t -> bool
  val is_const : t -> bool
  val is_bind : t -> bool
  val is_app : t -> bool

  (** {3 Containers} *)

  module Map : Sequence.Map.S with type key = term
  module Set : Sequence.Set.S with type elt = term

  module Tbl : sig
    include Hashtbl.S with type key = term
    val to_list : 'a t -> (key * 'a) list
    val of_list : ?init:'a t -> (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Sequence.t
    val of_seq : ?init:'a t -> (key * 'a) Sequence.t -> 'a t
  end

  (** {3 De Bruijn indices handling} *)

  module DB : sig
    val closed : ?depth:int -> t -> bool
      (** check whether the term is closed (all DB vars are bound within
          the term) *)

    val contains : t -> int -> bool
      (** Does t contains the De Bruijn variable of index n? *)

    val shift : ?depth:int -> int -> t -> t
      (** shift the non-captured De Bruijn indexes in the term by n *)

    val unshift : ?depth:int -> int -> t -> t
      (** Unshift the term (decrement indices of all free De Bruijn variables
          inside) by [n] *)

    val replace : ?depth:int -> t -> sub:t -> t
      (** [db_from_term t ~sub] replaces [sub] by a fresh De Bruijn index in [t]. *)

    val from_var : ?depth:int -> t -> var:t -> t
      (** [db_from_var t ~var] replace [var] by a De Bruijn symbol in t.
          Same as {!replace}. *)

    val eval : t DBEnv.t -> t -> t
      (** Evaluate the term in the given De Bruijn environment, by
          replacing De Bruijn indices by their value in the environment. *)
  end

  (** {3 High level constructors} *)

  val bind_vars : ?ty:t -> Sym.t -> t list -> t -> t
    (** bind several free variables in the term, transforming it
        to use De Bruijn indices *)

  (** {3 Positions} *)

  val at_pos : t -> Position.t -> t 
    (** retrieve subterm at pos, or raise Invalid_argument*)

  val replace_pos : t -> Position.t -> t -> t
    (** replace t|_p by the second term *)

  val replace : t -> old:t -> by:t -> t
    (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
        in [t] by the term [by]. *)

  (** {3 Variables} *)

  val vars_seq : t -> t Sequence.t
    (** Sequence of variables of the term. Each variable may occur several
        times in the sequence. *)

  val vars_set : ?init:unit Tbl.t -> t -> unit Tbl.t
    (** Add the variables of the term to a set (can be provided) *)

  val close_vars : ?ty:t -> Sym.t -> t -> t
    (** Close all free variables of the term using the binding symbol *)

  val ground : t -> bool
    (** true if the term contains no variables (either free or bounds) *)

  (** {3 IO} *)

  include Interfaces.PRINT with type t := t

  include Interfaces.SERIALIZABLE with type t := t
end

(** {2 Functor} *)

module type BASE_TERM = sig
  module Sym : SYMBOL
  module Data : DATA

  (* term *)
  type t

  (* head form *)
  and view =
    | Var of int
    | BVar of int
    | Bind of Sym.t * t
    | Const of Sym.t
    | App of t * t list

  type term = t

  val view : t -> view
  val ty : t -> t option

  val eq : t -> t -> bool
  val hash : t -> int
  val cmp : t -> t -> int

  val const : ?ty:t -> Sym.t -> t
  val app : ?ty:t -> t -> t list -> t
  val bind : ?ty:t -> Sym.t -> t -> t
  val var : ?ty:t -> int -> t
  val bvar : ?ty:t -> int -> t
end

(* common code for hashconsed and non-hashconsed terms *)
module Common(T : BASE_TERM) = struct
  open T

  let is_var t = match view t with | Var _ -> true | _ -> false
  let is_bvar t = match view t with | BVar _ -> true | _ -> false
  let is_const t = match view t with | Const _ -> true | _ -> false
  let is_bind t = match view t with | Bind _ -> true | _ -> false
  let is_app t = match view t with | App _ -> true | _ -> false

  (** {3 Containers} *)

  module T = struct
    type t = term
    let equal = eq
    let hash = hash
    let compare = cmp
  end

  module Set = Sequence.Set.Make(T)
  module Map = Sequence.Map.Make(T)

  module Tbl = struct
    include Hashtbl.Make(T)

    let of_seq ?(init=create 7) seq =
      seq (fun (k,v) -> replace init k v);
      init

    let to_seq tbl k =
      iter (fun key v -> k (key,v)) tbl

    let to_list tbl =
      fold (fun k v acc -> (k,v)::acc) tbl []

    let of_list ?init l = of_seq ?init (Sequence.of_list l)
  end

  (** {3 De Bruijn} *)
  
  module DB = struct
    (* check wether the term is closed w.r.t. De Bruijn variables *)
    let closed ?(depth=0) t =
      let rec closed depth t =
        begin match ty t with
          | None -> true
          | Some ty -> closed depth ty
        end &&
        match view t with
        | BVar i -> i < depth
        | Bind(_, t') -> closed (depth+1) t'
        | Const _
        | Var _ -> true
        | App (f, l) -> closed depth f && List.for_all (closed depth) l
      in
      closed depth t

    (* check whether t contains the De Bruijn symbol n *)
    let rec contains t n =
      begin match ty t with
        | None -> false
        | Some ty -> contains ty n
      end ||
      match view t with
      | BVar i -> i = n
      | Const _
      | Var _ -> false
      | Bind (_, t') -> contains t' (n+1)
      | App (f, l) ->
        contains f n || List.exists (fun t' -> contains t' n) l

    (* shift the non-captured De Bruijn indexes in the term by n *)
    let shift ?(depth=0) n t =
      (* traverse the term, looking for non-captured DB indexes.
         [depth] is the number of binders on the path from the root of the
         term, to the current position. *)
      let rec recurse depth t =
        let ty = match ty t with
          | None -> None
          | Some ty -> Some (recurse depth ty)
        in
        match view t with
          | Var i -> var ?ty i
          | Const s -> const ?ty s
          | BVar i ->
            if i >= depth
              then (* shift *)
                bvar ?ty (i + n)
              else bvar ?ty i
          | Bind (s, t') ->
            let t' = recurse (depth+1) t' in
            bind ?ty s t'
          | App (f, l) ->
            app ?ty (recurse depth f) (List.map (recurse depth) l)
      in
      assert (n >= 0);
      if depth=0 && n = 0
        then t
        else recurse depth t

    (* unshift the term (decrement indices of all free De Bruijn variables inside *)
    let unshift ?(depth=0) n t =
      (* only unlift DB symbol that are free. [depth] is the number of binders
         on the path from the root term. *)
      let rec recurse depth t =
        let ty = match ty t with
          | None -> None
          | Some ty -> Some (recurse depth ty)
        in
        match view t with
          | Var i -> var ?ty i
          | BVar i ->
            if i >= depth
              then (* unshift this free De Bruijn index *)
                bvar ?ty (i-n)
              else bvar ?ty i
          | Const s -> const ?ty s
          | Bind (s, t') ->
            let t' = recurse (depth+1) t' in
            bind ?ty s t'
          | App (f, l) ->
            app ?ty (recurse depth f) (List.map (recurse depth) l)
      in
      recurse depth t

    let replace ?(depth=0) t ~sub =
      (* recurse and replace [sub]. *)
      let rec recurse depth t =
        let ty = match ty t with
          | None -> None
          | Some ty -> Some (recurse depth ty)
        in
        match view t with
          | _ when eq t sub ->
            bvar ?ty depth  (* replace *)
          | Var i -> var ?ty i
          | BVar i -> bvar ?ty i
          | Const s -> const ?ty s
          | Bind (s, t') ->
            let t' = recurse (depth+1) t' in
            bind ?ty s t'
          | App (f, l) ->
            app ?ty (recurse depth f) (List.map (recurse depth) l)
      in
      recurse depth t

    let from_var ?depth t ~var =
      assert (is_var var);
      replace ?depth t ~sub:var

    let eval env t =
      let rec eval env t =
        let ty = match ty t with
          | None -> None
          | Some ty -> Some (eval env ty)
        in
        match view t with
          | Var i -> var ?ty i
          | Const s -> const ?ty s
          | BVar i ->
            begin match DBEnv.find env i with
              | None -> bvar ?ty i
              | Some t' ->
                (* need to shift this term, because we crossed [i] binder
                    from the scope [t'] was defined in. *)
                shift i t'
            end
          | Bind (s, t') ->
            let t' = eval (DBEnv.push_none env) t' in
            bind ?ty s t'
          | App (f, l) ->
            app ?ty (eval env f) (List.map (eval env) l)
      in
      eval env t
  end

  let bind_vars ?ty s vars t =
    List.fold_right
      (fun v t ->
        assert (is_var v);
        let t' = DB.replace (DB.shift 1 t) ~sub:v in
        bind ?ty s t')
      vars t

  (** {3 Positions} *)

  let rec at_pos t pos = match view t, pos with
    | _, [] -> t
    | (Var _ | BVar _), _::_ -> invalid_arg "wrong position in term"
    | Bind(_, t'), 0::subpos -> at_pos t' subpos
    | App (t, _), 0::subpos -> at_pos t subpos
    | App (_, l), n::subpos when n <= List.length l ->
      at_pos (List.nth l (n-1)) subpos
    | _ -> invalid_arg "index too high for subterm"

  let rec replace_pos t pos new_t = match view t, pos with
    | _, [] -> new_t
    | (Var _ | BVar _), _::_ -> invalid_arg "wrong position in term"
    | Bind (s, t'), 0::subpos ->
      bind ?ty:(ty t) s (replace_pos t' subpos new_t)
    | App (f, l), 0::subpos ->
      app ?ty:(ty t) (replace_pos f subpos new_t) l
    | App (f, l), n::subpos when n <= List.length l ->
      let t' = replace_pos (List.nth l (n-1)) subpos new_t in
      let l' = Util.list_set l n t' in
      app ?ty:(ty t) f l'
    | _ -> invalid_arg "index too high for subterm"

  (* [replace t ~old ~by] syntactically replaces all occurrences of [old]
      in [t] by the term [by]. *)
  let rec replace t ~old ~by = match view t with
    | _ when eq t old -> by
    | Var _ | BVar _ | Const _ -> t
    | Bind (s, t') ->
      bind ?ty:(ty t) s (replace t' ~old ~by)
    | App (f, l) ->
      let f' = replace f ~old ~by in
      let l' = List.map (fun t' -> replace t' ~old ~by) l in
      app ?ty:(ty t) f' l'

  (** {3 Variables} *)

  let rec vars_seq t k =
    begin match ty t with
      | None -> ()
      | Some ty -> vars_seq ty k
    end;
    match view t with
      | Var _ -> k t
      | BVar _
      | Const _ -> ()
      | Bind (_, t') -> vars_seq t' k
      | App (f, l) ->
        vars_seq f k;
        List.iter (fun t' -> vars_seq t' k) l

  let vars_set ?(init=Tbl.create 5) t =
    vars_seq t (fun v -> Tbl.replace init v ());
    init

  let close_vars ?ty s t =
    let vars = Tbl.to_list (vars_set t) in
    let vars = List.map fst vars in
    bind_vars ?ty s vars t

  let ground t = Sequence.is_empty (vars_seq t)

  (** {3 IO} *)

  let rec pp_depth depth buf t =
    begin match view t with
    | Var i -> Printf.bprintf buf "X%d" i
    | BVar i -> Printf.bprintf buf "Y%d" (depth-i-1)
    | Const s -> Sym.pp buf s
    | Bind (s, t') ->
      Printf.bprintf buf "%a Y%d. %a" Sym.pp s depth (_pp_surrounded (depth+1)) t'
    | App (f, l) ->
      assert (l <> []);
      _pp_surrounded depth buf f;
      List.iter
        (fun t' -> 
          Buffer.add_char buf ' '; _pp_surrounded depth buf t')
        l
    end;
    match ty t with
      | Some ty ->
        (* print type, if present *)
        Printf.bprintf buf ":%a" (_pp_surrounded depth) ty
      | _ -> ()

  and _pp_surrounded depth buf t = match view t with
    | Bind _
    | App _ ->
      Buffer.add_char buf '(';
      pp_depth depth buf t;
      Buffer.add_char buf ')'
    | _ -> pp_depth depth buf t

  let pp buf t = pp_depth 0 buf t
  let to_string t = Util.on_buffer pp t
  let fmt fmt t = Format.pp_print_string fmt (to_string t)

  let bij =
    let open Bij in
    let (!!!) = Lazy.force in
    fix
      (fun bij ->
        let bij_bind = lazy (triple (opt !!!bij) Sym.bij !!!bij) in
        let bij_var = lazy (pair (opt !!!bij) int_) in
        let bij_cst = lazy (pair (opt !!!bij) Sym.bij) in
        let bij_at = lazy (triple (opt !!!bij) !!!bij (list_ !!!bij)) in
        switch
          ~inject:(fun t -> match view t with
          | BVar i -> "bv", BranchTo (!!!bij_var, (ty t, i))
          | Var i -> "v", BranchTo (!!!bij_var, (ty t, i))
          | Const s -> "c", BranchTo (!!!bij_cst, (ty t, s))
          | Bind (s, t') -> "bind", BranchTo (!!!bij_bind, (ty t, s, t'))
          | App (t, l) -> "a", BranchTo (!!!bij_at, (ty t, t, l)))
          ~extract:(function
          | "bv" -> BranchFrom (!!!bij_var, fun (ty,i) -> bvar ?ty i)
          | "v" -> BranchFrom (!!!bij_var, fun (ty,i) -> var ?ty i)
          | "c" -> BranchFrom (!!!bij_cst, fun (ty,s) -> const ?ty s)
          | "bind" -> BranchFrom (!!!bij_bind, fun (ty,s,t') -> bind ?ty s t')
          | "a" -> BranchFrom (!!!bij_at, fun (ty, t, l) -> app ?ty t l)
          | _ -> raise (DecodingError "expected Term")))
end

module Base(Sym : SYMBOL)(Data : DATA) = struct
  module Sym = Sym
  module Data = Data

  (* term *)
  type t = {
    term : view;
    ty : t option;
    mutable data : Data.t;
    mutable flags : int;
  }
  (* head form *)
  and view =
    | Var of int
    | BVar of int
    | Bind of Sym.t * t
    | Const of Sym.t
    | App of t * t list
  type term = t

  let view t = t.term
  let ty t = t.ty
  let data t = t.data
  let set_data t data = t.data <- data

  let rec hash t =
    let h = match view t with
    | Var i -> i
    | BVar i -> Hash.combine 11 i
    | Bind (s, t') -> Hash.combine (Sym.hash s) (hash t')
    | Const s -> Sym.hash s
    | App (f, l) ->
      Hash.hash_list hash (hash f) l
    in
    _hash_ty h t
  and _hash_ty h t = match t.ty with
    | None -> h
    | Some ty -> Hash.combine h (hash ty)

  let rec eq t1 t2 =
    _eq_ty t1 t2 &&
    match view t1, view t2 with
    | Var i, Var j
    | BVar i, BVar j -> i = j
    | Const s1, Const s2 -> Sym.eq s1 s2
    | Bind (s1, t1'), Bind (s2, t2') ->
      Sym.eq s1 s2 && eq t1' t2'
    | App (f1, l1), App (f2, l2) ->
      eq f1 f2 && _eq_list l1 l2
    | _ -> false
  and _eq_ty t1 t2 = match ty t1, ty t2 with
    | None, None -> true
    | Some ty1, Some ty2 -> eq ty1 ty2
    | None, Some _
    | Some _, None -> false
  and _eq_list l1 l2 = match l1, l2 with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | t1::l1', t2::l2' -> eq t1 t2 && _eq_list l1' l2'

  let cmp t1 t2 = failwith "Scoped.cmp: not implemented"

  let _flag_gen = Util.Flag.create ()
  let new_flag () = Util.Flag.get_new _flag_gen
  let set_flag t flag truth =
    if truth
      then t.flags <- t.flags lor flag
      else t.flags <- t.flags land (lnot flag)
  let get_flag t flag = (t.flags land flag) != 0
  let flags t = t.flags

  (** {3 Constructors} *)

  let const ?ty s = { term=Const s; ty; flags=0; data=Data.create(); }

  let rec app ?ty f l =
    match f.term, l with
    | _, [] -> {f with ty; }
    | App (f', l'), _ -> app ?ty f' (l' @ l)
    | _ -> {term=App (f, l); ty; flags=0; data=Data.create(); }

  let var ?ty i = {term=Var i; ty; flags=0; data=Data.create(); }

  let bvar ?ty i = {term=BVar i; ty; flags=0; data=Data.create(); }

  let bind ?ty s t' = {term=Bind (s,t'); ty; flags=0; data=Data.create(); }
end

module BaseHashconsed(Sym : SYMBOL)(Data : DATA) = struct
  module Sym = Sym
  module Data = Data

  (* term *)
  type t = {
    term : view;
    ty : t option;
    mutable id : int;
    mutable data : Data.t;
    mutable flags : int;
  }
  (* head form *)
  and view =
    | Var of int
    | BVar of int
    | Bind of Sym.t * t
    | Const of Sym.t
    | App of t * t list
  type term = t

  let view t = t.term
  let ty t = t.ty
  let data t = t.data
  let set_data t data = t.data <- data

  let hash t = t.id
  let eq t1 t2 = t1 == t2
  let cmp t1 t2 = t1.id - t2.id

  let _hash_ty h t = match t.ty with
    | None -> h
    | Some ty -> Hash.combine h (hash ty)
  let _hash_norec t =
    let h = match view t with
    | Var i -> i
    | BVar i -> Hash.combine 11 i
    | Bind (s, t') -> Hash.combine (Sym.hash s) (hash t')
    | Const s -> Sym.hash s
    | App (f, l) ->
      Hash.hash_list hash (hash f) l
    in
    _hash_ty h t

  let rec _eq_norec t1 t2 =
    _eq_ty t1 t2 &&
    match view t1, view t2 with
    | Var i, Var j
    | BVar i, BVar j -> i = j
    | Const s1, Const s2 -> Sym.eq s1 s2
    | Bind (s1, t1'), Bind (s2, t2') ->
      Sym.eq s1 s2 && eq t1' t2'
    | App (f1, l1), App (f2, l2) ->
      eq f1 f2 && _eq_list l1 l2
    | _ -> false
  and _eq_ty t1 t2 = match ty t1, ty t2 with
    | None, None -> true
    | Some ty1, Some ty2 -> eq ty1 ty2
    | None, Some _
    | Some _, None -> false
  and _eq_list l1 l2 = match l1, l2 with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | t1::l1', t2::l2' -> eq t1 t2 && _eq_list l1' l2'

  let _flag_gen = Util.Flag.create ()
  let new_flag () = Util.Flag.get_new _flag_gen
  let set_flag t flag truth =
    if truth
      then t.flags <- t.flags lor flag
      else t.flags <- t.flags land (lnot flag)
  let get_flag t flag = (t.flags land flag) != 0
  let flags t = t.flags

  (** {3 Constructors} *)

  module H = Hashcons.Make(struct
    type t = term
    let equal = _eq_norec
    let hash = _hash_norec
    let tag i t = assert (t.id = ~-1); t.id <- i
  end)

  let const ?ty s =
    H.hashcons { term=Const s; id= ~-1; ty; flags=0; data=Data.create(); }

  let rec app ?ty f l =
    match f.term, l with
    | _, [] -> {f with ty; }
    | App (f', l'), _ -> app ?ty f' (l' @ l)
    | _ ->
      H.hashcons {term=App (f, l); id= ~-1; ty; flags=0; data=Data.create(); }

  let var ?ty i =
    H.hashcons {term=Var i; id= ~-1; ty; flags=0; data=Data.create(); }

  let bvar ?ty i =
    H.hashcons {term=BVar i; id= ~-1; ty; flags=0; data=Data.create(); }

  let bind ?ty s t' =
    H.hashcons {term=Bind (s,t'); id= ~-1; ty; flags=0; data=Data.create(); }
end

module UnitData = struct
  type t = unit
  let create() = ()
  let copy()=()
end

module MakeData(Sym : SYMBOL)(Data : DATA) = struct
  module B = Base(Sym)(Data)
  include B
  include Common(B)
end

module Make(Sym : SYMBOL) = MakeData(Sym)(UnitData)

module MakeHashconsedData(Sym : SYMBOL)(Data : DATA) = struct
  module B = BaseHashconsed(Sym)(Data)
  include B
  include Common(B)
end

module MakeHashconsed(Sym : SYMBOL) = MakeHashconsedData(Sym)(UnitData)
