
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

module Sym = Symbol

type symbol = Sym.t

module Kind = struct
  type t =
    | Type
    | FOTerm
    | HOTerm
    | BasicFOTerm
    | BasicHOTerm
    | Formula of t
    | Generic  (* other terms *)
end

(* term *)
type t = {
  term : view;
  ty : type_result;
  kind : Kind.t;
  mutable id : int;
  mutable flags : int;
}
(* head form *)
and view =
  | Var of int
  | BVar of int
  | Bind of Sym.t * t * t  (** Type, subterm*)
  | Const of Sym.t
  | Record of (string * t) list
  | App of t * t list
and type_result =
  | NoType
  | HasType of t

type term = t

let view t = t.term
let ty t = t.ty
let ty_exn t = match t.ty with
  | NoType -> raise (Invalid_argument "ScopedTerm.ty_exn")
  | HasType ty -> ty
let kind t = t.kind

let hash t = t.id
let eq t1 t2 = t1 == t2
let cmp t1 t2 = t1.id - t2.id

let _hash_ty h t =
  let hash_ty = match t.ty with
    | NoType -> 1
    | HasType ty -> ty.id
  in
  Hash.hash_int3 h hash_ty (Hashtbl.hash t.kind)
let _hash_norec t =
  let h = match view t with
  | Var i -> i
  | BVar i -> Hash.combine 11 i
  | Bind (s, varty, t') -> Hash.hash_int3 (Sym.hash s) (hash varty) (hash t')
  | Const s -> Sym.hash s
  | Record l ->
    Hash.hash_list (fun (s,t') -> Hash.combine (Hash.hash_string s) (hash t')) 17 l
  | App (f, l) ->
    Hash.hash_list hash (hash f) l
  in
  _hash_ty h t

let rec _eq_norec t1 t2 =
  _eq_ty t1 t2 &&
  t1.kind = t2.kind &&
  match t1.term, t2.term with
  | Var i, Var j
  | BVar i, BVar j -> i = j
  | Const s1, Const s2 -> Sym.eq s1 s2
  | Bind (s1, varty1, t1'), Bind (s2, varty2, t2') ->
    Sym.eq s1 s2 && eq varty1 varty2 && eq t1' t2'
  | App (f1, l1), App (f2, l2) ->
    eq f1 f2 && _eq_list l1 l2
  | _ -> false
and _eq_ty t1 t2 = eq t1.ty t2.ty
and _eq_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | t1::l1', t2::l2' -> eq t1 t2 && _eq_list l1' l2'

(** {3 Flags} *)

let _flag_gen = Util.Flag.create ()
let new_flag () = Util.Flag.get_new _flag_gen
let set_flag t flag truth =
  if truth
    then t.flags <- t.flags lor flag
    else t.flags <- t.flags land (lnot flag)
let get_flag t flag = (t.flags land flag) != 0
let flags t = t.flags

let flag_ground = new_flag()

let ground t = get_flag t flag_ground

(** {3 Constructors} *)

module H = Hashcons.Make(struct
  type t = term
  let equal = _eq_norec
  let hash = _hash_norec
  let tag i t = assert (t.id = ~-1); t.id <- i
end)

let const ~kind ~ty s =
  let my_t = { term=Const s; kind; id= ~-1; ty=HasType ty; flags=0; } in
  let t = H.hashcons my_t in
  if t == my_t then begin
    if ground ty then set_flag t flag_ground true;
  end;
  t

let rec app ~kind ~ty f l =
  match f.term, l with
  | _, [] -> {f with ty=HasType ty; }
  | App (f', l'), _ ->
    app ~kind ~ty f' (l' @ l)  (* flatten *)
  | _ ->
      let my_t = {term=App (f, l); kind; id= ~-1; ty=HasType ty; flags=0; } in
      let t = H.hashcons my_t in
      if t == my_t then begin
        if ground ty && ground f && List.for_all ground l
          then set_flag t flag_ground true;
      end;
      t

let var ~kind ~ty i =
  H.hashcons {term=Var i; kind; id= ~-1; ty=HasType ty; flags=0; }

let bvar ~kind ~ty i =
  H.hashcons {term=BVar i; kind; id= ~-1; ty=HasType ty; flags=0; }

let bind ~kind ~ty ~varty s t' =
  H.hashcons {term=Bind (s,varty,t'); kind; id= ~-1; ty=HasType ty; flags=0; }

(* if any string of [l] appears in [seen], fail *)
let rec __check_duplicates seen l = match l with
  | [] -> ()
  | (s,_) :: l' ->
      if List.mem s seen
      then failwith ("ill-formed record: field " ^ s ^ " appears twice")
      else __check_duplicates (s::seen) l'

let record ~kind ~ty l =
  __check_duplicates [] l;
  let l = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) l in
  let my_t = {term=Record l; kind; ty=HasType ty; id= ~-1; flags=0; } in
  let t = H.hashcons my_t in
  if t == my_t then begin
        if ground ty && List.for_all (fun (_,t) -> ground t) l
          then set_flag t flag_ground true;
  end;
  t

let rec tType =
  let _t = {term=Const (Sym.Conn Sym.TType); kind=Kind.Type;
            id= ~-1; ty=NoType; flags=flag_ground; } in
  H.hashcons _t

let cast ~ty old =
  let my_t = { old with id= ~-1; ty=HasType ty; } in
  let t = H.hashcons my_t in
  if t == my_t then begin
    if get_flag old flag_ground && ground ty
      then set_flag t flag_ground true;
  end;
  t

let change_kind ~kind old =
  let my_t = { old with kind; id= ~-1; } in
  H.hashcons my_t

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
      (match t.ty with
        | NoType -> true
        | HasType ty -> closed depth ty)
      &&
      match view t with
      | BVar i -> i < depth
      | Bind(_, varty, t') -> closed depth varty && closed (depth+1) t'
      | Const _
      | Var _ -> true
      | Record l -> List.for_all (fun (_,t') -> closed depth t') l
      | App (f, l) -> closed depth f && List.for_all (closed depth) l
    in
    closed depth t

  (* check whether t contains the De Bruijn symbol n *)
  let rec contains t n =
    (match t.ty with
      | NoType -> false
      | HasType ty -> contains ty n)
    ||
    match view t with
    | BVar i -> i = n
    | Const _
    | Var _ -> false
    | Bind (_, varty, t') -> contains varty n || contains t' (n+1)
    | Record l -> List.exists (fun (_,t') -> contains t' n) l
    | App (f, l) ->
      contains f n || List.exists (fun t' -> contains t' n) l

  (* shift the non-captured De Bruijn indexes in the term by n *)
  let shift ?(depth=0) n t =
    (* traverse the term, looking for non-captured DB indexes.
       [depth] is the number of binders on the path from the root of the
       term, to the current position. *)
    let rec recurse depth t =
      match t.ty with
      | NoType ->
        assert (t == tType);
        t
      | HasType ty ->
        let ty = recurse depth ty in
        match t.term with
          | _ when ground t -> t
          | Var i -> var ~kind:t.kind ~ty i
          | Const s -> const ~kind:t.kind ~ty s
          | BVar i ->
            if i >= depth
              then (* shift *)
                bvar ~kind:t.kind ~ty (i + n)
              else bvar ~kind:t.kind ~ty i
          | Bind (s, varty, t') ->
            let varty' = recurse depth varty in
            let t' = recurse (depth+1) t' in
            bind ~kind:t.kind ~ty ~varty:varty' s t'
          | Record l ->
            record ~kind:t.kind ~ty
              (List.map (fun (s,t') -> s, recurse depth t') l)
          | App (f, l) ->
            app ~kind:t.kind ~ty (recurse depth f) (List.map (recurse depth) l)
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
      match t.ty with
      | NoType ->
        assert (t == tType);
        t
      | HasType ty ->
        let ty = recurse depth ty in
        match view t with
          | _ when ground t -> t
          | Var i -> var ~kind:t.kind ~ty i
          | BVar i ->
            if i >= depth
              then (* unshift this free De Bruijn index *)
                bvar ~kind:t.kind ~ty (i-n)
              else bvar ~kind:t.kind ~ty i
          | Const s -> const ~kind:t.kind ~ty s
          | Bind (s, varty, t') ->
            let varty' = recurse depth varty in
            let t' = recurse (depth+1) t' in
            bind ~kind:t.kind ~ty ~varty:varty' s t'
          | Record l ->
            record ~kind:t.kind ~ty
              (List.map (fun (s,t') -> s, recurse depth t') l)
          | App (f, l) ->
            app ~kind:t.kind ~ty (recurse depth f) (List.map (recurse depth) l)
    in
    recurse depth t

  let replace ?(depth=0) t ~sub =
    (* recurse and replace [sub]. *)
    let rec recurse depth t =
      match t.ty with
      | NoType ->
        assert (t == tType);
        t
      | HasType ty ->
        let ty = recurse depth ty in
        match view t with
          | _ when eq t sub ->
            bvar ~kind:t.kind ~ty depth  (* replace *)
          | Var i -> var ~kind:t.kind ~ty i
          | BVar i -> bvar ~kind:t.kind ~ty i
          | Const s -> const ~kind:t.kind ~ty s
          | Bind (s, varty, t') ->
            let varty' = recurse depth varty in
            let t' = recurse (depth+1) t' in
            bind ~kind:t.kind ~ty ~varty:varty' s t'
          | Record l ->
            record ~kind:t.kind ~ty
              (List.map (fun (s,t') -> s, recurse depth t') l)
          | App (f, l) ->
            app ~kind:t.kind ~ty (recurse depth f) (List.map (recurse depth) l)
    in
    recurse depth t

  let from_var ?depth t ~var =
    assert (is_var var);
    replace ?depth t ~sub:var

  let eval env t =
    let rec eval env t =
      match t.ty with
      | NoType ->
        assert (t == tType);
        t
      | HasType ty ->
        let ty = eval env ty in
        match view t with
          | _ when ground t -> t
          | Var i -> var ~kind:t.kind ~ty i
          | Const s -> const ~kind:t.kind ~ty s
          | BVar i ->
            begin match DBEnv.find env i with
              | None -> bvar ~kind:t.kind ~ty i
              | Some t' ->
                (* need to shift this term, because we crossed [i] binder
                    from the scope [t'] was defined in. *)
                shift i t'
            end
          | Bind (s, varty, t') ->
            let varty' = eval env varty in
            let t' = eval (DBEnv.push_none env) t' in
            bind ~kind:t.kind ~ty ~varty:varty' s t'
          | Record l ->
            record ~kind:t.kind ~ty
              (List.map (fun (s,t') -> s, eval env t') l)
          | App (f, l) ->
            app ~kind:t.kind ~ty (eval env f) (List.map (eval env) l)
    in
    eval env t
end

let bind_vars ~kind ~ty s vars t =
  List.fold_right
    (fun v t ->
      assert (is_var v);
      let t' = DB.replace (DB.shift 1 t) ~sub:v in
      let varty = match v.ty with
        | NoType -> failwith "ScopedTerm.bind_vars: variable has no type"
        | HasType ty -> ty
      in
      bind ~kind ~ty ~varty s t')
    vars t

(** {3 Iterators} *)

module Seq = struct
  let rec vars t k = match view t with
    | _ when ground t -> ()
    | Var _ -> k t
    | BVar _
    | Const _ -> ()
    | App (head, l) -> vars head k; _vars_list l k
    | Record l -> List.iter (fun (s,t') -> vars t' k) l
    | Bind (_, varty, t') -> vars varty k; vars t' k
  and _vars_list l k = match l with
    | [] -> ()
    | t::l' -> vars t k; _vars_list l' k

  let rec subterms t k =
    k t;
    match view t with
    | Var _
    | BVar _
    | Const _ -> ()
    | Bind (_, varty, t') -> subterms varty k; subterms t' k
    | Record l -> List.iter (fun (s,t') -> subterms t' k) l
    | App(_, l) -> List.iter (fun t' -> subterms t' k) l

  let subterms_depth t k =
    let rec recurse depth t =
      k (t, depth);
      match view t with
      | App (_,l) ->
        let depth' = depth + 1 in
        List.iter (fun t' -> recurse depth' t') l
      | Record l -> List.iter (fun (s,t') -> recurse depth t') l
      | Bind (_, varty, t') -> recurse depth varty; recurse (depth+1) t'
      | _ -> ()
    in
    recurse 0 t

  let rec symbols t k = match view t with
    | Var _ | BVar _ -> ()
    | Const s -> k s
    | App (head, l) -> symbols head k; _symbols_list l k
    | Record l -> List.iter (fun (s,t') -> symbols t' k) l
    | Bind (s, varty, t') -> k s; symbols varty k; symbols t' k
  and _symbols_list l k = match l with
    | [] -> ()
    | t::l' -> symbols t k; _symbols_list l' k

  let rec types t k =
    begin match t.ty with
    | NoType -> ()
    | HasType ty -> k ty
    end;
    match view t with
    | Var _ | BVar _ | Const _ -> ()
    | App (head, l) -> types head k; List.iter (fun t' -> types t' k) l
    | Record l -> List.iter (fun (s,t') -> types t' k) l
    | Bind (_, _, t') -> types t' k

  let max_var seq =
    let r = ref 0 in
    seq (fun t -> match view t with Var i -> r := max i !r | _ -> ());
    !r

  let min_var seq =
    let r = ref max_int in
    seq (fun t -> match view t with Var i -> r := min i !r | _ -> ());
    !r

  let add_set = Sequence.fold (fun set t -> Set.add t set)

  let add_tbl tbl = Sequence.iter (fun t -> Tbl.replace tbl t ())
end

(** {3 Positions} *)

module Pos = struct
  let rec at t pos = match view t, pos with
    | _, [] -> t
    | (Var _ | BVar _), _::_ -> invalid_arg "wrong position in term"
    | Bind(_, _, t'), 0::subpos -> at t' subpos
    | App (t, _), 0::subpos -> at t subpos
    | App (_, l), n::subpos when n <= List.length l ->
      at (List.nth l (n-1)) subpos
    | _ -> invalid_arg "index too high for subterm"

  let rec replace t pos ~by = match t.ty, view t, pos with
    | _, _, [] -> by
    | _, (Var _ | BVar _), _::_ -> invalid_arg "wrong position in term"
    | HasType ty, Bind (s, varty, t'), 0::subpos ->
      bind ~kind:t.kind ~ty ~varty s (replace t' subpos ~by)
    | HasType ty, App (f, l), 0::subpos ->
      app ~kind:t.kind ~ty (replace f subpos ~by) l
    | HasType ty, App (f, l), n::subpos when n <= List.length l ->
      let t' = replace (List.nth l (n-1)) subpos ~by in
      let l' = Util.list_set l n t' in
      app ~kind:t.kind ~ty f l'
    | _ -> invalid_arg "index too high for subterm"
end

(* [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let rec replace t ~old ~by = match t.ty, view t with
  | _ when eq t old -> by
  | HasType ty, Bind (s, varty, t') ->
    bind ~kind:t.kind ~ty ~varty s (replace t' ~old ~by)
  | HasType ty, Record l ->
    record ~kind:t.kind ~ty
      (List.map (fun (s,t') -> s, replace t' ~old ~by) l)
  | HasType ty, App (f, l) ->
    let f' = replace f ~old ~by in
    let l' = List.map (fun t' -> replace t' ~old ~by) l in
    app ~kind:t.kind ~ty f' l'
  | _ -> t

(** {3 Variables} *)

let close_vars ~kind ~ty s t =
  let vars = Seq.add_set Set.empty (Seq.vars t) in
  bind_vars ~kind ~ty s (Set.elements vars) t

(** {3 Misc} *)

let rec size t = match view t with
  | Const _
  | Var _
  | BVar _ -> 1
  | Bind (_, _, t') -> 1 + size t'
  | Record l -> List.fold_left (fun acc (_,t') -> acc + size t') 1 l
  | App (head, l) -> _size_list (1 + size head) l
and _size_list acc l = match l with
  | [] -> acc
  | t::l' -> _size_list (acc + size t) l'

let rec depth t = match view t with
  | Const _
  | Var _
  | BVar _ -> 1
  | Bind (_, _, t') -> 1 + depth t'
  | Record l -> 1 + List.fold_left (fun acc (_,t') -> max acc (depth t')) 0 l
  | App (head, l) -> 1 + _depth_list (depth head) l
and _depth_list acc l = match l with
  | [] -> acc
  | t::l' -> _depth_list (max acc (depth t)) l'

let rec head t = match view t with
  | BVar _ | Var _ | Record _ -> None
  | Const s
  | Bind (s, _, _) -> Some s
  | App (h, _) -> head h

module PB = Position.Build

let rec _all_pos_rec f vars acc pb t = match view t with
  | Var _ | BVar _ ->
    if vars then f acc t (PB.to_pos pb) else acc
  | Const _ -> f acc t (PB.to_pos pb)
  | App (hd, tl) ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    let acc = _all_pos_rec f vars acc (PB.add pb 0) hd in
    _all_pos_rec_list f vars acc pb tl 1
  | Record l ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    let _, acc = List.fold_left
      (fun (i,acc) (_, t') ->
        i+1, _all_pos_rec f vars acc (PB.add pb i) t')
      (0, acc) l
    in acc
  | Bind (_, _, t') ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    _all_pos_rec f vars acc (PB.add pb 0) t'
and _all_pos_rec_list f vars acc pb l i = match l with
  | [] -> acc
  | t::l' ->
    let acc = _all_pos_rec f vars acc (PB.add pb i) t in
    _all_pos_rec_list f vars acc pb l' (i+1)

let all_positions ?(vars=false) ?(pos=[]) t acc f =
  _all_pos_rec f vars acc (PB.of_pos pos) t

(** {3 IO} *)

let rec pp_depth depth buf t =
  begin match view t with
  | Var i -> Printf.bprintf buf "X%d" i
  | BVar i -> Printf.bprintf buf "Y%d" (depth-i-1)
  | Const s -> Sym.pp buf s
  | Bind (s, varty, t') ->
    Printf.bprintf buf "%a Y%d:%a. %a" Sym.pp s depth
      (pp_depth depth) varty (_pp_surrounded (depth+1)) t'
  | Record [] ->
    Buffer.add_string buf "{}"
  | Record l ->
    Buffer.add_char buf '{';
    List.iteri
      (fun i (s, t') ->
        if i>0 then Buffer.add_string buf ", ";
        Printf.bprintf buf "%s: " s;
        pp_depth depth buf t')
      l;
    Buffer.add_char buf '}'
  | App (f, l) ->
    assert (l <> []);
    _pp_surrounded depth buf f;
    List.iter
      (fun t' ->
        Buffer.add_char buf ' '; _pp_surrounded depth buf t')
      l
  end;
  match t.ty, view t with
    | HasType ty, (Var _ | BVar _) ->
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

(* FIXME
let bij =
  let open Bij in
  let (!!!) = Lazy.force in
  fix
    (fun bij ->
      let bij_bind = lazy (triple !!!bij Sym.bij !!!bij) in
      let bij_var = lazy (pair !!!bij int_) in
      let bij_cst = lazy (pair !!!bij Sym.bij) in
      let bij_at = lazy (triple !!!bij !!!bij (list_ !!!bij)) in
      switch
        ~inject:(fun t -> match view t with
        | BVar i -> "bv", BranchTo (!!!bij_var, (ty t, i))
        | Var i -> "v", BranchTo (!!!bij_var, (ty t, i))
        | Const s -> "c", BranchTo (!!!bij_cst, (ty t, s))
        | Bind (s, t') -> "bind", BranchTo (!!!bij_bind, (ty t, s, t'))
        | App (t, l) -> "a", BranchTo (!!!bij_at, (ty t, t, l)))
        ~extract:(function
        | "bv" -> BranchFrom (!!!bij_var, fun (ty,i) -> bvar ~ty i)
        | "v" -> BranchFrom (!!!bij_var, fun (ty,i) -> var ~ty i)
        | "c" -> BranchFrom (!!!bij_cst, fun (ty,s) -> const ~ty s)
        | "bind" -> BranchFrom (!!!bij_bind, fun (ty,s,t') -> bind ~ty s t')
        | "a" -> BranchFrom (!!!bij_at, fun (ty, t, l) -> app ~ty t l)
        | _ -> raise (DecodingError "expected Term")))
*)
