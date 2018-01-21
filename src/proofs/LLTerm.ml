
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Terms For Proofs} *)

open Logtk

module Fmt = CCFormat
module I_map = Util.Int_map

let errorf msg = Util.errorf ~where:"llterm" msg

module Int_op = struct
  type t = Leq0 | Geq0 | Lt0 | Gt0 | Eq0 | Neq0 | Divisible_by of Z.t | Not_div_by of Z.t
  let equal a b = match a, b with
    | Divisible_by a, Divisible_by b -> Z.equal a b
    | Not_div_by a, Not_div_by b -> Z.equal a b
    | Divisible_by _, _ | _, Divisible_by _
    | Not_div_by _, _ | _, Not_div_by _ -> false
    | _ -> a=b
  let hash = function
    | Divisible_by n -> Hash.combine2 10 (Z.hash n)
    | Not_div_by n -> Hash.combine2 20 (Z.hash n)
    | x -> Hash.poly x
  let not = function
    | Leq0 -> Gt0 | Geq0 -> Lt0
    | Lt0 -> Geq0 | Gt0 -> Leq0
    | Eq0 -> Neq0 | Neq0 -> Eq0
    | Divisible_by n -> Not_div_by n | Not_div_by n -> Divisible_by n
  let pp out = function
    | Leq0 -> Fmt.fprintf out "=< 0"
    | Geq0 -> Fmt.fprintf out ">= 0"
    | Lt0 -> Fmt.fprintf out "< 0"
    | Gt0 -> Fmt.fprintf out "> 0"
    | Eq0 -> Fmt.fprintf out "= 0"
    | Neq0 -> Fmt.fprintf out "!= 0"
    | Divisible_by n -> Fmt.fprintf out "div_by %a" Z.pp_print n
    | Not_div_by n -> Fmt.fprintf out "not_div_by %a" Z.pp_print n
end

module Rat_op = struct
  type t = Leq0 | Geq0 | Lt0 | Gt0 | Eq0 | Neq0
  let equal : t -> t -> bool = (=)
  let hash : t -> int = Hash.poly
  let pp out = function
    | Leq0 -> Fmt.fprintf out "=< 0"
    | Geq0 -> Fmt.fprintf out ">= 0"
    | Lt0 -> Fmt.fprintf out "< 0"
    | Gt0 -> Fmt.fprintf out "> 0"
    | Eq0 -> Fmt.fprintf out "= 0"
    | Neq0 -> Fmt.fprintf out "!= 0"
  let not = function
    | Leq0 -> Gt0 | Geq0 -> Lt0
    | Lt0 -> Geq0 | Gt0 -> Leq0
    | Eq0 -> Neq0 | Neq0 -> Eq0
end

type t = {
  view: view;
  ty: t option;
  mutable id: int; (** unique ID *)
}

and view =
  | Type
  | Const of ID.t
  | App of t * t (** curried application *)
  | Arrow of t * t (** functional arrow *)
  | Var of var (** bound var *)
  | Bind of {
      binder: Binder.t;
      ty_var: t;
      body: t;
    }
  | AppBuiltin of Builtin.t * t list
  | Ite of t * t * t
  | Int_pred of Z.t linexp * Int_op.t
  | Rat_pred of Q.t linexp * Rat_op.t

and var = t HVar.t
and 'a linexp = {
  const: 'a;
  coeffs: (t * 'a) I_map.t;
}

type term = t
type ty = t

module type NUM = sig
  type t
  val equal : t -> t -> bool
  val zero : t
  val one : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
  val pp_print : t CCFormat.printer
end

module type LINEXP = sig
  type num
  type t = num linexp
  val zero : t
  val is_zero : t -> bool
  val is_const : t -> bool
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : num -> t -> t
  val add : num -> term -> t -> t
  val const : num -> t
  val monomial : num -> term -> t
  val monomial1 : term -> t
  val equal : t -> t -> bool
  val map : (term -> term) -> t -> t
  val subterms : t -> term Sequence.t
  val pp : term CCFormat.printer -> t CCFormat.printer
end

module Make_linexp(N : NUM) = struct
  type num = N.t
  type t = num linexp
  let zero : t = {const=N.zero; coeffs=I_map.empty}
  let is_const e = I_map.is_empty e.coeffs
  let is_zero e = is_const e && N.equal N.zero e.const
  let merge_ f a b : t = {
    const=f a.const b.const;
    coeffs=I_map.merge_safe a.coeffs b.coeffs
      ~f:(fun _ o -> match o with
        | `Left n | `Right n -> Some n
        | `Both ((t,a),(t2,b)) ->
          assert (t==t2);
          let c = f a b in
          if N.equal N.zero c then None else Some (t,c))
  }

  let (+) = merge_ N.(+)
  let (-) = merge_ N.(-)
  let ( * ) c e : t =
    if N.equal N.zero c then zero
    else {const=N.(c * e.const); coeffs=I_map.map (fun (t,n) -> t, N.(c*n)) e.coeffs}
  let const c = {const=c; coeffs=I_map.empty}
  let add c t e =
    let _, n = I_map.get_or ~default:(t,N.zero) t.id e.coeffs in
    let n = N.(n + c) in
    let coeffs =
      if N.equal N.zero n
      then I_map.remove t.id e.coeffs else I_map.add t.id (t,n) e.coeffs
    in
    {e with coeffs;}
  let monomial c t = {const=N.zero; coeffs=I_map.singleton t.id (t,c)}
  let monomial1 t = {const=N.zero; coeffs=I_map.singleton t.id (t,N.one)}
  let equal e1 e2 =
    N.equal e1.const e2.const &&
    I_map.equal (CCEqual.pair (==) N.equal) e1.coeffs e2.coeffs
  let hash hash_t e =
    let hash_n n = Hash.string @@ N.to_string n in
    Hash.combine3 10
      (hash_n e.const)
      (Hash.seq (Hash.pair hash_t hash_n) @@ I_map.values e.coeffs)
  let map f e =
    I_map.fold (fun _ (t,n) acc -> add n (f t) acc) e.coeffs (const e.const)

  let subterms e = I_map.values e.coeffs |> Sequence.map fst

  let pp pp_t out (e:t): unit =
    if is_const e then N.pp_print out e.const
    else (
      let pp_const out () =
        if N.equal N.zero e.const then ()
        else Fmt.fprintf out "@ + %a" N.pp_print e.const
      and pp_pair out (t,c) =
        if N.equal N.one c then pp_t out t
        else Fmt.fprintf out "@[<2>%a@ @<1>· %a@]" N.pp_print c pp_t t
      in
      Fmt.fprintf out "(@[<hv>%a%a@])"
        Fmt.(seq ~sep:(return "@ + ") pp_pair)
        (I_map.values e.coeffs) pp_const ()
    )
end

module Linexp_int = Make_linexp(Z)
module Linexp_rat = Make_linexp(Q)

let[@inline] view t = t.view
let[@inline] ty t = t.ty
let[@inline] ty_exn t = match t.ty with Some ty -> ty | None -> assert false
let[@inline] equal (a:t) (b:t) : bool = a == b
let[@inline] hash (a:t) : int = CCHash.int a.id
let[@inline] compare (a:t) (b:t) : int = CCInt.compare a.id b.id

module H_cons = Hashcons.Make(struct
    type t = term
    let equal a b =
      begin match a.ty, b.ty with
        | Some ty1, Some ty2 -> equal ty1 ty2
        | None, None -> true
        | Some _, None
        | None, Some _ -> false
      end &&
      begin match a.view, b.view with
        | Type, Type -> true
        | Const id1, Const id2 -> ID.equal id1 id2
        | App (f1,x1), App (f2,x2) -> equal f1 f2 && equal x1 x2
        | Arrow (a1,b1), Arrow (a2,b2) -> equal a1 a2 && equal b1 b2
        | Var v1, Var v2 -> HVar.equal equal v1 v2
        | Bind b1, Bind b2 ->
          Binder.equal b1.binder b2.binder &&
          equal b1.ty_var b2.ty_var &&
          equal b1.body b2.body
        | AppBuiltin (b1,l1), AppBuiltin (b2,l2) ->
          Builtin.equal b1 b2 &&
          CCList.equal equal l1 l2
        | Ite (a1,b1,c1), Ite (a2,b2,c2) ->
          equal a1 a2 && equal b1 b2 && equal c1 c2
        | Int_pred (l1,o1), Int_pred (l2,o2) ->
          Int_op.equal o1 o2 && Linexp_int.equal l1 l2
        | Rat_pred (l1,o1), Rat_pred (l2,o2) ->
          Rat_op.equal o1 o2 && Linexp_rat.equal l1 l2
        | Type, _
        | Const _, _
        | App _, _
        | Arrow _, _
        | Var _, _
        | Bind _, _
        | AppBuiltin _, _
        | Ite _, _
        | Int_pred _, _
        | Rat_pred _, _
          -> false
      end

    let hash (a:t) : int = match a.view with
      | Type -> 1
      | Const id -> CCHash.combine2 10 (ID.hash id)
      | Var v -> CCHash.combine2 20 (HVar.hash v)
      | App (f,x) -> CCHash.combine3 30 (hash f) (hash x)
      | Arrow (a,b) -> CCHash.combine3 35 (hash a) (hash b)
      | Bind b ->
        CCHash.combine4 40 (Binder.hash b.binder) (hash b.ty_var) (hash b.body)
      | AppBuiltin (b,l) ->
        CCHash.combine3 50 (Builtin.hash b) (CCHash.list hash l)
      | Ite (a,b,c) ->
        CCHash.combine4 60 (hash a)(hash b)(hash c)
      | Int_pred (l,o) -> Hash.combine3 70 (Linexp_int.hash hash l) (Int_op.hash o)
      | Rat_pred (l,o) -> Hash.combine3 80 (Linexp_rat.hash hash l) (Rat_op.hash o)

    let tag (i:int) (t:t) : unit =
      assert (t.id = -1);
      t.id <- i
  end)

let rec pp_rec depth out (t:t) = match view t with
  | Type -> Fmt.string out "type"
  | Const id -> ID.pp_fullc out id
  | App (f,a) -> Fmt.fprintf out "@[%a@ %a@]" (pp_rec depth) f (pp_rec_inner depth) a
  | Arrow (a,b) ->
    Fmt.fprintf out "@[%a@ @<1>→ %a@]" (pp_rec_inner depth) a (pp_rec depth) b
  | Var v -> Format.fprintf out "Y%d" (depth-HVar.id v-1)
  | AppBuiltin (Builtin.Box_opaque, [t]) ->
    Format.fprintf out "@<1>⟦@[%a@]@<1>⟧" (pp_rec depth) t
  | AppBuiltin (b, [a]) when Builtin.is_prefix b ->
    Format.fprintf out "@[<1>%a@ %a@]" Builtin.pp b (pp_rec_inner depth) a
  | AppBuiltin (b, ([t1;t2] | [_;t1;t2])) when Builtin.fixity b = Builtin.Infix_binary ->
    Format.fprintf out "@[<1>%a %a@ %a@]"
      (pp_rec_inner depth) t1 Builtin.pp b (pp_rec_inner depth) t2
  | AppBuiltin (b, l) when Builtin.is_infix b && List.length l > 0 ->
    Format.fprintf out "@[<hv>%a@]" (pp_infix_ depth b) l
  | AppBuiltin (b, []) -> Builtin.pp out b
  | AppBuiltin (b, l) ->
    Format.fprintf out "(@[<hv>%a@ %a@])"
      Builtin.pp b (Util.pp_list (pp_rec_inner depth)) l
  | Bind {ty_var;binder;body} ->
    Fmt.fprintf out "@[%a (@[Y%d:%a@]).@ %a@]"
      Binder.pp binder
      depth (pp_rec depth) ty_var
      (pp_rec @@ depth+1) body
  | Ite (a,b,c) ->
    Fmt.fprintf out "@[<hv2>ite %a@ %a@ %a@]"
      (pp_rec_inner depth) a
      (pp_rec_inner depth) b
      (pp_rec_inner depth) c
  | Int_pred (l,o) ->
    Fmt.fprintf out "(@[%a@ %a@])" (Linexp_int.pp (pp_rec_inner depth)) l Int_op.pp o
  | Rat_pred (l,o) ->
    Fmt.fprintf out "(@[%a@ %a@])" (Linexp_rat.pp (pp_rec_inner depth)) l Rat_op.pp o
and pp_rec_inner depth out t = match view t with
  | App _ | Bind _ | AppBuiltin (_,_::_) | Arrow _ | Ite _ ->
    Fmt.fprintf out "(%a)@{<Black>/%d@}" (pp_rec depth) t t.id
  | Type | Const _ | Var _ | AppBuiltin (_,[]) | Int_pred _ | Rat_pred _ ->
    Fmt.fprintf out "%a@{<Black>/%d@}" (pp_rec depth) t t.id
and pp_infix_ depth b out l = match l with
  | [] -> assert false
  | [t] -> pp_rec_inner depth out t
  | t :: l' ->
    Format.fprintf out "@[%a@]@ %a %a"
      (pp_rec_inner depth) t Builtin.pp b (pp_infix_ depth b) l'

let pp = pp_rec 0
let pp_inner = pp_rec_inner 0

let subterms (t:t) (k:t -> unit) : unit =
  let rec aux t =
    k t;
    CCOpt.iter aux (ty t);
    begin match view t with
      | Type | Const _ | Var _ -> ()
      | App (f,a) -> aux f; aux a
      | Arrow (a,b) -> aux a; aux b
      | Bind { body;_ } -> aux body
      | AppBuiltin  (_,l) -> List.iter aux l
      | Ite (a,b,c) -> aux a; aux b; aux c
      | Int_pred (l,_) -> Linexp_int.subterms l k
      | Rat_pred (l,_) -> Linexp_rat.subterms l k
    end
  in
  aux t

let[@inline] mk_ view ty : t =
  let t = {view; ty; id= -1; } in
  H_cons.hashcons t

let t_type = mk_ Type None
let[@inline] var v = mk_ (Var v) (Some (HVar.ty v))
let[@inline] const ~ty id = mk_ (Const id) (Some ty)
let prop = mk_ (AppBuiltin (Builtin.Prop,[])) (Some t_type)

let[@inline] is_type t : bool = match ty t with
  | Some ty -> ty == t_type
  | None -> false

let ite a b c =
  begin match ty b, ty c with
    | Some ty1, Some ty2 when equal ty1 ty2 ->
      mk_ (Ite (a,b,c)) (ty b)
    | _ ->
      errorf "type error:@ cannot build `@[ite %a %a %a@]`@ incompatible types"
        pp a pp b pp c
  end

let[@inline] app_ f x ~ty = mk_ (App (f,x)) (Some ty)
let[@inline] arrow_ a b = mk_ (Arrow (a,b)) (Some t_type)

let[@inline] bind_ ~ty binder ~ty_var body = mk_ (Bind {binder;ty_var;body}) (Some ty)

let id_eta_ = ID.make "test_eta_" (* privat to {!as_eta_expansion} *)

let[@inline] app_builtin ~ty b l =
  let mk_ b l = mk_ (AppBuiltin(b,l)) (Some ty) in
  begin match b, l with
    | Builtin.Not, [f'] ->
      begin match view f' with
        | AppBuiltin (Builtin.Eq,l) -> mk_ Builtin.Neq l
        | AppBuiltin (Builtin.Neq,l) -> mk_ Builtin.Eq l
        | AppBuiltin (Builtin.Not,[t]) -> t
        | AppBuiltin (Builtin.True,[]) -> mk_ Builtin.False []
        | AppBuiltin (Builtin.False,[]) -> mk_ Builtin.True []
        | _ -> mk_ b l
      end
    | _ -> mk_ b l
  end

let[@inline] builtin ~ty b = app_builtin ~ty b []

let bool = builtin ~ty:t_type Builtin.Prop
let true_ = builtin ~ty:bool Builtin.True
let false_ = builtin ~ty:bool Builtin.False
let of_bool b = if b then true_ else false_

let int_pred l o =
  if Linexp_int.is_const l then (
    let module O = Int_op in
    let n = l.const in
    begin match o with
      | O.Leq0 -> Z.sign n <= 0
      | O.Geq0 -> Z.sign n >= 0
      | O.Lt0 -> Z.sign n < 0
      | O.Gt0 -> Z.sign n > 0
      | O.Eq0 -> Z.sign n = 0
      | O.Neq0 -> Z.sign n <> 0
      | O.Divisible_by k -> Z.equal Z.zero (Z.rem n k)
      | O.Not_div_by k -> not (Z.equal Z.zero (Z.rem n k))
    end |> of_bool
  ) else mk_ (Int_pred (l,o)) (Some prop)

let rat_pred l o =
  if Linexp_rat.is_const l then (
    let module O = Rat_op in
    let n = l.const in
    begin match o with
      | O.Leq0 -> Q.sign n <= 0
      | O.Geq0 -> Q.sign n >= 0
      | O.Lt0 -> Q.sign n < 0
      | O.Gt0 -> Q.sign n > 0
      | O.Eq0 -> Q.sign n = 0
      | O.Neq0 -> Q.sign n <> 0
    end |> of_bool
  ) else mk_ (Rat_pred (l,o)) (Some prop)

let[@inline] map ~f ~bind:f_bind b_acc t = match view t with
  | Type -> t_type
  | Var v -> var (HVar.update_ty v ~f:(f b_acc))
  | Const id -> const ~ty:(f b_acc @@ ty_exn t) id
  | App (hd,a) -> app_ (f b_acc hd) (f b_acc a) ~ty:(f b_acc (ty_exn t))
  | Arrow (a,b) -> arrow_ (f b_acc a) (f b_acc b)
  | Bind b ->
    let b_acc' = f_bind b_acc in
    bind_ b.binder ~ty:(f b_acc @@ ty_exn t) ~ty_var:(f b_acc b.ty_var)
      (f b_acc' b.body)
  | AppBuiltin (b,l) ->
    app_builtin ~ty:(f b_acc @@ ty_exn t) b (List.map (f b_acc) l)
  | Ite (a,b,c) ->
    ite (f b_acc a) (f b_acc b) (f b_acc c)
  | Int_pred (l,o) -> int_pred (Linexp_int.map (f b_acc) l) o
  | Rat_pred (l,o) -> rat_pred (Linexp_rat.map (f b_acc) l) o

(* shift DB indices by [n] *)
let db_shift n (t:t) : t =
  let rec aux k t = match view t with
    | Var v ->
      if HVar.id v >= k then var (HVar.make (HVar.id v+n) ~ty:(HVar.ty v))
      else t
    | _ ->
      map ~f:aux ~bind:succ k t
  in
  aux 0 t

(* replace DB 0 by [sub] in [t] *)
let db_eval ~(sub:t) (t:t) : t =
  let rec aux k t = match view t with
    | Var v ->
      if HVar.id v = k then (
        (* shift [sub] by the number of binders added since binding point *)
        db_shift k sub
      ) else if (HVar.id v > k) then (
        (* shift down by 1, to account for the vanished binder *)
        var (HVar.make (HVar.id v-1) ~ty:(HVar.ty v))
      )
      else t
    | _ ->
      map ~f:aux ~bind:succ k t
  in
  aux 0 t

let bind ~ty binder ~ty_var body = match binder, view body with
  | Binder.Lambda, App (t, {view=Var v; _}) when HVar.id v = 0 ->
    (* eta reduction for λ:
       check if replacing [db0] with a fresh [c] in [t] contains [c] *)
    let c = const id_eta_ ~ty:(HVar.ty v) in
    let t_reduced = db_eval ~sub:c t in
    if subterms t_reduced |> Sequence.exists (equal c)
    then bind_ binder ~ty ~ty_var body
    else t_reduced
  | _ -> bind_ binder ~ty ~ty_var body

let app_ f x = match ty f, ty x with
  | Some {view=Arrow (a,b);_}, Some a' when equal a a' -> app_ f x ~ty:b
  | Some {view=Bind{binder=Binder.ForallTy;ty_var;body};_}, Some ty_x ->
    (* polymorphic type *)
    if not (equal ty_x t_type) then (
      errorf "cannot apply@ `@[<2>%a@ : %a@]`@ to non-type `@[<2>%a@ : %a@]`"
        pp f (Fmt.opt pp) (ty f) pp x (Fmt.opt pp) (ty x)
    );
    if not (equal ty_var t_type) then (
      errorf "ill-formed polymorphic type@ `%a`" pp (ty_exn f);
    );
    let ty = db_eval ~sub:x body in
    app_ f x ~ty
  | _ ->
    errorf "type error: cannot apply `@[<2>%a@ : %a@]`@ to `@[<2>%a@ : %a@]`"
      pp f (Fmt.opt pp) (ty f) pp x (Fmt.opt pp) (ty x)

let app f x = match view f with
  | Bind {binder=Binder.Lambda;ty_var;body} ->
    (* β-reduction *)
    begin match ty x with
      | Some ty_x when equal ty_var ty_x ->
        db_eval ~sub:x body
      | Some ty_x ->
        errorf "type error: cannot apply `%a`@ to `%a : %a`" pp f pp x pp ty_x
      | None -> errorf "type error: cannot apply `%a`@ to `%a : none`" pp f pp x
    end
  | _ -> app_ f x

let rec app_l f = function
  | [] -> f
  | a :: tail -> app_l (app f a) tail

let rec returns (t:t) : ty = match view t with
  | Arrow (_, ret) -> returns ret
  | _ -> t

let arrow a b = match ty a, ty b with
  | Some {view=Type;_}, Some {view=Type;_} -> arrow_ a b
  | _ when a == t_type && returns b == t_type -> arrow_ a b (* type constructor *)
  | _ ->
    errorf "type error: cannot make arrow between non-types@ :from `%a`@ :to `%a`"
      pp a pp b

let rec arrow_l l ret = match l with
  | [] -> ret
  | a :: tail -> arrow a (arrow_l tail ret)

let box_opaque t = app_builtin ~ty:(ty_exn t) Builtin.Box_opaque [t]

let id_eta_ = ID.make "test_eta_" (* privat to {!as_eta_expansion} *)

(* check if [body = t db0], with [db0 ∉ t].
   returns [Some (t shift -1)] if it's the case *)
let as_eta_expansion body : _ option = match view body with
  | App (t, {view=Var v; _}) when HVar.id v = 0 ->
    (* check if replacing [db0] with a fresh [c] in [t] contains [c] *)
    let c = const id_eta_ ~ty:(HVar.ty v) in
    let t_reduced = db_eval ~sub:c t in
    if subterms t_reduced |> Sequence.exists (equal c) then None else Some t_reduced
  | _ -> None

let[@inline] lambda ~ty_var body =
  bind Binder.Lambda ~ty:(arrow ty_var @@ ty_exn body) ~ty_var body

module Form = struct
  type t = term
  type view =
    | True
    | False
    | Or of t list
    | And of t list
    | Not of t
    | Equiv of t * t
    | Xor of t * t
    | Imply of t * t
    | Atom of t
    | Eq of t * t
    | Neq of t * t
    | Int_pred of Z.t linexp * Int_op.t
    | Rat_pred of Q.t linexp * Rat_op.t
    | Forall of {ty_var: ty; body: t}
    | Exists of {ty_var: ty; body: t}

  let pp = pp

  let view t = match view t with
    | AppBuiltin (Builtin.True,[]) -> True
    | AppBuiltin (Builtin.False,[]) -> False
    | AppBuiltin (Builtin.And, l) -> And l
    | AppBuiltin (Builtin.Or, l) -> Or l
    | AppBuiltin (Builtin.Not, [t]) -> Not t
    | AppBuiltin (Builtin.Eq, ([_;t;u]|[t;u])) -> Eq(t,u)
    | AppBuiltin (Builtin.Neq, ([_;t;u]|[t;u])) -> Neq(t,u)
    | AppBuiltin (Builtin.Imply, [t;u]) -> Imply(t,u)
    | AppBuiltin (Builtin.Equiv, [t;u]) -> Equiv(t,u)
    | AppBuiltin (Builtin.Xor, [t;u]) -> Xor(t,u)
    | Bind {binder=Binder.Forall; ty_var; body; _} -> Forall {ty_var;body}
    | Bind {binder=Binder.Exists; ty_var; body; _} -> Exists {ty_var;body}
    | Int_pred (l,o) -> Int_pred (l,o)
    | Rat_pred (l,o) -> Rat_pred (l,o)
    | _ -> Atom t

  let true_ = true_
  let false_ = false_
  let eq a b = app_builtin ~ty:(ty_exn a) Builtin.Eq [a;b]
  let neq a b = app_builtin ~ty:bool Builtin.Neq [a;b]
  let and_ a = app_builtin ~ty:bool Builtin.And a
  let or_ a = app_builtin ~ty:bool Builtin.Or a
  let equiv a b = app_builtin ~ty:(ty_exn a) Builtin.Equiv [a;b]
  let imply a b = app_builtin ~ty:(ty_exn a) Builtin.Imply [a;b]
  let xor a b = app_builtin ~ty:(ty_exn a) Builtin.Xor [a;b]
  let int_pred = int_pred
  let rat_pred = rat_pred
  let forall ~ty_var body = bind Binder.Forall ~ty:bool ~ty_var body
  let exists ~ty_var body = bind Binder.Exists ~ty:bool ~ty_var body

  let not_ a = match view a with
    | Eq (a,b) -> neq a b
    | Neq (a,b) -> eq a b
    | Not f -> f
    | Int_pred (l,o) -> int_pred l (Int_op.not o)
    | Rat_pred (l,o) -> rat_pred l (Rat_op.not o)
    | _ -> app_builtin ~ty:bool Builtin.Not [a]
end

module As_key = struct
  type t = term
  let compare = compare
end

module Set = CCSet.Make(As_key)

module Conv = struct
  module T = TypedSTerm

  type ctx = {
    depth: int; (* depth *)
    vars: (T.t,int * ty) Var.Subst.t; (* depth at binding site, + type *)
  }

  let create() : ctx = {depth=0; vars=Var.Subst.empty}

  let pp_ctx out (c:ctx) : unit =
    Fmt.fprintf out "(@[ctx@ :depth %d@ :vars %a@])"
      c.depth (Var.Subst.pp (Fmt.map fst Fmt.int)) c.vars

  let rec of_term (ctx:ctx) (t:T.t): t = match T.view t with
    | T.Var v ->
      begin match Var.Subst.find ctx.vars v with
        | None -> errorf "cannot find var `%a`@ in %a" Var.pp v pp_ctx ctx
        | Some (i,ty) ->
          assert (ctx.depth > i);
          let ty = db_shift (ctx.depth-i) ty in
          var (HVar.make (ctx.depth-i-1) ~ty)
      end
    | T.AppBuiltin (Builtin.Pseudo_de_bruijn i, []) ->
      (* NOTE: magic here. This was a free De Bruijn index, typically coming
         from rewriting under lambdas. Now we convert it back into a
         normal DB index. *)
      let ty = of_term ctx (T.ty_exn t) in
      var (HVar.make i ~ty)
    | T.Const id ->
      let ty = of_term ctx (T.ty_exn t) in
      const id ~ty
    | T.App (f, l) ->
      let f = of_term ctx f in
      let l = List.map (of_term ctx) l in
      app_l f l
    | T.Ite (a,b,c) ->
      let a = of_term ctx a in
      let b = of_term ctx b in
      let c = of_term ctx c in
      ite a b c
    | T.Bind (b,var,body) ->
      let ty = of_term ctx (T.ty_exn t) in
      let ty_var = of_term ctx (Var.ty var) in
      let ctx = {
        depth=ctx.depth+1;
        vars=Var.Subst.add ctx.vars var (ctx.depth,ty_var)
      } in
      bind b ~ty_var ~ty (of_term ctx body)
    | T.AppBuiltin (Builtin.TType, []) -> t_type
    | T.AppBuiltin (Builtin.Arrow, ret::l) ->
      let ret = of_term ctx ret in
      let l = List.map (of_term ctx) l in
      arrow_l l ret
    | T.AppBuiltin (b, l) ->
      let ty = T.ty_exn t in
      begin match b with
        | (Builtin.Greatereq | Builtin.Lesseq | Builtin.Less
          | Builtin.Greater | Builtin.Eq | Builtin.Neq) when List.exists is_arith l ->
          if List.exists is_int l then (
            conv_int_pred ctx ~ty b l
          ) else (
            assert (List.exists is_rat l);
            conv_rat_pred ctx ~ty b l
          )
        | _ -> conv_builtin ctx ~ty b l
      end
    | T.Let _ -> assert false (* FIXME *)
    | T.Match _ -> assert false (* FIXME? *)
    | T.Multiset _ -> assert false (* FIXME? *)
    | T.Record _ -> assert false (* FIXME? *)
    | T.Meta _ -> assert false

  and is_int t = T.Ty.equal T.Ty.int (T.ty_exn t)
  and is_rat t = T.Ty.equal T.Ty.rat (T.ty_exn t)
  and is_arith t = is_int t || is_rat t

  (* default conv for builtins *)
  and conv_builtin ctx ~ty b l =
    let ty = of_term ctx ty in
    let l = List.map (of_term ctx) l in
    app_builtin ~ty b l

  and conv_int_pred ctx ~ty b l : term =
    let module O = Int_op in
    let op = match b with
      | Builtin.Greatereq -> O.Geq0
      | Builtin.Lesseq -> O.Leq0
      | Builtin.Less -> O.Lt0
      | Builtin.Greater -> O.Gt0
      | Builtin.Eq -> O.Eq0
      | Builtin.Neq -> O.Neq0
      | _ -> assert false
    in
    match l with
      | [_; a; b] | [a;b] ->
        let a = conv_int_linexp ctx a in
        let b = conv_int_linexp ctx b in
        int_pred Linexp_int.(a - b) op
      | _ -> conv_builtin ctx ~ty b l

  and conv_rat_pred ctx ~ty b l : term =
    let module O = Rat_op in
    let op = match b with
      | Builtin.Greatereq -> O.Geq0
      | Builtin.Lesseq -> O.Leq0
      | Builtin.Less -> O.Lt0
      | Builtin.Greater -> O.Gt0
      | Builtin.Eq -> O.Eq0
      | Builtin.Neq -> O.Neq0
      | _ -> assert false
    in
    match l with
      | [_; a; b] | [a;b] ->
        let a = conv_rat_linexp ctx a in
        let b = conv_rat_linexp ctx b in
        rat_pred Linexp_rat.(a - b) op
      | _ -> conv_builtin ctx ~ty b l

  and conv_int_linexp ctx t : Linexp_int.t = match T.view t with
    | T.AppBuiltin (Builtin.Int z, []) -> Linexp_int.const z
    | T.AppBuiltin (Builtin.Sum, [_;a;b]) ->
      Linexp_int.(conv_int_linexp ctx a + conv_int_linexp ctx b)
    | T.AppBuiltin (Builtin.Difference, [_;a;b]) ->
      Linexp_int.(conv_int_linexp ctx a - conv_int_linexp ctx b)
    | T.AppBuiltin (Builtin.Product, [_;a;b]) ->
      begin match T.view a, T.view b with
        | T.AppBuiltin (Builtin.Int n,[]), _ ->
          Linexp_int.(n * conv_int_linexp ctx b)
        | _, T.AppBuiltin (Builtin.Int n,[]) ->
          Linexp_int.(n * conv_int_linexp ctx a)
        | _ -> Linexp_int.monomial1 (of_term ctx t)
      end
    | _ -> Linexp_int.monomial1 (of_term ctx t)

  and conv_rat_linexp ctx t : Linexp_rat.t = match T.view t with
    | T.AppBuiltin (Builtin.Rat z, []) -> Linexp_rat.const z
    | T.AppBuiltin (Builtin.Sum, [_;a;b]) ->
      Linexp_rat.(conv_rat_linexp ctx a + conv_rat_linexp ctx b)
    | T.AppBuiltin (Builtin.Difference, [_;a;b]) ->
      Linexp_rat.(conv_rat_linexp ctx a - conv_rat_linexp ctx b)
    | T.AppBuiltin (Builtin.Product, [_;a;b]) ->
      begin match T.view a, T.view b with
        | T.AppBuiltin (Builtin.Rat n,[]), _ ->
          Linexp_rat.(n * conv_rat_linexp ctx b)
        | _, T.AppBuiltin (Builtin.Rat n,[]) ->
          Linexp_rat.(n * conv_rat_linexp ctx a)
        | _ -> Linexp_rat.monomial1 (of_term ctx t)
      end
    | _ -> Linexp_rat.monomial1 (of_term ctx t)
end
