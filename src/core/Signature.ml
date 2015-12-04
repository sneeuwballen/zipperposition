
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Signature} *)

type t = Type.t ID.Map.t
(** A signature maps symbols to their sort *)

let empty = ID.Map.empty

let is_empty = ID.Map.is_empty

let singleton = ID.Map.singleton

let mem signature s = ID.Map.mem s signature

let find signature s =
  try Some (ID.Map.find s signature)
  with Not_found -> None

let find_exn signature s =
  ID.Map.find s signature

exception AlreadyDeclared of ID.t * Type.t * Type.t

let () = Printexc.register_printer
  (function
    | AlreadyDeclared (id, old, new_) ->
        Some (
          CCFormat.sprintf
          "@[<2>symbol %a@ is already declared with type @[%a@],@ \
          which is not compatible with @[%a@]@]"
          ID.pp id  Type.pp old Type.pp new_ )
    | _ -> None)

let declare signature id ty =
  try
    let ty' = find_exn signature id in
    raise (AlreadyDeclared (id, ty', ty))
  with Not_found ->
    if not (ScopedTerm.DB.closed (ty : Type.t :> ScopedTerm.t))
    then raise (Invalid_argument "Signature.declare: non-closed type");
    ID.Map.add id ty signature

let cardinal signature = ID.Map.cardinal signature

let arity signature s =
  let ty = find_exn signature s in
  match Type.arity ty with
  | Type.NoArity ->
      failwith (CCFormat.sprintf "symbol %a has ill-formed type %a" ID.pp s Type.pp ty)
  | Type.Arity (a,b) -> a, b

let is_ground signature =
  ID.Map.for_all (fun _ ty -> Type.is_ground ty) signature

let merge s1 s2 =
  ID.Map.merge
    (fun s t1 t2 -> match t1, t2 with
       | None, None -> assert false
       | Some ty1, Some ty2 ->
           if Type.equal ty1 ty2
           then Some ty1
           else raise (AlreadyDeclared (s, ty1, ty2))
       | Some s1, None -> Some s1
       | None, Some s2 -> Some s2)
    s1 s2

let filter s p = ID.Map.filter p s

let diff s1 s2 =
  ID.Map.merge
    (fun _ ty1 ty2 -> match ty1, ty2 with
       | Some ty1, None -> Some ty1
       | Some _, Some _
       | None, Some _
       | None, None -> None)
    s1 s2

let well_founded s =
  ID.Map.exists
    (fun _ ty -> match Type.arity ty with
       | Type.Arity (_, 0) -> true
       | _ -> false)
    s

module Seq = struct
  let symbols s =
    ID.Map.to_seq s |> Sequence.map fst

  let types s =
    ID.Map.to_seq s |> Sequence.map snd

  let to_seq = ID.Map.to_seq

  let of_seq = ID.Map.of_seq
end

let to_set s = Seq.symbols s |> ID.Set.of_seq

let to_list s = Seq.to_seq s |> Sequence.to_rev_list

let of_list s = Sequence.of_list s |> Seq.of_seq

let iter s f =
  ID.Map.iter f s

let fold s acc f =
  ID.Map.fold (fun s ty acc -> f acc s ty) s acc

let is_bool signature s =
  let rec is_bool ty = match Type.view ty with
    | Type.Builtin Type.Prop -> true
    | Type.Fun (_, ret) -> is_bool ret
    | Type.Forall ty' -> is_bool ty'
    | _ -> false
  in
  is_bool (find_exn signature s)

let is_not_bool signature s =
  not (is_bool signature s)

(** {2 IO} *)

let pp out s =
  let pp_pair out (s,ty) =
    Format.fprintf out "@[<hov2>%a:@ %a@]" ID.pp s Type.pp ty
  in
  Format.fprintf out "{@[<hv>";
  CCFormat.seq pp_pair out (Seq.to_seq s);
  Format.fprintf out "@]}";
  ()

let to_string = CCFormat.to_string pp

module Builtin = struct
  let prop2 = Type.([prop; prop] ==> prop)
  let prop1 = Type.([prop] ==> prop)
  let prop2poly = Type.(forall ([bvar 0; bvar 0] ==> prop))
  let ty_1_to_int = Type.(forall ([bvar 0] ==> Type.int))
  let ty2op = Type.(forall ([bvar 0; bvar 0] ==> bvar 0))
  let ty1op = Type.(forall ([bvar 0] ==> bvar 0))
  let ty2op_to_i = Type.([int;int] ==> int)

  let ty_exn = function
    | Builtin.True -> Type.prop
    | Builtin.False -> Type.prop
    | Builtin.Eq -> prop2poly
    | Builtin.Neq -> prop2poly
    | Builtin.Not -> prop1
    | Builtin.Imply -> prop2
    | Builtin.And -> prop2
    | Builtin.Or -> prop2
    | Builtin.Equiv -> prop2
    | Builtin.Xor -> prop2
    | Builtin.Less -> prop2poly
    | Builtin.Lesseq -> prop2poly
    | Builtin.Greater -> prop2poly
    | Builtin.Greatereq -> prop2poly
    | Builtin.Uminus -> ty1op
    | Builtin.Sum -> ty2op
    | Builtin.Difference -> ty2op
    | Builtin.Product -> ty2op
    | Builtin.Quotient -> ty2op
    | Builtin.Quotient_e -> ty2op_to_i
    | Builtin.Quotient_f -> ty2op_to_i
    | Builtin.Quotient_t -> ty2op_to_i
    | Builtin.Remainder_e -> ty2op_to_i
    | Builtin.Remainder_f -> ty2op_to_i
    | Builtin.Remainder_t -> ty2op_to_i
    | Builtin.Floor -> ty_1_to_int
    | Builtin.Ceiling -> ty_1_to_int
    | Builtin.Round -> ty_1_to_int
    | Builtin.Truncate -> ty_1_to_int
    | Builtin.To_int -> Type.(forall ([bvar 0] ==> int))
    | Builtin.To_rat -> Type.(forall ([bvar 0] ==> rat))
    | Builtin.Is_int -> Type.(forall ([bvar 0] ==> prop))
    | Builtin.Is_rat -> Type.(forall ([bvar 0] ==> prop))
    | _ -> invalid_arg "Sig.Builtin.ty_exn"

  let ty x = try Some (ty_exn x) with _ -> None
end
