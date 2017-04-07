
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
    if not (InnerTerm.DB.closed (ty : Type.t :> InnerTerm.t))
    then raise (Invalid_argument "Signature.declare: non-closed type");
    ID.Map.add id ty signature

let cardinal = ID.Map.cardinal

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
  let add_seq = ID.Map.add_seq
  let of_seq = ID.Map.of_seq
end

let to_set s = Seq.symbols s |> ID.Set.of_seq

let to_list = ID.Map.to_list
let add_list = ID.Map.add_list
let of_list = ID.Map.of_list

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
  Util.pp_seq ~sep:", " pp_pair out (Seq.to_seq s);
  Format.fprintf out "@]}";
  ()

let to_string = CCFormat.to_string pp
