
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Variable} *)

type +'a t = {
  id: ID.t;
  ty: 'a;
}
type 'a var = 'a t

let make ~ty id = {ty; id; }

let of_string ~ty name = {ty; id=ID.make name; }

let makef ~ty msg = CCFormat.ksprintf msg ~f:(of_string ~ty)

let gensym ~ty () = {ty; id=ID.gensym(); }

let copy v = make ~ty:v.ty (ID.copy v.id)

let update_ty v ~f = {v with ty=f v.ty; }

let ty t = t.ty
let id t = t.id
let name t = t.id.ID.name

let compare a b = ID.compare a.id b.id
let equal a b = ID.equal a.id b.id
let hash a = ID.hash a.id

let pp out a = ID.pp out a.id
let to_string a = ID.to_string a.id
let pp_full out a = ID.pp_full out a.id
let pp_fullc out a = ID.pp_fullc out a.id

module Set = struct
  type 'a t = 'a var ID.Map.t
  let is_empty = ID.Map.is_empty
  let empty = ID.Map.empty
  let add t v = ID.Map.add v.id v t
  let mem t v = ID.Map.mem v.id t
  let find_exn t id = ID.Map.find id t
  let find t id = try Some (find_exn t id) with Not_found -> None
  let diff a b =
    ID.Map.merge_safe a b
      ~f:(fun _ pair -> match pair with
        | `Left x -> Some x
        | `Right _ -> None
        | `Both _ -> None)
  let cardinal t = ID.Map.cardinal t
  let of_seq s = s |> Sequence.map (fun v->v.id, v) |> ID.Map.of_seq
  let add_seq m s = s |> Sequence.map (fun v->v.id, v) |> ID.Map.add_seq m
  let add_list m s = s |> List.map (fun v->v.id, v) |> ID.Map.add_list m
  let of_list l = l |> List.map (fun v->v.id,v) |> ID.Map.of_list
  let to_seq t = ID.Map.to_seq t |> Sequence.map snd
  let to_list t = ID.Map.fold (fun _ v acc ->v::acc) t []
  let pp out t = Util.pp_seq ~sep:", " pp out (to_seq t)
end

module Subst = struct
  type ('a,'b) t = ('a var * 'b) ID.Map.t
  let empty = ID.Map.empty
  let is_empty = ID.Map.is_empty
  let size = ID.Map.cardinal
  let add t v x = ID.Map.add v.id (v,x) t
  let singleton v x = add empty v x
  let mem t v = ID.Map.mem v.id t
  let find_exn t v = snd (ID.Map.find v.id t)
  let find t v = try Some (find_exn t v) with Not_found -> None
  let of_list l = l |> List.map (fun (v,x)->v.id,(v,x)) |> ID.Map.of_list
  let of_seq s = s |> Sequence.map (fun (v,x)->v.id, (v,x)) |> ID.Map.of_seq
  let to_seq t = ID.Map.to_seq t |> Sequence.map snd
  let to_list t = ID.Map.fold (fun _ tup acc -> tup::acc) t []
  let pp pp_v out t =
    let pp_pair out (v,x) =
      Format.fprintf out "@[%a → %a@]" pp_full v pp_v x
    in
    Format.fprintf out "@[%a@]" (Util.pp_seq ~sep:", " pp_pair) (to_seq t)
  let merge a b =
    ID.Map.merge_safe a b
      ~f:(fun _ v -> match v with
        | `Both (_,x) -> Some x (* favor right one *)
        | `Left x | `Right x -> Some x)
end
