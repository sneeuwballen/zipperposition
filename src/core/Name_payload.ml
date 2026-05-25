type t = ..

module Pure = struct
  let add ?(can_erase = fun _ -> false) p (l : t list) : t list =
    let rec aux p = function
      | [] -> [ p ]
      | p' :: tail when can_erase p' -> p :: tail
      | p' :: tail -> p' :: aux p tail
    in
    aux p l

  let find ~f:(p : t -> 'a option) (l : t list) : 'a option =
    match l with
    | [] -> None
    | e1 :: tail ->
      (match p e1, tail with
      | (Some _ as res), _ -> res
      | None, [] -> None
      | None, e2 :: tail2 ->
        (match p e2, tail2 with
        | (Some _ as res), _ -> res
        | None, [] -> None
        | None, e3 :: tail3 ->
          (match p e3 with
          | Some _ as res -> res
          | None -> CCList.find_map p tail3)))

  let exists ~f:p l =
    match l with
    | [] -> false
    | e :: _ when p e -> true
    | _ :: e :: _ when p e -> true
    | _ :: _ :: e :: _ when p e -> true
    | _ -> List.exists p l
end

open struct
  let payloads : t list Hstring.Tbl.t = Hstring.Tbl.create 32
end

let add ?can_erase (s : Hstring.t) p =
  let l = Hstring.Tbl.get_or payloads ~default:[] s in
  Hstring.Tbl.replace payloads s (Pure.add ?can_erase p l)

let[@inline] get s = Hstring.Tbl.get_or payloads ~default:[] s

(* TODO: keep moving payload to this. Then we can use Hstring in inner term,
    and make (de)ser work. *)

let find ~f s : 'a option = Pure.find ~f @@ get s
let exists ~f s : bool = Pure.exists ~f @@ get s
