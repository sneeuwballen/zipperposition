type t = ..

type state = { payloads: t list Hstring.Tbl.t }
(** Payload state for a scope: a mutable table from names to payload lists.
    Scopes are entered via {!with_scope}, which installs a copy of the current
    state; mutations are retracted when the scope exits. *)

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

let new_state () : state = { payloads = Hstring.Tbl.create 32 }
let copy (st : state) : state = { payloads = Hstring.Tbl.copy st.payloads }

(* Thread-local current scope. [None] means no scope is active on this
    thread; payload operations fail in that case rather than silently
    using stale state. *)
let tls : state option Thread_local_storage.t = Thread_local_storage.create ()
let get_opt () : state option = Option.join (Thread_local_storage.get_opt tls)
let set_opt (st : state option) : unit = Thread_local_storage.set tls st

let get_state () : state =
  match get_opt () with
  | Some st -> st
  | None ->
    invalid_arg
      "Name_payload: no payload scope on this thread (Name_payload.with_scope \
       required)"

(** [with_scope f] runs [f] in a fresh scope that starts as a copy of the
    current state (or empty if none). When [f] returns or raises, the previous
    state is restored, retracting all mutations made in [f]. *)
let with_scope (f : unit -> 'a) : 'a =
  let saved = get_opt () in
  let st =
    match saved with
    | Some st -> copy st
    | None -> new_state ()
  in
  set_opt (Some st);
  Fun.protect ~finally:(fun () -> set_opt saved) f

let add ?can_erase (s : Hstring.t) p =
  let st = get_state () in
  let l = Hstring.Tbl.get_or st.payloads ~default:[] s in
  Hstring.Tbl.replace st.payloads s (Pure.add ?can_erase p l)

let[@inline] get s = Hstring.Tbl.get_or (get_state ()).payloads ~default:[] s

let erase s ~f =
  let st = get_state () in
  let l = Hstring.Tbl.get_or st.payloads ~default:[] s in
  Hstring.Tbl.replace st.payloads s (List.filter (fun p -> not (f p)) l)

let clear () = Hstring.Tbl.clear (get_state ()).payloads

(* TODO: keep moving payload to this. Then we can use Hstring in inner term,
    and make (de)ser work. *)

let find ~f s : 'a option = Pure.find ~f @@ get s
let exists ~f s : bool = Pure.exists ~f @@ get s
