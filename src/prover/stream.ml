(** {1 Stream}*)

open Logtk

let stat_stream_create = Util.mk_stat "stream.create"
let section = Util.Section.make ~parent:Const.section "stm"

(** {2 Signature} *)

type t = {
  id: int;  (** unique ID of the stream *)
  parents: Clause.t list;
      (** parent clauses for inference generating this stream *)
  mutable penalty: int;  (** heuristic penalty, increased by every drip *)
  mutable hits: int;  (** how many attemts to retrieve unifier were there *)
  mutable stm: Clause.t option OSeq.t;  (** the stream itself *)
}

exception Empty_Stream
exception Drip_n_Unfinished of Clause.t option list * int * int

let id_count_ = ref 0

(** {2 Basics} *)

let make ?penalty:(p = 1) ~parents s =
  Util.incr_stat stat_stream_create;
  let id = !id_count_ in
  incr id_count_;
  { id; penalty = p; hits = 0; stm = s; parents }

let pp out s =
  Format.fprintf out "stm %i/%i/%i" s.id s.penalty s.hits;
  ()

let equal s1 s2 = s1.id = s2.id
let compare s1 s2 = Stdlib.compare s1.id s2.id
let id s = s.id
let hash s = Hashtbl.hash s.id

let is_empty s =
  try
    let _ = OSeq.head_exn s.stm in
    false
  with Invalid_argument _ -> true

let penalty s = s.penalty

(** {2 Access} *)

let drip s =
  try
    let x = OSeq.head_exn s.stm in
    s.stm <- OSeq.tail_exn s.stm;
    s.hits <- s.hits + 1;
    x
  with Invalid_argument _ -> raise Empty_Stream

let drip_n s n guard =
  let rec aux i acc =
    if i < n then (
      try
        let x = OSeq.head_exn s.stm in
        s.stm <- OSeq.tail_exn s.stm;
        s.hits <- s.hits + 1;
        aux (i + 1) (x :: acc)
      with Invalid_argument _ ->
        raise (Drip_n_Unfinished (List.rev acc, n, guard))
    ) else if guard > 0 && i >= n + guard then
      List.rev acc
    else
      aux (i + 1) acc
  in
  aux 0 []
