
(** {1 Stream}*)

open Logtk

let stat_stream_create = Util.mk_stat "stream.create"

(** {2 Signature} *)
module type S = Stream_intf.S

module type ARG = sig
  module Ctx : Ctx.S
  module C : Clause.S with module Ctx = Ctx
end

module Make(A:ARG) = struct
  module Ctx = A.Ctx
  module C = A.C

  type t = {
    id : int; (** unique ID of the stream *)
    penalty: int; (** heuristic penalty *)
    mutable stm : C.t option OSeq.t; (** the stream itself *)
  }

  exception Empty_Stream
  exception Drip_n_Unfinished of C.t option list * int

  let id_count_ = ref 0

  (** {2 Basics} *)

  let make ?penalty:(p=1) s =
    Util.incr_stat stat_stream_create;
    let id = !id_count_ in
    incr id_count_;
    { id; penalty = p; stm = s; }

  let equal s1 s2 = s1.id = s2.id
  let compare s1 s2 = Pervasives.compare s1.id s2.id
  let id s = s.id
  let hash s = Hashtbl.hash s.id

  (* TODO: does it really pop an element? *)
  (* normally it should be fine, check with Simon *)
  let is_empty s = OSeq.is_empty s.stm

  (* No length function because some streams are infinite *)

  let penalty s = s.penalty

  let drip s =
    match s.stm () with
    | OSeq.Nil -> raise Empty_Stream
    | OSeq.Cons (hd,tl) ->
      s.stm <- tl;
      hd
  (* let dripped = OSeq.nth 0 s.stm in
     s.stm <- OSeq.drop 1 s.stm;
     dripped *)

  let rec _drip_n st n guard =
    if guard = 0 then (st,[])
    else
      match n with
      | 0 -> (st,[])
      | _ ->
        match st () with
        | OSeq.Nil -> raise (Drip_n_Unfinished([],n))
        | OSeq.Cons (hd,tl) ->
          match hd with
          | None -> (
              try
                let (s',tl_res) = _drip_n tl n (guard - 1) in
                (s',tl_res)
              with
              | Drip_n_Unfinished (partial_res,n') -> raise (Drip_n_Unfinished((hd::partial_res),n'))
            )
          | _ -> (
              try
                let (s',tl_res) = _drip_n tl (n-1) (guard - 1) in
                (s',(hd::tl_res))
              with
              | Drip_n_Unfinished (partial_res,n') -> raise (Drip_n_Unfinished((hd::partial_res),n'))
            )

  let drip_n s n guard =
    try
      let (s',cl) = _drip_n s.stm n guard in
      s.stm <- s';
      cl
    with
    | Drip_n_Unfinished (res,n') ->
      s.stm <- OSeq.empty;
      raise (Drip_n_Unfinished (res,n'))

  let pp out s =
    Format.fprintf out "stream %i" s.id;
    ()

end
