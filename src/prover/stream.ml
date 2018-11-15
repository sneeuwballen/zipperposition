
(** {1 Stream}*)

open Logtk

(* TODO: what is the precise use of doing this? *)
let stat_stream_create = Util.mk_stat "stream.create"

(** {2 Signature} *)
module type S = Stream_intf.S

type 'a packed = (module S with type C.t = 'a)

module Make(X : sig
    module Ctx : Ctx.S
  end)
  : S with module Ctx = X.Ctx
= struct
  module Ctx = X.Ctx
  module C = Clause.Make(Ctx)


type t = {
  id : int; (** unique ID of the stream *)
  penalty: int; (** heuristic penalty *)
  mutable stm : C.t option OSeq.t; (** the stream itself *)
}

let id_count_ = ref 0

(** {2 Basics} *)

let make ?penalty:(p=1) s =
  let id = !id_count_ in
  incr id_count_;
  { id; penalty = p; stm = s; }

let equal s1 s2 = s1.id = s2.id
let compare s1 s2 = Pervasives.compare s1.id s2.id
let id s = s.id
let hash s = Hashtbl.hash s.id

(* TODO: does it really pop an element? *)
let is_empty s = OSeq.is_empty s.stm

(*
   TODO: what happens when the sequence is infinite? /!\ Caution: non-termination risk /!\
*)
let length s = OSeq.length s.stm

let penalty s = s.penalty

let pp out s =
  Format.fprintf out "stream %i" s.id;
  ()

end
