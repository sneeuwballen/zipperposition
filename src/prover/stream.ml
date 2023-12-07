
(** {1 Stream}*)

open Logtk

let stat_stream_create = Util.mk_stat "stream.create"
let section = Util.Section.make ~parent:Const.section "stm"


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
    parents : C.t list; (** parent clauses for inference generating this stream *)
    mutable penalty: int; (** heuristic penalty, increased by every drip *)
    mutable hits : int; (** how many attemts to retrieve unifier were there  *)
    mutable stm : C.t option OSeq.t; (** the stream itself *)
  }

  exception Empty_Stream
  exception Drip_n_Unfinished of C.t option list * int * int

  let id_count_ = ref 0

  (** {2 Basics} *)

  let make ?penalty:(p=1) ~parents s =
    Util.incr_stat stat_stream_create;
    let id = !id_count_ in
    incr id_count_;
    { id; penalty = p; hits=0; stm = s; parents }

  let pp out s =
    Format.fprintf out "stm %i/%i/%i" s.id s.penalty s.hits;
    ()

  let equal s1 s2 = s1.id = s2.id
  let compare s1 s2 = Stdlib.compare s1.id s2.id
  let id s = s.id
  let hash s = Hashtbl.hash s.id

  (* TODO: does it really pop an element? *)
  (* normally it should be fine, check with Simon *)
  let is_empty s = OSeq.is_empty s.stm

  (* No length function because some streams are infinite *)

  let penalty s = s.penalty

  let clause_penalty s = function 
    | None ->
      s.hits <- s.hits +1;
      max 2 (s.hits-16)
    | Some c ->
      s.hits <- s.hits +1;
      (C.penalty c) * (max 1 (s.hits-64)) 

  let is_orphaned s =
    List.exists C.is_redundant s.parents

  let drip s =
    let orig_penalty = s.penalty in
    let res = 
      if ClauseQueue.ignoring_orphans () && is_orphaned s then (
        s.penalty <- max_int;
        s.stm <- OSeq.empty;
        raise Empty_Stream
      ) else( 
        match s.stm () with
        | OSeq.Nil -> 
          s.penalty <- max_int;
          raise Empty_Stream
        | OSeq.Cons (hd,tl) ->
          s.stm <- tl;
          let cl_p = (clause_penalty s hd) in
          s.penalty <-  s.penalty + cl_p;
          hd) in
    if CCOpt.is_some res then 
      Util.debugf ~section 1 "drip:%d/%d->@[%a@]:@. @[%a@]@." (fun k -> k orig_penalty s.hits pp s (CCOpt.pp C.pp) res);
    res

  let drip_n s n guard =
    let rec _drip_n st n guard =
      if guard = 0 then (st,0,[])
      else
        match n with
        | 0 -> (st,0,[])
        | _ ->
          match st () with
          | OSeq.Nil -> raise (Drip_n_Unfinished([],0,n))
          | OSeq.Cons (hd,tl) ->
            let p = clause_penalty s hd in
            match hd with
            | None -> (
                try
                  let (s',p_tl,tl_res) = _drip_n tl n (guard - 1) in
                  (s',p+p_tl,tl_res)
                with
                | Drip_n_Unfinished (partial_res,p_partial,n') -> raise (Drip_n_Unfinished((hd::partial_res),p+p_partial,n'))
              )
            | _ -> (
                try
                  let (s',p_tl,tl_res) = _drip_n tl (n-1) (guard - 1) in
                  (s',p+p_tl,(hd::tl_res))
                with
                | Drip_n_Unfinished (partial_res,p_partial,n') -> raise (Drip_n_Unfinished((hd::partial_res),p+p_partial,n')))
    in

    let orig_penalty = s.penalty in
    let res =
      try
        let (s',p_add,cl) = _drip_n s.stm n guard in
        s.penalty <- s.penalty + p_add;
        s.stm <- s';
        cl
      with
      | Drip_n_Unfinished (res,p_add,n') ->
        s.stm <- OSeq.empty;
        s.penalty <- s.penalty + p_add;
        raise (Drip_n_Unfinished (res,p_add,n')) in
    Util.debugf ~section 1 "drip_n:%d->@[%a@]:@. @[%a@]@." (fun k -> k orig_penalty pp s (CCList.pp (CCOpt.pp C.pp)) res);
    res

end
