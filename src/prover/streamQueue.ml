
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 A priority queue of streams} *)

open Logtk
let section = Util.Section.make ~parent:Const.section "stmq"

module type S = StreamQueue_intf.S

module type ARG = sig
  module Stm : Stream.S
  module Env : Env.S
end

let k_guard = Flex_state.create_key ()
let k_ratio = Flex_state.create_key ()
let k_clause_num = Flex_state.create_key ()

module Make(A : ARG) = struct
  module Stm = A.Stm
  module E = A.Env

  (** {5 Weight functions} *)
  module WeightFun = struct
    type t = Stm.t -> int

    let penalty = Stm.penalty

    let combine ws =
      assert (ws <> []);
      assert (List.for_all (fun (_,c) -> c > 0) ws);
      fun c ->
        List.fold_left
          (fun sum (w,coeff) -> sum + coeff * w c)
          0 ws
  end

  module H = CCHeap.Make(struct
      (* heap ordered by [weight, real age(id)] *)
      type t = (int * Stm.t)
      let leq (i1, s1) (i2, s2) =
        i1 < i2 ||
        (i1 = i2 && Stm.compare s1 s2 <= 0)
    end)

  (** A priority queue of streams *)
  type t = {
    mutable hp : H.t;
    mutable time_before_fair: int;
    (* cycles from 0 to ratio, changed at every [take_stm_nb] and [take_stm_nb_fix_stm];
       when 0, remove an element from each stream in the queue;
       otherwise use the chosen heuristic. *)
    mutable fair_tries : int;
    ratio: int;
    guard: int;
    weight: Stm.t -> int; (* function that assigns an initial weight to a stream (based on what criteria?) *)
    mutable stm_nb: int;
    name: string;
  }

  (** generic stream queue based on some ordering on streams, given
        by a weight function *)
  let make ~guard ~ratio ~weight name =
    if ratio < 0 then invalid_arg "StreamQueue.make: ratio must be greater or equal to 0";
    {
      weight;
      name;
      ratio;
      time_before_fair=ratio;
      fair_tries = 0;
      guard;
      stm_nb = 0;
      hp = H.empty;
    }

  let is_empty (q:t) = H.is_empty (q.hp)

  let length q = H.size q.hp

  let add q s =
    (* No verification that the stream is already in there TODO: should it be verified?*)
    let w = q.weight s in
    let hp = H.insert (w, s) q.hp in
    q.stm_nb <- q.stm_nb + 1;
    q.hp <- hp

  let add_lst q sl = List.iter (add q) sl

  let rec _take_first guard q =
    if H.is_empty q.hp then None (* TODO: replace with cheaper test q.stm_nb = 0 ? *)
    else (
      if guard = 0 then raise Not_found;
      let dripped = ref None in
      let reduced_hp, (w, s) = H.take_exn q.hp in
      let new_hp =
        (
          try
            dripped := Stm.drip s;
            H.insert (q.weight s , s) reduced_hp
          (* No matter if a clause or None is dripped the penalty is the same:
             TODO: should the penalty be higher when None is dripped? *)
          with
          | Stm.Empty_Stream ->
            assert (q.stm_nb > 0); (* TODO: stronger but more costly assert using H.size ?*)
            q.stm_nb <- q.stm_nb - 1;
            reduced_hp
        ) in
      q.hp <- new_hp;
      match !dripped with
      | None -> _take_first (guard-1) q
      | Some _ ->
        !dripped
    )

  let take_first q =
    assert (q.guard >= 0);
    _take_first q.guard q

  let take_fair tries q =
    q.time_before_fair <- q.ratio;
    (* TODO: the heap is fully traversed two times, can both operations be done with one traversal? *)
    (* q.hp <- H.filter (fun (_,s) -> not (Stm.is_empty s)) q.hp;
       H.fold (fun res (_,s) -> Stm.drip s :: res) [] q.hp *)
    q.fair_tries <- q.fair_tries + 1;
    (* H.fold (fun res (_,s) -> Stm.drip s :: res) [] q.hp *)
    let all_stms = CCList.sort (fun (_,s) (_,s') -> CCInt.compare (Stm.id s) (Stm.id s')) (H.to_list q.hp) in
    let to_drip, rest = CCList.take_drop tries all_stms in
    let dripped = CCList.filter_map (fun (_, s) ->
        try
          Some (Stm.drip s, s)
        with Stm.Empty_Stream -> None) to_drip in
    let new_stms = (List.map (fun (_,s) -> Stm.penalty s, s) dripped) @ rest in
    q.hp <- H.of_list new_stms;
    q.stm_nb <- List.length new_stms;
    let taken = List.map fst dripped in
    Util.debugf ~section 1 "taken clauses: @[%a@]@." (fun k -> k (CCList.pp (CCOpt.pp Stm.C.pp)) taken);
    taken


  let rec take_fair_anyway q =
    if H.is_empty q.hp then [None]
    else (
      let res = CCList.filter_map CCFun.id (take_fair (H.size q.hp) q) in
      if CCList.is_empty res then take_fair_anyway q
      else (
        q.time_before_fair <- q.ratio;
        List.map CCOpt.return res)
    )

  let rec _take_nb q nb prev_res =
    if H.is_empty q.hp || nb = 0 then prev_res
    else
      try
        _take_nb q (nb-1) ((take_first q)::prev_res)
      with
      | Not_found -> prev_res

  let clauses_to_take q =
    let max_clause = E.flex_get k_clause_num in
    if max_clause < 0 then q.stm_nb
    else min max_clause q.stm_nb

  let take_stm_nb q =
    let n = clauses_to_take q in
    let taken =
      if q.time_before_fair = 0 then take_fair (q.fair_tries+1) q 
      else (
        q.time_before_fair <- q.time_before_fair - 1;
        (* Only extract clauses if the minimal stream weight is low *)
        if not (H.is_empty q.hp) && fst (H.find_min_exn q.hp) <= 50
        then _take_nb q n [] 
        else []
      ) in
    Util.debugf ~section 1 "taken clauses: @[%a@]@." (fun k -> k (CCList.pp (CCOpt.pp Stm.C.pp)) taken);
    taken

  let rec _take_stm_nb_fix_stm q n res =
    if n = 0 || H.is_empty q.hp then res
    else
      let red_hp, (w,s)= H.take_exn q.hp in
      try
        if Stm.is_empty s then
          ( q.stm_nb <- q.stm_nb - 1;
            q.hp <- red_hp;
            _take_stm_nb_fix_stm q n res
          )
        else
          (
            let res = Stm.drip_n s n q.guard in
            q.hp <- H.insert (q.weight s, s) red_hp;
            res
          )
      with
      | Stm.Drip_n_Unfinished (res', _, n') ->
        _take_stm_nb_fix_stm q n' (res'@res)

  let take_stm_nb_fix_stm q =
    let n = clauses_to_take q in
    if q.time_before_fair = 0 then take_fair (q.fair_tries+1) q
    else (
      q.time_before_fair <- q.time_before_fair - 1;
      _take_stm_nb_fix_stm q n []
    )


  let name q = q.name

  let default () : t =
    let open WeightFun in
    let weight = penalty in
    make ~guard:(E.flex_get k_guard)  ~ratio:(E.flex_get k_ratio) ~weight "default"

  let pp out q = CCFormat.fprintf out "queue %s" (name q)
  let to_string = CCFormat.to_string pp
end
