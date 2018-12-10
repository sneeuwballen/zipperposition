
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 A priority queue of streams} *)

module type S = StreamQueue_intf.S

module Make(Stm : Stream_intf.S) = struct
  module Stm = Stm

  (** {6 Weight functions} *)
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
    mutable time_before_drip: int;
    (* cycles from 0 to ratio, changed at every [take_first_if_available].
       when 0, pick a clause in hp; otherwise don't pick anything and decrease.
       reset at ratio when [take_first_anyway] is called *)
    ratio: int;
    guard: int;
    weight: Stm.t -> int; (* function that assigns an initial weight to a stream (based on what criteria?) *)
    mutable stm_nb: int;
    name: string;
  }

(** generic stream queue based on some ordering on streams, given
      by a weight function *)
  let make ~guard ~ratio ~weight name =
    if ratio <= 0 then invalid_arg "StreamQueue.make: ratio must be >0";
    {
      weight;
      name;
      ratio;
      time_before_drip=ratio;
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

  let rec _take_first_when_available guard q =
    if H.is_empty q.hp then None (* TODO: replace with cheaper test q.stm_nb = 0 ? *)
    else (
      if guard = 0 then raise Not_found;
      if q.time_before_drip = 0
      then (
        let dripped = ref None in
        let reduced_hp, (w, s) = H.take_exn q.hp in
        let new_hp =
         (
            try
              dripped := Stm.drip s;
              H.insert (w + (Stm.penalty s), s) reduced_hp
            (* No matter if a clause or None is dripped the penalty is the same:
               TODO: should the penalty be higher when None is dripped? *)
            with
              | Stm.Empty_Stream ->
                assert (q.stm_nb > 0);
                q.stm_nb <- q.stm_nb - 1;
                reduced_hp
          ) in
        q.hp <- new_hp;
        match !dripped with
          | None -> _take_first_when_available (guard-1) q
          | Some _ ->
            q.time_before_drip <- q.ratio;
            !dripped
      ) else (
        assert (q.time_before_drip > 0);
        q.time_before_drip <- q.time_before_drip - 1;
        None
      )
    )

  let take_first_when_available q =
    assert (q.guard >= 0);
    _take_first_when_available q.guard q

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
            H.insert (w + (Stm.penalty s), s) reduced_hp
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

  let rec take_first_anyway q =
    if H.is_empty q.hp then None
    else (
      let dripped = ref None in
      let reduced_hp, (w, s) = H.take_exn q.hp in
      let new_hp =
        (
          try
            dripped := Stm.drip s;
            H.insert (w + (Stm.penalty s), s) reduced_hp
          with
            | Stm.Empty_Stream ->
              assert (q.stm_nb > 0);
              q.stm_nb <- q.stm_nb - 1;
              reduced_hp
        ) in
      q.hp <- new_hp;
      match !dripped with
        | None -> take_first_anyway q
        | Some _ ->
          q.time_before_drip <- q.ratio;
          !dripped
    )

  let rec _take_nb q nb prev_res =
    if H.is_empty q.hp || nb = 0 then prev_res
    else
      try
        _take_nb q (nb-1) ((take_first q)::prev_res)
      with
        | Not_found -> prev_res

  let take_stm_nb q = _take_nb q q.stm_nb []

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
            q.hp <- H.insert (w + n * Stm.penalty s, s) red_hp;
            res
          )
      with
        | Stm.Drip_n_Unfinished (res', n') ->
          _take_stm_nb_fix_stm q n' (res'@res)

  let take_stm_nb_fix_stm q =
    _take_stm_nb_fix_stm q q.stm_nb []


  let name q = q.name

  let default () : t =
    let open WeightFun in
    let weight = penalty
    in
    make ~guard:100 ~ratio:1 ~weight "default"

  let pp out q = CCFormat.fprintf out "queue %s" (name q)
  let to_string = CCFormat.to_string pp
end
