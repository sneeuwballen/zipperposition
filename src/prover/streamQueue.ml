
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
      hp = H.empty;
    }

  let is_empty (q:t) = H.is_empty (q.hp)

  let length q = H.size q.hp

  let add q s =
    (* No verification that the stream is already in there TODO: should it be verified?*)
    let w = q.weight s in
    let hp = H.insert (w, s) q.hp in
    q.hp <- hp

  let add_lst q sl = List.iter (add q) sl

  let rec _take_first_when_available guard q =
    if H.is_empty q.hp then raise Not_found;
    if guard = 0 then None
    else (
      if q.time_before_drip = 0
      then (
        let dripped = ref None in
        let reduced_hp, (w, s) = H.take_exn q.hp in
        let new_hp =
          if Stm.is_empty s
          then reduced_hp
          else (
            dripped := Stm.drip s;
            H.insert (w + (Stm.penalty s), s) reduced_hp
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


  let rec take_first_anyway q =
    if H.is_empty q.hp then raise Not_found;
    let dripped = ref None in
    let reduced_hp, (w, s) = H.take_exn q.hp in
    let new_hp =
      if Stm.is_empty s
      then reduced_hp
      else (
          dripped := Stm.drip s;
          H.insert (w + (Stm.penalty s), s) reduced_hp
       ) in
    q.hp <- new_hp;
    match !dripped with
      | None -> take_first_anyway q
      | Some _ ->
        q.time_before_drip <- q.ratio;
        !dripped

  let name q = q.name

  let default () : t =
    let open WeightFun in
    let weight = penalty
    in
    make ~guard:100 ~ratio:1 ~weight "default"

  let pp out q = CCFormat.fprintf out "queue %s" (name q)
  let to_string = CCFormat.to_string pp
end
