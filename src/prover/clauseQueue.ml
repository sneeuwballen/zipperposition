
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of clauses} *)

open Logtk

module O = Ordering
module Lit = Literal
module Lits = Literals

module type S = ClauseQueue_intf.S

type profile = ClauseQueue_intf.profile

let profiles_ =
  let open ClauseQueue_intf in
  [ "default", P_default
  ; "bfs", P_bfs
  ; "almost-bfs", P_almost_bfs
  ; "explore", P_explore
  ; "ground", P_ground
  ; "goal", P_goal
  ]

let profile_of_string s =
  let s = s |> String.trim |> CCString.lowercase_ascii in
  try List.assoc s profiles_
  with Not_found -> invalid_arg ("unknown queue profile: " ^ s)

let _profile = ref ClauseQueue_intf.P_default
let get_profile () = !_profile
let set_profile p = _profile := p
let parse_profile s = _profile := (profile_of_string s)

let () =
  let o = Arg.Symbol (List.map fst profiles_, parse_profile) in
  Params.add_opts
    [ "--clause-queue", o,
      " choose which set of clause queues to use (for selecting next active clause)";
      "-cq", o, " alias to --clause-queue"
    ]

module Make(C : Clause_intf.S) = struct
  module C = C

  (* weight of a term [t], using the precedence's weight *)
  let term_weight t = Term.size t

  (** {6 Weight functions} *)
  module WeightFun = struct
    type t = C.t -> int

    let weight_lits_ l =
      Array.fold_left
        (fun acc lit -> acc + Lit.heuristic_weight term_weight lit)
        0 l

    let default c =
      (* maximum depth of types. Avoids reasoning on list (list (list .... (list int))) *)
      let _depth_ty =
        Lits.Seq.terms (C.lits c)
        |> Sequence.map Term.ty
        |> Sequence.map Type.depth
        |> Sequence.max ?lt:None
        |> CCOpt.map_or CCFun.id ~default:0
      in
      let w_lits = weight_lits_ (C.lits c) in
      w_lits * Array.length (C.lits c) + _depth_ty

    let penalty = C.penalty

    let penalty_coeff_ = 20

    let favor_pos_unit c =
      let is_unit_pos c = match C.lits c with
        | [| lit |] when Lit.is_pos lit -> true
        | _ -> false
      in
      if is_unit_pos c then 0 else penalty_coeff_

    (* favorize small number of variables in a clause *)
    let favor_small_num_vars c =
      (* number of distinct term variables *)
      let n_vars =
        Literals.vars (C.lits c)
        |> List.filter (fun v -> not (Type.is_tType (HVar.ty v)))
        |> List.length
      in
      let n =
        if n_vars < 4 then 0
        else if n_vars < 6 then 1
        else if n_vars < 8 then 3
        else n_vars
      in
      n * penalty_coeff_

    let favor_horn c =
      if Lits.is_horn (C.lits c) then 0 else penalty_coeff_

    let goal_threshold_ = 15

    let favor_goal c =
      if C.is_empty c then 0
      else
        let d = match C.distance_to_goal c with
          | Some d -> min d goal_threshold_
          | None -> goal_threshold_
        in
        1+d

    (* check whether a literal is a ground neg lit *)
    let is_ground_neg lit = Lit.is_neg lit && Lit.is_ground lit
    let all_ground_neg c = CCArray.for_all is_ground_neg (C.lits c)

    let favor_all_neg c =
      if all_ground_neg c then 0 else penalty_coeff_

    let favor_ground c = if C.is_ground c then 0 else penalty_coeff_

    let favor_non_all_neg c =
      if not (all_ground_neg c) then 0 else penalty_coeff_

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
      type t = (int * C.t)
      let leq (i1, c1) (i2, c2) =
        i1 < i2 ||
        (i1 = i2 && C.compare c1 c2 <= 0)
    end)

  (** A priority queue of clauses + FIFO queue *)
  type t =
    | FIFO of C.t Queue.t
    | Mixed of mixed

  and mixed = {
    mutable heap : H.t;
    mutable queue: C.t Queue.t;
    tbl: unit C.Tbl.t;
    mutable time_before_fifo: int;
    (* cycles from 0 to ratio, changed at every [take_first].
       when 0, pick in fifo; other pick from heap and decrease *)
    ratio: int;
    weight: C.t -> int;
    name: string;
  }

  (** generic clause queue based on some ordering on clauses, given
      by a weight function *)
  let make ~ratio ~weight name =
    if ratio <= 0 then invalid_arg "ClauseQueue.make: ratio must be >0";
    Mixed {
      weight;
      name;
      ratio;
      time_before_fifo=ratio;
      heap = H.empty;
      queue=Queue.create();
      tbl=C.Tbl.create 256;
    }

  let is_empty_mixed q = C.Tbl.length q.tbl = 0

  let is_empty (q:t) = match q with
    | FIFO q -> Queue.is_empty q
    | Mixed q -> is_empty_mixed q

  let length q = match q with
    | FIFO q -> Queue.length q
    | Mixed q -> C.Tbl.length q.tbl

  let add q c = match q with
    | FIFO q -> Queue.push c q
    | Mixed q ->
      if not (C.Tbl.mem q.tbl c) then (
        C.Tbl.add q.tbl c ();
        let w = q.weight c in
        let heap = H.insert (w, c) q.heap in
        q.heap <- heap;
        Queue.push c q.queue;
      )

  let add_seq q hcs = Sequence.iter (add q) hcs

  let rec take_first_mixed q =
    if is_empty_mixed q then raise Not_found;
    (* find next clause *)
    let c =
      if q.time_before_fifo = 0
      then (
        q.time_before_fifo <- q.ratio;
        Queue.pop q.queue
      ) else (
        assert (q.time_before_fifo > 0);
        q.time_before_fifo <- q.time_before_fifo - 1;
        let new_h, (_, c) = H.take_exn q.heap in
        q.heap <- new_h;
        c
      )
    in
    if C.Tbl.mem q.tbl c then (
      C.Tbl.remove q.tbl c;
      c
    ) else take_first_mixed q (* spurious *)

  let take_first = function
    | FIFO q ->
      if Queue.is_empty q then raise Not_found else Queue.pop q
    | Mixed q -> take_first_mixed q

  let name q = match q with
    | FIFO _ -> "bfs"
    | Mixed q -> q.name

  (** {6 Combination of queues} *)

  let goal_oriented () : t =
    let open WeightFun in
    let weight =
      combine [default, 4; favor_small_num_vars, 2;
               favor_goal, 1; favor_all_neg, 1; penalty, 1; ] in
    let name = "goal_oriented" in
    make ~ratio:6 ~weight name

  let bfs () : t = FIFO (Queue.create ())

  let almost_bfs () : t =
    let open WeightFun in
    let weight = combine [ default, 3; penalty, 2; ] in
    make ~ratio:1 ~weight "almost_bfs"

  let explore () : t =
    let open WeightFun in
    let weight =
      combine
        [default, 4; favor_small_num_vars, 1;
         favor_all_neg, 1; penalty, 1; ]
    in
    make ~ratio:6 ~weight "explore"

  let ground () : t =
    let open WeightFun in
    let weight =
      combine [favor_pos_unit, 1; favor_ground, 2;
               favor_small_num_vars, 10; penalty, 1 ]
    in
    make ~ratio:6 ~weight "ground"

  let default () : t =
    let open WeightFun in
    let weight =
      combine
        [ default, 3; favor_all_neg, 1; favor_small_num_vars, 2
        ; favor_goal, 1; favor_pos_unit, 1; penalty, 1; ]
    in
    make ~ratio:6 ~weight "default"

  let of_profile p =
    let open ClauseQueue_intf in
    match p with
      | P_default -> default ()
      | P_bfs -> bfs ()
      | P_almost_bfs -> almost_bfs ()
      | P_explore -> explore ()
      | P_ground -> ground ()
      | P_goal -> goal_oriented ()

  let pp out q = CCFormat.fprintf out "queue %s" (name q)
  let to_string = CCFormat.to_string pp
end
