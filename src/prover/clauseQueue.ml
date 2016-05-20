
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of clauses} *)

open Libzipperposition

module O = Ordering
module Lit = Literal
module Lits = Literals

module type S = ClauseQueue_intf.S

type profile = ClauseQueue_intf.profile

let profiles_ =
  let open ClauseQueue_intf in
  [ "default", P_default
  ; "bfs", P_bfs
  ; "explore", P_explore
  ; "ground", P_ground
  ; "goal", P_goal
  ; "goal_bfs", P_goal_bfs
  ]

let profile_of_string s =
  let s = s |> String.trim |> String.lowercase in
  try List.assoc s profiles_
  with Not_found -> invalid_arg ("unknown queue profile: " ^ s)

let _profile = ref ClauseQueue_intf.P_default
let get_profile () = !_profile
let set_profile p = _profile := p
let parse_profile s = _profile := (profile_of_string s)

let () =
  Params.add_opts
    [ "--clause-queue"
      , Arg.Symbol (List.map fst profiles_, parse_profile)
      , " choose which set of clause queues to use (for selecting next active clause)"
    ]

module Weight = struct
  type t = int list

  let compare a b = CCOrd.(list_ int_) a b
  let pp out = CCFormat.(list int) out
end

module Make(C : Clause.S) = struct
  module C = C

  (* weight of a term [t], using the precedence's weight *)
  let term_weight t = FOTerm.size t

  let weight_lits_ l =
    Array.fold_left
      (fun acc lit -> acc + Lit.heuristic_weight term_weight lit)
      0 l

  (** {6 Weight functions} *)
  module WeightFun = struct
    type single = C.t -> int
    type t = C.t -> Weight.t

    let default c =
      (* maximum depth of types. Avoids reasoning on list (list (list .... (list int))) *)
      let _depth_ty =
        Lits.Seq.terms (C.lits c)
        |> Sequence.map FOTerm.ty
        |> Sequence.map Type.depth
        |> Sequence.max ?lt:None
        |> CCOpt.maybe CCFun.id 0
      in
      let trail = C.trail c in
      let w_lits = weight_lits_ (C.lits c) in
      let w_trail =
        let module B = BBox in
        Trail.fold
          (fun acc t -> match B.payload (B.Lit.abs t) with
             | B.Fresh -> acc
             | B.Clause_component lits -> acc + weight_lits_ lits
             | B.Case p ->
               (* generic penalty for each inductive hypothesis *)
               acc + 2 * Ind_cst.path_length p
          )
          0 trail
      in
      w_lits * Array.length (C.lits c) + w_trail * (Trail.length trail) + _depth_ty

    let penalty_coeff_ = 20

    let favor_pos_unit c =
      let is_unit_pos c = match C.lits c with
        | [| lit |] when Lit.is_pos lit -> true
        | _ -> false
      in
      if is_unit_pos c then 0 else penalty_coeff_

    let favor_horn c =
      if Lits.is_horn (C.lits c) then 0 else penalty_coeff_

    let age c =
      if C.is_empty c then 0
      else C.id c

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

    let single w c = [w c]

    let combine_linear ws =
      assert (ws <> []);
      assert (List.for_all (fun (_,c) -> c > 0) ws);
      fun c ->
        let w = List.fold_left
          (fun sum (w,coeff) -> sum + coeff * w c)
          0 ws
        in
        [w]

    let rec combine_lexico_l = function
      | [] -> invalid_arg "combine_lexico_l"
      | [f] -> (fun c -> f c)
      | f :: tail -> (fun c -> f c @ combine_lexico_l tail c)

    let combine_lexico a b = combine_lexico_l [a;b]
  end

  module H = CCHeap.Make(struct
      type t = (Weight.t * C.t)
      let leq (w1, c1) (w2, c2) =
        let c_w = Weight.compare w1 w2 in
        c_w <= 0 || (c_w = 0 && C.compare c1 c2 <= 0)
    end)

  (** A priority queue of clauses, purely functional *)
  type t = {
    heap : H.t;
    functions : functions;
  }
  and functions = {
    weight : WeightFun.t;
    name : string;
  }

  (** generic clause queue based on some ordering on clauses, given
      by a weight function *)
  let make ~weight name =
    let functions = {
      weight;
      name;
    } in
    { heap = H.empty; functions; }

  let is_empty q =
    H.is_empty q.heap

  let add q c =
    let w = q.functions.weight c in
    let heap = H.insert (w, c) q.heap in
    { q with heap; }

  let adds q hcs =
    let heap =
      Sequence.fold
        (fun heap c ->
           let w = q.functions.weight c in
           H.insert (w,c) heap)
        q.heap hcs in
    { q with heap; }

  let take_first q =
    if is_empty q then raise Not_found;
    let new_h, (w, c) = H.take_exn q.heap in
    let q' = { q with heap=new_h; } in
    q', c, w

  let name q = q.functions.name

  (** {6 Combination of queues} *)

  let goal_oriented =
    let open WeightFun in
    let weight =
      combine_lexico_l
        [ single favor_goal
        ; combine_linear [age, 1; default, 4; favor_all_neg, 1]
        ]
    in
    let name = "goal" in
    make ~weight name

  let goal_bfs =
    let open WeightFun in
    let weight =
      combine_lexico_l
        [ single favor_goal
        ; single age
        ]
    in
    let name = "goal_bfs" in
    make ~weight name

  let bfs =
    let open WeightFun in
    let weight = single age in
    make ~weight "bfs"

  let explore =
    let open WeightFun in
    let weight = combine_linear [age, 1; default, 4; favor_all_neg, 1] in
    make ~weight "explore"

  let ground =
    let open WeightFun in
    let weight =
      combine_lexico_l
        [ single favor_ground
        ; combine_linear [age, 1; favor_pos_unit, 1; default, 2]
        ]
    in
    make ~weight "ground"

  let default =
    let open WeightFun in
    let weight =
      combine_linear
        [ age, 4; default, 3; favor_all_neg, 1
        ; favor_goal, 1; favor_pos_unit, 1]
    in
    make ~weight "default"

  let of_profile p =
    let open ClauseQueue_intf in
    match p with
    | P_default -> default
    | P_bfs -> bfs
    | P_explore -> explore
    | P_ground -> ground
    | P_goal -> goal_oriented
    | P_goal_bfs -> goal_bfs

  let pp out q = CCFormat.fprintf out "queue %s" (name q)
  let to_string = CCFormat.to_string pp
end
