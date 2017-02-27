
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
  Params.add_opts
    [ "--clause-queue"
    , Arg.Symbol (List.map fst profiles_, parse_profile)
    , " choose which set of clause queues to use (for selecting next active clause)"
    ]

module Make(C : Clause.S) = struct
  module C = C

  (* weight of a term [t], using the precedence's weight *)
  let term_weight t = FOTerm.size t

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
        |> Sequence.map FOTerm.ty
        |> Sequence.map Type.depth
        |> Sequence.max ?lt:None
        |> CCOpt.map_or CCFun.id ~default:0
      in
      let trail = C.trail c in
      let w_lits = weight_lits_ (C.lits c) in
      let w_trail =
        let module B = BBox in
        Trail.fold
          (fun acc t -> match B.payload (B.Lit.abs t) with
             | B.Fresh -> acc
             | B.Lemma _ -> acc
             | B.Clause_component lits -> acc + weight_lits_ lits
             | B.Case p ->
               (* penalize deep inductions quadratically *)
               acc + CCInt.pow (Ind_cst.path_length p) 2
          )
          0 trail
      in
      w_lits * Array.length (C.lits c) + w_trail + _depth_ty

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

    (* logarithmic function of age *)
    let age c =
      if C.is_empty c then 0
      else max 1 (C.id c+1 |> float |> log |> ceil |> int_of_float)

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
      (* heap ordered by [weight, exact age] *)
      type t = (int * int * C.t)
      let leq (i1, j1, c1) (i2, j2, c2) =
        i1 < i2 ||
        (i1 = i2 &&
         (j1 < j2 || (j1 = j2 && C.compare c1 c2 <= 0)))
    end)

  (** A priority queue of clauses, purely functional *)
  type t = {
    heap : H.t;
    functions : functions;
  }
  and functions = {
    weight : C.t -> int;
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
    let heap = H.insert (w, C.id c, c) q.heap in
    { q with heap; }

  let adds q hcs =
    let heap =
      Sequence.fold
        (fun heap c ->
           let w = q.functions.weight c in
           H.insert (w,C.id c,c) heap)
        q.heap hcs in
    { q with heap; }

  let take_first q =
    if is_empty q then raise Not_found;
    let new_h, (_, _, c) = H.take_exn q.heap in
    let q' = { q with heap=new_h; } in
    q', c

  let name q = q.functions.name

  (** {6 Combination of queues} *)

  let goal_oriented =
    let open WeightFun in
    let weight = combine [age, 1; default, 4; favor_goal, 1; favor_all_neg, 1] in
    let name = "goal_oriented" in
    make ~weight name

  let bfs =
    let open WeightFun in
    let weight = combine [age, 5; default, 1] in
    make ~weight "bfs"

  let explore =
    let open WeightFun in
    let weight = combine [age, 1; default, 4; favor_all_neg, 1] in
    make ~weight "explore"

  let ground =
    let open WeightFun in
    let weight = combine [age, 1; favor_pos_unit, 1; favor_ground, 2] in
    make ~weight "ground"

  let default =
    let open WeightFun in
    let weight =
      combine
        [ age, 4; default, 3; favor_all_neg, 1; favor_small_num_vars, 2
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

  let pp out q = CCFormat.fprintf out "queue %s" (name q)
  let to_string = CCFormat.to_string pp
end
