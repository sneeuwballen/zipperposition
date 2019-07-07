
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Priority Queue of clauses} *)

open Logtk

module O = Ordering
module Lit = Literal
module Lits = Literals

module type S = ClauseQueue_intf.S

type profile = ClauseQueue_intf.profile

let cr_var_ratio = ref 5
let cr_var_mul   = ref 1.1
let parameters_magnitude = ref `Large
let goal_penalty = ref false

let profiles_ =
  let open ClauseQueue_intf in
  [ "default", P_default
  ; "bfs", P_bfs
  ; "almost-bfs", P_almost_bfs
  ; "explore", P_explore
  ; "ground", P_ground
  ; "goal", P_goal
  ; "conjecture-relative", P_conj_rel
  ; "conjecture-relative-var", P_conj_rel_var
  ; "ho-weight", P_ho_weight
  ; "ho-weight-init", P_ho_weight_init
  ]

let profile_of_string s =
  let s = s |> String.trim |> CCString.lowercase_ascii in
  try
    match CCString.chop_prefix ~pre:"conjecture-relative-var" s with
    | Some suffix -> 
        let err_msg = "conjecutre-relative-var(ratio:int,var_mul:float,par:[L,S],goal_penalty:[true,false])" in
        let args = List.map (fun s -> 
                    String.trim (CCString.replace ~sub:")" ~by:"" 
                                  (CCString.replace ~sub:"(" ~by:"" s))) 
                  (CCString.split ~by:"," suffix) in
        if List.length args != 4 then (invalid_arg err_msg)
        else (
          let ratio = CCInt.of_string (List.nth args 0) in
          let var_mul =
            try float_of_string (List.nth args 1)
            with _ -> invalid_arg err_msg
          in
          let par_mag = List.nth args 2 in
          let goal_pen = List.nth args 3 in
          if CCOpt.is_none ratio then (
            invalid_arg err_msg;
          ) else (
            cr_var_ratio := CCOpt.get_exn ratio;
            cr_var_mul := var_mul;
            parameters_magnitude := if CCString.equal par_mag "l" then `Large
                                    else if CCString.equal par_mag "s" then `Small
                                    else invalid_arg err_msg;
            goal_penalty         := if CCString.prefix ~pre:"t" goal_pen then true
                                    else if CCString.prefix ~pre:"f" goal_pen then false
                                    else invalid_arg err_msg;
            ClauseQueue_intf.P_conj_rel_var
          )
        )
    | None ->  List.assoc s profiles_
  with Not_found -> invalid_arg ("unknown queue profile: " ^ s)

let _profile = ref ClauseQueue_intf.P_default
let get_profile () = !_profile
let set_profile p = _profile := p
let parse_profile s = _profile := (profile_of_string s)

let () =
  let o = Arg.String (parse_profile) in
  Params.add_opts
    [ "--clause-queue", o,
      " choose which set of clause queues to use (for selecting next active clause)";
      "-cq", o, " alias to --clause-queue"
    ];

  Params.add_to_mode "ho-pragmatic" (fun () ->
      _profile := P_conj_rel_var;
      cr_var_ratio := 8;
      cr_var_mul   := 1.05;
  );
  Params.add_to_mode "ho-competitive" (fun () ->
      _profile := P_conj_rel_var;
      cr_var_ratio := 8;
      cr_var_mul   := 1.05;
  );
  Params.add_to_mode "ho-complete-basic" (fun () ->
      _profile := P_conj_rel_var;
      cr_var_ratio := 8;
      cr_var_mul   := 1.05;
  );
  Params.add_to_mode "fo-complete-basic" (fun () ->
      _profile := P_conj_rel_var;
      cr_var_ratio := 8;
      cr_var_mul   := 1.05;
  );

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
        |> Iter.map Term.ty
        |> Iter.map Type.depth
        |> Iter.max ?lt:None
        |> CCOpt.map_or CCFun.id ~default:0
      in
      let w_lits = weight_lits_ (C.lits c) in
      w_lits * Array.length (C.lits c) + _depth_ty

    let ho_weight_calc c = 
      let all_terms c =
        C.Seq.terms c 
        |> Iter.flat_map (Term.Seq.subterms ~include_builtin:true) in

      let app_var_num c = 
        float_of_int @@ 
        Iter.fold (fun acc t -> 
          acc + (if Term.is_app_var t then 1 else 0 )) 0 (all_terms c) in
      
      let formulas_num c =
        float_of_int @@
        Iter.fold (fun acc t -> 
          acc + (if Term.is_formula t then 1 else 0 )) 0 (C.Seq.terms c) in
      
      let p_depth c = float_of_int (C.proof_depth c) in

      let t_depth c = C.Seq.terms c 
                      |> Iter.map Term.depth
                      |> Iter.max
                      |> CCOpt.get_or ~default:0
                      |> float_of_int in

      let weight c = float_of_int (weight_lits_ (C.lits c)) in

      let res = 
        int_of_float (weight c *. (1.25 ** (app_var_num c *. (1.35 ** p_depth c)))
                               *. (0.85 ** formulas_num c)
                               *. 1.05 ** t_depth c) in
      Util.debugf 5 "[C_W:]@ @[%a@]@ :@ %d(%g, %g, %g).\n"
        (fun k -> k C.pp c res (app_var_num c) (formulas_num c) (p_depth c));
      res

    let ho_weight_initial c =
      if C.proof_depth c  = 0 then 1
      else ho_weight_calc c

    let rec calc_tweight t sg v w c_mul =
      match Term.view t with 
         Term.AppBuiltin (_,l) -> 
            w + List.fold_left (fun acc t -> acc + 
                                    calc_tweight t sg v w c_mul) 0 l
         | Term.Var _ -> v
         | Term.DB _ -> w
         | Term.App (f, l) ->
            calc_tweight f sg v w c_mul +
              List.fold_left (fun acc t -> acc + calc_tweight t sg v w c_mul) 0 l
         
         | Term.Const id -> (int_of_float ((if Signature.sym_in_conj id sg then c_mul else 1.0)*.float_of_int w))
         | Term.Fun (_, t) -> calc_tweight t sg v w c_mul

     let calc_lweight l sg v w c_mul =
      assert (Literal.no_prop_invariant l);
      match l with 
      (* Special treatment of propositions *)
      | Lit.Equation (lhs,rhs,true) when Term.equal rhs Term.true_ 
                                          || Term.equal rhs Term.false_ ->
        calc_tweight lhs sg v w c_mul, Term.equal rhs Term.true_
      | Lit.Equation (lhs,rhs,sign) -> (calc_tweight lhs sg v w c_mul + 
                                        calc_tweight rhs sg v w c_mul, sign)
      | _ -> (0,false)

    let conj_relative ?(distinct_vars_mul=(-1.0)) c =
      let sgn = C.Ctx.signature () in
      let pos_mul, max_mul, v,f =
        match !parameters_magnitude with 
        |`Large -> (1.5,1.5,100,100)
        |`Small -> (2.0,1.5,2,3)
      in
      let conj_mul = 0.5 in
        Array.mapi (fun i xx -> i,xx) (C.lits c)
        |> 
        (Array.fold_left (fun acc (i,l) -> acc +. 
                          let l_w, l_s = (calc_lweight l sgn v f conj_mul) in 
                            ( if l_s then pos_mul else 1.0 )*.
                            ( if C.is_maxlit (c,0) Subst.empty ~idx:i then max_mul else 1.0)*. 
                            float_of_int l_w ) 0.0) 
        |> (fun res -> 
              if distinct_vars_mul < 0.0 then int_of_float res
              else
                let dist_vars = 
                 Literals.vars (C.lits c)
                 |> List.filter (fun v -> not (Type.is_tType (HVar.ty v)))  in
                let n_vars = List.length dist_vars + 1  in
                let dist_var_penalty = distinct_vars_mul ** (float_of_int n_vars) in
                let goal_dist_penalty = 
                  if !goal_penalty then (
                    let divider = 
                      match C.distance_to_goal c with
                      | Some d -> 1.5 ** (1.0 /. (1.0 +. (float_of_int @@ d)))
                      | None -> 1.0 in
                    1.0 /. divider
                  ) else 1.0 in
                let val_ = int_of_float (goal_dist_penalty *. dist_var_penalty *. res) in
                (Util.debugf  1 "cl: %a, w:%d\n" (fun k -> k C.pp c val_);
                val_))


    let penalty = C.penalty

    let penalize w c = assert (penalty c >= 1); w c * penalty c

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

  let add_seq q hcs = Iter.iter (add q) hcs

  let rec take_first_mixed q =
    if is_empty_mixed q then raise Not_found;
    let taken_from_fifo = ref true in
    (* find next clause *)
    let c =
      if q.time_before_fifo = 0
      then (
        (* q.time_before_fifo <- q.ratio; *)
        let res = Queue.pop q.queue in
        Util.debugf 1 "taking from fifo: %a.\n" (fun k -> k C.pp res);
        res
      ) else (
        assert (q.time_before_fifo > 0);
        (* q.time_before_fifo <- q.time_before_fifo - 1; *)
        taken_from_fifo := false;
        let new_h, (w, c) = H.take_exn q.heap in
        Util.debugf 1 "taken from heap: %a/h_weight:%d.\n" (fun k -> k C.pp c w);
        q.heap <- new_h;
        c
      )
    in
    if C.Tbl.mem q.tbl c then (
      if !taken_from_fifo then q.time_before_fifo <- q.ratio 
      else q.time_before_fifo <- q.time_before_fifo-1;
      C.Tbl.remove q.tbl c;
      Util.debugf 1 "accepted.\n" CCFun.id;
      c
    ) else (Util.debugf 1 "in q.tbl. rejected.\n" CCFun.id; take_first_mixed q (* spurious *))

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
      penalize ( 
        combine [default, 4; favor_small_num_vars, 2;
                favor_goal, 1; favor_all_neg, 1; ]
      ) in
    let name = "goal_oriented" in
    make ~ratio:6 ~weight name

  let bfs () : t = FIFO (Queue.create ())

  let almost_bfs () : t =
    let open WeightFun in
    let weight = 
      penalize ( combine [ default, 3; ] ) in
    make ~ratio:1 ~weight "almost_bfs"

  let explore () : t =
    let open WeightFun in
    let weight =
      penalize (
        combine
          [default, 4; favor_small_num_vars, 1;
          favor_all_neg, 1 ]
      )
    in
    make ~ratio:6 ~weight "explore"

  let ground () : t =
    let open WeightFun in
    let weight =
      penalize (
        combine [favor_pos_unit, 1; favor_ground, 2;
                favor_small_num_vars, 10; ]
      )
    in
    make ~ratio:6 ~weight "ground"

  let default () : t =
    let open WeightFun in
    let weight =
      penalize (
        combine
          [ default, 3; favor_all_neg, 1; favor_small_num_vars, 2
          ; favor_goal, 1; favor_pos_unit, 1; ]
      )
    in
    make ~ratio:6 ~weight "default"

  let conj_relative_mk () : t =
    make ~ratio:6 ~weight:WeightFun.conj_relative "conj_relative"

  let conj_var_relative_mk () : t =
    make ~ratio:!cr_var_ratio ~weight:(WeightFun.conj_relative ~distinct_vars_mul:!cr_var_mul)
         "conj_relative_var"

  let ho_weight () =
      make ~ratio:3 ~weight:WeightFun.ho_weight_calc "ho-weight"

  let ho_weight_init () =
      make ~ratio:5 ~weight:WeightFun.ho_weight_initial "ho-weight-init"

  let of_profile p =
    let open ClauseQueue_intf in
    match p with
      | P_default -> default ()
      | P_bfs -> bfs ()
      | P_almost_bfs -> almost_bfs ()
      | P_explore -> explore ()
      | P_ground -> ground ()
      | P_goal -> goal_oriented ()
      | P_conj_rel ->  conj_relative_mk ()
      | P_conj_rel_var -> conj_var_relative_mk ()
      | P_ho_weight -> ho_weight ()
      | P_ho_weight_init -> ho_weight_init ()

  let pp out q = CCFormat.fprintf out "queue %s" (name q)
  let to_string = CCFormat.to_string pp
end
