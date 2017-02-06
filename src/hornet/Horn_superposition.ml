
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(* {1 Superposition on Horn Clauses} *)

open Libzipperposition

module T = FOTerm
module C = Clause
module Fmt = CCFormat

let section = Util.Section.make "horn_sup"


module Make : State.THEORY_FUN = functor(Ctx : State_intf.CONTEXT) -> struct
  module Ctx = Ctx
  module B_lit = Ctx.B_lit

  (* index term->clause *)
  module CP_idx = NPDtree.MakeTerm(C.With_pos)

  let name = "horn_superposition"

  (* a simplification rule *)
  type rule_simp = C.t -> C.t option

  (* a simplification rule yielding multiple clauses *)
  type rule_simp_n = C.t -> C.t list option

  (* an inference rule *)
  type rule_infer = C.t -> C.t list

  (** {2 Avatar Splitting} *)

  (** Clauses that contain several "components" are split immediately.
      A clause [C] is splittable if it contains subsets of literals
      [C_1 \lor … \lor C_n]  where each [C_i]
      is a component.
      A component [C_i] is a non-empty subset of the literals of the clause,
      that shares no variables with the other [C_j | j≠i] *)

  module Avatar : sig
    val split : rule_simp_n
  end = struct
    let stat_avatar_split = Util.mk_stat "hornet.avatar_split"

    (* union-find that maps vars to list of literals, used for splitting *)
    module UF = UnionFind.Make(struct
        type key = T.var
        type value = Lit.t list
        let equal = HVar.equal Type.equal
        let hash = HVar.hash
        let zero = []
        let merge = List.rev_append
      end)

    let try_split_ lits c =
      assert (IArray.length lits >= 2);
      (* maps each variable to a list of literals. Sets can be merged whenever
         two variables occur in the same literal.  *)
      let uf_vars =
        IArray.to_seq lits
        |> Sequence.flat_map Lit.vars_seq
        |> T.VarSet.of_seq
        |> T.VarSet.to_list
        |> UF.create
      (* set of ground literals (each one is its own component) *)
      and cluster_ground =
        ref Lit.Set.empty
      in
      (* literals belong to either their own ground component, or to every
          sets in [uf_vars] associated to their variables *)
      IArray.iter
        (fun lit ->
           let v_opt = Lit.vars_seq lit |> Sequence.head in
           begin match v_opt with
             | None -> (* ground, lit has its own component *)
               cluster_ground := Lit.Set.add lit !cluster_ground
             | Some v ->
               (* merge other variables of the literal with [v] *)
               Lit.vars_seq lit
               |> Sequence.iter
                 (fun v' ->
                    UF.add uf_vars v' [lit];  (* lit is in the equiv class of [v'] *)
                    UF.union uf_vars v v')
           end)
        lits;

      (* now gather all the components as a literal list list *)
      let components = ref [] in
      Lit.Set.iter (fun lit -> components := [lit] :: !components) !cluster_ground;
      UF.iter uf_vars (fun _ comp -> components := comp :: !components);

      begin match !components with
        | [] -> assert (IArray.length lits=0); None
        | [_] -> None
        | _::_ ->
          (* do a simplification! *)
          Util.incr_stat stat_avatar_split;
          let proof = C.Proof.avatar_split c in
          let clauses =
            List.map
              (fun lits ->
                 let lits = IArray.of_list lits in
                 C.make lits proof)
              !components
          in
          Util.debugf ~section 4 "@[split of @[%a@]@ yields [@[<hv>%a@]]@]"
            (fun k->k C.pp c (Util.pp_list C.pp) clauses);
          let split_lits =
            List.map Ctx.B_lit.box_clause clauses
          in
          (* add boolean constraint: trail(c) => bigor_{name in clauses} name *)
          let bool_clause =
            Ctx.B_lit.neg (Ctx.B_lit.box_clause c) :: split_lits
          in
          Ctx.add_clause bool_clause;
          Util.debugf ~section 4 "@[constraint clause is @[%a@]@]"
            (fun k->k Ctx.B_lit.pp_clause bool_clause);
          (* return the clauses *)
          Some clauses
      end

    let split c : C.t list option = match C.proof c with
      | _ when IArray.length (C.lits c) <= 1 -> None
      | C.P_avatar_split _ | C.P_split _ -> None (* by construction, impossible *)
      | C.P_instance _ | C.P_from_stmt _ ->
        try_split_ (C.lits c) c
  end

  (** {2 Inst-Gen-Eq} *)
  module Inst_gen_eq : sig

  end = struct
    (* TODO:
       - rule to ground non-ground clauses (with a pointer to the grounding)
       - rule to select a horn subset in non-Horn non-ground clause components
    *)

    let stat_split = Util.mk_stat "hornet.split"
  end

  (** {2 Saturation Algorithm} *)

  let rules_simp : rule_simp list =
    [
    ]

  let rules_simp_n : rule_simp_n list =
    [ Avatar.split;
    ]

  let rules_infer : rule_infer list =
    [
    ]

  module Conflict_clause : sig
    type t = private C.t (* an empty clause *)

    val make : C.t -> t
    val to_bool_clause : t -> B_lit.t list
    val proof : t -> Ctx.Proof.t
    val pp : t CCFormat.printer
  end = struct
    type t = C.t (* an empty clause *)
    let make c = assert (C.is_empty c); c
    let to_bool_clause c : B_lit.t list = assert false (* TODO *)
    let proof c = Ctx.Proof.of_clause_proof (C.proof c)
    let pp = C.pp
  end

  (** Keeps a set of clauses that are saturated up to some limit.
      The limit is on derivations: a clause that has been derived using
      "too many" non-decreasing steps is thrown away.
      This is sufficient for saturation to always terminate.

      The state is a set of Horn clauses, and is backtrackable. *)

  module Saturate : sig
    type res =
      | Sat
      | Unsat of Conflict_clause.t list

    type stats = {
      num_clauses: int; (* number of clauses *)
    }

    val pp_stats : stats CCFormat.printer

    val stats : unit -> stats

    val set_limit : int -> unit
    (** Set the limit on derivations *)

    val add_clause : C.t -> res
    (** [add_clause c] adds the clause [c]  to the set, and saturates it
        again. If, during saturation, the empty clause is derived,
        [Unsat l] is returned (where [l] is a non-empty list of empty clauses).
        Otherwise, [Sat] is returned. *)

    val add_clauses : C.t list -> res
    (** Add a list of clauses *)
  end = struct
    type res =
      | Sat
      | Unsat of Conflict_clause.t list

    type stats = {
      num_clauses: int; (* number of clauses *)
    }

    let pp_stats out s: unit =
      Fmt.fprintf out "{@[num_clauses: %d@]}" s.num_clauses

    (** {6 Local State} *)

    let limit_ : int ref = ref 1


    let set_limit i = assert (i>0); limit_ := i

    let stats (): stats = assert false

    (** {6 Saturation} *)

    exception Conflict_exn of C.t

    let add_clause c =
      Util.debugf ~section 5 "@[<2>saturate.add_clause@ %a@]" (fun k->k C.pp c);
      assert false (*  TODO *)

    let add_clauses (l:C.t list) =
      assert false (* TODO *)
  end

  (** {2 Unit and Horn Clauses} *)

  let initial_clauses : C.t list =
    CCVector.to_seq Ctx.statements
    |> Sequence.flat_map Statement.Seq.forms
    |> Sequence.to_rev_list

  (* TODO
  (* index on the head terms (active paramodulation/resolution) *)
  let idx_heads0 : CP_idx.t =
    ??
    (* TODO
    (Sequence.of_list horn0
    |> Sequence.map C.Horn.concl_pos
                 *)

  let idx_heads : CP_idx.t ref = ref idx_heads0
     *)

  let on_assumption (lit:B_lit.t): unit =
    begin match B_lit.view lit, B_lit.sign lit with
      | B_lit.Box_clause c, true ->
        let res = Saturate.add_clause c in
        begin match res with
          | Saturate.Sat -> () (* ok *)
          | Saturate.Unsat [] -> assert false
          | Saturate.Unsat (c1::cs) ->
            (* TODO: proof management *)
            Ctx.add_clause_l
              (List.map Conflict_clause.to_bool_clause cs);
            Ctx.raise_conflict
              (Conflict_clause.to_bool_clause c1)
              (Conflict_clause.proof c1)
        end
      | B_lit.Ground_lit lit, sign ->
        assert false
      (* TODO : how to relate proofs?
        if sign
        then Saturate.add_clause (Clause.make_l [lit])
        else Saturate.add_clause (Clause.make_l [Lit.neg lit])
      *)
      | B_lit.Box_clause _,false -> () (* TODO: if unit negative, maybe? *)
      | B_lit.Select_lit (_,_), true -> () (* TODO: should add to saturate, too *)
      | B_lit.Depth_limit _, _
      | B_lit.Fresh _, _ -> ()
    end

  let set_depth_limit d =
    Saturate.set_limit d;
    (* add the set of initial clauses *)
    ignore (Saturate.add_clauses initial_clauses);
    ()

  let on_exit () =
    Util.debugf ~section 1 "@[<2>saturate:@ %a@]"
      (fun k->
         let stats = Saturate.stats() in
         k Saturate.pp_stats stats);
    ()
end

let theory : State.theory_fun = (module Make)

