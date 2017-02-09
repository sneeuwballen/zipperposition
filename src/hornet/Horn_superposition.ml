
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(* {1 Superposition on Horn Clauses} *)

open Libzipperposition

module T = FOTerm
module C = Clause
module HC = Horn_clause
module Fmt = CCFormat

open Hornet_types

let section = Util.Section.make "horn_sup"

module Make : State.THEORY_FUN = functor(Ctx : State_intf.CONTEXT) -> struct
  module Ctx = Ctx

  (* index term->clause *)
  module CP_idx = NPDtree.MakeTerm(HC.With_pos)

  let name = "horn_superposition"

  (* a simplification rule *)
  type 'a rule_simp = 'a -> 'a option

  (* a simplification rule yielding multiple clauses *)
  type 'a rule_simp_n = 'a -> 'a list option

  (* an inference rule *)
  type 'a rule_infer = 'a -> 'a list

  (** {2 Clause Sets} *)

  module type CLAUSE_SET = sig
    val add : HC.t -> unit
    val mem : HC.t -> bool
    val remove : HC.t -> unit
  end

  (* positive unit clauses *)
  module Active_set : sig
    include CLAUSE_SET
    val size: unit -> int

    (* index on the head equation sides of positive unit clauses
       (active res/paramodulation) *)
    val idx_heads : unit -> CP_idx.t

    (* index on subterms of the first body lit of non-unit clauses
       (passive res/paramodulation) *)
    val idx_body0_sub : unit -> CP_idx.t
  end = struct
    let tbl : unit HC.Tbl.t = HC.Tbl.create 512
    let size () = HC.Tbl.length tbl

    let idx_heads_ : CP_idx.t ref = ref (CP_idx.empty ())
    let idx_body0_sub_ : CP_idx.t ref = ref (CP_idx.empty ())

    let idx_heads () = !idx_heads_
    let idx_body0_sub () = !idx_body0_sub_

    type idx_elt = term * HC.With_pos.t

    type relevant_pos =
      | Head of idx_elt list * idx_elt list (* active, passive *)
      | Body0 of idx_elt list (* passive res, passive sup *)
      | No_pos

    let positions_body c =
      assert (HC.body_len c > 0);
      assert false (* TODO: all positions in which we can rewrite *)

    let position_sup t = [] (* TODO *)

    let relevant_pos (c:HC.t): relevant_pos =
      No_pos
      (* FIXME
      if HC.body_len c = 0
      then match HC.head c with
        | Lit.Atom (t,sign) ->
          assert sign;
          Head ([t, HC.head_pos c], position_sup t)
        | Lit.Eq (t,u,sign) ->
          assert sign;
          assert false
          (* TODO
          begin match Ord.compare Ctx.ord t u with
            | Comparison.Gt ->
              Position.With.(HC.body

          end
             *)
        | Lit.Bool _ -> No_pos
      else begin match HC.body0 c with
        | Lit.Bool _ -> No_pos
        | Lit.Atom (t,sign) ->
          assert sign;
          Body0 (Some (t, HC.body0_pos c), positions_body c)
        | Lit.Eq _ ->
          Body0 (None, positions_body c)
      end
         *)

    let add c =
      if not (HC.Tbl.mem tbl c) then (
        HC.Tbl.add tbl c ();
        begin match relevant_pos c with
          | Head (active, subs) ->
            idx_heads_ := CP_idx.add_list !idx_heads_ active;
            idx_body0_sub_ := CP_idx.add_list !idx_body0_sub_ subs
          | Body0 subs ->
            idx_body0_sub_ := CP_idx.add_list !idx_body0_sub_ subs
          | No_pos -> ()
        end
      )

    let mem c = HC.Tbl.mem tbl c

    let remove c =
      if HC.Tbl.mem tbl c then (
        HC.Tbl.remove tbl c;
        begin match relevant_pos c with
          | Head (active, subs) ->
            idx_heads_ := CP_idx.remove_list !idx_heads_ active;
            idx_body0_sub_ := CP_idx.remove_list !idx_body0_sub_ subs
          | Body0 subs ->
            idx_body0_sub_ := CP_idx.remove_list !idx_body0_sub_ subs
          | No_pos -> ()
        end
      )
  end

  module Passive_set : sig
    val add : HC.t -> unit
    val add_l : HC.t list -> unit
    val add_seq : HC.t Sequence.t -> unit
    val next : unit -> HC.t option
  end = struct
    (* priority queue *)
    module H = CCHeap.Make(struct
        type t = (int * HC.t)
        let leq (i1, c1) (i2, c2): bool =
          i1 < i2 || (i1 = i2 && HC.compare c1 c2 <= 0)
      end)

    (* "weight" of a clause. for now, just favor unit clauses *)
    let weight_ (c:HC.t): int =
      let n = HC.body_len c in
      1 + n

    let q_ : H.t  ref = ref H.empty

    let add1_ q c = H.add q (weight_ c,c)

    let add c = q_ := add1_ !q_ c
    let add_l l = q_ := List.fold_left add1_ !q_ l
    let add_seq l = q_ := Sequence.fold add1_ !q_ l

    let next () = match H.take !q_ with
      | None -> None
      | Some (new_q, (_,c)) ->
        q_ := new_q;
        Some c
  end

  (** {2 Simplifications} *)

  module Simplifications : sig
    val rules_simp_fast : HC.t rule_simp list
    val rules_simp_full : HC.t rule_simp list
    val rules_simp_n : HC.t rule_simp_n list
  end = struct
    (* simplification of first body literal *)
    let simp_body0 c: HC.t option = match HC.body0 c with
      | None -> None
      | Some (Lit.Bool true) ->
        (* trivial body literal, remove *)
        let c' =
          HC.make ~constr:(HC.constr c)
            (HC.head c) (HC.body_tail c) (Proof.hc_simplify c)
        in
        Some c'
      | Some (Lit.Eq (t, u, true)) when T.equal t u ->
        (* [a=a] -> true *)
        let c' =
          HC.make ~constr:(HC.constr c)
            (HC.head c) (HC.body_tail c) (Proof.hc_simplify c)
        in
        Some c'
      | Some lit when not (Lit.sign lit) -> assert false
      | Some _ -> None


    let rules_simp_fast = [ simp_body0 ]

    (* TODO: add some form of demodulation, both positive and in body0 *)
    (* TODO: rewriting *)

    let rules_simp_full = rules_simp_fast @ [ ]

    let rules_simp_n = [ ]
  end

  (** {2 Proofs of False} *)
  module Proof_of_false : sig
    type t = proof

    val bool_lits : t -> bool_lit Sequence.t

    val bool_lits_l : t -> bool_lit list

    val to_bool_clause : t -> bool_clause
  end = struct
    type t = proof

    let rec bool_lits p = match p with
      | P_from_stmt _ -> Sequence.empty
      | P_avatar_split c
      | P_split c ->
        begin match C.bool_lit c with
          | None -> Sequence.empty
          | Some (lazy blit) -> Sequence.return blit
        end
      | P_instance (c,_) -> bool_lits (C.proof c)
      | P_hc_superposition sup ->
        Sequence.append
          (HC.proof sup.hc_sup_active |> bool_lits)
          (HC.proof sup.hc_sup_passive |> bool_lits)
      | P_hc_simplify c -> bool_lits (HC.proof c)

    let bool_lits_l p = bool_lits p |> Sequence.to_rev_list

    (* find bool lits, then deduplicate and negate them *)
    let to_bool_clause (p:t): bool_clause =
      bool_lits p
      |> Bool_lit.Tbl.of_seq_count
      |> Bool_lit.Tbl.keys
      |> Sequence.map Bool_lit.neg
      |> Sequence.to_rev_list
  end

  (** {2 Saturation} *)

  (** Keeps a set of clauses that are saturated up to some limit.
      The limit is on derivations: a clause that has been derived using
      "too many" non-decreasing steps is thrown away.
      This is sufficient for saturation to always terminate.

      The state is a set of Horn clauses, and is backtrackable. *)

  let rules_infer : HC.t rule_infer list = [ ]

  module Saturate : sig
    type res =
      | Sat
      | Unsat of  Proof_of_false.t

    type stats = {
      num_clauses: int; (* number of clauses *)
    }

    val pp_stats : stats CCFormat.printer

    val stats : unit -> stats

    val set_limit : int -> unit
    (** Set the limit on derivations *)

    val add_clause : C.t -> res
    (** [add_clause c] adds the clause [c] to the set, and applies
        Avatar splitting and Inst_gen_eq to it. *)

    val add_clauses : C.t list -> res
    (** Add a list of clauses *)

    val add_horn : HC.t -> res
    (** [add_horn c] adds the clause [c]  to the set, and saturates it
        again. If, during saturation, the empty clause is derived,
        [Unsat l] is returned (where [l] is a non-empty list of empty clauses).
        Otherwise, [Sat] is returned. *)
  end = struct
    type res =
      | Sat
      | Unsat of Proof_of_false.t

    type stats = {
      num_clauses: int; (* number of clauses *)
    }

    let pp_stats out s: unit =
      Fmt.fprintf out "{@[num_clauses: %d@]}" s.num_clauses

    (** {6 Local State} *)

    let limit_ : int ref = ref 1

    let set_limit i = assert (i>0); limit_ := i

    let stats (): stats = {
      num_clauses=Active_set.size();
    }

    (** {6 Saturation} *)

    (* simplify using given rules *)
    let simplify rules0 c =
      let rec aux rules c = match rules with
        | [] -> c
        | r :: rules_tail ->
          begin match r c with
            | None -> aux rules_tail c
            | Some c' -> aux rules0 c (* from start *)
          end
      in
      aux rules0 c

    let simplify_fast = simplify Simplifications.rules_simp_fast
    let simplify_full = simplify Simplifications.rules_simp_full

    exception Conflict_exn of proof

    (* the main saturation loop *)
    let rec saturation_loop () = match Passive_set.next () with
      | None -> Sat
      | Some c ->
        Util.debugf ~section 2 "@[<2>@{<Blue>## saturate@}: given clause@ %a@]"
          (fun k->k HC.pp c);
        let c = simplify_full c in
        (* infer new clauses *)
        let new_c : HC.t Sequence.t =
          Sequence.of_list rules_infer
          |> Sequence.flat_map_l (fun rule -> rule c)
          |> Sequence.map simplify_fast
          |> Sequence.filter (fun c -> not (HC.is_trivial c))
        in
        Passive_set.add_seq new_c;
        saturation_loop ()

    let add_horn c : res =
      Util.debugf ~section 2 "@[<2>saturate.add_horn@ %a@]" (fun k->k HC.pp c);
      let c = simplify_fast c in
      Passive_set.add c;
      saturation_loop ()

    let add_clause c =
      Util.debugf ~section 3 "@[<2>saturate.add_clause@ %a@]" (fun k->k C.pp c);
      begin match C.classify c with
        | C.Horn hc -> add_horn hc
        | C.General -> Sat (* wait until it is split *)
      end

    let rec add_clauses (l:C.t list) = match l with
      | [] -> Sat
      | c :: tail ->
        begin match add_clause c with
          | Sat -> add_clauses tail
          | Unsat p -> Unsat p
        end
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

  let set_depth_limit d =
    Saturate.set_limit d;
    ()

  let presaturate () =
    (* add the set of initial clauses *)
    Util.debug ~section 2 "start presaturation";
    let res = Saturate.add_clauses initial_clauses in
    begin match res with
      | Saturate.Sat -> ()
      | Saturate.Unsat p -> Ctx.send_event (E_found_unsat p)
    end

  (* no direct communication with SAT solver *)
  let on_assumption _ = ()

  (* TODO *)
  let on_event e =
    begin match e with
      | E_add_component _
      | E_remove_component _
      | E_add_ground_lit _
      | E_remove_ground_lit _
      | E_select_lit (_,_,_)
      | E_unselect_lit _ ->
        () (* TODO: add/remove clauses from saturated set *)
      | E_stage Stage_presaturate ->
        presaturate ()
      | E_stage Stage_exit ->
        Util.debugf ~section 1 "@[<2>saturate:@ %a@]"
          (fun k->
             let stats = Saturate.stats() in
             k Saturate.pp_stats stats);
        ()
      | E_stage (Stage_start | Stage_init) -> ()
      | E_found_unsat _ -> ()
    end

  (* TODO:
     - a decent saturation state
     * active (unit) clauses
     * passive (horn) clauses
     * demod index
     - initial saturation (up to (parameter) initial depth)
     - react to some events
  *)
end

let theory : State.theory_fun = (module Make)

