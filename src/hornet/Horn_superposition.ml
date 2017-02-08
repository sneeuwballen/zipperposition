
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

  (** {2 Saturation Algorithm} *)

  let rules_simp : HC.t rule_simp list = [ ]

  let rules_simp_n : HC.t rule_simp_n list = [ ]

  let rules_infer : HC.t rule_infer list = [ ]

  module Conflict_clause : sig
    type t = private HC.t (* an empty clause *)

    val make : HC.t -> t
    val to_bool_clause : t -> Bool_lit.t list
    val proof : t -> HC.proof
    val pp : t CCFormat.printer
  end = struct
    type t = HC.t (* an empty clause *)
    let make c = c
    let to_bool_clause c : Bool_lit.t list = assert false (* TODO: walk the proof *)
    let proof _ = assert false (* TODO *)
    let pp = HC.pp
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
      | Unsat of Conflict_clause.t list

    type stats = {
      num_clauses: int; (* number of clauses *)
    }

    let pp_stats out s: unit =
      Fmt.fprintf out "{@[num_clauses: %d@]}" s.num_clauses

    (** {6 Local State} *)

    let limit_ : int ref = ref 1


    let set_limit i = assert (i>0); limit_ := i

    let stats (): stats = assert false (* TODO *)

    (** {6 Saturation} *)

    exception Conflict_exn of HC.t

    let add_horn c =
      Util.debugf ~section 3 "@[<2>saturate.add_horn@ %a@]" (fun k->k HC.pp c);
      assert false (*  TODO *)

    let add_clause c =
      Util.debugf ~section 3 "@[<2>saturate.add_clause@ %a@]" (fun k->k C.pp c);
      begin match C.classify c with
        | C.Horn hc -> add_horn hc
        | C.General -> Sat (* wait until it is split *)
      end

    let rec add_clauses (l:C.t list) = match l with
      | [] -> Sat
      | c :: tail ->
        match add_clause c with
          | Sat -> add_clauses tail
          | Unsat p -> Unsat p
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
      | Saturate.Unsat [] -> assert false
      | Saturate.Unsat (c::_) ->
        Ctx.send_event (E_found_unsat (Conflict_clause.proof c))
    end

  (* no direct communication with SAT solver *)
  let on_assumption _ = ()

  (* TODO *)
  let on_event e =
    begin match e with
      | E_add_component _
      | E_remove_component _
      | E_select_lit (_,_,_)
      | E_unselect_lit (_,_,_) ->
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

