
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Compute Precedence} *)

open Libzipperposition

module T = FOTerm

let prof_mk_prec = Util.mk_profiler "mk_precedence"

let section = Util.Section.make ~parent:Const.section "compute_prec"

type 'a parametrized = Statement.clause_t Sequence.t -> 'a

type t = {
  mutable constrs : (int * [`partial] Precedence.Constr.t) list;
  mutable constr_rules : (int * [`partial] Precedence.Constr.t parametrized) list;
  last_constr : [`total] Precedence.Constr.t;
  mutable weight_rule : (ID.t -> int) parametrized;
  mutable status : (ID.t * Precedence.symbol_status) list;
}

(* uniform weight *)
let _default_weight _ _ = 1

let create () =
  { constrs = [];
    constr_rules = [];
    last_constr = Precedence.Constr.alpha;
    weight_rule = _default_weight;
    status = [];
  }

let add_constr t p c =
  t.constrs <- (p,c)::t.constrs

let add_constrs t l =
  List.iter (fun (p,c) -> add_constr t p c) l

let add_constr_rule t p r =
  t.constr_rules <- (p, r) :: t.constr_rules

let set_weight_rule t r =
  t.weight_rule <- r

let add_status t l = t.status <- List.rev_append l t.status

let mk_precedence t seq =
  Util.enter_prof prof_mk_prec;
  (* set of symbols *)
  let symbols =
    seq
    |> Sequence.flat_map Statement.Seq.terms
    |> Sequence.flat_map T.Seq.symbols
    |> ID.Set.of_seq
    |> ID.Set.to_list
  in
  (* constraints *)
  let constrs =
    t.constrs @
    List.map (fun (p,rule) -> p, rule seq) t.constr_rules
  in
  Util.debugf ~section 2 "@[<2>%d precedence constraint(s)@]"
    (fun k->k(List.length constrs));
  let weight = t.weight_rule seq in
  let constr = Precedence.Constr.compose_sort constrs in
  let constr = Precedence.Constr.compose constr t.last_constr in
  let p = Precedence.create ~weight constr symbols in
  (* multiset status *)
  List.iter
    (fun (s,status) -> Precedence.declare_status p s status)
    t.status;
  Util.exit_prof prof_mk_prec;
  p
