
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Compute Precedence} *)

open Logtk

module T = FOTerm
module PF = PFormula

let prof_mk_prec = Util.mk_profiler "mk_precedence"

let section = Util.Section.make ~parent:Const.section "compute_prec"

type t = {
  mutable constrs : (int * [`partial] Precedence.Constr.t) list;
  mutable constr_rules : (int * (FOTerm.t Sequence.t -> [`partial] Precedence.Constr.t)) list;
  last_constr : [`total] Precedence.Constr.t;
  mutable weight_rule : (FOTerm.t Sequence.t -> ID.t -> int);
  mutable status : (ID.t * Precedence.symbol_status) list;
}

(* sequence of symbols -> sequence of (frequency, list of symbols with this freq) *)
let by_freq_ s =
  let tbl = ID.Tbl.create 32 in
  s (fun s ->
      let n = try ID.Tbl.find tbl s with Not_found -> 0 in
      ID.Tbl.replace tbl s (n+1)
    );
  let rev_tbl = Hashtbl.create (ID.Tbl.length tbl) in
  ID.Tbl.iter
    (fun s freq ->
       let l = try Hashtbl.find rev_tbl freq with Not_found -> [] in
       Hashtbl.replace rev_tbl freq (s :: l))
    tbl;
  Sequence.of_hashtbl rev_tbl

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
    |> Sequence.flat_map T.Seq.symbols
    |> ID.Set.of_seq
    |> ID.Set.to_list
  in
  (* constraints *)
  let constrs =
    t.constrs @
    List.map (fun (p,rule) -> p, rule seq) t.constr_rules
  in
  Util.debugf ~section 2 "@[<2>%d precedence constraints@]"
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
