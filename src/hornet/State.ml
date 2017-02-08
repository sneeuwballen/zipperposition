
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 State} *)

open Libzipperposition

module SI = Msat.Solver_intf
module FI = Msat.Formula_intf
module TI = Msat.Theory_intf

module Fmt = CCFormat

let section = Util.Section.(make ~parent:root) "state"

type term = FOTerm.t
type ty = Type.t
type statement = (Clause.t, term, ty) Statement.t
type proof = Hornet_types.proof

(** {2 Boolean Literal} *)

module type BOOL_LIT = State_intf.BOOL_LIT

(** {2 Context for Theories} *)

module type CONTEXT = State_intf.CONTEXT

type context = (module CONTEXT)

(** {2 Theory} *)

module type THEORY = State_intf.THEORY
module type THEORY_FUN = State_intf.THEORY_FUN

type theory_fun = State_intf.theory_fun

(** {2 State in a Module} *)

module type S = sig
  module B_lit : BOOL_LIT
  module M : SI.S with type St.formula = B_lit.t and type St.proof = proof
  module Ctx : CONTEXT with module B_lit = B_lit (* for theories *)
  val theories : (module THEORY) list
  val on_exit : unit -> unit
end

module type ARGS = State_intf.ARGS

module Make(A : ARGS) : S = struct
  module B_lit = Bool_lit.Make(struct end)

  exception Theory_conflict of B_lit.t list * Proof.t
  (** Raised by a handler when a conflict is detected *)

  module SAT_theory = struct
    type formula = B_lit.t
    type proof = Hornet_types.proof

    let backtrack_vec : (unit -> unit) CCVector.vector = CCVector.create ()

    let on_assumption_ : (B_lit.t -> unit) list ref = ref []

    type level = int (* offset in [backtrack] *)

    let dummy = 0

    let current_level () = CCVector.length backtrack_vec

    let backtrack lev =
      Util.debugf ~section 5 "@[<2>backtrack to level %d@]" (fun k->k lev);
      while CCVector.length backtrack_vec > lev do
        let f = CCVector.pop_exn backtrack_vec in
        f()
      done

    let assume slice : _ TI.res =
      try
        for i = slice.TI.start to slice.TI.start + slice.TI.length do
          let lit = slice.TI.get i in
          List.iter (fun f -> f lit) !on_assumption_
        done;
        TI.Sat
      with Theory_conflict (c,proof) ->
        TI.Unsat (c, proof)

    let if_sat _ = TI.Sat (* TODO: add corresponding hook in THEORY *)
  end

  module M =
    Msat.Solver.Make
      (B_lit)
      (SAT_theory)
      (struct end)

  module Ctx
    : CONTEXT with module B_lit = B_lit
  = struct
    include A
    module B_lit = B_lit

    type bool_clause = B_lit.t list
    let on_backtrack f = CCVector.push SAT_theory.backtrack_vec f
    let raise_conflict c proof = raise (Theory_conflict (c,proof))
    let add_clause_l l = M.assume l
    let add_clause c = add_clause_l [c]
    module Form = struct
      include Msat.Tseitin.Make(B_lit)
      let imply = make_imply
      let and_ = make_and
      let or_ = make_or
      let not_ = make_not
      let atom = make_atom
    end
    let add_form f = add_clause_l (Form.make_cnf f)
  end

  let add_on_assumption_ f =
    SAT_theory.on_assumption_ := f :: !SAT_theory.on_assumption_

  let theories : (module THEORY) list =
    List.map
      (fun (module Th_fun : THEORY_FUN) ->
         let module Th = Th_fun(Ctx) in
         Util.debugf ~section 2 "@[add_theory %s@]" (fun k->k Th.name);
         add_on_assumption_ Th.on_assumption;
         (module Th : THEORY))
      A.theories

  let on_exit () = List.iter (fun (module Th : THEORY) -> Th.on_exit ()) theories
end

(** {2 State} *)

type signature = Type.t ID.Map.t

type t = (module S)

let create (module A:ARGS) =
  let module M = Make(A) in
  let st = (module M : S) in
  st

let context (t:t) =
  let module M = (val t) in
  (module M.Ctx : CONTEXT)

(** {2 Result} *)

type res =
  | Sat
  | Unsat
  | Unknown

let pp_res out = function
  | Sat -> Fmt.string out "SAT"
  | Unsat -> Fmt.string out "UNSAT"
  | Unknown -> Fmt.string out "UNKNOWN"

let run (t:t): res =
  let module St = (val t) in
  let module F = St.Ctx.Form in
  (* currently at depth [d] *)
  let rec iter (d:int) =
    Util.debugf ~section 1 "@[<2>@{<Yellow>#### DEPTH %d ####@}@]" (fun k->k d);
    (* set depth limit *)
    List.iter
      (fun (module Th : THEORY) -> Th.set_depth_limit d)
      St.theories;
    (* solve under assumption [limit] *)
    let res = St.M.solve ~assumptions:[] () in
    begin match res with
      | St.M.Sat _ ->
        if d = St.Ctx.max_depth
        then Unknown (* TODO: completeness proof(!) *)
        else iter (d+1) (* increase depth *)
      | St.M.Unsat _ ->
        Util.debugf ~section 1 "@[Found unsat@]" (fun k->k);
        Unsat
    end
  in
  (* compute result *)
  let res = iter 1 in
  St.on_exit ();
  res


