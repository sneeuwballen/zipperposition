
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Quasipure Literal Elimination} *)
open Logtk
open Libzipperposition

module type S = sig
  module Env : Env.S
  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module CS = C.ClauseSet
  module L = Literal
  module T = Term
  module SAT = Sat_solver.Make ()
  module VarPayload = struct 
    type t = ID.t * bool
    let dummy = (ID.gensym (), false)
  end
  module BoolVar : Bool_lit_intf.S with type payload = VarPayload.t = struct
    include Bool_lit.Make(struct 
      include VarPayload
    end)

    let pp out bv =
      CCFormat.fprintf out "%a%s" ID.pp (fst (payload bv))
        (if snd (payload bv) then "+" else "-")
  end

  let do_qle cs =
    (* TODO: Check first-orderness of problem. *)
    let sym_of_lit lit =
      if L.is_predicate_lit lit then (
        match lit with
        | L.Equation(lhs, _, _) ->
          if (T.is_const (T.head_term lhs)) then (
            let sym = T.as_const_exn (T.head_term lhs) in
            let pos_var = BoolVar.make (sym, true) in
            let neg_var = BoolVar.make (sym, false) in
            Some (sym, (pos_var, neg_var))
          ) else None
        | _ -> None
      ) else None
    in
    let syms = ID.Tbl.create 128 in
    CS.iter (fun c ->
      Array.iter (fun (pred, vars) -> ID.Tbl.replace syms pred vars)
        (CCArray.filter_map sym_of_lit (C.lits c))) cs;
    CCFormat.printf "%a@." (ID.Tbl.pp ID.pp (CCPair.pp BoolVar.pp BoolVar.pp))
      syms;
    Iter.iter (fun (pos_var, neg_var) ->
        SAT.add_clause ~proof:Proof.Step.trivial [BoolVar.neg pos_var])
      (ID.Tbl.values syms)
  let setup () =
    Signal.once Env.on_start
      (fun () -> do_qle (CS.of_iter (Env.get_passive ())))
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module QLE = Make(E) in

    QLE.setup ()
  in
  { Extensions.default with Extensions.
                         name = "qle";
                         prio = 50;
                         env_actions = [action];
  }
