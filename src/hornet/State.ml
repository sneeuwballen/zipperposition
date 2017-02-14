
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 State} *)

open Libzipperposition
open Hornet_types

module SI = Msat.Solver_intf
module FI = Msat.Formula_intf
module TI = Msat.Theory_intf

module Fmt = CCFormat

let section = Util.Section.(make ~parent:root) "state"

type term = FOTerm.t
type ty = Type.t
type statement = (Clause.t, term, ty) Statement.t

(** {2 Context for Theories} *)

module type CONTEXT = State_intf.CONTEXT

type context = (module CONTEXT)

(** {2 Theory} *)

module type THEORY = State_intf.THEORY
module type THEORY_FUN = State_intf.THEORY_FUN

type theory_fun = State_intf.theory_fun

(** {2 State in a Module} *)

module type S = sig
  module M : SI.S with type St.formula = Bool_lit.t and type St.proof = proof
  module Ctx : CONTEXT (* for theories *)
  val theories : (module THEORY) list
  val pp_dimacs: unit -> unit
end

module type ARGS = State_intf.ARGS

(** Mapping between proofs and tags *)
module Proof_tbl : sig
  val tag_of_proof : proof -> int
  val proof_of_tag : int -> proof
end = struct
  let tbl : (int,proof) Hashtbl.t = Hashtbl.create 128
  let count = ref 0
  let tag_of_proof (p:proof): int =
    let n = CCRef.incr_then_get count in
    Hashtbl.add tbl n p;
    n
  let proof_of_tag i: proof = Hashtbl.find tbl i
end

module Make(A : ARGS) : S = struct
  (* defined below *)
  let send_event_ : (event -> unit) ref = ref (fun _ -> assert false)

  module SAT_theory = struct
    type formula = Bool_lit.t
    type proof = Hornet_types.proof

    let backtrack_vec : (unit -> unit) CCVector.vector = CCVector.create ()

    let on_assumption_ : (Bool_lit.t -> unit) list ref = ref []

    type level = int (* offset in [backtrack] *)

    let dummy = 0

    let current_level () = CCVector.length backtrack_vec

    let backtrack lev =
      Util.debugf ~section 2 "@[<2>@{<Blue>backtrack to level %d@}@]" (fun k->k lev);
      while CCVector.length backtrack_vec > lev do
        let f = CCVector.pop_exn backtrack_vec in
        f()
      done

    let assume slice : _ TI.res =
      for i = slice.TI.start to slice.TI.start + slice.TI.length - 1 do
        let lit = slice.TI.get i in
        List.iter (fun f -> f lit) !on_assumption_
      done;
      TI.Sat

    let if_sat _ =
      !send_event_ E_if_sat;
      TI.Sat
  end

  module M =
    Msat.Solver.Make
      (Bool_lit)
      (SAT_theory)
      (struct end)

  module Ctx : CONTEXT = struct
    include A
    module Bool_lit = Bool_lit

    type bool_clause = Bool_lit.t list
    let bool_state = Bool_lit.create_state ()
    let on_backtrack f = CCVector.push SAT_theory.backtrack_vec f
    let add_clause_l proof l =
      let tag = Proof_tbl.tag_of_proof proof in
      let l = List.rev_map (CCList.sort_uniq ~cmp:Bool_lit.compare) l in
      Util.debugf ~section 2 "@[<2>add_bool_clauses@ (@[<hv>%a@])@]"
        (fun k->k (Util.pp_list Bool_lit.pp_clause) l);
      M.assume ~tag l
    let add_clause p c = add_clause_l p [c]
    module Form = struct
      include Msat.Tseitin.Make(struct
          include Bool_lit
          let fresh () = fresh bool_state (* need to pass context implicitely here *)
        end)
      let imply = make_imply
      let and_ = make_and
      let or_ = make_or
      let not_ = make_not
      let atom = make_atom
    end
    let add_form p f = add_clause_l p (Form.make_cnf f)
    let send_event e = !send_event_ e

    let renaming_ = Subst.Renaming.create()
    let renaming_cleared () =
      Subst.Renaming.clear renaming_;
      renaming_
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

  let () =
    (* process one single event, possibly triggering other events *)
    let process_one_event e =
      Util.debugf ~section 5 "@[process_event@ %a@]" (fun k->k Event.pp e);
      try
        List.iter (fun (module Th : THEORY) -> Th.on_event e) theories
      with exc ->
        let trace = Util.Exn.string_of_backtrace () in
        Util.debugf ~section 1 "@[error when processing event %a@ %s@]"
          (fun k->k Event.pp e trace);
        raise exc
    in
    (* queue of events to handle *)
    let e_queue : event Queue.t = Queue.create() in
    let in_event : bool ref = ref false in
    let send_event e =
      if !in_event then Queue.push e e_queue
      else (
        (* loop for processing events *)
        in_event := true;
        process_one_event e;
        while not (Queue.is_empty e_queue) do
          let e = Queue.pop e_queue in
          process_one_event e;
        done;
        in_event := false;
      )
    in
    send_event_ := send_event

  (* print SAT problem *)
  let pp_dimacs () =
    CCOpt.iter
      (fun file ->
         Util.debugf ~section 1 "(@[export_dimacs@ %S@])" (fun k->k file);
         CCIO.with_out file
           (fun oc ->
              let out = Format.formatter_of_out_channel oc in
              M.export_dimacs out ();
              Format.pp_print_flush out ()))
      A.dimacs_file
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

(** {2 Rebuild Proof} *)

(* bool_clause -> 'a *)
module Bool_clause_tbl = CCHashtbl.Make(struct
    type t = bool_clause
    let equal = CCList.equal Bool_lit.equal
    let hash = Hash.list Bool_lit.hash
  end)

module Proof_build(X : sig
    module St : S
    val proof : St.M.Proof.proof
  end) =
struct
  (* convert the SAT proof into a normal proof *)
  let rebuild () : proof_with_res =
    let module Pr = X.St.M.Proof in
    (* map bool_clause -> its proof *)
    let tbl : proof Bool_clause_tbl.t = Bool_clause_tbl.create 64 in
    let bool_clause_of_sat c : bool_clause =
      Pr.to_list c |> List.map (fun a -> a.X.St.M.St.lit)
    in
    Pr.check X.proof;
    let rec aux p : proof * bool_clause = match Pr.expand p with
      | { Pr.conclusion=c; step=Pr.Assumption|Pr.Hypothesis } ->
        begin match X.St.M.get_tag c with
          | None -> assert false
          | Some tag ->
            let c = bool_clause_of_sat c in
            let proof = Proof_tbl.proof_of_tag tag in
            proof, c
        end
      | { Pr.step = Pr.Lemma lemma; conclusion=c } ->
        let c = bool_clause_of_sat c in
        lemma, c
      | { Pr.conclusion=c; step = Pr.Resolution (p1,p2,{X.St.M.St.lit;_}) } ->
        let c = bool_clause_of_sat c in
        (* atomic resolution step *)
        begin match Bool_clause_tbl.get tbl c with
          | Some pr -> pr, c
          | None ->
            let p1, c1 = aux p1 in
            let p2, c2 = aux p2 in
            let proof = Proof.bool_res lit c1 p1 c2 p2 in
            Bool_clause_tbl.add tbl c proof;
            proof, c
        end
    in
    let proof, c = aux X.proof in
    proof, PR_bool_clause c
end

(** {2 Result} *)

type res =
  | Sat
  | Unsat of proof_with_res
  | Unknown

let pp_res out = function
  | Sat -> Fmt.string out "SAT"
  | Unsat _ -> Fmt.string out "UNSAT"
  | Unknown -> Fmt.string out "UNKNOWN"

let run (t:t): res =
  let module St = (val t) in
  let module F = St.Ctx.Form in
  St.Ctx.send_event Hornet_types.(E_stage Stage_init);
  St.Ctx.send_event Hornet_types.(E_stage Stage_presaturate);
  St.Ctx.send_event Hornet_types.(E_stage Stage_start);
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
        then Unknown (* TODO: completeness proof(!); also, ask theories if complete *)
        else iter (d+1) (* increase depth *)
      | St.M.Unsat us ->
        Util.debugf ~section 1 "@[Found unsat@]" (fun k->k);
        let module Rebuild = Proof_build(struct
            module St = St
            let proof = us.SI.get_proof ()
          end) in
        let p = Rebuild.rebuild() in
        St.Ctx.send_event (Hornet_types.E_found_unsat p);
        Unsat p
    end
  in
  (* compute result *)
  let res = iter 1 in
  St.Ctx.send_event Hornet_types.(E_stage Stage_exit);
  St.pp_dimacs();
  res


