
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 State} *)

open Libzipperposition

module SI = Msat.Solver_intf
module FI = Msat.Formula_intf
module TI = Msat.Theory_intf

module Fmt = CCFormat

let section = Util.Section.(make ~parent:root) "state"

(** {2 Proofs} *)

module Proof : sig
  type t
end = struct
  type t = unit
end

(** {2 Boolean Literal} *)

(** Encapsulate objects into boolean literals that can be handled by
    the SAT solver *)

module type BOOL_LIT = sig
  type view =
    | Fresh of int
    | Select_lit of Clause.General.t * Clause.General.idx

  type t = private {
    view: view;
    sign: bool;
  }

  val fresh : unit -> t
  val select_lit : Clause.General.t -> Clause.General.idx -> t

  include Msat.Formula_intf.S with type t := t and type proof = Proof.t
end

module Bool_lit(X:sig end) : BOOL_LIT = struct
  type view =
    | Fresh of int
    | Select_lit of Clause.General.t * Clause.General.idx

  type proof = Proof.t

  module CG = Clause.General

  type t = {
    view: view;
    sign: bool;
  }

  let make_ sign view = {sign;view}

  let fresh =
    let n = ref 0 in
    fun () -> let lit = make_ true (Fresh !n) in incr n; lit

  let dummy = fresh()

  let select_lit c i = make_ true (Select_lit (c,i))

  let neg t = {t with sign=not t.sign}

  let norm (t:t): t * FI.negated =
    if t.sign
    then t, FI.Same_sign
    else neg t, FI.Negated

  let equal a b : bool =
    a.sign = b.sign
    &&
    begin match a.view, b.view with
      | Fresh i, Fresh j ->  i=j
      | Select_lit (c1,i1), Select_lit (c2,i2) ->
        Clause.equal (CG.as_clause c1) (CG.as_clause c2) && i1=i2
      | Fresh _, _
      | Select_lit _, _ -> false
    end

  let hash a : int = match a.view with
    | Fresh i -> Hash.combine3 10 (Hash.bool a.sign) (Hash.int i)
    | Select_lit (c,i) ->
      Hash.combine4 20 (Hash.bool a.sign)
        (Clause.hash (CG.as_clause c)) (Hash.int (i:>int))

  let print out l =
    let pp_view out = function
      | Fresh i -> Fmt.fprintf out "fresh_%d" i
      | Select_lit (c,i) ->
        Fmt.fprintf out "@[select@ :idx %d@ :clause %a@]" (i:>int) CG.pp c
    in
    if l.sign
    then Fmt.within "(" ")" pp_view out l.view
    else Fmt.fprintf out "(¬%a)" pp_view l.view
end

(** {2 Context for Theories} *)

module type CONTEXT = sig
  module B_lit : BOOL_LIT with type proof = Proof.t

  type bool_clause = B_lit.t list

  val raise_conflict : bool_clause -> Proof.t -> 'a

  val on_backtrack : (unit -> unit) -> unit
  (** Push the given callback on a stack. It will be
      called when the SAT solver backtracks. *)

  val add_clause : bool_clause -> unit
  val add_clause_l : bool_clause list -> unit
end

type context = (module CONTEXT)

(** {2 Theory} *)

module type THEORY = sig
  module Ctx : CONTEXT

  val name : string
  val on_assumption : Ctx.B_lit.t -> unit
end

module type THEORY_FUN = functor(C:CONTEXT) -> THEORY with module Ctx = C

type theory_fun = (module THEORY_FUN)

(** {2 State in a Module} *)

module type S = sig
  module B_lit : BOOL_LIT with type proof = Proof.t
  module M : SI.S with type St.formula = B_lit.t and type St.proof = Proof.t
  module Ctx : CONTEXT with module B_lit = B_lit (* for theories *)
end

module type ARG = sig
  val theories : theory_fun list
end

module Make(A : ARG) : S = struct
  module B_lit = Bool_lit(struct end)

  exception Theory_conflict of B_lit.t list * Proof.t
  (** Raised by a handler when a conflict is detected *)

  module SAT_theory = struct
    type formula = B_lit.t
    type proof = Proof.t

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
    module B_lit = B_lit
    type bool_clause = B_lit.t list
    let on_backtrack f = CCVector.push SAT_theory.backtrack_vec f
    let raise_conflict c proof = raise (Theory_conflict (c,proof))
    let add_clause_l l = M.assume l
    let add_clause c = add_clause_l [c]
  end

  let add_on_assumption_ f =
    SAT_theory.on_assumption_ := f :: !SAT_theory.on_assumption_

  let () =
    List.iter
      (fun (module Th_fun : THEORY_FUN) ->
         let module Th = Th_fun(Ctx) in
         Util.debugf ~section 2 "@[add_theory %s@]" (fun k->k Th.name);
         add_on_assumption_ Th.on_assumption)
      A.theories
end

type sat_t = (module S)

(** {2 State} *)

type signature = Type.t ID.Map.t

type t = {
  conf: Flex_state.t;
  ord: Ordering.t;
  signature: signature;
  sat: sat_t;
}

let create ~conf ~ord ~signature ~theories () =
  let module M = Make(struct let theories = theories end) in
  let sat = (module M : S) in
  { conf; ord; signature; sat }

let conf t = t.conf
let ord t = t.ord
