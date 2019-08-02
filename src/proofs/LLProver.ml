
(** {1 Low Level Prover} *)

open Logtk

module T = LLTerm
module F = LLTerm.Form
module Fmt = CCFormat

type form = LLTerm.Form.t

type res =
  | R_ok
  | R_fail

let section = LLProof.section
let stat_solve = Util.mk_stat "llproof.prove"
let prof_check = Util.mk_profiler "llproof.prove"

module Solver = Sidekick_msat_solver.Make(struct
    module Term = struct
      include T
      let ty = ty_exn
      type state = unit
      let bool () b = if b then Form.true_ else Form.false_
      let abs () t = abs t
      let map_shallow () = map_shallow
    end
    module Ty = struct
      include T
      let is_bool = equal bool
    end
    module Fun = struct
      type t = Bind of Binder.t * Ty.t | Builtin of Builtin.t | Const of ID.t
      let equal a b = match a, b with
        | Bind (b1,ty1), Bind(b2,ty2) -> Binder.equal b1 b2 && Ty.equal ty1 ty2
        | Builtin b1, Builtin b2 -> b1=b2
        | Const id1, Const id2 -> ID.equal id1 id2
        | (Bind _ | Builtin _ | Const _), _ -> false
      let hash _ = 0 (* TODO *)
      let pp out = function
        | Bind (b,_) -> Fmt.fprintf out "(@[bind %a@])" Binder.pp b
        | Builtin b -> Builtin.pp out b
        | Const id -> ID.pp out id
    end
    module Proof = struct type t = unit let pp = Fmt.(const string "<proof>") let default=() end

    module V = Sidekick_core.CC_view

    let cc_view (t:T.t) : _ V.t =
      match T.view t with
      | T.App (f, a) -> V.App_ho (f, Iter.return a)
      | T.AppBuiltin (Builtin.Box_opaque, _) -> V.Opaque t  (* simple equality *)
      | T.AppBuiltin (b,l) ->
        begin match F.view t with
          | F.Eq (a,b) -> V.Eq (a,b)
          | F.Not a -> V.Not a
          | _ -> 
            V.App_fun (Fun.Builtin b, Iter.of_list l)
        end
      | T.Ite (a,b,c) -> V.If (a,b,c)
      | T.Bind {body;binder;ty_var} ->
        V.App_fun (Fun.Bind (binder,ty_var), Iter.return body)
      | Int_pred _ | Rat_pred _ 
      | T.Const _ | T.Var _ | T.Type | T.Arrow _ -> V.Opaque t
  end)

(** main state *)
type t = Solver.t
type final_state = t

let solve_ (solver:t) : res =
  match Solver.solve ~assumptions:[] solver with
    | Solver.Unknown why ->
      Util.debugf ~section 5
        "(@[llprover.prove.fail@ :unknown %a@])" (fun k->k Solver.Unknown.pp why);
      R_fail
    | Solver.Unsat _ ->
      (* TODO: print/check? *)
      Util.debugf ~section 5
        "(@[llprover.prove.success@ :stats %a@])" (fun k->k Solver.pp_stats solver);
      R_ok
    | Solver.Sat m ->
      Util.debugf ~section 1 "(@[llprover.prove.failed@ :model %a@])"
        (fun k->k Solver.Model.pp m);
      R_fail

let can_check : LLProof.tag list -> bool =
  let open Builtin.Tag in
  let f = function
    | T_ho -> true
    | T_lra | T_lia | T_ind | T_data
    | T_distinct | T_ac _ | T_ext -> false
  in
  List.for_all f

module Gensym = struct
  type t = {
    mutable fresh: int;
  }

  let create () : t = {fresh=0}

  let fresh_term (self:t) ~pre (ty:T.t) : T.t =
    let name = Printf.sprintf "_tseitin_%s%d" pre self.fresh in
    self.fresh <- 1 + self.fresh;
    let id = ID.make name in
    T.const ~ty id
end

(* booleans *)
module Th_bool = Sidekick_th_bool_static.Make(struct
    module Gensym = Gensym
    module S = Solver
    module T = T
    type term = T.t

    module F = T.Form
    open Sidekick_th_bool_static

    let mk_bool () = function
      | B_bool true -> F.true_
      | B_bool false -> F.false_
      | B_or a -> F.or_ (Sidekick_util.IArray.to_list a)
      | B_and a -> F.and_ (Sidekick_util.IArray.to_list a)
      | B_equiv (a,b) -> F.equiv a b
      | B_not a -> F.not_ a
      | B_imply (a,b) -> F.imply_l (Sidekick_util.IArray.to_list a) b
      | B_eq (a,b) -> F.eq a b
      | B_ite (a,b,c) -> T.ite a b c
      | B_atom t -> t

    let view_as_bool t =
      match F.view t with
      | F.True -> B_bool true
      | F.False -> B_bool false
      | F.And l -> B_and (Sidekick_util.IArray.of_list l)
      | F.Or l -> B_or (Sidekick_util.IArray.of_list l)
      | F.Equiv (a,b) -> B_equiv (a,b)
      | F.Eq (a,b) -> B_eq (a,b)
      | F.Neq (a,b) -> B_not (F.eq a b)
      | F.Not a -> B_not a
      | F.Xor (a,b) -> B_equiv (a, F.not_ b)
      | F.Imply (a,b) -> B_imply (Sidekick_util.IArray.singleton a, b)
      | F.Atom t ->
        begin match T.view t with
          | Ite (a,b,c) -> B_ite (a,b,c)
          | _ -> B_atom t
        end
      | F.Int_pred _ | F.Rat_pred _ | F.Forall _ | F.Exists _ ->
        B_atom t
  end)

let prove (a:form list) (b:form) : _*_ =
  Util.debugf ~section 3
    "(@[@{<yellow>llprover.prove@}@ :hyps (@[<hv>%a@])@ :concl %a@])"
    (fun k->k (Util.pp_list T.pp) a T.pp b);
  Util.incr_stat stat_solve;
  Util.enter_prof prof_check;
  (* prove [a ∧ -b ⇒ ⊥] *)
  let theories = [Th_bool.theory] in
  let solver = Solver.create ~size:`Small ~store_proof:false ~theories () () in
  List.iter
    (fun t -> Solver.add_clause_l solver [Solver.mk_atom_t solver t])
    (T.Form.not_ b :: a);
  let res = solve_ solver in
  Util.exit_prof prof_check;
  res, solver

let pp_stats = Solver.pp_stats
