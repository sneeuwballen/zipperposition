
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 AC redundancy} *)

open Libzipperposition

module T = FOTerm
module Lit = Literal

open AC_intf

let section = Util.Section.(make ~parent:zip) "AC"

let prof_simplify = Util.mk_profiler "ac.simplify"

let stat_ac_simplify = Util.mk_stat "ac.simplify"
let stat_ac_redundant = Util.mk_stat "ac.redundant"

type spec = AC_intf.spec

module type S = AC_intf.S

module Make(Env : Env.S) : S with module Env = Env = struct
  module Ctx = Env.Ctx
  module Env = Env
  module C = Env.C

  let tbl = ID.Tbl.create 3
  let proofs = ID.Tbl.create 3
  let on_add = Signal.create ()

  let axioms s ty =
    let ty_args_n, ty_args, _ty_ret = Type.open_poly_fun ty in
    if List.length ty_args <> 2
    then Util.errorf ~where:"AC" "AC symbol `%a`must be of arity 2" ID.pp s;
    (* create type variables, for polymorphic AC symbols *)
    let ty_vars = CCList.init ty_args_n (fun i -> HVar.make ~ty:Type.tType i) in
    let ty_vars_t = List.map Type.var ty_vars in
    (* type applied to the new variables *)
    let ty' = Type.apply ty ty_vars_t in
    let n', ty_args, ty_ret = Type.open_poly_fun ty' in
    assert (n' = 0);
    (* check consistency of types *)
    begin match ty_args with
      | [a; b] ->
        if not (Type.equal a b && Type.equal a ty_ret)
        then Util.errorf ~where:"AC"
            "AC symbol `%a` argument types must be `@[%a@]`" ID.pp s Type.pp ty_ret;
      | _ -> assert false
    end;
    let x = T.var_of_int ~ty:ty_ret (ty_args_n + 1) in
    let y = T.var_of_int ~ty:ty_ret (ty_args_n + 2) in
    let z = T.var_of_int ~ty:ty_ret (ty_args_n + 3) in
    let f x y = T.app_full (T.const ~ty s) ty_vars_t [x;y] in
    let res = ref [] in
    (* build clause l=r *)
    let add_clause l r =
      let proof = ProofStep.mk_trivial in
      let c = C.create ~trail:Trail.empty [ Lit.mk_eq l r ] proof in
      C.set_flag SClause.flag_persistent c true;
      res := c :: !res
    in
    add_clause (f x y) (f y x);
    add_clause (f (f x y) z) (f x (f y z));
    add_clause (f x (f y z)) (f z (f x y));
    add_clause (f x (f y z)) (f y (f x z));
    add_clause (f x (f y z)) (f z (f y x));
    !res

  (* list of newly declared constants *)
  let new_ids_ : (ID.t * Type.t) list ref = ref []

  let add_ ?proof ~ty s =
    let proof = match proof with
      | Some p -> p
      | None -> (* new axioms *)
          List.map C.proof (axioms s ty)
    in
    if not (ID.Tbl.mem tbl s)
    then (
      let instance = {ty; sym=s} in
      new_ids_ := (s, ty) :: !new_ids_;
      ID.Tbl.replace tbl s instance;
      ID.Tbl.replace proofs s proof;
      Signal.send on_add instance
    )

  let is_ac s = ID.Tbl.mem tbl s

  let exists_ac () = ID.Tbl.length tbl > 0

  let find_proof s = ID.Tbl.find proofs s

  let symbols () =
    ID.Tbl.fold
      (fun s _ set -> ID.Set.add s set)
      tbl ID.Set.empty

  let symbols_of_terms seq =
    let module A = T.AC(struct
        let is_ac = is_ac
        let is_comm _ = false
      end) in
    A.symbols seq

  let proofs () =
    ID.Tbl.fold
      (fun _ proofs acc -> List.rev_append proofs acc)
      proofs []

  (** {2 Rules} *)

  let is_trivial_lit lit =
    exists_ac ()
    &&
    (
      let module A = T.AC(struct let is_ac = is_ac let is_comm _ = false end) in
      match Lit.View.as_eqn lit with
      | Some (l, r, true) -> A.equal l r
      | Some _ -> false
      | None -> false
    )

  let is_trivial c =
    let res = CCArray.exists is_trivial_lit (C.lits c) in
    if res then Util.incr_stat stat_ac_redundant;
    res

  (* simplify: remove literals that are redundant modulo AC *)
  let simplify c =
    Util.enter_prof prof_simplify;
    if exists_ac ()
    then (
      let n = Array.length (C.lits c) in
      let module A = T.AC(struct let is_ac = is_ac let is_comm _ = false end) in
      let lits = Array.to_list (C.lits c) in
      let lits =
        List.filter
          (fun lit -> match Lit.View.as_eqn lit with
             | Some (l, r, false) -> not (A.equal l r)
             | Some _
             | None -> true)
          lits
      in
      let n' = List.length lits in
      if n' < n && not (C.get_flag SClause.flag_persistent c)
      then (
        (* did some simplification *)
        let symbols = symbols_of_terms (C.Seq.terms c) in
        let symbols = Sequence.to_list (ID.Set.to_seq symbols) in
        let ac_proof = CCList.flat_map find_proof symbols in
        let premises = C.proof c :: ac_proof in
        let proof =
          ProofStep.mk_simp premises
          ~rule:(ProofStep.mk_rule ~comment:["ac"] "normalize")
        in
        let new_c = C.create ~trail:(C.trail c) lits proof in
        Util.exit_prof prof_simplify;
        Util.incr_stat stat_ac_simplify;
        Util.debugf ~section 3 "@[<2>@[%a@]@ AC-simplify into @[%a@]@]"
          (fun k->k C.pp c C.pp new_c);
        SimplM.return_new new_c
      ) else (
        (* no simplification *)
        Util.exit_prof prof_simplify;
        SimplM.return_same c
      )
    ) else SimplM.return_same c

  let install_rules_ () =
    (* enable AC inferences if needed *)
    if exists_ac ()
    then (
      Env.add_is_trivial is_trivial;
      Env.add_simplify simplify;
    );
    ()

  let add ?proof s ty =
    Util.debugf ~section 1 "@[enable AC redundancy criterion@ for `@[%a : @[%a@]@]`@]"
      (fun k->k ID.pp s Type.pp ty);
    (* is this the first case of AC symbols? If yes, then add inference rules *)
    let first = not (exists_ac ()) in
    if first then install_rules_ ();
    (* remember that the symbols is AC *)
    add_ ?proof ~ty s;
    (* add clauses *)
    let clauses = axioms s ty in
    Env.add_passive (Sequence.of_list clauses);
    ()

  (* TODO: proof stuff *)
  let scan_statement st =
    let module St = Statement in
    let has_ac_attr =
      List.exists
        (function St.A_AC -> true)
        (Statement.attrs st)
    in
    if has_ac_attr then match St.view st with
      | St.TyDecl (id, ty)
      | St.Def (id, ty, _) ->
        add id ty
      | St.Data _
      | St.RewriteTerm (_,_,_,_)
      | St.RewriteForm (_,_)
      | St.Assert _
      | St.Goal _
      | St.NegatedGoal _ ->
        Util.error ~where:"AC"
          "attribute 'AC' only supported on def/decl statements"

  (* add axioms of newly added constants to the passive set *)
  let add_new_axioms_ () =
    let l = !new_ids_ in
    new_ids_ := [];
    List.fold_left
      (fun acc (id,ty) ->
         let ax = axioms id ty in
         Util.debugf ~section 3
           "@[<2>add AC axioms for `%a : @[%a@]`:@ @[<hv>%a@]@]"
           (fun k->k ID.pp id Type.pp ty (Util.pp_list C.pp) ax);
         List.rev_append ax acc)
      [] l

  (* just look for AC axioms *)
  let setup () =
    Env.add_generate "AC.axioms" add_new_axioms_;
    Signal.on_every
      Env.on_input_statement scan_statement
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module AC = Make(E) in
    AC.setup ()
  in
  { Extensions.default with Extensions.
    name="ac";
    env_actions=[action];
  }
