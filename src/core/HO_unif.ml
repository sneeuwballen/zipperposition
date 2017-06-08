
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Higher-Order Unification} *)

module RW = Rewrite
module T = Term

let stat_unif_calls = Util.mk_stat "ho_unif.calls"
let stat_unif_steps = Util.mk_stat "ho_unif.steps"
let section = Util.Section.make "ho_unif"

type term = Term.t

type penalty = int
(** penalty on the search space *)

(* number of ty and non-ty arguments *)
let term_arity args =
  args
  |> Util.take_drop_while (fun t -> T.is_type t)
  |> CCPair.map List.length List.length

let enum_prop ((v:Term.var), sc_v) ~scope_new_vars : (Subst.t * penalty) list =
  let ty_v = HVar.ty v in
  let n, ty_args, ty_ret = Type.open_poly_fun ty_v in
  assert (Type.is_prop ty_ret);
  if n>0 then [] (* FIXME: what to do? *)
  else (
    (* local variables to build the λ-term *)
    let vars = List.mapi (fun i ty -> HVar.make ~ty i) ty_args in
    (* projection with "¬": [λvars. ¬ (F vars)] *)
    let l_not =
      let f = HVar.make 0 ~ty:ty_v in
      T.fun_of_fvars vars
        (T.Form.not_ (T.app (T.var f) (List.map T.var vars)))
    (* projection with "∧": [λvars. (F1 vars) ∧ (F2 vars)] *)
    and l_and =
      let f = HVar.make 0 ~ty:ty_v in
      let g = HVar.make 1 ~ty:ty_v in
      T.fun_of_fvars vars
        (T.Form.and_
           (T.app (T.var f) (List.map T.var vars))
           (T.app (T.var g) (List.map T.var vars)))
    (* projection with "=": [λvars. (F1 vars) = (F2 vars)]
       where [F1 : Πa. ty_args -> a] *)
    and l_eq =
      let a = HVar.make 0 ~ty:Type.tType in
      let ty_fun = Type.arrow ty_args (Type.var a) in
      let f = HVar.make 1 ~ty:ty_fun in
      let g = HVar.make 2 ~ty:ty_fun in
      T.fun_of_fvars vars
        (T.Form.eq
           (T.app (T.var f) (List.map T.var vars))
           (T.app (T.var g) (List.map T.var vars)))
    in
    List.map
      (fun (t,penalty) ->
         assert (InnerTerm.DB.closed (t: T.t :> InnerTerm.t));
         let subst =
           Subst.FO.of_list [((v:>InnerTerm.t HVar.t),sc_v),(t,scope_new_vars)]
         in
         subst, penalty)
      [ l_not, 2;
        l_and, 4;
        l_eq, 5;
      ]
  )
