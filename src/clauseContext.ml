
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Clause context}

    A clause with a "hole" in it. Filling the whole with a term [t] is called
    "applying the context to [t]".

    The point is to relate different applications of the same context. *)

module T = Logtk.FOTerm
module Subst = Logtk.Substs
module Lits = Literals

type term = T.t
type subst = Logtk.Substs.t

(** A context is represented as a regular array of literals, containing
    at least one specific variable [x], paired with this variable [x].
    Applying the context is a mere substitution *)
type t = {
  lits : Literals.t;
  var : T.t;
}
type ctx=t

let equal c1 c2 =
  T.eq c1.var c2.var && Lits.eq c1.lits c2.lits

let raw_lits t = t.lits

(* TODO: compare types of extruded variables;
   if same type, instantiate with some specific "diamond" of that type
   and check for alpha-equiv *)
let compare c1 c2 =
  CCOrd.(T.cmp c1.var c2.var <?> (Lits.compare, c1.lits, c2.lits))

let make lits ~var =
  assert (Lits.Seq.terms lits
          |> Sequence.exists (T.var_occurs ~var)
         );
  {lits;var}

let extract lits t =
  if Lits.Seq.terms lits |> Sequence.exists (T.subterm ~sub:t)
  then
    (* create fresh var to replace [t] *)
    let i = Lits.Seq.terms lits
            |> Sequence.flat_map T.Seq.vars
            |> T.Seq.max_var
    in
    let var = T.var ~ty:(T.ty t) (i+1) in
    (* replace [t] with [var] *)
    let lits = Array.map
        (Literal.map (fun root_t -> T.replace root_t ~old:t ~by:var))
        lits
    in
    Some {lits;var}
  else None

let extract_exn lits t = match extract lits t with
  | None -> invalid_arg "ClauseContext.extract_exn"
  | Some c -> c

let _apply_subst subst lits sc =
  let renaming = Subst.Renaming.create () in
  Array.map (fun lit -> Literal.apply_subst_no_simp ~renaming subst lit sc) lits

let apply {lits;var} t =
  let subst = Subst.FO.bind Subst.empty var 0 t 1 in
  _apply_subst subst lits 0

let apply_same_scope {lits;var} t =
  let subst = Subst.FO.bind Subst.empty var 0 t 0 in
  _apply_subst subst lits 0

let _diamond = Logtk.Symbol.of_string "◇"

let pp buf c =
  let cst = T.const ~ty:(T.ty c.var) _diamond in
  let lits = apply_same_scope c cst in
  Lits.pp buf lits

let print fmt c =
  let cst = T.const ~ty:(T.ty c.var) _diamond in
  let lits = apply_same_scope c cst in
  Lits.fmt fmt lits

module Set = Sequence.Set.Make(struct
    type t = ctx
    let compare = compare
  end)
