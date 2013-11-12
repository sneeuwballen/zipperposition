

module FO = struct
  module T = FOTerm
  module BT = Basic.FO

  let erase ?(depth=0) t =
    let rec erase t = match t.T.term with
    | T.Var i -> BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "X%d" i)
    | T.BoundVar i -> BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "Y%d" (depth-i-1))
    | T.Node (s, l) -> BT.app s (List.map erase l)
    in
    erase t
end

module Form = struct
  module F = FOFormula
  module BT = Basic.FO
  module BF = Basic.Form

  let erase ?(depth=0) f =
    let rec erase depth f = match f.F.form with
      | F.True -> BF.mk_true
      | F.False -> BF.mk_false
      | F.Atom p -> BF.atom (FO.erase ~depth p)
      | F.Equal (t1, t2) -> BF.mk_eq (FO.erase ~depth t1) (FO.erase ~depth t2)
      | F.And l -> BF.mk_and (List.map (erase depth) l)
      | F.Or l -> BF.mk_or (List.map (erase depth) l)
      | F.Not f' -> BF.mk_not (erase depth f')
      | F.Imply (f1, f2) -> BF.mk_imply (erase depth f1) (erase depth f2)
      | F.Equiv (f1, f2) -> BF.mk_equiv (erase depth f1) (erase depth f2)
      | F.Forall (ty, f') ->
        let v = BT.var ~ty:(TypeConversion.to_basic ty) (Util.sprintf "Y%d" depth) in
        BF.forall [v] (erase (depth+1) f')
      | F.Exists (ty, f') ->
        let v = BT.var ~ty:(TypeConversion.to_basic ty) (Util.sprintf "Y%d" depth) in
        BF.exists [v] (erase (depth+1) f')
    in
    erase depth f
end

module HO = struct
  module T = HOTerm
  module BT = Basic.HO

  let erase ?(depth=0) t =
    let rec erase depth t = match t.T.term with
    | T.Const s -> BT.const s
    | T.At (t, l) -> BT.app (erase depth t) (List.map (erase depth) l)
    | T.Var i ->
      BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "X%d" i)
    | T.BoundVar i ->
      BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "Y%d" (depth-i-1))
    | T.Lambda t' ->
      let var = BT.var ~ty:(TypeConversion.to_basic t.T.ty) (Util.sprintf "Y%d" depth) in
      BT.lambda ~var (erase (depth+1) t')
    in
    erase depth t
end
