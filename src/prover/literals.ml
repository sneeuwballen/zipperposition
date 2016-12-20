
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Array of literals} *)

open Libzipperposition

module Hash = CCHash
module BV = CCBV
module T = FOTerm
module S = Substs
module Lit = Literal

type term = FOTerm.t

type t = Literal.t array

let prof_maxlits = Util.mk_profiler "lits.maxlits"

let equal lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then true else
    Lit.equal lits1.(i) lits2.(i) && check (i+1)
  in
  if Array.length lits1 <> Array.length lits2
    then false
    else check 0

let equal_com lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then true else
    Lit.equal_com lits1.(i) lits2.(i) && check (i+1)
  in
  if Array.length lits1 <> Array.length lits2
    then false
    else check 0

let compare lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then 0 else
      let cmp = compare lits1.(i) lits2.(i) in
      if cmp = 0 then check (i+1) else cmp
  in
  if Array.length lits1 <> Array.length lits2
    then Array.length lits1 - Array.length lits2
    else check 0

let hash_fun lits h =
  Hash.array_ Lit.hash_fun lits h

let hash lits = Hash.apply hash_fun lits

let variant ?(subst=S.empty) (a1,sc1) (a2,sc2) k =
  (* match a1.(i...) with a2\bv *)
  let rec iter2 subst bv i =
    if i = Array.length a1
      then k subst
      else iter3 subst bv i 0
  (* find a matching literal for a1.(i), within a2.(j...) *)
  and iter3 subst bv i j =
    if j = Array.length a2
      then ()  (* stop *)
    else (
      if not (BV.get bv j)
        then (
          (* try to match i-th literal of a1 with j-th literal of a2 *)
          BV.set bv j;
          Lit.variant ~subst (a1.(i),sc1) (a2.(i),sc2)
            (fun subst -> iter2 subst bv (i+1));
          BV.reset bv j
        );
      iter3 subst bv i (j+1)
    )
  in
  if Array.length a1 = Array.length a2
    then
      let bv = BV.create ~size:(Array.length a1) false in
      iter2 subst bv 0

let are_variant a1 a2 =
  not (Sequence.is_empty (variant (Scoped.make a1 0) (Scoped.make a2 1)))

let weight lits =
  Array.fold_left (fun w lit -> w + Lit.weight lit) 0 lits

let depth lits =
  Array.fold_left (fun d lit -> max d (Lit.depth lit)) 0 lits

let vars lits =
  Sequence.of_array lits
  |> Sequence.flat_map Lit.Seq.vars
  |> T.VarSet.of_seq
  |> T.VarSet.to_list

let is_ground lits =
  CCArray.for_all Lit.is_ground lits

let to_form lits =
  let lits = Array.map Lit.Conv.to_form lits in
  Array.to_list lits

(** Apply the substitution to the array of literals, with scope *)
let apply_subst ~renaming subst (lits,sc) =
  Array.map
    (fun lit -> Lit.apply_subst ~renaming subst (lit,sc))
    lits

let map f lits =
  Array.map (fun lit -> Lit.map f lit) lits

(** bitvector of literals that are positive *)
let pos lits =
  let bv = BV.create ~size:(Array.length lits) false in
  for i = 0 to Array.length lits - 1 do
    if Lit.is_pos lits.(i) then BV.set bv i
  done;
  bv

(** bitvector of literals that are positive *)
let neg lits =
  let bv = BV.create ~size:(Array.length lits) false in
  for i = 0 to Array.length lits - 1 do
    if Lit.is_neg lits.(i) then BV.set bv i
  done;
  bv

(** Multiset of literals, with their index *)
module MLI = Multiset.Make(struct
  type t = Lit.t * int
  let compare (l1,i1)(l2,i2) =
    if i1=i2 then Lit.compare l1 l2 else Pervasives.compare i1 i2
end)

let _compare_lit_with_idx ~ord (lit1,i1) (lit2,i2) =
  if i1=i2
    then Comparison.Eq (* ignore collisions *)
    else Lit.Comp.compare ~ord lit1 lit2

let _to_multiset_with_idx lits =
  CCArray.foldi
    (fun acc i x -> MLI.add acc (x,i))
    MLI.empty lits

(* TODO: optimize! quite a bottleneck on pb47.p with NoSelection *)
let maxlits_l ~ord lits =
  Util.enter_prof prof_maxlits;
  let m = _to_multiset_with_idx lits in
  let max = MLI.max_seq (_compare_lit_with_idx ~ord) m
    |> Sequence.map2 (fun x _ -> x)
    |> Sequence.to_list
  in
  Util.exit_prof prof_maxlits;
  max

let maxlits ~ord lits =
  Util.enter_prof prof_maxlits;
  let m = _to_multiset_with_idx lits in
  let max = MLI.max_seq (_compare_lit_with_idx ~ord) m
    |> Sequence.map2 (fun x _ -> snd x)
    |> Sequence.to_list
    |> BV.of_list
  in
  Util.exit_prof prof_maxlits;
  max

let is_max ~ord lits =
  (*
  let max = maxlits_l ~ord lits in
  fun i -> List.exists (fun (_,j) -> i=j) max
  *)
  let m = _to_multiset_with_idx lits in
  fun i ->
    let lit = lits.(i) in
    MLI.is_max (_compare_lit_with_idx ~ord) (lit,i) m

let is_trivial lits =
  CCArray.exists Lit.is_trivial lits

let is_absurd lits =
  CCArray.for_all Lit.is_absurd lits

module Seq = struct
  let vars lits =
    Sequence.of_array lits |> Sequence.flat_map Lit.Seq.vars
  let terms a =
    Sequence.of_array a |> Sequence.flat_map Lit.Seq.terms
  let to_form a = Sequence.of_array a |> Sequence.map Lit.Conv.to_form
end

(** {3 High Order combinators} *)

module Pos = struct
  let _fail_lits lits pos =
    let msg =
      CCFormat.sprintf "@[invalid position @[%a@]@ in lits [@[%a@]]@]"
        Position.pp pos (CCFormat.array Lit.pp) lits
    in invalid_arg msg

  let _fail_pos pos =
    let msg =
      CCFormat.sprintf
      "@[<2>invalid literal-array position@ @[%a@]@]" Position.pp pos in
    invalid_arg msg

  let at lits pos = match pos with
    | Position.Arg (idx, pos') when idx >= 0 && idx < Array.length lits ->
      Lit.Pos.at lits.(idx) pos'
    | _ -> _fail_lits lits pos

  let lit_at lits pos = match pos with
    | Position.Arg (i, pos') when i >= 0 && i < Array.length lits ->
      lits.(i), pos'
    | _ -> _fail_lits lits pos

  let replace lits ~at ~by = match at with
    | Position.Arg (idx, pos') when idx >= 0 && idx < Array.length lits ->
      lits.(idx) <- Lit.Pos.replace lits.(idx) ~at:pos' ~by
    | _ -> _fail_lits lits at

  let idx = function
    | Position.Arg(i, _) -> i
    | p -> _fail_pos p

  let tail = function
    | Position.Arg (_, pos') -> pos'
    | p -> _fail_pos p

  let cut = function
    | Position.Arg (i, pos') -> i, pos'
    | p -> _fail_pos p
end

module Conv = struct
  let of_forms ?hooks forms =
    let forms = Array.of_list forms in
    Array.map (Lit.Conv.of_form ?hooks) forms

  let to_forms ?hooks lits =
    Array.to_list (Array.map (Lit.Conv.to_form ?hooks) lits)
end

module View = struct
  let get_eqn lits pos = match pos with
    | Position.Arg (idx, pos') when idx < Array.length lits ->
      Lit.View.get_eqn lits.(idx) pos'
    | _ -> None

  let get_arith lits pos = match pos with
    | Position.Arg (idx, pos') when idx < Array.length lits ->
      Lit.View.focus_arith lits.(idx) pos'
    | _ -> None

  let _unwrap2 ~msg f x y = match f x y with
    | Some z -> z
    | None -> invalid_arg msg

  let get_eqn_exn =
    _unwrap2 ~msg:"get_eqn: improper position" get_eqn

  let get_arith_exn =
    _unwrap2 ~msg:"get_arith: improper position" get_arith
end

let fold_lits ~eligible lits k =
  let rec aux i =
    if i = Array.length lits then ()
    else if not (eligible i lits.(i)) then aux (i+1)
    else (
      k (lits.(i), i);
      aux (i+1)
    )
  in
  aux 0

let fold_eqn ?(both=true) ?sign ~ord ~eligible lits k =
  let sign_ok s = match sign with
    | None -> true
    | Some sign -> sign = s
  in
  let rec aux i =
    if i = Array.length lits then ()
    else if not (eligible i lits.(i)) then aux (i+1)
    else (
      begin match lits.(i) with
      | Lit.Equation (l,r,sign) when sign_ok sign ->
        begin match Ordering.compare ord l r with
        | Comparison.Gt ->
            k (l, r, sign, Position.(arg i @@ left @@ stop))
        | Comparison.Lt ->
            k (r, l, sign, Position.(arg i @@ right @@ stop))
        | Comparison.Eq
        | Comparison.Incomparable ->
          if both
          then (
            (* visit both sides of the equation *)
            k (r, l, sign, Position.(arg i @@ right @@ stop));
            k (l, r, sign, Position.(arg i @@ left @@ stop))
          ) else
            (* only one side *)
            k (l, r, sign, Position.(arg i @@ left @@ stop))
        end
      | Lit.Prop (p, sign) when sign_ok sign ->
          k (p, T.true_, sign, Position.(arg i @@ left @@ stop))
      | Lit.Prop _
      | Lit.Equation _
      | Lit.Arith _
      | Lit.True
      | Lit.False -> ()
      end;
      aux (i+1)
    )
  in
  aux 0

let fold_arith ~eligible lits k =
  let rec aux i =
    if i = Array.length lits then ()
    else if not (eligible i lits.(i)) then aux (i+1)
    else (
      begin match Lit.View.get_arith lits.(i) with
      | None -> ()
      | Some x ->
          let pos = Position.(arg i stop) in
          k (x, pos)
      end;
      aux (i+1)
    )
  in aux 0

let fold_arith_terms ~eligible ~which ~ord lits k =
  let module M = Monome in let module MF = Monome.Focus in
  fold_arith ~eligible lits
    (fun (a_lit, pos) ->
      (* do we use the given term? *)
      let do_term =
        match which with
        | `All -> (fun _ -> true)
        | `Max ->
          let max_terms = ArithLit.max_terms ~ord a_lit in
          fun t -> CCList.Set.mem ~eq:T.equal t max_terms
      in
      ArithLit.Focus.fold_terms ~pos a_lit
        (fun (foc_lit, pos) ->
          let t = ArithLit.Focus.term foc_lit in
          if do_term t then k (t, foc_lit, pos))
    )

let fold_terms ?(vars=false) ?ty_args ~(which : [< `All|`Max])
~ord ~subterms ~eligible lits k =
  let rec aux i =
    if i = Array.length lits then ()
    else if not (eligible i lits.(i))
      then aux (i+1) (* ignore lit *)
    else (
      Lit.fold_terms
        ~position:Position.(arg i stop) ?ty_args ~vars ~which ~ord ~subterms
        lits.(i) k;
      aux (i+1)
    )
  in
  aux 0

let symbols ?(init=ID.Set.empty) lits =
  Sequence.of_array lits
    |> Sequence.flat_map Lit.Seq.symbols
    |> ID.Set.add_seq init

(** {3 IO} *)

let pp out lits =
  if Array.length lits = 0 then CCFormat.string out "⊥"
  else
    let pp_lit = CCFormat.hovbox Lit.pp in
    Format.fprintf out "[@[<hv>%a@]]"
      (CCFormat.array ~start:"" ~stop:"" ~sep:" ∨ " pp_lit) lits

let pp_vars out lits =
  let pp_vars out = function
    | [] -> ()
    | l ->
      Format.fprintf out "forall @[%a@].@ "
        (Util.pp_list ~sep:" " Type.pp_typed_var) l
  in
  let vars_ =
    Seq.vars lits |> T.VarSet.of_seq |> T.VarSet.to_list
  in
  Format.fprintf out "@[<2>%a%a@]" pp_vars vars_ pp lits

let pp_tstp out lits =
  if Array.length lits = 0 then CCFormat.string out "$false"
  else
    Format.fprintf out "@[%a@]"
      (CCFormat.array ~start:"" ~stop:"" ~sep:" | " Lit.pp_tstp) lits

let to_string a = CCFormat.to_string pp a

(** {2 Special kinds of array} *)

(** Recognized whether the clause is a Range-Restricted Horn clause *)
let is_RR_horn_clause lits =
  let bv = pos lits in
  match BV.to_list bv with
  | [i] ->
    (* single positive lit, check variables restrictions, ie all vars
        occur in the head *)
    let hd_vars = Lit.vars lits.(i) in
    List.length hd_vars = List.length (vars lits)
  | _ -> false

(** Recognizes Horn clauses (at most one positive literal) *)
let is_horn lits =
  let bv = pos lits in
  BV.cardinal bv <= 1

let is_pos_eq lits =
  match lits with
  | [| Lit.Equation (l,r,true) |] -> Some (l,r)
  | [| Lit.Prop(p,true) |] -> Some (p, T.true_)
  | [| Lit.True |] -> Some (T.true_, T.true_)
  | _ -> None
