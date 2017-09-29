
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Polynomial} *)

module type IntegerModule = sig
  type t

  val zero : t
  val one : t

  val add : t -> t -> t
  val mult : int -> t -> t
  val compare : t -> t -> int

  val pp : t CCFormat.printer
end

module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val pp : t CCFormat.printer
end

module type S = sig
  type t
  type coeff
  type indet

  val const : coeff -> t
  val indet : indet -> t

  val add : t -> t -> t

  val mult_const : int -> t -> t
  val mult_indet : indet -> t -> t

  val compare : t -> t -> int

  val pp : t CCFormat.printer
end

module Make(Coeff : IntegerModule)(Indet : OrderedType) = struct
  type coeff = Coeff.t
  type indet = Indet.t

  module Monomial = CCMultiSet.Make(Indet)
  module P = CCMap.Make(Monomial)
  type t = Coeff.t P.t

  let const w =
    if w = Coeff.zero
    then P.empty
    else P.singleton Monomial.empty w

  let indet x = P.singleton (Monomial.singleton x) Coeff.one

  let add p q =
    P.merge_safe p q
      ~f:(fun _ w -> match w with
        | `Left x | `Right x -> Some x
        | `Both (w1,w2) ->
          assert (Coeff.add w1 w2 <> Coeff.zero);
          Some (Coeff.add w1 w2))

  let mult_const c p =
    if c=0
    then P.empty
    else P.map (fun w -> Coeff.mult c w) p

  let mult_indet x p : t =
    P.fold
      (fun m w new_p -> P.add (Monomial.add m x) w new_p)
      p P.empty

  let compare p1 p2 =
    (* Comparison result for each monomial *)
    let monom_compare : int P.t = P.merge
        (fun _ w1 w2 -> match w1, w2 with
           | Some _, None -> Some 1
           | None, Some _ -> Some (-1)
           | Some a1, Some a2 -> Some (Coeff.compare a1 a2)
           | None, None -> None)
        p1 p2
    in
    (* Iterate over the monomial results and return
       Some 1 or Some -1 if all monomials point in the same direction
       Some 0            if there are contradicting monomials
       None              if all monomials are equal    *)
    let result =
      P.fold (fun _ c res -> match res with
        (* Contradicting monomials already detected *)
        | Some 0 -> Some 0
        (* Current monomial does not contradict the preliminary result *)
        | Some r when (r < 0 && c <= 0) || (r > 0 && c >= 0) -> Some r
        (* Current monomial contradicts the preliminary result *)
        | Some r when (r < 0 && c > 0) || (r > 0 && c < 0) -> Some 0
        (* All monomials were equal so far *)
        | None when c = 0 -> None
        (* First differing monomial *)
        | None when c <> 0 -> Some c
        (* Exhaustive matching *)
        | _ -> assert false) monom_compare None
    in
    begin match result with
      | Some r -> r
      | None -> 0
    end

  let monomial_pp out (a:Monomial.t): unit =
    Format.fprintf out "(%a)" (Util.pp_list ~sep:"*" Indet.pp) (Monomial.to_list a)

  let pp out (a:t): unit =
    Format.fprintf out "Poly[%a]"
      (Util.pp_list ~sep:" + " (CCPair.pp ~sep:" * " monomial_pp Coeff.pp)) (P.bindings a)
end
