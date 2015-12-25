
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {6 Generic multisets} *)

module type S = Multiset_intf.S

module Make(E : Map.OrderedType) = struct
  type elt = E.t

  type t = (elt * Z.t) list (* map element -> multiplicity *)

  let empty = []

  let size = List.length

  let cardinal m =
    List.fold_left (fun acc (_,n) -> Z.add n acc) Z.zero m

  let is_empty = function
    | [] -> true
    | _ -> false

  let rec mem m x = match m with
    | [] -> false
    | (y,_)::m' ->
        let c = E.compare x y in
        c = 0 || (c < 0 && mem m' x)

  let rec find m x = match m with
    | [] -> Z.zero
    | (y,n) :: m' ->
        let c = E.compare x y in
        if c = 0 then n
        else if c < 0 then find m' x
        else Z.zero

  let singleton x = [x, Z.one]

  let add_coeff m x n =
    let rec aux m x n = match m with
      | [] -> [x, n]
      | (y,n') :: m' ->
          let c = E.compare x y in
          if c=0 then (x,Z.add n n') :: m'
          else if c < 0 then (x,n)::m
          else (y,n') :: aux m' x n
    in
    assert (Z.geq n Z.zero);
    aux m x n

  let add m x = add_coeff m x Z.one

  let doubleton x y = add (singleton x) y

  let _cons x n m =
    if Z.gt n Z.zero
    then (x,n) :: m
    else m

  let rec _map f m = match m with
    | [] -> []
    | (x,n)::m' ->
        let n' = f x n in
        _cons x n' (_map f m')

  (* merge two lists together *)
  let rec _merge f m1 m2 = match m1, m2 with
    | [], _ -> m2
    | _, [] -> m1
    | (x1,n1)::m1', (x2,n2)::m2' ->
        let c = E.compare x1 x2 in
        if c < 0
        then _cons x1 (f n1 Z.zero) (_merge f m1' m2)
        else if c > 0
        then _cons x2 (f Z.zero n2) (_merge f m1 m2')
        else
          _cons x1 (f n1 n2) (_merge f m1' m2')

  let union = _merge Z.max

  let intersection = _merge Z.min

  let sum = _merge Z.add

  let difference = _merge Z.sub

  let product n m =
    if Z.sign n <= 0 then empty
    else _map (fun _ n' -> Z.mul n n') m

  let map f m =
    List.fold_left
      (fun acc (x,n) -> add_coeff acc (f x) n)
      empty m

  let map_coeff f m = _map f m

  let filter_map f m =
    List.fold_left
      (fun acc (x, n) ->
         match f x n with
         | None -> acc
         | Some (x',n') -> add_coeff acc x' n'
      ) empty m

  let filter p m = map_coeff (fun x n -> if p x n then n else Z.zero) m

  let flat_map f m =
    List.fold_left
      (fun m' (x,_) ->
         let m'' = f x in
         union m' m''
      ) empty m

  module Seq = struct
    let of_seq m seq =
      let m = ref m in
      seq (fun x -> m := add !m x);
      !m

    let to_seq m k =
      List.iter (fun (x,n) -> for _i=1 to Z.to_int n do k x; done) m

    let of_coeffs m seq =
      let m = ref m in
      seq (fun (x,n) -> m := add_coeff !m x n);
      !m

    let to_coeffs m k = List.iter k m
  end

  let iter f m = Seq.to_seq m f
  let iter_coeffs f m = Seq.to_coeffs m (fun (x,n) -> f x n)

  let fold f acc m =
    let acc = ref acc in
    Seq.to_seq m (fun x -> acc := f !acc x);
    !acc

  let fold_coeffs f acc m =
    List.fold_left (fun acc (x,n) -> f acc x n) acc m

  let for_all p m = List.for_all (fun (x,_) -> p x) m
  let exists p m = List.exists (fun (x,_) -> p x) m

  let choose m = match m with
    | [] -> raise Not_found
    | (x,_)::_ -> x

  let of_list = List.fold_left add empty

  let of_coeffs =
    List.fold_left
      (fun acc (x,n) -> add_coeff acc x n)
      empty

  let of_iarray = IArray.fold add empty

  let of_array = Array.fold_left add empty

  let to_list m = m

  let rec eq m1 m2 = match m1, m2 with
    | [], [] -> true
    | [], _
    | _, [] -> false
    | (x1,n1)::m1', (x2,n2)::m2' ->
        E.compare x1 x2 = 0 && Z.equal n1 n2 && eq m1' m2'

  let rec cancel m1 m2 = match m1, m2 with
    | [], _
    | _, [] -> m1, m2
    | (x1,n1)::m1', (x2,n2)::m2' ->
        let c = E.compare x1 x2 in
        if c = 0
        then match Z.compare n1 n2 with
          | 0 -> cancel m1' m2'  (* remove from both sides *)
          | n when n<0 ->
              let m1'', m2'' = cancel m1' m2' in
              m1'', (x2, Z.sub n2 n1) :: m2''  (* keep some at right *)
          | _ ->
              let m1'', m2'' = cancel m1' m2' in
              (x1, Z.sub n1 n2) :: m1'', m2''  (* keep some at left *)
        else if c < 0
        then
          let m1'', m2'' = cancel m1' m2 in
          (x1,n1)::m1'', m2''
        else
          let m1'', m2'' = cancel m1 m2' in
          m1'', (x2,n2)::m2''

  let compare_partial f m1 m2 =
    let m1, m2 = cancel m1 m2 in
    (* for now we can break the sorted list invariant. We look for some
       element of [m1] or [m2] that isn't dominated by any element of
       the other list. [m_i @ rest_i] is the set of potentially maximal
       elements of the multset. *)
    let rec check_left ~max1 m1 ~max2 m2 = match m1 with
      | [] ->
          (* max2 is true if some terms are not dominated within m2 *)
          let max2 = max2 || (m2<>[]) in
          begin match max1, max2 with
            | true, true -> Comparison.Incomparable
            | true, false -> Comparison.Gt
            | false, true -> Comparison.Lt
            | false, false -> Comparison.Eq
          end
      | (x1,n1)::m1' ->
          (* remove terms of [m2] that are dominated by [x1] *)
          filter_with ~max1 x1 n1 m1' ~max2 m2 []
    and filter_with ~max1 x1 n1 m1' ~max2 m2 rest2 = match m2 with
      | [] ->
          (* [x1] is not dominated *)
          check_left ~max1:true m1' ~max2 rest2
      | (x2,n2)::m2' ->
          begin match f x1 x2 with
            | Comparison.Eq ->
                let c = Z.compare n1 n2 in
                if c < 0
                then (* remove x1 *)
                  check_left ~max1 m1' ~max2 (List.rev_append m2 rest2)
                else if c > 0
                then (* remove x2 *)
                  filter_with ~max1 x1 Z.(n1-n2) m1' ~max2 m2' rest2
                else (* remove both *)
                  check_left ~max1 m1' ~max2 (List.rev_append m2' rest2)
            | Comparison.Incomparable ->
                (* keep both *)
                filter_with ~max1 x1 n1 m1' ~max2 m2' ((x2,n2)::rest2)
            | Comparison.Gt ->
                (* remove x2 *)
                filter_with ~max1 x1 n1 m1' ~max2 m2' rest2
            | Comparison.Lt ->
                (* remove x1 *)
                check_left ~max1 m1' ~max2 (List.rev_append m2 rest2)
          end
    in
    check_left ~max1:false m1 ~max2:false m2

  let compare m1 m2 =
    let rec aux m1 m2 = match m1, m2 with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | (x1,n1)::m1', (x2,n2)::m2' ->
          let c = E.compare x1 x2 in
          if c = 0
          then
            let c' = aux m1' m2' in
            if c' <> 0 then c'
            else Z.compare n1 n2
          else if c < 0
          then
            let c' = aux m1' m2 in
            if c' <> 0 then c'
            else 1
          else (* c > 0 *)
            let c' = aux m1 m2' in
            if c' <> 0 then c'
            else -1
    in
    aux m1 m2

  let is_max f x m =
    List.for_all
      (fun (y,_) -> match f x y with
         | Comparison.Lt -> false
         | _ -> true)
      m

  (* iterate on the max elements *)
  let max_seq f m k =
    (* acc: set of max terms so far. Find other max terms
       within [m] (none of which is comparable with elements of [acc]) *)
    let rec filter m k = match m with
      | [] -> ()
      | (x,n)::m' -> check_max x n m' [] k
    and check_max x n m rest k = match m with
      | [] ->
          (* success, [x] is max *)
          k x n;
          filter rest k
      | (y,n') :: m' ->
          begin match f x y with
            | Comparison.Lt ->
                (* failure, drop [x] since it's not maximal *)
                filter (List.rev_append rest m) k
            | Comparison.Gt ->
                (* drop [y] *)
                check_max x n m' rest k
            | Comparison.Eq ->
                (* merge x and y together *)
                check_max x Z.(add n n') m' rest k
            | Comparison.Incomparable ->
                (* keep both [x] and [y] *)
                check_max x n m' ((y,n')::rest) k
          end
    in
    filter m k

  let max f m =
    max_seq f m |> Sequence.fold2 add_coeff empty

  let max_l f l =
    max_seq f (of_list l)
    |> Sequence.fold2 (fun acc x _n -> x::acc) []

  let compare_partial_l f l1 l2 =
    compare_partial f (of_list l1) (of_list l2)

  let pp pp_x out l =
    Format.fprintf out "{@[<hov>%a@]}"
      (Util.pp_list ~sep:", "
         (fun out (x,n) -> Format.fprintf out "%a: %s" pp_x x (Z.to_string n)))
      l
end
