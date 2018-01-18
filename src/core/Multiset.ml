
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {6 Generic multisets} *)

module type S = Multiset_intf.S

module Make(E : Map.OrderedType) = struct
  type elt = E.t

  module M = CCMap.Make(E)

  type t = Z.t M.t (* map element -> multiplicity *)

  let empty = M.empty

  let size = M.cardinal

  let cardinal m = M.fold (fun _ n acc -> Z.(n+acc)) m Z.zero

  let is_empty = M.is_empty

  let mem m x = M.mem x m

  let find m x =
    try M.find x m
    with Not_found -> Z.zero

  let singleton x = M.singleton x Z.one

  let add_coeff m x n =
    let n = Z.add n (find m x) in
    if Z.equal Z.zero n
    then M.remove x m
    else M.add x n m

  let add m x = add_coeff m x Z.one

  let doubleton x y = add (singleton x) y

  let _map f m : t = M.mapi f m

  (* merge two multisets together *)
  let _merge f m1 m2 =
    M.merge_safe m1 m2
      ~f:(fun _ v -> match v with
        | `Left n
        | `Right n -> Some n
        | `Both (n1,n2) ->
          let n = f n1 n2 in
          if Z.equal n Z.zero then None else Some n)

  let union = _merge Z.max

  let intersection = _merge Z.min

  let sum = _merge Z.add

  let difference = _merge Z.sub

  let product n m =
    if Z.sign n <= 0 then empty
    else _map (fun _ n' -> Z.mul n n') m

  let map f (m:t) : t =
    M.fold
      (fun x n acc -> add_coeff acc (f x) n)
      m empty

  let map_coeff f m = _map f m

  let filter_map f (m:t) =
    M.fold
      (fun x n acc -> match f x n with
         | None -> acc
         | Some (x',n') -> add_coeff acc x' n')
      m empty

  let filter p m = map_coeff (fun x n -> if p x n then n else Z.zero) m

  let flat_map f (m:t): t =
    M.fold
      (fun x _ m' ->
         let m'' = f x in
         union m' m'')
      m empty

  module Seq = struct
    let of_seq m seq =
      let m = ref m in
      seq (fun x -> m := add !m x);
      !m

    let to_seq (m:t) k =
      M.iter (fun x n -> for _i=1 to Z.to_int n do k x; done) m

    let of_coeffs m seq =
      let m = ref m in
      seq (fun (x,n) -> m := add_coeff !m x n);
      !m

    let to_coeffs m k = M.iter (fun x n -> k (x,n)) m
  end

  let iter f m = Seq.to_seq m f
  let iter_coeffs f m = Seq.to_coeffs m (fun (x,n) -> f x n)

  let fold f acc m =
    let acc = ref acc in
    Seq.to_seq m (fun x -> acc := f !acc x);
    !acc

  let fold_coeffs f acc (m:t) = M.fold (fun x n acc -> f acc x n) m acc

  let for_all p m = M.for_all (fun x _ -> p x) m
  let exists p m = M.exists (fun x _ -> p x) m

  let choose m = M.choose m |> fst

  let of_list = List.fold_left add empty

  let of_coeffs =
    List.fold_left
      (fun acc (x,n) -> add_coeff acc x n)
      empty

  let of_iarray = IArray.fold add empty

  let of_array = Array.fold_left add empty

  (* list, sorted by increasing key *)
  let to_list m =
    M.fold (fun x n acc -> (x,n)::acc) m []
    |> List.rev

  let equal m1 m2 = M.equal Z.equal m1 m2

  let cancel m1 m2 =
    let diff m1 m2 =
      M.merge_safe m1 m2
        ~f:(fun _ v -> match v with
          | `Left n -> Some n
          | `Right _ -> None
          | `Both (n1,n2) ->
            let n = Z.(n1-n2) in
            if Z.sign n <= 0 then None else Some n)
    in
    diff m1 m2, diff m2 m1

  let rec cancel_l m1 m2 = match m1, m2 with
    | [], _
    | _, [] -> m1, m2
    | (x1,n1)::m1', (x2,n2)::m2' ->
      let c = E.compare x1 x2 in
      if c = 0
      then match Z.compare n1 n2 with
        | 0 -> cancel_l m1' m2'  (* remove from both sides *)
        | n when n<0 ->
          let m1'', m2'' = cancel_l m1' m2' in
          m1'', (x2, Z.sub n2 n1) :: m2''  (* keep some at right *)
        | _ ->
          let m1'', m2'' = cancel_l m1' m2' in
          (x1, Z.sub n1 n2) :: m1'', m2''  (* keep some at left *)
      else if c < 0
      then
        let m1'', m2'' = cancel_l m1' m2 in
        (x1,n1)::m1'', m2''
      else
        let m1'', m2'' = cancel_l m1 m2' in
        m1'', (x2,n2)::m2''

  let compare_partial f m1 m2 =
    let m1, m2 = cancel_l (to_list m1) (to_list m2) in
    (* for now we can break the sorted list invariant. We look for some
       element of [m1] or [m2] that isn't dominated by any element of
       the other list. [m_i @ rest_i] is the set of potentially maximal
       elements of the multset.
       @param max1 true if there is a maximal elt in [m1] (not < than anything in m2)
       @param neq true if the sets can't be equal *)
    let rec check_left ~neq ~max1  m1 ~max2 m2 = match m1 with
      | [] ->
        (* max2 is true if some terms are not dominated within m2 *)
        let max2 = max2 || m2<>[] in
        begin match max1, max2 with
          | true, true -> Comparison.Incomparable
          | true, false -> Comparison.Gt
          | false, true -> Comparison.Lt
          | false, false ->
            if neq (* can't be equal? *)
            then Comparison.Incomparable
            else Comparison.Eq
        end
      | (x1,n1)::m1' ->
        (* remove terms of [m2] that are dominated by [x1] *)
        assert (Z.sign n1 > 0);
        filter_with ~neq ~max1 x1 n1 m1' ~max2 m2 []
    and filter_with ~neq ~max1 x1 n1 m1' ~max2 m2 rest2 = match m2 with
      | _ when Z.sign n1 <= 0 ->
        check_left ~neq ~max1 m1' ~max2 m2 (* all [x1] exhausted *)
      | [] ->
        (* [x1] is not dominated *)
        check_left ~neq ~max1:true m1' ~max2 rest2
      | (x2,n2)::m2' ->
        begin match f x1 x2 with
          | Comparison.Eq ->
            let c = Z.compare n1 n2 in
            if c < 0
            then (* remove x1 *)
              check_left ~neq ~max1 m1' ~max2 ((x2, Z.(n2-n1)) :: List.rev_append m2' rest2)
            else if c > 0
            then (* remove x2 *)
              filter_with ~neq ~max1 x1 Z.(n1-n2) m1' ~max2 m2' rest2
            else (* remove both *)
              check_left ~neq ~max1 m1' ~max2 (List.rev_append m2' rest2)
          | Comparison.Incomparable ->
            (* keep both *)
            filter_with ~neq ~max1 x1 n1 m1' ~max2 m2' ((x2,n2)::rest2)
          | Comparison.Gt ->
            (* remove x2 *)
            filter_with ~neq:true ~max1 x1 n1 m1' ~max2 m2' rest2
          | Comparison.Lt ->
            (* remove x1 *)
            check_left ~neq:true ~max1 m1' ~max2 (List.rev_append m2 rest2)
        end
    in
    check_left ~neq:false ~max1:false m1 ~max2:false m2

  let compare_l m1 m2 =
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

  let compare m1 m2 = compare_l (to_list m1) (to_list m2)

  let is_max f x m =
    M.for_all
      (fun y _ -> match f x y with
         | Comparison.Lt -> false
         | _ -> true)
      m

  (* iterate on the max elements *)
  let max_seq f (m:t) k =
    let m = ref m in
    while not (M.is_empty !m) do
      let x, n = M.choose !m in
      m := M.remove x !m;
      try
        let n' =
          M.fold
            (fun y n' acc -> match f x y with
               | Comparison.Lt -> raise Exit
               | Comparison.Eq ->
                 (* merge [x] and [y] together *)
                 m := M.remove y !m;
                 Z.(acc+n')
               | Comparison.Gt ->
                 m := M.remove y !m;
                 acc (* remove [y] *)
               | Comparison.Incomparable -> acc)
            !m n
        in
        k (x, n')
      with Exit -> ()
    done

  let max f m =
    max_seq f m
    |> Sequence.fold (fun m (c,t) -> add_coeff m c t) empty

  let max_l f l =
    max_seq f (of_list l)
    |> Sequence.fold (fun acc (x,_) -> x::acc) []

  let compare_partial_l f l1 l2 =
    compare_partial f (of_list l1) (of_list l2)

  let pp pp_x out m =
    let pp_p out (x,n) = Format.fprintf out "%a: %s" pp_x x (Z.to_string n) in
    Format.fprintf out "{@[<hov>%a@]}" (Util.pp_seq ~sep:", " pp_p) (M.to_seq m)
end
