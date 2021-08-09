
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
    let of_iter m seq =
      let m = ref m in
      seq (fun x -> m := add !m x);
      !m

    let to_iter (m:t) k =
      M.iter (fun x n -> for _i=1 to Z.to_int n do k x; done) m

    let of_coeffs m seq =
      let m = ref m in
      seq (fun (x,n) -> m := add_coeff !m x n);
      !m

    let to_coeffs m k = M.iter (fun x n -> k (x,n)) m
  end

  let iter f m = Seq.to_iter m f
  let iter_coeffs f m = Seq.to_coeffs m (fun (x,n) -> f x n)

  let fold f acc m =
    let acc = ref acc in
    Seq.to_iter m (fun x -> acc := f !acc x);
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

  let list_of_coeffs =
    List.fold_left
      (fun acc (x, n) -> List.rev_append (CCList.repeat (Z.to_int n) [x]) acc)
      []

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

  let do_compare_partial_strict ~met_geq_or_leq f m1 m2 =
    let m1, m2 = cancel_l (to_list m1) (to_list m2) in
    (* for now we can break the sorted list invariant. We look for some
       element of [m1] or [m2] that isn't dominated by any element of
       the other list. [m_i @ rest_i] is the set of potentially maximal
       elements of the multset.
       @param max1 true if there is a maximal elt in [m1] (not < than anything in m2)
       @param neq true if the sets can't be equal *)
    let rec check_left ~met_gt ~met_lt ~maxs1 m1 m2 = match m1 with
      | [] ->
        let maxs2 = m2 in
        begin match maxs1, maxs2 with
          | [], [] ->
            if met_gt || met_lt (* can't be equal? *)
            then Comparison.Nonstrict.Incomparable
            else Eq
          | [], _ -> Lt
          | _, [] -> Gt
          | _, _ -> Incomparable
        end
      | (x1,n1)::m1' ->
        (* remove terms of [m2] that are dominated by [x1] *)
        assert (Z.sign n1 > 0);
        filter_with ~met_gt ~met_lt ~maxs1 x1 n1 m1' m2 []
    and filter_with ~met_gt ~met_lt ~maxs1 x1 n1 m1' m2 seen2 = match m2 with
      | _ when Z.sign n1 <= 0 ->
        check_left ~met_gt ~met_lt ~maxs1 m1' m2 (* all [x1] exhausted *)
      | [] ->
        (* [x1] is not dominated *)
        check_left ~met_gt ~met_lt ~maxs1:((x1,n1) :: maxs1) m1' seen2
      | (x2,n2)::m2' ->
        begin match f x1 x2 with
          | Comparison.Nonstrict.Eq ->
            let c = Z.compare n1 n2 in
            if c < 0
            then (* remove x1 *)
              check_left ~met_gt ~met_lt ~maxs1 m1'
                ((x2, Z.(n2-n1)) :: List.rev_append m2' seen2)
            else if c > 0
            then (* remove [x2] *)
              filter_with ~met_gt ~met_lt ~maxs1 x1 Z.(n1-n2) m1' m2' seen2
            else (* remove both *)
              check_left ~met_gt ~met_lt ~maxs1 m1'
                (List.rev_append m2' seen2)
          | Geq | Leq ->
            (* keep both *)
            met_geq_or_leq := true;
            filter_with ~met_gt ~met_lt ~maxs1 x1 n1 m1' m2' ((x2,n2)::seen2)
          | Incomparable ->
            (* keep both *)
            filter_with ~met_gt ~met_lt ~maxs1 x1 n1 m1' m2' ((x2,n2)::seen2)
          | Gt ->
            (* remove [x2] *)
            filter_with ~met_gt:true ~met_lt ~maxs1 x1 n1 m1' m2' seen2
          | Lt ->
            (* remove x1 *)
            check_left ~met_gt ~met_lt:true ~maxs1 m1' (List.rev_append m2 seen2)
        end
    in
    check_left ~met_gt:false ~met_lt:false ~maxs1:[] m1 m2

  (* Given two multisets [m1] and [m2] of nonstrict comparable elements,
     look for a permultation so that [m1] and [m2] can be enumerated as
     [[y1, ..., yn]] and [[xn, ..., xm]] with [n >= m] and [y_i >= x_i] for
     every [i <= m]. Since the order is partial, in the worst case all
     permutations must be tried. To alleviate this, we immediately give up if
     [m] or [n] is larger than a fixed threshold. Many of these comparisons will
     be between literals, which typically contain only two elements. *)
  let find_geq_mates f m1 m2 =
    let len1 = List.length m1 and len2 = List.length m2 in
    if len1 < len2 then
      false
    else if len1 > 4 then
      (* avoid explosition *)
      false
    else
      let rec find_mates m1 m2 = match m2 with
          [] -> true
        | x2 :: m2' ->
          let rec find_mate_for_x2 seen1 m1 = match m1 with
            | [] -> false
            | (x1 :: m1') ->
              if f x1 x2 = Comparison.Nonstrict.Geq
                  && find_mates (List.rev_append seen1 m1') m2' then
                true
              else
                find_mate_for_x2 (x1 :: seen1) m1'
          in
          find_mate_for_x2 [] m1
      in
      find_mates m1 m2

  (* The code is based on that of [do_compare_partial_strict]. It is inpired by
     Definition 2.2 of Thiemann, Allais, and Nagele, "On the formalization of
     termination techniques based on multiset orderings". Due to the algorithmic
     complexity of the problem, this code is not designed to be complete.

     The multiset [m1] is partitioned into a set of "dominators" consisting of
     those elements that were found to strictly domiate a "dominated" element of
     [m2]. The remaining elements of [m1] and [m2] are put in [left1] and
     [left2], and we must look for an injection from [left2] to [left1] such that
     the element from [m2] is nonstrictly dominated. If this fails, we try to find
     an injection between [m2] and [m1] instead. *)
  let do_geq_partial_slow f m1 m2 =
    let m1, m2 = cancel_l (to_list m1) (to_list m2) in
    let rec check_left ~met_gt ~left1 see1 left2 = match see1 with
      | [] ->
        if find_geq_mates f (list_of_coeffs left1) (list_of_coeffs left2) then
          if met_gt then  (* any [m2] element strictly dominated? *)
            Comparison.Nonstrict.Gt
          else
            Geq
        else
          (* try a nonstrict comparison instead *)
          if met_gt
              && find_geq_mates f (list_of_coeffs m1) (list_of_coeffs m2) then
            Geq
          else
            Incomparable
      | (x1,n1)::see1' ->
        (* remove terms of [left2] that are dominated by [x1] *)
        assert (Z.sign n1 > 0);
        filter_with ~met_gt ~met_x1_gt:false ~left1 x1 n1 see1' left2 []
    and filter_with ~met_gt ~met_x1_gt ~left1 x1 n1 m1' m2 seen2 = match m2 with
      | _ when Z.sign n1 <= 0 ->
        check_left ~met_gt ~left1 m1' m2 (* all [x1] exhausted *)
      | [] ->
        let left1' = if met_x1_gt then left1 else (x1,n1) :: left1 in
        check_left ~met_gt ~left1:left1' m1' seen2
      | (x2,n2)::m2' ->
        begin match f x1 x2 with
          | Comparison.Nonstrict.Eq ->
            let c = Z.compare n1 n2 in
            if c < 0
            then (* remove x1 *)
              check_left ~met_gt ~left1 m1'
                ((x2, Z.(n2-n1)) :: List.rev_append m2' seen2)
            else if c > 0
            then (* remove [x2] *)
              filter_with ~met_gt ~met_x1_gt ~left1 x1 Z.(n1-n2) m1' m2' seen2
            else (* remove both *)
              check_left ~met_gt ~left1 m1' (List.rev_append m2' seen2)
          | Lt | Geq | Leq | Incomparable ->
            (* keep both *)
            filter_with ~met_gt ~met_x1_gt ~left1 x1 n1 m1' m2' ((x2,n2)::seen2)
          | Gt ->
            (* remove [x2] *)
            filter_with ~met_gt:true ~met_x1_gt:true ~left1 x1 n1 m1' m2' seen2
        end
    in
    check_left ~met_gt:false ~left1:[] m1 m2

  let compare_partial_nonstrict f m1 m2 =
    let met_geq_or_leq = ref false in
    let cmp = do_compare_partial_strict ~met_geq_or_leq f m1 m2 in
    match cmp, !met_geq_or_leq with
    | Comparison.Nonstrict.Incomparable, true ->
      begin match do_geq_partial_slow f m1 m2 with
        | Comparison.Nonstrict.Incomparable ->
          Comparison.Nonstrict.opp (do_geq_partial_slow f m2 m1)
        | cmp -> cmp
      end
    | _, _ -> cmp

  let compare_partial f m1 m2 =
    Comparison.of_nonstrict (compare_partial_nonstrict (fun k1 k2 ->
      Comparison.to_nonstrict (f k1 k2)) m1 m2)

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
    M.for_all (fun y _ -> not (Comparison.is_Lt_or_Leq (f x y))) m

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
               | Comparison.Nonstrict.Lt | Leq -> raise Exit
               | Eq ->
                 (* merge [x] and [y] together *)
                 m := M.remove y !m;
                 Z.(acc+n')
               | Gt | Geq ->
                 m := M.remove y !m;
                 acc (* remove [y] *)
               | Incomparable -> acc)
            !m n
        in
        k (x, n')
      with Exit -> ()
    done

  let max f m =
    max_seq f m
    |> Iter.fold (fun m (c, t) -> add_coeff m c t) empty

  let max_l f l =
    max_seq f (of_list l)
    |> Iter.fold (fun acc (x, _) -> x :: acc) []

  let compare_partial_l f l1 l2 =
    compare_partial f (of_list l1) (of_list l2)

  let compare_partial_nonstrict_l f l1 l2 =
    compare_partial_nonstrict f (of_list l1) (of_list l2)

  let pp pp_x out m =
    let pp_p out (x,n) = Format.fprintf out "%a: %s" pp_x x (Z.to_string n) in
    Format.fprintf out "{@[<hov>%a@]}" (Util.pp_iter ~sep:", " pp_p) (M.to_iter m)
end
