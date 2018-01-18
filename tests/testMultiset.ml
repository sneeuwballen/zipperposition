
open Logtk
open OUnit
module Q = QCheck

module M = Multiset.Make(struct
  type t = int
  let compare i j=Pervasives.compare i j
end)

let f x y =
  if x = y then Comparison.Eq
  else if x < y then Comparison.Lt
  else Comparison.Gt

let test_max _ =
  let m = M.of_list [1;2;2;3;1] in
  assert_equal (M.of_list [3]) (M.max f m)

let test_compare _ =
  let m1 = M.of_list [1;1;2;3] in
  let m2 = M.of_list [1;2;2;3] in
  assert_equal ~printer:Comparison.to_string Comparison.Lt (M.compare_partial f m1 m2);
  assert_bool "ord" (M.compare m1 m2 < 0);
  ()

let test_cardinal_size _ =
  let m = M.of_coeffs [1, Z.(~$ 2); 3, Z.(~$ 40)] in
  assert_equal 2 (M.size m);
  assert_equal ~cmp:Z.equal ~printer:Z.to_string Z.(~$ 42) (M.cardinal m);
  ()

let _sign = function
  | 0 -> 0
  | n when n < 0 -> -1
  | _ -> 1

let gen1 =
  let pp1 = CCFormat.to_string (M.pp CCFormat.int) in
  let shrink_z z =
    try Z.to_int z |> Q.Shrink.int |> Q.Iter.map Z.of_int
    with _ -> Q.Iter.empty
  in
  let shrink2 = Q.Shrink.pair Q.Shrink.nil shrink_z in
  let shrink l =
    M.to_list l
    |> Q.Shrink.(list ~shrink:shrink2)
    |> Q.Iter.map M.of_coeffs
  in
  Q.(small_list small_int
     |> map M.of_list
     |> set_print pp1
     |> set_shrink shrink)

let compare_and_partial =
  (* "naive" comparison function (using the general ordering on multisets) *)
  let compare' m1 m2 =
    let f x y = Comparison.of_total (CCInt.compare x y) in
    Comparison.to_total (M.compare_partial f m1 m2)
  in
  let prop (m1,m2) =
    _sign (compare' m1 m2) = _sign (M.compare m1 m2)
  in
  QCheck.Test.make
    ~name:"multiset_compare_and_compare_partial" ~long_factor:3 ~count:1000
    (Q.pair gen1 gen1) prop

(* partial order for tests *)
let partial_ord (x:int) y =
  if x=y then Comparison.Eq
  else if (x/5=y/5 && x mod 5 <> y mod 5) then Comparison.Incomparable
  else CCInt.compare (x/5) (y/5) |> Comparison.of_total

let compare_partial_sym =
  let prop (m1,m2) =
    let c1 = M.compare_partial partial_ord m1 m2 in
    let c2 =  Comparison.opp (M.compare_partial partial_ord m2 m1) in
    if c1=c2 
    then true
    else Q.Test.fail_reportf "comparison: %a vs %a" Comparison.pp c1 Comparison.pp c2
  in
  QCheck.Test.make
    ~name:"multiset_compare_partial_sym" ~long_factor:3 ~count:13_000
    Q.(pair gen1 gen1) prop

let compare_partial_trans =
  let prop (m1,m2,m3) =
    let c1 = M.compare_partial partial_ord m1 m2 in
    let c2 = M.compare_partial partial_ord m2 m3 in
    let c3 = M.compare_partial partial_ord m1 m3 in
    begin match c1,c2, c3 with
      | Comparison.Incomparable, _, _
      | _, Comparison.Incomparable, _
      | _, _, Comparison.Incomparable
      | Comparison.Lt, Comparison.Gt, _
      | Comparison.Gt, Comparison.Lt, _ -> Q.assume_fail()  (* ignore *)
      | Comparison.Eq, Comparison.Eq, Comparison.Eq -> true
      | Comparison.Gt, Comparison.Gt, Comparison.Gt
      | Comparison.Gt, Comparison.Eq, Comparison.Gt
      | Comparison.Eq, Comparison.Gt, Comparison.Gt -> true
      | Comparison.Lt, Comparison.Lt, Comparison.Lt
      | Comparison.Lt, Comparison.Eq, Comparison.Lt
      | Comparison.Eq, Comparison.Lt, Comparison.Lt -> true
      | Comparison.Lt, _, Comparison.Eq
      | Comparison.Gt, _, Comparison.Eq
      | _, Comparison.Lt, Comparison.Eq
      | _, Comparison.Gt, Comparison.Eq
      | (Comparison.Eq | Comparison.Lt), Comparison.Lt, Comparison.Gt
      | (Comparison.Eq | Comparison.Gt), Comparison.Gt, Comparison.Lt
      | Comparison.Lt, Comparison.Eq, Comparison.Gt
      | Comparison.Gt, Comparison.Eq, Comparison.Lt
      | Comparison.Eq, Comparison.Eq, (Comparison.Lt | Comparison.Gt)
        ->
        Q.Test.fail_reportf
          "comp %a %a %a" Comparison.pp c1 Comparison.pp c2 Comparison.pp c3
    end
  in
  QCheck.Test.make
    ~name:"multiset_compare_partial_trans" ~long_factor:3 ~count:13_000
    (Q.triple gen1 gen1 gen1) prop

let max_seq_correct =
  let prop m =
    let l1 = M.max_seq partial_ord m |> Sequence.map fst |> Sequence.to_list in
    let l2 = M.to_list m |> List.map fst |> List.filter (fun x -> M.is_max partial_ord x m) in
    if l1=l2 then true
    else Q.Test.fail_reportf "@[max_seq %a,@ max %a@]"
        CCFormat.Dump.(list int) l1
        CCFormat.Dump.(list int) l2
  in
  Q.Test.make ~name:"multiset_max_seq" ~long_factor:5 ~count:10_000 gen1 prop

let max_is_max =
  let pp = CCFormat.to_string (M.pp CCFormat.int) in
  let gen = Q.(map M.of_list (list small_int)) in
  let gen = Q.set_print pp gen in
  let prop m =
    let f x y = Comparison.of_total (Pervasives.compare x y) in
    let l = M.max f m |> M.to_list |> List.map fst in
    List.for_all (fun x -> M.is_max f x m) l
  in
  Q.Test.make
    ~name:"multiset_max_l_is_max" ~long_factor:3 ~count:1000
    gen prop

let suite =
  "test_multiset" >:::
    [ "max" >:: test_max
    ; "compare" >:: test_compare
    ; "cardinal_size" >:: test_cardinal_size
    ]

let props =
  [ compare_and_partial;
    compare_partial_sym;
    compare_partial_trans;
    max_is_max;
    max_seq_correct;
  ]
