
open Libzipperposition
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

let test_max () =
  let m = M.of_list [1;2;2;3;1] in
  assert_equal (M.of_list [3]) (M.max f m)

let test_compare () =
  let m1 = M.of_list [1;1;2;3] in
  let m2 = M.of_list [1;2;2;3] in
  assert_equal ~printer:Comparison.to_string Comparison.Lt (M.compare_partial f m1 m2);
  assert_bool "ord" (M.compare m1 m2 < 0);
  ()

let test_cardinal_size () =
  let m = M.of_coeffs [1, Z.(~$ 2); 3, Z.(~$ 40)] in
  assert_equal 2 (M.size m);
  assert_equal ~cmp:Z.equal ~printer:Z.to_string Z.(~$ 42) (M.cardinal m);
  ()

let _sign = function
  | 0 -> 0
  | n when n < 0 -> -1
  | _ -> 1

let compare_and_partial =
  let gen = Q.Gen.(
    pair (list small_int) (list small_int) >>= fun (l1, l2) ->
    return (M.of_list l1, M.of_list l2)
  )
  and pp =
    let pp1 = CCFormat.to_string (M.pp CCFormat.int) in
    Q.Print.pair pp1 pp1
  in
  let gen = QCheck.make ~print:pp gen in
  (* "naive" comparison function (using the general ordering on multisets) *)
  let compare' m1 m2 =
    let f x y = Comparison.of_total (Pervasives.compare x y) in
    Comparison.to_total (M.compare_partial f m1 m2)
  in
  let prop (m1,m2) =
    _sign (compare' m1 m2) = _sign (M.compare m1 m2)
  in
  QCheck.Test.make ~name:"multiset_compare_and_compare_partial" ~count:1000 gen prop

let max_is_max =
  let pp = CCFormat.to_string (M.pp CCFormat.int) in
  let gen = Q.(map M.of_list (list small_int)) in
  let gen = Q.set_print pp gen in
  let prop m =
    let f x y = Comparison.of_total (Pervasives.compare x y) in
    let l = M.max f m |> M.to_list |> List.map fst in
    List.for_all (fun x -> M.is_max f x m) l
  in
  Q.Test.make ~name:"multiset_max_l_is_max" ~count:1000 gen prop

let suite =
  "test_multiset" >:::
    [ "max" >:: test_max
    ; "compare" >:: test_compare
    ; "cardinal_size" >:: test_cardinal_size
    ]

let props =
  [ compare_and_partial
  ; max_is_max
  ]
