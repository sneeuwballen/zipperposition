
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Test terms *)

open Logtk

module O = Ordinal

let long_factor = 5

let arb : O.t QCheck.arbitrary =
  let gen_const = QCheck.Gen.(0--15 >|= O.const) in
  let gen_sized =
    QCheck.Gen.(fix (fun self n ->
        if n=0 then gen_const
        else
          frequency
            [ 3, gen_const;
              1, map2 O.add (self (n-1)) (self (n-1));
            ]))
  in
  let gen = QCheck.Gen.(0 -- 4 >>= gen_sized) in
  let rec shrink (o:O.t) =
    let open QCheck.Iter in
    match o with
      | O.Zero -> empty
      | O.Sum l ->
        (QCheck.Shrink.list ~shrink:shrink_pair l >|= O.of_list)
  and shrink_pair (i,n) =
    let open QCheck.Iter in
    (QCheck.Shrink.int i >|= fun i -> i,n)
    <+>
    (shrink n >|= fun n -> i,n)
  in
  QCheck.make gen ~shrink ~print:O.to_string

let check_invariant =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_invariant" arb O.check_inv_

let check_plus_invariant =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_plus_invariant"
    (QCheck.pair arb arb) (fun (a,b) -> O.check_inv_ (O.add a b))

let check_plus_com =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_plus_com"
    (QCheck.pair arb arb) (fun (a,b) -> O.equal (O.add a b) (O.add b a))

let check_plus_assoc =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_plus_assoc"
    (QCheck.triple arb arb arb)
    (fun (a,b,c) -> O.equal (O.add a (O.add b c)) (O.add (O.add a b) c))

let check_mult_invariant =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_mult_invariant"
    (QCheck.pair arb arb) (fun (a,b) -> O.check_inv_ (O.mult a b))

let check_mult_com =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_mult_com"
    (QCheck.pair arb arb) (fun (a,b) -> O.equal (O.mult a b) (O.mult b a))

let check_mult_assoc =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_mult_assoc"
    (QCheck.triple arb arb arb)
    (fun (a,b,c) -> O.equal (O.mult a (O.mult b c)) (O.mult (O.mult a b) c))

let check_distrib =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_distrib"
    (QCheck.triple arb arb arb)
    (fun (a,b,c) ->
       O.equal
         (O.mult a (O.add b c))
         (O.add (O.mult a b) (O.mult a c)))

let check_ord_plus =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_ord_plus"
    (QCheck.triple arb arb arb)
    (fun (a,b,c) ->
       if O.compare a b < 0
       then O.compare (O.add a c) (O.add b c) < 0
       else QCheck.assume_fail ())

let check_ord_mult =
  QCheck.Test.make ~long_factor ~count:1000 ~name:"ordinal_ord_mult"
    (QCheck.triple arb arb arb)
    (fun (a,b,c) ->
       if O.compare a b < 0 && O.compare c O.zero > 0
       then O.compare (O.mult a c) (O.mult b c) < 0
       else QCheck.assume_fail ())

let props =
  [ check_invariant;
    check_plus_invariant;
    check_plus_com;
    check_plus_assoc;
    check_mult_invariant;
    check_mult_com;
    check_mult_assoc;
    check_distrib;
    check_ord_plus;
    check_ord_mult;
  ]
