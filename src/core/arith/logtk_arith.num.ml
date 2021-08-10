
module Z = struct

  include Big_int
  type t = big_int
  let of_int = big_int_of_int
  let of_string = big_int_of_string
  let one = of_int 1
  let zero = of_int 0
  let minus_one = of_int (-1)
  let to_string = string_of_big_int
  let pred = pred_big_int
  let succ = succ_big_int
  let add = add_big_int
  let sub = sub_big_int
  let mul = mult_big_int
  let sign = sign_big_int
  let gcd = gcd_big_int
  let div = div_big_int
  let ( * ) = mul
  let (-) = sub
  let (+) = add
  let (/) = div
  let lcm a b = (a * b) / gcd a b
  let max = max_big_int
  let min = min_big_int
  let to_int_exn i = int_of_big_int i
  let geq a b = compare_big_int a b >= 0
  let leq a b = compare_big_int a b <= 0
  let gt a b = compare_big_int a b > 0
  let lt a b = compare_big_int a b < 0
  let hash x = Hashtbl.hash (to_string x)
  let rem = mod_big_int
  let div_rem a b = div a b, rem a b
  let abs = abs_big_int
  let compare = compare_big_int
  let equal = eq_big_int
  let neg = minus_big_int
  let pp_print out n = Format.fprintf out "%s" (string_of_big_int n)
end

module Q = struct
  open Ratio
  type t = Ratio.ratio
  let of_int = ratio_of_int
  let of_bigint = ratio_of_big_int
  let abs = abs_ratio
  let zero = of_int 0
  let one = of_int 1
  let minus_one = of_int ~-1
  let add = add_ratio
  let mul = mult_ratio
  let div = div_ratio
  let sub = sub_ratio
  let sign = sign_ratio
  let to_string = string_of_ratio
  let to_bigint = floor_ratio
  let lt = lt_ratio
  let gt = gt_ratio
  let leq = le_ratio
  let geq = ge_ratio
  let pp_print out n = Format.fprintf out "%s" (to_string n)
  let hash x = Hashtbl.hash (to_string x)
  let neg = minus_ratio
  let equal = eq_ratio
  let compare = compare_ratio
  let of_string = ratio_of_string
  let ( * ) = mul
  let (-) = sub
  let (+) = add
  let (/) = div
  let of_ints a b = of_int a / of_int b
end

