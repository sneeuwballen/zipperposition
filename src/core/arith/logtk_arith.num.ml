
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
  let div = div_big_int
  let rem = mod_big_int
  let abs = abs_big_int
  let compare = compare_big_int
  let equal = (=)
  let neg = minus_big_int
end

module Q = struct
end

