(* signature, ie symbols and operations on symbols *)

type symbol = string

let eq x y = x = y
let compare = Pervasives.compare
let pp_symbol formatter s = Format.pp_print_string formatter s

(* special symbols *)
let eq_symbol = "="  (* symbol for equality predicate *)
let true_symbol = "true" (* symbol for 'true' *)
let bool_symbol = "Bool" (* symbol for boolean *)
let univ_symbol = "U" (* symbol for universal (terms) sort *)

(* from string *)
let str_to_sym s = s
