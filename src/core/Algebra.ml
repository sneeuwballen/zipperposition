type operator = int -> int -> int 

type t = {
  name: string;
  coeff_app: operator;
  empty: int;
  accumulator: operator;
}

let create name accumulator coeff_app empty = 
  { name; coeff_app; empty; accumulator; }

let name alg = alg.name 
let accumulator alg = alg.accumulator 
let coeff_app alg = alg.coeff_app 
let empty alg = alg.empty

let sum_algebra = {
  name = "sum_algebra";
  coeff_app = ( * ) ;
  empty = 0;
  accumulator = ( + );
}

let max_algebra = {
  name = "max_algebra";
  coeff_app = ( + );
  accumulator = max;
  empty = 0;
}

let alg_of_string s =

  let alg_map = 
    [
      "sum", sum_algebra;
      "max", max_algebra;
    ]
  in 
  try 
    List.assoc s alg_map
  with Not_found -> invalid_arg "Algebra not found"
  