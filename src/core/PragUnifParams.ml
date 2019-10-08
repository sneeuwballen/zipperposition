let max_depth = ref 4
let max_app_projections = ref 2
let max_var_imitations = ref 1
let max_rigid_imitations = ref 3
let max_identifications = ref 0
let max_elims           = ref 0

let _cons_e = ref true
let _imit_first = ref false

let pattern_decider = ref true
let solid_decider = ref false

let solidification_limit = ref 5

let max_inferences = ref (-1)

let max_unifs_solid_ff = ref (-1)
let use_weight_for_solid_subsumption = ref true

type e_dir = 
 | LowToHigh 
 | HighToLow

let elim_direction = ref HighToLow


let all_params_to_max () = 
  max_depth := 1000;
  max_app_projections := 1000;
  max_var_imitations := 1000;
  max_rigid_imitations := 1000;
  max_identifications := 1000;
  max_elims           := 1000;
  max_inferences := -1