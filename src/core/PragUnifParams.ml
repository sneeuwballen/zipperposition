let k_max_depth = Flex_state.create_key ()
let k_max_app_projections = Flex_state.create_key ()
let k_max_var_imitations = Flex_state.create_key ()
let k_max_rigid_imitations = Flex_state.create_key ()
let k_max_identifications = Flex_state.create_key ()
let k_max_elims           = Flex_state.create_key ()
let k_imit_first = Flex_state.create_key ()
let k_logop_mode : [`Conservative | `Pragmatic | `Off ] Flex_state.key = Flex_state.create_key ()

let k_pattern_decider = Flex_state.create_key ()
let k_solid_decider = Flex_state.create_key ()
let k_fixpoint_decider = Flex_state.create_key ()

let k_solidification_limit = Flex_state.create_key ()

let k_max_inferences = Flex_state.create_key ()
let k_unif_alg_is_terminating = Flex_state.create_key ()
let k_schedule_inferences = Flex_state.create_key ()

let k_max_unifs_solid_ff = Flex_state.create_key ()
let k_use_weight_for_solid_subsumption = Flex_state.create_key ()
let k_sort_constraints = Flex_state.create_key ()

let k_oracle_composer : (((Subst.t option OSeq.t) OSeq.t -> Subst.t option OSeq.t) Flex_state.key) =  (Flex_state.create_key ()) 

let k_try_lfho = Flex_state.create_key ()

let k_skip_multiplier = Flex_state.create_key ()
