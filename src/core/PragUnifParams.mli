val k_max_depth : int Flex_state.key
val k_max_app_projections : int Flex_state.key
val k_max_var_imitations : int Flex_state.key
val k_max_rigid_imitations : int Flex_state.key
val k_max_identifications : int Flex_state.key
val k_max_elims           : int Flex_state.key

val k_imit_first : bool Flex_state.key
val k_logop_mode : [`Conservative | `Pragmatic | `Off ] Flex_state.key

val k_pattern_decider : bool Flex_state.key
val k_solid_decider : bool Flex_state.key
val k_fixpoint_decider : bool Flex_state.key

val k_solidification_limit : int Flex_state.key

val k_max_inferences : int Flex_state.key
val k_unif_alg_is_terminating : bool Flex_state.key
val k_schedule_inferences : bool Flex_state.key

val k_max_unifs_solid_ff : int Flex_state.key
val k_use_weight_for_solid_subsumption : bool Flex_state.key
val k_sort_constraints : bool Flex_state.key

val k_try_lfho : bool Flex_state.key

val k_oracle_composer : (((Subst.t option OSeq.t) OSeq.t -> Subst.t option OSeq.t) Flex_state.key)

val k_skip_multiplier : float Flex_state.key
