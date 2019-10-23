val max_depth : int ref
val max_app_projections : int ref
val max_var_imitations : int ref
val max_rigid_imitations : int ref
val max_identifications : int ref
val max_elims           : int ref

type e_dir = 
 | LowToHigh 
 | HighToLow

val elim_direction : e_dir ref

val _cons_e : bool ref
val _imit_first : bool ref

val pattern_decider : bool ref
val solid_decider : bool ref
val fixpoint_decider : bool ref

val solidification_limit : int ref

val max_inferences : int ref

val max_unifs_solid_ff : int ref
val use_weight_for_solid_subsumption : bool ref

val all_params_to_max : unit -> unit
