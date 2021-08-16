open Logtk

exception IsNotCombinator

val mk_s: args:Term.t list -> alpha:Term.t -> beta:Term.t -> gamma:Term.t -> Term.t
val mk_b: args:Term.t list -> alpha:Term.t -> beta:Term.t -> gamma:Term.t -> Term.t
val mk_c: args:Term.t list -> alpha:Term.t -> beta:Term.t -> gamma:Term.t -> Term.t
val mk_k: args:Term.t list -> alpha:Term.t -> beta:Term.t -> Term.t
val mk_i: args:Term.t list -> alpha:Term.t -> Term.t


val bunder_optimizations: (Term.t -> Term.t option) list
val curry_optimizations: (Term.t -> Term.t option) list
val narrow_rules: (Term.t -> Term.t option) list
val abf : rules:(Term.t -> Term.t option) list -> Logtk.Lambda.term -> Term.t
val comb2lam : Term.t -> Term.t
val comb_normalize : Term.t -> Term.t CCOpt.t
val cmp_by_max_weak_r_len : Term.t -> Term.t -> Logtk.Comparison.t * Term.t * Term.t

val narrow: Term.t -> Term.t
