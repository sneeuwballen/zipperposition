
(* TODO: Re-order the definitions *)

(** Modular and incremental implementation of the simplex. *)

(** The types of the variables used by the equations to solve *)
module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  (** The given type of the variables *)
  type var

  (** The type of a (possibly not solved) linear system *)
  type t

  (** Generic type returned when solving the simplex. A solution is a list of bindings
      that satisfies all the constraints inside the system. If the system is unsatisfiable,
      an explanation of type ['cert] is returned. *)
  type 'cert res =
    | Solution of (var * Q.t) list
    | Unsatisfiable of 'cert

  (** An unsatisfiability explanation is a couple [(x, expr)]. If [expr] is the empty list, then there is a contradiction
      between two given bounds of [x]. Else, the explanation is an equality [x = expr] that is valid (it can be derived
      from the original equations of the system) from which a bound can be deduced which contradicts an already given bound of the system. *)
  type k_cert = var * (Q.t * var) list

  type n_cert = cert_tree option ref
  and cert_tree =
    | Branch of var * Z.t * n_cert * n_cert
    | Explanation of k_cert
    (* TODO: Better explanation ? *)
  (** The type of explanation for integer systems. In one case, the system is unsatisfiable when seen as a rational system.
      In the other case, two cases are considered : for a given variable [x] and a bound [b], either [x] is lower than [b],
      or it is greater then [b + 1]. *)

  type k_res = k_cert res
  type n_res = n_cert res
  (** Types returned when solving a system. *)

  (** The type of traces of the optimizations performed on a simplex. *)
  type optim =
    | Tight of var
    | Multiply of var * Q.t

  (** TODO *)
  type debug_printer = Format.formatter -> t -> unit

  (** {3 Simplex construction} *)

  (** The empty system *)
  val empty       : t

  (** [add_eq s (x, eq)] returns a system containing the same constraints as [s], plus the equation (x = eq). *)
  val add_eq      : t -> var * (Q.t * var) list -> t

  (** [add_bounds (x, lower, upper)] returns a system containing the same contraints as [s], plus
      the bounds [lower] and [upper] for the given variable [x]. If the bound is loose on one side (no upper bounds for instance),
      the values [Zarith.Q.inf] and [Zarith.Q.minus_inf] can be used. By default, in a system, all variables have no bounds,
      i.e have lower bound [Zarith.Q.minus_inf] and upper bound [Zarith.Q.inf]. *)
  val add_bounds  : t -> var * Q.t * Q.t -> t

  (** {3 Simplex solving} *)

  (** [ksolve s] solves the system [s] and returns a solution, if one exists. This function may chnge
      the internal representation of the system to that of an equivalent one (permutation of basic and
      non basic variables and pivot operation on the tableaux).
      @param debug An optional debug option can be given, and will be applied to all systems encountered
      while solving the system, including the initial and final states of the system. Can be used for
      printing intermediate states of the system. *)
  val ksolve       : ?debug:(debug_printer) -> t -> k_res

  (** [nsolve s int_vars] solve the system [s] considering the variables in [int_vars] as integers instead
      of rationals. There is no guarantee that this function will terminate (for instance, on the system
      ([1 <= 3 * x + 3 * y <= 2], [nsolve] will NOT terminate), it hence recommended to apply a global
      bounds to the variables of a system before trying to solve it with this function.
      @param debug An optional debug option can be given, and will be applied to all systems encountered
      at the end of a branch while solving the system. Can be used for printing intermediate states of the system. *)
  val nsolve      : t -> var list -> n_res

  (** [safe_nsolve s int_vars] solves the system [s] considering the variables in [int_vars] as integers.
      This function always terminate, thanks to a global bound that is applied to all variables of int_vars.
      The global bound is also returned to allow for verification.
      Due to the bounds being very high, [safe_nsolve] may take a lot of time and memory. It is recommended to apply
      some optimizations before trying to solve system using this function. *)
  val safe_nsolve : t -> var list -> Q.t * n_res

  (** {3 Simplex optimizations} *)
  (* TODO: do not modify simplexes in place/ export functions to apply optimization traces ?*)

  (** [tighten int_vars s] tightens all the bounds of the variables in [int_vars] and returns the list of optimizations
      performed on the system. *)
  val tighten : var list -> t -> optim list

  (** [normalize int_vars s], normalizes the system [s] (in place), and returns the list of optimizations performed on it.
      [int_vars] is the list of variable that should be assigned an integer.
      A normalized system has only integer coeficients in his tableaux. Furthermore, in any line (i.e in the expression of a basic variable [x]),
      the gcd of all coeficients is [1]. This includes the bounds of [x], except in the following case.
      If all pertinent variables (have a non-zero coeficient) in the expression of [x] are in [int_vars], then the bounds are divided by the gcd
      of the coeficients in the expression, and then rounded (since we can deduce that [x] must be an integer as well). *)
  val normalize : var list -> t -> optim list

  (** Apply all optimizations to a simplex. *)
  val preprocess  : t -> var list -> optim list

  (** Apply the given optimizations to the simplex. *)
  val apply_optims : (t -> optim list) list -> t -> optim list

  (** {3 Access functions} *)
  (* TODO: add new access functions ? *)

  (** [get_tab s] returns the current tableaux of [s] as a triple [(l, l', tab)] where [l] is the list of the
      non-basic variables, [l'] the list of basic variables and [tab] the list of the rows of the tableaux in
      the same order as [l] and [l']. *)
  val get_tab     : t -> var list * var list * Q.t list list

  (** [get_assign s] returns the current (partial) assignment of the variables in [s] as a list of bindings.
      Only non-basic variables (as given by [get_tab]) should appear in this assignent. As such, and according to
      simplex invariants, all variables in the assignment returned should satisfy their bounds. *)
  val get_assign  : t -> (var * Q.t) list

  (* [get_full_assign s] returns the current values of all the variables present in the system.
      Notice that it doesn't mean the assignment returned satisfies all bounds.*)
  val get_full_assign : t -> (var * Q.t) list

  (** [get_bounds s x] returns the pair [(low, upp)] of the current bounds for the variable [x]. 
      Notice that it is possible that [low] is strictly greater than [upp]. *)
  val get_bounds : t -> var -> Q.t * Q.t

  (** [get_all_bounds s] returns the list of all the explicit bounds of [s]. Any variable not present
      in the return value is assumed to have no bounds (i.e lower bound [Zarith.Q.minus_inf] and
      upper bound [Zarith.Q.inf]). *)
  val get_all_bounds : t -> (var * (Q.t * Q.t)) list

  (** {3 Printing functions} *)

  (** [print_debug print_var] returns a suitable function for printing debug info on the current state of a system.
      It can for instance be used as the debug function of [solve] to see the evolution of the simplex. *)
  val print_debug : (Format.formatter -> var -> unit) -> Format.formatter -> t -> unit


end

(** Functor building an implementation of the simplex solver given a totally ordered
    type for the variables *)
module Make : functor (Var : OrderedType) -> S with type var = Var.t


(** {2 Higher-Level Interface} *)

module type HELPER = sig
  (** User provided variable type *)
  type external_var

  type var = private
    | Intern of int
    | Extern of external_var

  (** Fresh internal variable *)
  val fresh_var : unit -> var

  (** Lift an external variable in the [var] type *)
  val mk_var : external_var -> var

  (** the usual system, but the extended variable type *)
  include S with type var := var

  type monome = (Q.t * external_var) list

  type op = LessEq | Eq | GreaterEq

  type constraint_ = op * monome * Q.t

  val add_constraints : t -> constraint_ list -> t
end

module MakeHelp(Var : OrderedType) : HELPER with type external_var = Var.t
