
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Skolem symbols} *)

(** A Skolem symbol is a witness for an existential property, used
    in {!CNF}.

    Typically, we transform [∃x. p(x)] into [p(sk_x)] where [sk_x]
    is a fresh Skolem constant, "witnessing" the existential property.
*)

type type_ = TypedSTerm.t
type term = TypedSTerm.t
type form = TypedSTerm.t

type ctx
(** Context needed to create new symbols *)

val create :
  ?prefix:string -> ?prop_prefix:string ->
  ?on_new:(ID.t -> type_ -> unit) ->
  unit ->
  ctx
(** New skolem contex. A prefix can be provided, which will be
    added to all newly created skolem symbols.
    @param prefix used to name skolem functions/constants
    @param prop_prefix used to name sub-formulas during CNF
    @param on_id function called whenever a Skolem symbol is created *)

val fresh_skolem : ctx:ctx -> ty:type_ -> vars_count:int -> ID.t
(** Just obtain a fresh skolem symbol. It is also declared
    in the inner signature. *)

val fresh_skolem_prefix : ctx:ctx -> ty:type_ -> vars_count:int -> string -> ID.t
(** Fresh symbol with a different name *)

val pop_new_skolem_symbols : ctx:ctx -> (ID.t * type_) list
(** Remove and return the list of newly created Skolem symbols *)

val counter : ctx -> int
(** Monotonic counter, increased at every definition *)

(** {2 Skolemization} *)

val skolem_form : ctx:ctx -> (type_, term) Var.Subst.t -> type_ Var.t -> form -> term
(** [skolem_form ~ctx subst var f] returns a term [t] made of a new symbol
    applied to the free variables of [f] that do not occur in [subst].
    This term should replace the variable [var], occurring free in [f].

    For instance, [skolem_form ~ctx ø Y p(a, b, Y, X)] will yield
    something like [sk42(X)].
*)

(** {2 Definitions of Formulas} *)

type polarity =
  [ `Pos
  | `Neg
  | `Both
  ]

val pp_polarity : polarity CCFormat.printer

type form_definition = private {
  form: form;
  proxy_id: ID.t; (* name *)
  (* the defined object *)
  proxy : term;
  (* atom/term standing for the defined object *)
  proxy_ty : type_;
  (* type of [proxy_id] *)
  rw_rules: bool;
  (* do we add rewrite rules (instead of an axiom)?
     [proxy -> true if form]
     [proxy -> false if not form] (depending on polarity) *)
  polarity : polarity;
  proof: Proof.step;
  (* source for this definition *)
  as_stmt: Statement.input_t list lazy_t;
}

val pp_form_definition : form_definition CCFormat.printer

val define_form :
  ?pattern:string ->
  ctx:ctx ->
  rw_rules:bool ->
  polarity:polarity ->
  parents:Proof.Parent.t list ->
  form ->
  form_definition
(** [define ~ctx f] returns a new predicate for [f],
    with the free variables of [f] as arguments.

    @return the atomic formula that stands for [f]. *)

type term_definition = private {
  td_id: ID.t;
  td_ty: type_;
  td_rules: (form, term, type_) Statement.def_rule list;
  td_as_def: (form,term,type_) Statement.def;
  td_proof: Proof.step;
  td_stmt: Statement.input_t list lazy_t;
}

val define_term :
  ?pattern:string ->
  ctx:ctx ->
  parents:Proof.Parent.t list ->
  (term list * term) list ->
  term_definition
(** [define_term l] introduces a new function symbol [f] that is
    defined by:
    - for each [args, rhs] in [l], [f args = rhs]
    @param pattern used to name the new function in an informative way
*)

type definition =
  | Def_form of form_definition
  | Def_term of term_definition

val pp_definition : definition CCFormat.printer

val new_definitions : ctx:ctx -> definition list
(** Return the new definitions, without side effects *)

val pop_new_definitions : ctx:ctx -> definition list
(** List of new definitions, that were introduced since the last
    call to {!new_definitions}. The list can be obtained only once,
    after which those definitions are not "new" anymore.

    Will call {!remove_def} so there is no risk of re-using a definition
    with a new polarity. *)

val def_as_stmt : definition -> Statement.input_t list
(** Project the definition into a list of statements *)

(** {2 Attribute} *)
