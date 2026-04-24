exception UnsupportedLiteralKind

module Make (S : sig
  val st : Flex_state.t
end) : sig
  val subsumes : Literals.t -> Literals.t -> bool
end
