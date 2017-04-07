
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Interfaces} *)

module type EQ = sig
  type t
  val equal : t -> t -> bool
end

module type HASH = sig
  include EQ
  val hash : t -> int
end

module type ORD = sig
  type t
  val compare : t -> t -> int
end

(** Print a type t *)
module type PRINT = sig
  type t
  val pp : t CCFormat.printer
  val to_string : t -> string
end

module type PRINT1 = sig
  type 'a t
  val pp : 'a CCFormat.printer -> 'a t CCFormat.printer
  val to_string : 'a CCFormat.printer -> 'a t -> string
end

module type PRINT2 = sig
  type ('a, 'b) t
  val pp : 'a CCFormat.printer -> 'b CCFormat.printer -> ('a,'b) t CCFormat.printer
  val to_string : 'a CCFormat.printer -> 'b CCFormat.printer -> ('a,'b) t -> string
end

module type PRINT3 = sig
  type ('a, 'b, 'c) t
  val pp : 'a CCFormat.printer -> 'b CCFormat.printer -> 'c CCFormat.printer -> ('a,'b,'c) t CCFormat.printer
  val to_string : 'a CCFormat.printer -> 'b CCFormat.printer -> 'c CCFormat.printer -> ('a,'b,'c) t -> string
end

(** Register printers by name *)
module type PRINT_OVERLOAD = sig
  type t
  val pp_with : string -> t CCFormat.printer
  val add_printer : string -> t CCFormat.printer -> unit
  val set_default_printer : string -> unit   (** Used by PRINT.pp... *)
end

module type PRINT_DE_BRUIJN = sig
  type t
  type term
  type print_hook = int -> term CCFormat.printer -> Format.formatter -> term -> bool
  (** User-provided hook that can be used to print terms (for composite cases)
      before the default printing occurs. The int argument is the De Bruijn
      depth in the term.
      A hook takes as arguments the depth and the recursive printing function
      that it can use to print subterms.
      A hook should return [true] if it fired, [false] to fall back
      on the default printing. *)

  val pp_depth : ?hooks:print_hook list -> int -> t CCFormat.printer
end

module type ITER = sig
  type 'a t

  val to_seq : 'a t -> 'a Sequence.t
  val of_seq : ?init:'a t -> 'a Sequence.t -> 'a t
  val to_list : 'a t -> 'a list
  val of_list : ?init:'a t -> 'a list -> 'a t
end

module type MONOID = sig
  type t

  val zero : t
  val plus : t -> t -> t
end

module type GROUP = sig
  include MONOID
  val inverse : t -> t
end
