(* signature, ie symbols and operations on symbols *)

(* 
 * - checking equality should be efficient
 * - atoms have to be equipped with a total order relation
 *)
type symbol

val eq : symbol -> symbol -> bool
val compare : symbol -> symbol -> int
val pp_symbol : Format.formatter -> symbol -> unit  (* pretty print object *)

(* special symbols *)
val eq_symbol : symbol  (* symbol for equality predicate *)
val true_symbol : symbol (* symbol for 'true' *)
val bool_symbol : symbol (* symbol for boolean *)
val univ_symbol : symbol (* symbol for universal (terms) sort *)

(* from string *)
val str_to_sym : string -> symbol

(* why is this in the /leaves/ signature?!
type input
val embed : input -> t foterm
(* saturate [proof] [type] -> [proof] * [type] *)
val saturate : input -> input -> t foterm * t foterm
*)
