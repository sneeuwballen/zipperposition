
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Print Proofs} *)

open Hornet_types

type t = proof_with_res

val pp_dag : t CCFormat.printer
(** Print the proof as text *)

val pp_dot : ?compress:bool -> t CCFormat.printer
(** Print proof in DOT format
    @param compress if true, show compressed proof *)

val pp_dot_file : ?compress:bool -> string -> t -> unit
(** [pp_dot_file file p] writes the proof [p] into the given file,
    in DOT format
    @param compress if true, show compressed proof *)
