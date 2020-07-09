
open Logtk

(** As described in FBoolSup paper, Boolean selection function
    selects positions in the clause that are non-interpreted 
    Boolean subterms. *)

type t = Literal.t array -> Position.t list

type parametrized = strict:bool -> ord:Ordering.t -> t

let no_select _ = []

let from_string ~ord s = no_select

let all () = []