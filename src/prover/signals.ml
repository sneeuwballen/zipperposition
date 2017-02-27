
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Useful signals} *)

open Logtk

let (on_print_stats : unit Signal.t) = Signal.create ()
(** Called when/if it's time to print statistics *)

let (on_before_process_file : string Signal.t) = Signal.create ()
(** Called before starting solving a problem *)

let (on_exit : int Signal.t) = Signal.create ()
(** Called before exit *)

let (on_dot_output : unit Signal.t) = Signal.create ()
(** Called when dot printers should activate *)

