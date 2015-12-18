
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Dynamic extensions} *)

open Logtk

type 'a or_error = [ `Ok of 'a | `Error of string ]

(** {2 Type Definitions} *)

(** An extension is allowed to modify an environment *)
type action =
  | Do of ((module Env.S) -> unit)

type prec_action =
  | Prec_do of (Compute_prec.t -> unit)

type init_action =
  | Init_do of (unit -> unit)

type t = {
  name : string;
  prio : int;  (** the lower, the more urgent, the earlier it is loaded *)
  prec_actions : prec_action list;
  init_actions : init_action list;
  actions : action list;
}
(** An extension contains a number of actions that can modify a {!PEnv},
    an environment {!Env.S} or run some initialization action
    (typically, add some CLI argument) *)

let default = {
  name="<no name>";
  prio = 50;
  prec_actions= [];
  init_actions = [];
  actions = [];
}

(** {2 Registration} *)

let section = Const.section

let (__current : t or_error ref) = ref (`Error "could not load plugin")

let _extensions = Hashtbl.create 11

(* TODO: use a mutex? *)

let register self =
  (* register by name, if loading succeeded *)
  if not (Hashtbl.mem _extensions self.name)
  then (
    Util.debugf ~section 1 "register extension %s..." (fun k->k self.name);
    Hashtbl.replace _extensions self.name self
  );
  __current := `Ok self

let dyn_load filename =
  let filename = Dynlink.adapt_filename filename in
  (* be sure no previous plugin remains *)
  __current := `Error ("could not load file " ^ filename);
  (* load the plugin, that should have called [register] *)
  let current =
    try
      Dynlink.loadfile filename;
      !__current
    with Dynlink.Error e ->
      let s = Dynlink.error_message e in
      Util.debugf ~section 0 "@[error loading plugin `%s`:@ %s@]" (fun k->k filename s);
      let msg = "could not load " ^ filename ^ ": " ^ s in
      `Error msg
  in
  current

(** Apply the extension to the Env.t *)
let apply_env ~env ext =
  List.iter (fun (Do f) -> f env) ext.actions

let apply_prec c ext =
  List.iter (fun (Prec_do f) -> f c) ext.prec_actions

let init ext =
  Util.debugf ~section 5 "run init actions of %s" (fun k->k ext.name);
  List.iter (fun (Init_do f) -> f ()) ext.init_actions

let cmp_prio_name a b =
  if a.prio = b.prio
  then String.compare a.name b.name
  else compare a.prio b.prio

let extensions () =
  Sequence.of_hashtbl _extensions
  |> Sequence.map snd
  |> Sequence.sort_uniq ~cmp:cmp_prio_name  (* sort by increasing priority *)
  |> Sequence.to_list

let by_name name =
  try Some (Hashtbl.find _extensions name)
  with Not_found -> None

let names () =
  Sequence.of_hashtbl _extensions
  |> Sequence.map fst
  |> Sequence.to_list
