
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Dynamic extensions} *)

open Logtk

type 'a or_error = [ `Ok of 'a | `Error of string ]

(** {2 Type Definitions} *)

(** An extension is allowed to modify an environment *)
type action =
  | Do of ((module Env.S) -> unit)

type penv_action =
  | Penv_do of (PEnv.t -> unit)

type init_action =
  | Init_do of (unit -> unit)

type t = {
  name : string;
  penv_actions : penv_action list;
  init_actions : init_action list;
  actions : action list;
}
(** An extension contains a number of actions that can modify a {!PEnv},
    an environment {!Env.S} or run some initialization action
    (typically, add some CLI argument) *)

let default = {
  name="<no name>";
  penv_actions = [];
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
  then begin
    Util.debug ~section 1 "register extension %s..." self.name;
    Hashtbl.replace _extensions self.name self
  end;
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
      Util.debug ~section 0 "error loading plugin %s: %s" filename s;
      let msg = "could not load " ^ filename ^ ": " ^ s in
      `Error msg
  in
  current

(** Apply the extension to the Env.t *)
let apply_env ~env ext =
  List.iter (fun (Do f) -> f env) ext.actions

let apply_penv ~penv ext =
  List.iter (fun (Penv_do f) -> f penv) ext.penv_actions

let init ext =
  Util.debug ~section 5 "run init actions of %s" ext.name;
  List.iter (fun (Init_do f) -> f ()) ext.init_actions

let extensions () =
  Sequence.of_hashtbl _extensions
    |> Sequence.map snd
    |> Sequence.to_rev_list

let by_name name =
  try Some (Hashtbl.find _extensions name)
  with Not_found -> None

let names () =
  Sequence.of_hashtbl _extensions
    |> Sequence.map fst
    |> Sequence.to_rev_list
