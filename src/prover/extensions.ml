
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Dynamic extensions} *)

open Logtk

type 'a or_error = ('a, string) CCResult.t

(** {2 Type Definitions} *)

type state = Flex_state.t

(** An extension is allowed to modify an environment *)
type env_action = (module Env.S) -> unit

type prec_action = state -> Compute_prec.t -> Compute_prec.t
(** Actions that modify the set of rules {!Compute_prec} *)

type 'a state_actions = ('a -> state -> state) list
(* a list of actions parametrized by ['a] *)

type t = {
  name : string;
  prio : int;  (** the lower, the more urgent, the earlier it is loaded *)
  start_file_actions : string state_actions;
  post_parse_actions : UntypedAST.statement Sequence.t state_actions;
  post_typing_actions : TypeInference.typed_statement CCVector.ro_vector state_actions;
  post_cnf_actions: Statement.clause_t CCVector.ro_vector state_actions;
  ord_select_actions : (Ordering.t * Selection.t) state_actions;
  ctx_actions : (module Ctx_intf.S) state_actions;
  prec_actions : prec_action list;
  env_actions : env_action list;
}

let default = {
  name="<no name>";
  prio = 50;
  prec_actions= [];
  start_file_actions=[];
  post_parse_actions=[];
  post_typing_actions=[];
  post_cnf_actions=[];
  ord_select_actions=[];
  ctx_actions=[];
  env_actions=[];
}

(** {2 Registration} *)

let section = Const.section

let __current : t or_error ref = ref (CCResult.Error "could not load plugin")

let _extensions = Hashtbl.create 11

let register self =
  (* register by name, if loading succeeded *)
  if not (Hashtbl.mem _extensions self.name)
  then (
    Util.debugf ~section 1 "register extension `%s`..." (fun k->k self.name);
    Hashtbl.replace _extensions self.name self
  );
  __current := CCResult.Ok self

let cmp_prio_name a b =
  if a.prio = b.prio
  then String.compare a.name b.name
  else compare a.prio b.prio

let extensions () =
  CCHashtbl.values _extensions
  |> Sequence.sort_uniq ~cmp:cmp_prio_name  (* sort by increasing priority *)
  |> Sequence.to_list

let by_name name =
  try Some (Hashtbl.find _extensions name)
  with Not_found -> None

let names () = CCHashtbl.keys _extensions
