
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting} *)

open Libzipperposition

module T = FOTerm
module Stmt = Statement

let section = Util.Section.zip

module Make(E : Env_intf.S) = struct

  (* TODO: rewrite system (merge with demodulation?) *)

  let setup() = ()
end

let post_cnf stmts st =
  CCVector.iter Statement.scan_stmt_for_defined_cst stmts;
  st

let env_action (module E : Env_intf.S) =
  let module M = Make(E) in
  M.setup ()

let extension =
  let open Extensions in
  { default with
    name = "rewriting";
    post_cnf_actions=[post_cnf];
    env_actions=[env_action];
  }

